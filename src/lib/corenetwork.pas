{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit corenetwork;

interface

//{$DEFINE VERBOSE}

uses
   baseunix, unixtype, sockets, exceptions;

const
   timeoutForever = -1;
   kMaxBuffer = 1024 * 16; // max buffer 16KB per connection

type
   TBaseSocket = class;
   TListenerSocket = class;
   TNetworkSocket = class;

   TNewConnectionEvent = procedure (ListenerSocket: TListenerSocket) of object;
   TDisconnectEvent = procedure (Socket: TBaseSocket) of object;
   TOverflowEvent = procedure (Socket: TNetworkSocket) of object;

   TBaseSocket = class
    protected
      FConnected: Boolean;
      FOnDisconnect: TDisconnectEvent;
      FSocketNumber: cint;
    public
      procedure Disconnect(); virtual; abstract;
      function Read(): Boolean; virtual; abstract;
      property Connected: Boolean read FConnected;
      property OnDisconnect: TDisconnectEvent read FOnDisconnect write FOnDisconnect;
   end;

   TListenerSocket = class(TBaseSocket)
    protected
      FOnNewConnection: TNewConnectionEvent;
      FPort: Word;
    public
      constructor Create(Port: Word);
      destructor Destroy(); override;
      procedure Disconnect(); override;
      function Read(): Boolean; override;
      property OnNewConnection: TNewConnectionEvent read FOnNewConnection write FOnNewConnection;
   end;

   TNetworkSocket = class(TBaseSocket)
    protected
      FAddr: TINetSockAddr;
      FPendingWrites: Pointer;
      FPendingWritesLength: Cardinal;
      FOnOverflow: TOverflowEvent;
      function InternalRead(Data: array of byte): Boolean; virtual; abstract;
    public
      constructor Create(Listener: TListenerSocket);
      destructor Destroy(); override;
      procedure Disconnect(); override;
      function Read(): Boolean; override;
      procedure Write();
      procedure Write(const S: RawByteString); { Do not pass an empty string }
      procedure Write(const S: array of Byte); { Do not pass an empty array }
      procedure Write(const S: Pointer; const Len: Cardinal);
      property OnOverflow: TOverflowEvent read FOnOverflow write FOnOverflow;
   end;

   PSocketListItem = ^TSocketListItem;
   TSocketListItem = record
     Next: PSocketListItem;
     Value: TBaseSocket;
   end;

   TNetworkServer = class
    protected
      FList: PSocketListItem;
      FFileDescriptorSet, FWriteFileDescriptorSet: TFDSet;
      FMaxSocketNumber: cint;
      FHavePendingDisconnects: Boolean;
      procedure Add(Socket: TBaseSocket);
      procedure Remove(Socket: TBaseSocket);
      procedure Empty();
      procedure HandleNewConnection(ListenerSocket: TListenerSocket);
      procedure HandleDisconnect(Socket: TBaseSocket);
      procedure HandleOverflow(Socket: TNetworkSocket);
      function CreateNetworkSocket(ListenerSocket: TListenerSocket): TNetworkSocket; virtual; abstract;
    public
      constructor Create();
      constructor Create(Port: Word); // automatically creates a listener
      destructor Destroy(); override;
      function AddListener(const Port: Word): TListenerSocket;
      function Select(Timeout: cint): Boolean; // timeoutForever (-1) or time in milliseconds; returns True if timed out
   end;

implementation

uses
   sysutils {$IFDEF VERBOSE}, errors {$ENDIF}; // errors is for StrError

// {$DEFINE DISABLE_NAGLE} // should not be necessary

constructor TListenerSocket.Create(Port: Word);
var
   Addr: TINetSockAddr;
   Argument: Integer;
begin
   inherited Create();
   FPort := Port;
   FSocketNumber := fpSocket(AF_INET, SOCK_STREAM, 0);
   if (FSocketNumber < 0) then
      raise ESocketError.Create(SocketError);
   Argument := 1;
   fpSetSockOpt(FSocketNumber, SOL_SOCKET, SO_REUSEADDR, @Argument, SizeOf(Argument));
   Addr.sin_family := AF_INET;
   Addr.sin_addr.s_addr := htonl(INADDR_ANY);
   Addr.sin_port := htons(FPort);
   if (fpBind(FSocketNumber, PSockAddr(@Addr), SizeOf(Addr)) <> 0) then
      raise ESocketError.Create(SocketError);
   if (fpListen(FSocketNumber, 32) <> 0) then // allow up to 32 pending connections at once
      raise ESocketError.Create(SocketError);
   FConnected := True;
end;

destructor TListenerSocket.Destroy();
begin
   Assert(not FConnected);
   inherited;
end;

procedure TListenerSocket.Disconnect();
begin
   if (FConnected) then
   begin
      if (Assigned(FOnDisconnect)) then
         FOnDisconnect(Self);
      if (fpClose(FSocketNumber) <> 0) then
         raise EKernelError.Create(fpGetErrNo);
   end;
   FConnected := False;
end;

function TListenerSocket.Read(): Boolean;
begin
   Assert(FConnected);
   Assert(Assigned(FOnNewConnection));
   FOnNewConnection(Self);
   Result := True;
end;


constructor TNetworkSocket.Create(Listener: TListenerSocket);
var
   Options: cint;
   AddrLen: PSockLen;
   {$IFDEF DISABLE_NAGLE}
   Argument: Integer;
   {$ENDIF}
begin
   inherited Create();
   New(AddrLen);
   AddrLen^ := SizeOf(FAddr);
   FSocketNumber := fpAccept(Listener.FSocketNumber, PSockAddr(@FAddr), AddrLen);
   Dispose(AddrLen);
   if (FSocketNumber < 0) then
      raise ESocketError.Create(SocketError);
   Options := FpFcntl(FSocketNumber, F_GETFL);
   if (Options < 0) then
      raise EKernelError.Create(fpGetErrNo);
   Options := FpFcntl(FSocketNumber, F_SETFL, Options or O_NONBLOCK);
   if (Options < 0) then
      raise EKernelError.Create(fpGetErrNo);
   {$IFDEF DISABLE_NAGLE}
   Argument := 1;
   fpSetSockOpt(FSocketNumber, IPPROTO_TCP, TCP_NODELAY, @Argument, SizeOf(Argument));
   {$ENDIF}
   FConnected := True;
end;

destructor TNetworkSocket.Destroy();
begin
   Assert(not FConnected);
   if (FPendingWritesLength > 0) then
   begin
      Assert(Assigned(FPendingWrites));
      FreeMem(FPendingWrites, FPendingWritesLength);
      FPendingWrites := nil;
      FPendingWritesLength := 0;
   end;
   inherited;
end;

procedure TNetworkSocket.Disconnect();
var
   Error: Integer;
begin
   if (FConnected) then
   begin
      if (Assigned(FOnDisconnect)) then
         FOnDisconnect(Self);
      Error := fpShutdown(FSocketNumber, 2);
      if ((Error <> 0) and (SocketError <> 107)) then // 107 = already disconnected
      begin
         {$IFDEF DEBUG}
            {$IFDEF VERBOSE} Writeln('Raising the following socket error while disconnecting: ', StrError(SocketError)); {$ENDIF}
            raise ESocketError.Create(SocketError);
         {ELSE}
            {$IFDEF VERBOSE} Writeln('Ignoring the following socket error while disconnecting: ', StrError(SocketError)); {$ENDIF}
         {$ENDIF}
      end;
   end;
   FConnected := False;
end;

function TNetworkSocket.Read(): Boolean;
var
   Data: array of Byte;
   Received: ssize_t;
begin
   if (FConnected) then
   begin
      SetLength(Data, 4096); // smaller than High(size_t)
      Received := fpRecv(FSocketNumber, @Data[0], Length(Data), 0); {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
      if (Received > 0) then
         SetLength(Data, Received); // else doesn't matter, Data isn't used
   end;
   // Received=0 means that the peer gracefully disconnected
   Result := FConnected and (Received > 0) and InternalRead(Data);
end;

procedure TNetworkSocket.Write();
begin
   Write(nil, 0);
end;

procedure TNetworkSocket.Write(const S: RawByteString);
begin
   Assert(Length(S) > 0);
   Write(@S[1], Length(S)); {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
end;

procedure TNetworkSocket.Write(const S: array of byte);
begin
   Assert(Length(S) > 0);
   Write(@S[0], Length(S)); {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
end;

procedure TNetworkSocket.Write(const S: Pointer; const Len: Cardinal);

(*
  function fpSend(s: cint; msg: pointer; len: size_t; flags: cint): ssize_t;
  var
     i: Cardinal;
  begin
     system.Write('fpSend called to send: ');
     for i := 0 to len-1 do
        system.Write(Chr(Byte((Msg+i)^)));
     Writeln();
     system.Write('                  hex: ');
     for i := 0 to len-1 do
        system.Write(IntToHex(Byte((Msg+i)^), 2), ' ');
     Writeln();
     Result := Sockets.fpSend(s, msg, len, flags);
  end;
*)

var
   Sent: ssize_t;
   OldBuffer: Pointer;
   NeedCopy: Boolean;
begin
   if (not FConnected) then
      Exit; // Don't bother doing any work if we're not going to send it anyway
   if (Len > 0) then
   begin
      Assert(Assigned(S));
      if (FPendingWritesLength > 0) then
      begin
         Assert(Assigned(FPendingWrites));
         ReallocMem(FPendingWrites, FPendingWritesLength + Len);
         {$POINTERMATH ON}
         Move(S^, (FPendingWrites + FPendingWritesLength)^, Len);
         {$POINTERMATH OFF}
         Inc(FPendingWritesLength, Len);
         NeedCopy := False;
      end
      else
      begin
         Assert(not Assigned(FPendingWrites));
         FPendingWrites := S;
         FPendingWritesLength := Len;
         NeedCopy := True;
      end;
   end
   else // we are called with Len=0 when the select loop just wants us to try flushing our buffer
      NeedCopy := False;
   // if we get here and FPendingWritesLength = 0, that probably means
   // that we were called once and failed to send everything, then
   // immediately called again and successfully sent everything that
   // time, and only then did the select loop get around to us.
   if (FPendingWritesLength > 0) then
   begin
      // MSG_NOSIGNAL suppresses SIGPIPE on Linux
      Sent := fpSend(FSocketNumber, FPendingWrites, FPendingWritesLength, {$IFDEF Linux} MSG_NOSIGNAL {$ELSE} 0 {$ENDIF});
      if (Sent < FPendingWritesLength) then
      begin
         if (Assigned(FOnOverflow) and
             ((Sent >= 0) or
              (SocketError = ESysEAgain) or
              (SocketError = ESysEWouldBlock) or
              (SocketError = ESysEIntr)) and
             (FPendingWritesLength - Sent <= kMaxBuffer)) then
         begin
            if (Sent > 0) then
            begin
               OldBuffer := FPendingWrites;
               Assert(FPendingWritesLength > Sent);
               Dec(FPendingWritesLength, Sent);
               FPendingWrites := GetMem(FPendingWritesLength);
               {$POINTERMATH ON}
               Move((OldBuffer + Sent)^, FPendingWrites^, FPendingWritesLength);
               {$POINTERMATH OFF}
               if (not NeedCopy) then
                  FreeMem(OldBuffer, FPendingWritesLength + Sent); {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
            end
            else
            if (NeedCopy) then
            begin
               OldBuffer := FPendingWrites;
               FPendingWrites := GetMem(FPendingWritesLength);
               Move(OldBuffer^, FPendingWrites^, FPendingWritesLength);
            end;
            Assert(FConnected);
            {$IFDEF DEBUG}
            {$IFDEF VERBOSE} Writeln('Had overflow sending data to socket number ', FSocketNumber, '; now have ', FPendingWritesLength, ' bytes left to send.'); {$ENDIF}
            {$ENDIF}
            FOnOverflow(Self);
         end
         else
         begin
            if (NeedCopy) then
            begin
               // don't bother getting a copy of the buffer
               FPendingWrites := nil;
               FPendingWritesLength := 0;
            end; // else, keep on to buffer until we are freed
            {$IFDEF DEBUG}
            // mostly for fun, document what error codes you get here
            // (the real reason is to find out if there's codes we'll get that are actual problems we should deal with)
            // see the man page for unix send(), |man 7 tcp|, and |man 7 ip| for descriptions of other possible codes
            if ((SocketError <> ESysEPipe) and // 32 = broken pipe (only on non-Linux; on Linux we set MSG_NOSIGNAL to prevent this)
                (SocketError <> ESysEConnReset) and // 104 = connection reset by peer
                (SocketError <> ESysETimedOut) and // 110 = connection timed out
                (SocketError <> ESysEHostUnreach) and // 113 = no route to host
                (SocketError <> ESysENoTTY) and // 25 = not a typewriter (inappropriate ioctl for device)
                (SocketError <> ESysENotConn) and // 107 = transport endpoint is not connected
                (SocketError <> ESysEAgain) and // from above
                (SocketError <> ESysEWouldBlock) and // from above
                (SocketError <> ESysEIntr)) then // from above
               raise ESocketError.Create(SocketError);
            {$IFDEF VERBOSE} Writeln('Disconnecting in response to the following socket error (number ', SocketError, ') while writing: ', StrError(SocketError)); {$ENDIF}
            {$ENDIF}
            Disconnect();
            // XXX somehow the server crashes in heaptrc if you get here
         end;
      end
      else
      begin
         if (not NeedCopy) then
            FreeMem(FPendingWrites, FPendingWritesLength);
         FPendingWrites := nil;
         FPendingWritesLength := 0;
      end;
   end
   else
   begin
      Assert(Len = 0);
      Assert(not Assigned(FPendingWrites));
      Assert(not NeedCopy);
   end;
end;


constructor TNetworkServer.Create();
begin
   inherited Create();
   fpFD_ZERO(FFileDescriptorSet);
   fpFD_ZERO(FWriteFileDescriptorSet);
end;

constructor TNetworkServer.Create(Port: Word);
begin
   Create();
   AddListener(Port);
end;

destructor TNetworkServer.Destroy();
begin
   Empty();
   inherited;
end;

function TNetworkServer.AddListener(const Port: Word): TListenerSocket;
begin
   Result := TListenerSocket.Create(Port);
   Result.OnNewConnection := @Self.HandleNewConnection;
   Add(Result);
end;

procedure TNetworkServer.Add(Socket: TBaseSocket);
var
   Item: PSocketListItem;
begin
   New(Item);
   Item^.Next := FList;
   Item^.Value := Socket;
   FList := Item;
   if (FMaxSocketNumber < Socket.FSocketNumber) then
      FMaxSocketNumber := Socket.FSocketNumber;
   fpFD_SET(Socket.FSocketNumber, FFileDescriptorSet);
   Socket.OnDisconnect := @Self.HandleDisconnect;
   if (Socket is TNetworkSocket) then
      (Socket as TNetworkSocket).OnOverflow := @Self.HandleOverflow;
end;

procedure TNetworkServer.Remove(Socket: TBaseSocket);
var
   Item: PSocketListItem;
   Last: ^PSocketListItem;
begin
   // this acts like the disconnect logic at the end of Select()
   fpFD_CLR(Socket.FSocketNumber, FFileDescriptorSet);
   fpFD_CLR(Socket.FSocketNumber, FWriteFileDescriptorSet);
   Last := @FList;
   Item := FList;
   while (Assigned(Item)) do
   begin
      if (Item^.Value = Socket) then
      begin
         Last^ := Item^.Next;
         Dispose(Item);
         Item := nil;
      end
      else
      begin
         Last := @Item^.Next;
         Item := Item^.Next;
      end;
   end;
end;

procedure TNetworkServer.Empty();
var
   Item: PSocketListItem;
begin
   while (Assigned(FList)) do
   begin
      Item := FList;
      FList := FList^.Next;
      Assert(Assigned(Item^.Value));
      Item^.Value.Disconnect();
      Item^.Value.Destroy();
      Dispose(Item);
   end;
   fpFD_ZERO(FFileDescriptorSet);
   fpFD_ZERO(FWriteFileDescriptorSet);
end;

procedure TNetworkServer.HandleNewConnection(ListenerSocket: TListenerSocket);
begin
   Add(CreateNetworkSocket(ListenerSocket));
end;

procedure TNetworkServer.HandleDisconnect(Socket: TBaseSocket);
begin
   FHavePendingDisconnects := True;
end;

procedure TNetworkServer.HandleOverflow(Socket: TNetworkSocket);
{$IFOPT C+}
var
   Item: PSocketListItem;
{$ENDIF}
begin
   fpFD_SET(Socket.FSocketNumber, FWriteFileDescriptorSet);
   {$IFOPT C+}
   Item := FList;
   while (Assigned(Item)) do
   begin
      if (Item^.Value = Socket) then
         Break;
      Item := Item^.Next;
   end;
   Assert(Assigned(Item));
   {$ENDIF}
end;

function TNetworkServer.Select(Timeout: cint): Boolean;
var
   fdsRead, fdsWrite, fdsExcept: TFDSet;
   MaxSetBitNumber, Pending, IsSet: cint;
   {$IFDEF DEBUG} OriginalPending: cint; {$ENDIF}
   Item: PSocketListItem;
   Last: ^PSocketListItem;
   Disconnect: Boolean;
begin
   Result := False;
   fdsRead := FFileDescriptorSet;
   fdsWrite := FWriteFileDescriptorSet;
   fdsExcept := FFileDescriptorSet;
   MaxSetBitNumber := FMaxSocketNumber+1; {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
   Pending := fpSelect(MaxSetBitNumber, @fdsRead, @fdsWrite, @fdsExcept, Timeout);
   if (Pending < 0) then
   begin
      case fpGetErrNo of
         ESysEIntr: Exit; { probably received ^C or an alarm }
      else
         raise EKernelError.Create(fpGetErrNo);
      end;
   end;
   if (Pending = 0) then
   begin
      Result := True;
      Exit;
   end;
   Item := FList;
   {$IFDEF DEBUG} OriginalPending := Pending; {$ENDIF}
   while ((Pending > 0) and Assigned(Item)) do
   begin
      Disconnect := False;
      // check if this socket was flagged for reading
      IsSet := fpFD_ISSET(Item^.Value.FSocketNumber, fdsRead);
      Assert(IsSet >= 0);
      if (IsSet > 0) then
      begin
         Dec(Pending);
         try
            if (not Item^.Value.Read()) then
               Disconnect := True;
         except
            on E: Exception do
            begin
               ReportException(E);
               Disconnect := True;
            end;
         end;
      end;
      // check if this socket was flagged for writing
      IsSet := fpFD_ISSET(Item^.Value.FSocketNumber, fdsWrite);
      Assert(IsSet >= 0);
      if (IsSet > 0) then
      begin
         Dec(Pending);
         fpFD_CLR(Item^.Value.FSocketNumber, FWriteFileDescriptorSet);
         Assert(Item^.Value is TNetworkSocket);
         (Item^.Value as TNetworkSocket).Write(); // can call its own Disconnect()
      end;
      // check if this socket was flagged for some sort of problem
      IsSet := fpFD_ISSET(Item^.Value.FSocketNumber, fdsExcept);
      Assert(IsSet >= 0);
      if (IsSet > 0) then
      begin
         Dec(Pending);
         Assert(Item^.Value is TNetworkSocket);
         Disconnect := True;
      end;
      // clean up and move on
      if (Disconnect) then
         Item^.Value.Disconnect();
      Item := Item^.Next;
   end;
   Assert(Pending = 0);
   if (FHavePendingDisconnects) then
   begin
      Last := @FList;
      Item := FList;
      while (Assigned(Item)) do
      begin
         if (not Item^.Value.Connected) then
         begin
            Assert(Last^ = Item);
            // act like Remove()
            fpFD_CLR(Item^.Value.FSocketNumber, FFileDescriptorSet);
            fpFD_CLR(Item^.Value.FSocketNumber, FWriteFileDescriptorSet);
            Assert(Assigned(Item^.Value));
            Item^.Value.Destroy();
            {$IFDEF C+} Item^.Value := nil; {$ENDIF}
            Last^ := Item^.Next;
            Dispose(Item);
            Item := Last^;
         end
         else
         begin
            Last := @Item^.Next;
            Item := Item^.Next;
         end;
      end;
      FHavePendingDisconnects := False;
   end;
end;

end.
