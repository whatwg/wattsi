{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit corewebsocket;

interface

uses
   corenetwork, sysutils;

//{$DEFINE WEBSOCKET_VERBOSE}

type
   TWebSocket = class(TNetworkSocket)
    protected
     type
      TWebSocketState = (wsRequestLine,
                         wsFieldTrailingEnd, wsFieldEnd, wsFieldNameStart, wsFieldName, wsFieldSeparator, wsFieldValue, wsHandshakeEnd,
                         wsFrameByte1, wsFrameByte2, wsFrameExtendedLength16, wsFrameExtendedLength64, wsFrameMask, wsFramePayload,
                         wsError);
      TWebSocketFrameType = (ftContinuation := $00, ftText := $01, ftBinary := $02, ftClose := $08, ftPing := $09, ftPong := $0A);
      TWebSocketFrame = record
       FrameType: TWebSocketFrameType;
       FinalFrame: Boolean;
       MaskKey: array[0..3] of Byte;
       Length, Index: Cardinal;
       Data: RawByteString;
      end;
     var
      FState: TWebSocketState;
      FCanWriteFrames: Boolean;
      FCurrentFrame: TWebSocketFrame;
      FBufferType: TWebSocketFrameType;
      FBuffer: RawByteString;
      FCurrentFieldName, FCurrentFieldValue, FHandshakeKey: RawByteString;
      function InternalRead(Data: array of Byte): Boolean; override;
      procedure CheckField(); virtual;
      procedure Handshake(); virtual;
      procedure ProcessFrame();
    // API for subclasses:
      procedure HandleMessage(s: RawByteString); virtual; abstract;
      procedure WriteFrame(s: RawByteString); {$IFDEF DEBUG} virtual; {$ENDIF}
    public
      constructor Create(Listener: TListenerSocket);
      property Ready: Boolean read FCanWriteFrames;
    end;

implementation

uses
   sha1, base64encoder;

constructor TWebSocket.Create(Listener: TListenerSocket);
begin
   inherited Create(Listener);
   FState := wsRequestLine;
end;

function TWebSocket.InternalRead(Data: array of Byte): Boolean;
var
   Index: Cardinal;
   c: Byte;
begin
   Result := True;
   Assert(Length(Data) > 0);
   for Index := 0 to Length(Data)-1 do {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
   begin
      c := Data[Index];
      case FState of
        wsRequestLine, wsFieldTrailingEnd:
         case c of
          $0D: FState := wsFieldEnd;
         end;
        wsFieldEnd:
         case c of
          $0A: FState := wsFieldNameStart;
          else
             FState := wsError;
             Result := False;
             Exit;
         end;
        wsFieldNameStart:
         case c of
          $0D: FState := wsHandshakeEnd;
          Ord(':'): FState := wsFieldTrailingEnd;
          else
             FCurrentFieldName := Chr(c);
             FState := wsFieldName;
         end;
        wsFieldName:
         case c of
          Ord(':'): FState := wsFieldSeparator;
          $0D: FState := wsFieldEnd;
          else
             FCurrentFieldName := FCurrentFieldName + Chr(c);
         end;
        wsFieldSeparator:
         case c of
          Ord(' '): begin FState := wsFieldValue; FCurrentFieldValue := ''; end;
          $0D: FState := wsFieldEnd;
          else
             FState := wsError;
             Result := False;
             Exit;
         end;
        wsFieldValue:
         case c of
          $0D: begin FState := wsFieldEnd; CheckField(); end;
          else
             FCurrentFieldValue := FCurrentFieldValue + Chr(c);
         end;
        wsHandshakeEnd:
         case c of
          $0A: begin FState := wsFrameByte1; Handshake(); end;
          else
             FState := wsError;
             Result := False;
             Exit;
         end;
        wsFrameByte1:
         begin
            FCurrentFrame.FrameType := TWebSocketFrameType(c and $0F);
            // assume bits 5, 6, and 7 are zero (extension bits, we don't negotiate any extensions)
            FCurrentFrame.FinalFrame := (c and $80) = $80;
            FState := wsFrameByte2;
         end;
        wsFrameByte2:
         begin
            FCurrentFrame.Length := (c and $7F);
            // assume bit 8 is set (masking bit, client must mask)
            FCurrentFrame.Index := 0;
            case (FCurrentFrame.Length) of
             0..125: FState := wsFrameMask;
             126: begin FCurrentFrame.Length := 0; FState := wsFrameExtendedLength16; end;
             127: begin FCurrentFrame.Length := 0; FState := wsFrameExtendedLength64; end;
             else Assert(False);
            end;
         end;
        wsFrameExtendedLength16, wsFrameExtendedLength64:
         begin
            FCurrentFrame.Length := FCurrentFrame.Length or (c shl (FCurrentFrame.Index * 8));
            Inc(FCurrentFrame.Index);
            if (((FState = wsFrameExtendedLength16) and (FCurrentFrame.Index = 2)) or
                ((FState = wsFrameExtendedLength64) and (FCurrentFrame.Index = 8))) then
            begin
               FCurrentFrame.Index := 0;
               FState := wsFrameMask;
            end;
         end;
        wsFrameMask:
         begin
            FCurrentFrame.MaskKey[FCurrentFrame.Index] := c;
            Inc(FCurrentFrame.Index);
            if (FCurrentFrame.Index = 4) then
            begin
               if (FCurrentFrame.Length > 1024) then
               begin
                  FState := wsError;
                  Result := False;
                  Exit;
               end
               else
               begin
                  SetLength(FCurrentFrame.Data, FCurrentFrame.Length);
                  if (FCurrentFrame.Length > 0) then
                  begin
                     FCurrentFrame.Index := 1;
                     FState := wsFramePayload;
                  end
                  else
                  begin
                     ProcessFrame();
                     FState := wsFrameByte1;
                  end;
               end;
            end;
         end;
        wsFramePayload:
         begin
            FCurrentFrame.Data[FCurrentFrame.Index] := Chr(c xor FCurrentFrame.MaskKey[(FCurrentFrame.Index-1) mod 4]);
            Inc(FCurrentFrame.Index);
            if (FCurrentFrame.Index > FCurrentFrame.Length) then
            begin
               ProcessFrame();
               FState := wsFrameByte1;
            end;
         end;
        else Assert(False);
      end;
   end;
end;

procedure TWebSocket.CheckField();
begin
   if (FCurrentFieldName = 'Sec-WebSocket-Key') then
      FHandshakeKey := FCurrentFieldValue
   { ... Host, Origin, Sec-WebSocket-Protocol, ... }
end;

procedure TWebSocket.Handshake();
var
   Challenge, Response: RawByteString;
   Digest: TSHA1Digest;
   Index: Cardinal;
begin
   Challenge := FHandshakeKey + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
   Digest := SHA1String(Challenge);
   Response := '';
   for Index := Low(Digest) to High(Digest) do
      Response := Response + Chr(Digest[Index]);
   Response := Base64(Response);
   Write('HTTP/1.1 101 WebSocket Protocol Handshake'#13#10);
   Write('Upgrade: WebSocket'#13#10);
   Write('Connection: Upgrade'#13#10);
   Write('Sec-WebSocket-Accept: ' + Response + #13#10);
   Write(#13#10);
   FCanWriteFrames := True;
end;

procedure TWebSocket.ProcessFrame();
begin
   case (FCurrentFrame.FrameType) of
    ftContinuation:
     begin
        FBuffer := FBuffer + FCurrentFrame.Data;
        if ((FCurrentFrame.FinalFrame) and (FBufferType = ftText)) then
        begin
           HandleMessage(FBuffer);
           FBuffer := '';
        end;
     end;
    ftText:
     begin
        FBuffer := FCurrentFrame.Data;
        FBufferType := ftText;
        if (FCurrentFrame.FinalFrame) then
        begin
           HandleMessage(FBuffer);
           FBuffer := '';
        end;
     end;
    ftBinary:
     begin
        // we don't support binary frames
        FBuffer := '';
        FBufferType := ftBinary;
     end;
    ftClose:
     begin
        // we don't support this per spec, just close the connection
        Disconnect();
     end;
    ftPing, ftPong:
     begin
        // we don't support ping frames
     end;
   end;
end;

procedure TWebSocket.WriteFrame(s: RawByteString);
begin
   {$IFDEF WEBSOCKET_VERBOSE} Writeln('Sending: ', s); {$ENDIF}
   Assert(FCanWriteFrames);
   Write([$81]); // unfragmented text frame
   if (Length(s) > 65536) then
      Write([127,
             Byte((Length(s) shr 8*7) and $FF),
             Byte((Length(s) shr 8*6) and $FF),
             Byte((Length(s) shr 8*5) and $FF),
             Byte((Length(s) shr 8*4) and $FF),
             Byte((Length(s) shr 8*3) and $FF),
             Byte((Length(s) shr 8*2) and $FF),
             Byte((Length(s) shr 8*1) and $FF),
             Byte((Length(s)        ) and $FF)])
   else
   if (Length(s) > 126) then
      Write([126,
             Byte((Length(s) shr 8*1) and $FF),
             Byte((Length(s)        ) and $FF)])
   else
      Write([Byte(Length(s))]);
   if (Length(s) > 0) then
      Write(s);
end;

end.
