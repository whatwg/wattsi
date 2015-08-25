{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit unixutils;

interface

var
   Aborted: Boolean = False;

procedure HookSignalHandlers();

implementation

uses
   sysutils, baseunix, exceptions;

procedure SigIntHandler(Signal: Longint; Info: PSigInfo; Context: PSigContext); cdecl;
begin
   {$IFDEF DEBUG}
     Writeln('caught ^C; aborting at:');
     Writeln(GetStackTrace());
     Writeln();
   {$ENDIF}
   Aborted := True;
end;

{$IFNDEF Linux}
procedure SigPipeHandler(Signal: Longint; Info: PSigInfo; Context: PSigContext); cdecl;
begin
   {$IFDEF DEBUG}
     Writeln();
     Writeln('caught SIGPIPE');
     Writeln(GetStackTrace());
     Writeln();
   {$ENDIF}
end;
{$ENDIF}

procedure HookSignalHandlers();
var
   NewAction: PSigActionRec;
begin
   New(NewAction);
   if (not Assigned(NewAction)) then
      OutOfMemoryError();
   try
      {$IFDEF Linux} NewAction^.sa_restorer := nil; {$ENDIF}
      FillByte(NewAction^.sa_mask, SizeOf(NewAction^.sa_mask), 0);

      // SIGINT - one-off handler
      NewAction^.sa_handler := @SigIntHandler;
      NewAction^.sa_flags := SA_ONESHOT;
      if (fpSigAction(SIGINT, NewAction, nil) <> 0) then
         raise EKernelError.Create(fpGetErrNo);

      {$IFNDEF Linux}
      // SIGPIPE - persistent handler
      // On Linux we use MSG_NOSIGNAL on send() instead -- see corenetwork.pas
      NewAction^.sa_handler := @SigPipeHandler;
      NewAction^.sa_flags := SA_RESTART;
      if (fpSigAction(SIGPIPE, NewAction, nil) <> 0) then
         raise EKernelError.Create(fpGetErrNo);
      {$ENDIF}

   finally
      Dispose(NewAction);
   end;
end;

end.