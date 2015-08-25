{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit exceptions;

interface

uses
   sysutils, baseunix;

type
   EKernelError = class(Exception)
      constructor Create(AErrorCode: cint);
   end;
   ESocketError = class(Exception)
      constructor Create(AErrorCode: cint);
   end;
   ESyntaxError = class(Exception)
   end;
   ECaughtException = class end;

   TReportExceptionEvent = procedure(E: Exception) of object;

procedure ReportCurrentException();
procedure ReportException(E: Exception);
procedure ReportExceptionAndFree(E: Exception); { for unraised exceptions }
function SetReportExceptionMethod(AReportExceptionMethod: TReportExceptionEvent): TReportExceptionEvent;

type
   TXXX = record end unimplemented;

function XXX: Variant; unimplemented;

implementation

uses
   errors;

const
   KernelErrorMsg: String = 'kernel error %d: %s';
   SocketErrorMsg: String = 'socket error %d: %s';

var
   ReportExceptionMethod: TReportExceptionEvent = nil;

constructor EKernelError.Create(AErrorCode: cint);
begin
   inherited Create(Format(KernelErrorMsg, [AErrorCode, StrError(AErrorCode)]));
end;

constructor ESocketError.Create(AErrorCode: cint);
begin
   inherited Create(Format(SocketErrorMsg, [AErrorCode, StrError(AErrorCode)]));
end;

procedure WriteBacktrace(Address: Pointer; Frames: PPointer; FrameCount: Integer);
var
   FrameNumber: Cardinal;
begin
   Writeln('Backtrace:');
   if (Address = nil) then
      Writeln('  dereferenced nil pointer')
   else
      Writeln(BackTraceStrFunc(Address));
   if (FrameCount > 0) then
      for FrameNumber := 0 to FrameCount-1 do {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
         Writeln(BackTraceStrFunc(Frames[FrameNumber]));
end;

procedure ReportCurrentException();
begin
   Assert(Assigned(RaiseList));
   Assert(Assigned(RaiseList^.FObject));
   if (RaiseList^.FObject is Exception) then
      Writeln(RaiseList^.FObject.ClassName, ' exception: ', (RaiseList^.FObject as Exception).Message)
   else
      Writeln(RaiseList^.FObject.ClassName, ' exception');
   WriteBacktrace(RaiseList^.Addr, RaiseList^.Frames, RaiseList^.FrameCount);
end;

procedure ReportException(E: Exception);
begin
   if (Assigned(ReportExceptionMethod)) then
      ReportExceptionMethod(E)
   else
   begin
      Writeln(E.Message);
      Dump_Stack(Output, Get_Frame);
   end;
end;

procedure ReportExceptionAndFree(E: Exception);
begin
   ReportException(E);
   E.Free();
end;

function SetReportExceptionMethod(AReportExceptionMethod: TReportExceptionEvent): TReportExceptionEvent;
begin
   Result := ReportExceptionMethod;
   ReportExceptionMethod := AReportExceptionMethod;
end;

{$WARNINGS OFF}
function XXX: Variant;
begin
   //Assert(False, 'Not Implemented');
   raise Exception.Create('Not Implemented') at get_caller_addr(get_frame), get_caller_frame(get_frame);  
end;
{$WARNINGS ON}

procedure AssertionHandler(const Message, FileName: ShortString; LineNo: Longint; ErrorAddr: Pointer);
var
   CompleteMessage: AnsiString;
begin
   if (Message <> '') then
      CompleteMessage := 'Assertion "' + Message + '" failed on line ' + IntToStr(LineNo) + ' of ' + FileName
   else
      CompleteMessage := 'Assertion failed on line ' + IntToStr(LineNo) + ' of ' + FileName;
   {$IFDEF DEBUG}
   Writeln('Raising assertion: ', CompleteMessage);
   {$ENDIF}
   raise EAssertionFailed.Create(CompleteMessage) at Get_Caller_Addr(ErrorAddr), Get_Caller_Frame(ErrorAddr);
end;

initialization
   {$IFDEF DEBUG} AssertErrorProc := @AssertionHandler; {$ENDIF}
end.
