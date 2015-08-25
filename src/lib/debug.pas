{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
{$IFNDEF DEBUG} {$FATAL This unit should only be included in debug mode.} {$ENDIF}
unit debug;

interface

const
   HeapInfoSize = 16;

type
   PHeapInfo = ^THeapInfo;
   THeapInfo = String[HeapInfoSize];

procedure HexDump(var Data; const Size, SubSize: Cardinal);
function GetStackTrace(): AnsiString;

{$IFNDEF OPT}
function SetHeapInfo(S: THeapInfo): THeapInfo;
function SetHeapInfoTruncated(S: AnsiString): THeapInfo; // only adds the end of the string
{$ENDIF}

implementation

uses
{$WARNINGS OFF} // otherwise it warns that we should use -gl instead of "uses lineinfo", but we want visibility into the unit...
   {$IFNDEF OPT}
   heaptrc,
   lineinfo,
   {$ENDIF}
{$WARNINGS ON}
   sysutils;

procedure HexDump(var Data; const Size, SubSize: Cardinal);
var
   Index, LineIndex: Cardinal;
begin
   Index := 0;
   LineIndex := 0;
   while Index < Size do
   begin
      Write(IntToHex(PByte(Pointer(@Data) + Index)^, 2));
      Inc(Index);
      Inc(LineIndex);
      if ((LineIndex >= 80) or (Index mod SubSize = 0)) then
      begin
         Writeln();
         LineIndex := 0;
      end
      else
      if (LineIndex mod 8 = 0) then
      begin
         Write(' ');
      end;
   end;
end;

function GetStackTrace(): AnsiString;
// the following is a verbatim copy from http://wiki.freepascal.org/Logging_exceptions
var
  I: Longint;
  prevbp: Pointer;
  CallerFrame,
  CallerAddress,
  bp: Pointer;
  Report: string;
const
  MaxDepth = 20;
begin
  Report := '';
  bp := get_frame;
  // This trick skip SendCallstack item
  // bp:= get_caller_frame(get_frame);
  try
    prevbp := bp - 1;
    I := 0;
    while bp > prevbp do begin
       CallerAddress := get_caller_addr(bp);
       CallerFrame := get_caller_frame(bp);
       if (CallerAddress = nil) then
         Break;
       Report := Report + BackTraceStrFunc(CallerAddress) + LineEnding;
       Inc(I);
       if (I >= MaxDepth) or (CallerFrame = nil) then
         Break;
       prevbp := bp;
       bp := CallerFrame;
     end;
   except
     { prevent endless dump if an exception occured }
   end;
   // end of copy from http://wiki.freepascal.org/Logging_exceptions
   Result := Report;
end;

{$IFNDEF OPT}
var
   CurrentHeapInfo: THeapInfo = '';
   ReportAllocations: Boolean = False;

function SetHeapInfo(S: THeapInfo): THeapInfo;
begin
   Result := CurrentHeapInfo;
   CurrentHeapInfo := S;
end;

function SetHeapInfoTruncated(S: AnsiString): THeapInfo;
begin
   Result := CurrentHeapInfo;
   if (Length(S) > HeapInfoSize) then
      CurrentHeapInfo := Copy(S, Length(S)-HeapInfoSize+1, HeapInfoSize)
   else
      CurrentHeapInfo := S;
end;

procedure HeapInfoFiller(P: Pointer);
var
   FunctionName, SourceFile: ShortString;
   LineNumber: Longint;
   Frame: Pointer;
begin
   if (ReportAllocations) then
   begin
      ReportAllocations := False;
      FunctionName := '';
      SourceFile := '';
      LineNumber := 0;
      Frame := Get_Frame;
      while (Assigned(Frame) and
             {$HINTS OFF} GetLineInfo(PtrUInt(Get_Caller_Addr(Frame)), {$HINTS ON} FunctionName, SourceFile, LineNumber) and 
             ((SourceFile = '') or (SourceFile = 'lib/heaptrc.pp') or (SourceFile = 'lib/debug.pas'))) do
         Frame := Get_Caller_Frame(Frame);
      if (SourceFile <> '') then
      begin
         if (FunctionName <> '') then
            Writeln('Allocating memory for ', FunctionName, '() in ', SourceFile, ' line ', LineNumber, '; ', CurrentHeapInfo)
         else
            Writeln('Allocating memory for ', SourceFile, ' line ', LineNumber, '; ', CurrentHeapInfo);
      end;
      ReportAllocations := True;
   end;
   Assert(Assigned(P));
   PHeapInfo(P)^ := CurrentHeapInfo;
end;

procedure HeapInfoDisplayer(var PText: Text; P: Pointer);
begin
   if (PHeapInfo(P)^ <> '') then
   begin
      Writeln(PText, 'Debug log: ', PHeapInfo(P)^); // http://bugs.freepascal.org/view.php?id=25916
      Writeln(PText);
   end;
end;

initialization
   Assert(SizeOf(THeapInfo) = HeapInfoSize+1);
   SetHeapInfo('initialization');
   SetHeapExtraInfo(SizeOf(THeapInfo), @HeapInfoFiller, @HeapInfoDisplayer);
   //QuickTrace := False; { ridiculously slow }
   //ReportAllocations := True; { ridiculously verbose }
   //KeepReleased := True;
finalization
   SetHeapInfo('finalisation');
{$ENDIF}
end.
