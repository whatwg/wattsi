{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit rtlutils;

interface

function GetRefCount(constref S: UTF8String): SizeInt; inline;

{$IFOPT C+} procedure AssertStringIsConstant(constref S: UTF8String); {$ENDIF}
{$IFOPT C+} procedure AssertStringIsReffed(constref S: UTF8String; const MinRef: Cardinal); {$ENDIF}

implementation

type
   PAnsiRec = ^TAnsiRec;
   TAnsiRec = record
      // based on TAnsiRec in astrings.inc
      CodePage: TSystemCodePage;
      ElementSize: Word;
      Dummy: DWord;
      RefCount: SizeInt;
      Length: SizeInt;
      Data: record end;
   end;

function GetRefCount(constref S: UTF8String): SizeInt;
var
   StringStart: PAnsiRec;
begin
   if (S <> '') then
   begin
      StringStart := PAnsiRec(Pointer(S)-SizeOf(TAnsiRec));
      Result := StringStart^.RefCount;
   end
   else
      Result := 1;
end;

{$IFOPT C+}
procedure AssertStringIsConstant(constref S: UTF8String);
var
   StringStart: PAnsiRec;
begin
   if (S <> '') then
   begin
      StringStart := PAnsiRec(Pointer(S)-SizeOf(TAnsiRec));
      Assert(StringStart^.RefCount = -1);
   end;
end;
{$ENDIF}

{$IFOPT C+}
procedure AssertStringIsReffed(constref S: UTF8String; const MinRef: Cardinal);
var
   StringStart: PAnsiRec;
begin
   if (S <> '') then
   begin
      StringStart := PAnsiRec(Pointer(S)-SizeOf(TAnsiRec));
      Assert(StringStart^.RefCount >= MinRef);
   end;
end;
{$ENDIF}

end.