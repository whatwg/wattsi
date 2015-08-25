{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit stringutils;

// This unit is intended to be a drop-in replacement for the ropes
// unit, but using straight strings instead of ropes

interface

//{$DEFINE TESTS}

uses
   unicode, utf8, genericutils;

type
   TUTF8StringPointer = record
    private
      {$IFOPT C+} FData: PUTF8String; {$ENDIF}
      FIndex: Cardinal;
      FLength: TZeroableUTF8SequenceLength;
    public
      {$IFOPT C+} function IsZeroWidth(): Boolean; {$ENDIF}
      {$IFOPT C+} function IsEOF(): Boolean; {$ENDIF}
      {$IFOPT C+} function GetByte(): Byte; {$ENDIF}
      {$IFOPT C+} procedure AssertIdentity(const Data: PUTF8String); {$ENDIF}
      procedure AdvanceToAfter(); inline; // changes to a zero-width pointer; only valid if FLength > 0
      procedure AdvanceToAfter(const Character: TUnicodeCodepoint); inline; // changes to a zero-width pointer
      function AdvanceToNext(constref Data: UTF8String): TUnicodeCodepoint;
      procedure SetToZeroWidth(); inline; // changes to a zero-width pointer
   end;
   operator = (constref Op1, Op2: TUTF8StringPointer): Boolean;
   operator < (constref Op1, Op2: TUTF8StringPointer): Boolean;
   operator <= (constref Op1, Op2: TUTF8StringPointer): Boolean;

type
   UTF8StringEnumerator = class
    private
      FTarget: PUTF8String;
      FPosition: Cardinal;
      FAdvance: TZeroableUTF8SequenceLength;
      {$IFOPT C+} FDidRead: Boolean; {$ENDIF}
      function GetCurrent(): TUnicodeCodepoint; inline;
      function GetAdvance(): TUTF8SequenceLength; inline;
    public
      constructor Create(const NewTarget: PUTF8String);
      {$IFOPT C+} procedure AssertIdentity(const Target: PUTF8String); {$ENDIF}
      function MoveNext(): Boolean; inline; // you must call this first, and never twice in a row without an intervening call to Current or ReturnToPointer()
      property Current: TUnicodeCodepoint read GetCurrent; // you must only call this once after each call to MoveNext() or ReturnToPointer()
      function GetPointer(): TUTF8StringPointer; inline; // you must call this after first calling Current or ReturnToPointer()
      procedure ReturnToPointer(constref NewPosition: TUTF8StringPointer); // after this call, you must call MoveNext() or GetPointer()
      property CurrentLength: TUTF8SequenceLength read GetAdvance; // only valid after Current or GetPointer()/ReturnToPointer()
      function GetCurrentAsUTF8(): UTF8String; inline; // when CurrentLength is valid (note: expensive)
      function GetRawPointer(): Pointer; inline; // only valid when CurrentLength is valid; do not read more than CurrentLength bytes from this point
   end;

   CutUTF8String = record
    private
      FValue: UTF8String;
      function GetIsEmpty(): Boolean; inline;
      function GetAsString(): UTF8String; inline;
    public
      class function CreateFrom(const NewString: TUnicodeCodepointArray): CutUTF8String; static;
      class function CreateFrom(const NewString: UTF8String): CutUTF8String; static;
      function GetAsStringSlow(): UTF8String; inline;
      property IsEmpty: Boolean read GetIsEmpty;
      property AsString: UTF8String read GetAsString; // destroys self
   end;

   UTF8StringHelper = type helper for UTF8String
    private
      function GetLength(): Cardinal; inline;
      function GetIsEmpty(): Boolean; inline;
      function GetAsString(): UTF8String;
    public
      procedure AppendDestructively(var NewString: CutUTF8String); // destroys argument
      procedure AppendDestructively(var NewString: UTF8String); // destroys argument
      procedure Append(const NewString: PUTF8String);
      procedure Append(const NewString: TUnicodeCodepoint); deprecated 'highly inefficient - use a ShortString instead if possible';
      procedure Append(const NewString: TUnicodeCodepointArray);
      procedure Append(const NewData: Pointer; const NewLength: QWord);
      function Extract(constref NewStart, NewEnd: TUTF8StringPointer): CutUTF8String;
        // includes NewEnd if it is non-zero-width, excludes otherwise
        // this does a memory copy into a new string
      function CountCharacters(constref NewStart, NewEnd: TUTF8StringPointer): Cardinal;
        // includes NewEnd if it is non-zero-width, excludes otherwise
        // note: this one is expensive (it iterates over the string, parsing UTF8)
      procedure InplaceReplace(const Character: TUnicodeCodepoint; var Position: TUTF8StringPointer); // changes Position to a zero-width pointer
      function GetEnumerator(): UTF8StringEnumerator; inline; // for some reason this doesn't get seen by for-in loops
      property Length: Cardinal read GetLength;
      property IsEmpty: Boolean read GetIsEmpty;
      property AsString: UTF8String read GetAsString;
   end;
   operator enumerator (var Value: UTF8String): UTF8StringEnumerator; inline; // needed for for-in loops

type
   UTF8StringUtils = specialize DefaultUtils <UTF8String>;

function CodepointArrayToUTF8String(const Value: TUnicodeCodepointArray): UTF8String;
// there's no UTF8StringToCodepointArray -- use the enumerator

implementation

uses
   sysutils;

{$IFOPT C+}
function TUTF8StringPointer.IsZeroWidth(): Boolean;
begin
   Result := FLength = 0;
end;
{$ENDIF}

{$IFOPT C+}
function TUTF8StringPointer.IsEOF(): Boolean;
begin
   Result := (FLength = 0) and (FIndex = Length(FData^)+1);
end;
{$ENDIF}

{$IFOPT C+}
function TUTF8StringPointer.GetByte(): Byte;
begin
   Assert(FIndex > 0);
   Assert(FLength > 0);
   Assert(FIndex <= Length(FData^));
   Result := Ord(FData^[FIndex]);
end;
{$ENDIF}

{$IFOPT C+}
procedure TUTF8StringPointer.AssertIdentity(const Data: PUTF8String);
begin
   Assert(FData = Data);
end;
{$ENDIF}

procedure TUTF8StringPointer.AdvanceToAfter();
begin
   Assert(FLength <> 0);
   Inc(FIndex, FLength);
   FLength := 0;
end;

procedure TUTF8StringPointer.AdvanceToAfter(const Character: TUnicodeCodepoint);
{$IFOPT C+}
var
   CheckedCodepoint: TUnicodeCodepoint;
   CheckedLength: TUTF8SequenceLength;
{$ENDIF}
begin
   {$IFOPT C+}
   CheckedCodepoint := UTF8ToCodepoint(FData, FIndex, CheckedLength);
   Assert(CheckedCodepoint = Character);
   if (FLength > 0) then
      Assert(CheckedLength = FLength);
   {$ENDIF}
   Inc(FIndex, CodepointToUTF8Length(Character));
   FLength := 0;
end;

function TUTF8StringPointer.AdvanceToNext(constref Data: UTF8String): TUnicodeCodepoint;
{$IFOPT C+}
var
   CheckedLength: TUTF8SequenceLength;
{$ENDIF}
begin
   {$IFOPT C+} Assert(@Data = FData); {$ENDIF}
   Assert(FLength > 0);
   {$IFOPT C+}
     UTF8ToCodepoint(@Data, FIndex, CheckedLength);
     Assert(CheckedLength = FLength);
   {$ENDIF}
   Inc(FIndex, FLength);
   Assert(FIndex <= Length(Data));
   Result := UTF8ToCodepoint(@Data, FIndex, TUTF8SequenceLength(FLength));
   Assert(FIndex + FLength - 1 <= Length(Data));
end;

procedure TUTF8StringPointer.SetToZeroWidth();
begin
   Assert(FLength <> 0);
   FLength := 0;
end;

operator = (constref Op1, Op2: TUTF8StringPointer): Boolean;
begin
   {$IFOPT C+} Assert(Op1.FData = Op2.FData); {$ENDIF}
   Result := (Op1.FIndex = Op2.FIndex) and (Op1.FLength = Op2.Flength);
end;

operator < (constref Op1, Op2: TUTF8StringPointer): Boolean;
begin
   {$IFOPT C+} Assert(Op1.FData = Op2.FData); {$ENDIF}
   Result := (Op1.FIndex < Op2.FIndex) or
             ((Op1.FIndex = Op2.FIndex) and (Op1.FLength < Op2.FLength));
end;

operator <= (constref Op1, Op2: TUTF8StringPointer): Boolean;
begin
   {$IFOPT C+} Assert(Op1.FData = Op2.FData); {$ENDIF}
   Result := (Op1.FIndex < Op2.FIndex) or
             ((Op1.FIndex = Op2.FIndex) and (Op1.FLength <= Op2.FLength));
end;

constructor UTF8StringEnumerator.Create(const NewTarget: PUTF8String);
begin
   Assert(Assigned(NewTarget));
   FTarget := NewTarget;
   FPosition := 0;
   FAdvance := 1;
   {$IFOPT C+} FDidRead := True; {$ENDIF}
end;

{$IFOPT C+}
procedure UTF8StringEnumerator.AssertIdentity(const Target: PUTF8String);
begin
   Assert(FTarget = Target);
end;
{$ENDIF}

function UTF8StringEnumerator.MoveNext(): Boolean;
begin
   {$IFDEF VERBOSE} Writeln(' - MoveNext()'); {$ENDIF}
   {$IFOPT C+} Assert(FDidRead); {$ENDIF}
   Assert(FAdvance > 0);
   Inc(FPosition, FAdvance);
   Result := FPosition <= Length(FTarget^);
   {$IFOPT C+}
   if (not Result) then
      FAdvance := High(FAdvance);
   {$ENDIF}
   {$IFOPT C+} FDidRead := not Result; {$ENDIF}
end;

function UTF8StringEnumerator.GetCurrent(): TUnicodeCodepoint;
begin
   {$IFOPT C+} Assert(not FDidRead); {$ENDIF}
   Assert(FAdvance > 0);
   Result := UTF8ToCodepoint(FTarget, FPosition, TUTF8SequenceLength(FAdvance));
   {$IFOPT C+} FDidRead := True; {$ENDIF}
   {$IFDEF VERBOSE} Writeln(' - GetCurrent(): ', CodepointToUTF8(Result).AsString); {$ENDIF}
end;

function UTF8StringEnumerator.GetPointer(): TUTF8StringPointer;
begin
   {$IFDEF VERBOSE} Writeln(' - GetPointer()'); {$ENDIF}
   {$IFOPT C+} Assert(FDidRead); {$ENDIF}
   Assert(FPosition > 0);
   {$IFOPT C+} Result.FData := FTarget; {$ENDIF}
   Result.FIndex := FPosition;
   if (FPosition <= Length(FTarget^)) then
      Result.FLength := FAdvance
   else
      Result.FLength := 0;      
end;

procedure UTF8StringEnumerator.ReturnToPointer(constref NewPosition: TUTF8StringPointer);
begin
   {$IFDEF VERBOSE} Writeln(' - ReturnToPointer()'); {$ENDIF}
   Assert(NewPosition.FIndex > 0);
   {$IFOPT C+} Assert((not NewPosition.IsZeroWidth()) or (NewPosition.IsEOF())); {$ENDIF}
   {$IFOPT C+} Assert(NewPosition.FData = FTarget); {$ENDIF}
   FPosition := NewPosition.FIndex;
   FAdvance := NewPosition.FLength; // $R-
   {$IFOPT C+} FDidRead := True; {$ENDIF}
end;

function UTF8StringEnumerator.GetRawPointer(): Pointer; inline;
begin
   Result := @FTarget^[FPosition];
end;

function UTF8StringEnumerator.GetAdvance(): TUTF8SequenceLength;
begin
   Assert(FAdvance > 0);
   {$IFOPT C+} Assert(FDidRead); {$ENDIF}
   Result := TUTF8SequenceLength(FAdvance);
end;

function UTF8StringEnumerator.GetCurrentAsUTF8(): UTF8String;
begin
   Assert(FAdvance > 0);
   Result := Copy(FTarget^, FPosition, FAdvance);
end;


class function CutUTF8String.CreateFrom(const NewString: TUnicodeCodepointArray): CutUTF8String;
begin
   Result.FValue := CodepointArrayToUTF8String(NewString);
end;

class function CutUTF8String.CreateFrom(const NewString: UTF8String): CutUTF8String;
begin
   Result.FValue := NewString;
end;

function CutUTF8String.GetIsEmpty(): Boolean;
begin
   Result := Length(FValue) = 0;
end;

function CutUTF8String.GetAsString(): UTF8String;
begin
   Result := FValue;
   {$IFOPT C+} FValue := ''; {$ENDIF}
end; 

function CutUTF8String.GetAsStringSlow(): UTF8String;
begin
   Result := FValue;
end; 


function UTF8StringHelper.GetLength(): Cardinal;
begin
   Result := System.Length(Self); // $R-
end;

function UTF8StringHelper.GetIsEmpty(): Boolean;
begin
   Result := Self = '';
end;

function UTF8StringHelper.GetAsString(): UTF8String;
begin
   Result := Self;
end;

procedure UTF8StringHelper.AppendDestructively(var NewString: CutUTF8String);
begin
   Self := Self + NewString.FValue;
   {$IFOPT C+} NewString.FValue := ''; {$ENDIF}
end;

procedure UTF8StringHelper.AppendDestructively(var NewString: UTF8String);
begin
   Self := Self + NewString;
   {$IFOPT C+} NewString := ''; {$ENDIF}
end;

procedure UTF8StringHelper.Append(const NewString: PUTF8String);
begin
   Self := Self + NewString^;
end;

procedure UTF8StringHelper.Append(const NewString: TUnicodeCodepoint);
begin
   Self := Self + CodepointToUTF8(NewString);
end;

procedure UTF8StringHelper.Append(const NewString: TUnicodeCodepointArray);
var
   Index, NewLength, NewOffset: Cardinal;
   NewStringSegment: TUTF8Sequence;
begin
   Assert(System.Length(NewString) > 0);
   NewLength := System.Length(Self); // $R-
   Assert(NewLength < High(NewOffset)); // Length() return an Integer, so this is true
   NewOffset := NewLength+1; // $R-
   for Index := Low(NewString) to High(NewString) do // $R-
      Inc(NewLength, CodepointToUTF8Length(NewString[Index]));
   SetLength(Self, NewLength);
   for Index := Low(NewString) to High(NewString) do // $R-
   begin
      NewStringSegment := CodepointToUTF8(NewString[Index]);
      Move(NewStringSegment.Start, Self[NewOffset], NewStringSegment.Length);
      Inc(NewOffset, NewStringSegment.Length);
   end;
   Assert(NewOffset = System.Length(Self)+1);
end;

procedure UTF8StringHelper.Append(const NewData: Pointer; const NewLength: QWord);
var
   Index: Cardinal;
begin
   Index := System.Length(Self); // $R-
   if ((NewLength > High(System.Length(Self))) or (NewLength > High(System.Length(Self))-Index)) then
      raise Exception.Create('too much data');
   SetLength(Self, Index+NewLength);
   Move(NewData^, Self[Index+1], NewLength); // $R-
end;

function UTF8StringHelper.Extract(constref NewStart, NewEnd: TUTF8StringPointer): CutUTF8String;
var
   EffectiveLength: Integer;
begin
   {$IFOPT C+}
   Assert(NewStart.FData = @Self);
   Assert(NewEnd.FData = @Self);
   {$ENDIF}
   Assert(NewStart.FIndex <= System.Length(Self));
   Assert(NewStart.FIndex > 0);
   Assert(NewEnd.FIndex + NewEnd.FLength <= System.Length(Self)+1);
   Assert(NewEnd.FIndex + NewEnd.FLength > 0);
   Assert(NewStart.FIndex <= NewEnd.FIndex);
   EffectiveLength := NewEnd.FIndex + NewEnd.FLength - NewStart.FIndex; // $R-
   Assert(EffectiveLength >= 0);
   if (EffectiveLength > 0) then
      Result.FValue := Copy(Self, NewStart.FIndex, EffectiveLength)
   else
      Result.FValue := '';
end;

function UTF8StringHelper.CountCharacters(constref NewStart, NewEnd: TUTF8StringPointer): Cardinal;
var
   Index, EndIndex: Cardinal;
begin
   Index := NewStart.FIndex;
   Assert(NewEnd.FIndex + NewEnd.FLength < High(EndIndex));
   EndIndex := NewEnd.FIndex + NewEnd.FLength; // $R-
   Result := 0;
   while (Index < EndIndex) do
   begin
      Inc(Result);
      Inc(Index, UTF8ToUTF8Length(@Self, Index));
   end;
   Assert(Index = EndIndex);
end;

procedure UTF8StringHelper.InplaceReplace(const Character: TUnicodeCodepoint; var Position: TUTF8StringPointer);
var
   Value: TUTF8Sequence;
begin
   {$IFOPT C+} Assert(Position.FData = @Self); {$ENDIF}
   Assert(Position.FIndex <= System.Length(Self));
   Assert(Position.FIndex > 0);
   Value := CodepointToUTF8(Character);
   Assert(Position.FIndex + Value.Length <= System.Length(Self));
   Move(Value.Start, Self[Position.FIndex], Value.Length);
   Inc(Position.FIndex, Value.Length);
   Position.FLength := 0;
end;

function UTF8StringHelper.GetEnumerator(): UTF8StringEnumerator; inline;
begin
   Result := UTF8StringEnumerator.Create(@Self);
end;

operator enumerator (var Value: UTF8String): UTF8StringEnumerator; inline;
begin
   Result := UTF8StringEnumerator.Create(@Value);
end;

function CodepointArrayToUTF8String(const Value: TUnicodeCodepointArray): UTF8String;
var
   Index, NewLength, Offset: Cardinal;
   Character: TUTF8Sequence;
begin
   if (Length(Value) = 0) then
   begin
      Result := '';
      exit;
   end;
   NewLength := 0;
   for Index := Low(Value) to High(Value) do // $R-
      Inc(NewLength, CodepointToUTF8Length(Value[Index]));
   SetLength(Result, NewLength);
   Offset := 1;
   for Index := Low(Value) to High(Value) do // $R-
   begin
      Character := CodepointToUTF8(Value[Index]);
      Move(Character.Start, Result[Offset], Character.Length);
      Inc(Offset, Character.Length);
   end;
end;

{$IFDEF DEBUG}
procedure TestIterator();
var
   S: UTF8String;
   Index: TUnicodeCodepointRange;
   Position: Cardinal;
   SubS: TUTF8Sequence;
   C: TUnicodeCodepoint;
begin
   SetLength(S, ($80 * 1) + (($7FF-$7F) * 2) + ((($FFFF-$7FF) - ($E000-$D800)) * 3) + (($10FFFF - $FFFF) * 4));
   Position := 1;
   for Index := $0 to $D7FF do
   begin
      SubS := CodepointToUTF8(Index);
      Move(SubS.Start, S[Position], SubS.Length);
      Inc(Position, SubS.Length);
   end;
   for Index := $E000 to $10FFFF do
   begin
      SubS := CodepointToUTF8(Index);
      Move(SubS.Start, S[Position], SubS.Length);
      Inc(Position, SubS.Length);
   end;
   Assert(Position = Length(S)+1);
   Index := -1;
   for C in S do
   begin
      Inc(Index);
      if (Index = $D800) then
         Index := $E000;
      Assert(C = Index, 'Expected U+' + IntToHex(Index, 4) + ' but found U+' + IntToHex(C.Value, 4));
   end;
   Assert(Index = $10FFFF);
end;
{$ENDIF}

initialization
   {$IFDEF TESTS}
      TestIterator();
   {$ENDIF}
end.