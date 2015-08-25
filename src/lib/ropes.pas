{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit ropes;

// Note: This is not a http://en.wikipedia.org/wiki/Rope_(data_structure)
// It's just a way to create a string by just referencing underlying strings.

// WARNING: It's very easy, with this API, to shoot yourself in the
// foot. In particular, never assign a rope to another unless you
// immediately discard the former; you'll end up with pointers
// pointing into all kinds of crazy locations and will either crash or
// have security bugs.

interface

uses unicode, utf8;

// Avoid assigning Ropes to other objects (e.g. pass them around using
// pointers or as var or constref arguments). It's ok to do so, but
// it's expensive, since we have to do an exception-safe, thread-safe
// locked reference count inc/dec.

// XXX improvements:
//      - avoid Length(), it's expensive
//      - make kMinSubslots bigger

type
   PRope = ^Rope;
   RopeInternals = record
    private
     type
      TRopeFragmentKind = (rfUTF8Buffer, rfUTF8Inline, rfCodepoints);
    strict private
     type
      TFixedSizeRopeFragment = record
         case TRopeFragmentKind of
          rfUTF8Buffer: (BufferValue: PByte; BufferLength: Cardinal);
          rfUTF8Inline: (InlineValue: array[0..0] of Byte; InlineLength: Byte); // store at least one byte
          rfCodepoints: (CodepointsValue: array[0..1] of TUnicodeCodepoint; CodepointsLength: Byte); // store at least two characters
      end;
    private
     const
      FragmentPayloadSize = SizeOf(TFixedSizeRopeFragment);
      UTF8InlineSize = FragmentPayloadSize - SizeOf(Byte);
      CodepointsSize = (FragmentPayloadSize-SizeOf(Byte)) div SizeOf(TUnicodeCodepoint);
     type
      TUTF8InlineIndex = 0..UTF8InlineSize-1;
      TUTF8InlineIndexPlusOne = 0..UTF8InlineSize;
      TCodepointsIndex = 0..CodepointsSize-1;
      TCodepointsIndexPlusOne = 0..CodepointsSize;
      TInlineString = String[UTF8InlineSize];
      PRopeFragment = ^TRopeFragment;
      TRopeFragment = record
         Next: PRopeFragment;
         case Kind: TRopeFragmentKind of
          // it's safe to assume that the first two here end on UTF-8 character boundaries
          rfUTF8Buffer: (BufferValue: PByte; BufferLength: Cardinal);
          rfUTF8Inline: (InlineValue: array[TUTF8InlineIndex] of Byte; InlineLength: Byte); // make sure this has a trailing zero so we can decode it safely
          rfCodepoints: (CodepointsValue: array[TCodepointsIndex] of TUnicodeCodepoint; CodepointsLength: Byte);
      end;
      TRopeFragmentPosition = record // http://bugs.freepascal.org/view.php?id=26402
         CurrentFragment: PRopeFragment;
         case TRopeFragmentKind of // CurrentFragment.Kind
          rfUTF8Buffer: (BufferIndex: Cardinal; BufferCodepointLength: TZeroableUTF8SequenceLength);
          rfUTF8Inline: (InlineIndex: TUTF8InlineIndexPlusOne; InlineCodepointLength: TZeroableUTF8SequenceLength);
          rfCodepoints: (CodepointsIndex: TCodepointsIndexPlusOne; CodepointsIndexIncluded: Boolean);
      end;
      function Serialise(const FirstFragment: PRopeFragment): UTF8String;
   end;

   TRopePointer = record
    private
      {$IFOPT C+} FRope: PRope; {$ENDIF}
      FPosition: RopeInternals.TRopeFragmentPosition;
      FCurrentCharacter: TUnicodeCodepoint;
    public
      {$IFOPT C+} function IsZeroWidth(): Boolean; {$ENDIF}
      {$IFOPT C+} function IsEOF(): Boolean; {$ENDIF}
      {$IFOPT C+} procedure AssertIdentity(const Data: PRope); {$ENDIF}
      procedure AdvanceToAfter(); inline; // changes to a zero-width pointer; only valid if currently non-zero
      procedure ReadCurrent();
      procedure AdvanceToNext();
      procedure AdvanceToNextFaster(); // still slow, but does not keep track of current character
      procedure SetToZeroWidth(); inline; // changes to a zero-width pointer
      property CurrentCharacter: TUnicodeCodepoint read FCurrentCharacter; // only valid after AdvanceToNext() or ReadCurrent()
   end;

   CutRope = record
    // use this kind of rope when you want to make sure that if it is appended to another rope,
    // it's done as cheaply as currently supported
    private
      FValue: array of RopeInternals.TRopeFragment; // never grow this array once it's done, or you'll trash the Next pointers
      {$IFOPT C+} FRead: Boolean; {$ENDIF}
      function GetIsEmpty(): Boolean; inline;
      function GetAsString(): UTF8String;
    public
      class function CreateFrom(const NewString: TUnicodeCodepointArray): CutRope; static; inline;
      class function CreateFrom(const NewString: UTF8String): CutRope; static; inline; // this had better be a constant string
      function GetAsStringSlow(): UTF8String; // same as .AsString, but allows you to do it more thn once which is a waste in production code)
      property IsEmpty: Boolean read GetIsEmpty;
      property AsString: UTF8String read GetAsString; // destroys self, to encourage memoisation by the caller if necessary
   end;

   RopeEnumerator = class;
   Rope = record
    private
     const
      kMinBaseSlots = 3;
      kMinSubslots = 5;
     var
      FFilledLength, FLastArrayFilledLength: Cardinal;
      FLast: RopeInternals.PRopeFragment;
      FValue: array of array of RopeInternals.TRopeFragment;
      procedure EnsureSize(const NeededBaseSlots: Cardinal = 1; const NeededSubslots: Cardinal = 1); inline; // inline even though it's complex, because we call it with constants and i want parts of it to get optimised away
      // note: never grow a subslot array, since doing so would move the data around and you'd have to redo all the Next pointers
      function GetIsEmpty(): Boolean; inline;
      function GetAsString(): UTF8String;
    public
      {$IFOPT C+} procedure AssertInit(); {$ENDIF}
      procedure AppendDestructively(var NewString: CutRope); inline; // destroys argument
      procedure AppendDestructively(var NewString: Rope); inline; // destroys argument
      procedure Append(const NewString: PUTF8String); inline;
      procedure Append(const NewString: RopeInternals.TInlineString); inline;
      procedure Append(const NewString: TUnicodeCodepoint); inline;
      procedure Append(const NewString: TUnicodeCodepointArray); inline; // length of data must be <= CodepointsSize (2)
      procedure Append(const NewData: Pointer; const NewLength: QWord); inline;
      procedure AppendPossiblyIncomplete(const NewData: Pointer; const NewLength: QWord); inline; unimplemented;
      function Extract(constref NewStart, NewEnd: TRopePointer): CutRope; inline;
       // includes NewEnd if it is non-zero-width, excludes otherwise
       // this does a memory copy into a new string
      function ExtractAll(): CutRope; inline;
      function ExtractToEnd(constref NewStart: TRopePointer): CutRope; inline;
      function CountCharacters(constref NewStart, NewEnd: TRopePointer): Cardinal; inline;
       // includes NewEnd if it is non-zero-width, excludes otherwise
       // note: this one is expensive (it iterates over the string, parsing UTF8)
      function GetEnumerator(): RopeEnumerator; inline;
      property IsEmpty: Boolean read GetIsEmpty;
      property AsString: UTF8String read GetAsString;
   end;

   RopeEnumerator = class
    private
      FPointer: TRopePointer;
    public
      constructor Create(const NewTarget: PRope);
      {$IFOPT C+} procedure AssertIdentity(constref Target: Rope); {$ENDIF}
      function MoveNext(): Boolean; inline;
      property Current: TUnicodeCodepoint read FPointer.FCurrentCharacter;
      function GetPointer(): TRopePointer; inline;
      procedure ReturnToPointer(constref NewPointer: TRopePointer); inline;
      function GetCurrentAsUTF8(): TUTF8Sequence; inline;
   end;

   operator = (constref Op1, Op2: TRopePointer): Boolean;
   operator < (constref Op1, Op2: TRopePointer): Boolean;
   operator <= (constref Op1, Op2: TRopePointer): Boolean;
   operator = (constref Op1, Op2: Rope): Boolean;

{$IFDEF VERBOSE} var DebugNow: Boolean = False; {$ENDIF}

implementation

uses
   exceptions, sysutils {$IFOPT C+}, rtlutils {$ENDIF};

function Max(const Value1, Value2: Cardinal): Cardinal; inline;
begin
   if (Value1 > Value2) then
      Result := Value1
   else
      Result := Value2;
end;

function Max(const Value1, Value2, Value3: Cardinal): Cardinal; inline;
begin
   if (Value1 > Value2) then
   begin
      if (Value1 > Value3) then
         Result := Value1 // 1 > 3, 2
      else
         Result := Value3; // 3 > 1 > 2
   end
   else
   begin
      if (Value2 > Value3) then
         Result := Value2 // 2 > 3, 1
      else
         Result := Value3; // 3 > 2 > 1
   end;
end;


function RopeInternals.Serialise(const FirstFragment: PRopeFragment): UTF8String;
var
   NeededLength, Offset, Index: Cardinal;
   CurrentFragment: RopeInternals.PRopeFragment;
   Scratch: TUTF8Sequence;
begin
   Assert(Assigned(FirstFragment));
   NeededLength := 0;
   CurrentFragment := FirstFragment;
   repeat
      case CurrentFragment^.Kind of
         rfUTF8Buffer: Inc(NeededLength, CurrentFragment^.BufferLength);
         rfUTF8Inline: Inc(NeededLength, CurrentFragment^.InlineLength);
         rfCodepoints:
            begin
               Assert(CurrentFragment^.CodepointsLength > 0);
               for Index := 0 to CurrentFragment^.CodepointsLength-1 do // $R-
                  Inc(NeededLength, CodepointToUTF8Length(CurrentFragment^.CodepointsValue[Index]));
            end;
      else
         Assert(False);
      end;
      Assert(CurrentFragment <> CurrentFragment^.Next);
      CurrentFragment := CurrentFragment^.Next;
   until not Assigned(CurrentFragment);
   SetLength(Result, NeededLength);
   Offset := 1;
   CurrentFragment := FirstFragment;
   repeat
      case CurrentFragment^.Kind of
         rfUTF8Buffer:
            begin
               {$IFOPT C+}
               try
               {$ENDIF}
                  Move(CurrentFragment^.BufferValue^, Result[Offset], CurrentFragment^.BufferLength);
               {$IFOPT C+}
               except
                  ReportCurrentException();
                  FillChar(Result[Offset], CurrentFragment^.BufferLength, '%');
               end;
               {$ENDIF}
               Inc(Offset, CurrentFragment^.BufferLength);
            end;
         rfUTF8Inline:
            begin
               Move(CurrentFragment^.InlineValue[0], Result[Offset], CurrentFragment^.InlineLength);
               Inc(Offset, CurrentFragment^.InlineLength);
            end;
         rfCodepoints:
            for Index := 0 to CurrentFragment^.CodepointsLength-1 do // $R-
            begin
               Scratch := CodepointToUTF8(CurrentFragment^.CodepointsValue[Index]);
               Move(Scratch.Start, Result[Offset], Scratch.Length);
               Inc(Offset, Scratch.Length);
            end;
      else
         Assert(False);
      end;
      Assert(CurrentFragment <> CurrentFragment^.Next);
      CurrentFragment := CurrentFragment^.Next;
   until not Assigned(CurrentFragment);
   Assert(Offset = Length(Result)+1);
end;


{$IFOPT C+}
function TRopePointer.IsZeroWidth(): Boolean;
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: IsZeroWidth() on rope pointer @', IntToHex(PtrUInt(@Self), 16)); {$ENDIF}
   {$IFOPT C+} Assert(Assigned(FRope)); {$ENDIF}
   case (FPosition.CurrentFragment^.Kind) of
      rfUTF8Buffer: Result := FPosition.BufferCodepointLength = 0;
      rfUTF8Inline: Result := FPosition.InlineCodepointLength = 0;
      rfCodepoints: Result := not FPosition.CodepointsIndexIncluded;
   else
      Assert(False);
   end;
   if (FCurrentCharacter.Value >= 0) then
      Assert(not Result)
   else
      Assert(Result);
end;
{$ENDIF}

{$IFOPT C+}
function TRopePointer.IsEOF(): Boolean;
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: IsEOF() on rope pointer @', IntToHex(PtrUInt(@Self), 16)); {$ENDIF}
   {$IFOPT C+} Assert(Assigned(FRope)); {$ENDIF}
   if (not Assigned(FPosition.CurrentFragment)) then
   begin
      {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   not Assigned(FPosition.CurrentFragment)', IntToHex(PtrUInt(@Self), 16)); {$ENDIF}
      Result := True;
   end
   else
   if (Assigned(FPosition.CurrentFragment^.Next)) then
   begin
      {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   Assigned(FPosition.CurrentFragment^.Next) on rope pointer @', IntToHex(PtrUInt(@Self), 16)); {$ENDIF}
      Result := False;
   end
   else
   begin
      {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   not Assigned(FPosition.CurrentFragment^.Next) on rope pointer @', IntToHex(PtrUInt(@Self), 16)); {$ENDIF}
      {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   FPosition.CurrentFragment^.Kind = ', FPosition.CurrentFragment^.Kind); {$ENDIF}
      case (FPosition.CurrentFragment^.Kind) of
         rfUTF8Buffer: Result := (FPosition.BufferCodepointLength = 0) and (FPosition.BufferIndex = FPosition.CurrentFragment^.BufferLength);
         rfUTF8Inline: Result := (FPosition.InlineCodepointLength = 0) and (FPosition.InlineIndex = FPosition.CurrentFragment^.InlineLength);
         rfCodepoints: Result := (not FPosition.CodepointsIndexIncluded) and (FPosition.CodepointsIndex = FPosition.CurrentFragment^.CodepointsLength);
      else
         Assert(False);
      end;
   end;
   {$IFOPT C+}
   if (Result) then // {BOGUS Warning: Function result variable does not seem to initialized}
      Assert((FCurrentCharacter = kEOF) or (FCurrentCharacter = kNone), 'expected EOF (or none) but had ' +  FCurrentCharacter.GetDebugDescription())
   else
      Assert(FCurrentCharacter <> kEOF);
   {$ENDIF}
end;
{$ENDIF}

{$IFOPT C+}
procedure TRopePointer.AssertIdentity(const Data: PRope);
begin
   {$IFOPT C+} Assert(Assigned(FRope)); {$ENDIF}
   Assert(Data = FRope);
end;
{$ENDIF}


procedure TRopePointer.AdvanceToAfter();
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: AdvanceToAfter() on rope pointer @', IntToHex(PtrUInt(@Self), 16)); {$ENDIF}
   {$IFOPT C+} Assert(Assigned(FRope)); {$ENDIF}
   {$IFOPT C+} Assert(not IsZeroWidth()); {$ENDIF}
   {$IFOPT C+} Assert(not IsEOF()); {$ENDIF}
   case (FPosition.CurrentFragment^.Kind) of
      rfUTF8Buffer:
         begin
            Inc(FPosition.BufferIndex, FPosition.BufferCodepointLength);
            FPosition.BufferCodepointLength := 0;
         end;
      rfUTF8Inline:
         begin
            Inc(FPosition.InlineIndex, FPosition.InlineCodepointLength);
            FPosition.InlineCodepointLength := 0;
         end;
      rfCodepoints:
         begin
            Inc(FPosition.CodepointsIndex);
            FPosition.CodepointsIndexIncluded := False;
         end;
   else
      Assert(False);
   end;
   {$IFOPT C+} FCurrentCharacter := kNone; {$ENDIF}
end;

procedure TRopePointer.ReadCurrent();
var
   LocalBuffer: array[0..4] of Byte;
   NewLength: TUTF8SequenceLength;
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: ReadCurrent() on rope pointer @', IntToHex(PtrUInt(@Self), 16)); {$ENDIF}

   {$IFOPT C+}
   Assert(Assigned(FRope));
   Assert(not IsEOF());
   {$ENDIF}

   case (FPosition.CurrentFragment^.Kind) of
      rfUTF8Buffer:
         begin
            case (FPosition.CurrentFragment^.BufferLength-FPosition.BufferIndex) of
               0: begin
                     Assert(False);
                  end;
               1: begin
                     LocalBuffer[0] := (FPosition.CurrentFragment^.BufferValue+FPosition.BufferIndex)^;
                     LocalBuffer[1] := 0;
                     FCurrentCharacter := UTF8ToCodepoint(@LocalBuffer[0], NewLength);
                  end;
               2: begin
                     LocalBuffer[0] := (FPosition.CurrentFragment^.BufferValue+FPosition.BufferIndex)^;
                     LocalBuffer[1] := (FPosition.CurrentFragment^.BufferValue+FPosition.BufferIndex+1)^;
                     LocalBuffer[2] := 0;
                     FCurrentCharacter := UTF8ToCodepoint(@LocalBuffer[0], NewLength);
                  end;
               3: begin
                     LocalBuffer[0] := (FPosition.CurrentFragment^.BufferValue+FPosition.BufferIndex)^;
                     LocalBuffer[1] := (FPosition.CurrentFragment^.BufferValue+FPosition.BufferIndex+1)^;
                     LocalBuffer[2] := (FPosition.CurrentFragment^.BufferValue+FPosition.BufferIndex+2)^;
                     LocalBuffer[3] := 0;
                     FCurrentCharacter := UTF8ToCodepoint(@LocalBuffer[0], NewLength);
                  end;
            else
               begin
                  FCurrentCharacter := UTF8ToCodepoint(FPosition.CurrentFragment^.BufferValue + FPosition.BufferIndex, NewLength);
               end;
            end;
            Assert(NewLength = FPosition.BufferCodepointLength);
         end;
      rfUTF8Inline:
         begin
            Assert(FPosition.InlineIndex < FPosition.CurrentFragment^.InlineLength);
            FCurrentCharacter := UTF8ToCodepoint(@FPosition.CurrentFragment^.InlineValue[FPosition.InlineIndex], NewLength);
            Assert(NewLength = FPosition.InlineCodepointLength);
         end;
      rfCodepoints:
         begin
            Assert(FPosition.CodepointsIndex < FPosition.CurrentFragment^.CodepointsLength);
            FCurrentCharacter := FPosition.CurrentFragment^.CodepointsValue[FPosition.CodepointsIndex];
         end;
   else
      Assert(False);
   end;
end;

procedure TRopePointer.AdvanceToNext();
var
   LocalBuffer: array[0..4] of Byte;

   procedure MoveToNext();
   var
      Before, After: PtrUInt;
   begin
      {$IFDEF VERBOSE} if (DebugNow) then Writeln('  AdvanceToNext.MoveToNext() on rope pointer @', IntToHex(PtrUInt(@Self), 16)); {$ENDIF}
      // this is called if we've exhausted the current fragment
      if (Assigned(FPosition.CurrentFragment^.Next)) then
      begin
         {$IFDEF VERBOSE} if (DebugNow) then Writeln('    FPosition.CurrentFragment @', IntToHex(PtrUInt(FPosition.CurrentFragment), 16), ' and is ', FPosition.CurrentFragment^.Kind, ' (rope pointer now @', IntToHex(PtrUInt(@Self), 16), ')'); {$ENDIF}
         {$IFDEF VERBOSE} if (DebugNow) then Writeln('    FPosition.CurrentFragment^.Next @', IntToHex(PtrUInt(FPosition.CurrentFragment^.Next), 16), ' and is ', FPosition.CurrentFragment^.Next^.Kind, ' (rope pointer now @', IntToHex(PtrUInt(@Self), 16), ')'); {$ENDIF}
         //Before := PtrUInt(FPosition.CurrentFragment);
         Assert(FPosition.CurrentFragment <> FPosition.CurrentFragment^.Next);
         FPosition.CurrentFragment := FPosition.CurrentFragment^.Next;
         {$IFDEF VERBOSE} if (DebugNow) then Writeln('    FPosition.CurrentFragment is now @', IntToHex(PtrUInt(FPosition.CurrentFragment), 16), ' and is ', FPosition.CurrentFragment^.Kind, ' (rope pointer now @', IntToHex(PtrUInt(@Self), 16), ')'); {$ENDIF}
         //Assert(Before <> PtrUInt(FPosition.CurrentFragment));
         case (FPosition.CurrentFragment^.Kind) of
            rfUTF8Buffer:
               begin
                  FPosition.BufferIndex := 0;
                  Assert(FPosition.CurrentFragment^.BufferLength > 0);
                  case (FPosition.CurrentFragment^.BufferLength) of
                     1: begin
                           LocalBuffer[0] := FPosition.CurrentFragment^.BufferValue^;
                           LocalBuffer[1] := 0;
                           FCurrentCharacter := UTF8ToCodepoint(@LocalBuffer[0], FPosition.BufferCodepointLength);
                        end;
                     2: begin
                           LocalBuffer[0] := (FPosition.CurrentFragment^.BufferValue)^;
                           LocalBuffer[1] := (FPosition.CurrentFragment^.BufferValue+1)^;
                           LocalBuffer[2] := 0;
                           FCurrentCharacter := UTF8ToCodepoint(@LocalBuffer[0], FPosition.BufferCodepointLength);
                        end;
                     3: begin
                           LocalBuffer[0] := (FPosition.CurrentFragment^.BufferValue)^;
                           LocalBuffer[1] := (FPosition.CurrentFragment^.BufferValue+1)^;
                           LocalBuffer[2] := (FPosition.CurrentFragment^.BufferValue+2)^;
                           LocalBuffer[3] := 0;
                           FCurrentCharacter := UTF8ToCodepoint(@LocalBuffer[0], FPosition.BufferCodepointLength);
                        end;
                  else
                     FCurrentCharacter := UTF8ToCodepoint(FPosition.CurrentFragment^.BufferValue, FPosition.BufferCodepointLength);
                  end;
                  Assert(FPosition.BufferIndex <= FPosition.CurrentFragment^.BufferLength); // should not go over what we have...
               end;
            rfUTF8Inline:
               begin
                  FPosition.InlineIndex := 0;
                  Assert(FPosition.CurrentFragment^.InlineLength > 0);
                  FCurrentCharacter := UTF8ToCodepoint(@FPosition.CurrentFragment^.InlineValue[0], FPosition.InlineCodepointLength);
                  Assert(FPosition.InlineIndex+FPosition.BufferCodepointLength <= FPosition.CurrentFragment^.InlineLength);
               end;
            rfCodepoints:
               begin
                  FPosition.CodepointsIndex := 0;
                  Assert(FPosition.CurrentFragment^.CodepointsLength > 0);
                  FCurrentCharacter := FPosition.CurrentFragment^.CodepointsValue[0];
                  FPosition.CodepointsIndexIncluded := True;
               end;
         else
            begin
               Assert(False);
               {$IFOPT C+} FCurrentCharacter := kNone; {$ENDIF}
            end;
         end;
         {$IFDEF VERBOSE} if (DebugNow) then Writeln('    new fragment is @', IntToHex(PtrUInt(FPosition.CurrentFragment), 16), ' and is ', FPosition.CurrentFragment^.Kind, '; FCurrentCharacter is U+', IntToHex(FCurrentCharacter.Value, 5), ' (rope pointer now @', IntToHex(PtrUInt(@Self), 16), ')'); {$ENDIF}
      end
      else
      begin
         {$IFDEF VERBOSE} if (DebugNow) then Writeln('    reached end of rope pointer @', IntToHex(PtrUInt(@Self), 16)); {$ENDIF}
         FCurrentCharacter := kEOF;
         case (FPosition.CurrentFragment^.Kind) of
            rfUTF8Buffer: FPosition.BufferCodepointLength := 0;
            rfUTF8Inline: FPosition.InlineCodepointLength := 0;
            rfCodepoints: FPosition.CodepointsIndexIncluded := False;
         else
            Assert(False);
         end;
      end;
   end;

begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: AdvanceToNext() on rope pointer @', IntToHex(PtrUInt(@Self), 16)); {$ENDIF}
   {$IFOPT C+} Assert(Assigned(FRope)); {$ENDIF}
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('  current fragment @', IntToHex(PtrUInt(FPosition.CurrentFragment), 16), ' is ', FPosition.CurrentFragment^.Kind, ' (rope pointer now @', IntToHex(PtrUInt(@Self), 16), ')'); {$ENDIF}
   case (FPosition.CurrentFragment^.Kind) of
      rfUTF8Buffer:
         begin
            {$IFDEF VERBOSE} if (DebugNow) then Writeln('  current fragment is rfUTF8Buffer (rope pointer now @', IntToHex(PtrUInt(@Self), 16), ')'); {$ENDIF}
            {$IFDEF VERBOSE} if (DebugNow) then Writeln('  FPosition.BufferCodepointLength = ', FPosition.BufferCodepointLength, ' (rope pointer now @', IntToHex(PtrUInt(@Self), 16), ')'); {$ENDIF}
            if (FPosition.BufferCodepointLength > 0) then
               Inc(FPosition.BufferIndex, FPosition.BufferCodepointLength);
            {$IFDEF VERBOSE} if (DebugNow) then Writeln('  FPosition.BufferIndex (post inc) = ', FPosition.BufferIndex, ' (rope pointer now @', IntToHex(PtrUInt(@Self), 16), ')'); {$ENDIF}
            Assert(FPosition.BufferIndex <= FPosition.CurrentFragment^.BufferLength); // UTF-8 can't span boundaries, so we can't have advanced past the boundary
            case (FPosition.CurrentFragment^.BufferLength-FPosition.BufferIndex) of
               0: begin
                     MoveToNext();
                  end;
               1: begin
                     LocalBuffer[0] := (FPosition.CurrentFragment^.BufferValue+FPosition.BufferIndex)^;
                     LocalBuffer[1] := 0;
                     FCurrentCharacter := UTF8ToCodepoint(@LocalBuffer[0], FPosition.BufferCodepointLength);
                     Assert(FPosition.BufferIndex+FPosition.BufferCodepointLength <= FPosition.CurrentFragment^.BufferLength); // should still not be more than we have...
                  end;
               2: begin
                     LocalBuffer[0] := (FPosition.CurrentFragment^.BufferValue+FPosition.BufferIndex)^;
                     LocalBuffer[1] := (FPosition.CurrentFragment^.BufferValue+FPosition.BufferIndex+1)^;
                     LocalBuffer[2] := 0;
                     FCurrentCharacter := UTF8ToCodepoint(@LocalBuffer[0], FPosition.BufferCodepointLength);
                     Assert(FPosition.BufferIndex+FPosition.BufferCodepointLength <= FPosition.CurrentFragment^.BufferLength); // should still not be more than we have...
                  end;
               3: begin
                     LocalBuffer[0] := (FPosition.CurrentFragment^.BufferValue+FPosition.BufferIndex)^;
                     LocalBuffer[1] := (FPosition.CurrentFragment^.BufferValue+FPosition.BufferIndex+1)^;
                     LocalBuffer[2] := (FPosition.CurrentFragment^.BufferValue+FPosition.BufferIndex+2)^;
                     LocalBuffer[3] := 0;
                     FCurrentCharacter := UTF8ToCodepoint(@LocalBuffer[0], FPosition.BufferCodepointLength);
                     Assert(FPosition.BufferIndex+FPosition.BufferCodepointLength <= FPosition.CurrentFragment^.BufferLength); // should still not be more than we have...
                  end;
            else
               begin
                  FCurrentCharacter := UTF8ToCodepoint(FPosition.CurrentFragment^.BufferValue + FPosition.BufferIndex, FPosition.BufferCodepointLength);
                  Assert(FPosition.BufferIndex+FPosition.BufferCodepointLength <= FPosition.CurrentFragment^.BufferLength); // should still not be more than we have...
               end;
            end;
         end;
      rfUTF8Inline:
         begin
            {$IFDEF VERBOSE} if (DebugNow) then Writeln('  current fragment is rfUTF8Inline (rope pointer now @', IntToHex(PtrUInt(@Self), 16), ')'); {$ENDIF}
            {$IFDEF VERBOSE} if (DebugNow) then Writeln('  FPosition.InlineCodepointLength = ', FPosition.InlineCodepointLength, ' (rope pointer now @', IntToHex(PtrUInt(@Self), 16), ')'); {$ENDIF}
            if (FPosition.InlineCodepointLength > 0) then
               Inc(FPosition.InlineIndex, FPosition.InlineCodepointLength);
            {$IFDEF VERBOSE} if (DebugNow) then Writeln('  FPosition.InlineIndex (post inc) = ', FPosition.InlineIndex, ' (rope pointer now @', IntToHex(PtrUInt(@Self), 16), ')'); {$ENDIF}
            Assert(FPosition.InlineIndex <= FPosition.CurrentFragment^.InlineLength); // UTF-8 can't span boundaries, so we can't have advanced past the boundary
            if (FPosition.InlineIndex = FPosition.CurrentFragment^.InlineLength) then
            begin
               MoveToNext();
            end
            else
            begin
               FCurrentCharacter := UTF8ToCodepoint(@FPosition.CurrentFragment^.InlineValue[FPosition.InlineIndex], FPosition.InlineCodepointLength);
               Assert(FPosition.InlineIndex+FPosition.BufferCodepointLength <= FPosition.CurrentFragment^.InlineLength); // should still not be more than we have...
            end;
         end;
      rfCodepoints:
         begin
            {$IFDEF VERBOSE} if (DebugNow) then Writeln('  current fragment is rfCodepoints (rope pointer now @', IntToHex(PtrUInt(@Self), 16), ')'); {$ENDIF}
            {$IFDEF VERBOSE} if (DebugNow) then Writeln('  FPosition.CodepointsIndexIncluded = ', FPosition.CodepointsIndexIncluded, ' (rope pointer now @', IntToHex(PtrUInt(@Self), 16), ')'); {$ENDIF}
            if (FPosition.CodepointsIndexIncluded) then
               Inc(FPosition.CodepointsIndex);
            {$IFDEF VERBOSE} if (DebugNow) then Writeln('  FPosition.CodepointsIndex (post inc) = ', FPosition.CodepointsIndex, ' (rope pointer now @', IntToHex(PtrUInt(@Self), 16), ')'); {$ENDIF}
            Assert(FPosition.CodepointsIndex <= FPosition.CurrentFragment^.CodepointsLength);
            if (FPosition.CodepointsIndex = FPosition.CurrentFragment^.CodepointsLength) then
            begin
               MoveToNext()
            end
            else
            begin
               FCurrentCharacter := FPosition.CurrentFragment^.CodepointsValue[FPosition.CodepointsIndex];
               FPosition.CodepointsIndexIncluded := True;
            end;
         end;
   else
      Assert(False);
   end;
end;

procedure TRopePointer.AdvanceToNextFaster();
var
   LocalBuffer: array[0..4] of Byte;

   procedure MoveToNext();
   begin
      // this is called if we've exhausted the current fragment
      if (Assigned(FPosition.CurrentFragment^.Next)) then
      begin
         Assert(FPosition.CurrentFragment <> FPosition.CurrentFragment^.Next);
         FPosition.CurrentFragment := FPosition.CurrentFragment^.Next;
         case (FPosition.CurrentFragment^.Kind) of
            rfUTF8Buffer:
               begin
                  FPosition.BufferIndex := 0;
                  Assert(FPosition.CurrentFragment^.BufferLength > 0);
                  case (FPosition.CurrentFragment^.BufferLength) of
                     1: begin
                           LocalBuffer[0] := FPosition.CurrentFragment^.BufferValue^;
                           LocalBuffer[1] := 0;
                           FPosition.BufferCodepointLength := UTF8ToUTF8Length(@LocalBuffer[0]);
                        end;
                     2: begin
                           LocalBuffer[0] := (FPosition.CurrentFragment^.BufferValue)^;
                           LocalBuffer[1] := (FPosition.CurrentFragment^.BufferValue+1)^;
                           LocalBuffer[2] := 0;
                           FPosition.BufferCodepointLength := UTF8ToUTF8Length(@LocalBuffer[0]);
                        end;
                     3: begin
                           LocalBuffer[0] := (FPosition.CurrentFragment^.BufferValue)^;
                           LocalBuffer[1] := (FPosition.CurrentFragment^.BufferValue+1)^;
                           LocalBuffer[2] := (FPosition.CurrentFragment^.BufferValue+2)^;
                           LocalBuffer[3] := 0;
                           FPosition.BufferCodepointLength := UTF8ToUTF8Length(@LocalBuffer[0]);
                        end;
                  else
                     FPosition.BufferCodepointLength := UTF8ToUTF8Length(FPosition.CurrentFragment^.BufferValue);
                  end;
                  Assert(FPosition.BufferIndex <= FPosition.CurrentFragment^.BufferLength); // should not go over what we have...
               end;
            rfUTF8Inline:
               begin
                  FPosition.InlineIndex := 0;
                  Assert(FPosition.CurrentFragment^.InlineLength > 0);
                  FPosition.InlineCodepointLength := UTF8ToUTF8Length(@FPosition.CurrentFragment^.InlineValue[0]);
                  Assert(FPosition.InlineIndex+FPosition.BufferCodepointLength <= FPosition.CurrentFragment^.InlineLength);
               end;
            rfCodepoints:
               begin
                  FPosition.CodepointsIndex := 0;
                  Assert(FPosition.CurrentFragment^.CodepointsLength > 0);
                  FPosition.CodepointsIndexIncluded := True;
               end;
         else
            begin
               Assert(False);
            end;
         end;
      end
      else
      begin
         case (FPosition.CurrentFragment^.Kind) of
            rfUTF8Buffer: FPosition.BufferCodepointLength := 0;
            rfUTF8Inline: FPosition.InlineCodepointLength := 0;
            rfCodepoints: FPosition.CodepointsIndexIncluded := False;
         else
            Assert(False);
         end;
      end;
   end;

begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: AdvanceToNextFaster() on rope pointer @', IntToHex(PtrUInt(@Self), 16)); {$ENDIF}
   {$IFOPT C+} Assert(Assigned(FRope)); {$ENDIF}
   case (FPosition.CurrentFragment^.Kind) of
      rfUTF8Buffer:
         begin
            if (FPosition.BufferCodepointLength > 0) then
               Inc(FPosition.BufferIndex, FPosition.BufferCodepointLength);
            Assert(FPosition.BufferIndex <= FPosition.CurrentFragment^.BufferLength); // UTF-8 can't span boundaries, so we can't have advanced past the boundary
            case (FPosition.CurrentFragment^.BufferLength-FPosition.BufferIndex) of
               0: begin
                     MoveToNext();
                  end;
               1: begin
                     LocalBuffer[0] := (FPosition.CurrentFragment^.BufferValue+FPosition.BufferIndex)^;
                     LocalBuffer[1] := 0;
                     FPosition.BufferCodepointLength := UTF8ToUTF8Length(@LocalBuffer[0]);
                     Assert(FPosition.BufferIndex+FPosition.BufferCodepointLength <= FPosition.CurrentFragment^.BufferLength); // should still not be more than we have...
                  end;
               2: begin
                     LocalBuffer[0] := (FPosition.CurrentFragment^.BufferValue+FPosition.BufferIndex)^;
                     LocalBuffer[1] := (FPosition.CurrentFragment^.BufferValue+FPosition.BufferIndex+1)^;
                     LocalBuffer[2] := 0;
                     FPosition.BufferCodepointLength := UTF8ToUTF8Length(@LocalBuffer[0]);
                     Assert(FPosition.BufferIndex+FPosition.BufferCodepointLength <= FPosition.CurrentFragment^.BufferLength); // should still not be more than we have...
                  end;
               3: begin
                     LocalBuffer[0] := (FPosition.CurrentFragment^.BufferValue+FPosition.BufferIndex)^;
                     LocalBuffer[1] := (FPosition.CurrentFragment^.BufferValue+FPosition.BufferIndex+1)^;
                     LocalBuffer[2] := (FPosition.CurrentFragment^.BufferValue+FPosition.BufferIndex+2)^;
                     LocalBuffer[3] := 0;
                     FPosition.BufferCodepointLength := UTF8ToUTF8Length(@LocalBuffer[0]);
                     Assert(FPosition.BufferIndex+FPosition.BufferCodepointLength <= FPosition.CurrentFragment^.BufferLength); // should still not be more than we have...
                  end;
            else
               begin
                  FPosition.BufferCodepointLength := UTF8ToUTF8Length(FPosition.CurrentFragment^.BufferValue + FPosition.BufferIndex);
                  Assert(FPosition.BufferIndex+FPosition.BufferCodepointLength <= FPosition.CurrentFragment^.BufferLength); // should still not be more than we have...
               end;
            end;
         end;
      rfUTF8Inline:
         begin
            if (FPosition.InlineCodepointLength > 0) then
               Inc(FPosition.InlineIndex, FPosition.InlineCodepointLength);
            Assert(FPosition.InlineIndex <= FPosition.CurrentFragment^.InlineLength); // UTF-8 can't span boundaries, so we can't have advanced past the boundary
            if (FPosition.InlineIndex = FPosition.CurrentFragment^.InlineLength) then
            begin
               MoveToNext();
            end
            else
            begin
               FPosition.BufferCodepointLength := UTF8ToUTF8Length(@FPosition.CurrentFragment^.InlineValue[FPosition.InlineIndex]);
               Assert(FPosition.InlineIndex+FPosition.BufferCodepointLength <= FPosition.CurrentFragment^.InlineLength); // should still not be more than we have...
            end;
         end;
      rfCodepoints:
         begin
            if (FPosition.CodepointsIndexIncluded) then
               Inc(FPosition.CodepointsIndex);
            Assert(FPosition.CodepointsIndex <= FPosition.CurrentFragment^.CodepointsLength);
            if (FPosition.CodepointsIndex = FPosition.CurrentFragment^.CodepointsLength) then
            begin
               MoveToNext()
            end
            else
            begin
               FPosition.CodepointsIndexIncluded := True;
            end;
         end;
   else
      Assert(False);
   end;
   {$IFOPT C+} FCurrentCharacter := kNone; {$ENDIF}
end;

procedure TRopePointer.SetToZeroWidth();
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: SetToZeroWidth() on rope pointer @', IntToHex(PtrUInt(@Self), 16)); {$ENDIF}
   {$IFOPT C+} Assert(Assigned(FRope)); {$ENDIF}
   {$IFOPT C+} Assert(not IsZeroWidth()); {$ENDIF}
   {$IFOPT C+} Assert(not IsEOF()); {$ENDIF}
   case (FPosition.CurrentFragment^.Kind) of
      rfUTF8Buffer: FPosition.BufferCodepointLength := 0;
      rfUTF8Inline: FPosition.InlineCodepointLength := 0;
      rfCodepoints: FPosition.CodepointsIndexIncluded := False;
   else
      Assert(False);
   end;
   {$IFOPT C+} FCurrentCharacter := kNone; {$ENDIF}
end;


function CutRope.GetIsEmpty(): Boolean;
begin
   Result := Length(FValue) = 0;
end;

function CutRope.GetAsString(): UTF8String;
begin
   {$IFOPT C+} Assert(not FRead); {$ENDIF}
   if (Length(FValue) = 0) then
   begin
      Result := '';
      exit;
   end;
   Result := RopeInternals.Serialise(@FValue[0]);
   {$IFOPT C+} FRead := True; {$ENDIF}
end;

function CutRope.GetAsStringSlow(): UTF8String;
begin
   if (Length(FValue) = 0) then
   begin
      Result := '';
      exit;
   end;
   Result := RopeInternals.Serialise(@FValue[0]);
end;

class function CutRope.CreateFrom(const NewString: TUnicodeCodepointArray): CutRope;
var
   Index: Cardinal;
begin
   Assert(Length(NewString) <= RopeInternals.CodepointsSize);
   Assert(Length(NewString) > 0);
   SetLength(Result.FValue, 1);
   Result.FValue[0].Kind := rfCodepoints;
   for Index := Low(NewString) to High(NewString) do // $R-
      Result.FValue[0].CodepointsValue[Index] := NewString[Index];
   Result.FValue[0].CodepointsLength := Length(NewString); // $R-
   Result.FValue[0].Next := nil;
   {$IFOPT C+} Result.FRead := False; {$ENDIF}
end;

class function CutRope.CreateFrom(const NewString: UTF8String): CutRope;
begin
   Assert(NewString <> '');
   {$IFOPT C+} AssertStringIsConstant(NewString); {$ENDIF}
   SetLength(Result.FValue, 1);
   Result.FValue[0].Kind := rfUTF8Buffer;
   Result.FValue[0].BufferValue := PByte(NewString);
   Result.FValue[0].BufferLength := Length(NewString); // $R-
   Assert(not Assigned(Result.FValue[0].Next));
   {$IFOPT C+} Result.FRead := False; {$ENDIF}
end;


procedure Rope.EnsureSize(const NeededBaseSlots: Cardinal = 1; const NeededSubslots: Cardinal = 1);
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: EnsureSize(', NeededBaseSlots, ', ', NeededSubslots, ') on rope @', IntToHex(PtrUInt(@Self), 16), ' with data @', IntToHex(PtrUInt(FValue), 16)); {$ENDIF}
   Assert(NeededBaseSlots > 0);
   Assert((NeededBaseSlots = 1) or (NeededSubslots = 0));
   if (Length(FValue) = 0) then
   begin
      FLast := nil;
      SetLength(FValue, Max(NeededBaseSlots, kMinBaseSlots));
      if (NeededSubslots > 0) then
      begin
         Assert(Length(FValue) > 0);
         SetLength(FValue[0], Max(NeededSubslots, kMinSubslots));
         FFilledLength := 1;
      end
      else
         FFilledLength := 0;
      FLastArrayFilledLength := 0;
   end
   else
   begin
      Assert(NeededBaseSlots > 0);
      Assert(FFilledLength > 0); // shouldn't ever end up here the second time if we haven't used a base slot
      if ((NeededSubslots = 0) or ((NeededSubslots > 0) and (FLastArrayFilledLength >= Length(FValue[FFilledLength-1])))) then
      begin
         if (FFilledLength+NeededBaseSlots > Length(FValue)) then
         begin
            // XXX should handle overflows here
            SetLength(FValue, Max(FFilledLength + NeededBaseSlots, Length(FValue) * 2)); // XXX // $R-
         end;
         if (NeededSubslots > 0) then
         begin
            SetLength(FValue[FFilledLength], Max(NeededSubslots, kMinSubslots));
            Inc(FFilledLength);
            FLastArrayFilledLength := 0;
         end;
      end;
   end;
end;

function Rope.GetIsEmpty(): Boolean;
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: GetIsEmpty() on rope @', IntToHex(PtrUInt(@Self), 16), ' with data @', IntToHex(PtrUInt(FValue), 16)); {$ENDIF}
   Result := Length(FValue) = 0;
   {$IFOPT C+}
   Assert(Result = not Assigned(FLast));
   {$ENDIF}
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   Result = ', Result); {$ENDIF}
end;

function Rope.GetAsString(): UTF8String;
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: GetAsString() on rope @', IntToHex(PtrUInt(@Self), 16), ' with data @', IntToHex(PtrUInt(FValue), 16)); {$ENDIF}
   if (Length(FValue) = 0) then
   begin
      {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   Empty Rope'); {$ENDIF}
      Result := '';
      exit;
   end;
   Assert(FFilledLength > 0);
   Assert(Length(FValue) >= FFilledLength);
   Assert(Length(FValue) > 0);
   Assert(Length(FValue[0]) > 0);
   Assert(Assigned(FLast));
   Result := RopeInternals.Serialise(@FValue[0][0]);
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   Result = "', Result, '"'); {$ENDIF}
end;


{$IFOPT C+}
procedure Rope.AssertInit();
begin
   Assert(Length(FValue) = 0, 'Somehow this Rope has a value with length ' + IntToStr(Length(FValue)));
   Assert(not Assigned(FLast));
end;
{$ENDIF}


procedure Rope.AppendDestructively(var NewString: CutRope);
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: AppendDestructively(CutRope) on rope @', IntToHex(PtrUInt(@Self), 16), ' with data @', IntToHex(PtrUInt(FValue), 16)); {$ENDIF}
   if (Length(NewString.FValue) > 0) then
   begin
      EnsureSize(1, 0);
      Assert(FFilledLength < Length(FValue));
      FValue[FFilledLength] := NewString.FValue;
      {$IFOPT C+} NewString.FValue := nil; {$ENDIF}
      if (Assigned(FLast)) then
      begin
         FLast^.Next := @FValue[FFilledLength][0];
         Assert(FLast^.Next <> FLast);
      end;
      FLastArrayFilledLength := Length(FValue[FFilledLength]); // $R-
      FLast := @FValue[FFilledLength][FLastArrayFilledLength-1];
      Inc(FFilledLength);
      Assert(not Assigned(FLast^.Next));
   end;
end;

procedure Rope.AppendDestructively(var NewString: Rope);
var
   Index: Cardinal;
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: AppendDestructively(Rope) on rope @', IntToHex(PtrUInt(@Self), 16), ' with data @', IntToHex(PtrUInt(FValue), 16)); {$ENDIF}
   if (Length(NewString.FValue) > 0) then
   begin
      Assert(NewString.FFilledLength > 0);
      Assert(NewString.FFilledLength <= Length(NewString.FValue));
      if (Length(FValue) = 0) then
      begin
         FValue := NewString.FValue;
         FFilledLength := NewString.FFilledLength;
      end
      else
      begin
         Assert(Assigned(FLast));
         if (FFilledLength+NewString.FFilledLength < Length(FValue)) then
            EnsureSize(NewString.FFilledLength, 0);
         Assert(FFilledLength + NewString.FFilledLength <= Length(FValue));
         for Index := 0 to NewString.FFilledLength-1 do // $R-
            FValue[FFilledLength+Index] := NewString.FValue[Index];
         FLast^.Next := @FValue[FFilledLength][0];
         Assert(FLast^.Next <> FLast);
         Inc(FFilledLength, NewString.FFilledLength);
         Assert(NewString.FLast = @FValue[FFilledLength-1][High(FValue[FFilledLength-1])]);
      end;
      FLastArrayFilledLength := NewString.FLastArrayFilledLength;
      FLast := NewString.FLast;
      Assert(not Assigned(FLast^.Next));
      {$IFOPT C+}
      NewString.FValue := nil;
      NewString.FFilledLength := 0;
      NewString.FLastArrayFilledLength := 0;
      NewString.FLast := nil;
      {$ENDIF}
   end;
end;

procedure Rope.Append(const NewString: PUTF8String);
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: Append(PUTF8String) on rope @', IntToHex(PtrUInt(@Self), 16), ' with data @', IntToHex(PtrUInt(FValue), 16)); {$ENDIF}
   Assert(Assigned(NewString));
   Assert(NewString^ <> '');
   EnsureSize(1, 1);
   Assert(FFilledLength > 0);
   Assert(FLastArrayFilledLength < Length(FValue[FFilledLength-1]));
   FValue[FFilledLength-1][FLastArrayFilledLength].Kind := rfUTF8Buffer;
   FValue[FFilledLength-1][FLastArrayFilledLength].BufferValue := PByte(NewString^);
   FValue[FFilledLength-1][FLastArrayFilledLength].BufferLength := Length(NewString^); // $R-
   if (Assigned(FLast)) then
   begin
      FLast^.Next := @FValue[FFilledLength-1][FLastArrayFilledLength];
      Assert(FLast^.Next <> FLast);
   end;
   FLast := @FValue[FFilledLength-1][FLastArrayFilledLength];
   Inc(FLastArrayFilledLength);
end;

procedure Rope.Append(const NewString: RopeInternals.TInlineString);
var
   Index: Cardinal;
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: Append(ShortString) on rope @', IntToHex(PtrUInt(@Self), 16), ' with data @', IntToHex(PtrUInt(FValue), 16)); {$ENDIF}
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   Length(FValue)=', Length(FValue)); {$ENDIF}
   Assert(Length(NewString) <= RopeInternals.UTF8InlineSize, 'Maximum size of short string is ' + IntToStr(RopeInternals.UTF8InlineSize));
   if ((not Assigned(FLast)) or (FLast^.Kind <> rfUTF8Inline) or (RopeInternals.UTF8InlineSize - FLast^.InlineLength < Length(NewString))) then
   begin
      EnsureSize(1, 1);
      {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   now Length(FValue)=', Length(FValue)); {$ENDIF}
      {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   now FFilledLength=', FFilledLength); {$ENDIF}
      {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   now Length(FValue[FFilledLength-1])=', Length(FValue[FFilledLength-1])); {$ENDIF}
      {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   now FLastArrayFilledLength=', FLastArrayFilledLength); {$ENDIF}
      Assert(FFilledLength > 0);
      Assert(FLastArrayFilledLength < Length(FValue[FFilledLength-1]));
      if (Assigned(FLast)) then
      begin
         FLast^.Next := @FValue[FFilledLength-1][FLastArrayFilledLength];
         Assert(FLast^.Next <> FLast);
      end;
      FLast := @FValue[FFilledLength-1][FLastArrayFilledLength];
      FLast^.Kind := rfUTF8Inline;
      FLast^.InlineLength := 0;
      Inc(FLastArrayFilledLength);
   end;
   for Index := 1 to Length(NewString) do
      FLast^.InlineValue[FLast^.InlineLength+Index-1] := Byte(NewString[Index]);
   Inc(FLast^.InlineLength, Length(NewString));
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   content is now: "' + AsString + '"'); {$ENDIF}
end;

procedure Rope.Append(const NewString: TUnicodeCodepoint);
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: Append(TUnicodeCodepoint) on rope @', IntToHex(PtrUInt(@Self), 16), ' with data @', IntToHex(PtrUInt(FValue), 16)); {$ENDIF}
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   Length(FValue)=', Length(FValue)); {$ENDIF}
   EnsureSize(1, 1);
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   now Length(FValue)=', Length(FValue)); {$ENDIF}
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   now FFilledLength=', FFilledLength); {$ENDIF}
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   now Length(FValue[FFilledLength-1])=', Length(FValue[FFilledLength-1])); {$ENDIF}
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   now FLastArrayFilledLength=', FLastArrayFilledLength); {$ENDIF}
   Assert(FFilledLength > 0);
   Assert(FLastArrayFilledLength < Length(FValue[FFilledLength-1]));
   FValue[FFilledLength-1][FLastArrayFilledLength].Kind := rfCodepoints;
   FValue[FFilledLength-1][FLastArrayFilledLength].CodepointsValue[0] := NewString;
   FValue[FFilledLength-1][FLastArrayFilledLength].CodepointsLength := 1;
   if (Assigned(FLast)) then
   begin
      FLast^.Next := @FValue[FFilledLength-1][FLastArrayFilledLength];
      Assert(FLast^.Next <> FLast);
   end;
   FLast := @FValue[FFilledLength-1][FLastArrayFilledLength];
   Inc(FLastArrayFilledLength);
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   content is now: "' + AsString + '"'); {$ENDIF}
end;

procedure Rope.Append(const NewString: TUnicodeCodepointArray);
var
   Index: Cardinal;
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: Append(TUnicodeCodepointArray) on rope @', IntToHex(PtrUInt(@Self), 16), ' with data @', IntToHex(PtrUInt(FValue), 16)); {$ENDIF}
   Assert(Length(NewString) <= RopeInternals.CodepointsSize);
   Assert(Length(NewString) > 0);
   EnsureSize(1, 1);
   Assert(FFilledLength > 0);
   Assert(FLastArrayFilledLength < Length(FValue[FFilledLength-1]));
   FValue[FFilledLength-1][FLastArrayFilledLength].Kind := rfCodepoints;
   for Index := Low(NewString) to High(NewString) do // $R-
      FValue[FFilledLength-1][FLastArrayFilledLength].CodepointsValue[Index] := NewString[Index];
   FValue[FFilledLength-1][FLastArrayFilledLength].CodepointsLength := Length(NewString); // $R-
   if (Assigned(FLast)) then
   begin
      FLast^.Next := @FValue[FFilledLength-1][FLastArrayFilledLength];
      Assert(FLast^.Next <> FLast);
   end;
   FLast := @FValue[FFilledLength-1][FLastArrayFilledLength];
   Inc(FLastArrayFilledLength);
end;

procedure Rope.Append(const NewData: Pointer; const NewLength: QWord);
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: Append(Pointer) on rope @', IntToHex(PtrUInt(@Self), 16), ' with data @', IntToHex(PtrUInt(FValue), 16)); {$ENDIF}
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes:   Data at ', IntToHex(PtrUInt(NewData), 16), ' with length ', NewLength); {$ENDIF}
   Assert(NewLength > 0);
   Assert(NewLength < High(Cardinal));
   EnsureSize(1, 1);
   Assert(FFilledLength > 0);
   Assert(FLastArrayFilledLength < Length(FValue[FFilledLength-1]));
   FValue[FFilledLength-1][FLastArrayFilledLength].Kind := rfUTF8Buffer;
   FValue[FFilledLength-1][FLastArrayFilledLength].BufferValue := NewData;
   // XXX should handle buffers bigger than Cardinal
   FValue[FFilledLength-1][FLastArrayFilledLength].BufferLength := NewLength; // XXX // $R-
   if (Assigned(FLast)) then
   begin
      FLast^.Next := @FValue[FFilledLength-1][FLastArrayFilledLength];
      Assert(FLast^.Next <> FLast);
   end;
   FLast := @FValue[FFilledLength-1][FLastArrayFilledLength];
   Inc(FLastArrayFilledLength);
end;

procedure Rope.AppendPossiblyIncomplete(const NewData: Pointer; const NewLength: QWord);
begin
   Assert(False); // this is not implemented
   // what you'd have to do here is look at the last few characters, and if they could be the start of an incomplete sequence, but them in a separate
   // inline fragment. Then, the next time you add something, see if the previous thing is an inline fragment, and if it is, move the first few characters
   // (as many as are needed to finish reading the sequence) into the same buffer, then move the pointer of the next fragment up accordingly.
end;

function Rope.Extract(constref NewStart, NewEnd: TRopePointer): CutRope;
var
   FragmentCount, FragmentIndex: Cardinal;
   CurrentFragment: RopeInternals.PRopeFragment;
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: Extract() on rope @', IntToHex(PtrUInt(@Self), 16), ' with data @', IntToHex(PtrUInt(FValue), 16)); {$ENDIF}

   {$IFOPT C+}
   Assert(NewStart.FRope = @Self);
   Assert(NewEnd.FRope = @Self);
   {$ENDIF}

   FragmentCount := 1;
   CurrentFragment := NewStart.FPosition.CurrentFragment;
   Assert(Assigned(CurrentFragment));
   while (CurrentFragment <> NewEnd.FPosition.CurrentFragment) do
   begin
      Assert(CurrentFragment <> CurrentFragment^.Next);
      CurrentFragment := CurrentFragment^.Next;
      Assert(Assigned(CurrentFragment));
      Inc(FragmentCount);
   end;
   SetLength(Result.FValue, FragmentCount);
   FragmentIndex := 0;
   CurrentFragment := NewStart.FPosition.CurrentFragment;
   Result.FValue[FragmentIndex] := CurrentFragment^;
   while (CurrentFragment <> NewEnd.FPosition.CurrentFragment) do
   begin
      Inc(FragmentIndex);
      Assert(CurrentFragment <> CurrentFragment^.Next);
      CurrentFragment := CurrentFragment^.Next;
      Assert(Assigned(CurrentFragment));
      Result.FValue[FragmentIndex] := CurrentFragment^;
   end;
   Assert(FragmentIndex = FragmentCount-1);
   if (FragmentIndex > 1) then
      for FragmentIndex := Low(Result.FValue) to High(Result.FValue)-1 do // $R-
      begin
         Result.FValue[FragmentIndex].Next := @Result.FValue[FragmentIndex+1];
         Assert(Result.FValue[FragmentIndex].Next <> @Result.FValue[FragmentIndex]);
      end;
   Result.FValue[High(Result.FValue)].Next := nil;
   Assert(not Assigned(Result.FValue[FragmentCount-1].Next));
   case (NewStart.FPosition.CurrentFragment^.Kind) of
      rfUTF8Buffer:
         begin
            Inc(Result.FValue[0].BufferValue, NewStart.FPosition.BufferIndex);
            Dec(Result.FValue[0].BufferLength, NewStart.FPosition.BufferIndex);
         end;
      rfUTF8Inline:
         begin
            Assert(Result.FValue[0].InlineLength-NewStart.FPosition.InlineIndex > 0);
            Move(Result.FValue[0].InlineValue[NewStart.FPosition.InlineIndex], Result.FValue[0].InlineValue[0], Result.FValue[0].InlineLength-NewStart.FPosition.InlineIndex);
            Dec(Result.FValue[0].InlineLength, NewStart.FPosition.InlineIndex);
         end;
      rfCodepoints:
         begin
            Assert(Result.FValue[0].CodepointsLength-NewStart.FPosition.CodepointsIndex > 0);
            Move(Result.FValue[0].CodepointsValue[NewStart.FPosition.CodepointsIndex], Result.FValue[0].CodepointsValue[0], (Result.FValue[0].CodepointsLength-NewStart.FPosition.CodepointsIndex) * SizeOf(TUnicodeCodepoint));
            Dec(Result.FValue[0].CodepointsLength, NewStart.FPosition.CodepointsIndex);
         end;
   else
      Assert(False);
   end;
   case (NewEnd.FPosition.CurrentFragment^.Kind) of
      rfUTF8Buffer:
         Result.FValue[High(Result.FValue)].BufferLength := PtrUInt(NewEnd.FPosition.CurrentFragment^.BufferValue) + NewEnd.FPosition.BufferIndex + NewEnd.FPosition.BufferCodepointLength - PtrUInt(Result.FValue[High(Result.FValue)].BufferValue); // $R-
      rfUTF8Inline:
         begin
            Assert(Result.FValue[High(Result.FValue)].InlineLength >= NewEnd.FPosition.InlineIndex + NewEnd.FPosition.InlineCodepointLength);
            Result.FValue[High(Result.FValue)].InlineLength := NewEnd.FPosition.InlineIndex + NewEnd.FPosition.InlineCodepointLength; // $R-
         end;
      rfCodepoints:
         begin
            if (NewEnd.FPosition.CodepointsIndexIncluded) then
            begin
               Assert(Result.FValue[High(Result.FValue)].CodepointsLength >= NewEnd.FPosition.CodepointsIndex + 1);
               Result.FValue[High(Result.FValue)].CodepointsLength := NewEnd.FPosition.CodepointsIndex + 1; // $R-
            end
            else
            begin
               Assert(Result.FValue[High(Result.FValue)].CodepointsLength >= NewEnd.FPosition.CodepointsIndex);
               Result.FValue[High(Result.FValue)].CodepointsLength := NewEnd.FPosition.CodepointsIndex;
            end;
         end;
   else
      Assert(False);
   end;
   {$IFOPT C+} Result.FRead := False; {$ENDIF}
end;

function Rope.ExtractAll(): CutRope;
var
   FragmentCount, FragmentIndex: Cardinal;
   CurrentFragment: RopeInternals.PRopeFragment;
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: ExtractAll() on rope @', IntToHex(PtrUInt(@Self), 16), ' with data @', IntToHex(PtrUInt(FValue), 16)); {$ENDIF}
   FragmentCount := 0;
   if (Length(FValue) > 0) then
   begin
      Assert(FFilledLength > 0);
      Assert(Assigned(FLast));
      CurrentFragment := @FValue[0][0];
      Assert(Assigned(CurrentFragment));
      repeat
         Inc(FragmentCount);
         Assert(CurrentFragment <> CurrentFragment^.Next);
         CurrentFragment := CurrentFragment^.Next;
      until (not Assigned(CurrentFragment));
      SetLength(Result.FValue, FragmentCount);
      FragmentIndex := 0;
      CurrentFragment := @FValue[0][0];
      repeat
         Result.FValue[FragmentIndex] := CurrentFragment^;
         Inc(FragmentIndex);
         Assert(CurrentFragment <> CurrentFragment^.Next);
         CurrentFragment := CurrentFragment^.Next;
      until (not Assigned(CurrentFragment));
      if (FragmentIndex > 1) then
         for FragmentIndex := Low(Result.FValue) to High(Result.FValue)-1 do // $R-
         begin
            Result.FValue[FragmentIndex].Next := @Result.FValue[FragmentIndex+1];
            Assert(Result.FValue[FragmentIndex].Next <> @Result.FValue[FragmentIndex]);
         end;
      Result.FValue[High(Result.FValue)].Next := nil;
      Assert(not Assigned(Result.FValue[FragmentCount-1].Next));
      {$IFOPT C+} Result.FRead := False; {$ENDIF}
   end
   else
   begin
      Result := Default(CutRope);
   end;
end;

function Rope.ExtractToEnd(constref NewStart: TRopePointer): CutRope;
var
   FragmentCount, FragmentIndex: Cardinal;
   CurrentFragment: RopeInternals.PRopeFragment;
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: ExtractToEnd() on rope @', IntToHex(PtrUInt(@Self), 16), ' with data @', IntToHex(PtrUInt(FValue), 16)); {$ENDIF}
   {$IFOPT C+} Assert(NewStart.FRope = @Self); {$ENDIF}
   FragmentCount := 0;
   CurrentFragment := NewStart.FPosition.CurrentFragment;
   Assert(Assigned(CurrentFragment));
   repeat
      Inc(FragmentCount);
      Assert(CurrentFragment <> CurrentFragment^.Next);
      CurrentFragment := CurrentFragment^.Next;
   until not Assigned(CurrentFragment);
   SetLength(Result.FValue, FragmentCount);
   FragmentIndex := 0;
   CurrentFragment := NewStart.FPosition.CurrentFragment;
   repeat
      Result.FValue[FragmentIndex] := CurrentFragment^;
      Assert(CurrentFragment <> CurrentFragment^.Next);
      CurrentFragment := CurrentFragment^.Next;
      Inc(FragmentIndex);
   until not Assigned(CurrentFragment);
   Assert(FragmentIndex = FragmentCount);
   if (FragmentIndex > 1) then
      for FragmentIndex := Low(Result.FValue) to High(Result.FValue)-1 do // $R-
      begin
         Result.FValue[FragmentIndex].Next := @Result.FValue[FragmentIndex+1];
         Assert(Result.FValue[FragmentIndex].Next <> @Result.FValue[FragmentIndex]);
      end;
   Result.FValue[High(Result.FValue)].Next := nil;
   Assert(not Assigned(Result.FValue[FragmentCount-1].Next));
   case (NewStart.FPosition.CurrentFragment^.Kind) of
      rfUTF8Buffer:
         begin
            Inc(Result.FValue[0].BufferValue, NewStart.FPosition.BufferIndex);
            Dec(Result.FValue[0].BufferLength, NewStart.FPosition.BufferIndex);
         end;
      rfUTF8Inline:
         begin
            Assert(Result.FValue[0].InlineLength-NewStart.FPosition.InlineIndex > 0);
            Move(Result.FValue[0].InlineValue[NewStart.FPosition.InlineIndex], Result.FValue[0].InlineValue[0], Result.FValue[0].InlineLength-NewStart.FPosition.InlineIndex);
            Dec(Result.FValue[0].InlineLength, NewStart.FPosition.InlineIndex);
         end;
      rfCodepoints:
         begin
            Assert(Result.FValue[0].CodepointsLength-NewStart.FPosition.CodepointsIndex > 0);
            Move(Result.FValue[0].CodepointsValue[NewStart.FPosition.CodepointsIndex], Result.FValue[0].CodepointsValue[0], (Result.FValue[0].CodepointsLength-NewStart.FPosition.CodepointsIndex) * SizeOf(TUnicodeCodepoint));
            Dec(Result.FValue[0].CodepointsLength, NewStart.FPosition.CodepointsIndex);
         end;
   else
      Assert(False);
   end;
   {$IFOPT C+} Result.FRead := False; {$ENDIF}
end;

function Rope.CountCharacters(constref NewStart, NewEnd: TRopePointer): Cardinal;
var
   Index: TRopePointer;
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: CountCharacters() on rope @', IntToHex(PtrUInt(@Self), 16), ' with data @', IntToHex(PtrUInt(FValue), 16)); {$ENDIF}

   {$IFOPT C+}
   Assert(NewStart.FRope = @Self);
   Assert(NewEnd.FRope = @Self);
   Assert(NewStart <= NewEnd);
   {$ENDIF}

   Result := 0;
   Index := NewStart;
   while (Index <> NewEnd) do
   begin
      Inc(Result);
      Index.AdvanceToNextFaster();
   end;
   case (NewEnd.FPosition.CurrentFragment^.Kind) of
      rfUTF8Buffer: if (NewEnd.FPosition.BufferCodepointLength > 0) then Inc(Result);
      rfUTF8Inline: if (NewEnd.FPosition.InlineCodepointLength > 0) then Inc(Result);
      rfCodepoints: if (NewEnd.FPosition.CodepointsIndexIncluded) then Inc(Result);
   else
      Assert(False);
   end;
end;

function Rope.GetEnumerator(): RopeEnumerator;
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: GetEnumerator() on rope @', IntToHex(PtrUInt(@Self), 16), ' with data @', IntToHex(PtrUInt(FValue), 16)); {$ENDIF}
   Result := RopeEnumerator.Create(@Self);
end;


constructor RopeEnumerator.Create(const NewTarget: PRope);
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: Create() on rope enumerator @', IntToHex(PtrUInt(Self), 16)); {$ENDIF}
   {$IFOPT C+} FPointer.FRope := NewTarget; {$ENDIF}
   Assert(Assigned(NewTarget));
   if (Length(NewTarget^.FValue) > 0) then
   begin
      Assert(Assigned(NewTarget^.FLast));
      Assert(NewTarget^.FFilledLength > 0);
      Assert(Length(NewTarget^.FValue) > 0);
      Assert(Length(NewTarget^.FValue[0]) > 0);
      Assert(not NewTarget^.IsEmpty);
      Assert(NewTarget^.AsString <> '');
      FPointer.FPosition.CurrentFragment := @(NewTarget^.FValue[0][0]);
      {$IFOPT C+} FPointer.FCurrentCharacter := kNone; {$ENDIF}
      {$IFOPT C+}
      case FPointer.FPosition.CurrentFragment^.Kind of // CurrentFragment.Kind
         rfUTF8Buffer:
            begin
               Assert(FPointer.FPosition.BufferIndex = 0);
               Assert(FPointer.FPosition.BufferCodepointLength = 0);
            end;
         rfUTF8Inline:
            begin
               Assert(FPointer.FPosition.InlineIndex = 0);
               Assert(FPointer.FPosition.InlineCodepointLength = 0);
            end;
         rfCodepoints:
            begin
               FPointer.FPosition.CodepointsIndex := 0;
               FPointer.FPosition.CodepointsIndexIncluded := False;
            end;
         else
            Assert(False);
      end;
      {$ENDIF}

      {$IFDEF VERBOSE}
         if (DebugNow) then
         begin
            Writeln('Ropes:   enumerator is for rope with text: ' + NewTarget^.AsString);
            if (FPointer.IsEOF()) then
               Writeln('Ropes:   enumerator at end of file')
            else
               Writeln('Ropes:   enumerator has data');
         end;
      {$ENDIF}

   end
   else
   begin
      Assert(not Assigned(NewTarget^.FLast));
      Assert(NewTarget^.FFilledLength = 0);
      Assert(Length(NewTarget^.FValue) = 0);
      Assert(NewTarget^.IsEmpty);
      Assert(NewTarget^.AsString = '');
      Assert(not Assigned(FPointer.FPosition.CurrentFragment));

      {$IFOPT C+}
         FPointer.FCurrentCharacter := kEOF;
         Assert(FPointer.IsEOF());
      {$ENDIF}

   end;
end;


{$IFOPT C+}
procedure RopeEnumerator.AssertIdentity(constref Target: Rope);
begin
   FPointer.AssertIdentity(@Target);
end;
{$ENDIF}


function RopeEnumerator.MoveNext(): Boolean;
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: MoveNext() on rope enumerator @', IntToHex(PtrUInt(Self), 16)); {$ENDIF}
   if (Assigned(FPointer.FPosition.CurrentFragment)) then
   begin
      {$IFOPT C+} Assert(not FPointer.IsEOF()); {$ENDIF}
      FPointer.AdvanceToNext();
      Result := FPointer.FCurrentCharacter <> kEOF;
   end
   else
   begin
      Assert(FPointer.FCurrentCharacter = kEOF);
      Result := False;
   end;
end;

function RopeEnumerator.GetPointer(): TRopePointer;
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: GetPointer() on rope enumerator @', IntToHex(PtrUInt(Self), 16)); {$ENDIF}
   Result := FPointer;
end;

procedure RopeEnumerator.ReturnToPointer(constref NewPointer: TRopePointer);
begin
   {$IFDEF VERBOSE} if (DebugNow) then Writeln('Ropes: ReturnToPointer() on rope enumerator @', IntToHex(PtrUInt(Self), 16)); {$ENDIF}
   {$IFOPT C+} Assert(NewPointer.FRope = FPointer.FRope); {$ENDIF}
   FPointer := NewPointer;
end;

function RopeEnumerator.GetCurrentAsUTF8(): TUTF8Sequence;
begin
   Result := CodepointToUTF8(FPointer.FCurrentCharacter);
end;

operator = (constref Op1, Op2: TRopePointer): Boolean;
begin {BOGUS Warning: Function result variable does not seem to initialized}
   {$IFOPT C+} Assert(Op1.FRope = Op2.FRope); {$ENDIF}
   if (Op1.FPosition.CurrentFragment = Op2.FPosition.CurrentFragment) then
   begin
      Assert(Assigned(Op1.FPosition.CurrentFragment));
      case (Op1.FPosition.CurrentFragment^.Kind) of
         rfUTF8Buffer: Result := (Op1.FPosition.BufferIndex = Op2.FPosition.BufferIndex) and (Op1.FPosition.BufferCodepointLength = Op2.FPosition.BufferCodepointLength);
         rfUTF8Inline: Result := (Op1.FPosition.InlineIndex = Op2.FPosition.InlineIndex) and (Op1.FPosition.InlineCodepointLength = Op2.FPosition.InlineCodepointLength);
         rfCodepoints: Result := (Op1.FPosition.CodepointsIndex = Op2.FPosition.CodepointsIndex) and (Op1.FPosition.CodepointsIndexIncluded = Op2.FPosition.CodepointsIndexIncluded);
         else Assert(False);
      end;
   end
   else
      Result := False;
end;

operator < (constref Op1, Op2: TRopePointer): Boolean;
var
   Fragment: RopeInternals.PRopeFragment;
begin {BOGUS Warning: Function result variable does not seem to initialized}
   {$IFOPT C+} Assert(Op1.FRope = Op2.FRope); {$ENDIF}
   if (Op1.FPosition.CurrentFragment = Op2.FPosition.CurrentFragment) then
   begin
      Assert(Assigned(Op1.FPosition.CurrentFragment));
      case (Op1.FPosition.CurrentFragment^.Kind) of
         rfUTF8Buffer: Result := (Op1.FPosition.BufferIndex < Op2.FPosition.BufferIndex) or
                                 ((Op1.FPosition.BufferIndex = Op2.FPosition.BufferIndex) and (Op1.FPosition.BufferCodepointLength < Op2.FPosition.BufferCodepointLength));
         rfUTF8Inline: Result := (Op1.FPosition.InlineIndex < Op2.FPosition.InlineIndex) or
                                 ((Op1.FPosition.InlineIndex = Op2.FPosition.InlineIndex) and (Op1.FPosition.InlineCodepointLength < Op2.FPosition.InlineCodepointLength));
         rfCodepoints: Result := (Op1.FPosition.CodepointsIndex < Op2.FPosition.CodepointsIndex) or
                                 ((Op1.FPosition.CodepointsIndex = Op2.FPosition.CodepointsIndex) and (not Op1.FPosition.CodepointsIndexIncluded) and (Op2.FPosition.CodepointsIndexIncluded));
         else Assert(False);
      end;
   end
   else
   begin
      Fragment := Op1.FPosition.CurrentFragment;
      repeat
         if (Fragment = Op2.FPosition.CurrentFragment) then
         begin
            Result := True;
            exit;
         end;
         Assert(Fragment <> Fragment^.Next);
         Fragment := Fragment^.Next;
      until not Assigned(Fragment);
      Result := False;
   end;
end;

operator <= (constref Op1, Op2: TRopePointer): Boolean;
var
   Fragment: RopeInternals.PRopeFragment;
begin {BOGUS Warning: Function result variable does not seem to initialized}
   {$IFOPT C+} Assert(Op1.FRope = Op2.FRope); {$ENDIF}
   if (Op1.FPosition.CurrentFragment = Op2.FPosition.CurrentFragment) then
   begin
      Assert(Assigned(Op1.FPosition.CurrentFragment));
      case (Op1.FPosition.CurrentFragment^.Kind) of
         rfUTF8Buffer: Result := (Op1.FPosition.BufferIndex < Op2.FPosition.BufferIndex) or
                                 ((Op1.FPosition.BufferIndex = Op2.FPosition.BufferIndex) and (Op1.FPosition.BufferCodepointLength <= Op2.FPosition.BufferCodepointLength));
         rfUTF8Inline: Result := (Op1.FPosition.InlineIndex < Op2.FPosition.InlineIndex) or
                                 ((Op1.FPosition.InlineIndex = Op2.FPosition.InlineIndex) and (Op1.FPosition.InlineCodepointLength <= Op2.FPosition.InlineCodepointLength));
         rfCodepoints: Result := (Op1.FPosition.CodepointsIndex < Op2.FPosition.CodepointsIndex) or
                                 ((Op1.FPosition.CodepointsIndex = Op2.FPosition.CodepointsIndex) and ((not Op1.FPosition.CodepointsIndexIncluded) or (Op2.FPosition.CodepointsIndexIncluded)));
         else Assert(False);
      end;
   end
   else
   begin
      Fragment := Op1.FPosition.CurrentFragment;
      repeat
         if (Fragment = Op2.FPosition.CurrentFragment) then
         begin
            Result := True;
            exit;
         end;
         Assert(Fragment <> Fragment^.Next);
         Fragment := Fragment^.Next;
      until not Assigned(Fragment);
      Result := False;
   end;
end;

operator = (constref Op1, Op2: Rope): Boolean;
var
   E1, E2: RopeEnumerator;
   GotMore1, GotMore2: Boolean;

   function IsNext(): Boolean;
   begin
      GotMore1 := E1.MoveNext();
      GotMore2 := E2.MoveNext();
      Result := (GotMore1) and (GotMore2);
   end;

begin
   E1 := RopeEnumerator.Create(@Op1);
   E2 := RopeEnumerator.Create(@Op2);
   while (IsNext()) do
   begin
      if (E1.Current <> E2.Current) then
      begin
         Result := False;
         E1.Free();
         E2.Free();
         exit;
      end;
   end;
   Result := (not GotMore1) and  {BOGUS Warning: Local variable "GotMore1" does not seem to be initialized}
             (not GotMore2);     {BOGUS Warning: Local variable "GotMore2" does not seem to be initialized}
   E1.Free();
   E2.Free();
end;

end.
