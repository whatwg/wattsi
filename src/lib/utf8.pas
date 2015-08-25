{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
{$CODEPAGE UTF8}
unit utf8;

interface

//{$DEFINE TESTS}

uses
   unicode;

type
   TUTF8SequenceLength = 1..4;
   TZeroableUTF8SequenceLength = 0..4;
   TUTF8Sequence = packed record
     case Byte of
        0: (Length: TUTF8SequenceLength;
            case Byte of 
               0: (Byte1, Byte2, Byte3, Byte4: Byte);
               1: (Start: record end));
        1: (AsString: String[4]);
   end;
   operator := (const Value: TUTF8Sequence): UTF8String; deprecated;
   operator + (const Op1: TUTF8Sequence; const Op2: UTF8String): UTF8String;
   operator + (const Op1: UTF8String; const Op2: TUTF8Sequence): UTF8String;
   operator + (const Op1, Op2: TUTF8Sequence): UTF8String;

function IsValidUTF8(const Value: RawByteString): Boolean;

function UTF8ToCodepoint(const Data: PByte; out BytesRead: TZeroableUTF8SequenceLength): TUnicodeCodepoint; inline;
function UTF8ToCodepoint(const Data: PByte; out BytesRead: TUTF8SequenceLength): TUnicodeCodepoint; inline;
function UTF8ToUTF8Length(const Data: PByte): TUTF8SequenceLength; inline;
  // WARNING: There are no length guards in these!
  // Value needs to be at least 4 bytes long if you have no idea what it is.
  // If it is less than four bytes, then there needs to be at least one byte
  // with its high bit unset in the first three bytes.
  // A simple way to guard against this is to add a trailing #0 to your input.

function UTF8ToCodepoint(const Data: PUTF8String; const Position: Cardinal; out BytesRead: TZeroableUTF8SequenceLength): TUnicodeCodepoint; inline;
function UTF8ToCodepoint(const Data: PUTF8String; const Position: Cardinal; out BytesRead: TUTF8SequenceLength): TUnicodeCodepoint; inline;
function UTF8ToUTF8Length(const Data: PUTF8String; const Position: Cardinal): TUTF8SequenceLength; inline;
  // these handle the length guarding

function CodepointToUTF8(const Value: TUnicodeCodepoint): TUTF8Sequence; inline;
function CodepointToUTF8Length(const Value: TUnicodeCodepoint): TUTF8SequenceLength; inline;

implementation

uses exceptions, sysutils;

// TUTF8Sequence

operator := (const Value: TUTF8Sequence): UTF8String;
begin
   Result := Value.AsString;
end;

operator + (const Op1: TUTF8Sequence; const Op2: UTF8String): UTF8String;
begin
   Result := Op1.AsString + Op2;
end;

operator + (const Op1: UTF8String; const Op2: TUTF8Sequence): UTF8String;
begin
   Result := Op1 + Op2.AsString;
end;

operator + (const Op1, Op2: TUTF8Sequence): UTF8String;
begin
   Result := Op1.AsString + Op2.AsString;
end;

// API

function IsValidUTF8(const Value: RawByteString): Boolean;
var
   Index: Cardinal;

   function ConsumeContinuationByte(): Boolean; inline;
   begin
      Inc(Index);
      Result := (Index <= Length(Value)) and (Ord(Value[Index]) in [$80..$BF]);
   end;

var
   C: Char;
   Codepoint: Cardinal;
begin
   Result := False;
   Index := 1;
   while (Index <= Length(Value)) do
   begin
      C := Value[Index];
      case Ord(C) of
         $80..$BF: Exit; // unexpected continuation byte
         $C0, $C1: Exit; // forcibly an overlong sequence
         $C2..$DF: if (not ConsumeContinuationByte()) then Exit; // two-byte sequence
         $E0: // three-byte sequence with possible overlong sequence
           begin
              if (not (ConsumeContinuationByte() and ConsumeContinuationByte())) then Exit; // short
              Codepoint := ((Ord(Value[Index-1])-$80) shl 6) + (Ord(Value[Index])-$80); // $R-
              if (Codepoint < $0800) then
                 Exit; // overlong
           end;
         $E1..$EC: if (not (ConsumeContinuationByte() and ConsumeContinuationByte())) then Exit; // three-byte sequence
         $ED: // three-byte sequence with possible surrogates
           begin
              if (not (ConsumeContinuationByte() and ConsumeContinuationByte())) then Exit; // short
              Codepoint := $D000 + ((Ord(Value[Index-1])-$80) shl 6) + (Ord(Value[Index])-$80); // $R-
              Assert(Codepoint < $E000);
              if (Codepoint >= $D800) then
                 Exit; // surrogate
           end;
         $EE..$EF: if (not (ConsumeContinuationByte() and ConsumeContinuationByte())) then Exit; // three-byte sequence
         $F0: // four-byte sequence with possible overlong sequence
           begin
              if (not (ConsumeContinuationByte() and ConsumeContinuationByte() and ConsumeContinuationByte())) then Exit; // short
              Codepoint := ((Ord(Value[Index-2])-$80) shl 12) + ((Ord(Value[Index-1])-$80) shl 6) + (Ord(Value[Index])-$80); // $R-
              if (Codepoint < $10000) then
                 Exit; // overlong
           end;
         $F1..$F3: if (not (ConsumeContinuationByte() and ConsumeContinuationByte() and ConsumeContinuationByte())) then Exit; // four-byte sequence
         $F4: // four-byte sequence with possible too-high codepoint
           begin
              if (not (ConsumeContinuationByte() and ConsumeContinuationByte() and ConsumeContinuationByte())) then Exit; // short
              Codepoint := $100000 + ((Ord(Value[Index-2])-$80) shl 12) + ((Ord(Value[Index-1])-$80) shl 6) + (Ord(Value[Index])-$80); // $R-
              if (Codepoint > $10FFFF) then
                 Exit; // overlong
           end;
         $F5..$FD: Exit; // forcibly an invalid Unicode character
         $FE, $FF: Exit; // non-UTF-8 bytes
      end;
      Inc(Index);
   end;
   Result := True;
end;

function UTF8ToCodepoint(const Data: PByte; out BytesRead: TUTF8SequenceLength): TUnicodeCodepoint;

   procedure Return(const Codepoint: TUnicodeCodepoint; const Length: TUTF8SequenceLength); inline;
   begin
      Result := Codepoint;
      BytesRead := Length;
   end;

begin
   case (Data+0)^ of
      $00..$7F: Return((Data+0)^, 1); // ASCII
      $C2..$DF:
         if ((Data+1)^ in [$80..$BF]) then
            Return(((Data+0)^ and %00011111) shl 6 +
                   ((Data+1)^ and %00111111), 2) // $R-
         else // missing continuation byte
            Return($FFFD, 1);
      $E0: // three-byte sequence with possible overlong sequence
         if ((Data+1)^ in [$A0..$BF]) then
         begin
            if ((Data+2)^ in [$80..$BF]) then
            begin
               Assert(((Data+0)^ and %00001111) = 0);
               Return({((Data+0)^ and %00001111) shl 12 +} // always zero
                       ((Data+1)^ and %00111111) shl 6 +
                       ((Data+2)^ and %00111111), 3); // $R-
            end
            else
               Return($FFFD, 2);
         end
         else
            Return($FFFD, 1);
      $E1..$EC, $EE..$EF: // three-byte sequence
         if ((Data+1)^ in [$80..$BF]) then
         begin
            if ((Data+2)^ in [$80..$BF]) then
            begin
               Return(((Data+0)^ and %00001111) shl 12 +
                      ((Data+1)^ and %00111111) shl 6 +
                      ((Data+2)^ and %00111111), 3); // $R-
            end
            else
               Return($FFFD, 2);
         end
         else
            Return($FFFD, 1);
      $ED: // three-byte sequence with possible surrogates
         if ((Data+1)^ in [$80..$9F]) then
         begin
            if ((Data+2)^ in [$80..$BF]) then
            begin
               Return(((Data+0)^ and %00001111) shl 12 +
                      ((Data+1)^ and %00111111) shl 6 +
                      ((Data+2)^ and %00111111), 3); // $R-
               Assert(Result.Value < $D800);
            end
            else
               Return($FFFD, 2);
         end
         else
            Return($FFFD, 1);
      $F0: // four-byte sequence with possible overlong sequence
         if ((Data+1)^ in [$90..$BF]) then
         begin
            if ((Data+2)^ in [$80..$BF]) then
            begin
               if ((Data+3)^ in [$80..$BF]) then
               begin
                  Assert(((Data+0)^ and %00001111) = 0);
                  Return({((Data+0)^ and %00001111) shl 18 +} // always zero
                          ((Data+1)^ and %00111111) shl 12 +
                          ((Data+2)^ and %00111111) shl 6 +
                          ((Data+3)^ and %00111111), 4); // $R-
               end
               else
                  Return($FFFD, 3);
            end
            else
               Return($FFFD, 2);
         end
         else
            Return($FFFD, 1);
      $F1..$F3:
         if ((Data+1)^ in [$80..$BF]) then
         begin
            if ((Data+2)^ in [$80..$BF]) then
            begin
               if ((Data+3)^ in [$80..$BF]) then
               begin
                  Return(((Data+0)^ and %00001111) shl 18 +
                         ((Data+1)^ and %00111111) shl 12 +
                         ((Data+2)^ and %00111111) shl 6 +
                         ((Data+3)^ and %00111111), 4); // $R-
               end
               else
                  Return($FFFD, 3);
            end
            else
               Return($FFFD, 2);
         end
         else
            Return($FFFD, 1);
      $F4: // four-byte sequence with possible too-high codepoint
         if ((Data+1)^ in [$80..$8F]) then
         begin
            if ((Data+2)^ in [$80..$BF]) then
            begin
               if ((Data+3)^ in [$80..$BF]) then
               begin
                  Return(((Data+0)^ and %00001111) shl 18 +
                         ((Data+1)^ and %00111111) shl 12 +
                         ((Data+2)^ and %00111111) shl 6 +
                         ((Data+3)^ and %00111111), 4); // $R-
                  Assert(Result.Value <= $10FFFF);
               end
               else
                  Return($FFFD, 3);
            end
            else
               Return($FFFD, 2);
         end
         else
            Return($FFFD, 1);
   else
      // $80..$BF, // unexpected continuation byte
      // $C0, $C1, // forcibly an overlong sequence
      // $F5..$FD, // forcibly an invalid Unicode character
      // $FE, $FF: // non-UTF-8 bytes
         Return($FFFD, 1);
   end;
   Assert((Result.Value < $D800) or (Result.Value > $DFFF)); // not a surrogate // $DFA- for Result
   Assert(Result.Value <= $10FFFF);
end;

function UTF8ToCodepoint(const Data: PByte; out BytesRead: TZeroableUTF8SequenceLength): TUnicodeCodepoint;
begin
   Result := UTF8ToCodepoint(Data, TUTF8SequenceLength(BytesRead));
end;

function UTF8ToCodepoint(const Data: PUTF8String; const Position: Cardinal; out BytesRead: TUTF8SequenceLength): TUnicodeCodepoint;
var
   Buffer: array[0..3] of AnsiChar;
begin
   if (Position+3 <= Length(Data^)) then
   begin
      Result := UTF8ToCodepoint(PByte(PPointer(Data)^+Position-1), BytesRead);
   end
   else
   begin
      Buffer[0] := Data^[Position];
      if (Position+1 <= Length(Data^)) then
      begin
         Buffer[1] := Data^[Position+1];
         if (Position+2 <= Length(Data^)) then
         begin
            Buffer[2] := Data^[Position+2];
            Assert(Position+3 > Length(Data^));
            Buffer[3] := #0;
         end
         else
         begin
            Buffer[2] := #0;
         end;
      end
      else
      begin
         Buffer[1] := #0;
      end;
      Result := UTF8ToCodepoint(PByte(@Buffer), BytesRead);
   end;
end;

function UTF8ToCodepoint(const Data: PUTF8String; const Position: Cardinal; out BytesRead: TZeroableUTF8SequenceLength): TUnicodeCodepoint;
begin
   Result := UTF8ToCodepoint(Data, Position, TUTF8SequenceLength(BytesRead));
end;

function UTF8ToUTF8Length(const Data: PByte): TUTF8SequenceLength; inline;
begin
   case (Data+0)^ of
      $00..$7F: Result := 1; // ASCII
      $C2..$DF:
         if ((Data+1)^ in [$80..$BF]) then
            Result := 2
         else // missing continuation byte
            Result := 1;
      $E0: // three-byte sequence with possible overlong sequence
         if ((Data+1)^ in [$A0..$BF]) then
         begin
            if ((Data+2)^ in [$80..$BF]) then
               Result := 3
            else
               Result := 2;
         end
         else
            Result := 1;
      $E1..$EC, $EE..$EF: // three-byte sequence
         if ((Data+1)^ in [$80..$BF]) then
         begin
            if ((Data+2)^ in [$80..$BF]) then
               Result := 3
            else
               Result := 2;
         end
         else
            Result := 1;
      $ED: // three-byte sequence with possible surrogates
         if ((Data+1)^ in [$80..$9F]) then
         begin
            if ((Data+2)^ in [$80..$BF]) then
               Result := 3
            else
               Result := 2;
         end
         else
            Result := 1;
      $F0: // four-byte sequence with possible overlong sequence
         if ((Data+1)^ in [$90..$BF]) then
         begin
            if ((Data+2)^ in [$80..$BF]) then
            begin
               if ((Data+3)^ in [$80..$BF]) then
                  Result := 4
               else
                  Result := 3;
            end
            else
               Result := 2;
         end
         else
            Result := 1;
      $F1..$F3:
         if ((Data+1)^ in [$80..$BF]) then
         begin
            if ((Data+2)^ in [$80..$BF]) then
            begin
               if ((Data+3)^ in [$80..$BF]) then
                  Result := 4
               else
                  Result := 3;
            end
            else
               Result := 2;
         end
         else
            Result := 1;
      $F4: // four-byte sequence with possible too-high codepoint
         if ((Data+1)^ in [$80..$8F]) then
         begin
            if ((Data+2)^ in [$80..$BF]) then
            begin
               if ((Data+3)^ in [$80..$BF]) then
                  Result := 4
               else
                  Result := 3;
            end
            else
               Result := 2;
         end
         else
            Result := 1;
   else
      // $80..$BF, // unexpected continuation byte
      // $C0, $C1, // forcibly an overlong sequence
      // $F5..$FD, // forcibly an invalid Unicode character
      // $FE, $FF: // non-UTF-8 bytes
         Result := 1;
   end;
end;

function UTF8ToUTF8Length(const Data: PUTF8String; const Position: Cardinal): TUTF8SequenceLength;
var
   Buffer: array[0..3] of AnsiChar;
begin
   if (Position+3 <= Length(Data^)) then
   begin
      Result := UTF8ToUTF8Length(PByte(PPointer(Data)^+Position-1));
   end
   else
   begin
      Buffer[0] := Data^[Position];
      if (Position+1 <= Length(Data^)) then
      begin
         Buffer[1] := Data^[Position+1];
         if (Position+2 <= Length(Data^)) then
         begin
            Buffer[2] := Data^[Position+2];
            Assert(Position+3 > Length(Data^));
            Buffer[3] := #0;
         end
         else
         begin
            Buffer[2] := #0;
         end;
      end
      else
      begin
         Buffer[1] := #0;
      end;
      Result := UTF8ToUTF8Length(PByte(@Buffer));
   end;
end;

function CodepointToUTF8(const Value: TUnicodeCodepoint): TUTF8Sequence;
begin
   Assert(Value.Value <> kEOF, 'Cannot convert virtual EOF codepoint to UTF-8');
   case (Value.Value) of
      $000000 .. $00007F:
         begin
            Result.Length := 1;
            Result.Byte1 := Byte(Value.Value);
            Exit;
         end;
      $000080 .. $0007FF:
         begin
            Result.Length := 2;
            Result.Byte1 := Byte(%11000000 or (Value.Value shr 6));
            Result.Byte2 := Byte(%10000000 or (Value.Value and %00000111111));
            Exit;
         end;
      $000800 .. $00D7FF,
      $00E000 .. $00FFFF:
         begin
            Result.Length := 3;
            Result.Byte1 := Byte(%11100000 or (Value.Value shr 12));
            Result.Byte2 := Byte(%10000000 or (Value.Value and %0000111111000000) shr 6);
            Result.Byte3 := Byte(%10000000 or (Value.Value and %0000000000111111));
            Exit;
         end;
      $010000 .. $10FFFF:
         begin
            Result.Length := 4;
            Result.Byte1 := Byte(%11110000 or (Value.Value shr 18));
            Result.Byte2 := Byte(%10000000 or (Value.Value and %0000111111000000000000) shr 12);
            Result.Byte3 := Byte(%10000000 or (Value.Value and %0000000000111111000000) shr 6);
            Result.Byte4 := Byte(%10000000 or (Value.Value and %0000000000000000111111));
            Exit;
         end;
   end;
   Assert((Value.Value < $D800) or (Value.Value > $DFFF), 'Surrogate character passed to CodepointToUTF8()');
   Assert(Value.Value <= $10FFFF, 'Out of range value passed to CodepointToUTF8()');
   Assert(False, 'Internal error in CodepointToUTF8() for value ' + IntToStr(Value.Value));
   Result.AsString := '';
end;

function CodepointToUTF8Length(const Value: TUnicodeCodepoint): TUTF8SequenceLength;
begin
   Assert(Value.Value <> kEOF, 'Cannot convert virtual EOF codepoint to UTF-8');
   case (Value.Value) of
      $000000 .. $00007F: Result := 1;
      $000080 .. $0007FF: Result := 2;
      $000800 .. $00D7FF,
      $00E000 .. $00FFFF: Result := 3;
      $010000 .. $10FFFF: Result := 4;
      else
         Assert((Value.Value < $D800) or (Value.Value > $DFFF), 'Surrogate character passed to CodepointToUTF8Length()');
         Assert(Value.Value <= $10FFFF, 'Out of range value passed to CodepointToUTF8Length()');
         Assert(False, 'Internal error in CodepointToUTF8Length() for value ' + IntToStr(Value.Value));
         Result := 1;
   end;
end;

{$IFDEF DEBUG}
procedure TestUTF8Decoder();
var
   S: UTF8String;
   Incrementor: TUTF8SequenceLength;
   Index, TestIndex: Integer;
begin
   S := #$ed#$a0#$80#$ed#$b0#$80#0#0#0#0; // U+D800 U+DC00, paired UTF-8 surrogates
   Assert(Length(S) = 10, IntToStr(Length(S)));
   Index := 1;
   Assert(UTF8ToCodepoint(PByte(@S[Index]), Incrementor) = $FFFD);
   Assert(Incrementor = 1); Inc(Index, Incrementor);
   Assert(UTF8ToCodepoint(PByte(@S[Index]), Incrementor) = $FFFD);
   Assert(Incrementor = 1); Inc(Index, Incrementor);
   Assert(UTF8ToCodepoint(PByte(@S[Index]), Incrementor) = $FFFD);
   Assert(Incrementor = 1); Inc(Index, Incrementor);
   Assert(UTF8ToCodepoint(PByte(@S[Index]), Incrementor) = $FFFD);
   Assert(Incrementor = 1); Inc(Index, Incrementor);
   Assert(UTF8ToCodepoint(PByte(@S[Index]), Incrementor) = $FFFD);
   Assert(Incrementor = 1); Inc(Index, Incrementor);
   Assert(UTF8ToCodepoint(PByte(@S[Index]), Incrementor) = $FFFD);
   Assert(Incrementor = 1); Inc(Index, Incrementor);
   Assert(Index = 7);
   S := #$ef#$bf#$be#0#0#0#0; // U+FFFE
   Index := 1;
   Assert(UTF8ToCodepoint(PByte(@S[Index]), Incrementor) = $FFFE);
   Assert(Incrementor = 3); Inc(Index, Incrementor);
   Assert(Index = 4);
   S := #$c0#$80#$e0#$80#$80#$f0#$80#$80#$80#$f8#$80#$80#$80#$80#$fc#$80#$80#$80#$80#$80#0#0#0#0; // overlong nulls
   Index := 1;
   for TestIndex := 1 to 20 do // $R-
   begin
      Assert(UTF8ToCodepoint(PByte(@S[Index]), Incrementor) = $FFFD);
   Assert(Incrementor = 1); Inc(Index, Incrementor);
   end;
   Assert(Index = 21);
   S := #$ef#$bf#$be#0#0#0#0; // U+FFFE
   Index := 1;
   Assert(UTF8ToCodepoint(PByte(@S[Index]), Incrementor) = $FFFE);
   Assert(Incrementor = 3); Inc(Index, Incrementor);
   Assert(Index = 4);
   S := #$F0#$A4#$AD#$A2; // U+24B62
   Index := 1;
   Assert(UTF8ToCodepoint(PByte(@S[Index]), Incrementor) = $24B62);
   Assert(Incrementor = 4); Inc(Index, Incrementor);
   Assert(Index = 5);
end;

procedure TestUTF8Encoder();
begin
   Assert(CodepointToUTF8($0024).AsString = #$24);
   Assert(CodepointToUTF8($00A2).AsString = #$C2#$A2);
   Assert(CodepointToUTF8($20AC).AsString = #$E2#$82#$AC);
   Assert(CodepointToUTF8($24B62).AsString = #$F0#$A4#$AD#$A2);
end;

procedure TestUTF8();
var
   S: UTF8String;
   SubS: TUTF8Sequence;
   Index: TUnicodeCodepointRange;
   Position: Cardinal;
   Incrementor: TUTF8SequenceLength;
begin
   SetLength(S, ($80 * 1) + (($7FF-$7F) * 2) + ((($FFFF-$7FF) - ($E000-$D800)) * 3) + (($10FFFF - $FFFF) * 4) + 1);
   S[Length(S)] := #0; // fence for UTF8ToCodepoint()
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
   Assert(Position = Length(S));
   Index := -1;
   Position := 1;
   while (Position < Length(S)) do
   begin
      Inc(Index);
      if (Index = $D800) then
         Index := $E000;
      if (UTF8ToCodepoint(PByte(@S[Position]), Incrementor) <> Index) then
      begin
         Writeln('looking for character with index U+', IntToHex(Index, 4), ' at position ', Position);
         Writeln('  ', IntToHex(Ord(S[Position]), 2), ' ',
                       IntToHex(Ord(S[Position+1]), 2), ' ',
                       IntToHex(Ord(S[Position+2]), 2), ' ',
                       IntToHex(Ord(S[Position+3]), 2), ' ',
                 'Value found was: U+', IntToHex(UTF8ToCodepoint(PByte(@S[Position]), Incrementor).Value, 4), ' using ', Incrementor, ' bytes');
      end;
      Assert(UTF8ToCodepoint(PByte(@S[Position]), Incrementor) = Index);
      Inc(Position, Incrementor);
   end;
   Assert(Index = $10FFFF);
   Assert(Position = Length(S));
end;
{$ENDIF}

initialization
   {$IFDEF TESTS}
      TestUTF8Decoder();
      TestUTF8Encoder();
      TestUTF8();
   {$ENDIF}
end.