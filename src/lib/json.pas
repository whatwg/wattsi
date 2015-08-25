{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
{$CODEPAGE UTF8}
unit json;

interface

//{$DEFINE TESTS}

uses
   hashtable, stringutils;

// This is a read-only JSON interface.

// The JSON specs (there's at least 3) are a bit vague about some
// details, so I've made the following decisions for the purposes of
// this implementation:
//  - the only supported input encoding is UTF-8
//  - the root can be any value (this contradicts RFC4627, but matches
//    RFC7159 and ECMA404, JSON.org is silent on this)
//  - whitespace is allowed before or after any token (this contradicts
//    the json.org description, but matches the RFCs, and more or less
//    matches ECMA404)
//  - duplicate keys are fatally invalid (this contradicts all the
//    specs, especially the RFC)
//  - key order for keys in objects is lost
//  - lone surrogate escapes are invalid (this contradicts all the specs
//    but is required if we're parsing to UTF-8)

type
   TJSONKey = record
    private
     type
      TJSONKeyMode = (kmNumeric, kmString);
     var
      Mode: TJSONKeyMode;
      NumericValue: Cardinal;
      StringValue: UTF8String;
   end;
   operator := (const Value: Cardinal): TJSONKey;
   operator := (const Value: UTF8String): TJSONKey;

type
   TJSON = class;

   TJSONEnumerator = class
    strict protected
      function GetCurrent(): TJSON; virtual;
    public
      function MoveNext(): Boolean; virtual;
      property Current: TJSON read GetCurrent;
   end;

   TJSON = class abstract
    strict protected
      function GetItem(const Key: TJSONKey): TJSON; virtual;
      function GetLength(): Cardinal; virtual;
    public
      property Items[const Key: TJSONKey]: TJSON read GetItem; default;
      property Length: Cardinal read GetLength;
      function GetEnumerator(): TJSONEnumerator; virtual;
   end;

type
   TJSONObject = class(TJSON)
    protected
     type
      TJSONHashTable = specialize THashTable <UTF8String, TJSON, UTF8StringUtils>;
      TEnumerator = class(TJSONEnumerator)
       strict protected
         FEnumerator: TJSONHashTable.TValueEnumerator;
         function GetCurrent(): TJSON; override;
       private
         constructor Create(const Home: TJSONObject);
       public
         destructor Destroy(); override;
         function MoveNext(): Boolean; override;
      end;
     var
      FItems: TJSONHashTable;
      function GetItem(const Key: TJSONKey): TJSON; override;
      function GetLength(): Cardinal; override;
    public
     type
      TKeyEnumerator = TJSONHashTable.TKeyEnumerator;
      destructor Destroy(); override;
      function GetEnumerator(): TJSONEnumerator; override;
      function Keys(): TKeyEnumerator;
   end;
   operator := (const Value: TJSON): TJSONObject;

type
   TJSONArray = class(TJSON)
    protected
     type
      TEnumerator = class(TJSONEnumerator)
       strict protected
         FHome: TJSONArray;
         FCurrent: TJSON;
         FPosition: Cardinal;
       private
         constructor Create(Home: TJSONArray);
       public
         function GetCurrent(): TJSON; override;
         function MoveNext(): Boolean; override;
      end;
     var
      FItems: array of TJSON;
      function GetItem(const Key: TJSONKey): TJSON; override;
      function GetLength(): Cardinal; override;
    public
      destructor Destroy(); override;
      function GetEnumerator(): TJSONEnumerator; override;
   end;
   operator := (const Value: TJSON): TJSONArray;

type
   TJSONNumber = class(TJSON)
    protected
      FValue: Double;
   end;
   operator := (const Value: TJSON): Double;
   operator = (const Op1: TJSON; const Op2: Double): Boolean;

type
   TJSONString = class(TJSON)
    protected
      FValue: UTF8String;
   end;
   operator := (const Value: TJSON): UTF8String;
   operator = (const Op1: TJSON; const Op2: UTF8String): Boolean;

type
   TJSONBoolean = class(TJSON)
    protected
      FValue: Boolean;
   end;
   operator := (const Value: TJSON): Boolean;
   operator = (const Op1: TJSON; const Op2: Boolean): Boolean;

// XXX should probably have the reverse = operators too

function ParseJSON(const Input: UTF8String): TJSON;

implementation

uses
   {$IFDEF TESTS} utf8, resutils, {$ENDIF}
   unicode, hashfunctions, exceptions, sysutils;

operator := (const Value: Cardinal): TJSONKey;
begin
   Result.Mode := kmNumeric;
   Result.NumericValue := Value;
end;

operator := (const Value: UTF8String): TJSONKey;
begin
   Result.Mode := kmString;
   Result.StringValue := Value;
end;

function TJSONEnumerator.GetCurrent(): TJSON;
begin
   Result := nil;
end;

function TJSONEnumerator.MoveNext(): Boolean;
begin
   Result := False;
end;

function TJSON.GetItem(const Key: TJSONKey): TJSON;
begin
   if (Key.Mode = kmNumeric) then
      raise EConvertError.Create('Not an array')
   else
      raise EConvertError.Create('Not an object');
   Result := nil;
end;

function TJSON.GetLength(): Cardinal;
begin
   raise EConvertError.Create('Not an array or object');
   Result := 0;
end;

function TJSON.GetEnumerator(): TJSONEnumerator;
begin
   Result := TJSONEnumerator.Create();
end;

constructor TJSONObject.TEnumerator.Create(const Home: TJSONObject);
begin
   inherited Create();
   FEnumerator := Home.FItems.Values;
end;

destructor TJSONObject.TEnumerator.Destroy();
begin
   FEnumerator.Free();
   inherited;
end;

function TJSONObject.TEnumerator.GetCurrent(): TJSON;
begin
   Result := FEnumerator.Current;
end;

function TJSONObject.TEnumerator.MoveNext(): Boolean;
begin
   Result := FEnumerator.MoveNext()
end;

function TJSONObject.GetItem(const Key: TJSONKey): TJSON;
begin
   if (Key.Mode = kmString) then
      Result := FItems[Key.StringValue]
   else
      Result := inherited;
end;

function TJSONObject.GetLength(): Cardinal;
begin
   Result := FItems.Count;
end;

destructor TJSONObject.Destroy();
var
   Child: TJSON;
begin
   for Child in FItems.Values do
      Child.Free();
   FItems.Free();
   inherited;
end;

function TJSONObject.GetEnumerator(): TJSONEnumerator;
begin
   Result := TEnumerator.Create(Self);
end;

function TJSONObject.Keys(): TJSONHashTable.TKeyEnumerator;
begin
   Result := FItems.GetEnumerator();
end;

operator := (const Value: TJSON): TJSONObject;
begin
   Result := Value as TJSONObject;
end;

constructor TJSONArray.TEnumerator.Create(Home: TJSONArray);
begin
   inherited Create();
   FHome := Home;
end;

function TJSONArray.TEnumerator.GetCurrent(): TJSON;
begin
   Result := FCurrent;
end;

function TJSONArray.TEnumerator.MoveNext(): Boolean;
begin
   if (FPosition < FHome.Length) then
   begin
      FCurrent := FHome[FPosition];
      Inc(FPosition);
      Result := True;
   end
   else
      Result := False;
end;

function TJSONArray.GetItem(const Key: TJSONKey): TJSON;
begin
   Assert(Assigned(FItems));
   if (Key.Mode = kmNumeric) then
      Result := FItems[Key.NumericValue]
   else
      Result := inherited;
end;

function TJSONArray.GetLength(): Cardinal;
begin
   Result := System.Length(FItems); // $R-
end;

destructor TJSONArray.Destroy();
var
   Child: TJSON;
begin
   for Child in FItems do {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
      Child.Free();
   inherited;
end;

function TJSONArray.GetEnumerator(): TJSONEnumerator;
begin
   Result := TEnumerator.Create(Self);
end;

operator := (const Value: TJSON): TJSONArray;
begin
   Result := Value as TJSONArray;
end;

operator := (const Value: TJSON): Double;
begin
   Result := (Value as TJSONNumber).FValue;
end;

operator = (const Op1: TJSON; const Op2: Double): Boolean;
begin
   Result := AssigneD(Op1) and (Op1 is TJSONNumber) and ((Op1 as TJSONNumber).FValue = Op2);
end;

operator := (const Value: TJSON): UTF8String;
begin
   Result := (Value as TJSONString).FValue;
end;

operator = (const Op1: TJSON; const Op2: UTF8String): Boolean;
begin
   Result := Assigned(Op1) and (Op1 is TJSONString) and ((Op1 as TJSONString).FValue = Op2);
end;

operator := (const Value: TJSON): Boolean;
begin
   Result := (Value as TJSONBoolean).FValue;
end;

operator = (const Op1: TJSON; const Op2: Boolean): Boolean;
begin
   Result := Assigned(Op1) and (Op1 is TJSONBoolean) and ((Op1 as TJSONBoolean).FValue = Op2);
end;

function ParseJSON(const Input: UTF8String): TJSON;
var
   Enumerator: UTF8StringEnumerator;
   CurrentCharacter: TUnicodeCodepoint;
   Line, Column: Cardinal;

   function GetNextCharacter(): TUnicodeCodepoint; inline;
   begin
       if (Enumerator.MoveNext()) then
         CurrentCharacter := Enumerator.Current
      else
         CurrentCharacter := kEOF;
      if (CurrentCharacter = $000A) then
      begin
         Inc(Line);
         Column := 1;
      end
      else
         Inc(Column);
      Result := CurrentCharacter;
      //if (Result <> kEOF) then
      //   Writeln(CodepointToUTF8(Result).AsString);
   end;

   procedure Error(const Message: UTF8String);
   begin
      raise ESyntaxError.CreateFmt('Invalid JSON: %s at line %d column %d', [Message, Line, Column]);
   end;

   procedure SkipWhitespace();
   begin
      repeat
         GetNextCharacter();
      until (CurrentCharacter <> $0020) and
            (CurrentCharacter <> $0009) and
            (CurrentCharacter <> $000A) and
            (CurrentCharacter <> $000D);
   end;

   procedure SkipWhitespaceFromCurrent();
   begin
      while ((CurrentCharacter = $0020) or
             (CurrentCharacter = $0009) or
             (CurrentCharacter = $000A) or
             (CurrentCharacter = $000D)) do
         GetNextCharacter();
   end;

   function ParseValue(): TJSON; forward;

   function ParseNumber(): TJSONNumber;
   var
      IsNegative, IsNegativeExponent: Boolean;
      IntegerComponent, FractionalComponentValue, FractionalComponentLength, Exponent: Int64;
   begin
      {$PUSH}
      {$OVERFLOWCHECKS ON}
      {$RANGECHECKS ON}
      if (CurrentCharacter = Ord('-')) then
      begin
         IsNegative := True;
         GetNextCharacter();
      end
      else
         IsNegative := False;
      IntegerComponent := 0;
      if (CurrentCharacter <> Ord('0')) then
      begin
         repeat
            case (CurrentCharacter.Value) of
               Ord('0'): IntegerComponent := IntegerComponent * 10;
               Ord('1'): IntegerComponent := IntegerComponent * 10 + 1;
               Ord('2'): IntegerComponent := IntegerComponent * 10 + 2;
               Ord('3'): IntegerComponent := IntegerComponent * 10 + 3;
               Ord('4'): IntegerComponent := IntegerComponent * 10 + 4;
               Ord('5'): IntegerComponent := IntegerComponent * 10 + 5;
               Ord('6'): IntegerComponent := IntegerComponent * 10 + 6;
               Ord('7'): IntegerComponent := IntegerComponent * 10 + 7;
               Ord('8'): IntegerComponent := IntegerComponent * 10 + 8;
               Ord('9'): IntegerComponent := IntegerComponent * 10 + 9;
            else
               Break;
            end;
            GetNextCharacter();
         until false;
      end
      else
         GetNextCharacter();
      if (CurrentCharacter.Value = Ord('.')) then
      begin
         FractionalComponentLength := 1;
         FractionalComponentValue := 0;
         repeat
            GetNextCharacter();
            FractionalComponentLength := FractionalComponentLength * 10;
            case (CurrentCharacter.Value) of
               Ord('0'): FractionalComponentValue := FractionalComponentValue * 10;
               Ord('1'): FractionalComponentValue := FractionalComponentValue * 10 + 1;
               Ord('2'): FractionalComponentValue := FractionalComponentValue * 10 + 2;
               Ord('3'): FractionalComponentValue := FractionalComponentValue * 10 + 3;
               Ord('4'): FractionalComponentValue := FractionalComponentValue * 10 + 4;
               Ord('5'): FractionalComponentValue := FractionalComponentValue * 10 + 5;
               Ord('6'): FractionalComponentValue := FractionalComponentValue * 10 + 6;
               Ord('7'): FractionalComponentValue := FractionalComponentValue * 10 + 7;
               Ord('8'): FractionalComponentValue := FractionalComponentValue * 10 + 8;
               Ord('9'): FractionalComponentValue := FractionalComponentValue * 10 + 9;
            else
               Break;
            end;
         until false;
      end
      else
      begin
         FractionalComponentLength := 1;
         FractionalComponentValue := 0;
      end;
      if ((CurrentCharacter = Ord('e')) or (CurrentCharacter = Ord('E'))) then
      begin
         GetNextCharacter();
         IsNegativeExponent := False;
         if (CurrentCharacter = Ord('+')) then
         begin
            GetNextCharacter();
         end
         else 
         if (CurrentCharacter = Ord('-')) then
         begin
            IsNegativeExponent := True;
            GetNextCharacter();
         end;
         Exponent := 0;
         repeat
            case (CurrentCharacter.Value) of
               Ord('0'): Exponent := Exponent * 10;
               Ord('1'): Exponent := Exponent * 10 + 1;
               Ord('2'): Exponent := Exponent * 10 + 2;
               Ord('3'): Exponent := Exponent * 10 + 3;
               Ord('4'): Exponent := Exponent * 10 + 4;
               Ord('5'): Exponent := Exponent * 10 + 5;
               Ord('6'): Exponent := Exponent * 10 + 6;
               Ord('7'): Exponent := Exponent * 10 + 7;
               Ord('8'): Exponent := Exponent * 10 + 8;
               Ord('9'): Exponent := Exponent * 10 + 9;
            else
               Break;
            end;
            GetNextCharacter();
         until false;
         if (IsNegativeExponent) then
            Exponent := -Exponent;
      end
      else
      begin
         Exponent := 0;
      end;
      Result := TJSONNumber.Create();
      Result.FValue := (IntegerComponent + FractionalComponentValue / FractionalComponentLength) * Exp(Exponent*Ln(10)); // $R-
      if (IsNegative) then
         Result.FValue := -Result.FValue;
      {$POP}
   end;

   function ParseFourHexadecimalDigitsToUnicodeCodepoint(): TUnicodeCodepointRange;

      function GetHexDigit: Byte;
      begin
         case GetNextCharacter().Value of
            Ord('0'): Result := 0;
            Ord('1'): Result := 1;
            Ord('2'): Result := 2;
            Ord('3'): Result := 3;
            Ord('4'): Result := 4;
            Ord('5'): Result := 5;
            Ord('6'): Result := 6;
            Ord('7'): Result := 7;
            Ord('8'): Result := 8;
            Ord('9'): Result := 9;
            Ord('A'), Ord('a'): Result := 10;
            Ord('B'), Ord('b'): Result := 11;
            Ord('C'), Ord('c'): Result := 12;
            Ord('D'), Ord('d'): Result := 13;
            Ord('E'), Ord('e'): Result := 14;
            Ord('F'), Ord('f'): Result := 15;
         else
            Error('invalid hex digit');
            Result := $FF;
         end;
      end;

   begin
      Result := GetHexDigit() shl 12 + // $R-
                GetHexDigit() shl 8 +
                GetHexDigit() shl 4 +
                GetHexDigit();
   end;

   function ParseString(): UTF8String;
   var
      StartPointer, DestinationPointer, BookmarkPointer: TUTF8StringPointer;
      EscapedCharacter1, EscapedCharacter2: TUnicodeCodepointRange;
      HadEscapes: Boolean;
   begin
      StartPointer := Enumerator.GetPointer();
      StartPointer.AdvanceToAfter();
      DestinationPointer := StartPointer;
      // DestinationPointer.SetToZeroWidth(); // if we ever make TUTF8StringPointer support actually advancing through the string, then we'll need to zero-out the end pointer here
      Assert(Input.Extract(StartPointer, DestinationPointer).IsEmpty);
      HadEscapes := False;
      while (GetNextCharacter() <> Ord('"')) do
      begin
         case (CurrentCharacter.Value) of
            Ord('\'):
               begin
                  HadEscapes := True;
                  case (GetNextCharacter().Value) of
                     Ord('"'): Input.InplaceReplace(Ord('"'), DestinationPointer);
                     Ord('\'): Input.InplaceReplace(Ord('\'), DestinationPointer);
                     Ord('/'): Input.InplaceReplace(Ord('/'), DestinationPointer);
                     Ord('b'): Input.InplaceReplace($0008, DestinationPointer);
                     Ord('f'): Input.InplaceReplace($000C, DestinationPointer);
                     Ord('n'): Input.InplaceReplace($000A, DestinationPointer);
                     Ord('r'): Input.InplaceReplace($000D, DestinationPointer);
                     Ord('t'): Input.InplaceReplace($0009, DestinationPointer);
                     Ord('u'):
                        begin
                           EscapedCharacter1 := ParseFourHexadecimalDigitsToUnicodeCodepoint();
                           BookmarkPointer := Enumerator.GetPointer();
                           if ((EscapedCharacter1 >= $D800) and (EscapedCharacter1 <= $DBFF) and 
                               (GetNextCharacter() = Ord('\')) and
                               (GetNextCharacter() = Ord('u'))) then
                           begin
                              EscapedCharacter2 := ParseFourHexadecimalDigitsToUnicodeCodepoint();
                              if ((EscapedCharacter2 >= $DC00) and (EscapedCharacter2 <= $DFFF)) then
                              begin
                                 Input.InplaceReplace($10000 + (EscapedCharacter1 - $D800) * $400 + (EscapedCharacter2 - $DC00), DestinationPointer); // $R-
                              end
                              else
                              begin
                                 Input.InplaceReplace(EscapedCharacter1, DestinationPointer);
                                 Input.InplaceReplace(EscapedCharacter2, DestinationPointer);
                              end;
                           end
                           else
                           begin
                              Input.InplaceReplace(EscapedCharacter1, DestinationPointer);
                              Enumerator.ReturnToPointer(BookmarkPointer);
                           end;
                        end;
                  else
                     Error('invalid string escape');
                  end;
               end;
            $0000..$001F: Error('control character in string');
            kEOF: Error('unexpected end of file in string');
         else
            if (HadEscapes) then
               Input.InplaceReplace(CurrentCharacter, DestinationPointer)
            else
               DestinationPointer.AdvanceToAfter(CurrentCharacter);
         end;
      end;
      Result := Input.Extract(StartPointer, DestinationPointer).AsString;
   end;

   function ParseStringAsValue(): TJSONString;
   var
      Value: UTF8String;
   begin
      Value := ParseString();
      Result := TJSONString.Create();
      Result.FValue := Value;
   end;

   function ParseTrue(): TJSONBoolean;
   begin
      if ((GetNextCharacter() <> Ord('r')) or
          (GetNextCharacter() <> Ord('u')) or
          (GetNextCharacter() <> Ord('e'))) then
         Error('unrecognised keyword');
      Result := TJSONBoolean.Create();
      Result.FValue := True;
   end;

   function ParseFalse(): TJSONBoolean;
   begin
      if ((GetNextCharacter() <> Ord('a')) or
          (GetNextCharacter() <> Ord('l')) or
          (GetNextCharacter() <> Ord('s')) or
          (GetNextCharacter() <> Ord('e'))) then
         Error('unrecognised keyword');
      Result := TJSONBoolean.Create();
   end;

   function ParseNull(): TJSON;
   begin
      if ((GetNextCharacter() <> Ord('u')) or
          (GetNextCharacter() <> Ord('l')) or
          (GetNextCharacter() <> Ord('l'))) then
         Error('unrecognised keyword');
      Result := nil;
   end;

   function ParseObject(): TJSONObject;
   var
      Key: UTF8String;
   begin
      Result := TJSONObject.Create();
      Result.FItems := TJSONObject.TJSONHashTable.Create(@UTF8StringHash32);
      try
         SkipWhitespace();
         if (CurrentCharacter <> Ord('}')) then
            repeat
               if (CurrentCharacter <> Ord('"')) then
                  Error('invalid key in object');
               Key := ParseString();
               if (Result.FItems.Has(Key)) then
                  Error('duplicate key in object');
               SkipWhitespace();
               if (CurrentCharacter <> Ord(':')) then
                  Error('missing colon after object key');
               SkipWhitespace();
               Result.FItems[Key] := ParseValue();
               if (CurrentCharacter <> Ord(',')) then
                  Break;
               SkipWhitespace();
            until False;
         if (CurrentCharacter <> Ord('}')) then
            Error('missing comma or closing brace after object value');
      except
         Result.Free();
         raise;
      end;
   end;

   function ParseArray(): TJSONArray;
   begin
      Result := TJSONArray.Create();
      try
         SkipWhitespace();
         if (CurrentCharacter <> Ord(']')) then
            repeat
               SetLength(Result.FItems, Length(Result.FItems)+1);
               Result.FItems[High(Result.FItems)] := ParseValue();
               if (CurrentCharacter <> Ord(',')) then
                  Break;
               SkipWhitespace();
            until False;
         if (CurrentCharacter <> Ord(']')) then
            Error('missing comma or closing bracket after array value');
      except
         Result.Free();
         raise;
      end;
   end;

   function ParseValue(): TJSON;
   begin
      case (CurrentCharacter.Value) of
         Ord('{'): begin Result := ParseObject(); SkipWhitespace(); end;
         Ord('['): begin Result := ParseArray(); SkipWhitespace(); end;
         Ord('-'), Ord('0')..Ord('9'): begin Result := ParseNumber(); SkipWhitespaceFromCurrent(); end;
         Ord('"'): begin Result := ParseStringAsValue(); SkipWhitespace(); end;
         Ord('t'): begin Result := ParseTrue(); SkipWhitespace(); end;
         Ord('f'): begin Result := ParseFalse(); SkipWhitespace(); end;
         Ord('n'): begin Result := ParseNull(); SkipWhitespace(); end;
      else
         Error('invalid value');
         Result := nil;
      end;
   end;

begin
   Enumerator := Input.GetEnumerator();
   Line := 1;
   Column := 0;
   try
      SkipWhitespace();
      Result := ParseValue();
      if (CurrentCharacter <> kEOF) then
         SkipWhitespace();
      if (CurrentCharacter <> kEOF) then
      begin
         Result.Free();
         Error('trailing garbage');
      end;
   finally
      Enumerator.Free();
   end;
end;

{$IFDEF TESTS}
{$IFOPT C+} {$ELSE} {$FATAL Can't run tests without assertion support} {$ENDIF}
{$RESOURCE tests/json.rc}
procedure TestJSON();
var
   ParsedData: TJSON;

   function ReadTestData(const TestName: AnsiString): AnsiString;

    procedure ConvertToString(const Data: Pointer; const Size: Cardinal);
    begin
       SetLength(Result, Size);
       Move(Data^, Result[1], Size);
    end;

   begin
      {$IFOPT C+} Result := #0#0#0; {$ENDIF}
      ReadFromResources('testdata', TestName, @ConvertToString);
      {$IFOPT C+} Assert(Result <> #0#0#0); {$ENDIF}
   end;

begin
   ParsedData := ParseJSON(ReadTestData('json1'));
   Assert(Assigned(ParsedData));
   Assert(ParsedData is TJSONArray);
   Assert(Assigned(ParsedData[0]));
   Assert(ParsedData[0] is TJSONObject);
   Assert(Assigned(ParsedData[0]['object']));
   Assert(ParsedData[0]['object'] is TJSONObject);
   Assert(ParsedData[0]['object'].Length = 0);
   Assert(ParsedData[0]['array'] is TJSONArray);
   Assert(ParsedData[0]['array'].Length = 0);
   Assert(ParsedData[0]['number'] is TJSONNumber);
   Assert(ParsedData[0]['number'] = 0);
   Assert(ParsedData[0]['string'] is TJSONString);
   Assert(ParsedData[0]['string'] = '');
   Assert(ParsedData[0]['true'] is TJSONBoolean);
   Assert(ParsedData[0]['true'] = True);
   Assert(ParsedData[0]['false'] is TJSONBoolean);
   Assert(ParsedData[0]['false'] = False);
   Assert(ParsedData[0]['null'] = nil);
   Assert(ParsedData[0].Length = 7);
   Assert(ParsedData[1]['object']['foo'] = 'bar');
   Assert(ParsedData[1]['object'].Length = 1);
   Assert(ParsedData[1]['array'][0] = 'foo');
   Assert(ParsedData[1]['array'][1] = 'bar');
   Assert(ParsedData[1]['array'].Length = 2);
   Assert(ParsedData[1]['number'] = 900);
   Assert(ParsedData[1]['string'] = 'foo');
   Assert(ParsedData[2]['string'] = 'a"\/'#$8#$C#$A#$D#$9#$E2#$98#$BA#$F0#$9D#$84#$9E'b');
   Assert(ParsedData[2]['number'] = 900);
   Assert(ParsedData[2].Length = 2);
   Assert(ParsedData.Length = 3);
   FreeAndNil(ParsedData);
   ParsedData := ParseJSON(ReadTestData('json2'));
   Assert(Assigned(ParsedData));
   Assert(ParsedData is TJSONArray);
   Assert(ParsedData.Length = 0);
   FreeAndNil(ParsedData);
   ParsedData := ParseJSON(ReadTestData('json3'));
   Assert(Assigned(ParsedData));
   Assert(ParsedData is TJSONArray);
   Assert(ParsedData[0] = nil);
   Assert(ParsedData[1] = nil);
   Assert(ParsedData[2] = nil);
   Assert(ParsedData[3] = False);
   Assert(ParsedData[4] = 0);
   Assert(ParsedData[5] = '');
   Assert(ParsedData[6] = nil);
   Assert(ParsedData.Length = 7);
   FreeAndNil(ParsedData);
   ParsedData := ParseJSON(ReadTestData('json4'));
   Assert(Assigned(ParsedData));
   Assert(ParsedData is TJSONObject);
   Assert(ParsedData[''] = 'this should parse, despite starting with a space character, tab, and newline');
   Assert(ParsedData.Length = 1);
   FreeAndNil(ParsedData);
   ParsedData := ParseJSON(ReadTestData('json5'));
   Assert(Assigned(ParsedData));
   Assert(ParsedData is TJSONObject);
   Assert(ParsedData[''] = #0 + CodepointToUTF8($FFFF));
   Assert(ParsedData['0'] = False);
   Assert(ParsedData['1'] is TJSONArray);
   Assert(ParsedData['1'][0] is TJSONArray);
   Assert(ParsedData['1'][0].Length = 0);
   Assert(ParsedData['1'][1] is TJSONArray);
   Assert(ParsedData['1'][1].Length = 0);
   Assert(ParsedData['1'][2] is TJSONArray);
   Assert(ParsedData['1'][2].Length = 0);
   Assert(ParsedData['1'][3] is TJSONArray);
   Assert(ParsedData['1'][3].Length = 0);
   Assert(ParsedData['1'][4] is TJSONArray);
   Assert(ParsedData['1'][4].Length = 0);
   Assert(ParsedData['1'][5] is TJSONObject);
   Assert(ParsedData['1'][5].Length = 0);
   Assert(ParsedData['1'][6] is TJSONObject);
   Assert(ParsedData['1'][6].Length = 0);
   Assert(ParsedData['1'].Length = 7);
   Assert(ParsedData.Length = 3);
   FreeAndNil(ParsedData);
   ParsedData := ParseJSON(ReadTestData('json6'));
   Assert(not Assigned(ParsedData));
   FreeAndNil(ParsedData); // redundant, hopefully...
   ParsedData := ParseJSON(ReadTestData('json7'));
   Assert(Assigned(ParsedData));
   Assert(ParsedData = False);
   FreeAndNil(ParsedData);
   ParsedData := ParseJSON(ReadTestData('json8'));
   Assert(Assigned(ParsedData));
   Assert(ParsedData = True);
   FreeAndNil(ParsedData);
   ParsedData := ParseJSON(ReadTestData('json9'));
   Assert(Assigned(ParsedData));
   Assert(ParsedData = #9);
   FreeAndNil(ParsedData);
   ParsedData := ParseJSON(ReadTestData('json10'));
   Assert(Assigned(ParsedData));
   Assert(ParsedData = 0);
   FreeAndNil(ParsedData);
   ParsedData := ParseJSON(ReadTestData('json11'));
   Assert(Assigned(ParsedData));
   Assert(ParsedData = #$E2#$98#$BA, (ParsedData as TJSONString).FValue);
   FreeAndNil(ParsedData);
end;
{$ENDIF}

initialization
   {$IFDEF TESTS} TestJSON(); {$ENDIF}
end.
