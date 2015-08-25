{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
program test;

{$IFDEF DEBUG} {$DEFINE PARSEERROR} {$ENDIF}

uses
   {$IFDEF DEBUG} debug, {$ENDIF} json, dom, webdom, htmlparser, utf8, exceptions, sysutils, fileutils, stringutils, math, {$IFDEF PARSEERROR} plasticarrays, {$ENDIF} canonicalstrings, ropes;

const
   forever = False;

{$IFDEF DEBUG}
procedure Verify(const Condition: Boolean; const Message: UTF8String);
begin // like Assert, but for tests
   if (not Condition) then
      raise Exception.Create(Message);
end;

type
   TTestHTMLParser = class(THTMLParser)
    protected
     var
      ExpectedTokens: TJSONArray;
      {$IFDEF PARSEERROR} FRecentParseErrors: specialize PlasticArray<UTF8String, UTF8StringUtils>; {$ENDIF}
      FCurrentExpectedTokenIndex: Cardinal;
      {$IFDEF PARSEERROR} FExpectedParseErrorCount: Cardinal; {$ENDIF}
      procedure RunTokeniserTest(const InitialState: TTokeniserState; const LastStartTag: UTF8String; const Input: UTF8String; const Output: TJSONArray {$IFDEF PARSEERROR}; const ExpectedParseErrorCount: Cardinal {$ENDIF});
      {$IFDEF PARSEERROR} procedure CountParseErrors(const Message: UTF8String); {$ENDIF}
    public
      class procedure ProcessTokeniserTest(const Test: TJSONObject);
      class procedure ProcessParserTest(const TestInput, TestOutput: UTF8String; {$IFDEF PARSEERROR} const TestErrors: Cardinal; {$ENDIF} const SetScriptingEnabled: Boolean);
   end;

{$IFDEF PARSEERROR}
procedure TTestHTMLParser.CountParseErrors(const Message: UTF8String);
begin
   FRecentParseErrors.Push(Message);
   Verify(FRecentParseErrors.Length <= FExpectedParseErrorCount, 'unexpected parser error with message "' + Message + '" (this was error ' + IntToStr(FRecentParseErrors.Length) + '; expected at most ' + IntToStr(FExpectedParseErrorCount) + ' errors)');
end;
{$ENDIF}

procedure TTestHTMLParser.RunTokeniserTest(const InitialState: TTokeniserState; const LastStartTag: UTF8String; const Input: UTF8String; const Output: TJSONArray {$IFDEF PARSEERROR}; const ExpectedParseErrorCount: Cardinal {$ENDIF});

var
   CharacterTokensOffset: Cardinal;

   procedure SkipTokeniserParseErrors();
   begin
      while ((FCurrentExpectedTokenIndex < ExpectedTokens.Length) and (ExpectedTokens[FCurrentExpectedTokenIndex] = 'ParseError')) do
         Inc(FCurrentExpectedTokenIndex);
   end;

   procedure VerifyExpectedTokenType(const GotName: UTF8String; const Args: Cardinal);
   begin
      if (GotName <> 'Character') then
         Verify(CharacterTokensOffset = 1, 'did not see sufficient character tokens');
      SkipTokeniserParseErrors();
      Verify(FCurrentExpectedTokenIndex < ExpectedTokens.Length, 'unexpected extraneous token');
      Verify(ExpectedTokens[FCurrentExpectedTokenIndex] is TJSONArray, 'test format error');
      Verify(ExpectedTokens[FCurrentExpectedTokenIndex].Length >= 1, 'test format error');
      Verify(ExpectedTokens[FCurrentExpectedTokenIndex][0] = GotName, 'wrong token found - found ' + GotName + ' but wanted ' + UTF8String(ExpectedTokens[FCurrentExpectedTokenIndex][0]));
      Verify(ExpectedTokens[FCurrentExpectedTokenIndex].Length >= 1 + Args, 'test format error');
   end;

   procedure VerifyStrings(StringToCheck: UTF8String);
   var
      MaxLength: Cardinal;
      ExpectedString: UTF8String;
   begin
      Assert(StringToCheck <> '', 'got empty source characters');
      repeat
         VerifyExpectedTokenType('Character', 1);
         ExpectedString := ExpectedTokens[FCurrentExpectedTokenIndex][1];
         Assert(CharacterTokensOffset <= Length(ExpectedString));
         MaxLength := Min(Length(ExpectedString) - CharacterTokensOffset + 1, Length(StringToCheck)); // $R-
         Assert(MaxLength > 0);
         Verify(Copy(ExpectedString, CharacterTokensOffset, MaxLength) = Copy(StringToCheck, 1, MaxLength),
                'expected character tokens "' + Copy(ExpectedString, CharacterTokensOffset, MaxLength) + '" but found tokens "' + Copy(StringToCheck, 1, MaxLength) + '"');
         if (Length(StringToCheck) <= MaxLength) then
            StringToCheck := ''
         else
            StringToCheck := Copy(StringToCheck, MaxLength+1, Length(StringToCheck) - MaxLength); // http://bugs.freepascal.org/view.php?id=26403
         Inc(CharacterTokensOffset, MaxLength);
         if (CharacterTokensOffset > Length(ExpectedString)) then
         begin
            CharacterTokensOffset := 1;
            Inc(FCurrentExpectedTokenIndex);
         end;
      until StringToCheck = '';
   end;

var
   TagName, AttributeName, AttributeValue, CommentValue: UTF8String;
   ExpectSelfClosing: Boolean;
   {$IFDEF PARSEERROR} Error: UTF8String; {$ENDIF}
   Attributes: TElement.TAttributeHashTable;
begin
   ExpectedTokens := Output;
   if (Length(Input) > 0) then
      SpoonFeed(@Input[1], Length(Input)); // $R-
   FInputStream.Freeze();
   {$IFDEF PARSEERROR}
      FExpectedParseErrorCount := ExpectedParseErrorCount;
      OnParseError := @CountParseErrors;
   {$ENDIF}
   FTokeniserState := InitialState;
   FLastStartTag := LastStartTag;
   CharacterTokensOffset := 1;
   try
      repeat
         Assert(not FCurrentToken.Ready);
         Assert(FCurrentToken.Kind = tkNone);
         Tokenise(); // updates FCurrentToken
         Assert(FCurrentToken.Kind <> tkNone);
         if (FCurrentToken.HavePendingCharacters) then
            VerifyStrings(FCurrentToken.ExtractSourceCharacters(FInputStream.Data).GetAsStringSlow())
         else
            Assert(FCurrentToken.Kind <> tkSourceCharacters);
         Assert(FCurrentToken.Kind <> tkNone);
         case (FCurrentToken.Kind) of
            tkDOCTYPE:
               begin
                  VerifyExpectedTokenType('DOCTYPE', 4);
                  Verify(Assigned(ExpectedTokens[FCurrentExpectedTokenIndex][1]) = FCurrentToken.DOCTYPENamePresent, 'unexpected DOCTYPE name presence');
                  if (FCurrentToken.DOCTYPENamePresent) then
                     Verify(ExpectedTokens[FCurrentExpectedTokenIndex][1] = FCurrentToken.DOCTYPEName.AsString, 'unexpected DOCTYPE name value');
                  Verify(Assigned(ExpectedTokens[FCurrentExpectedTokenIndex][2]) = FCurrentToken.PublicIDPresent, 'unexpected DOCTYPE public ID presence');
                  if (FCurrentToken.PublicIDPresent) then
                     Verify(ExpectedTokens[FCurrentExpectedTokenIndex][2] = FCurrentToken.PublicID.AsString, 'unexpected DOCTYPE public ID value');
                  Verify(Assigned(ExpectedTokens[FCurrentExpectedTokenIndex][3]) = FCurrentToken.SystemIDPresent, 'unexpected DOCTYPE system ID presence');
                  if (FCurrentToken.SystemIDPresent) then
                     Verify(ExpectedTokens[FCurrentExpectedTokenIndex][3] = FCurrentToken.SystemID.AsString, 'unexpected DOCTYPE system ID value');
                  Verify(ExpectedTokens[FCurrentExpectedTokenIndex][4] = not FCurrentToken.ForceQuirksFlag, 'unexpected Force Quirks flag value');
                  Inc(FCurrentExpectedTokenIndex);
               end;
            tkStartTag:
               begin
                  VerifyExpectedTokenType('StartTag', 2);
                  TagName := ExpectedTokens[FCurrentExpectedTokenIndex][1];
                  Verify(FCurrentToken.TagName.AsString = TagName, 'unexpected tag name; wanted "' + TagName +'" but got "' + FCurrentToken.TagName.AsString + '"');
                  Attributes := FCurrentToken.TakeAttributes();
                  if (not Assigned(Attributes)) then
                     Verify((ExpectedTokens[FCurrentExpectedTokenIndex][2] as TJSONObject).Length = 0, 'unexpected number of attributes')
                  else
                  begin
                     Verify(Attributes.Count = (ExpectedTokens[FCurrentExpectedTokenIndex][2] as TJSONObject).Length, 'unexpected number of attributes');
                     for AttributeName in Attributes do
                     begin
                        AttributeValue := ExpectedTokens[FCurrentExpectedTokenIndex][2][AttributeName];
                        Verify(Attributes[AttributeName].AsString = AttributeValue, 'unexpected attribute value for attribute "' + AttributeName + '": wanted "' + AttributeValue + '" but got "' + Attributes[AttributeName].AsString + '"');
                     end;
                     Attributes.Free();
                  end;
                  ExpectSelfClosing := (ExpectedTokens[FCurrentExpectedTokenIndex].Length >= 4) and (ExpectedTokens[FCurrentExpectedTokenIndex][3] = True);
                  Verify(FCurrentToken.SelfClosingFlag = ExpectSelfClosing, 'self-closing flag did not match expectations');
                  Inc(FCurrentExpectedTokenIndex);
               end;
            tkEndTag:
               begin
                  VerifyExpectedTokenType('EndTag', 1);
                  TagName := ExpectedTokens[FCurrentExpectedTokenIndex][1];
                  Verify(FCurrentToken.TagName.AsString = TagName, 'unexpected tag name; wanted "' + TagName +'" but got "' + FCurrentToken.TagName.AsString + '"');
                  Inc(FCurrentExpectedTokenIndex);
               end;
            tkSourceCharacters: ; // handled earlier
            tkExtraCharacters, tkExtraSpaceCharacter:
               begin
                  VerifyStrings(CodepointArrayToUTF8String(FCurrentToken.ExtraChars));
               end;
            tkNullCharacter:
               begin
                  VerifyStrings(#$00);
               end;
            tkComment:
               begin
                  VerifyExpectedTokenType('Comment', 1);
                  CommentValue := ExpectedTokens[FCurrentExpectedTokenIndex][1];
                  Verify(CommentValue = FCurrentToken.CommentValue.AsString, 'comment had wrong data: expected "' + CommentValue + '" but got "' + FCurrentToken.CommentValue.AsString + '"');
                  Inc(FCurrentExpectedTokenIndex);
               end;
            tkEOF:
               begin
                  {$IFDEF PARSEERROR} Verify(FRecentParseErrors.Length = FExpectedParseErrorCount, 'incorrect number of parser errors'); {$ENDIF}
                  SkipTokeniserParseErrors();
                  Verify(FCurrentExpectedTokenIndex = ExpectedTokens.Length, 'early EOF - seen ' + IntToStr(FCurrentExpectedTokenIndex) + ' tokens, but expected ' + IntToStr(ExpectedTokens.Length));
                  break;
               end;
            else Assert(False);
         end;
         FCurrentToken.Reset();
         Assert(not FCurrentToken.Ready);
         Assert(FCurrentToken.Kind = tkNone);
      until forever;
   except
      {$IFDEF PARSEERROR}
         Writeln(FRecentParseErrors.Length, ' parse errors in last test:');
         for Error in FRecentParseErrors do
            Writeln(' * ', Error);
      {$ENDIF}
      raise;
   end;
end;

class procedure TTestHTMLParser.ProcessTokeniserTest(const Test: TJSONObject);

   function StringToTokeniserState(const Value: UTF8String): THTMLParser.TTokeniserState;
   begin
      if (Value = 'data state') then
         Result := tsDataState
      else
      if (Value = 'RCDATA state') then
         Result := tsRcdataState
      else
      if (Value = 'RAWTEXT state') then
         Result := tsRawtextState
      else
      if (value = 'PLAINTEXT state') then
         Result := tsPlaintextState
      else
         raise Exception.CreateFmt('Need to support tokeniser state "%s"', [Value]);
   end;

var
   Index: Cardinal;
   {$IFDEF PARSEERROR} ExpectedParseErrorCount: Cardinal; {$ENDIF}
   InitialStates: array of TTokeniserState;
   InitialState: TTokeniserState;
   TestParser: TTestHTMLParser;
   LastStartTag: UTF8String;
begin
//   ropes.DebugNow := Test['description'] = 'Ampersand EOF';
//   htmlparser.DebugNow := Test['description'] = 'Ampersand EOF';
   if (Assigned(Test['doubleEscaped']) and (Test['doubleEscaped'] = True)) then
      Exit; // not supported currently
   if (Assigned(Test['initialStates'])) then
   begin
      SetLength(InitialStates, Test['initialStates'].Length);
      if (Test['initialStates'].Length > 0) then
         for Index := 0 to Test['initialStates'].Length-1 do // $R-
            InitialStates[Index] := StringToTokeniserState(Test['initialStates'][Index]);
   end
   else
   begin
      SetLength(InitialStates, 1);
      InitialStates[0] := tsInitialState;
   end;
   if (Assigned(Test['lastStartTag'])) then
      LastStartTag := Test['lastStartTag']
   else
      LastStartTag := '';
   {$IFDEF PARSEERROR}
      ExpectedParseErrorCount := 0;                      
      if (Test['output'].Length > 0) then
         for Index := 0 to Test['output'].Length-1 do // $R-
            if (Test['output'][Index] = 'ParseError') then
               Inc(ExpectedParseErrorCount);
   {$ENDIF}
   for InitialState in InitialStates do {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
   begin
      TestParser := TTestHTMLParser.Create();
      try
         TestParser.RunTokeniserTest(InitialState, LastStartTag, Test['input'], Test['output'] {$IFDEF PARSEERROR}, ExpectedParseErrorCount {$ENDIF});
      finally
         TestParser.Free();
      end;
   end;
end;

class procedure TTestHTMLParser.ProcessParserTest(const TestInput, TestOutput: UTF8String; {$IFDEF PARSEERROR} const TestErrors: Cardinal; {$ENDIF} const SetScriptingEnabled: Boolean);
var
   TestParser: TTestHTMLParser;
   {$IFDEF PARSEERROR} Error: UTF8String; {$ENDIF}
   ActualOutput: UTF8String;
   OutputDocument: TDocument;
begin
//   DebugNow := TestInput = '<p><b><b><b><b><p>x';
//   if DebugNow then
//      Writeln('Hello, debug mode enabled for this test: "', TestInput, '"');
   TestParser := TTestHTMLParser.Create();
   OutputDocument := nil;
   try
      TestParser.ScriptingEnabled := SetScriptingEnabled;
      if (Length(TestInput) > 0) then
         TestParser.SpoonFeed(@TestInput[1], Length(TestInput)); // $R-
      {$IFDEF PARSEERROR} TestParser.FExpectedParseErrorCount := TestErrors; {$ENDIF}
      {$IFDEF PARSEERROR} TestParser.OnParseError := @TestParser.CountParseErrors; {$ENDIF}
      ActualOutput := '';
      try
         OutputDocument := TestParser.Parse();
         Assert(Assigned(OutputDocument));
         ActualOutput := SerialiseDOMForTestOutput(OutputDocument);
         Verify(ActualOutput = TestOutput, 'resulting parse tree does not match expectations');
         {$IFDEF PARSEERROR} Verify(TestParser.FRecentParseErrors.Length = TestErrors, 'incorrect number of parser errors'); {$ENDIF}
      except
         Writeln('Error: FAILED');
         Writeln('TestInput: ', TestInput);
         Writeln('Scripting Enabled: ', SetScriptingEnabled);
         Writeln('Expected Output: ');
         Writeln(TestOutput);
         Writeln('Actual Output: ');
         if (ActualOutput <> '') then
            Writeln(ActualOutput)
         else
         if (Assigned(TestParser.FDocument)) then
            Writeln(SerialiseDOMForTestOutput(TestParser.FDocument))
         else
            Writeln(' - no test output obtained');
         {$IFDEF PARSEERROR}
            Writeln(TestParser.FRecentParseErrors.Length, ' parse errors in last test (expected ', TestErrors, '):');
            for Error in TestParser.FRecentParseErrors do
               Writeln(' * ', Error);
         {$ENDIF}
         raise;
      end;
   finally
      OutputDocument.Free();
      TestParser.Free();
   end;
end;

procedure RunTokeniserTestFile(const FileName: UTF8String; var TestNumber: Cardinal);
var
   ParsedData: TJSON;
   Tests: TJSONArray;
   Index: Cardinal;
   Description: UTF8String;
begin
   ParsedData := ParseJSON(ReadTextFile(FileName));
   Tests := ParsedData['tests'];
   try
      if (Assigned(Tests) and (Tests.Length > 0)) then
         for Index := 0 to Tests.Length-1 do // $R-
            try
               Inc(TestNumber);
               // Write(#$0D + 'Test #', TestNumber, '     ');
               TTestHTMLParser.ProcessTokeniserTest(Tests[Index]);
            except
               on E: Exception do
               begin
                  //Writeln('Error: FAILED');
                  if (Assigned(Tests[Index]['description'])) then
                     Description := Tests[Index]['description']
                  else
                     Description := '(no test description)';
                  Writeln(FileName, '(0,1) Error: failed on subtest "', Description, '"');
                  ReportCurrentException();
                  raise;
               end;
            end;
   finally
      FreeAndNil(ParsedData);
   end;
end;

procedure RunParserTestFile(const FileName: UTF8String; var TestNumber: Cardinal);
var
   Buffer: UTF8String;
   EndOfBuffer: Boolean;
   Position, LineNumber: Cardinal;

   function ReadLine(): UTF8String;
   var
      StartIndex: Cardinal;
   begin
      Inc(Position);
      Inc(LineNumber);
      StartIndex := Position;
      while ((Position < Length(Buffer)) and (Buffer[Position] <> #$000A)) do
         Inc(Position);
      SetLength(Result, Position-StartIndex);
      if (Length(Result) > 0) then
         Move(Buffer[StartIndex], Result[1], Length(Result));
      EndOfBuffer := Position >= Length(Buffer);
   end;

type
   TScriptingMode = (smOn, smOff, smAgnostic);

var
   F: File;
   S, InputStream, ExpectedOutput: UTF8String;
   ErrorCount, SubtestCount, StartLineNumber: Cardinal;
   Scripting: TScriptingMode;
begin
   {$PUSH}
   {$IOCHECKS ON}
   // Write(FileName, '(1,1) ');
   Assign(F, FileName);
   Reset(F, 1);
   SetLength(Buffer, FileSize(F));
   BlockRead(F, Buffer[1], Length(Buffer), ErrorCount); // $R-
   if (ErrorCount <> Length(Buffer)) then
      raise Exception.Create('read error for test ' + FileName);
   Close(F);
   {$POP}
   EndOfBuffer := False;
   Position := 0;
   SubtestCount := 0;
   LineNumber := 0;
   S := ReadLine();
   if (S <> '#data') then
      raise Exception.CreateFmt('Bogus test format: "%s"', [S]);
   while not EndOfBuffer do
   begin
      Inc(TestNumber);
      Inc(SubtestCount);
      {$IFDEF DEBUG} SetHeapInfoTruncated(FileName + ':' + IntToStr(SubtestCount)); {$ENDIF}
      StartLineNumber := LineNumber;
      try
         InputStream := '';
         repeat
            S := ReadLine();
            if ((S = '') or (S[1] <> '#')) then
            begin
               if (InputStream <> '') then
                  InputStream := InputStream + #$0A + S
               else
                  InputStream := S;
            end;
         until (EndOfBuffer) or ((S <> '') and (S[1] = '#'));
         {$IFDEF PARSEERROR} ErrorCount := 0; {$ENDIF}
         if (S = '#errors') then
         begin
            repeat
               S := ReadLine();
               {$IFDEF PARSEERROR}
                  if ((S <> '') and (S[1] <> '#')) then
                     Inc(ErrorCount);
               {$ENDIF}
            until ((S = '') or (S[1] = '#'));
         end;
         if (S = '#document-fragment') then
         begin
            repeat
               S := ReadLine();
            until (EndOfBuffer) or (S = '#data');
            continue;
            XXX;
         end;
         if (S = '#script-off') then
         begin
            S := ReadLine();
            Scripting := smOff;
         end
         else
         if (S = '#script-on') then
         begin
            S := ReadLine();
            Scripting := smOn;
         end
         else
            Scripting := smAgnostic;
         if (S = '#document') then
         begin
            ExpectedOutput := '';
            repeat
               S := ReadLine();
               if (S = '#data') then
                  break;
               if (ExpectedOutput <> '') then
                  ExpectedOutput := ExpectedOutput + #$0A + S
               else
                  ExpectedOutput := S;
            until EndOfBuffer;
         end;
         if (ExpectedOutput <> '') then
            while (ExpectedOutput[Length(ExpectedOutput)] = #$000A) do
               SetLength(ExpectedOutput, Length(ExpectedOutput)-1);
         if (ExpectedOutput = '') then
            raise Exception.Create('Missing expected output in test');
         if (Scripting <> smOff) then
            TTestHTMLParser.ProcessParserTest(InputStream, ExpectedOutput, {$IFDEF PARSEERROR} ErrorCount, {$ENDIF} True);
         if (Scripting <> smOn) then
            TTestHTMLParser.ProcessParserTest(InputStream, ExpectedOutput, {$IFDEF PARSEERROR} ErrorCount, {$ENDIF} False);
      except
         on E: Exception do
         begin
            Writeln(FileName, '(', StartLineNumber, ',1) Error: failed on subtest "', SubtestCount, '"');
            ReportCurrentException();
            raise;
         end;
      end;
   end;
   //Writeln('Hint: PASSED');
end;

type
   TFileTestCallback = procedure (const FileName: UTF8String; var TestIndex: Cardinal);

var
   TestIndex: Cardinal;

procedure RunTestFiles(const Path, Pattern: AnsiString; const Callback: TFileTestCallback);
var
   FileRecord: TSearchRec;
begin
   try
      if (FindFirst(Path + Pattern, 0, FileRecord) = 0) then
         repeat
            Callback(Path + FileRecord.Name, TestIndex);
         until FindNext(FileRecord) <> 0;
   finally
      FindClose(FileRecord);
   end;
end;
{$ENDIF}

procedure ParseSpec();
var
   TestParser: THTMLParser;
   SpecSource: TFileData;
   OutputDocument: TDocument;
begin
   Writeln('HTML spec test');
   Writeln('Reading...');
   SpecSource := ReadFile('src/spec.html');
   Writeln('Parsing...');
   TestParser := THTMLParser.Create();
   try
      TestParser.SpoonFeed(SpecSource.Start, SpecSource.Length);
      OutputDocument := TestParser.Parse();
//      Writeln('Serialising...');
//      try
//         Writeln(SerialiseDOMForTestOutput(OutputDocument));
//      finally
         OutputDocument.Free();
//      end;
   finally
      TestParser.Free();
      SpecSource.Destroy();
   end;
end;

{
procedure TestSpecific(const Data: UTF8String);
var
   TestParser: THTMLParser;
   OutputDocument: TDocument;
begin
   TestParser := THTMLParser.Create();
   try
      TestParser.SpoonFeed(@Data[1], Length(Data));
      OutputDocument := TestParser.Parse();
      try
         Writeln(SerialiseDOMForTestOutput(OutputDocument));
      finally
         OutputDocument.Free();
      end;
   finally
      TestParser.Free();
   end;
end;
}

begin
   {$IFDEF DEBUG}
   TestIndex := 0;
   try
      // Write('Testing...');
      RunTestFiles('/home/ianh/hixie.ch/software/libraries/pascal/html/src/html5lib-tests/tokenizer/', '*.test', @RunTokeniserTestFile);
      RunTestFiles('/home/ianh/hixie.ch/software/libraries/pascal/html/src/html5lib-tests/tree-construction/', '*.dat', @RunParserTestFile);
      Writeln(TestIndex, ' tests passed');
   except
      Writeln('FAILED ON TEST ', TestIndex);
   end;
   {$ELSE}
   ParseSpec();
      // records:
      //    3.932s (user) + 0.100s (sys) = 4.0s for just parse
      //    6.488s (user) + 1.416s (sys) = 7.9s for parse+serialise;
      //    6.868s (user) + 1.288s (sys) = 8.2s for parse+serialise after preparing for ropes (maybe testing against new file?)
      //    7.052s (user) + 2.220s (sys) = 9.3s for parse+serialise after ropes :-(
   {$ENDIF}
end.
