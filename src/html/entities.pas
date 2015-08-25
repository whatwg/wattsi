{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit entities;

interface

uses
   unicode;

type
   TNamedCharacterReferenceParserResult = (ncOngoing,
                                           ncOngoingButBookmark, // implies that a bookmark should be taken in case of ncFinishedMissingSemicolon
                                           ncFinishedGood,
                                           ncFinishedMissingSemicolonNeedBacktrack, // implies that we should jump back to the last ncOngoingButBookmark bookmark
                                           ncFinishedNone); // implies the whole thing should be unconsumed
   TNamedCharacterReferenceParser = record
    private
     var
      FState: Cardinal;
      FLastStepReturnValue: TNamedCharacterReferenceParserResult;
      FFinalParsedValue: TUnicodeCodepointArray;
    public
     procedure Init();
     function Step(const Character: TUnicodeCodepoint): TNamedCharacterReferenceParserResult;
     property LastStepReturnValue: TNamedCharacterReferenceParserResult read FLastStepReturnValue;
     property FinalParsedValue: TUnicodeCodepointArray read FFinalParsedValue;
   end;

implementation

uses
   sysutils; // {$IFOPT C+}, utf8 {$ENDIF};

procedure TNamedCharacterReferenceParser.Init();
begin
   FState := 0;
end;

function TNamedCharacterReferenceParser.Step(const Character: TUnicodeCodepoint): TNamedCharacterReferenceParserResult;

// XXX should run the tests with this on and with this off to see what different it makes
// for now i can't because the compiler says "procedure too complex, needs too many registers"
//{$DEFINE INLINERESULT}

   procedure Incomplete(const NextState: Cardinal); {$IFDEF INLINERESULT} inline; {$ENDIF}
   begin
      FLastStepReturnValue := ncOngoing;
      Assert(NextState > FState);
      FState := NextState;
   end;

   procedure IncompleteButBookmark(const NextState: Cardinal); {$IFDEF INLINERESULT} inline; {$ENDIF}
   begin
      FLastStepReturnValue := ncOngoingButBookmark;
      Assert(NextState > FState);
      FState := NextState;
   end;

   procedure Fail(); {$IFDEF INLINERESULT} inline; {$ENDIF}
   begin
      FLastStepReturnValue := ncFinishedNone;
   end;

   procedure FinishedWithSemicolon(const Value: TUnicodeCodepoint); {$IFDEF INLINERESULT} inline; {$ENDIF}
   begin
      FLastStepReturnValue := ncFinishedGood;
      SetLength(FFinalParsedValue, 1);
      FFinalParsedValue[0] := Value;
   end;

   procedure FinishedWithSemicolon(const Value1, Value2: TUnicodeCodepoint); {$IFDEF INLINERESULT} inline; {$ENDIF}
   begin
      FLastStepReturnValue := ncFinishedGood;
      SetLength(FFinalParsedValue, 2);
      FFinalParsedValue[0] := Value1;
      FFinalParsedValue[1] := Value2;
   end;

   procedure FailButBacktrack(const Value: TUnicodeCodepoint); {$IFDEF INLINERESULT} inline; {$ENDIF}
   begin
      FLastStepReturnValue := ncFinishedMissingSemicolonNeedBacktrack;
      SetLength(FFinalParsedValue, 1);
      FFinalParsedValue[0] := Value;
   end;

begin
   Assert(Length(FFinalParsedValue) = 0);
   case (FState) of
   {$INCLUDE entities/entities.inc}
     else Assert(False, 'Reached state ' + IntToStr(FState) + ' which is invalid');
   end;
   Result := FLastStepReturnValue;
end;

end.