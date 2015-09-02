{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit htmlparser;

interface

uses
   unicode, plasticarrays, utf8, dom, webdom, canonicalstrings, {$IFDEF USEROPES} ropes {$ELSE} stringutils {$ENDIF}, wires, genericutils;

// This parser intentionally differs from the standard in the following ways:
//  - it assumes UTF-8
//  - it doesn't support scripting, document.write(), the insertion point
//  - it doesn't support streaming input; all data must be provided up-front
//  - it assumes that documents are not iframe srcdoc documents
//  - because it assumes no script is mutating the document as it goes,
//    certain features are simplified. For example, the "reconstruct the
//    active formatting elements" algorithm just clones the original, it
//    doesn't bother to store the original's token.

// don't forget to also set DebugNow appropriately
// {$DEFINE VERBOSETABLE}
// {$DEFINE VERBOSETOKENISER}
// {$DEFINE VERBOSEENUMERATOR}
// {$DEFINE VERBOSEAAA}
// {$DEFINE VERBOSETEXT}

// if you want line/col information, enable this (it's always enabled when assertions are enabled)
// {$DEFINE LINES}
{$IFOPT C+} {$DEFINE LINES} {$ENDIF}

// if you want parse error information, enable this (it's always enabled in debug mode)
// {$DEFINE PARSEERROR}
{$IFDEF DEBUG} {$DEFINE PARSEERROR} {$ENDIF}

// When optimising for a specific data set
//{$DEFINE INSTRUMENTING}

type
{$IFDEF USEROPES}
   TParserString = Rope;
   TCutParserString = CutRope;
   TParserStringPointer = TRopePointer;
   TParserStringEnumerator = RopeEnumerator;
{$ELSE}
   {$WARNING Ropes not enabled -- compile with USEROPES defined for optimal performance}
   TParserString = UTF8String;
   TCutParserString = CutUTF8String;
   TParserStringPointer = TUTF8StringPointer;
   TParserStringEnumerator = UTF8StringEnumerator;
{$ENDIF}

{$IFDEF PARSEERROR}
   TParseErrorHandler = procedure(const Message: UTF8String) of object;
{$ENDIF}

   THTMLParser = class
    protected
     type
      TInputStream = class
       strict private
        type
         TInputStreamFlag = (isAteCR, isManipulated, isPreConsumed {$IFDEF PARSEERROR}, isBadAlreadyNoted {$ENDIF} {$IFOPT C+}, isStarted, isSeenEOF {$ENDIF});
        var
         FData: TParserString;
         FEnumerator: TParserStringEnumerator;
         FInputStreamFlags: set of TInputStreamFlag;
         {$IFDEF LINES} FLine, FColumn: Cardinal; {$ENDIF}
         FLastInputCharacter: TUnicodeCodepoint;
         {$IFNDEF USEROPES} function GetCurrentCharacterLength(): TUTF8SequenceLength; inline; {$ENDIF}
         function GetFlag(const Flag: TInputStreamFlag): Boolean;
       public
        type
         TBookmark = record
           private
            FPosition: TParserStringPointer;
            FLastInputCharacter: TUnicodeCodepoint;
            FInputStreamFlags: set of TInputStreamFlag;
            function GetFlag(const Flag: TInputStreamFlag): Boolean;
           public
            function GetPointer(): TParserStringPointer;
            property WasManipulated: Boolean index isManipulated read GetFlag;
         end;
         constructor Create();
         destructor Destroy(); override;
         {$IFNDEF USEROPES} procedure PushData(const NewData: UTF8String); inline; {$ENDIF}
         procedure PushData(const NewData: Pointer; const NewLength: QWord); inline;
         procedure Freeze(); // do not use enumerator before this
         procedure Advance();
         function GetBookmark(): TBookmark;
         procedure ReturnToBookmark(constref NewPosition: TBookmark);
         procedure Unconsume();
         function Extract(constref NewStart, NewEnd: TParserStringPointer): TCutParserString;
         function GetPointer(): TParserStringPointer; inline;
         {$IFOPT C+} function GetDebugData(): UTF8String; {$ENDIF}
         {$IFNDEF USEROPES} function GetRawPointer(): Pointer; inline; {$ENDIF}
         {$IFDEF PARSEERROR} procedure NotedThatInputIsBad(); inline; {$ENDIF}
         property CurrentCharacter: TUnicodeCodepoint read FLastInputCharacter;
         {$IFNDEF USEROPES} property CurrentCharacterLength: TUTF8SequenceLength read GetCurrentCharacterLength; {$ENDIF}
         property WasManipulated: Boolean index isManipulated read GetFlag;
         {$IFDEF PARSEERROR} property WasNotedBad: Boolean index isBadAlreadyNoted read GetFlag; {$ENDIF}
         {$IFOPT C+} property WasStarted: Boolean index isStarted read GetFlag; {$ENDIF}
         property Data: TParserString read FData;
         {$IFDEF LINES}
         property Line: Cardinal read FLine;
         property Column: Cardinal read FColumn;
         {$ENDIF}
      end;
      TTokeniserState = (tsInitialState, tsDataState, tsRcdataState, tsRawtextState, tsScriptDataState, tsPlaintextState,
                         tsTagOpenState, tsEndTagOpenState, tsTagNameState, tsRcdataLessThanSignState, tsRcdataEndTagOpenState,
                         tsRcdataEndTagNameState, tsRawtextLessThanSignState, tsRawtextEndTagOpenState, tsRawtextEndTagNameState,
                         tsScriptDataLessThanSignState, tsScriptDataEndTagOpenState, tsScriptDataEndTagNameState,
                         tsScriptDataEscapeStartState, tsScriptDataEscapeStartDashState, tsScriptDataEscapedState,
                         tsScriptDataEscapedDashState, tsScriptDataEscapedDashDashState, tsScriptDataEscapedLessThanSignState,
                         tsScriptDataEscapedEndTagOpenState, tsScriptDataEscapedEndTagNameState, tsScriptDataDoubleEscapeStartState,
                         tsScriptDataDoubleEscapedState, tsScriptDataDoubleEscapedDashState, tsScriptDataDoubleEscapedDashDashState,
                         tsScriptDataDoubleEscapedLessThanSignState, tsScriptDataDoubleEscapeEndState, tsBeforeAttributeNameState,
                         tsAttributeNameState, tsAfterAttributeNameState, tsBeforeAttributeValueState,
                         tsAttributeValueDoubleQuotedState, tsAttributeValueSingleQuotedState, tsAttributeValueUnquotedState,
                         {tsCharacterReferenceInAttributeValueState,} tsAfterAttributeValueQuotedState, tsSelfClosingStartTagState,
                         tsBogusCommentState, tsMarkupDeclarationOpenState, tsCommentStartState, tsCommentStartDashState,
                         tsCommentState, tsCommentEndDashState, tsCommentEndState, tsCommentEndBangState, tsDoctypeState,
                         tsBeforeDoctypeNameState, tsDoctypeNameState, tsAfterDoctypeNameState, tsAfterDoctypePublicKeywordState,
                         tsBeforeDoctypePublicIdentifierState, tsDoctypePublicIdentifierDoubleQuotedState,
                         tsDoctypePublicIdentifierSingleQuotedState, tsAfterDoctypePublicIdentifierState,
                         tsBetweenDoctypePublicAndSystemIdentifiersState, tsAfterDoctypeSystemKeywordState,
                         tsBeforeDoctypeSystemIdentifierState, tsDoctypeSystemIdentifierDoubleQuotedState,
                         tsDoctypeSystemIdentifierSingleQuotedState, tsAfterDoctypeSystemIdentifierState, tsBogusDoctypeState,
                         tsCdataSectionState);
      TTokenKind = (tkNone, tkDOCTYPE, tkStartTag, tkEndTag, tkSourceCharacters, tkExtraCharacters, tkExtraSpaceCharacter, tkNullCharacter, tkComment, tkEOF);
      TToken = record // this is an expensive type, as it is basically all token types at once, so don't create many
       {$IFDEF PARSEERROR}
         strict private
          type
            TTokenParseErrorCallback = procedure (Message: UTF8String; const Count: Cardinal = 1) of object;
          var
            FOnParseError: TTokenParseErrorCallback;
       {$ENDIF}
       public
         Kind: TTokenKind;
         Ready: Boolean;
         {$IFDEF PARSEERROR} procedure Init(NewOnParseError: TTokenParseErrorCallback); {$ENDIF}
         procedure Reset();
         procedure Destroy();
         procedure Emit();
       public
         // tkDOCTYPE
         DOCTYPEName, PublicID, SystemID: TParserString;
         DOCTYPENamePresent, PublicIDPresent, SystemIDPresent, ForceQuirksFlag: Boolean;
         procedure PrepareDOCTYPE();
       public
         // tkStartTag, tkEndTag
         DraftTagName: TWire; // only for writing
         TagName: TCanonicalString; // only for reading
         SelfClosingFlag: Boolean;
         {$IFDEF PARSEERROR} SelfClosingAcknowledged: Boolean; {$ENDIF}
         CurrentAttributeName: TWire;
         CurrentAttributeValue: TParserString;
       strict private
         FAttributes: TElement.TAttributeHashTable;
         procedure PrepareTag(const NewKind: TTokenKind);
         procedure SaveAttribute(); inline;
         procedure ReplaceAttribute(const OldName, NewName: UTF8String); inline;
       public
         procedure PrepareStartTag(); inline;
         procedure PrepareEndTag(); inline;
         procedure RetractTag(); inline;
         procedure PrepareAttribute(); inline;
         {$IFDEF PARSEERROR} procedure AcknowledgeSelfClosingFlag(); inline; {$ENDIF}
         function TakeAttributes(): TElement.TAttributeHashTable; inline;
         function GetAttribute(const AttributeName: UTF8String): UTF8String;
         function HasAttributes(const AttributeNames: array of UTF8String): Boolean;
         procedure AdjustMathMLAttributes();
         procedure AdjustSVGAttributes();
         procedure AdjustForeignAttributes();
       public
         // Source Characters
        type
         TSpaceState = (ssHaveLeadingSpace, ssHaveNonSpace, ssMightHaveNonLeadingSpace);
        var
         HavePendingCharacters: Boolean;
         SpaceState: set of TSpaceState;
         // if you append source characters with a gap between subsequent pointers, the characters between the pointers
         // will be added; they will be assumed to be non-space if you call AppendSourceNonSpaceCharacter(), and
         // space if you call AppendSourceSpaceCharacter(); but only do this from AppendFromBookmarkedCharacterToInputCharacter()!
       {$IFOPT C-} strict private {$ENDIF}
         FCharStart, FCharEnd: TParserStringPointer;
       public
         procedure AppendSourceNonSpaceCharacter(constref NewPointer: TParserStringPointer); inline;
         procedure AppendSourceSpaceCharacter(constref NewPointer: TParserStringPointer); inline;
         procedure AppendSourceNonSpaceBetweenBookmarks(constref StartPointer, EndPointer: TParserStringPointer);
         procedure EmitSourceCharacters(); // flushes the token if HavePendingCharacters
         procedure SkipLeadingSpaces(constref Data: TParserString);
         function SkipLeadingNonSpaces(constref Data: TParserString): Cardinal;
         function SkipOneLeadingNewline(constref Data: TParserString): Boolean; inline; // returns true if there's still more characters to deal with
         function ExtractLeadingSpaces(constref Data: TParserString): TCutParserString; // mutates self
         function ExtractSourceCharacters(constref Data: TParserString): TCutParserString; inline; // doesn't mutate self
         function GetCharacterCount(constref Data: TParserString): Cardinal; inline; // expensive
       public
         // Extra Characters
         ExtraChars: TUnicodeCodepointArray;
         procedure EmitExtraSpaceCharacter(const Character: TUnicodeCodepoint); inline;
         procedure EmitExtraNonSpaceCharacter(const Character: TUnicodeCodepoint); inline;
         procedure EmitExtraAmbiguousCharacters(const Characters: TUnicodeCodepointArray); inline;
         procedure EmitNullCharacter(); inline;
       public
         // Comment
         CommentValue: TParserString;
         procedure PrepareComment(); inline;
       public
         // tkEOF
         procedure EmitEOF();
      end;
      TInsertionModeHandler = procedure (var Token: TToken) of object;
      TInsertionModeHandlerUtils = specialize DefaultUnorderedUtils <TInsertionModeHandler>;
     const
      Marker = nil;
     var
      FProprietaryVoids: specialize PlasticArray <TCanonicalString, TCanonicalString>;
      FInputStream: TInputStream;
      {$IFDEF PARSEERROR} FOnParseError: TParseErrorHandler; {$ENDIF}
      FTokeniserState: TTokeniserState;
      FCurrentToken: TToken; // only access this from tokeniser code; tree construction code should pass the Token in arguments
      FInsertionMode, FOriginalInsertionMode: TInsertionModeHandler;
      FDocument: TDocument;
      FStackOfTemplateInsertionModes: specialize PlasticArray<TInsertionModeHandler, TInsertionModeHandlerUtils>;
      FContextElement, FHeadElementPointer, FFormElementPointer: TElement;
      FLastStartTag: UTF8String;
      FTemporaryBuffer: TWire;
      FPendingTableCharacterTokensList: TParserString;
      FStackOfOpenElements: specialize PlasticArray<TElement, TObjectUtils>;
      FListOfActiveFormattingElements: specialize PlasticArray<TElement, TObjectUtils>;
      FFosterParenting: Boolean; // only true while processing the "anything else" clause of TheInTableInsertionMode
      FFramesetOkFlag: Boolean;
      FFragmentParsingMode, FScriptingFlag: Boolean;
      FPendingTableCharacterTokensListHasNonSpaces: Boolean; // only useful in TheInTableTextInsertionMode
      {$IFDEF PARSEERROR} procedure ParseError(Message: UTF8String; const Count: Cardinal = 1); {$ENDIF}
      procedure TreeConstructionDispatcher(var Token: TToken); inline;
      function StackOfOpenElementsHas(constref Namespace, LocalName: TCanonicalString): Boolean;
      function StackOfOpenElementsHasElementOtherThan(const TargetProperty: TElementProperties): Boolean;
      function StackOfOpenElementsHasInSpecificScope(constref Target: TCanonicalString; const ParticularScope: TElementProperties): Boolean; // nsHTML is assumed for Target
      function StackOfOpenElementsHasInSpecificScope(const Target: TElementProperties; const ParticularScope: TElementProperties): Boolean;
      function StackOfOpenElementsHasInSpecificScope(const Target: TElement; const ParticularScope: TElementProperties): Boolean;
      function StackOfOpenElementsHasInScope(constref Target: TCanonicalString): Boolean; inline;
      function StackOfOpenElementsHasInScope(const Target: TElementProperties): Boolean; inline;
      function StackOfOpenElementsHasInScope(const Target: TElement): Boolean; inline;
      function StackOfOpenElementsHasInListItemScope(constref Target: TCanonicalString): Boolean; inline;
      function StackOfOpenElementsHasInButtonScope(constref Target: TCanonicalString): Boolean; inline;
      function StackOfOpenElementsHasInTableScope(constref Target: TCanonicalString): Boolean; inline;
      function StackOfOpenElementsHasInTableScope(const Target: TElementProperties): Boolean; inline;
      function StackOfOpenElementsHasInSelectScope(constref Target: TCanonicalString): Boolean; inline;
      procedure InsertMarkerAtEndOfListOfActiveFormattingElements(); inline;
      procedure PushOntoTheListOfActiveFormattingElements(const Target: TElement);
      procedure ReconstructTheActiveFormattingElements();
      procedure ClearTheListOfActiveFormattingElementsUpToTheLastMarker(); inline;
      procedure Tokenise();
      function GetCurrentNode(): TElement; inline; // don't call directly, use CurrentNode
      function GetAdjustedCurrentNode(): TElement; inline; // don't call directly, use AdjustedCurrentNode
      procedure InsertNodeAtAppropriatePlaceForInsertingANode(const Node: TNode; Target: TElement = nil);
      function CreateAnElementFor(constref Token: TToken; constref Namespace: TCanonicalString): TElement;
      function InsertAForeignElementFor(constref Token: TToken; constref Namespace: TCanonicalString): TElement;
      function InsertAForeignElement(const Element: TElement): TElement;
      function InsertAnHTMLElementFor(constref Token: TToken): TElement; inline;
      function InsertAnHTMLElement(const Element: TElement): TElement; inline;
      procedure InsertCharacters(var Data: TCutParserString);
      {$IFDEF USEROPES} procedure InsertCharacters(var Data: TParserString); {$ENDIF}
      procedure InsertCharacters(const Data: UTF8String);
      procedure InsertCharacters(const Data: TUnicodeCodepointArray); inline;
      procedure InsertCharactersFor(constref Token: TToken); inline;
      procedure InsertLeadingSpacesFor(var Token: TToken); inline;
      function CreateACommentFor(var Token: TToken): TComment; inline;
      procedure InsertAComment(var Token: TToken); inline;
      procedure ResetTheInsertionModeAppropriately();
      procedure GenericRCDataElementParsingAlgorithm(constref Token: TToken);
      procedure GenericRawTextElementParsingAlgorithm(constref Token: TToken);
      procedure GenerateImpliedEndTags(); inline;
      procedure GenerateAllImpliedEndTagsThoroughly(); inline;
      procedure GenerateImpliedEndTagsExceptFor(constref Exception: TCanonicalString); inline;
      procedure TheInitialInsertionMode(var Token: TToken);
      procedure TheBeforeHTMLInsertionMode(var Token: TToken);
      procedure TheBeforeHeadInsertionMode(var Token: TToken);
      procedure TheInHeadInsertionMode(var Token: TToken);
      procedure TheInHeadNoScriptInsertionMode(var Token: TToken);
      procedure TheAfterHeadInsertionMode(var Token: TToken);
      procedure TheInBodyInsertionMode(var Token: TToken);
      procedure TheTextInsertionMode(var Token: TToken);
      procedure TheInTableInsertionMode(var Token: TToken);
      procedure TheInTableTextInsertionMode(var Token: TToken);
      procedure TheInCaptionInsertionMode(var Token: TToken);
      procedure TheInColumnGroupInsertionMode(var Token: TToken);
      procedure TheInTableBodyInsertionMode(var Token: TToken);
      procedure TheInRowInsertionMode(var Token: TToken);
      procedure TheInCellInsertionMode(var Token: TToken);
      procedure TheInSelectInsertionMode(var Token: TToken);
      procedure TheInSelectInTableInsertionMode(var Token: TToken);
      procedure TheInTemplateInsertionMode(var Token: TToken);
      procedure TheAfterBodyInsertionMode(var Token: TToken);
      procedure TheInFramesetInsertionMode(var Token: TToken);
      procedure TheAfterFramesetInsertionMode(var Token: TToken);
      procedure TheAfterAfterBodyInsertionMode(var Token: TToken);
      procedure TheAfterAfterFramesetInsertionMode(var Token: TToken);
      procedure TheSkipNewLineInsertionMode(var Token: TToken);
      procedure TheSkipNewLineThenTextInsertionMode(var Token: TToken);
      procedure TheRulesForParsingTokensInForeignContent(var Token: TToken);
      property CurrentNode: TElement read GetCurrentNode; // only check this if there is one
      property AdjustedCurrentNode: TElement read GetAdjustedCurrentNode; // only check this if there is one
    public
      constructor Create();
      destructor Destroy(); override;
      {$IFNDEF USEROPES}
      procedure SpoonFeed(const Data: UTF8String); // call this any number of times until all characters have been provided
      {$ENDIF}
      procedure SpoonFeed(const Data: Pointer; const Length: QWord); // call this any number of times until all characters have been provided
      procedure RegisterProperietaryVoidElements(const TagNames: array of TCanonicalString);
      function Parse(): TDocument; // then call this
      // XXX need a fragment parsing mode (if we support fragment parsing, set FFragmentParsingMode to true)
      {$IFDEF PARSEERROR} property OnParseError: TParseErrorHandler read FOnParseError write FOnParseError; {$ENDIF}
      property ScriptingEnabled: Boolean read FScriptingFlag write FScriptingFlag;
   end;

{$IF DEFINED(VERBOSETABLE) OR DEFINED(VERBOSETOKENISER) OR DEFINED(VERBOSEENUMERATOR) OR DEFINED(VERBOSEAAA) OR DEFINED(VERBOSETEXT)}
var
   DebugNow: Boolean = False;
{$ENDIF}

implementation

uses exceptions, entities, sysutils, specutils;

const
   forever = False;

function PrefixMatch(const Value, Candidate: UTF8String): Boolean;
type
   TBlob = Pointer;
   PBlobArray = ^TBlobArray;
   TBlobArray = array[0..0] of TBlob;
var
   Steps, EndOfBlobs: Cardinal;
   Index: Cardinal;
   Blob1, Blob2: PBlobArray;
begin
   Assert(Length(Candidate) > 0);
   if (Length(Candidate) > Length(Value)) then
   begin
      Result := False;
      exit;
   end;
   Assert(Length(Value) >= Length(Candidate));
   Assert(Length(Candidate) > 0);
   Assert(SizeOf(TBlob) > 0);
   Steps := Length(Candidate) div SizeOf(TBlob); // $R-
   if (Steps > 0) then
   begin
      Blob1 := PBlobArray(@Value[1]);
      Blob2 := PBlobArray(@Candidate[1]);
      for Index := 0 to Steps-1 do // $R-
         if (Blob1^[Index] <> Blob2^[Index]) then
         begin
            Result := False;
            exit;
         end;
      Assert(High(EndOfBlobs) > Length(Candidate));
      EndOfBlobs := Steps * SizeOf(TBlob); // $R-
   end
   else
   begin
      EndOfBlobs := 0;
   end;
   if (EndOfBlobs < Length(Candidate)) then
      for Index := EndOfBlobs+1 to Length(Candidate) do // $R-
         if (Value[Index] <> Candidate[Index]) then
         begin
            Result := False;
            exit;
         end;
   Result := True;
end;

function IsSpaceCharacter(const Candidate: TUnicodeCodepoint): Boolean;
begin
   Assert(Candidate <> $0000);
   case (Candidate.Value) of
      $0009, $000A, $000C, $000D, $0020: Result := True;
      else
         Result := False;
   end;
end;

{$IFOPT C+}
function IsSpaceCharacter(const Candidate: Byte): Boolean;
begin
   Assert(Candidate <> $00);
   case (Candidate) of
      $09, $0A, $0C, $0D, $20: Result := True;
      else
         Result := False;
   end;
end;
{$ENDIF}

function THTMLParser.TInputStream.TBookmark.GetFlag(const Flag: TInputStreamFlag): Boolean;
begin
   Result := Flag in FInputStreamFlags;
end;

function THTMLParser.TInputStream.TBookmark.GetPointer(): TParserStringPointer;
begin
   Result := FPosition;
end;

constructor THTMLParser.TInputStream.Create();
begin
   inherited;
   {$IFDEF LINES}
   FLine := 1;
   FColumn := 1;
   {$ENDIF}
end;

destructor THTMLParser.TInputStream.Destroy();
begin
   FEnumerator.Free();
   inherited;
end;

{$IFNDEF USEROPES}
function THTMLParser.TInputStream.GetCurrentCharacterLength(): TUTF8SequenceLength; inline;
begin
   Assert(Assigned(FEnumerator));
   Result := FEnumerator.CurrentLength;
end;

procedure THTMLParser.TInputStream.PushData(const NewData: UTF8String);
begin
   {$IFOPT C+} Assert(not (isStarted in FInputStreamFlags)); {$ENDIF}
   Assert(not Assigned(FEnumerator));
   FData.Append(@NewData);
end;
{$ENDIF}

procedure THTMLParser.TInputStream.PushData(const NewData: Pointer; const NewLength: QWord);
begin
   {$IFOPT C+} Assert(not (isStarted in FInputStreamFlags)); {$ENDIF}
   Assert(not Assigned(FEnumerator));
   FData.Append(NewData, NewLength);
end;

procedure THTMLParser.TInputStream.Freeze();
begin
   {$IFOPT C+} Assert(not (isStarted in FInputStreamFlags)); {$ENDIF}
   Assert(not Assigned(FEnumerator));
   FEnumerator := TParserStringEnumerator.Create(@FData);
end;

procedure THTMLParser.TInputStream.Advance();
begin
   Assert(Assigned(FEnumerator));
   {$IFOPT C+} Include(FInputStreamFlags, isStarted); {$ENDIF}
   if (isPreConsumed in FInputStreamFlags) then
   begin
      Exclude(FInputStreamFlags, isPreConsumed);
      {$IFDEF VERBOSEENUMERATOR} if (DebugNow) then Writeln('Advance(): preconsumed - exiting with ', FLastInputCharacter.GetDebugDescription()); {$ENDIF}
      exit;
   end;
   {$IFOPT C+} Assert(not (isSeenEOF in FInputStreamFlags)); {$ENDIF}
   Exclude(FInputStreamFlags, isManipulated);
   repeat
      {$IFDEF VERBOSEENUMERATOR} if (DebugNow) then Writeln('Advance(): calling MoveNext() at ', FLine, ':', FColumn); {$ENDIF}
      if (not FEnumerator.MoveNext()) then
      begin
         {$IFDEF VERBOSEENUMERATOR} if (DebugNow) then Writeln('Advance(): MoveNext() returned false'); {$ENDIF}
         FLastInputCharacter := kEOF;
         Include(FInputStreamFlags, isManipulated);
         {$IFOPT C+} Include(FInputStreamFlags, isSeenEOF); {$ENDIF}
      end
      else
      begin
         FLastInputCharacter := FEnumerator.Current;
         {$IFDEF VERBOSEENUMERATOR} if (DebugNow) then Writeln('Advance(): Current is ', FLastInputCharacter.GetDebugDescription()); {$ENDIF}
         // CRLF handling
         if (FLastInputCharacter = $000D) then
         begin
            // turn CR into LF
            {$IFDEF VERBOSEENUMERATOR} if (DebugNow) then Writeln('Advance(): turning CR into LF'); {$ENDIF}
            FLastInputCharacter := $000A;
            Include(FInputStreamFlags, isManipulated);
            Include(FInputStreamFlags, isAteCR);
         end
         else
         if (isAteCR in FInputStreamFlags) then
         begin
            {$IFDEF VERBOSEENUMERATOR} if (DebugNow) then Writeln('Advance(): checking if this is an LF, since last we saw a CR'); {$ENDIF}
            // skip LF if following CR
            Exclude(FInputStreamFlags, isAteCR);
            if (FLastInputCharacter = $000A) then
            begin
               {$IFDEF VERBOSEENUMERATOR} if (DebugNow) then Writeln('Advance(): yup, this is an LF; looping over to MoveNext() again'); {$ENDIF}
               continue;
            end;
         end;
         {$IFDEF LINES}
         // count lines
         if (FLastInputCharacter = $000A) then
         begin
            {$IFDEF VERBOSEENUMERATOR} if (DebugNow) then Writeln('Advance(): incrementing line...'); {$ENDIF}
            Inc(FLine);
            FColumn := 1;
         end
         else
            Inc(FColumn);
         {$ENDIF}
      end;
      exit;
   until forever;
   {$IFDEF VERBOSEENUMERATOR} if (DebugNow) then Writeln('Advance(): exiting with ', FLastInputCharacter.GetDebugDescription()); {$ENDIF}
end;

function THTMLParser.TInputStream.GetFlag(const Flag: TInputStreamFlag): Boolean;
begin
   Result := Flag in FInputStreamFlags;
end;

{$IFDEF PARSEERROR}
procedure THTMLParser.TInputStream.NotedThatInputIsBad();
begin
   Include(FInputStreamFlags, isBadAlreadyNoted);
end;
{$ENDIF}

function THTMLParser.TInputStream.GetBookmark(): TBookmark;
begin
   {$IFDEF VERBOSEENUMERATOR} if (DebugNow) then Writeln('GetBookmark() - GetPointer()'); {$ENDIF}
   Assert(Assigned(FEnumerator));
   Result.FPosition := FEnumerator.GetPointer();
   Result.FLastInputCharacter := FLastInputCharacter;
   Result.FInputStreamFlags := FInputStreamFlags;
   {$IFOPT C+} Result.FPosition.AssertIdentity(@FData); {$ENDIF}
end;

{$IFOPT C+}
function THTMLParser.TInputStream.GetDebugData(): UTF8String;

   function GetPos(): UTF8String;
   begin
      Result := 'L' + IntToStr(FLine) + ' C' + IntToStr(FColumn);
   end;

begin
   if (FLastInputCharacter = kEOF) then
      Result := '[EOF ' + GetPos() + ']'
   else
      Result := CodepointToUTF8(FLastInputCharacter) + ' (' + GetPos() + ')';
end;
{$ENDIF}

procedure THTMLParser.TInputStream.ReturnToBookmark(constref NewPosition: TBookmark);
begin
   {$IFDEF VERBOSEENUMERATOR} if (DebugNow) then Writeln('ReturnToBookmark() - ReturnToPointer()'); {$ENDIF}
   Assert(Assigned(FEnumerator));
   FEnumerator.ReturnToPointer(NewPosition.FPosition);
   FLastInputCharacter := NewPosition.FLastInputCharacter;
   FInputStreamFlags := NewPosition.FInputStreamFlags;
end;

procedure THTMLParser.TInputStream.Unconsume();
begin
   Assert(not (isPreConsumed in FInputStreamFlags));
   Include(FInputStreamFlags, isPreConsumed);
end;

function THTMLParser.TInputStream.Extract(constref NewStart, NewEnd: TParserStringPointer): TCutParserString;
begin
   Result := FData.Extract(NewStart, NewEnd);
end;

function THTMLParser.TInputStream.GetPointer(): TParserStringPointer; inline;
begin
   {$IFDEF VERBOSEENUMERATOR} if (DebugNow) then Writeln('GetPointer() - GetPointer()'); {$ENDIF}
   Assert(Assigned(FEnumerator));
   Result := FEnumerator.GetPointer();
end;

{$IFNDEF USEROPES}
function THTMLParser.TInputStream.GetRawPointer(): Pointer; inline;
begin
   {$IFDEF VERBOSEENUMERATOR} if (DebugNow) then Writeln('GetRawPointer() - GetRawPointer'); {$ENDIF}
   Assert(Assigned(FEnumerator));
   Result := FEnumerator.GetRawPointer();
end;
{$ENDIF}

{$IFDEF PARSEERROR}
procedure THTMLParser.TToken.Init(NewOnParseError: TTokenParseErrorCallback);
begin
   FOnParseError := NewOnParseError;
   Assert(Assigned(FOnParseError));
end;
{$ENDIF}

procedure THTMLParser.TToken.Reset();
begin
   {$IFDEF PARSEERROR} Assert(Assigned(FOnParseError)); {$ENDIF}
   Assert(Kind <> tkNone);
   if (Kind in [tkStartTag, tkEndTag]) then
   begin
      if (Assigned(FAttributes)) then
         FAttributes.Empty();
   end;
   Kind := tkNone;
   HavePendingCharacters := False;
   SpaceState := [];
   Ready := False;
end;

procedure THTMLParser.TToken.Destroy();
begin
   {$IFDEF PARSEERROR} Assert(Assigned(FOnParseError)); {$ENDIF}
   FAttributes.Free();
end;

procedure THTMLParser.TToken.Emit();
begin
   {$IFDEF PARSEERROR} Assert(Assigned(FOnParseError)); {$ENDIF}
   Assert(Kind in [tkDOCTYPE, tkStartTag, tkEndTag, tkComment]);
   if (Kind in [tkStartTag, tkEndTag]) then
   begin
      TagName := Intern(DraftTagName.AsString);
      if (not CurrentAttributeName.IsEmpty) then
         SaveAttribute();
      {$IFDEF PARSEERROR}
         if (Kind = tkEndTag) then
         begin
            if (Assigned(FAttributes) and (FAttributes.Count > 0)) then
               FOnParseError('End tag had attributes');
            if (SelfClosingFlag) then
               FOnParseError('End tag had trailing slash');
         end;
      {$ENDIF}
   end;
   Assert(Kind in [tkDOCTYPE, tkStartTag, tkEndTag, tkComment]);
   Ready := True;
end;

procedure THTMLParser.TToken.PrepareDOCTYPE();
begin
   Assert(Kind = tkNone);
   Kind := tkDOCTYPE;
   DOCTYPENamePresent := False;
   DOCTYPEName := Default(TParserString);
   PublicIDPresent := False;
   PublicID := Default(TParserString);
   SystemIDPresent := False;
   SystemID := Default(TParserString);
   ForceQuirksFlag := False;
end;

procedure THTMLParser.TToken.PrepareTag(const NewKind: TTokenKind);
begin
   Assert(Kind = tkNone);
   Kind := NewKind;
   DraftTagName.Init();
   SelfClosingFlag := False;
   {$IFDEF PARSEERROR} SelfClosingAcknowledged := False; {$ENDIF}
   CurrentAttributeName.Init();
end;

procedure THTMLParser.TToken.SaveAttribute();
var
   CachedAttributeName: UTF8String;
begin
   {$IFDEF PARSEERROR} Assert(Assigned(FOnParseError)); {$ENDIF}
   Assert(not CurrentAttributeName.IsEmpty);
   CachedAttributeName := CurrentAttributeName.AsString;
   if (not Assigned(FAttributes)) then
      FAttributes := TElement.TAttributeHashTable.Create()
   else
   if (FAttributes.Has(CachedAttributeName)) then
   begin
      {$IFDEF PARSEERROR} FOnParseError('duplicate attribute ("' + CachedAttributeName + '")'); {$ENDIF}
      exit;
   end;
   FAttributes.Add(CachedAttributeName, CurrentAttributeValue);
   CurrentAttributeName.Init();
   CurrentAttributeValue := Default(TParserString);
end;

procedure THTMLParser.TToken.PrepareStartTag();
begin
   PrepareTag(tkStartTag);
end;

procedure THTMLParser.TToken.PrepareEndTag();
begin
   PrepareTag(tkEndTag);
end;

procedure THTMLParser.TToken.RetractTag();
begin
   Assert(Kind in [tkStartTag, tkEndTag]);
   {$IFOPT C+} Kind := tkNone; {$ENDIF}
   Assert(not Ready);
end;

procedure THTMLParser.TToken.PrepareAttribute();
begin
   if (not CurrentAttributeName.IsEmpty) then
   begin
      SaveAttribute();
      CurrentAttributeName.Init();
      CurrentAttributeValue := Default(TParserString);
   end;
   Assert(CurrentAttributeValue.IsEmpty);
end;

{$IFDEF PARSEERROR}
procedure THTMLParser.TToken.AcknowledgeSelfClosingFlag();
begin
   Assert(Kind = tkStartTag);
   SelfClosingAcknowledged := True;
end;
{$ENDIF}

function THTMLParser.TToken.TakeAttributes(): TElement.TAttributeHashTable;
begin
   if (Assigned(FAttributes) and (FAttributes.Count > 0)) then
   begin
      Result := FAttributes;
      FAttributes := nil;
   end
   else
   begin
      Result := nil;
   end;
end;

function THTMLParser.TToken.GetAttribute(const AttributeName: UTF8String): UTF8String;
begin
   if (Assigned(FAttributes)) then
      Result := FAttributes[AttributeName].AsString
   else
      Result := '';
end;

function THTMLParser.TToken.HasAttributes(const AttributeNames: array of UTF8String): Boolean;
var
   Name: UTF8String;
begin
   if (Assigned(FAttributes)) then
      for Name in AttributeNames do // http://bugs.freepascal.org/view.php?id=25703
         if (FAttributes.Has(Name)) then
         begin
            Result := True;
            exit;
         end;
   Result := False;
end;

procedure THTMLParser.TToken.ReplaceAttribute(const OldName, NewName: UTF8String);
begin
   if (FAttributes.Has(OldName)) then
   begin
      FAttributes[NewName] := FAttributes[OldName];
      FAttributes.Remove(OldName);
   end;
end;

procedure THTMLParser.TToken.AdjustMathMLAttributes();
begin
   if (not Assigned(FAttributes)) then exit;
   ReplaceAttribute('definitionurl', 'definitionURL');
end;

procedure THTMLParser.TToken.AdjustSVGAttributes();
begin
   if (not Assigned(FAttributes)) then exit;
   ReplaceAttribute('attributename', 'attributeName');
   ReplaceAttribute('attributetype', 'attributeType');
   ReplaceAttribute('basefrequency', 'baseFrequency');
   ReplaceAttribute('baseprofile', 'baseProfile');
   ReplaceAttribute('calcmode', 'calcMode');
   ReplaceAttribute('clippathunits', 'clipPathUnits');
   ReplaceAttribute('diffuseconstant', 'diffuseConstant');
   ReplaceAttribute('edgemode', 'edgeMode');
   ReplaceAttribute('filterunits', 'filterUnits');
   ReplaceAttribute('glyphref', 'glyphRef');
   ReplaceAttribute('gradienttransform', 'gradientTransform');
   ReplaceAttribute('gradientunits', 'gradientUnits');
   ReplaceAttribute('kernelmatrix', 'kernelMatrix');
   ReplaceAttribute('kernelunitlength', 'kernelUnitLength');
   ReplaceAttribute('keypoints', 'keyPoints');
   ReplaceAttribute('keysplines', 'keySplines');
   ReplaceAttribute('keytimes', 'keyTimes');
   ReplaceAttribute('lengthadjust', 'lengthAdjust');
   ReplaceAttribute('limitingconeangle', 'limitingConeAngle');
   ReplaceAttribute('markerheight', 'markerHeight');
   ReplaceAttribute('markerunits', 'markerUnits');
   ReplaceAttribute('markerwidth', 'markerWidth');
   ReplaceAttribute('maskcontentunits', 'maskContentUnits');
   ReplaceAttribute('maskunits', 'maskUnits');
   ReplaceAttribute('numoctaves', 'numOctaves');
   ReplaceAttribute('pathlength', 'pathLength');
   ReplaceAttribute('patterncontentunits', 'patternContentUnits');
   ReplaceAttribute('patterntransform', 'patternTransform');
   ReplaceAttribute('patternunits', 'patternUnits');
   ReplaceAttribute('pointsatx', 'pointsAtX');
   ReplaceAttribute('pointsaty', 'pointsAtY');
   ReplaceAttribute('pointsatz', 'pointsAtZ');
   ReplaceAttribute('preservealpha', 'preserveAlpha');
   ReplaceAttribute('preserveaspectratio', 'preserveAspectRatio');
   ReplaceAttribute('primitiveunits', 'primitiveUnits');
   ReplaceAttribute('refx', 'refX');
   ReplaceAttribute('refy', 'refY');
   ReplaceAttribute('repeatcount', 'repeatCount');
   ReplaceAttribute('repeatdur', 'repeatDur');
   ReplaceAttribute('requiredextensions', 'requiredExtensions');
   ReplaceAttribute('requiredfeatures', 'requiredFeatures');
   ReplaceAttribute('specularconstant', 'specularConstant');
   ReplaceAttribute('specularexponent', 'specularExponent');
   ReplaceAttribute('spreadmethod', 'spreadMethod');
   ReplaceAttribute('startoffset', 'startOffset');
   ReplaceAttribute('stddeviation', 'stdDeviation');
   ReplaceAttribute('stitchtiles', 'stitchTiles');
   ReplaceAttribute('surfacescale', 'surfaceScale');
   ReplaceAttribute('systemlanguage', 'systemLanguage');
   ReplaceAttribute('tablevalues', 'tableValues');
   ReplaceAttribute('targetx', 'targetX');
   ReplaceAttribute('targety', 'targetY');
   ReplaceAttribute('textlength', 'textLength');
   ReplaceAttribute('viewbox', 'viewBox');
   ReplaceAttribute('viewtarget', 'viewTarget');
   ReplaceAttribute('xchannelselector', 'xChannelSelector');
   ReplaceAttribute('ychannelselector', 'yChannelSelector');
   ReplaceAttribute('zoomandpan', 'zoomAndPan');
end;

procedure THTMLParser.TToken.AdjustForeignAttributes();
begin
   if (not Assigned(FAttributes)) then exit;
   // since we don't support attribute namespaces natively, instead we use
   // the form '<namespaceid> <attributelocalname>' and don't use prefixes
   ReplaceAttribute('xlink:actuate', 'xlink actuate');
   ReplaceAttribute('xlink:arcrole', 'xlink arcrole');
   ReplaceAttribute('xlink:href', 'xlink href');
   ReplaceAttribute('xlink:role', 'xlink role');
   ReplaceAttribute('xlink:show', 'xlink show');
   ReplaceAttribute('xlink:title', 'xlink title');
   ReplaceAttribute('xlink:type', 'xlink type');
   ReplaceAttribute('xml:base', 'xml base');
   ReplaceAttribute('xml:lang', 'xml lang');
   ReplaceAttribute('xml:space', 'xml space');
   ReplaceAttribute('xmlns', 'xmlns xmlns');
   ReplaceAttribute('xmlns:xlink', 'xmlns xlink');
end;

procedure THTMLParser.TToken.AppendSourceNonSpaceCharacter(constref NewPointer: TParserStringPointer);
begin
   Assert(Kind = tkNone);
   {$IFOPT C+} Assert((not NewPointer.IsZeroWidth()) or (NewPointer.IsEOF())); {$ENDIF}
   {$IFNDEF USEROPES} {$IFOPT C+} Assert(NewPointer.IsEOF() or not IsSpaceCharacter(NewPointer.GetByte())); {$ENDIF} {$ENDIF}
   if (not HavePendingCharacters) then
   begin
      {$IFOPT C+} Assert(not NewPointer.IsEOF()); {$ENDIF}
      Assert(SpaceState = []);
      HavePendingCharacters := True;
      FCharStart := NewPointer;
   end;
   Include(SpaceState, ssHaveNonSpace);
   FCharEnd := NewPointer;
end;

procedure THTMLParser.TToken.AppendSourceSpaceCharacter(constref NewPointer: TParserStringPointer);
begin
   Assert(Kind = tkNone);
   {$IFOPT C+} Assert((not NewPointer.IsZeroWidth()) or (NewPointer.IsEOF())); {$ENDIF}
   {$IFNDEF USEROPES} {$IFOPT C+} Assert(NewPointer.IsEOF() or IsSpaceCharacter(NewPointer.GetByte())); {$ENDIF} {$ENDIF}
   if (not HavePendingCharacters) then
   begin
      {$IFOPT C+} Assert(not NewPointer.IsEOF()); {$ENDIF}
      Assert(SpaceState = []);
      HavePendingCharacters := True;
      Include(SpaceState, ssHaveLeadingSpace);
      FCharStart := NewPointer;
   end
   else
   begin
      Assert(SpaceState * [ssHaveLeadingSpace, ssHaveNonSpace] <> []);
      Include(SpaceState, ssMightHaveNonLeadingSpace);
   end;
   FCharEnd := NewPointer;
end;

procedure THTMLParser.TToken.AppendSourceNonSpaceBetweenBookmarks(constref StartPointer, EndPointer: TParserStringPointer);
begin
   if (not HavePendingCharacters) then
   begin
      Assert(SpaceState = []);
      FCharStart := StartPointer;
      HavePendingCharacters := True;
   end;
   {$IFOPT C+}
   // XXX should assert no spaces in this string, using IsSpaceCharacter(StartPointer.GetByte()) and an enumerator walking it
   {$ENDIF}
   Include(SpaceState, ssHaveNonSpace);
   FCharEnd := EndPointer;
end;

procedure THTMLParser.TToken.EmitSourceCharacters();
begin
   Assert(Kind = tkNone);
   if (HavePendingCharacters) then
   begin
      Assert(SpaceState * [ssHaveLeadingSpace, ssHaveNonSpace] <> []);
      Kind := tkSourceCharacters;
      Ready := True;
   end
   else
      Assert(SpaceState = []);
end;

procedure THTMLParser.TToken.SkipLeadingSpaces(constref Data: TParserString);
begin
   Assert(HavePendingCharacters);
   Assert(ssHaveLeadingSpace in SpaceState);
   Assert(ssHaveNonSpace in SpaceState);
   {$IFDEF USEROPES}
   repeat
      FCharStart.AdvanceToNext();
   until not IsSpaceCharacter(FCharStart.CurrentCharacter);
   {$ELSE}
   while (IsSpaceCharacter(FCharStart.AdvanceToNext(Data))) do ;
   {$ENDIF}
   Assert(FCharStart <= FCharEnd);
   Exclude(SpaceState, ssHaveLeadingSpace);
end;

function THTMLParser.TToken.SkipLeadingNonSpaces(constref Data: TParserString): Cardinal;
begin
   Assert(HavePendingCharacters);
   Assert(not (ssHaveLeadingSpace in SpaceState));
   Assert(ssHaveNonSpace in SpaceState);
   Assert(FCharStart <= FCharEnd);
   {$IFOPT C+} Assert((not FCharStart.IsZeroWidth()) or (FCharStart.IsEOF())); {$ENDIF}
   {$IFOPT C+} Assert((not FCharEnd.IsZeroWidth()) or (FCharEnd.IsEOF())); {$ENDIF}
   Result := 1;
   while (FCharStart < FCharEnd) do
   begin
      {$IFDEF USEROPES}
      FCharStart.AdvanceToNext();
      if (IsSpaceCharacter(FCharStart.CurrentCharacter)) then
      {$ELSE}
      if (IsSpaceCharacter(FCharStart.AdvanceToNext(Data))) then
      {$ENDIF}
      begin
         Include(SpaceState, ssHaveLeadingSpace);
         exit;
      end;
      Inc(Result);
   end;
   Assert(FCharStart = FCharEnd); // which would mean we're holding one character, a non-space, which we're now skipping
   HavePendingCharacters := False;
end;

function THTMLParser.TToken.SkipOneLeadingNewline(constref Data: TParserString): Boolean;
begin
   Assert(HavePendingCharacters);
   {$IFOPT C+} Assert((not FCharStart.IsZeroWidth()) or (FCharStart.IsEOF())); {$ENDIF}
   {$IFOPT C+} Assert((not FCharEnd.IsZeroWidth()) or (FCharEnd.IsEOF())); {$ENDIF}
   if (ssHaveLeadingSpace in SpaceState) then
   begin
      {$IFOPT C+} Assert(not FCharStart.IsEOF()); {$ENDIF}
      {$IFOPT C+} Assert(FCharStart <= FCharEnd); {$ENDIF}
      {$IFDEF USERPOPES}
      FCharStart.ReadCurrent();
      if (FCharStart.CurrentCharacter = $000A) then
      {$ELSE}
      if (Data.Extract(FCharStart, FCharStart).AsString = Chr($0A)) then
      {$ENDIF}
      begin
         {$IFDEF USEROPES}
         FCharStart.AdvanceToNext();
         if (not IsSpaceCharacter(FCharStart.CurrentCharacter)) then
         {$ELSE}
         if (not IsSpaceCharacter(FCharStart.AdvanceToNext(Data))) then
         {$ENDIF}
            Exclude(SpaceState, ssHaveLeadingSpace);
         HavePendingCharacters := FCharStart <= FCharEnd;
      end;
   end;
   Result := HavePendingCharacters;
end;

function THTMLParser.TToken.ExtractLeadingSpaces(constref Data: TParserString): TCutParserString;
var
   OriginalFCharStart, LeadingSpacesEnd: TParserStringPointer;
begin
   Assert(HavePendingCharacters);
   Assert(ssHaveLeadingSpace in SpaceState);
   Assert(ssHaveNonSpace in SpaceState);
   OriginalFCharStart := FCharStart;
   {$IFDEF USEROPES}
   repeat
      FCharStart.AdvanceToNext();
   until not IsSpaceCharacter(FCharStart.CurrentCharacter);
   {$ELSE}
   while (IsSpaceCharacter(FCharStart.AdvanceToNext(Data))) do ;
   {$ENDIF}
   Assert(FCharStart <= FCharEnd);
   Assert(OriginalFCharStart < FCharStart);
   LeadingSpacesEnd := FCharStart;
   LeadingSpacesEnd.SetToZeroWidth();
   Result := Data.Extract(OriginalFCharStart, LeadingSpacesEnd);
   Exclude(SpaceState, ssHaveLeadingSpace);
end;

function THTMLParser.TToken.ExtractSourceCharacters(constref Data: TParserString): TCutParserString;
begin
   Assert(HavePendingCharacters);
   Assert(FCharStart <= FCharEnd);
   Result := Data.Extract(FCharStart, FCharEnd);
end;

function THTMLParser.TToken.GetCharacterCount(constref Data: TParserString): Cardinal;
begin
   Assert(HavePendingCharacters);
   Assert(FCharStart <= FCharEnd);
   Result := Data.CountCharacters(FCharStart, FCharEnd);
end;

procedure THTMLParser.TToken.EmitExtraSpaceCharacter(const Character: TUnicodeCodepoint);
begin
   Assert(Kind = tkNone);
   {$IFOPT C+} Assert(IsSpaceCharacter(Character)); {$ENDIF}
   Kind := tkExtraSpaceCharacter;
   SetLength(ExtraChars, 1);
   ExtraChars[0] := Character;
   Assert(Kind <> tkNone);
   Ready := True;
end;

procedure THTMLParser.TToken.EmitExtraNonSpaceCharacter(const Character: TUnicodeCodepoint);
begin
   Assert(Kind = tkNone);
   {$IFOPT C+} Assert(not IsSpaceCharacter(Character)); {$ENDIF}
   Kind := tkExtraCharacters;
   SetLength(ExtraChars, 1);
   ExtraChars[0] := Character;
   Assert(Kind <> tkNone);
   Ready := True;
end;

procedure THTMLParser.TToken.EmitExtraAmbiguousCharacters(const Characters: TUnicodeCodepointArray);
begin
   Assert(Kind = tkNone);
   ExtraChars := Characters;
   if (Length(Characters) = 1) then
   begin
      case (Characters[0].Value) of
         $0009, $000A, $000C, $000D, $0020: Kind := tkExtraSpaceCharacter;
         else Kind := tkExtraCharacters;
      end;
   end
   else
   begin
      Assert(Length(Characters) = 2);
      {$IFOPT C+}
         Assert(not IsSpaceCharacter(Characters[0]));
         Assert(not IsSpaceCharacter(Characters[1]));
      {$ENDIF}
      Kind := tkExtraCharacters;
   end;
   Assert(Kind <> tkNone);
   Ready := True;
end;

procedure THTMLParser.TToken.EmitNullCharacter();
begin
   Assert(Kind = tkNone);
   Kind := tkNullCharacter;
   Ready := True;
end;

procedure THTMLParser.TToken.PrepareComment();
begin
   Assert(Kind = tkNone);
   Kind := tkComment;
   CommentValue := Default(TParserString);
end;

procedure THTMLParser.TToken.EmitEOF();
var
   S: UTF8String;
begin
   Str(Kind, S);
   Assert(Kind = tkNone, S);
   Kind := tkEOF;
   Assert(Kind <> tkNone);
   Ready := True;
end;


constructor THTMLParser.Create();
begin
   inherited;
   FInsertionMode := @TheInitialInsertionMode;
   FInputStream := TInputStream.Create();
   {$IFDEF PARSEERROR} FCurrentToken.Init(@ParseError); {$ENDIF}
   FFramesetOkFlag := True;
end;

destructor THTMLParser.Destroy();
begin
   //{$IFOPT C+} Assert(FInputStream.WasStarted); {$ENDIF}
   FInputStream.Free();
   FCurrentToken.Destroy();
   FDocument.Free(); // in case the parser failed to return somehow
   inherited;
end;

{$IFNDEF USEROPES}
procedure THTMLParser.SpoonFeed(const Data: UTF8String);
begin
   Assert(Assigned(FInputStream));
   {$IFOPT C+} Assert(not FInputStream.WasStarted); {$ENDIF}
   FInputStream.PushData(Data);
end;
{$ENDIF}

procedure THTMLParser.SpoonFeed(const Data: Pointer; const Length: QWord);
begin
   Assert(Assigned(FInputStream));
   {$IFOPT C+} Assert(not FInputStream.WasStarted); {$ENDIF}
   FInputStream.PushData(Data, Length);
end;

procedure THTMLParser.RegisterProperietaryVoidElements(const TagNames: array of TCanonicalString);
var
   Name: TCanonicalString;
begin
   {$IFOPT C+} Assert(not FInputStream.WasStarted); {$ENDIF}
   for Name in TagNames do
      FProprietaryVoids.Push(Name);
end;

function THTMLParser.Parse(): TDocument;
var
   OldKind: TTokenKind;
begin
   {$IFOPT C+} Assert(not FInputStream.WasStarted); {$ENDIF}
   FInputStream.Freeze();
   Assert(FInsertionMode = @TheInitialInsertionMode);
   FDocument := TDocument.Create();
   repeat
      Tokenise(); // updates FCurrentToken
      Assert(FCurrentToken.Kind <> tkNone);
      Assert(Assigned(FInsertionMode));
      if (FCurrentToken.HavePendingCharacters) then
      begin
         OldKind := FCurrentToken.Kind;
         FCurrentToken.Kind := tkSourceCharacters;
         TreeConstructionDispatcher(FCurrentToken);
         FCurrentToken.Kind := OldKind;
      end
      else
         Assert(FCurrentToken.Kind <> tkSourceCharacters);
      Assert(FCurrentToken.Kind <> tkNone);
      if (FCurrentToken.Kind <> tkSourceCharacters) then
      begin
         Assert(Assigned(FInsertionMode));
         TreeConstructionDispatcher(FCurrentToken);
         if (FCurrentToken.Kind = tkStartTag) then
         begin
            FLastStartTag := FCurrentToken.TagName.AsString;
            {$IFDEF PARSEERROR}
               if (FCurrentToken.SelfClosingFlag and not FCurrentToken.SelfClosingAcknowledged) then
                  ParseError('Self-closing tag syntax used inappropriately');
            {$ENDIF}
         end;
         if (FCurrentToken.Kind = tkEOF) then
            break;
      end;
      FCurrentToken.Reset();
   until forever;
   {$IFOPT C+} Assert(FInputStream.WasStarted); {$ENDIF}
   Result := FDocument;
   FDocument := nil;
end;

{$IFDEF PARSEERROR}
procedure THTMLParser.ParseError(Message: UTF8String; const Count: Cardinal = 1);
var
   Index: Cardinal;
begin
   Assert(Count >= 1);
   {$IFDEF LINES}
   Message := '(' + IntToStr(FInputStream.Line) + ',' + IntToStr(FInputStream.Column) + ') ' + Message;
   {$ENDIF}
   if (Assigned(FOnParseError)) then
   begin
      if (Count > 1) then
      begin
         for Index := 1 to Count do
            FOnParseError(Message + ' (' + IntToStr(Index) + ' of ' + IntToStr(Count) + ')');
      end
      else
      begin
         FOnParseError(Message);
      end;
   end
   else
      raise ESyntaxError.Create(Message);
end;
{$ENDIF}

procedure THTMLParser.TreeConstructionDispatcher(var Token: TToken);
var
   CachedAdjustedCurrentNode: TElement;
begin
   if (FStackOfOpenElements.Length = 0) then
   begin
      FInsertionMode(Token); // http://bugs.freepascal.org/view.php?id=26403
   end
   else
   begin
      CachedAdjustedCurrentNode := AdjustedCurrentNode;
      Assert(Assigned(CachedAdjustedCurrentNode));
      if ((CachedAdjustedCurrentNode.NamespaceURL = nsHTML) or
          (CachedAdjustedCurrentNode.HasProperties(propMathMLTextIntegrationPoint) and
           (((Token.Kind = tkStartTag) and (Token.TagName <> eMGlyph) and (Token.TagName <> eMAlignMark)) or
            (Token.Kind in [tkSourceCharacters, tkExtraCharacters, tkExtraSpaceCharacter, tkNullCharacter]))) or
          (CachedAdjustedCurrentNode.IsIdentity(nsMathML, eAnnotationXML) and (Token.Kind = tkStartTag) and (Token.TagName = eSVG)) or
          (CachedAdjustedCurrentNode.HasProperties(propHTMLIntegrationPoint) and
           (Token.Kind in [tkStartTag, tkSourceCharacters, tkExtraCharacters, tkExtraSpaceCharacter, tkNullCharacter])) or
          (Token.Kind = tkEOF)) then
         FInsertionMode(Token)
      else
         TheRulesForParsingTokensInForeignContent(Token);
   end;
end;

function THTMLParser.StackOfOpenElementsHas(constref Namespace, LocalName: TCanonicalString): Boolean;
var
   StackIndex: Cardinal;
begin
   StackIndex := FStackOfOpenElements.Length;
   while (StackIndex > 0) do
   begin
      Dec(StackIndex); // $R-
      if (FStackOfOpenElements[StackIndex].IsIdentity(Namespace, LocalName)) then
      begin
         Result := True;
         exit;
      end;
   end;
   Result := False;
end;

function THTMLParser.StackOfOpenElementsHasElementOtherThan(const TargetProperty: TElementProperties): Boolean;
var
   StackIndex: Cardinal;
begin
   Assert(TargetProperty <> 0);
   StackIndex := FStackOfOpenElements.Length;
   while (StackIndex > 0) do
   begin
      Dec(StackIndex); // $R-
      if (not FStackOfOpenElements[StackIndex].HasProperties(TargetProperty)) then
      begin
         Result := True;
         exit;
      end;
   end;
   Result := False;
end;

function THTMLParser.StackOfOpenElementsHasInSpecificScope(constref Target: TCanonicalString; const ParticularScope: TElementProperties): Boolean; // nsHTML assumed for the Target node
var
   Node: TElement;
   Index: Cardinal;
begin
   Assert(FStackOfOpenElements.Length > 0);
   Index := FStackOfOpenElements.Length-1; // $R-
   Node := FStackOfOpenElements[Index];
   Assert(Assigned(Node));
   while (not Node.IsIdentity(nsHTML, Target)) do
   begin
      if (Node.HasProperties(ParticularScope)) then
      begin
         Result := False;
         exit;
      end;
      Assert(Node.ParentNode is TElement);
      Dec(Index);
      Node := FStackOfOpenElements[Index];
      Assert(Assigned(Node)); // if this fails, check that eHTML has ParticularScope set (and maybe figure out how to assert it)
   end;
   Result := True;
end;

function THTMLParser.StackOfOpenElementsHasInSpecificScope(const Target: TElementProperties; const ParticularScope: TElementProperties): Boolean;
var
   Node: TElement;
   Index: Cardinal;
begin
   Assert(FStackOfOpenElements.Length > 0);
   Index := FStackOfOpenElements.Length-1; // $R-
   Node := FStackOfOpenElements[Index];
   Assert(Assigned(Node));
   while (not Node.HasProperties(Target)) do
   begin
      if (Node.HasProperties(ParticularScope)) then
      begin
         Result := False;
         exit;
      end;
      Assert(Node.ParentNode is TElement);
      Dec(Index);
      Node := FStackOfOpenElements[Index];
      Assert(Assigned(Node)); // if this fails, check that eHTML has ParticularScope set (and maybe figure out how to assert it)
   end;
   Result := True;
end;

function THTMLParser.StackOfOpenElementsHasInSpecificScope(const Target: TElement; const ParticularScope: TElementProperties): Boolean;
var
   Node: TElement;
   Index: Cardinal;
begin
   Assert(Assigned(Target));
   Assert(FStackOfOpenElements.Length > 0);
   Index := FStackOfOpenElements.Length-1; // $R-
   Node := FStackOfOpenElements[Index];
   Assert(Assigned(Node));
   while (Node <> Target) do
   begin
      if (Node.HasProperties(ParticularScope)) then
      begin
         Result := False;
         exit;
      end;
      Assert(Node.ParentNode is TElement);
      Dec(Index);
      Node := FStackOfOpenElements[Index];
      Assert(Assigned(Node)); // if this fails, check that eHTML has ParticularScope set (and maybe figure out how to assert it)
   end;
   Result := True;
end;

function THTMLParser.StackOfOpenElementsHasInScope(constref Target: TCanonicalString): Boolean;
begin
   Result := StackOfOpenElementsHasInSpecificScope(Target, propGenericScope);
end;

function THTMLParser.StackOfOpenElementsHasInScope(const Target: TElementProperties): Boolean;
begin
   Result := StackOfOpenElementsHasInSpecificScope(Target, propGenericScope);
end;

function THTMLParser.StackOfOpenElementsHasInScope(const Target: TElement): Boolean;
begin
   Result := StackOfOpenElementsHasInSpecificScope(Target, propGenericScope);
end;

function THTMLParser.StackOfOpenElementsHasInListItemScope(constref Target: TCanonicalString): Boolean;
begin
   Result := StackOfOpenElementsHasInSpecificScope(Target, propListItemScope);
end;

function THTMLParser.StackOfOpenElementsHasInButtonScope(constref Target: TCanonicalString): Boolean;
begin
   Result := StackOfOpenElementsHasInSpecificScope(Target, propButtonScope);
end;

function THTMLParser.StackOfOpenElementsHasInTableScope(constref Target: TCanonicalString): Boolean;
begin
   Result := StackOfOpenElementsHasInSpecificScope(Target, propTableScope);
end;

function THTMLParser.StackOfOpenElementsHasInTableScope(const Target: TElementProperties): Boolean;
begin
   Result := StackOfOpenElementsHasInSpecificScope(Target, propTableScope);
end;

function THTMLParser.StackOfOpenElementsHasInSelectScope(constref Target: TCanonicalString): Boolean;
var
   Node: TElement;
begin
   Node := CurrentNode;
   Assert(Assigned(Node));
   while (not Node.IsIdentity(nsHTML, Target)) do
   begin
      if (not Node.HasProperties(propSelectScope)) then
      begin
         Result := False;
         exit;
      end;
      Assert(Node.ParentNode is TElement);
      Node := Node.ParentNode as TElement;
   end;
   Result := True;
end;

procedure THTMLParser.InsertMarkerAtEndOfListOfActiveFormattingElements();
begin
   FListOfActiveFormattingElements.Push(Marker);
   Assert(FListOfActiveFormattingElements.Length > 0);
end;

procedure THTMLParser.PushOntoTheListOfActiveFormattingElements(const Target: TElement);

   function AttributesMatch(const A, B: TElement.TAttributeHashTable): Boolean;
   var
      Name: UTF8String;
   begin
      if (Assigned(A) and Assigned(B) and (A.Count = B.Count)) then
      begin
         for Name in A do
            if ((not B.Has(Name)) or (B[Name] <> A[Name])) then
            begin
               Result := False;
               exit;
            end;
         Result := True;
      end
      else
         Result := ((not Assigned(A)) and (not Assigned(B)));
   end;

var
   StartIndex, Index: Cardinal;
   FirstMatchIndex, Count: Cardinal;
begin
   StartIndex := FListOfActiveFormattingElements.Length;
   if (StartIndex > 0) then
   begin
      // Noah's Ark Clause
      // find last marker or start of list
      repeat
         Dec(StartIndex);
         if (FListOfActiveFormattingElements[StartIndex] = Marker) then
         begin
            Inc(StartIndex);
            break;
         end;
      until StartIndex = 0;
      // now walk from that marker to the end of the list, checking for matches
      Count := 0;
      Index := StartIndex;
      while ((Index < FListOfActiveFormattingElements.Length) and (Count < 3)) do
      begin
         if ((FListOfActiveFormattingElements[Index].LocalName = Target.LocalName) and
             (FListOfActiveFormattingElements[Index].NamespaceURL = Target.NamespaceURL) and
             (AttributesMatch(FListOfActiveFormattingElements[Index].Attributes, Target.Attributes))) then
         begin
            if (Count = 0) then
               FirstMatchIndex := Index;
            Inc(Count);
         end;
         Inc(Index);
      end;
      if (Count >= 3) then
         FListOfActiveFormattingElements.RemoveAt(FirstMatchIndex); // $DFA- for FirstMatchIndex
   end;
   FListOfActiveFormattingElements.Push(Target);
end;

procedure THTMLParser.ReconstructTheActiveFormattingElements();
var
   Index: Cardinal;
begin
   if (FListOfActiveFormattingElements.Length = 0) then
      exit;
   if ((FListOfActiveFormattingElements.Last = Marker) or (FStackOfOpenElements.Contains(FListOfActiveFormattingElements.Last))) then
      exit;
   Index := FListOfActiveFormattingElements.Length-1; // $R-
   while (Index > 0) do
   begin
      Dec(Index);
      if ((FListOfActiveFormattingElements[Index] = Marker) or (FStackOfOpenElements.Contains(FListOfActiveFormattingElements[Index]))) then
      begin
         Inc(Index);
         break;
      end;
   end;
   repeat
      FListOfActiveFormattingElements[Index] := InsertAnHTMLElement(FListOfActiveFormattingElements[Index].CloneNode());
      Inc(Index);
   until Index >= FListOfActiveFormattingElements.Length;
end;

procedure THTMLParser.ClearTheListOfActiveFormattingElementsUpToTheLastMarker();
begin
   Assert(FListOfActiveFormattingElements.Length >= 1);
   repeat until FListOfActiveFormattingElements.Pop() = Marker;
end;

procedure THTMLParser.Tokenise();
const
   kNoAdditionalAllowedCharacter = kEOF;
type
   TCharacterReferenceMode = (crInText, crInAttribute);

   function ConsumeACharacterReference(out Characters: TUnicodeCodepointArray;
                                       out Reconsume: Boolean;
                                       const Mode: TCharacterReferenceMode;
                                       const AdditionalAllowedCharacter: TUnicodeCodepoint): Boolean;
   var
      StartOfCharacterReferenceBookmark: THTMLParser.TInputStream.TBookmark;

      procedure ParseNumericCharacterReference(); inline;
      var
         ShowGenericISO88591Error: Boolean;
         GivenValue, DigitCount: Cardinal;
      begin
         FInputStream.Advance();
         GivenValue := 0;
         DigitCount := 0;
         case (FInputStream.CurrentCharacter.Value) of
            $0078, $0058: // &#x...
               begin
                  // hex digits
                  repeat
                     FInputStream.Advance();
                     {$PUSH}
                     {$OVERFLOWCHECKS ON}
                     {$RANGECHECKS ON}
                     try
                        case (FInputStream.CurrentCharacter.Value) of
                           Ord('0'): GivenValue := GivenValue * 16 + 0; // $R-
                           Ord('1'): GivenValue := GivenValue * 16 + 1; // $R-
                           Ord('2'): GivenValue := GivenValue * 16 + 2; // $R-
                           Ord('3'): GivenValue := GivenValue * 16 + 3; // $R-
                           Ord('4'): GivenValue := GivenValue * 16 + 4; // $R-
                           Ord('5'): GivenValue := GivenValue * 16 + 5; // $R-
                           Ord('6'): GivenValue := GivenValue * 16 + 6; // $R-
                           Ord('7'): GivenValue := GivenValue * 16 + 7; // $R-
                           Ord('8'): GivenValue := GivenValue * 16 + 8; // $R-
                           Ord('9'): GivenValue := GivenValue * 16 + 9; // $R-
                           Ord('A'), Ord('a'): GivenValue := GivenValue * 16 + 10; // $R-
                           Ord('B'), Ord('b'): GivenValue := GivenValue * 16 + 11; // $R-
                           Ord('C'), Ord('c'): GivenValue := GivenValue * 16 + 12; // $R-
                           Ord('D'), Ord('d'): GivenValue := GivenValue * 16 + 13; // $R-
                           Ord('E'), Ord('e'): GivenValue := GivenValue * 16 + 14; // $R-
                           Ord('F'), Ord('f'): GivenValue := GivenValue * 16 + 15; // $R-
                           else
                              break;
                        end;
                        Inc(DigitCount);
                     except
                        on ERangeError do
                           GivenValue := High(GivenValue);
                     end;
                     {$POP}
                  until forever;
               end;
            else
               begin
                  // decimal digits
                  repeat
                     {$PUSH}
                     {$OVERFLOWCHECKS ON}
                     {$RANGECHECKS ON}
                     try
                        case (FInputStream.CurrentCharacter.Value) of
                           Ord('0'): GivenValue := GivenValue * 10 + 0; // $R-
                           Ord('1'): GivenValue := GivenValue * 10 + 1; // $R-
                           Ord('2'): GivenValue := GivenValue * 10 + 2; // $R-
                           Ord('3'): GivenValue := GivenValue * 10 + 3; // $R-
                           Ord('4'): GivenValue := GivenValue * 10 + 4; // $R-
                           Ord('5'): GivenValue := GivenValue * 10 + 5; // $R-
                           Ord('6'): GivenValue := GivenValue * 10 + 6; // $R-
                           Ord('7'): GivenValue := GivenValue * 10 + 7; // $R-
                           Ord('8'): GivenValue := GivenValue * 10 + 8; // $R-
                           Ord('9'): GivenValue := GivenValue * 10 + 9; // $R-
                           else
                              break;
                        end;
                        Inc(DigitCount);
                     except
                        on ERangeError do
                           GivenValue := High(GivenValue);
                     end;
                     {$POP}
                     FInputStream.Advance();
                  until forever;
               end;
         end;
         if (DigitCount = 0) then
         begin
            {$IFDEF PARSEERROR} ParseError('"&#" used without digits'); {$ENDIF}
            FInputStream.ReturnToBookmark(StartOfCharacterReferenceBookmark);
            SetLength(Characters, 0);
            Result := False;
            exit;
         end;
         if (FInputStream.CurrentCharacter = $003B) then // semicolon
         begin
            FInputStream.Advance();
         end
         else
         begin
            {$IFDEF PARSEERROR} ParseError('numeric character reference missing trailing semicolon'); {$ENDIF}
         end;
         Reconsume := True;
         SetLength(Characters, 1);
         ShowGenericISO88591Error := True;
         case (GivenValue) of
            $00:
               begin
                  Characters[0] := $FFFD;
                  ShowGenericISO88591Error := False;
                  {$IFDEF PARSEERROR} ParseError('numeric character reference to U+0000'); {$ENDIF}
               end;
            $80: Characters[0] := $20AC;
            // 81 below
            $82: Characters[0] := $201A;
            $83: Characters[0] := $0192;
            $84: Characters[0] := $201E;
            $85: Characters[0] := $2026;
            $86: Characters[0] := $2020;
            $87: Characters[0] := $2021;
            $88: Characters[0] := $02C6;
            $89: Characters[0] := $2030;
            $8A: Characters[0] := $0160;
            $8B: Characters[0] := $2039;
            $8C: Characters[0] := $0152;
            // 8D below
            $8E: Characters[0] := $017D;
            // 8F, 90 below
            $91: Characters[0] := $2018;
            $92: Characters[0] := $2019;
            $93: Characters[0] := $201C;
            $94: Characters[0] := $201D;
            $95: Characters[0] := $2022;
            $96: Characters[0] := $2013;
            $97: Characters[0] := $2014;
            $98: Characters[0] := $02DC;
            $99: Characters[0] := $2122;
            $9A: Characters[0] := $0161;
            $9B: Characters[0] := $203A;
            $9C: Characters[0] := $0153;
            // 9D below
            $9E: Characters[0] := $017E;
            $9F: Characters[0] := $0178;
            $D800..$DFFF:
               begin
                  Characters[0] := $FFFD;
                  ShowGenericISO88591Error := False;
                  {$IFDEF PARSEERROR} ParseError('numeric character reference to surrogate'); {$ENDIF}
               end;
            $0001..$0008, $000D..$001F,
            $7F, $81, $8D, $8F, $90, $9D, // this line is $007F..$009F
            $FDD0..$FDEF, $000B, $FFFE, $FFFF, $1FFFE, $1FFFF,
            $2FFFE, $2FFFF, $3FFFE, $3FFFF, $4FFFE, $4FFFF,
            $5FFFE, $5FFFF, $6FFFE, $6FFFF, $7FFFE, $7FFFF,
            $8FFFE, $8FFFF, $9FFFE, $9FFFF, $AFFFE, $AFFFF,
            $BFFFE, $BFFFF, $CFFFE, $CFFFF, $DFFFE, $DFFFF,
            $EFFFE, $EFFFF, $FFFFE, $FFFFF, $10FFFE, $10FFFF:
               begin
                  Characters[0] := GivenValue; // $R-
                  ShowGenericISO88591Error := False;
                  {$IFDEF PARSEERROR} ParseError('numeric character reference to non-Unicode character'); {$ENDIF}
               end;
            else
               if (GivenValue > $10FFFF) then
               begin
                  Characters[0] := $FFFD;
                  ShowGenericISO88591Error := False;
                  {$IFDEF PARSEERROR} ParseError('numeric character reference to value above U+10FFFF'); {$ENDIF}
               end
               else
               begin
                  Assert(GivenValue >= Low(Characters[0].Value));
                  Assert(GivenValue <= High(Characters[0].Value));
                  Characters[0] := GivenValue; // $R-
                  ShowGenericISO88591Error := False;
               end;
         end;
         if (ShowGenericISO88591Error) then
         begin
            {$IFDEF PARSEERROR} ParseError('numeric character reference to control character interpreted as ISO-8859-1 code point'); {$ENDIF}
         end;
      end;

      procedure ParseNamedCharacterReference(); inline;
      var
         EndOfLastValidReferenceBookmark: THTMLParser.TInputStream.TBookmark;
         {$IFDEF PARSEERROR} DidSeeAtLeastOneAlphanumericCharacter: Boolean; {$ENDIF}
         {$IFOPT C+} DidBookmark: Boolean; {$ENDIF}
         NamedCharacterReferenceParser: TNamedCharacterReferenceParser;
      begin
         NamedCharacterReferenceParser.Init();
         {$IFOPT C+} DidBookmark := False; {$ENDIF}
         while (NamedCharacterReferenceParser.Step(FInputStream.CurrentCharacter) in [ncOngoing, ncOngoingButBookmark]) do
         begin
            if (NamedCharacterReferenceParser.LastStepReturnValue = ncOngoingButBookmark) then
            begin
               {$IFOPT C+} DidBookmark := True; {$ENDIF}
               EndOfLastValidReferenceBookmark := FInputStream.GetBookmark();
            end;
            FInputStream.Advance();
         end;
         case (NamedCharacterReferenceParser.LastStepReturnValue) of
            ncFinishedGood:
               begin
                  Characters := NamedCharacterReferenceParser.FinalParsedValue;
               end;
            ncFinishedMissingSemicolonNeedBacktrack:
               begin
                  {$IFOPT C+} Assert(DidBookmark); {$ENDIF}
                  if (Mode = crInAttribute) then
                  begin
                     FInputStream.ReturnToBookmark(EndOfLastValidReferenceBookmark); {BOGUS Warning: Local variable "EndOfLastValidReferenceBookmark" does not seem to be initialized} // i hope it's bogus, anyway
                     FInputStream.Advance();
                     case (FInputStream.CurrentCharacter.Value) of
                        $003D, Ord('0')..Ord('9'), Ord('A')..Ord('Z'), Ord('a')..Ord('z'):
                           begin
                              {$IFDEF PARSEERROR}
                                 if (FInputStream.CurrentCharacter = $003D) then
                                    ParseError('named character reference in attribute value terminated by equals sign');
                              {$ENDIF}
                              FInputStream.ReturnToBookmark(StartOfCharacterReferenceBookmark);
                              SetLength(Characters, 0);
                              exit;
                           end;
                     end;
                  end;
                  FInputStream.ReturnToBookmark(EndOfLastValidReferenceBookmark); {BOGUS Warning: Local variable "EndOfLastValidReferenceBookmark" does not seem to be initialized} // i hope it's bogus, anyway
                  {$IFDEF PARSEERROR}
                     if (FInputStream.CurrentCharacter <> kEOF) then
                        ParseError('missing semicolon at end of named character reference (found "' + CodepointToUTF8(FInputStream.CurrentCharacter) + '" instead)')
                     else
                        ParseError('unexpected EOF while parsing named character reference');
                  {$ENDIF}
                  Characters := NamedCharacterReferenceParser.FinalParsedValue;
               end;
            ncFinishedNone:
               begin
                  FInputStream.ReturnToBookmark(StartOfCharacterReferenceBookmark);
                  Assert(FInputStream.CurrentCharacter = Ord('&'), IntToStr(FInputStream.CurrentCharacter.Value));
                  SetLength(Characters, 0);
                  {$IFDEF PARSEERROR}
                     DidSeeAtLeastOneAlphanumericCharacter := False;
                     FInputStream.Advance();
                     repeat
                        case (FInputStream.CurrentCharacter.Value) of
                           Ord('0')..Ord('9'),
                           Ord('A')..Ord('Z'),
                           Ord('a')..Ord('z'):
                              begin
                                 DidSeeAtLeastOneAlphanumericCharacter := True;
                                 FInputStream.Advance();
                              end;
                           Ord(';'):
                              begin
                                 if (DidSeeAtLeastOneAlphanumericCharacter) then
                                    ParseError('not a valid named character reference');
                                 break;
                              end;
                           else break;
                        end;
                     until forever;
                     FInputStream.ReturnToBookmark(StartOfCharacterReferenceBookmark);
                     Assert(FInputStream.CurrentCharacter = Ord('&'));
                  {$ENDIF}
               end;
            else Assert(False);
         end;
         // beware when adding code here: there's an "exit" above.
      end;

   var
      CurrentCharacter: TUnicodeCodepoint;
   begin
      Reconsume := False;
      Assert(Length(Characters) = 0); {BOGUS Warning: Variable "Characters" does not seem to be initialized}
      // if we ever make this into a non-blocking streaming parser, this needs to be turned into tokeniser stages
      StartOfCharacterReferenceBookmark := FInputStream.GetBookmark();
      FInputStream.Advance();
      CurrentCharacter := FInputStream.CurrentCharacter;
      if (CurrentCharacter = AdditionalAllowedCharacter) then
         CurrentCharacter := kEOF; // so we fall through to the first entry in the case below
      case (CurrentCharacter.Value) of
         $0009, $000A, $000C, $0020, $003C, $0026, kEOF:
            begin
               FInputStream.ReturnToBookmark(StartOfCharacterReferenceBookmark);
               SetLength(Characters, 0);
            end;
         $0023: ParseNumericCharacterReference();
         else ParseNamedCharacterReference();
      end;
      Result := Length(Characters) > 0;
   end;

   procedure AppendInputNonSpaceCharacter(); inline;
   begin
      Assert(FInputStream.CurrentCharacter <> $0000);
      Assert(not FInputStream.WasManipulated);
      {$IFOPT C+} Assert(not IsSpaceCharacter(FInputStream.CurrentCharacter)); {$ENDIF}
      FCurrentToken.AppendSourceNonSpaceCharacter(FInputStream.GetPointer());
      {$IFOPT C+} FCurrentToken.FCharStart.AssertIdentity(@FInputStream.Data); {$ENDIF}
      {$IFOPT C+} FCurrentToken.FCharEnd.AssertIdentity(@FInputStream.Data); {$ENDIF}
   end;

   procedure AppendInputSpaceCharacter(); inline;
   begin
      {$IFOPT C+} Assert(IsSpaceCharacter(FInputStream.CurrentCharacter)); {$ENDIF}
      if (FInputStream.WasManipulated) then
      begin
         Assert(FInputStream.CurrentCharacter = $000A);
         FCurrentToken.EmitExtraSpaceCharacter(FInputStream.CurrentCharacter)
      end
      else
      begin
         FCurrentToken.AppendSourceSpaceCharacter(FInputStream.GetPointer());
         {$IFOPT C+} FCurrentToken.FCharStart.AssertIdentity(@FInputStream.Data); {$ENDIF}
         {$IFOPT C+} FCurrentToken.FCharEnd.AssertIdentity(@FInputStream.Data); {$ENDIF}
      end;
   end;

   procedure AppendBookmarkedCharacter(constref Bookmark: THTMLParser.TInputStream.TBookmark); inline;
   begin
      Assert(not Bookmark.WasManipulated);
      FCurrentToken.AppendSourceNonSpaceCharacter(Bookmark.GetPointer());
      {$IFOPT C+} FCurrentToken.FCharStart.AssertIdentity(@FInputStream.Data); {$ENDIF}
      {$IFOPT C+} FCurrentToken.FCharEnd.AssertIdentity(@FInputStream.Data); {$ENDIF}
   end;

   procedure AppendFromBookmarkedCharacterToInputCharacter(constref Bookmark: THTMLParser.TInputStream.TBookmark); inline;
   begin
      Assert(not Bookmark.WasManipulated);
      {$IFOPT C+} Assert(not Bookmark.GetPointer().IsEOF()); {$ENDIF}
      {$IFOPT C+} Assert(not Bookmark.GetPointer().IsZeroWidth()); {$ENDIF}
      Assert(FInputStream.CurrentCharacter <> kEOF);
      Assert(not FInputStream.WasManipulated);
      {$IFOPT C+} Assert(not FInputStream.GetPointer().IsZeroWidth()); {$ENDIF}
      FCurrentToken.AppendSourceNonSpaceBetweenBookmarks(Bookmark.GetPointer(), FInputStream.GetPointer());
      {$IFOPT C+} FCurrentToken.FCharStart.AssertIdentity(@FInputStream.Data); {$ENDIF}
      {$IFOPT C+} FCurrentToken.FCharEnd.AssertIdentity(@FInputStream.Data); {$ENDIF}
   end;

   procedure AppendFromBookmarkedCharacterToBeforeInputCharacter(constref Bookmark: THTMLParser.TInputStream.TBookmark); inline;
   var
      NewEndPointer: TParserStringPointer;
   begin
      Assert(not Bookmark.WasManipulated);
      {$IFOPT C+} Assert(not Bookmark.GetPointer().IsEOF()); {$ENDIF}
      {$IFOPT C+} Assert(not Bookmark.GetPointer().IsZeroWidth()); {$ENDIF}
      Assert((not FInputStream.WasManipulated) or (FInputStream.CurrentCharacter = kEOF));
      NewEndPointer := FInputStream.GetPointer();
      if (FInputStream.CurrentCharacter <> kEOF) then
         NewEndPointer.SetToZeroWidth();
      {$IFOPT C+} Assert(NewEndPointer.IsZeroWidth()); {$ENDIF}
      FCurrentToken.AppendSourceNonSpaceBetweenBookmarks(Bookmark.GetPointer(), NewEndPointer);
      {$IFOPT C+} FCurrentToken.FCharStart.AssertIdentity(@FInputStream.Data); {$ENDIF}
      {$IFOPT C+} FCurrentToken.FCharEnd.AssertIdentity(@FInputStream.Data); {$ENDIF}
   end;

   procedure MarkupDeclarationOpenState(); inline;
   var
      Bookmark: THTMLParser.TInputStream.TBookmark;

      procedure BogusComment(); inline;
      begin
         {$IFDEF PARSEERROR}
            {$IFOPT C+}
               ParseError('bogus comment (started with character ' + FInputStream.GetDebugData() + ')');
            {$ELSE}
               ParseError('bogus comment');
            {$ENDIF}
         {$ENDIF}
         FInputStream.ReturnToBookmark(Bookmark);
         FInputStream.Unconsume();
         FTokeniserState := tsBogusCommentState;
      end;

      procedure TryForRealComment(); inline;
      begin
         // seen -
         FInputStream.Advance();
         if (FInputStream.CurrentCharacter = $002D) then // -
         begin
            FCurrentToken.PrepareComment();
            FTokeniserState := tsCommentStartState;
         end
         else
         begin
            BogusComment();
         end;
      end;

      procedure TryForDOCTYPE(); inline;
      begin
         // seen Dd
         FInputStream.Advance();
         case (FInputStream.CurrentCharacter.Value) of
            $004F, $006F: // Oo
               begin
                  FInputStream.Advance();
                  case (FInputStream.CurrentCharacter.Value) of
                     $0043, $0063: // Cc
                        begin
                           FInputStream.Advance();
                           case (FInputStream.CurrentCharacter.Value) of
                              $0054, $0074: // Tt
                                 begin
                                    FInputStream.Advance();
                                    case (FInputStream.CurrentCharacter.Value) of
                                       $0059, $0079: // Yy
                                          begin
                                             FInputStream.Advance();
                                             case (FInputStream.CurrentCharacter.Value) of
                                                $0050, $0070: // Pp
                                                   begin
                                                      FInputStream.Advance();
                                                      case (FInputStream.CurrentCharacter.Value) of
                                                         $0045, $0065: // Ee
                                                            begin
                                                               FTokeniserState := tsDoctypeState;
                                                               exit;
                                                            end;
                                                      end;
                                                   end;
                                             end;
                                          end;
                                    end;
                                 end;
                           end;
                        end;
                  end;
               end;
         end;
         BogusComment();
      end;
      
      procedure TryForCDATASection(); inline;
      begin
         // seen [
         if ((FStackOfOpenElements.Length > 0) and (AdjustedCurrentNode.NamespaceURL <> nsHTML)) then
         begin
            FInputStream.Advance();
            case (FInputStream.CurrentCharacter.Value) of
               $0043, $0063: // Cc
                  begin
                     FInputStream.Advance();
                     case (FInputStream.CurrentCharacter.Value) of
                        $0044, $0064: // Dd
                           begin
                              FInputStream.Advance();
                              case (FInputStream.CurrentCharacter.Value) of
                                 $0041, $0061: // Aa
                                    begin
                                       FInputStream.Advance();
                                       case (FInputStream.CurrentCharacter.Value) of
                                          $0054, $0064: // Tt
                                             begin
                                                FInputStream.Advance();
                                                case (FInputStream.CurrentCharacter.Value) of
                                                   $0041, $0061: // Aa
                                                      begin
                                                         FInputStream.Advance();
                                                         case (FInputStream.CurrentCharacter.Value) of
                                                            $005B: // [
                                                               begin
                                                                  FTokeniserState := tsCdataSectionState;
                                                                  FCurrentToken.EmitSourceCharacters();
                                                                  exit;
                                                               end;
                                                         end;
                                                      end;
                                                end;
                                             end;
                                       end;
                                    end;
                              end;
                           end;
                     end;
                  end;
            end;
         end;
         BogusComment();
      end;

   begin
      Bookmark := FInputStream.GetBookmark();
      case (FInputStream.CurrentCharacter.Value) of
         $002D:
            TryForRealComment();
         $0044, $0064:
            TryForDOCTYPE();
         $005B:
            TryForCDATASection();
         else
            BogusComment();
      end;
   end;

   {$IFDEF USEROPES}
   procedure BogusCommentState(); inline;
   var
      Started: Boolean;
      StartOfRun, EndOfRun: TParserStringPointer;

      procedure Flush(); inline;
      var
         TemporaryCutRope: CutRope;
      begin
         if (Started) then
         begin
            TemporaryCutRope := FInputStream.Extract(StartOfRun, EndOfRun);
            FCurrentToken.CommentValue.AppendDestructively(TemporaryCutRope);
            Started := False;
         end;
      end;

      procedure Absorb(); inline;
      begin
         if (FInputStream.WasManipulated) then
         begin
            Flush();
            FCurrentToken.CommentValue.Append(FInputStream.CurrentCharacter);
         end
         else
         begin
            if (not Started) then
            begin
               Started := True;
               StartOfRun := FInputStream.GetPointer();
            end;
            EndOfRun := FInputStream.GetPointer();
         end;
      end;

   begin
      FCurrentToken.PrepareComment();
      Assert(FCurrentToken.CommentValue.IsEmpty);
      Started := False;
      repeat
         case (FInputStream.CurrentCharacter.Value) of
            $003E, kEOF:
               begin
                  Flush();
                  break;
               end;
            $0000:
               begin
                  Assert(not FInputStream.WasManipulated);
                  Flush();
                  FCurrentToken.CommentValue.Append($FFFD);
               end;
            $0001..$0008, $000B, $000E..$001F, $007F..$009F,
            $FDD0..$FDEF, $FFFE, $FFFF, $1FFFE, $1FFFF, $2FFFE,
            $2FFFF, $3FFFE, $3FFFF, $4FFFE, $4FFFF, $5FFFE, $5FFFF,
            $6FFFE, $6FFFF, $7FFFE, $7FFFF, $8FFFE, $8FFFF, $9FFFE,
            $9FFFF, $AFFFE, $AFFFF, $BFFFE, $BFFFF, $CFFFE, $CFFFF,
            $DFFFE, $DFFFF, $EFFFE, $EFFFF, $FFFFE, $FFFFF, $10FFFE,
            $10FFFF:
               begin
                  {$IFDEF PARSEERROR}
                     if (not FInputStream.WasNotedBad) then
                        ParseError('control characters and permanently undefined Unicode characters (noncharacters) are not allowed.');
                     // not calling FInputStream.NotedThatInputIsBad() since we know we won't reexamine it
                  {$ENDIF}
                  Absorb();
               end;
            else
               Absorb();
         end;
         FInputStream.Advance();
      until forever;
      FTokeniserState := tsDataState;
      FCurrentToken.Emit();
      if (FInputStream.CurrentCharacter = kEOF) then
         FInputStream.Unconsume();
   end;
   {$ELSE}
   procedure BogusCommentState(); inline;
   const
      FFFD: TUTF8Sequence = (AsString: #$EF#$BF#$BD);
   var
      StartBookmark: THTMLParser.TInputStream.TBookmark;
      CommentSize, Index: Cardinal;
      EncodedCharacter: TUTF8Sequence;

   var
      StartOfGoodRun: Pointer;
      LengthOfGoodRun: Cardinal;

      procedure Flush(); inline;
      begin
         if (LengthOfGoodRun > 0) then
         begin
            Assert(Index <= Length(FCurrentToken.CommentValue));
            Move(StartOfGoodRun^, FCurrentToken.CommentValue[Index], LengthOfGoodRun);
            Inc(Index, LengthOfGoodRun);
            LengthOfGoodRun := 0;
         end;
      end;

      procedure Absorb(); inline;
      begin
         if (FInputStream.WasManipulated) then
         begin
            Flush();
            EncodedCharacter := CodepointToUTF8(FInputStream.CurrentCharacter);
            Assert(Index+EncodedCharacter.Length-1 <= Length(FCurrentToken.CommentValue));
            Move(EncodedCharacter.Start, FCurrentToken.CommentValue[Index], EncodedCharacter.Length);
            Inc(Index, EncodedCharacter.Length);
         end
         else
         begin
            if (LengthOfGoodRun = 0) then
               StartOfGoodRun := FInputStream.GetRawPointer();
            Inc(LengthOfGoodRun, FInputStream.CurrentCharacterLength);
         end;
      end;

   begin
      StartBookmark := FInputStream.GetBookmark();
      FCurrentToken.PrepareComment();
      CommentSize := 0;
      repeat
         case (FInputStream.CurrentCharacter.Value) of
            $003E, kEOF: break; 
            $0000: Inc(CommentSize, FFFD.Length);
            else Inc(CommentSize, FInputStream.CurrentCharacterLength);
         end;
         FInputStream.Advance();
      until forever;
      SetLength(FCurrentToken.CommentValue, CommentSize);
      FInputStream.ReturnToBookmark(StartBookmark);
      Index := 1;
      LengthOfGoodRun := 0;
      repeat
         case (FInputStream.CurrentCharacter.Value) of
            $003E, kEOF:
               begin
                  Flush();
                  break;
               end;
            $0000:
               begin
                  Assert(not FInputStream.WasManipulated);
                  Flush();
                  Assert(Index+FFFD.Length-1 <= Length(FCurrentToken.CommentValue));
                  Move(FFFD.Start, FCurrentToken.CommentValue[Index], FFFD.Length);
                  Inc(Index, FFFD.Length);
               end;
            $0001..$0008, $000B, $000E..$001F, $007F..$009F,
            $FDD0..$FDEF, $FFFE, $FFFF, $1FFFE, $1FFFF, $2FFFE,
            $2FFFF, $3FFFE, $3FFFF, $4FFFE, $4FFFF, $5FFFE, $5FFFF,
            $6FFFE, $6FFFF, $7FFFE, $7FFFF, $8FFFE, $8FFFF, $9FFFE,
            $9FFFF, $AFFFE, $AFFFF, $BFFFE, $BFFFF, $CFFFE, $CFFFF,
            $DFFFE, $DFFFF, $EFFFE, $EFFFF, $FFFFE, $FFFFF, $10FFFE,
            $10FFFF:
               begin
                  {$IFDEF PARSEERROR}
                     if (not FInputStream.WasNotedBad) then
                        ParseError('control characters and permanently undefined Unicode characters (noncharacters) are not allowed.');
                     // not calling FInputStream.NotedThatInputIsBad() since we know we won't reexamine it
                  {$ENDIF}
                  Absorb();
               end;
            else
               Absorb();
         end;
         FInputStream.Advance();
      until forever;
      Assert(LengthOfGoodRun = 0);
      Assert(Index = CommentSize+1);
      FTokeniserState := tsDataState;
      FCurrentToken.Emit();
      if (FInputStream.CurrentCharacter = kEOF) then
         FInputStream.Unconsume();
   end;
   {$ENDIF}

   procedure CdataSectionState(out Reconsume: Boolean); inline;
   var
      StartPointer, EndPointer: TParserStringPointer;
   begin
      StartPointer := FInputStream.GetPointer();
      repeat
         case (FInputStream.CurrentCharacter.Value) of
            kEOF:
               begin
                  EndPointer := FInputStream.GetPointer();
                  if (StartPointer < EndPointer) then
                     FCurrentToken.AppendSourceNonSpaceBetweenBookmarks(StartPointer, EndPointer);
                  FTokeniserState := tsDataState;
                  Reconsume := True;
                  exit;
               end;
            $0000: // null
               begin
                  Assert(not FInputStream.WasManipulated);
                  EndPointer := FInputStream.GetPointer();
                  EndPointer.SetToZeroWidth();
                  if (StartPointer < EndPointer) then
                     FCurrentToken.AppendSourceNonSpaceBetweenBookmarks(StartPointer, EndPointer);
                  FCurrentToken.EmitNullCharacter();
                  Reconsume := False;
                  exit;
               end;
            $005D: // ]
               begin
                  Assert(not FInputStream.WasManipulated);
                  EndPointer := FInputStream.GetPointer();
                  FInputStream.Advance();
                  case (FInputStream.CurrentCharacter.Value) of
                     $005D: // ]
                        begin
                           repeat
                              FInputStream.Advance();
                              case (FInputStream.CurrentCharacter.Value) of
                                 $005D: // ]
                                    begin
                                       EndPointer.AdvanceToNext({$IFNDEF USEROPES} FInputStream.Data {$ENDIF});
                                    end;
                                 $003E: // >
                                    begin
                                       EndPointer.SetToZeroWidth();
                                       if (StartPointer < EndPointer) then
                                          FCurrentToken.AppendSourceNonSpaceBetweenBookmarks(StartPointer, EndPointer);
                                       FCurrentToken.EmitSourceCharacters();
                                       FTokeniserState := tsDataState;
                                       Reconsume := False;
                                       exit;
                                    end;
                                 else
                                    break;
                              end;
                           until forever;
                        end;
                  end;
               end;
            else
            begin
               if (FInputStream.WasManipulated) then
               begin
                  EndPointer := FInputStream.GetPointer();
                  EndPointer.SetToZeroWidth();
                  if (StartPointer < EndPointer) then
                     FCurrentToken.AppendSourceNonSpaceBetweenBookmarks(StartPointer, EndPointer);
                  Assert(FInputStream.CurrentCharacter <> $000D);
                  Assert(FInputStream.CurrentCharacter = $000A); // pretty sure this is the only way we can get here
                  case (FInputStream.CurrentCharacter.Value) of
                     $0009, $000A, $000C, $0020: FCurrentToken.EmitExtraSpaceCharacter(FInputStream.CurrentCharacter);
                     else FCurrentToken.EmitExtraNonSpaceCharacter(FInputStream.CurrentCharacter);
                  end;
                  Reconsume := False;
                  exit;
               end;
               Assert(not FInputStream.WasManipulated);
               FInputStream.Advance();
            end;
         end;
      until forever;
   end;

   function SkipPUBLIC(): Boolean; inline;
   var
      Bookmark: THTMLParser.TInputStream.TBookmark;
   begin
      Bookmark := FInputStream.GetBookmark();
      case (FInputStream.CurrentCharacter.Value) of
         $0050, $0070:
            begin
               FInputStream.Advance();
               case (FInputStream.CurrentCharacter.Value) of
                  $0055, $0075:
                     begin
                        FInputStream.Advance();
                        case (FInputStream.CurrentCharacter.Value) of
                           $0042, $0062:
                              begin
                                 FInputStream.Advance();
                                 case (FInputStream.CurrentCharacter.Value) of
                                    $004C, $006C:
                                       begin
                                          FInputStream.Advance();
                                          case (FInputStream.CurrentCharacter.Value) of
                                             $0049, $0069:
                                                begin
                                                   FInputStream.Advance();
                                                   case (FInputStream.CurrentCharacter.Value) of
                                                      $0043, $0063:
                                                         begin
                                                            Result := True;
                                                            exit;
                                                         end;
                                                   end;
                                                end;
                                          end;
                                       end;
                                 end;
                              end;
                        end;
                     end;
               end;
            end;
      end;
      FInputStream.ReturnToBookmark(Bookmark);
      Result := False;
   end;

   function SkipSYSTEM(): Boolean; inline;
   var
      Bookmark: THTMLParser.TInputStream.TBookmark;
   begin
      Bookmark := FInputStream.GetBookmark();
      case (FInputStream.CurrentCharacter.Value) of
         $0053, $0073:
            begin
               FInputStream.Advance();
               case (FInputStream.CurrentCharacter.Value) of
                  $0059, $0079:
                     begin
                        FInputStream.Advance();
                        case (FInputStream.CurrentCharacter.Value) of
                           $0053, $0073:
                              begin
                                 FInputStream.Advance();
                                 case (FInputStream.CurrentCharacter.Value) of
                                    $0054, $0074:
                                       begin
                                          FInputStream.Advance();
                                          case (FInputStream.CurrentCharacter.Value) of
                                             $0045, $0065:
                                                begin
                                                   FInputStream.Advance();
                                                   case (FInputStream.CurrentCharacter.Value) of
                                                      $004D, $006D:
                                                         begin
                                                            Result := True;
                                                            exit;
                                                         end;
                                                   end;
                                                end;
                                          end;
                                       end;
                                 end;
                              end;
                        end;
                     end;
               end;
            end;
      end;
      FInputStream.ReturnToBookmark(Bookmark);
      Result := False;
   end;

   function CurrentTokenIsAppropriateEndTagToken(): Boolean;
   begin
      Assert(FCurrentToken.Kind = tkEndTag);
      Result := FCurrentToken.DraftTagName = FLastStartTag;
   end;

   procedure ConsumeACharacterReferenceInText(); inline;
   var
      Reconsume: Boolean;
      Characters: TUnicodeCodepointArray;
   begin
      if (not ConsumeACharacterReference(Characters, Reconsume, crInText, kNoAdditionalAllowedCharacter)) then
      begin
         Assert(FInputStream.CurrentCharacter = $0026);
         AppendInputNonSpaceCharacter();
         Assert(not Reconsume);
         {$IFOPT C+} FCurrentToken.FCharStart.AssertIdentity(@FInputStream.Data); {$ENDIF}
         {$IFOPT C+} FCurrentToken.FCharEnd.AssertIdentity(@FInputStream.Data); {$ENDIF}
      end
      else
      begin
         FCurrentToken.EmitExtraAmbiguousCharacters(Characters);
         if (Reconsume) then
            FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
      end;
   end;

   function ConsumeACharacterReferenceInAttribute(const AdditionalAllowedCharacter: TUnicodeCodepoint): Boolean; inline;
   var
      Characters: TUnicodeCodepointArray;
   begin
      if (not ConsumeACharacterReference(Characters, Result, crInAttribute, AdditionalAllowedCharacter)) then
      begin
         Assert(FInputStream.CurrentCharacter = $0026);
         FCurrentToken.CurrentAttributeValue.Append(FInputStream.CurrentCharacter);
         Assert(not Result);
      end
      else
      begin
         FCurrentToken.CurrentAttributeValue.Append(Characters);
      end;
   end;

var
   StartOfPotentialTokenBookmark: THTMLParser.TInputStream.TBookmark;
   Reconsume: Boolean;
begin
   repeat
      Assert(not FCurrentToken.Ready);
      {$IFDEF VERBOSETOKENISER} Writeln('Tokeniser: ', FTokeniserState); {$ENDIF}
      FInputStream.Advance();
      repeat
         {$IFDEF PARSEERROR}
            case (FInputStream.CurrentCharacter.Value) of
               $0001..$0008, $000B, $000E..$001F, $007F..$009F,
               $FDD0..$FDEF, $FFFE, $FFFF, $1FFFE, $1FFFF, $2FFFE,
               $2FFFF, $3FFFE, $3FFFF, $4FFFE, $4FFFF, $5FFFE, $5FFFF,
               $6FFFE, $6FFFF, $7FFFE, $7FFFF, $8FFFE, $8FFFF, $9FFFE,
               $9FFFF, $AFFFE, $AFFFF, $BFFFE, $BFFFF, $CFFFE, $CFFFF,
               $DFFFE, $DFFFF, $EFFFE, $EFFFF, $FFFFE, $FFFFF, $10FFFE,
               $10FFFF:
                  begin
                     if (not FInputStream.WasNotedBad) then
                     begin
                        ParseError('control characters and permanently undefined Unicode characters (noncharacters) are not allowed.');
                        FInputStream.NotedThatInputIsBad();
                     end;
                  end;
            end;
         {$ENDIF}
         {$IFDEF INSTRUMENTING} Writeln('TOKENISER: ', FTokeniserState); {$ENDIF INSTRUMENTING}
         case (FTokeniserState) of
            // initial state is at the bottom
            tsDataState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $000D, $0020: AppendInputSpaceCharacter();
                  $0026: ConsumeACharacterReferenceInText();
                  $003C:
                     begin
                        StartOfPotentialTokenBookmark := FInputStream.GetBookmark();
                        FTokeniserState := tsTagOpenState;
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('U+0000 not valid in text'); {$ENDIF}
                        FCurrentToken.EmitNullCharacter();
                     end;
                  kEOF: FCurrentToken.EmitEOF();
                  else AppendInputNonSpaceCharacter();
               end;
            // this state is common so it's been moved up
            tsTagNameState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020: FTokeniserState := tsBeforeAttributeNameState;
                  $002F: FTokeniserState := tsSelfClosingStartTagState;
                  $003E:
                     begin
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  Ord('A')..Ord('Z'): FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected U+0000 in tag name'); {$ENDIF}
                        FCurrentToken.DraftTagName.Append($FFFD);
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in tag name'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.RetractTag();
                        continue; // not emitting anything, so ok to reconsume directly
                     end;
                  else
                     FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter);
               end;
            // this state is common so it's been moved up
            tsAttributeValueDoubleQuotedState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0022: FTokeniserState := tsAfterAttributeValueQuotedState;
                  $0026: if (ConsumeACharacterReferenceInAttribute($0022)) then continue;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected U+0000 in attribute value'); {$ENDIF}
                        FCurrentToken.CurrentAttributeValue.Append($FFFD);
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in attribute value'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.RetractTag();
                        continue; // not emitting anything, so ok to reconsume directly
                     end;
                  else
                     FCurrentToken.CurrentAttributeValue.Append(FInputStream.CurrentCharacter);
               end;
            // common state
            tsCommentState:
               case (FInputStream.CurrentCharacter.Value) of
                  $002D:
                     begin
                        FTokeniserState := tsCommentEndDashState;
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected U+0000 in comment'); {$ENDIF}
                        FCurrentToken.CommentValue.Append($FFFD);
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in comment'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                  begin
                     FCurrentToken.CommentValue.Append(FInputStream.CurrentCharacter);
                  end;
               end;
            // common state
            tsTagOpenState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0021: FTokeniserState := tsMarkupDeclarationOpenState;
                  $002F: FTokeniserState := tsEndTagOpenState;
                  Ord('A')..Ord('Z'):
                     begin
                        FCurrentToken.PrepareStartTag();
                        FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                        FTokeniserState := tsTagNameState;
                     end;
                  Ord('a')..Ord('z'):
                     begin
                        FCurrentToken.PrepareStartTag();
                        FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter);
                        FTokeniserState := tsTagNameState;
                     end;
                  $003F:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected "?" after "<"'); {$ENDIF}
                        FTokeniserState := tsBogusCommentState;
                        continue; // includes the character that jumped us into the bogus comment state
                     end;
                  else
                  begin
                     {$IFDEF PARSEERROR} ParseError('unescaped "<" in text'); {$ENDIF}
                     FTokeniserState := tsDataState;
                     AppendBookmarkedCharacter(StartOfPotentialTokenBookmark); {BOGUS Warning: Local variable "StartOfPotentialTokenBookmark" does not seem to be initialized}
                     continue; // ok to reconsume directly since we are emitting a character
                  end;
               end;
            // common state
            tsAttributeNameState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020: FTokeniserState := tsAfterAttributeNameState;
                  $002F: FTokeniserState := tsSelfClosingStartTagState;
                  $003D: FTokeniserState := tsBeforeAttributeValueState;
                  $003E:
                     begin
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  Ord('A')..Ord('Z'): FCurrentToken.CurrentAttributeName.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected U+0000 in attribute name'); {$ENDIF}
                        FCurrentToken.CurrentAttributeName.Append($FFFD);
                     end;
                  $0022, $0027, $003C: 
                     begin
                        {$IFDEF PARSEERROR} ParseError('invalid character in attribute name'); {$ENDIF}
                        FCurrentToken.CurrentAttributeName.Append(FInputStream.CurrentCharacter);
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in attribute name'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.RetractTag();
                        continue; // not emitting anything, so ok to reconsume directly
                     end;
                  else
                     FCurrentToken.CurrentAttributeName.Append(FInputStream.CurrentCharacter);
               end;
            // common state
            tsEndTagOpenState:
               case (FInputStream.CurrentCharacter.Value) of
                  Ord('A')..Ord('Z'):
                     begin
                        FCurrentToken.PrepareEndTag();
                        FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                        FTokeniserState := tsTagNameState;
                     end;
                  Ord('a')..Ord('z'):
                     begin
                        FCurrentToken.PrepareEndTag();
                        FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter);
                        FTokeniserState := tsTagNameState;
                     end;
                  $003E:
                     begin
                        FCurrentToken.EmitSourceCharacters();
                        {$IFDEF PARSEERROR} ParseError('</> sequence'); {$ENDIF}
                        FTokeniserState := tsDataState;
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in end tag'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        AppendFromBookmarkedCharacterToBeforeInputCharacter(StartOfPotentialTokenBookmark);
                        FInputStream.Unconsume(); // can't just "continue" since we just emitted characters
                     end;
                  else
                  begin
                     {$IFDEF PARSEERROR} ParseError('unexpected characters in end tag'); {$ENDIF}
                     FTokeniserState := tsBogusCommentState;
                     continue; // reconsume this character in the bogus comment state, since otherwise we miss the first one
                  end;
               end;
            tsRcdataState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $000D, $0020: AppendInputSpaceCharacter();
                  $0026: ConsumeACharacterReferenceInText();
                  $003C:
                     begin
                        StartOfPotentialTokenBookmark := FInputStream.GetBookmark();
                        FTokeniserState := tsRcdataLessThanSignState;
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('U+0000 not valid in text'); {$ENDIF}
                        FCurrentToken.EmitExtraNonSpaceCharacter($FFFD);
                     end;
                  kEOF: FCurrentToken.EmitEOF();
                  else AppendInputNonSpaceCharacter();
               end;
            tsRawtextState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $000D, $0020: AppendInputSpaceCharacter();
                  $003C:
                     begin
                        StartOfPotentialTokenBookmark := FInputStream.GetBookmark();
                        FTokeniserState := tsRawtextLessThanSignState;
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('U+0000 not valid in text'); {$ENDIF}
                        FCurrentToken.EmitExtraNonSpaceCharacter($FFFD);
                     end;
                  kEOF: FCurrentToken.EmitEOF();
                  else AppendInputNonSpaceCharacter();
               end;
            tsScriptDataState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $000D, $0020: AppendInputSpaceCharacter();
                  $003C:
                     begin
                        StartOfPotentialTokenBookmark := FInputStream.GetBookmark();
                        FTokeniserState := tsScriptDataLessThanSignState;
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('U+0000 not valid in text'); {$ENDIF}
                        FCurrentToken.EmitExtraNonSpaceCharacter($FFFD);
                     end;
                  kEOF: FCurrentToken.EmitEOF();
                  else AppendInputNonSpaceCharacter();
               end;
            tsPlaintextState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $000D, $0020: AppendInputSpaceCharacter();
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('U+0000 not valid in text'); {$ENDIF}
                        FCurrentToken.EmitExtraNonSpaceCharacter($FFFD);
                     end;
                  kEOF: FCurrentToken.EmitEOF();
                  else AppendInputNonSpaceCharacter();
               end;
            // tsTagOpenState is earlier
            // tsEndTagOpenState is earlier
            // tsTagNameState is near the top
            tsRcdataLessThanSignState:
               case (FInputStream.CurrentCharacter.Value) of
                  $002F: FTokeniserState := tsRcdataEndTagOpenState;
                  else
                  begin
                     FTokeniserState := tsRcdataState;
                     AppendBookmarkedCharacter(StartOfPotentialTokenBookmark);
                     continue; // ok to reconsume directly since we are emitting a character
                  end;
               end;
            tsRcdataEndTagOpenState:
               case (FInputStream.CurrentCharacter.Value) of
                  Ord('A')..Ord('Z'):
                     begin
                        FCurrentToken.PrepareEndTag();
                        FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                        FTokeniserState := tsRcdataEndTagNameState;
                     end;
                  Ord('a')..Ord('z'):
                     begin
                        FCurrentToken.PrepareEndTag();
                        FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter);
                        FTokeniserState := tsRcdataEndTagNameState;
                     end;
                  else
                  begin
                     FTokeniserState := tsRcdataState;
                     AppendFromBookmarkedCharacterToBeforeInputCharacter(StartOfPotentialTokenBookmark);
                     continue; // ok to reconsume directly since we are emitting a character
                  end;
               end;
            tsRcdataEndTagNameState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020:
                     begin
                        if (CurrentTokenIsAppropriateEndTagToken()) then
                           FTokeniserState := tsBeforeAttributeNameState
                        else
                        begin
                           FTokeniserState := tsRcdataState;
                           FCurrentToken.RetractTag();
                           AppendFromBookmarkedCharacterToBeforeInputCharacter(StartOfPotentialTokenBookmark);
                           continue; // ok to reconsume directly since we are emitting a character
                        end;
                     end;
                  $002F:
                     begin
                        if (CurrentTokenIsAppropriateEndTagToken()) then
                           FTokeniserState := tsSelfClosingStartTagState
                        else
                        begin
                           FTokeniserState := tsRcdataState;
                           FCurrentToken.RetractTag();
                           AppendFromBookmarkedCharacterToInputCharacter(StartOfPotentialTokenBookmark);
                           continue; // ok to reconsume directly since we are emitting a character
                        end;
                     end;
                  $003E:
                     begin
                        if (CurrentTokenIsAppropriateEndTagToken()) then
                        begin
                           FTokeniserState := tsDataState;
                           FCurrentToken.Emit();
                        end
                        else
                        begin
                           FTokeniserState := tsRcdataState;
                           FCurrentToken.RetractTag();
                           AppendFromBookmarkedCharacterToInputCharacter(StartOfPotentialTokenBookmark);
                           continue; // ok to reconsume directly since we are emitting a character
                        end;
                     end;
                  Ord('A')..Ord('Z'):
                     FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                  Ord('a')..Ord('z'):
                     FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter.Value);
                  else
                  begin
                     FTokeniserState := tsRcdataState;
                     FCurrentToken.RetractTag();
                     AppendFromBookmarkedCharacterToBeforeInputCharacter(StartOfPotentialTokenBookmark);
                     continue; // ok to reconsume directly since we are emitting a character
                  end;
               end;
            tsRawtextLessThanSignState:
               case (FInputStream.CurrentCharacter.Value) of
                  $002F: FTokeniserState := tsRawtextEndTagOpenState;
                  else
                  begin
                     FTokeniserState := tsRawtextState;
                     AppendBookmarkedCharacter(StartOfPotentialTokenBookmark);
                     continue; // ok to reconsume directly since we are emitting a character
                  end;
               end;
            tsRawtextEndTagOpenState:
               case (FInputStream.CurrentCharacter.Value) of
                  Ord('A')..Ord('Z'):
                     begin
                        FCurrentToken.PrepareEndTag();
                        FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                        FTokeniserState := tsRawtextEndTagNameState;
                     end;
                  Ord('a')..Ord('z'):
                     begin
                        FCurrentToken.PrepareEndTag();
                        FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter);
                        FTokeniserState := tsRawtextEndTagNameState;
                     end;
                  else
                  begin
                     FTokeniserState := tsRawtextState;
                     AppendFromBookmarkedCharacterToBeforeInputCharacter(StartOfPotentialTokenBookmark);
                     continue; // ok to reconsume directly since we are emitting a character
                  end;
               end;
            tsRawtextEndTagNameState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020:
                     begin
                        if (CurrentTokenIsAppropriateEndTagToken()) then
                           FTokeniserState := tsBeforeAttributeNameState
                        else
                        begin
                           FTokeniserState := tsRawtextState;
                           FCurrentToken.RetractTag();
                           AppendFromBookmarkedCharacterToBeforeInputCharacter(StartOfPotentialTokenBookmark);
                           continue; // ok to reconsume directly since we are emitting a character
                        end;
                     end;
                  $002F:
                     begin
                        if (CurrentTokenIsAppropriateEndTagToken()) then
                           FTokeniserState := tsSelfClosingStartTagState
                        else
                        begin
                           FTokeniserState := tsRawtextState;
                           FCurrentToken.RetractTag();
                           AppendFromBookmarkedCharacterToInputCharacter(StartOfPotentialTokenBookmark);
                           continue; // ok to reconsume directly since we are emitting a character
                        end;
                     end;
                  $003E:
                     begin
                        if (CurrentTokenIsAppropriateEndTagToken()) then
                        begin
                           FTokeniserState := tsDataState;
                           FCurrentToken.Emit();
                        end
                        else
                        begin
                           FTokeniserState := tsRawtextState;
                           FCurrentToken.RetractTag();
                           AppendFromBookmarkedCharacterToInputCharacter(StartOfPotentialTokenBookmark);
                           continue; // ok to reconsume directly since we are emitting a character
                        end;
                     end;
                  Ord('A')..Ord('Z'):
                     FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                  Ord('a')..Ord('z'):
                     FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter.Value);
                  else
                  begin
                     FTokeniserState := tsRawtextState;
                     FCurrentToken.RetractTag();
                     AppendFromBookmarkedCharacterToBeforeInputCharacter(StartOfPotentialTokenBookmark);
                     continue; // ok to reconsume directly since we are emitting a character
                  end;
               end;
            tsScriptDataLessThanSignState:
               case (FInputStream.CurrentCharacter.Value) of
                  $002F: FTokeniserState := tsScriptDataEndTagOpenState; // "/"
                  $0021: // !
                     begin
                        FTokeniserState := tsScriptDataEscapeStartState;
                        AppendFromBookmarkedCharacterToInputCharacter(StartOfPotentialTokenBookmark);
                     end;
                  else
                  begin
                     FTokeniserState := tsScriptDataState;
                     AppendBookmarkedCharacter(StartOfPotentialTokenBookmark);
                     continue; // ok to reconsume directly since we are emitting a character
                  end;
               end;
            tsScriptDataEndTagOpenState:
               case (FInputStream.CurrentCharacter.Value) of
                  Ord('A')..Ord('Z'):
                     begin
                        FCurrentToken.PrepareEndTag();
                        FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                        FTokeniserState := tsScriptDataEndTagNameState;
                     end;
                  Ord('a')..Ord('z'):
                     begin
                        FCurrentToken.PrepareEndTag();
                        FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter);
                        FTokeniserState := tsScriptDataEndTagNameState;
                     end;
                  else
                  begin
                     FTokeniserState := tsScriptDataState;
                     AppendFromBookmarkedCharacterToBeforeInputCharacter(StartOfPotentialTokenBookmark);
                     continue; // ok to reconsume directly since we are emitting a character
                  end;
               end;
            tsScriptDataEndTagNameState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020:
                     begin
                        if (CurrentTokenIsAppropriateEndTagToken()) then
                           FTokeniserState := tsBeforeAttributeNameState
                        else
                        begin
                           FTokeniserState := tsScriptDataState;
                           FCurrentToken.RetractTag();
                           AppendFromBookmarkedCharacterToBeforeInputCharacter(StartOfPotentialTokenBookmark);
                           continue; // ok to reconsume directly since we are emitting a character
                        end;
                     end;
                  $002F:
                     begin
                        if (CurrentTokenIsAppropriateEndTagToken()) then
                           FTokeniserState := tsSelfClosingStartTagState
                        else
                        begin
                           FTokeniserState := tsScriptDataState;
                           FCurrentToken.RetractTag();
                           AppendFromBookmarkedCharacterToInputCharacter(StartOfPotentialTokenBookmark);
                           continue; // ok to reconsume directly since we are emitting a character
                        end;
                     end;
                  $003E:
                     begin
                        if (CurrentTokenIsAppropriateEndTagToken()) then
                        begin
                           FTokeniserState := tsDataState;
                           FCurrentToken.Emit();
                        end
                        else
                        begin
                           FTokeniserState := tsScriptDataState;
                           FCurrentToken.RetractTag();
                           AppendFromBookmarkedCharacterToInputCharacter(StartOfPotentialTokenBookmark);
                           continue; // ok to reconsume directly since we are emitting a character
                        end;
                     end;
                  Ord('A')..Ord('Z'):
                     FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                  Ord('a')..Ord('z'):
                     FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter.Value);
                  else
                  begin
                     FTokeniserState := tsScriptDataState;
                     FCurrentToken.RetractTag();
                     AppendFromBookmarkedCharacterToBeforeInputCharacter(StartOfPotentialTokenBookmark);
                     continue; // ok to reconsume directly since we are emitting a character
                  end;
               end;
            tsScriptDataEscapeStartState:
               case (FInputStream.CurrentCharacter.Value) of
                  $002D:
                     begin
                        FTokeniserState := tsScriptDataEscapeStartDashState;
                        AppendInputNonSpaceCharacter();
                     end;
                  else
                  begin
                     FTokeniserState := tsScriptDataState;
                     continue; // ok to reconsume directly since we are emitting a character
                  end;
               end;
            tsScriptDataEscapeStartDashState:
               case (FInputStream.CurrentCharacter.Value) of
                  $002D:
                     begin
                        FTokeniserState := tsScriptDataEscapedDashDashState;
                        AppendInputNonSpaceCharacter();
                     end;
                  else
                  begin
                     FTokeniserState := tsScriptDataState;
                     continue; // ok to reconsume directly since we are emitting a character
                  end;
               end;
            tsScriptDataEscapedState:
               case (FInputStream.CurrentCharacter.Value) of
                  $002D:
                     begin
                        FTokeniserState := tsScriptDataEscapedDashState;
                        AppendInputNonSpaceCharacter();
                     end;
                  $003C: // <
                     begin
                        FTokeniserState := tsScriptDataEscapedLessThanSignState;
                        StartOfPotentialTokenBookmark := FInputStream.GetBookmark();
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('U+0000 not valid in text'); {$ENDIF}
                        FCurrentToken.EmitExtraNonSpaceCharacter($FFFD);
                     end;
                  kEOF:
                     begin
                        FTokeniserState := tsDataState;
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in script'); {$ENDIF}
                        continue; // ok to reconsume directly since we are emitting a character
                     end;
                  $0009, $000A, $000C, $0020:
                     begin
                        AppendInputSpaceCharacter();
                     end;
                  else
                  begin
                     AppendInputNonSpaceCharacter();
                  end;
               end;
            tsScriptDataEscapedDashState:
               case (FInputStream.CurrentCharacter.Value) of
                  $002D:
                     begin
                        FTokeniserState := tsScriptDataEscapedDashDashState;
                        AppendInputNonSpaceCharacter();
                     end;
                  $003C:
                     begin
                        FTokeniserState := tsScriptDataEscapedLessThanSignState;
                        StartOfPotentialTokenBookmark := FInputStream.GetBookmark();
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('U+0000 not valid in text'); {$ENDIF}
                        FCurrentToken.EmitExtraNonSpaceCharacter($FFFD);
                     end;
                  kEOF:
                     begin
                        FTokeniserState := tsDataState;
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in script'); {$ENDIF}
                        continue; // ok to reconsume directly since we are emitting a character
                     end;
                  $0009, $000A, $000C, $0020:
                     begin
                        FTokeniserState := tsScriptDataEscapedState;
                        AppendInputSpaceCharacter();
                     end;
                  else
                  begin
                     FTokeniserState := tsScriptDataEscapedState;
                     AppendInputNonSpaceCharacter();
                  end;
               end;
            tsScriptDataEscapedDashDashState:
               case (FInputStream.CurrentCharacter.Value) of
                  $002D:
                     begin
                        AppendInputNonSpaceCharacter();
                     end;
                  $003C:
                     begin
                        FTokeniserState := tsScriptDataEscapedLessThanSignState;
                        StartOfPotentialTokenBookmark := FInputStream.GetBookmark();
                     end;
                  $003E:
                     begin
                        FTokeniserState := tsScriptDataState;
                        AppendInputNonSpaceCharacter();
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('U+0000 not valid in text'); {$ENDIF}
                        FTokeniserState := tsScriptDataEscapedState;
                        FCurrentToken.EmitExtraNonSpaceCharacter($FFFD);
                     end;
                  kEOF:
                     begin
                        FTokeniserState := tsDataState;
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in script'); {$ENDIF}
                        continue; // ok to reconsume directly since we are emitting a character
                     end;
                  $0009, $000A, $000C, $0020:
                     begin
                        FTokeniserState := tsScriptDataEscapedState;
                        AppendInputSpaceCharacter();
                     end;
                  else
                  begin
                     FTokeniserState := tsScriptDataEscapedState;
                     AppendInputNonSpaceCharacter();
                  end;
               end;
            tsScriptDataEscapedLessThanSignState:
               case (FInputStream.CurrentCharacter.Value) of
                  $002F:
                     begin
                        FTokeniserState := tsScriptDataEscapedEndTagOpenState;
                     end;
                  Ord('A')..Ord('Z'):
                     begin
                        FTemporaryBuffer.Init();
                        FTemporaryBuffer.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                        AppendFromBookmarkedCharacterToInputCharacter(StartOfPotentialTokenBookmark);
                        FTokeniserState := tsScriptDataDoubleEscapeStartState;
                     end;
                  Ord('a')..Ord('z'):
                     begin
                        FTemporaryBuffer.Init();
                        FTemporaryBuffer.Append(FInputStream.CurrentCharacter);
                        AppendFromBookmarkedCharacterToInputCharacter(StartOfPotentialTokenBookmark);
                        FTokeniserState := tsScriptDataDoubleEscapeStartState;
                     end;
                  else
                  begin
                     FTokeniserState := tsScriptDataEscapedState;
                     AppendFromBookmarkedCharacterToBeforeInputCharacter(StartOfPotentialTokenBookmark);
                     continue; // ok to reconsume directly since we are emitting a character
                  end;
               end;
            tsScriptDataEscapedEndTagOpenState:
               case (FInputStream.CurrentCharacter.Value) of
                  Ord('A')..Ord('Z'):
                     begin
                        FCurrentToken.PrepareEndTag();
                        FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                        FTokeniserState := tsScriptDataEscapedEndTagNameState;
                     end;
                  Ord('a')..Ord('z'):
                     begin
                        FCurrentToken.PrepareEndTag();
                        FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter);
                        FTokeniserState := tsScriptDataEscapedEndTagNameState;
                     end;
                  else
                  begin
                     FTokeniserState := tsScriptDataEscapedState;
                     AppendFromBookmarkedCharacterToBeforeInputCharacter(StartOfPotentialTokenBookmark);
                     continue; // ok to reconsume directly since we are emitting a character
                  end;
               end;
            tsScriptDataEscapedEndTagNameState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020:
                     begin
                        if (CurrentTokenIsAppropriateEndTagToken()) then
                           FTokeniserState := tsBeforeAttributeNameState
                        else
                        begin
                           FTokeniserState := tsScriptDataEscapedState;
                           FCurrentToken.RetractTag();
                           AppendFromBookmarkedCharacterToBeforeInputCharacter(StartOfPotentialTokenBookmark);
                           continue; // ok to reconsume directly since we are emitting a character
                        end;
                     end;
                  $002F:
                     begin
                        if (CurrentTokenIsAppropriateEndTagToken()) then
                           FTokeniserState := tsSelfClosingStartTagState
                        else
                        begin
                           FTokeniserState := tsScriptDataEscapedState;
                           FCurrentToken.RetractTag();
                           AppendFromBookmarkedCharacterToBeforeInputCharacter(StartOfPotentialTokenBookmark);
                           continue; // ok to reconsume directly since we are emitting a character
                        end;
                     end;
                  $003E:
                     begin
                        if (CurrentTokenIsAppropriateEndTagToken()) then
                        begin
                           FTokeniserState := tsDataState;
                           FCurrentToken.Emit();
                        end
                        else
                        begin
                           FTokeniserState := tsScriptDataEscapedState;
                           FCurrentToken.RetractTag();
                           AppendFromBookmarkedCharacterToBeforeInputCharacter(StartOfPotentialTokenBookmark);
                           continue; // ok to reconsume directly since we are emitting a character
                        end;
                     end;
                  Ord('A')..Ord('Z'):
                     FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                  Ord('a')..Ord('z'):
                     FCurrentToken.DraftTagName.Append(FInputStream.CurrentCharacter.Value);
                  else
                  begin
                     FTokeniserState := tsScriptDataEscapedState;
                     FCurrentToken.RetractTag();
                     AppendFromBookmarkedCharacterToBeforeInputCharacter(StartOfPotentialTokenBookmark);
                     continue; // ok to reconsume directly since we are emitting a character
                  end;
               end;
            tsScriptDataDoubleEscapeStartState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020:
                     begin
                        if (FTemporaryBuffer = 'script') then
                           FTokeniserState := tsScriptDataDoubleEscapedState
                        else
                           FTokeniserState := tsScriptDataEscapedState;
                        AppendInputSpaceCharacter();
                     end;
                  $002F, $003E:
                     begin
                        if (FTemporaryBuffer = 'script') then
                           FTokeniserState := tsScriptDataDoubleEscapedState
                        else
                           FTokeniserState := tsScriptDataEscapedState;
                        AppendInputNonSpaceCharacter();
                     end;
                  Ord('A')..Ord('Z'):
                     begin
                        FTemporaryBuffer.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                        AppendInputNonSpaceCharacter();
                     end;
                  Ord('a')..Ord('z'):
                     begin
                        FTemporaryBuffer.Append(FInputStream.CurrentCharacter);
                        AppendInputNonSpaceCharacter();
                     end;
                  else
                  begin
                     FTokeniserState := tsScriptDataEscapedState;
                     continue; // ok to reconsume directly since we are emitting a character
                  end;
               end;
            tsScriptDataDoubleEscapedState:
               case (FInputStream.CurrentCharacter.Value) of
                  $002D:
                     begin
                        FTokeniserState := tsScriptDataDoubleEscapedDashState;
                        AppendInputNonSpaceCharacter();
                     end;
                  $003C:
                     begin
                        FTokeniserState := tsScriptDataDoubleEscapedLessThanSignState;
                        AppendInputNonSpaceCharacter();
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('U+0000 not valid in text'); {$ENDIF}
                        FCurrentToken.EmitExtraNonSpaceCharacter($FFFD);
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in script'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        continue; // ok to reconsume directly since we are emitting a character
                     end;
                  $0009, $000A, $000C, $0020:
                     begin
                        AppendInputSpaceCharacter();
                     end;
                  else
                  begin
                     AppendInputNonSpaceCharacter();
                  end;
               end;
            tsScriptDataDoubleEscapedDashState:
               case (FInputStream.CurrentCharacter.Value) of
                  $002D:
                     begin
                        FTokeniserState := tsScriptDataDoubleEscapedDashDashState;
                        AppendInputNonSpaceCharacter();
                     end;
                  $003C:
                     begin
                        FTokeniserState := tsScriptDataDoubleEscapedLessThanSignState;
                        AppendInputNonSpaceCharacter();
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('U+0000 not valid in text'); {$ENDIF}
                        FTokeniserState := tsScriptDataDoubleEscapedState;
                        FCurrentToken.EmitExtraNonSpaceCharacter($FFFD);
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in script'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        continue; // ok to reconsume directly since we are emitting a character
                     end;
                  $0009, $000A, $000C, $0020:
                     begin
                        FTokeniserState := tsScriptDataDoubleEscapedState;
                        AppendInputSpaceCharacter();
                     end;
                  else
                  begin
                     FTokeniserState := tsScriptDataDoubleEscapedState;
                     AppendInputNonSpaceCharacter();
                  end;
               end;
            tsScriptDataDoubleEscapedDashDashState:
               case (FInputStream.CurrentCharacter.Value) of
                  $002D:
                     begin
                        AppendInputNonSpaceCharacter();
                     end;
                  $003C:
                     begin
                        FTokeniserState := tsScriptDataDoubleEscapedLessThanSignState;
                        AppendInputNonSpaceCharacter();
                     end;
                  $003E:
                     begin
                        FTokeniserState := tsScriptDataState;
                        AppendInputNonSpaceCharacter();
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('U+0000 not valid in text'); {$ENDIF}
                        FTokeniserState := tsScriptDataDoubleEscapedState;
                        FCurrentToken.EmitExtraNonSpaceCharacter($FFFD);
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in script'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        continue; // ok to reconsume directly since we are emitting a character
                     end;
                  $0009, $000A, $000C, $0020:
                     begin
                        FTokeniserState := tsScriptDataDoubleEscapedState;
                        AppendInputSpaceCharacter();
                     end;
                  else
                  begin
                     FTokeniserState := tsScriptDataDoubleEscapedState;
                     AppendInputNonSpaceCharacter();
                  end;
               end;
            tsScriptDataDoubleEscapedLessThanSignState:
               case (FInputStream.CurrentCharacter.Value) of
                  $002F:
                     begin
                        FTemporaryBuffer.Init();
                        FTokeniserState := tsScriptDataDoubleEscapeEndState;
                        AppendInputNonSpaceCharacter();
                     end;
                  else
                  begin
                     FTokeniserState := tsScriptDataDoubleEscapedState;
                     continue; // ok to reconsume directly since we are emitting a character
                  end;
               end;
            tsScriptDataDoubleEscapeEndState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020:
                     begin
                        if (FTemporaryBuffer = 'script') then
                           FTokeniserState := tsScriptDataEscapedState
                        else
                           FTokeniserState := tsScriptDataDoubleEscapedState;
                        AppendInputSpaceCharacter();
                     end;
                  $002F, $003E:
                     begin
                        if (FTemporaryBuffer = 'script') then
                           FTokeniserState := tsScriptDataEscapedState
                        else
                           FTokeniserState := tsScriptDataDoubleEscapedState;
                        AppendInputNonSpaceCharacter();
                     end;
                  Ord('A')..Ord('Z'):
                     begin
                        FTemporaryBuffer.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                        AppendInputNonSpaceCharacter();
                     end;
                  Ord('a')..Ord('z'):
                     begin
                        FTemporaryBuffer.Append(FInputStream.CurrentCharacter);
                        AppendInputNonSpaceCharacter();
                     end;
                  else
                  begin
                     FTokeniserState := tsScriptDataDoubleEscapedState;
                     continue; // ok to reconsume directly since we are emitting a character
                  end;
               end;
            tsBeforeAttributeNameState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020: ;
                  $002F: FTokeniserState := tsSelfClosingStartTagState;
                  $003E:
                     begin
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  Ord('A')..Ord('Z'):
                     begin
                        FCurrentToken.PrepareAttribute();
                        FCurrentToken.CurrentAttributeName.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                        FTokeniserState := tsAttributeNameState;
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected U+0000 in attribute name'); {$ENDIF}
                        FCurrentToken.PrepareAttribute();
                        FCurrentToken.CurrentAttributeName.Append($FFFD); // $R-
                        FTokeniserState := tsAttributeNameState;
                     end;
                  $0022, $0027, $003C, $003D:
                     begin
                        {$IFDEF PARSEERROR} ParseError('invalid character starting attribute name'); {$ENDIF}
                        FCurrentToken.PrepareAttribute();
                        FCurrentToken.CurrentAttributeName.Append(FInputStream.CurrentCharacter);
                        FTokeniserState := tsAttributeNameState;
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in tag'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.RetractTag();
                        continue; // not emitting anything, so ok to reconsume directly
                     end;
                  else
                  begin
                     FCurrentToken.PrepareAttribute();
                     FCurrentToken.CurrentAttributeName.Append(FInputStream.CurrentCharacter);
                     FTokeniserState := tsAttributeNameState;
                  end;
               end;
            tsAfterAttributeNameState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020: ;
                  $002F: FTokeniserState := tsSelfClosingStartTagState;
                  $003D: FTokeniserState := tsBeforeAttributeValueState;
                  $003E:
                     begin
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  Ord('A')..Ord('Z'):
                     begin
                        FCurrentToken.PrepareAttribute();
                        FCurrentToken.CurrentAttributeName.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                        FTokeniserState := tsAttributeNameState;
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected U+0000 in attribute name'); {$ENDIF}
                        FCurrentToken.PrepareAttribute();
                        FCurrentToken.CurrentAttributeName.Append($FFFD); // $R-
                        FTokeniserState := tsAttributeNameState;
                     end;
                  $0022, $0027, $003C:
                     begin
                        {$IFDEF PARSEERROR} ParseError('invalid character starting attribute name'); {$ENDIF}
                        FCurrentToken.PrepareAttribute();
                        FCurrentToken.CurrentAttributeName.Append(FInputStream.CurrentCharacter);
                        FTokeniserState := tsAttributeNameState;
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in attribute name'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.RetractTag();
                        continue; // not emitting anything, so ok to reconsume directly
                     end;
                  else
                  begin
                     FCurrentToken.PrepareAttribute();
                     FCurrentToken.CurrentAttributeName.Append(FInputStream.CurrentCharacter);
                     FTokeniserState := tsAttributeNameState;
                  end;
               end;
            tsBeforeAttributeValueState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020: ;
                  $0022: FTokeniserState := tsAttributeValueDoubleQuotedState;
                  $0026:
                     begin
                        FTokeniserState := tsAttributeValueUnquotedState;
                        continue; // not emitting anything, so ok to reconsume directly
                     end;
                  $0027: FTokeniserState := tsAttributeValueSingleQuotedState;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected U+0000 in attribute value'); {$ENDIF}
                        FCurrentToken.CurrentAttributeValue.Append($FFFD);
                        FTokeniserState := tsAttributeValueUnquotedState;
                     end;
                  $003E:
                     begin
                        {$IFDEF PARSEERROR} ParseError('missing attribute value'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  $003C, $003D, $0060:
                     begin
                        {$IFDEF PARSEERROR} ParseError('invalid character in attribute value'); {$ENDIF}
                        FCurrentToken.CurrentAttributeValue.Append(FInputStream.CurrentCharacter);
                        FTokeniserState := tsAttributeValueUnquotedState;
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in attribute value'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.RetractTag();
                        continue; // not emitting anything, so ok to reconsume directly
                     end;
                  else
                     begin
                        FCurrentToken.CurrentAttributeValue.Append(FInputStream.CurrentCharacter);
                        FTokeniserState := tsAttributeValueUnquotedState;
                     end;
               end;
            // tsAttributeValueDoubleQuotedState was moved up
            tsAttributeValueSingleQuotedState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0027: FTokeniserState := tsAfterAttributeValueQuotedState;
                  $0026: if (ConsumeACharacterReferenceInAttribute($0027)) then continue;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected U+0000 in attribute value'); {$ENDIF}
                        FCurrentToken.CurrentAttributeValue.Append($FFFD);
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in attribute value'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.RetractTag();
                        continue; // not emitting anything, so ok to reconsume directly
                     end;
                  else
                     FCurrentToken.CurrentAttributeValue.Append(FInputStream.CurrentCharacter);
               end;
            tsAttributeValueUnquotedState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020: FTokeniserState := tsBeforeAttributeNameState;
                  $0026: if (ConsumeACharacterReferenceInAttribute($003E)) then continue;
                  $003E:
                     begin
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected U+0000 in attribute value'); {$ENDIF}
                        FCurrentToken.CurrentAttributeValue.Append($FFFD);
                     end;
                  $0022, $0027, $003C, $003D, $0060:
                     begin
                        {$IFDEF PARSEERROR} ParseError('invalid character in attribute value'); {$ENDIF}
                        FCurrentToken.CurrentAttributeValue.Append(FInputStream.CurrentCharacter);
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in attribute value'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.RetractTag();
                        continue; // not emitting anything, so ok to reconsume directly
                     end;
                  else
                     FCurrentToken.CurrentAttributeValue.Append(FInputStream.CurrentCharacter);
               end;
            tsAfterAttributeValueQuotedState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020: FTokeniserState := tsBeforeAttributeNameState;
                  $002F: FTokeniserState := tsSelfClosingStartTagState;
                  $003E:
                     begin
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in attribute value'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.RetractTag();
                        continue; // not emitting anything, so ok to reconsume directly
                     end;
                  else
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected character after attribute'); {$ENDIF}
                        FTokeniserState := tsBeforeAttributeNameState;
                        continue; // not emitting anything, so ok to reconsume directly
                     end;
               end;
            tsSelfClosingStartTagState:
               case (FInputStream.CurrentCharacter.Value) of
                  $003E:
                     begin
                        FCurrentToken.SelfClosingFlag := True;
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in start tag'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.RetractTag();
                        continue; // not emitting anything, so ok to reconsume directly
                     end;
                  else
                  begin
                     {$IFDEF PARSEERROR} ParseError('unexpected EOF in start tag'); {$ENDIF}
                     FTokeniserState := tsBeforeAttributeNameState;
                     continue; // not emitting anything, so ok to reconsume directly
                  end;
               end;
            tsBogusCommentState: BogusCommentState();
            tsMarkupDeclarationOpenState: MarkupDeclarationOpenState();
            tsCommentStartState:
               case (FInputStream.CurrentCharacter.Value) of
                  $002D:
                     begin
                        FTokeniserState := tsCommentStartDashState;
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected U+0000 in comment'); {$ENDIF}
                        FCurrentToken.CommentValue.Append($FFFD);
                        FTokeniserState := tsCommentState;
                     end;
                  $003E:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected <!--> form'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in comment'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                  begin
                     FCurrentToken.CommentValue.Append(FInputStream.CurrentCharacter);
                     FTokeniserState := tsCommentState;
                  end;
               end;
            tsCommentStartDashState:
               case (FInputStream.CurrentCharacter.Value) of
                  $002D:
                     begin
                        FTokeniserState := tsCommentEndState;
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected U+0000 in comment'); {$ENDIF}
                        FCurrentToken.CommentValue.Append($002D);
                        FCurrentToken.CommentValue.Append($FFFD);
                        FTokeniserState := tsCommentState;
                     end;
                  $003E:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected <!---> form'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in comment'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                  begin
                     FCurrentToken.CommentValue.Append($002D);
                     FCurrentToken.CommentValue.Append(FInputStream.CurrentCharacter);
                     FTokeniserState := tsCommentState;
                  end;
               end;
            // tsCommentState is earlier
            tsCommentEndDashState:
               case (FInputStream.CurrentCharacter.Value) of
                  $002D:
                     begin
                        FTokeniserState := tsCommentEndState;
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected U+0000 in comment'); {$ENDIF}
                        FCurrentToken.CommentValue.Append($002D);
                        FCurrentToken.CommentValue.Append($FFFD);
                        FTokeniserState := tsCommentState;
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in comment'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                  begin
                     FCurrentToken.CommentValue.Append($002D);
                     FCurrentToken.CommentValue.Append(FInputStream.CurrentCharacter);
                     FTokeniserState := tsCommentState;
                  end;
               end;
            tsCommentEndState:
               case (FInputStream.CurrentCharacter.Value) of
                  $003E:
                     begin
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected U+0000 in comment'); {$ENDIF}
                        FCurrentToken.CommentValue.Append($002D);
                        FCurrentToken.CommentValue.Append($002D);
                        FCurrentToken.CommentValue.Append($FFFD);
                        FTokeniserState := tsCommentState;
                     end;
                  $0021:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected ! after -- in comment'); {$ENDIF}
                        FTokeniserState := tsCommentEndBangState;
                     end;
                  $002D:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected --- in comment'); {$ENDIF}
                        FCurrentToken.CommentValue.Append($002D);
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in comment'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                  begin
                     {$IFDEF PARSEERROR} ParseError('unexpected -- in comment'); {$ENDIF}
                     FCurrentToken.CommentValue.Append($002D);
                     FCurrentToken.CommentValue.Append($002D);
                     FCurrentToken.CommentValue.Append(FInputStream.CurrentCharacter);
                     FTokeniserState := tsCommentState;
                  end;
               end;
            tsCommentEndBangState:
               case (FInputStream.CurrentCharacter.Value) of
                  $002D:
                     begin
                        FCurrentToken.CommentValue.Append($002D);
                        FCurrentToken.CommentValue.Append($002D);
                        FCurrentToken.CommentValue.Append($0021);
                        FTokeniserState := tsCommentEndDashState;
                     end;
                  $003E:
                     begin
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected U+0000 in comment'); {$ENDIF}
                        FCurrentToken.CommentValue.Append($002D);
                        FCurrentToken.CommentValue.Append($002D);
                        FCurrentToken.CommentValue.Append($0021);
                        FCurrentToken.CommentValue.Append($FFFD);
                        FTokeniserState := tsCommentState;
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in comment'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                  begin
                     FCurrentToken.CommentValue.Append($002D);
                     FCurrentToken.CommentValue.Append($002D);
                     FCurrentToken.CommentValue.Append($0021);
                     FCurrentToken.CommentValue.Append(FInputStream.CurrentCharacter);
                     FTokeniserState := tsCommentState;
                  end;
               end;
            tsDoctypeState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020: FTokeniserState := tsBeforeDoctypeNameState;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in DOCTYPE'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.PrepareDOCTYPE();
                        FCurrentToken.ForceQuirksFlag := True;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                  begin
                     {$IFDEF PARSEERROR} ParseError('unexpected character in DOCTYPE'); {$ENDIF}
                     FTokeniserState := tsBeforeDoctypeNameState;
                     continue; // not emitting anything, so ok to reconsume directly
                  end;
               end;
            tsBeforeDoctypeNameState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020: ;
                  Ord('A')..Ord('Z'):
                     begin
                        FCurrentToken.PrepareDOCTYPE();
                        FCurrentToken.DOCTYPENamePresent := True;
                        FCurrentToken.DOCTYPEName.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                        FTokeniserState := tsDoctypeNameState;
                     end;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected null in DOCTYPE name'); {$ENDIF}
                        FCurrentToken.PrepareDOCTYPE();
                        FCurrentToken.DOCTYPENamePresent := True;
                        FCurrentToken.DOCTYPEName.Append($FFFD);
                        FTokeniserState := tsDoctypeNameState;
                     end;
                  $003E:
                     begin
                        {$IFDEF PARSEERROR} ParseError('missing name in DOCTYPE'); {$ENDIF}
                        FCurrentToken.PrepareDOCTYPE();
                        FCurrentToken.ForceQuirksFlag := True;
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in DOCTYPE before name'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.PrepareDOCTYPE();
                        FCurrentToken.ForceQuirksFlag := True;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                  begin
                     FCurrentToken.PrepareDOCTYPE();
                     FCurrentToken.DOCTYPENamePresent := True;
                     FCurrentToken.DOCTYPEName.Append(FInputStream.CurrentCharacter);
                     FTokeniserState := tsDoctypeNameState;
                  end;
               end;
            tsDoctypeNameState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020: FTokeniserState := tsAfterDoctypeNameState;
                  $003E:
                     begin
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  Ord('A')..Ord('Z'): FCurrentToken.DOCTYPEName.Append(FInputStream.CurrentCharacter.Value + $0020); // $R-
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected null in DOCTYPE name'); {$ENDIF}
                        FCurrentToken.DOCTYPEName.Append($FFFD);
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF after DOCTYPE name'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.ForceQuirksFlag := True;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                     FCurrentToken.DOCTYPEName.Append(FInputStream.CurrentCharacter);
               end;
            tsAfterDoctypeNameState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020: ;
                  $003E:
                     begin
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF after DOCTYPE name'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.ForceQuirksFlag := True;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                  begin
                     if (SkipPUBLIC()) then
                     begin
                        FTokeniserState := tsAfterDoctypePublicKeywordState;
                     end
                     else
                     if (SkipSYSTEM()) then
                     begin
                        FTokeniserState := tsAfterDoctypeSystemKeywordState;
                     end
                     else
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected characters after DOCTYPE name'); {$ENDIF}
                        FCurrentToken.ForceQuirksFlag := True;
                        FTokeniserState := tsBogusDoctypeState;
                     end;
                  end;
               end;
            tsAfterDoctypePublicKeywordState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020: FTokeniserState := tsBeforeDoctypePublicIdentifierState;
                  $0022:
                     begin
                        {$IFDEF PARSEERROR} ParseError('missing whitespace in DOCTYPE after PUBLIC keyword'); {$ENDIF}
                        FCurrentToken.PublicIDPresent := True;
                        FTokeniserState := tsDoctypePublicIdentifierDoubleQuotedState;
                     end;
                  $0027:
                     begin
                        {$IFDEF PARSEERROR} ParseError('missing whitespace in DOCTYPE after PUBLIC keyword'); {$ENDIF}
                        FCurrentToken.PublicIDPresent := True;
                        FTokeniserState := tsDoctypePublicIdentifierSingleQuotedState;
                     end;
                  $003E:
                     begin
                        {$IFDEF PARSEERROR} ParseError('missing identifier in DOCTYPE after PUBLIC keyword'); {$ENDIF}
                        FCurrentToken.ForceQuirksFlag := True;
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in DOCTYPE after PUBLIC keyword'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.ForceQuirksFlag := True;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                  begin
                     {$IFDEF PARSEERROR} ParseError('unexpected characters in DOCTYPE after PUBLIC keyword'); {$ENDIF}
                     FCurrentToken.ForceQuirksFlag := True;
                     FTokeniserState := tsBogusDoctypeState;
                  end;
               end;
            tsBeforeDoctypePublicIdentifierState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020: ;
                  $0022:
                     begin
                        FCurrentToken.PublicIDPresent := True;
                        FTokeniserState := tsDoctypePublicIdentifierDoubleQuotedState;
                     end;
                  $0027:
                     begin
                        FCurrentToken.PublicIDPresent := True;
                        FTokeniserState := tsDoctypePublicIdentifierSingleQuotedState;
                     end;
                  $003E:
                     begin
                        {$IFDEF PARSEERROR} ParseError('missing identifier in DOCTYPE after PUBLIC keyword'); {$ENDIF}
                        FCurrentToken.ForceQuirksFlag := True;
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in DOCTYPE after PUBLIC keyword'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.ForceQuirksFlag := True;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                  begin
                     {$IFDEF PARSEERROR} ParseError('unexpected characters in DOCTYPE after PUBLIC keyword'); {$ENDIF}
                     FCurrentToken.ForceQuirksFlag := True;
                     FTokeniserState := tsBogusDoctypeState;
                  end;
               end;
            tsDoctypePublicIdentifierDoubleQuotedState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0022: FTokeniserState := tsAfterDoctypePublicIdentifierState;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected U+0000 in DOCTYPE public identifier'); {$ENDIF}
                        FCurrentToken.PublicID.Append($FFFD);
                     end;
                  $003E:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected end of DOCTYPE in public identifier'); {$ENDIF}
                        FCurrentToken.ForceQuirksFlag := True;
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in DOCTYPE in public identifier'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.ForceQuirksFlag := True;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                     FCurrentToken.PublicID.Append(FInputStream.CurrentCharacter);
               end;
            tsDoctypePublicIdentifierSingleQuotedState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0027: FTokeniserState := tsAfterDoctypePublicIdentifierState;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected U+0000 in DOCTYPE public identifier'); {$ENDIF}
                        FCurrentToken.PublicID.Append($FFFD);
                     end;
                  $003E:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected end of DOCTYPE in public identifier'); {$ENDIF}
                        FCurrentToken.ForceQuirksFlag := True;
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in DOCTYPE in public identifier'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.ForceQuirksFlag := True;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                     FCurrentToken.PublicID.Append(FInputStream.CurrentCharacter);
               end;
            tsAfterDoctypePublicIdentifierState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020: FTokeniserState := tsBetweenDoctypePublicAndSystemIdentifiersState;
                  $003E:
                     begin
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  $0022:
                     begin
                        {$IFDEF PARSEERROR} ParseError('missing whitespace in DOCTYPE after public identifier'); {$ENDIF}
                        FCurrentToken.SystemIDPresent := True;
                        FTokeniserState := tsDoctypeSystemIdentifierDoubleQuotedState;
                     end;
                  $0027:
                     begin
                        {$IFDEF PARSEERROR} ParseError('missing whitespace in DOCTYPE after public identifier'); {$ENDIF}
                        FCurrentToken.SystemIDPresent := True;
                        FTokeniserState := tsDoctypeSystemIdentifierSingleQuotedState;
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in DOCTYPE after public identifier'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.ForceQuirksFlag := True;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                  begin
                     {$IFDEF PARSEERROR} ParseError('unexpected characters in DOCTYPE after public identifier'); {$ENDIF}
                     FCurrentToken.ForceQuirksFlag := True;
                     FTokeniserState := tsBogusDoctypeState;
                  end;
               end;
            tsBetweenDoctypePublicAndSystemIdentifiersState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020: ;
                  $003E:
                     begin
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  $0022:
                     begin
                        FCurrentToken.SystemIDPresent := True;
                        FTokeniserState := tsDoctypeSystemIdentifierDoubleQuotedState;
                     end;
                  $0027:
                     begin
                        FCurrentToken.SystemIDPresent := True;
                        FTokeniserState := tsDoctypeSystemIdentifierSingleQuotedState;
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in DOCTYPE after public identifier'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.ForceQuirksFlag := True;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                  begin
                     {$IFDEF PARSEERROR} ParseError('unexpected characters in DOCTYPE after public identifier'); {$ENDIF}
                     FCurrentToken.ForceQuirksFlag := True;
                     FTokeniserState := tsBogusDoctypeState;
                  end;
               end;
            tsAfterDoctypeSystemKeywordState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020: FTokeniserState := tsBeforeDoctypeSystemIdentifierState;
                  $0022:
                     begin
                        {$IFDEF PARSEERROR} ParseError('missing whitespace in DOCTYPE after SYSTEM keyword'); {$ENDIF}
                        FCurrentToken.SystemIDPresent := True;
                        FTokeniserState := tsDoctypeSystemIdentifierDoubleQuotedState;
                     end;
                  $0027:
                     begin
                        {$IFDEF PARSEERROR} ParseError('missing whitespace in DOCTYPE after SYSTEM keyword'); {$ENDIF}
                        FCurrentToken.SystemIDPresent := True;
                        FTokeniserState := tsDoctypeSystemIdentifierSingleQuotedState;
                     end;
                  $003E:
                     begin
                        {$IFDEF PARSEERROR} ParseError('missing identifier in DOCTYPE after SYSTEM keyword'); {$ENDIF}
                        FCurrentToken.ForceQuirksFlag := True;
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in DOCTYPE after SYSTEM keyword'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.ForceQuirksFlag := True;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                  begin
                     {$IFDEF PARSEERROR} ParseError('unexpected characters in DOCTYPE after SYSTEM keyword'); {$ENDIF}
                     FCurrentToken.ForceQuirksFlag := True;
                     FTokeniserState := tsBogusDoctypeState;
                  end;
               end;
            tsBeforeDoctypeSystemIdentifierState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020: ;
                  $0022:
                     begin
                        FCurrentToken.SystemIDPresent := True;
                        FTokeniserState := tsDoctypeSystemIdentifierDoubleQuotedState;
                     end;
                  $0027:
                     begin
                        FCurrentToken.SystemIDPresent := True;
                        FTokeniserState := tsDoctypeSystemIdentifierSingleQuotedState;
                     end;
                  $003E:
                     begin
                        {$IFDEF PARSEERROR} ParseError('missing identifier in DOCTYPE after SYSTEM keyword'); {$ENDIF}
                        FCurrentToken.ForceQuirksFlag := True;
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in DOCTYPE after SYSTEM keyword'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.ForceQuirksFlag := True;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                  begin
                     {$IFDEF PARSEERROR} ParseError('unexpected characters in DOCTYPE after SYSTEM keyword'); {$ENDIF}
                     FCurrentToken.ForceQuirksFlag := True;
                     FTokeniserState := tsBogusDoctypeState;
                  end;
               end;
            tsDoctypeSystemIdentifierDoubleQuotedState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0022: FTokeniserState := tsAfterDoctypeSystemIdentifierState;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected U+0000 in DOCTYPE system identifier'); {$ENDIF}
                        FCurrentToken.SystemID.Append($FFFD);
                     end;
                  $003E:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected end of DOCTYPE in system identifier'); {$ENDIF}
                        FCurrentToken.ForceQuirksFlag := True;
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in DOCTYPE in system identifier'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.ForceQuirksFlag := True;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                     FCurrentToken.SystemID.Append(FInputStream.CurrentCharacter);
               end;
            tsDoctypeSystemIdentifierSingleQuotedState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0027: FTokeniserState := tsAfterDoctypeSystemIdentifierState;
                  $0000:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected U+0000 in DOCTYPE system identifier'); {$ENDIF}
                        FCurrentToken.SystemID.Append($FFFD);
                     end;
                  $003E:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected end of DOCTYPE in system identifier'); {$ENDIF}
                        FCurrentToken.ForceQuirksFlag := True;
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in DOCTYPE in system identifier'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.ForceQuirksFlag := True;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                     FCurrentToken.SystemID.Append(FInputStream.CurrentCharacter);
               end;
            tsAfterDoctypeSystemIdentifierState:
               case (FInputStream.CurrentCharacter.Value) of
                  $0009, $000A, $000C, $0020: ;
                  $003E:
                     begin
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  kEOF:
                     begin
                        {$IFDEF PARSEERROR} ParseError('unexpected EOF in DOCTYPE after system identifier'); {$ENDIF}
                        FTokeniserState := tsDataState;
                        FCurrentToken.ForceQuirksFlag := True;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
                  else
                  begin
                     {$IFDEF PARSEERROR} ParseError('unexpected characters in DOCTYPE after system identifier'); {$ENDIF}
                     FTokeniserState := tsBogusDoctypeState;
                  end;
               end;
            tsBogusDoctypeState:
               case (FInputStream.CurrentCharacter.Value) of
                  $003E:
                     begin
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                     end;
                  kEOF:
                     begin
                        FTokeniserState := tsDataState;
                        FCurrentToken.Emit();
                        FInputStream.Unconsume(); // can't just "continue" since we need to emit something first
                     end;
               end;
            tsCdataSectionState:
               begin
                  CdataSectionState(Reconsume);
                  if (Reconsume) then
                     continue;
               end;
            // less seen states
            tsInitialState:
               begin
                  FTokeniserState := tsDataState;
                  case (FInputStream.CurrentCharacter.Value) of
                     $FEFF: break; // consume next character
                     else continue; // reconsume
                  end;
               end;
            else Assert(False);
         end;
         break;
      until forever;
   until FCurrentToken.Ready;
   Assert(FCurrentToken.Ready);
   Assert(FCurrentToken.Kind <> tkNone);
end;

function THTMLParser.GetCurrentNode(): TElement;
begin
   Assert(FStackOfOpenElements.Length > 0);
   Result := FStackOfOpenElements.Last;
end;

function THTMLParser.GetAdjustedCurrentNode(): TElement;
begin
   if ((FStackOfOpenElements.Length = 1) and (FFragmentParsingMode)) then
   begin
      Result := FContextElement;
   end
   else
   begin
      Assert(FStackOfOpenElements.Length > 0);
      Result := FStackOfOpenElements.Last;
   end;
end;

procedure THTMLParser.InsertNodeAtAppropriatePlaceForInsertingANode(const Node: TNode; Target: TElement = nil);
var
   LastTable: TElement;
   Index: Cardinal;
begin
   Assert(FStackOfOpenElements.Length > 0);
   Assert(Assigned(Node));
   Assert(not (Node is TDocument));
   Assert(not (Node is TText));
   if (not Assigned(Target)) then
      Target := CurrentNode;
   if (FFosterParenting and Target.HasProperties(propHTML or propFosterParent)) then
   begin
      Index := FStackOfOpenElements.Length;
      Assert(Index > 1);
      repeat
         Dec(Index);
         if (FStackOfOpenElements[Index].IsIdentity(nsHTML, eTemplate)) then
         begin
            FStackOfOpenElements[Index].AppendChild(Node);
            exit;
         end
         else
         if (FStackOfOpenElements[Index].IsIdentity(nsHTML, eTable)) then
         begin
            LastTable := FStackOfOpenElements[Index];
            if (Assigned(LastTable.ParentNode)) then
            begin
               if (LastTable.ParentNode is TElement) then
               begin
                  (LastTable.ParentNode as TElement).InsertBefore(Node, LastTable);
               end;
            end
            else
            begin
               Assert(Index > 0);
               FStackOfOpenElements[Index-1].AppendChild(Node); // $R-
            end;
            exit;
         end;
      until (Index = 1);
      FStackOfOpenElements[0].AppendChild(Node);
   end
   else
   begin
      Target.AppendChild(Node);
   end;
end;

function THTMLParser.CreateAnElementFor(constref Token: TToken; constref Namespace: TCanonicalString): TElement;
var
   CachedAttributeValue: UTF8String;
begin
   Assert(Token.Kind in [tkStartTag, tkEndTag]);
   if (Namespace = nsHTML) then
      Result := ConstructHTMLElement(Token.TagName, Token.TakeAttributes())
   else
   if (Namespace = nsMathML) then
   begin
      Result := ConstructMathMLElement(Token.TagName, Token.TakeAttributes());
      if (Result.LocalName = eAnnotationXML) then
      begin
         CachedAttributeValue := ASCIILowerCase(Result.GetAttribute('encoding').AsString);
         if ((CachedAttributeValue = 'text/html') or (CachedAttributeValue = 'application/xhtml+xml')) then
            Result.AddProperty(propHTMLIntegrationPoint);
      end;
   end
   else
   if (Namespace = nsSVG) then
      Result := ConstructSVGElement(Token.TagName, Token.TakeAttributes())
   else
      Result := TElement.Create(Namespace, Token.TagName, Token.TakeAttributes());
   Assert(Assigned(Result));
end;

function THTMLParser.InsertAForeignElementFor(constref Token: TToken; constref Namespace: TCanonicalString): TElement;
begin
   Result := CreateAnElementFor(Token, Namespace);
   InsertNodeAtAppropriatePlaceForInsertingANode(Result);
   FStackOfOpenElements.Push(Result);
end;

function THTMLParser.InsertAForeignElement(const Element: TElement): TElement;
begin
   Result := Element;
   InsertNodeAtAppropriatePlaceForInsertingANode(Element);
   FStackOfOpenElements.Push(Element);
end;

function THTMLParser.InsertAnHTMLElementFor(constref Token: TToken): TElement;
begin
   Result := InsertAForeignElementFor(Token, nsHTML);
end;

function THTMLParser.InsertAnHTMLElement(const Element: TElement): TElement;
begin
   Result := InsertAForeignElement(Element);
end;

// WARNING, THIS PROCEDURE IS EXACTLY THE SAME AS THE NEXT ONE, EDIT WITH CARE
procedure THTMLParser.InsertCharacters(var Data: TCutParserString);
var
   Index: Cardinal;
   Container: TElement;
begin
   {$IFDEF VERBOSETEXT} if (DebugNow) then Writeln('InsertCharacters(''', Data, ''')'); {$ENDIF}
   Assert(not Data.IsEmpty);
   Assert(FStackOfOpenElements.Length > 0);
   if (FFosterParenting and CurrentNode.HasProperties(propHTML or propFosterParent)) then
   begin
      {$IFDEF VERBOSETEXT} if (DebugNow) then Writeln('   Foster Parenting...'); {$ENDIF}
      Container := nil;
      Index := FStackOfOpenElements.Length;
      Assert(Index > 1);
      repeat
         Dec(Index);
         if (FStackOfOpenElements[Index].IsIdentity(nsHTML, eTemplate)) then
         begin
            Container := FStackOfOpenElements[Index];
            break;
         end
         else
         if (FStackOfOpenElements[Index].IsIdentity(nsHTML, eTable)) then
         begin
            if (Assigned(FStackOfOpenElements[Index].ParentNode)) then
            begin
               Container := FStackOfOpenElements[Index];
               if (Container.ParentNode is TElement) then
               begin
                  if (Assigned(Container.PreviousSibling) and (Container.PreviousSibling is TText)) then
                     (Container.PreviousSibling as TText).AppendDataDestructively(Data)
                  else
                     (Container.ParentNode as TElement).InsertBefore(TText.CreateDestructively(Data), Container);
               end;
               // else it's a TDocument, do nothing
               exit;
            end
            else
            begin
               break;
            end;
         end;
      until (Index = 1);
      Assert(Index > 0);
      if (not Assigned(Container)) then
         Container := FStackOfOpenElements[Index-1]; // $R-
   end
   else
   begin
      Container := CurrentNode;
   end;
   {$IFDEF VERBOSETEXT}
      if (DebugNow) then
      begin
         Writeln('   Going to insert in ', Container.LocalName.AsString, ', whose parent is ', (Container.ParentNode as TElement).LocalName.AsString, ' and it has:');
         for Index := 0 to (Container.ParentNode as TElement).ChildNodes.Length-1 do
         begin
            if ((Container.ParentNode as TElement).ChildNodes[Index] is TElement) then
               Writeln('     container child ', Index, ' <', ((Container.ParentNode as TElement).ChildNodes[Index] as TElement).LocalName.AsString, '>')
            else
            if ((Container.ParentNode as TElement).ChildNodes[Index] is TText) then
               Writeln('     container child ', Index, ' "', ((Container.ParentNode as TElement).ChildNodes[Index] as TText).Data, '"')
            else
               Writeln('     container child ', Index, ' ', (Container.ParentNode as TElement).ChildNodes[Index].ClassName);
         end;
      end;
   {$ENDIF}
   Assert(Assigned(Container));
   if (Assigned(Container.LastChild) and (Container.LastChild is TText)) then
   begin
      {$IFDEF VERBOSETEXT} if (DebugNow) then Writeln('   Going to append to the text node at the end of that node.'); {$ENDIF}
      (Container.LastChild as TText).AppendDataDestructively(Data)
   end
   else
   begin
      {$IFDEF VERBOSETEXT} if (DebugNow) then Writeln('   Appending a new text node at the end of that node.'); {$ENDIF}
      Container.AppendChild(TText.CreateDestructively(Data));
   end;
   {$IFDEF VERBOSETEXT}
      if (DebugNow) then
      begin
         Writeln('   Ok, the container now has the following children:');
         for Index := 0 to Container.ChildNodes.Length-1 do
         begin
            if (Container.ChildNodes[Index] is TElement) then
               Writeln('     container child ', Index, ' <', (Container.ChildNodes[Index] as TElement).LocalName.AsString, '>')
            else
            if (Container.ChildNodes[Index] is TText) then
               Writeln('     container child ', Index, ' "', (Container.ChildNodes[Index] as TText).Data, '"')
            else
               Writeln('     container child ', Index, ' ', Container.ChildNodes[Index].ClassName);
         end;
      end;
      if (DebugNow) then
         Writeln('   DOM is now:'#10, SerialiseDOMForTestOutput(FDocument));
   {$ENDIF}
end;

// WARNING, THIS PROCEDURE IS EXACTLY THE SAME AS THE PREVIOUS ONE, EDIT WITH CARE
{$IFDEF USEROPES}
procedure THTMLParser.InsertCharacters(var Data: TParserString);
var
   Index: Cardinal;
   Container: TElement;
begin
   {$IFDEF VERBOSETEXT} if (DebugNow) then Writeln('InsertCharacters(''', Data, ''')'); {$ENDIF}
   Assert(not Data.IsEmpty);
   Assert(FStackOfOpenElements.Length > 0);
   if (FFosterParenting and CurrentNode.HasProperties(propHTML or propFosterParent)) then
   begin
      {$IFDEF VERBOSETEXT} if (DebugNow) then Writeln('   Foster Parenting...'); {$ENDIF}
      Container := nil;
      Index := FStackOfOpenElements.Length;
      Assert(Index > 1);
      repeat
         Dec(Index);
         if (FStackOfOpenElements[Index].IsIdentity(nsHTML, eTemplate)) then
         begin
            Container := FStackOfOpenElements[Index];
            break;
         end
         else
         if (FStackOfOpenElements[Index].IsIdentity(nsHTML, eTable)) then
         begin
            if (Assigned(FStackOfOpenElements[Index].ParentNode)) then
            begin
               Container := FStackOfOpenElements[Index];
               if (Container.ParentNode is TElement) then
               begin
                  if (Assigned(Container.PreviousSibling) and (Container.PreviousSibling is TText)) then
                     (Container.PreviousSibling as TText).AppendDataDestructively(Data)
                  else
                     (Container.ParentNode as TElement).InsertBefore(TText.CreateDestructively(Data), Container);
               end;
               // else it's a TDocument, do nothing
               exit;
            end
            else
            begin
               break;
            end;
         end;
      until (Index = 1);
      Assert(Index > 0);
      if (not Assigned(Container)) then
         Container := FStackOfOpenElements[Index-1]; // $R-
   end
   else
   begin
      Container := CurrentNode;
   end;
   {$IFDEF VERBOSETEXT}
      if (DebugNow) then
      begin
         Writeln('   Going to insert in ', Container.LocalName.AsString, ', whose parent is ', (Container.ParentNode as TElement).LocalName.AsString, ' and it has:');
         for Index := 0 to (Container.ParentNode as TElement).ChildNodes.Length-1 do
         begin
            if ((Container.ParentNode as TElement).ChildNodes[Index] is TElement) then
               Writeln('     container child ', Index, ' <', ((Container.ParentNode as TElement).ChildNodes[Index] as TElement).LocalName.AsString, '>')
            else
            if ((Container.ParentNode as TElement).ChildNodes[Index] is TText) then
               Writeln('     container child ', Index, ' "', ((Container.ParentNode as TElement).ChildNodes[Index] as TText).Data, '"')
            else
               Writeln('     container child ', Index, ' ', (Container.ParentNode as TElement).ChildNodes[Index].ClassName);
         end;
      end;
   {$ENDIF}
   Assert(Assigned(Container));
   if (Assigned(Container.LastChild) and (Container.LastChild is TText)) then
   begin
      {$IFDEF VERBOSETEXT} if (DebugNow) then Writeln('   Going to append to the text node at the end of that node.'); {$ENDIF}
      (Container.LastChild as TText).AppendDataDestructively(Data)
   end
   else
   begin
      {$IFDEF VERBOSETEXT} if (DebugNow) then Writeln('   Appending a new text node at the end of that node.'); {$ENDIF}
      Container.AppendChild(TText.CreateDestructively(Data));
   end;
   {$IFDEF VERBOSETEXT}
      if (DebugNow) then
      begin
         Writeln('   Ok, the container now has the following children:');
         for Index := 0 to Container.ChildNodes.Length-1 do
         begin
            if (Container.ChildNodes[Index] is TElement) then
               Writeln('     container child ', Index, ' <', (Container.ChildNodes[Index] as TElement).LocalName.AsString, '>')
            else
            if (Container.ChildNodes[Index] is TText) then
               Writeln('     container child ', Index, ' "', (Container.ChildNodes[Index] as TText).Data, '"')
            else
               Writeln('     container child ', Index, ' ', Container.ChildNodes[Index].ClassName);
         end;
      end;
      if (DebugNow) then
         Writeln('   DOM is now:'#10, SerialiseDOMForTestOutput(FDocument));
   {$ENDIF}
end;
{$ENDIF}

procedure THTMLParser.InsertCharacters(const Data: UTF8String);
var
   Placeholder: TCutParserString;
begin
   Placeholder := TCutParserString.CreateFrom(Data);
   InsertCharacters(Placeholder);
   Assert(Placeholder.IsEmpty);
end;

procedure THTMLParser.InsertCharacters(const Data: TUnicodeCodepointArray);
var
   Placeholder: TCutParserString;
begin
   Placeholder := TCutParserString.CreateFrom(Data);
   InsertCharacters(Placeholder);
   Assert(Placeholder.IsEmpty);
end;

procedure THTMLParser.InsertCharactersFor(constref Token: TToken);
var
   Placeholder: TCutParserString;
begin
   Assert(Token.Kind = tkSourceCharacters);
   Placeholder := Token.ExtractSourceCharacters(FInputStream.Data);
   InsertCharacters(Placeholder);
end;

procedure THTMLParser.InsertLeadingSpacesFor(var Token: TToken);
var
   Placeholder: TCutParserString;
begin
   Assert(Token.Kind = tkSourceCharacters);
   Placeholder := Token.ExtractLeadingSpaces(FInputStream.Data);
   InsertCharacters(Placeholder);
end;

function THTMLParser.CreateACommentFor(var Token: TToken): TComment;
begin
   Result := TComment.CreateDestructively(Token.CommentValue); // http://bugs.freepascal.org/view.php?id=26403
end;

procedure THTMLParser.InsertAComment(var Token: TToken);
begin
   InsertNodeAtAppropriatePlaceForInsertingANode(CreateACommentFor(Token)); // http://bugs.freepascal.org/view.php?id=26403
end;

procedure THTMLParser.ResetTheInsertionModeAppropriately();
var
   Last: Boolean;
   Node, Ancestor: TElement;
   NodeIndex, AncestorIndex: Cardinal;
begin
   Assert(FStackOfOpenElements.Length > 0);
   Last := False;
   NodeIndex := FStackOfOpenElements.Length;
   repeat
      Dec(NodeIndex);
      if (NodeIndex = 0) then
      begin
         Last := True;
         // XXX could make this go faster by making the root <html> the FContextElement when not FFragmentParsingMode
         if (FFragmentParsingMode) then
            Node := FContextElement
         else
            Node := FStackOfOpenElements[NodeIndex];
      end
      else
      begin
         Node := FStackOfOpenElements[NodeIndex];
      end;
      Assert(Assigned(Node));
      if (Node.NamespaceURL = nsHTML) then
      begin
         if (Node.LocalName = eSelect) then
         begin
            if (not Last) then
            begin
               AncestorIndex := NodeIndex;
               while (AncestorIndex > 0) do
               begin
                  Dec(AncestorIndex);
                  Ancestor := FStackOfOpenElements[AncestorIndex];
                  if (Ancestor.NamespaceURL = nsHTML) then
                  begin
                     if (Ancestor.LocalName = eTemplate) then
                        break;
                     if (Ancestor.LocalName = eTable) then
                     begin
                        FInsertionMode := @TheInSelectInTableInsertionMode;
                        exit;
                     end;
                  end;
               end;
            end;
            FInsertionMode := @TheInSelectInsertionMode;
            exit;
         end
         else
         if (Node.HasProperties(propTableCell)) then // td, th
         begin
            if (not Last) then
            begin
               FInsertionMode := @TheInCellInsertionMode;
               exit;
            end;
         end
         else
         if (Node.LocalName = eTR) then
         begin
            FInsertionMode := @TheInRowInsertionMode;
            exit;
         end
         else
         if (Node.HasProperties(propTableSection)) then // tbody, thead, tfoot
         begin
            FInsertionMode := @TheInTableBodyInsertionMode;
            exit;
         end
         else
         if (Node.LocalName = eCaption) then
         begin
            FInsertionMode := @TheInCaptionInsertionMode;
            exit;
         end
         else
         if (Node.LocalName = eColGroup) then
         begin
            FInsertionMode := @TheInColumnGroupInsertionMode;
            exit;
         end
         else
         if (Node.LocalName = eTable) then
         begin
            FInsertionMode := @TheInTableInsertionMode;
            exit;
         end
         else
         if (Node.LocalName = eTemplate) then
         begin
            FInsertionMode := FStackOfTemplateInsertionModes.Last;
            exit;
         end
         else
         if (Node.LocalName = eHead) then
         begin
            if (not Last) then
            begin
               FInsertionMode := @TheInHeadInsertionMode;
               exit;
            end;
         end
         else
         if (Node.LocalName = eBody) then
         begin
            FInsertionMode := @TheInBodyInsertionMode;
            exit;
         end
         else
         if (Node.LocalName = eFrameset) then
         begin
            FInsertionMode := @TheInFramesetInsertionMode;
            exit;
         end
         else
         if (Node.LocalName = eHTML) then
         begin
            if (not Assigned(FHeadElementPointer)) then
               FInsertionMode := @TheBeforeHeadInsertionMode
            else
               FInsertionMode := @TheAfterHeadInsertionMode;
            exit;
         end;
      end;
   until Last;
   FInsertionMode := @TheInBodyInsertionMode;
end;

procedure THTMLParser.GenericRCDataElementParsingAlgorithm(constref Token: TToken);
begin
   InsertAnHTMLElementFor(Token);
   FTokeniserState := tsRcdataState;
   FOriginalInsertionMode := FInsertionMode;
   FInsertionMode := @TheTextInsertionMode;
end;

procedure THTMLParser.GenericRawTextElementParsingAlgorithm(constref Token: TToken);
begin
   InsertAnHTMLElementFor(Token);
   FTokeniserState := tsRawtextState;
   FOriginalInsertionMode := FInsertionMode;
   FInsertionMode := @TheTextInsertionMode;
end;

procedure THTMLParser.GenerateImpliedEndTags();
begin
   while (CurrentNode.HasProperties(propBodyImpliedEndTag)) do
      FStackOfOpenElements.Pop();
end;

procedure THTMLParser.GenerateAllImpliedEndTagsThoroughly();
begin
   while (CurrentNode.HasProperties(propTemplateImpliedEndTag)) do
      FStackOfOpenElements.Pop();
end;

procedure THTMLParser.GenerateImpliedEndTagsExceptFor(constref Exception: TCanonicalString);
begin
   while (CurrentNode.HasProperties(propBodyImpliedEndTag) and not CurrentNode.IsIdentity(nsHTML, Exception)) do
      FStackOfOpenElements.Pop();
end;

procedure THTMLParser.TheInitialInsertionMode(var Token: TToken);
var
   RawPublicID, RawSystemID: UTF8String;
   ConvertedPublicID, ConvertedSystemID: UTF8String;

   function PublicIDIsQuirkyID(): Boolean;
   begin
      Assert(Token.PublicIDPresent);
      Result := ((ConvertedPublicID = '-//w3o//dtd w3 html strict 3.0//en//') or
                 (ConvertedPublicID = '-/w3c/dtd html 4.0 transitional/en') or
                 (ConvertedPublicID = 'html') or
                 PrefixMatch(ConvertedPublicID, '+//silmaril//dtd html pro v0r11 19970101//') or
                 PrefixMatch(ConvertedPublicID, '-//advasoft ltd//dtd html 3.0 aswedit + extensions//') or
                 PrefixMatch(ConvertedPublicID, '-//as//dtd html 3.0 aswedit + extensions//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html 2.0 level 1//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html 2.0 level 2//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html 2.0 strict level 1//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html 2.0 strict level 2//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html 2.0 strict//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html 2.0//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html 2.1e//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html 3.0//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html 3.2 final//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html 3.2//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html 3//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html level 0//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html level 1//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html level 2//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html level 3//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html strict level 0//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html strict level 1//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html strict level 2//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html strict level 3//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html strict//') or
                 PrefixMatch(ConvertedPublicID, '-//ietf//dtd html//') or
                 PrefixMatch(ConvertedPublicID, '-//metrius//dtd metrius presentational//') or
                 PrefixMatch(ConvertedPublicID, '-//microsoft//dtd internet explorer 2.0 html strict//') or
                 PrefixMatch(ConvertedPublicID, '-//microsoft//dtd internet explorer 2.0 html//') or
                 PrefixMatch(ConvertedPublicID, '-//microsoft//dtd internet explorer 2.0 tables//') or
                 PrefixMatch(ConvertedPublicID, '-//microsoft//dtd internet explorer 3.0 html strict//') or
                 PrefixMatch(ConvertedPublicID, '-//microsoft//dtd internet explorer 3.0 html//') or
                 PrefixMatch(ConvertedPublicID, '-//microsoft//dtd internet explorer 3.0 tables//') or
                 PrefixMatch(ConvertedPublicID, '-//netscape comm. corp.//dtd html//') or
                 PrefixMatch(ConvertedPublicID, '-//netscape comm. corp.//dtd strict html//') or
                 PrefixMatch(ConvertedPublicID, '-//o''reilly and associates//dtd html 2.0//') or
                 PrefixMatch(ConvertedPublicID, '-//o''reilly and associates//dtd html extended 1.0//') or
                 PrefixMatch(ConvertedPublicID, '-//o''reilly and associates//dtd html extended relaxed 1.0//') or
                 PrefixMatch(ConvertedPublicID, '-//softquad software//dtd hotmetal pro 6.0::19990601::extensions to html 4.0//') or
                 PrefixMatch(ConvertedPublicID, '-//softquad//dtd hotmetal pro 4.0::19971010::extensions to html 4.0//') or
                 PrefixMatch(ConvertedPublicID, '-//spyglass//dtd html 2.0 extended//') or
                 PrefixMatch(ConvertedPublicID, '-//sq//dtd html 2.0 hotmetal + extensions//') or
                 PrefixMatch(ConvertedPublicID, '-//sun microsystems corp.//dtd hotjava html//') or
                 PrefixMatch(ConvertedPublicID, '-//sun microsystems corp.//dtd hotjava strict html//') or
                 PrefixMatch(ConvertedPublicID, '-//w3c//dtd html 3 1995-03-24//') or
                 PrefixMatch(ConvertedPublicID, '-//w3c//dtd html 3.2 draft//') or
                 PrefixMatch(ConvertedPublicID, '-//w3c//dtd html 3.2 final//') or
                 PrefixMatch(ConvertedPublicID, '-//w3c//dtd html 3.2//') or
                 PrefixMatch(ConvertedPublicID, '-//w3c//dtd html 3.2s draft//') or
                 PrefixMatch(ConvertedPublicID, '-//w3c//dtd html 4.0 frameset//') or
                 PrefixMatch(ConvertedPublicID, '-//w3c//dtd html 4.0 transitional//') or
                 PrefixMatch(ConvertedPublicID, '-//w3c//dtd html experimental 19960712//') or
                 PrefixMatch(ConvertedPublicID, '-//w3c//dtd html experimental 970421//') or
                 PrefixMatch(ConvertedPublicID, '-//w3c//dtd w3 html//') or
                 PrefixMatch(ConvertedPublicID, '-//w3o//dtd w3 html 3.0//') or
                 PrefixMatch(ConvertedPublicID, '-//webtechs//dtd mozilla html 2.0//') or
                 PrefixMatch(ConvertedPublicID, '-//webtechs//dtd mozilla html//') or
                 ((not Token.SystemIDPresent) and
                     (PrefixMatch(ConvertedPublicID, '-//w3c//dtd html 4.01 frameset//') or
                     (PrefixMatch(ConvertedPublicID, '-//w3c//dtd html 4.01 transitional//')))));
   end;

   function SystemIDIsQuirkyID(): Boolean;
   begin
      Assert(Token.SystemIDPresent);
      Result := ConvertedSystemID = 'http://www.ibm.com/data/dtd/v11/ibmxhtml1-transitional.dtd';
   end;

   function PublicIDIsLimitedQuirkyID(): Boolean;
   begin
      Assert(Token.PublicIDPresent);
      Result := (PrefixMatch(ConvertedPublicID, '-//w3c//dtd xhtml 1.0 frameset//') or
                 PrefixMatch(ConvertedPublicID, '-//w3c//dtd xhtml 1.0 transitional//') or
                 (Token.SystemIDPresent and
                    (PrefixMatch(ConvertedPublicID, '-//w3c//dtd html 4.01 frameset//') or
                    (PrefixMatch(ConvertedPublicID, '-//w3c//dtd html 4.01 transitional//')))));
   end;

var
   SelectedDocumentMode: TDocument.TDocumentMode;
   DOCTYPENameIsHTML: Boolean;
begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkSourceCharacters:
         begin
            if (ssHaveNonSpace in Token.SpaceState) then
            begin
               if (ssHaveLeadingSpace in Token.SpaceState) then
                  Token.SkipLeadingSpaces(FInputStream.Data); // ignore spaces
               // fall through to "anything else" clause
            end
            else
               exit; // ignore spaces
         end;
      tkExtraSpaceCharacter: exit; // ignore spaces
      tkComment:
         begin
            FDocument.AppendChild(CreateACommentFor(Token));
            exit;
         end;
      tkDOCTYPE:
         begin
            Assert(Token.DOCTYPENamePresent xor (Token.DOCTYPEName.IsEmpty));
            Assert(Token.PublicIDPresent or (Token.PublicID.IsEmpty));
            Assert(Token.SystemIDPresent or (Token.SystemID.IsEmpty));
            DOCTYPENameIsHTML := Token.DOCTYPENamePresent and (Token.DOCTYPEName.AsString = 'html');
            RawPublicID := Token.PublicID.AsString;
            ConvertedPublicID := ASCIILowerCase(RawPublicID);
            RawSystemID := Token.SystemID.AsString;
            ConvertedSystemID := ASCIILowerCase(RawSystemID);
            {$IFDEF PARSEERROR}
               if (((not DOCTYPENameIsHTML) or
                    (Token.PublicIDPresent) or
                    (Token.SystemIDPresent and (RawSystemID <> 'about:legacy-compat'))) and not
                   ((DOCTYPENameIsHTML and (RawPublicID = '-//W3C//DTD HTML 4.0//EN') and ((not Token.SystemIDPresent) or (RawSystemID = 'http://www.w3.org/TR/REC-html40/strict.dtd'))) or
                    (DOCTYPENameIsHTML and (RawPublicID = '-//W3C//DTD HTML 4.01//EN') and ((not Token.SystemIDPresent) or (RawSystemID = 'http://www.w3.org/TR/html4/strict.dtd'))) or
                    (DOCTYPENameIsHTML and (RawPublicID = '-//W3C//DTD XHTML 1.0 Strict//EN') and (RawSystemID = 'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd')) or
                    (DOCTYPENameIsHTML and (RawPublicID = '-//W3C//DTD XHTML 1.1//EN') and (RawSystemID = 'http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd')))) then
                  ParseError('bad DOCTYPE');
            {$ENDIF}
            if ((Token.ForceQuirksFlag) or
                (not DOCTYPENameIsHTML) or
                (Token.PublicIDPresent and PublicIDIsQuirkyID()) or
                (Token.SystemIDPresent and SystemIDIsQuirkyID())) then
               SelectedDocumentMode := dmQuirksMode
            else
            if (Token.PublicIDPresent and PublicIDIsLimitedQuirkyID()) then
               SelectedDocumentMode := dmLimitedQuirksMode
            else
               SelectedDocumentMode := dmNoQuirksMode;
            FDocument.AppendChild(TDocumentType.CreateDestructively(Token.DOCTYPEName, Token.PublicID, Token.SystemID));
            FDocument.SetDocumentMode(SelectedDocumentMode);
            FInsertionMode := @TheBeforeHTMLInsertionMode;
            exit;
         end;
   end;
   // anything else:
   {$IFDEF PARSEERROR} ParseError('missing DOCTYPE'); {$ENDIF}
   FDocument.SetDocumentMode(dmQuirksMode);
   FInsertionMode := @TheBeforeHTMLInsertionMode;
   TreeConstructionDispatcher(Token);
end;

procedure THTMLParser.TheBeforeHTMLInsertionMode(var Token: TToken);
var
   Element: TElement;
begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkDOCTYPE:
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected DOCTYPE'); {$ENDIF}
            exit;
         end;
      tkComment:
         begin
            FDocument.AppendChild(CreateACommentFor(Token)); // http://bugs.freepascal.org/view.php?id=26403
            exit;
         end;
      tkSourceCharacters:
         begin
            if (ssHaveNonSpace in Token.SpaceState) then
            begin
               if (ssHaveLeadingSpace in Token.SpaceState) then
                  Token.SkipLeadingSpaces(FInputStream.Data); // ignore spaces
               // fall through to "anything else" clause
            end
            else
               exit; // ignore spaces
         end;
      tkExtraSpaceCharacter: exit; // ignore spaces
      tkStartTag:
         if (Token.TagName = eHTML) then
         begin
            Element := CreateAnElementFor(Token, nsHTML);
            FDocument.AppendChild(Element);
            FStackOfOpenElements.Push(Element);
            FInsertionMode := @TheBeforeHeadInsertionMode;
            exit;
         end;
      tkEndTag:
         if ((Token.TagName <> eHead) and (Token.TagName <> eBody) and (Token.TagName <> eHTML) and (Token.TagName <> eBR)) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected end tag'); {$ENDIF}
            exit;
         end;
   end;
   // anything else
   Element := ConstructHTMLElement(eHTML);
   FDocument.AppendChild(Element);
   FStackOfOpenElements.Push(Element);
   FInsertionMode := @TheBeforeHeadInsertionMode;
   TreeConstructionDispatcher(Token);
end;

procedure THTMLParser.TheBeforeHeadInsertionMode(var Token: TToken);
begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkSourceCharacters:
         begin
            if (ssHaveNonSpace in Token.SpaceState) then
            begin
               if (ssHaveLeadingSpace in Token.SpaceState) then
                  Token.SkipLeadingSpaces(FInputStream.Data); // ignore spaces
               // fall through to "anything else" clause
            end
            else
               exit; // ignore spaces
         end;
      tkExtraSpaceCharacter: exit; // ignore spaces
      tkComment:
         begin
            InsertAComment(Token);
            exit;
         end;
      tkDOCTYPE:
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected DOCTYPE'); {$ENDIF}
            exit;
         end;
      tkStartTag:
         if (Token.TagName = eHTML) then
         begin
            TheInBodyInsertionMode(Token);
            exit;
         end
         else
         if (Token.TagName = eHead) then
         begin
            FHeadElementPointer := InsertAnHTMLElementFor(Token);
            FInsertionMode := @TheInHeadInsertionMode;
            exit;
         end;
      tkEndTag:
         if ((Token.TagName <> eHead) and (Token.TagName <> eBody) and (Token.TagName <> eHTML) and (Token.TagName <> eBR)) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected end tag'); {$ENDIF}
            exit;
         end;
   end;
   // anything else
   FHeadElementPointer := InsertAnHTMLElement(ConstructHTMLElement(eHead));
   FInsertionMode := @TheInHeadInsertionMode;
   TreeConstructionDispatcher(Token);
end;

procedure THTMLParser.TheInHeadInsertionMode(var Token: TToken);
begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkSourceCharacters:
         begin
            if (ssHaveNonSpace in Token.SpaceState) then
            begin
               if (ssHaveLeadingSpace in Token.SpaceState) then
                  InsertLeadingSpacesFor(Token); // http://bugs.freepascal.org/view.php?id=26403
               // fall through to "anything else" clause
            end
            else
            begin
               InsertCharactersFor(Token);
               exit;
            end;
         end;
      tkExtraSpaceCharacter:
         begin
            InsertCharacters(Token.ExtraChars);
            exit;
         end;
      tkComment:
         begin
            InsertAComment(Token);
            exit;
         end;
      tkDOCTYPE:
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected DOCTYPE'); {$ENDIF}
            exit;
         end;
      tkStartTag:
         if (Token.TagName = eHTML) then
         begin
            TheInBodyInsertionMode(Token);
            exit;
         end
         else
         if ((Token.TagName = eBase) or (Token.TagName = eBaseFont) or (Token.TagName = eBGSound) or (Token.TagName = eLink) or
             (Token.TagName = eMeta)) then // <meta>'s special encoding logic is not supported
         begin
            InsertAnHTMLElementFor(Token);
            FStackOfOpenElements.Pop();
            {$IFDEF PARSEERROR} Token.AcknowledgeSelfClosingFlag(); {$ENDIF}
            exit;
         end
         else
         if (Token.TagName = eTitle) then
         begin
            GenericRCDataElementParsingAlgorithm(Token);
            exit;
         end
         else
         if ((Token.TagName = eNoFrames) or (Token.TagName = eStyle) or ((Token.TagName = eNoScript) and (FScriptingFlag))) then
         begin
            GenericRawTextElementParsingAlgorithm(Token);
            exit;
         end
         else
         if ((Token.TagName = eNoScript) and (not FScriptingFlag)) then
         begin
            InsertAnHTMLElementFor(Token);
            FInsertionMode := @TheInHeadNoScriptInsertionMode;
            exit;
         end
         else
         if (Token.TagName = eTemplate) then
         begin
            InsertAnHTMLElementFor(Token);
            InsertMarkerAtEndOfListOfActiveFormattingElements();
            FFramesetOkFlag := False;
            FInsertionMode := @TheInTemplateInsertionMode;
            FStackOfTemplateInsertionModes.Push(@TheInTemplateInsertionMode);
            exit;
         end
         else
         if (Token.TagName = eScript) then
         begin
            // script execution is not supported, so script-specific logic is skipped
            // this means it simplifies down to just the following line:
            InsertAnHTMLElementFor(Token);
            FTokeniserState := tsScriptDataState;
            FOriginalInsertionMode := FInsertionMode;
            FInsertionMode := @TheTextInsertionMode;
            exit;
         end
         else
         if (Token.TagName = eHead) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected head start tag'); {$ENDIF}
            exit;
         end;
      tkEndTag:
         if (Token.TagName = eHead) then
         begin
            FStackOfOpenElements.Pop();
            FInsertionMode := @TheAfterHeadInsertionMode;
            exit;
         end
         else
         if (Token.TagName = eTemplate) then
         begin
            if (not StackOfOpenElementsHas(nsHTML, eTemplate)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected template end tag'); {$ENDIF}
               exit;
            end;
            GenerateAllImpliedEndTagsThoroughly();
            {$IFDEF PARSEERROR}
               if (not CurrentNode.IsIdentity(nsHTML, eTemplate)) then
                  ParseError('mismatched template end tag');
            {$ENDIF}
            while (not FStackOfOpenElements.Pop().IsIdentity(nsHTML, eTemplate)) do ;
            ClearTheListOfActiveFormattingElementsUpToTheLastMarker();
            FStackOfTemplateInsertionModes.Pop();
            ResetTheInsertionModeAppropriately();
            exit;
         end
         else
         if ((Token.TagName <> eBody) and (Token.TagName <> eHTML) and (Token.TagName <> eBR)) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected end tag'); {$ENDIF}
            exit;
         end;
   end;
   // anything else
   Assert(CurrentNode.IsIdentity(nsHTML, eHead));
   FStackOfOpenElements.Pop();
   FInsertionMode := @TheAfterHeadInsertionMode;
   TreeConstructionDispatcher(Token);
end;

procedure THTMLParser.TheInHeadNoScriptInsertionMode(var Token: TToken);
begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkSourceCharacters:
         begin
            // from "in head" mode
            if (ssHaveNonSpace in Token.SpaceState) then
            begin
               if (ssHaveLeadingSpace in Token.SpaceState) then
                  InsertLeadingSpacesFor(Token); // http://bugs.freepascal.org/view.php?id=26403
               // fall through to "anything else" clause
            end
            else
            begin
               InsertCharactersFor(Token);
               exit;
            end;
         end;
      tkExtraSpaceCharacter:
         begin
            // from "in head" mode
            InsertCharacters(Token.ExtraChars);
            exit;
         end;
      tkComment:
         begin
            // from "in head" mode
            InsertAComment(Token);
            exit;
         end;
      tkDOCTYPE:
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected DOCTYPE'); {$ENDIF}
            exit;
         end;
      tkStartTag:
         if (Token.TagName = eHTML) then
         begin
            TheInBodyInsertionMode(Token);
            exit;
         end
         else
         if ((Token.TagName = eBaseFont) or
             (Token.TagName = eBGSound) or
             (Token.TagName = eLink) or
             (Token.TagName = eMeta) or
             (Token.TagName = eNoScript) or
             (Token.TagName = eStyle)) then // <meta>'s special encoding logic is not supported
         begin
            // from "in head" mode
            InsertAnHTMLElementFor(Token);
            FStackOfOpenElements.Pop();
            {$IFDEF PARSEERROR} Token.AcknowledgeSelfClosingFlag(); {$ENDIF}
            exit;
         end
         else
         if ((Token.TagName = eHead) or (Token.TagName = eNoScript)) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected start tag in head noscript'); {$ENDIF}
            exit;
         end;
      tkEndTag:
         if (Token.TagName = eNoScript) then
         begin
            Assert(CurrentNode.IsIdentity(nsHTML, eNoScript));
            FStackOfOpenElements.Pop();
            Assert(CurrentNode.IsIdentity(nsHTML, eHead));
            FInsertionMode := @TheInHeadInsertionMode;
            exit;
         end
         else
         if (Token.TagName <> eBR) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected end tag in head noscript'); {$ENDIF}
            exit;
         end;
   end;
   // anything else
   {$IFDEF PARSEERROR} ParseError('unexpected token in head noscript'); {$ENDIF}
   Assert(CurrentNode.IsIdentity(nsHTML, eNoScript));
   FStackOfOpenElements.Pop();
   Assert(CurrentNode.IsIdentity(nsHTML, eHead));
   FInsertionMode := @TheInHeadInsertionMode;
   TreeConstructionDispatcher(Token);
end;

procedure THTMLParser.TheAfterHeadInsertionMode(var Token: TToken);
begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkSourceCharacters:
         begin
            if (ssHaveNonSpace in Token.SpaceState) then
            begin
               if (ssHaveLeadingSpace in Token.SpaceState) then
                  InsertLeadingSpacesFor(Token); // http://bugs.freepascal.org/view.php?id=26403
               // fall through to "anything else" clause
            end
            else
            begin
               InsertCharactersFor(Token);
               exit;
            end;
         end;
      tkExtraSpaceCharacter:
         begin
            InsertCharacters(Token.ExtraChars);
            exit;
         end;
      tkComment:
         begin
            InsertAComment(Token);
            exit;
         end;
      tkDOCTYPE:
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected DOCTYPE'); {$ENDIF}
            exit;
         end;
      tkStartTag:
         if (Token.TagName = eHTML) then
         begin
            TheInBodyInsertionMode(Token);
            exit;
         end
         else
         if (Token.TagName = eBody) then
         begin
            InsertAnHTMLElementFor(Token);
            FFramesetOkFlag := False;
            FInsertionMode := @TheInBodyInsertionMode;
            exit;
         end
         else
         if (Token.TagName = eFrameset) then
         begin
            InsertAnHTMLElementFor(Token);
            FInsertionMode := @TheInFramesetInsertionMode;
            exit;
         end
         else
         if ((Token.TagName = eBase) or (Token.TagName = eBaseFont) or (Token.TagName = eBGSound) or (Token.TagName = eLink) or
             (Token.TagName = eMeta) or (Token.TagName = eNoFrames) or (Token.TagName = eScript) or (Token.TagName = eStyle) or
             (Token.TagName = eTemplate) or (Token.TagName = eTitle)) then
         begin
            Assert(Assigned(FHeadElementPointer));
            {$IFDEF PARSEERROR} ParseError('unexpected start tag after head element'); {$ENDIF}
            FStackOfOpenElements.Push(FHeadElementPointer);
            TheInHeadInsertionMode(Token);
            FStackOfOpenElements.Remove(FHeadElementPointer);
            exit;
         end
         else
         if (Token.TagName = eHead) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected head start tag'); {$ENDIF}
            exit;
         end;
      tkEndTag:
         if (Token.TagName = eTemplate) then
         begin
            TheInHeadInsertionMode(Token);
            exit;
         end
         else
         if ((Token.TagName <> eBody) and (Token.TagName <> eHTML) and (Token.TagName <> eBR)) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected end tag'); {$ENDIF}
            exit;
         end;
   end;
   // anything else
   Assert(CurrentNode.IsIdentity(nsHTML, eHTML));
   InsertAnHTMLElement(ConstructHTMLElement(eBody));
   FInsertionMode := @TheInBodyInsertionMode;
   TreeConstructionDispatcher(Token);
end;

procedure THTMLParser.TheInBodyInsertionMode(var Token: TToken);

   procedure CloseAPElement();
   begin
      GenerateImpliedEndTagsExceptFor(eP);
      {$IFDEF PARSEERROR}
         if (not CurrentNode.IsIdentity(nsHTML, eP)) then
            ParseError('parse error while closing p element');
      {$ENDIF}
      repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, eP);
   end;

   procedure AnyOtherEndTag();
   var
      Index: Cardinal;
   begin
      Assert(FStackOfOpenElements.Length > 0);
      Index := FStackOfOpenElements.Length-1; // $R-
      repeat
         if (FStackOfOpenElements[Index].IsIdentity(nsHTML, Token.TagName)) then
         begin
            GenerateImpliedEndTagsExceptFor(Token.TagName);
            {$IFDEF PARSEERROR}
               if (Index <> FStackOfOpenElements.Length-1) then
                  ParseError('unexpected end tag');
            {$ENDIF}
            Assert(FStackOfOpenElements.Length > Index);
            repeat
               FStackOfOpenElements.Pop();
            until FStackOfOpenElements.Length <= Index;
            exit;
         end
         else
         if (FStackOfOpenElements[Index].HasProperties(propSpecial)) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected end tag'); {$ENDIF}
            exit;
         end;
         Assert(Index > 0);
         Dec(Index);
      until forever;
   end;

   procedure CallAdoptionAgency(constref Subject: TCanonicalString);
   var
      OuterLoopCounter, InnerLoopCounter,
      FormattingElementIndexInList, FormattingElementIndexInStack,
      FurthestBlockIndexInStack, Bookmark,
      NodeIndexInStack, NodeIndexInList, LastNodeIndexInStack: Cardinal;
      FormattingElement, FurthestBlock, CommonAncestor, NewNode, LastNode: TElement;
      NodeIsInList: Boolean;
      {$IFDEF VERBOSEAAA} DebugIndex: Integer; {$ENDIF}
   begin
      {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('AAA for ', Subject.AsString); {$ENDIF}
      if (CurrentNode.IsIdentity(nsHTML, Subject) and (not FListOfActiveFormattingElements.Contains(CurrentNode))) then
      begin
         {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('AAA-exit1'); {$ENDIF}
         FStackOfOpenElements.Pop();
         exit;
      end;
      if (FListOfActiveFormattingElements.Length = 0) then
      begin
         {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('AAA-exit2'); {$ENDIF}
         AnyOtherEndTag();
         exit;
      end;
      {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('AAA: Starting Outer Loop'); {$ENDIF}
      OuterLoopCounter := 0;
      while (OuterLoopCounter < 8) do // outer loop
      begin
         Inc(OuterLoopCounter);
         {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('     OuterLoopCounter: ', OuterLoopCounter); {$ENDIF}
         Assert(FListOfActiveFormattingElements.Length > 0);
         {$IFDEF VERBOSEAAA}
            if (DebugNow) then
            begin
               Write('     FListOfActiveFormattingElements:');
               for DebugIndex := 0 to FListOfActiveFormattingElements.Length-1 do // $R-
                  if (FListOfActiveFormattingElements[DebugIndex] = Marker) then
                     Write(' [marker]')
                  else
                     Write(' ', FListOfActiveFormattingElements[DebugIndex].LocalName.AsString);
               Writeln();
               Write('     FStackOfOpenElements:');
               for DebugIndex := 0 to FStackOfOpenElements.Length-1 do // $R-
                  Write(' ', FStackOfOpenElements[DebugIndex].LocalName.AsString);
               Writeln();
            end;
         {$ENDIF}
         FormattingElementIndexInList := FListOfActiveFormattingElements.Length-1; // $R-
         {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('     FormattingElementIndexInList starting as: ', FormattingElementIndexInList); {$ENDIF}
         repeat
            if (FListOfActiveFormattingElements[FormattingElementIndexInList] = Marker) then
            begin
               {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('AAA-exit3a - formatting element not between last marker and end of list'); {$ENDIF}
               AnyOtherEndTag();
               exit;
            end;
            if (FListOfActiveFormattingElements[FormattingElementIndexInList].IsIdentity(nsHTML, Subject)) then
               break;
            if (FormattingElementIndexInList = 0) then
            begin
               {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('AAA-exit3b - formatting element not in list (no markers)'); {$ENDIF}
               AnyOtherEndTag();
               exit;
            end;
            Dec(FormattingElementIndexInList);
         until forever;
         {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('     FormattingElementIndexInList changed to: ', FormattingElementIndexInList); {$ENDIF}
         FormattingElement := FListOfActiveFormattingElements[FormattingElementIndexInList];
         {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('     FormattingElement: ', FormattingElement.LocalName.AsString); {$ENDIF}
         if (not FStackOfOpenElements.Contains(FormattingElement, FormattingElementIndexInStack)) then
         begin
            {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('AAA-exit4'); {$ENDIF}
            {$IFDEF PARSEERROR} ParseError('adoption problem'); {$ENDIF}
            FListOfActiveFormattingElements.RemoveAt(FormattingElementIndexInList);
            exit;
         end;
         Assert(FormattingElementIndexInStack < FStackOfOpenElements.Length);
         Assert(FormattingElementIndexInStack > 0); // 0 should be the <html> node
         if (not StackOfOpenElementsHasInScope(Token.TagName)) then
         begin
            {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('AAA-exit5'); {$ENDIF}
            {$IFDEF PARSEERROR} ParseError('adoption problem'); {$ENDIF}
            exit;
         end;
         {$IFDEF PARSEERROR}
         if (FormattingElement <> CurrentNode) then
            ParseError('unexpected end tag');
         {$ENDIF}
         Assert(FormattingElementIndexInStack < High(FurthestBlockIndexInStack));
         FurthestBlockIndexInStack := FormattingElementIndexInStack+1; // $R-
         FurthestBlock := nil; // in case we don't find one
         while ((FurthestBlockIndexInStack < FStackOfOpenElements.Length) and
                (not FStackOfOpenElements[FurthestBlockIndexInStack].HasProperties(propSpecial))) do
            Inc(FurthestBlockIndexInStack);
         if (FurthestBlockIndexInStack >= FStackOfOpenElements.Length) then
         begin
            {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('     there is no furthest block'); {$ENDIF}
            FStackOfOpenElements.Length := FormattingElementIndexInStack; // pop it and all subsequent nodes from the stack
            FListOfActiveFormattingElements.RemoveAt(FormattingElementIndexInList);
            {$IFDEF VERBOSEAAA}
               if (DebugNow) then
               begin
                  Write('     FListOfActiveFormattingElements:');
                  for DebugIndex := 0 to FListOfActiveFormattingElements.Length-1 do // $R-
                     if (FListOfActiveFormattingElements[DebugIndex] = Marker) then
                        Write(' [marker]')
                     else
                        Write(' ', FListOfActiveFormattingElements[DebugIndex].LocalName.AsString);
                  Writeln();
                  Write('     FStackOfOpenElements:');
                  for DebugIndex := 0 to FStackOfOpenElements.Length-1 do // $R-
                     Write(' ', FStackOfOpenElements[DebugIndex].LocalName.AsString);
                  Writeln();
               end;
            {$ENDIF}
            {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('AAA-exit7'); {$ENDIF}
            exit;
         end;
         FurthestBlock := FStackOfOpenElements[FurthestBlockIndexInStack];
         {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('     FurthestBlock: ', FurthestBlock.LocalName.AsString); {$ENDIF}
         CommonAncestor := FStackOfOpenElements[FormattingElementIndexInStack-1]; // $R-
         {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('     CommonAncestor: ', CommonAncestor.LocalName.AsString); {$ENDIF}
         Bookmark := FormattingElementIndexInList;
         {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('        Bookmark: ', Bookmark); {$ENDIF}
         NodeIndexInStack := FurthestBlockIndexInStack;
         LastNodeIndexInStack := FurthestBlockIndexInStack;
         InnerLoopCounter := 0;
         {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('     Starting Inner Loop'); {$ENDIF}
         repeat
            Inc(InnerLoopCounter);
            {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('        InnerLoopCounter: ', InnerLoopCounter); {$ENDIF}
            {$IFDEF VERBOSEAAA}
               if (DebugNow) then
               begin
                  Write('        FListOfActiveFormattingElements:');
                  for DebugIndex := 0 to FListOfActiveFormattingElements.Length-1 do // $R-
                     if (FListOfActiveFormattingElements[DebugIndex] = Marker) then
                        Write(' [marker]')
                     else
                        Write(' ', FListOfActiveFormattingElements[DebugIndex].LocalName.AsString);
                  Writeln();
                  Write('        FStackOfOpenElements:');
                  for DebugIndex := 0 to FStackOfOpenElements.Length-1 do // $R-
                     Write(' ', FStackOfOpenElements[DebugIndex].LocalName.AsString);
                  Writeln();
               end;
            {$ENDIF}
            {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('        Node: ', FStackOfOpenElements[NodeIndexInStack].LocalName.AsString); {$ENDIF}
            {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('        LastNode: ', FStackOfOpenElements[LastNodeIndexInStack].LocalName.AsString); {$ENDIF}
            {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('        Letting node be the previous node...'); {$ENDIF}
            Dec(NodeIndexInStack);
            {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('        Node: ', FStackOfOpenElements[NodeIndexInStack].LocalName.AsString); {$ENDIF}
            {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('        LastNode: ', FStackOfOpenElements[LastNodeIndexInStack].LocalName.AsString); {$ENDIF}
            if (NodeIndexInStack = FormattingElementIndexInStack) then
            begin
               {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('        Reached Node = Formatting Element case, ending inner loop'); {$ENDIF}
               break;
            end;
            NodeIsInList := FListOfActiveFormattingElements.Contains(FStackOfOpenElements[NodeIndexInStack], NodeIndexInList);
            {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('        Node is in list: ', NodeIsInList); {$ENDIF}
            if (NodeIsInList and (InnerLoopCounter > 3)) then
            begin
               {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('        ...and inner loop counter is more than three, so removing from list'); {$ENDIF}
               Assert(NodeIndexInList < FListOfActiveFormattingElements.Length);
               FListOfActiveFormattingElements.RemoveAt(NodeIndexInList);
               Assert(NodeIndexInList <= Bookmark);
               // if (NodeIndexInList <= Bookmark) then
                  Dec(Bookmark);
               NodeIsInList := False;
            end;
            if (not NodeIsInList) then
            begin
               {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('        node not in list, so doing that and relooping'); {$ENDIF}
               FStackOfOpenElements.RemoveAt(NodeIndexInStack);
               Assert(FormattingElementIndexInStack < NodeIndexInStack);
               Assert(FurthestBlockIndexInStack > NodeIndexInStack);
               Dec(FurthestBlockIndexInStack);
               Assert(FStackOfOpenElements[FurthestBlockIndexInStack] = FurthestBlock);
               Assert(LastNodeIndexInStack > NodeIndexInStack);
               Dec(LastNodeIndexInStack);
               continue;
            end;
            {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('        going to create a new node'); {$ENDIF}
            Assert(FListOfActiveFormattingElements[NodeIndexInList] = FStackOfOpenElements[NodeIndexInStack]);
            NewNode := FStackOfOpenElements[NodeIndexInStack].CloneNode();
            {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('        created a new ', NewNode.LocalName.AsString); {$ENDIF}
            FListOfActiveFormattingElements[NodeIndexInList] := NewNode;
            {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('        replaced it in the list at index ', NodeIndexInList); {$ENDIF}
            FStackOfOpenElements[NodeIndexInStack] := NewNode;
            {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('        replaced it in the stack at index ', NodeIndexInStack); {$ENDIF}
            if (LastNodeIndexInStack = FurthestBlockIndexInStack) then
            begin
               Assert(NodeIndexInList < High(Bookmark));
               Bookmark := NodeIndexInList; // $R-
               {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('        New Bookmark: ', Bookmark); {$ENDIF}
            end;
            LastNode := FStackOfOpenElements[LastNodeIndexInStack];
            {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('        Inserting Last Node (', LastNode.LocalName.AsString, ') into Node (', NewNode.LocalName.AsString, '): ', Bookmark); {$ENDIF}
            if (Assigned(LastNode.ParentNode)) then
               LastNode.Remove();
            NewNode.AppendChild(LastNode);
            LastNodeIndexInStack := NodeIndexInStack;
         until forever;
         {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('     Done with inner loop'); {$ENDIF}
         {$IFDEF VERBOSEAAA}
            if (DebugNow) then
            begin
               Write('     FListOfActiveFormattingElements:');
               for DebugIndex := 0 to FListOfActiveFormattingElements.Length-1 do // $R-
                  if (FListOfActiveFormattingElements[DebugIndex] = Marker) then
                     Write(' [marker]')
                  else
                     Write(' ', FListOfActiveFormattingElements[DebugIndex].LocalName.AsString);
               Writeln();
               Write('     FStackOfOpenElements:');
               for DebugIndex := 0 to FStackOfOpenElements.Length-1 do // $R-
                  Write(' ', FStackOfOpenElements[DebugIndex].LocalName.AsString);
               Writeln();
            end;
         {$ENDIF}
         Assert(Assigned(CommonAncestor));
         if (Assigned(FStackOfOpenElements[LastNodeIndexInStack].ParentNode)) then
            FStackOfOpenElements[LastNodeIndexInStack].Remove();
         {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('     Inserting LastNode (', FStackOfOpenElements[LastNodeIndexInStack].LocalName.AsString, ') into CommonAncestor (', CommonAncestor.LocalName.AsString, ')'); {$ENDIF}
         InsertNodeAtAppropriatePlaceForInsertingANode(FStackOfOpenElements[LastNodeIndexInStack], CommonAncestor);
         NewNode := FormattingElement.CloneNode();
         {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('     Created ', NewNode.LocalName.AsString); {$ENDIF}
         Assert(Assigned(FurthestBlock));
         {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('     Swapping children from FurthestBlock to that new node'); {$ENDIF}
         NewNode.SwapChildNodes(FurthestBlock);
         {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('     Appending the new node to FurthestBlock'); {$ENDIF}
         FurthestBlock.AppendChild(NewNode);
         Assert(FListOfActiveFormattingElements[FormattingElementIndexInList] = FormattingElement);
         {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('     FListOfActiveFormattingElements.RemoveShiftLeftInsert(): Removing from ', FormattingElementIndexInList, ' and inserting at ', Bookmark, ' -- array length: ', FListOfActiveFormattingElements.Length); {$ENDIF}
         FListOfActiveFormattingElements.RemoveShiftLeftInsert(FormattingElementIndexInList, Bookmark, NewNode);
         {$IFDEF VERBOSEAAA}
            if (DebugNow) then
            begin
               Write('     FListOfActiveFormattingElements:');
               for DebugIndex := 0 to FListOfActiveFormattingElements.Length-1 do // $R-
                  if (FListOfActiveFormattingElements[DebugIndex] = Marker) then
                     Write(' [marker]')
                  else
                     Write(' ', FListOfActiveFormattingElements[DebugIndex].LocalName.AsString);
               Writeln();
            end;
         {$ENDIF}
         Assert(FurthestBlockIndexInStack < High(FurthestBlockIndexInStack));
         {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('     FStackOfOpenElements.RemoveShiftLeftInsert(): Removing from ', FormattingElementIndexInStack, ' and inserting at ', FurthestBlockIndexInStack, ' -- array length: ', FStackOfOpenElements.Length); {$ENDIF}
         FStackOfOpenElements.RemoveShiftLeftInsert(FormattingElementIndexInStack, FurthestBlockIndexInStack, NewNode); // $R-
         {$IFDEF VERBOSEAAA}
            if (DebugNow) then
            begin
               Write('     FStackOfOpenElements:');
               for DebugIndex := 0 to FStackOfOpenElements.Length-1 do // $R-
                  Write(' ', FStackOfOpenElements[DebugIndex].LocalName.AsString);
               Writeln();
            end;
         {$ENDIF}
         {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('     Done shifting things around...'); {$ENDIF}
         {$IFDEF VERBOSEAAA}
            if (DebugNow) then
            begin
               Write('     FListOfActiveFormattingElements:');
               for DebugIndex := 0 to FListOfActiveFormattingElements.Length-1 do // $R-
                  if (FListOfActiveFormattingElements[DebugIndex] = Marker) then
                     Write(' [marker]')
                  else
                     Write(' ', FListOfActiveFormattingElements[DebugIndex].LocalName.AsString);
               Writeln();
               Write('     FStackOfOpenElements:');
               for DebugIndex := 0 to FStackOfOpenElements.Length-1 do // $R-
                  Write(' ', FStackOfOpenElements[DebugIndex].LocalName.AsString);
               Writeln();
            end;
         {$ENDIF}
      end;
      {$IFDEF VERBOSEAAA} if (DebugNow) then Writeln('     Done with outer loop'); {$ENDIF}
   end;

var
   Attributes: TElement.TAttributeHashTable;
   HTMLElement, BodyElement, NewElement, OldElement, Node: TElement;
   AttributeName, TemporaryString: UTF8String;
   Index: Cardinal;
   TemporaryAttributeValue: TParserString;
begin
   Assert(Assigned(FDocument));
   {$IFDEF INSTRUMENTING}
      Writeln('INBODY: ', Token.Kind);
      if (Token.Kind = tkStartTag) then
         Writeln('INBODY START TAG: ', Token.TagName.AsString);
      if (Token.Kind = tkEndTag) then
         Writeln('INBODY END TAG: ', Token.TagName.AsString);
   {$ENDIF}
   case (Token.Kind) of

      // the states here are in order of most common to least common on some test input
      tkSourceCharacters:
         begin
            ReconstructTheActiveFormattingElements();
            InsertCharactersFor(Token);
            if (ssHaveNonSpace in Token.SpaceState) then
               FFramesetOkFlag := False;
         end;
      tkStartTag:
         // the conditionals here are in order of most common to least
         // common on some test input. only the most common
         // conditionals are hoisted up here; the rest are left in the
         // original spec order below (<code> was about 60 times more
         // common than <hr>, the least-common hoisted start tag)
         if (Token.TagName = eCode) then
         begin
            ReconstructTheActiveFormattingElements();
            PushOntoTheListOfActiveFormattingElements(InsertAnHTMLElementFor(Token));
         end
         else
         if (Token.TagName = eSpan) then
         begin
            // any other start tag
            ReconstructTheActiveFormattingElements();
            InsertAnHTMLElementFor(Token);
         end
         else
         if (Token.TagName = eP) then
         begin
            if (StackOfOpenElementsHasInButtonScope(eP)) then
               CloseAPElement();
            InsertAnHTMLElementFor(Token);
         end
         else
         if (Token.TagName = eVar) then
         begin
            // any other start tag
            ReconstructTheActiveFormattingElements();
            InsertAnHTMLElementFor(Token);
         end
         else
         if (Token.TagName = eLI) then
         begin
            FFramesetOkFlag := False;
            Assert(FStackOfOpenElements.Length > 0);
            Index := FStackOfOpenElements.Length-1; // $R-
            repeat
               if (FStackOfOpenElements[Index].IsIdentity(nsHTML, eLI)) then
               begin
                  GenerateImpliedEndTagsExceptFor(eLI);
                  {$IFDEF PARSEERROR}
                     if (not CurrentNode.IsIdentity(nsHTML, eLI)) then
                        ParseError('unexpected li start tag');
                  {$ENDIF}
                  repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, eLI);
                  break;
               end
               else
               if (FStackOfOpenElements[Index].HasProperties(propSpecialish)) then
               begin
                  break;
               end
               else
               begin
                  Assert(Index > 0);
                  Dec(Index);
               end;
            until forever;
            if (StackOfOpenElementsHasInButtonScope(eP)) then
               CloseAPElement();
            InsertAnHTMLElementFor(Token);
         end
         else
         if (Token.TagName = eDfn) then
         begin
            // any other start tag
            ReconstructTheActiveFormattingElements();
            InsertAnHTMLElementFor(Token);
         end
         else
         if ((Token.TagName = eDD) or (Token.TagName = eDT)) then
         begin
            FFramesetOkFlag := False;
            Assert(FStackOfOpenElements.Length > 0);
            Index := FStackOfOpenElements.Length-1; // $R-
            repeat
               if (FStackOfOpenElements[Index].IsIdentity(nsHTML, eDD)) then
               begin
                  GenerateImpliedEndTagsExceptFor(eDD);
                  {$IFDEF PARSEERROR}
                     if (not CurrentNode.IsIdentity(nsHTML, eDD)) then
                        ParseError('unexpected li start tag');
                  {$ENDIF}
                  repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, eDD);
                  break;
               end
               else
               if (FStackOfOpenElements[Index].IsIdentity(nsHTML, eDT)) then
               begin
                  GenerateImpliedEndTagsExceptFor(eDT);
                  {$IFDEF PARSEERROR}
                     if (not CurrentNode.IsIdentity(nsHTML, eDT)) then
                        ParseError('unexpected li start tag');
                  {$ENDIF}
                  repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, eDT);
                  break;
               end
               else
               if (FStackOfOpenElements[Index].HasProperties(propSpecialish)) then
               begin
                  break;
               end
               else
               begin
                  Assert(Index > 0);
                  Dec(Index);
               end;
            until forever;
            if (StackOfOpenElementsHasInButtonScope(eP)) then
               CloseAPElement();
            InsertAnHTMLElementFor(Token);
         end
         else
         if (Token.TagName = eI) then
         begin
            ReconstructTheActiveFormattingElements();
            PushOntoTheListOfActiveFormattingElements(InsertAnHTMLElementFor(Token));
         end
         else
         if (Token.TagName = eDiv) then
         begin
            if (StackOfOpenElementsHasInButtonScope(eP)) then
               CloseAPElement();
            InsertAnHTMLElementFor(Token);
         end
         else
         if (Token.TagName = eA) then
         begin
            if (FListOfActiveFormattingElements.Length > 0) then
            begin
               Index := FListOfActiveFormattingElements.Length;
               repeat
                  Assert(Index > 0);
                  Dec(Index); // $R-
                  if (FListOfActiveFormattingElements[Index] = Marker) then
                  begin
                     break;
                  end
                  else
                  if (FListOfActiveFormattingElements[Index].IsIdentity(nsHTML, eA)) then
                  begin
                     {$IFDEF PARSEERROR} ParseError('unexpected a start tag in a element'); {$ENDIF}
                     OldElement := FListOfActiveFormattingElements[Index];
                     CallAdoptionAgency(eA);
                     // OldElement might not have been removed from the list and stack, so force-remove them (these are expensive no-ops if the element is gone now):
                     FListOfActiveFormattingElements.Remove(OldElement);
                     FStackOfOpenElements.Remove(OldElement);
                     break;
                  end;
               until (Index = 0);
            end;
            ReconstructTheActiveFormattingElements();
            PushOntoTheListOfActiveFormattingElements(InsertAnHTMLElementFor(Token));
         end
         else
         if (Token.TagName = ePre) then
         begin
            if (StackOfOpenElementsHasInButtonScope(eP)) then
               CloseAPElement();
            InsertAnHTMLElementFor(Token);
            FOriginalInsertionMode := FInsertionMode;
            FInsertionMode := @TheSkipNewLineInsertionMode;
            FFramesetOkFlag := False;
         end
         else
         if ((Token.TagName = eOL) or (Token.TagName = eDL)) then
         begin
            if (StackOfOpenElementsHasInButtonScope(eP)) then
               CloseAPElement();
            InsertAnHTMLElementFor(Token);
         end
         else
         if (Token.TagName = eHR) then
         begin
            if (StackOfOpenElementsHasInButtonScope(eP)) then
               CloseAPElement();
            InsertAnHTMLElementFor(Token);
            FStackOfOpenElements.Pop();
            {$IFDEF PARSEERROR} Token.AcknowledgeSelfClosingFlag(); {$ENDIF}
            FFramesetOkFlag := false;
         end
         else
         // remaining conditionals in no particular order
         if (Token.TagName = eHTML) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected html start tag'); {$ENDIF}
            if (StackOfOpenElementsHas(nsHTML, eTemplate)) then
               exit;
            Attributes := Token.TakeAttributes();
            if (Assigned(Attributes)) then
            begin
               HTMLElement := FStackOfOpenElements[0];
               Assert(HTMLElement is THTMLHTMLElement);
               for AttributeName in Attributes do
                  if (not HTMLElement.HasAttribute(AttributeName)) then
                  begin
                     TemporaryAttributeValue := Attributes[AttributeName];
                     HTMLElement.SetAttributeDestructively(AttributeName, TemporaryAttributeValue);
                  end;
               Attributes.Free();
            end;
         end
         else
         if ((Token.TagName = eBase) or (Token.TagName = eBaseFont) or (Token.TagName = eBGSound) or (Token.TagName = eLink) or
             (Token.TagName = eMeta) or (Token.TagName = eNoFrames) or (Token.TagName = eScript) or (Token.TagName = eStyle) or
             (Token.TagName = eTemplate) or (Token.TagName = eTitle)) then
         begin
            TheInHeadInsertionMode(Token);
         end
         else
         if (Token.TagName = eBody) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected body start tag'); {$ENDIF}
            if ((FStackOfOpenElements.Length < 2) or 
                (not FStackOfOpenElements[1].IsIdentity(nsHTML, eBody)) or
                (StackOfOpenElementsHas(nsHTML, eTemplate))) then
               exit; // ignore the token
            FFramesetOkFlag := False;
            Attributes := Token.TakeAttributes();
            if (Assigned(Attributes)) then
            begin
               BodyElement := FStackOfOpenElements[1];
               Assert(BodyElement is THTMLBodyElement);
               for AttributeName in Attributes do
                  if (not BodyElement.HasAttribute(AttributeName)) then
                  begin
                     TemporaryAttributeValue := Attributes[AttributeName];
                     BodyElement.SetAttributeDestructively(AttributeName, TemporaryAttributeValue);
                  end;
               Attributes.Free();
            end;
         end
         else
         if (Token.TagName = eFrameset) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected body frameset start tag'); {$ENDIF}
            if ((FStackOfOpenElements.Length < 2) or 
                (not FStackOfOpenElements[1].IsIdentity(nsHTML, eBody))) then
               exit; // ignore the token
            if (not FFramesetOkFlag) then
               exit; // ignore the token
            BodyElement := FStackOfOpenElements[1];
            Assert(Assigned(BodyElement.ParentNode));
            BodyElement.Remove();
            while (FStackOfOpenElements.Length > 1) do
               FStackOfOpenElements.Pop();
            BodyElement.Free();
            {$IFOPT C+} BodyElement := nil; {$ENDIF}
            InsertAnHTMLElementFor(Token);
            FInsertionMode := @TheInFramesetInsertionMode;
         end
         else
         if ((Token.TagName = eAddress) or
             (Token.TagName = eArticle) or
             (Token.TagName = eAside) or
             (Token.TagName = eBlockQuote) or
             (Token.TagName = eCenter) or
             (Token.TagName = eDetails) or
             (Token.TagName = eDialog) or
             (Token.TagName = eDir) or
             //(Token.TagName = eDiv) or // hoisted higher
             //(Token.TagName = eDL) or // hoisted higher
             (Token.TagName = eFieldSet) or
             (Token.TagName = eFigCaption) or
             (Token.TagName = eFigure) or
             (Token.TagName = eFooter) or
             (Token.TagName = eHeader) or
             (Token.TagName = eHGroup) or
             (Token.TagName = eMain) or
             (Token.TagName = eMenu) or
             (Token.TagName = eNav) or
             //(Token.TagName = eOL) or // hoisted higher
             //(Token.TagName = eP) or // hoisted higher
             (Token.TagName = eSection) or
             (Token.TagName = eSummary) or
             (Token.TagName = eUL)) then
         begin
            if (StackOfOpenElementsHasInButtonScope(eP)) then
               CloseAPElement();
            InsertAnHTMLElementFor(Token);
         end
         else
         if ((Token.TagName = eH1) or
             (Token.TagName = eH2) or
             (Token.TagName = eH3) or
             (Token.TagName = eH4) or
             (Token.TagName = eH5) or
             (Token.TagName = eH6)) then
         begin
            if (StackOfOpenElementsHasInButtonScope(eP)) then
               CloseAPElement();
            if (CurrentNode.HasProperties(propHeading)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected nesting of h1-h6 elements'); {$ENDIF}
               FStackOfOpenElements.Pop();
            end;
            InsertAnHTMLElementFor(Token);
         end
         else
         if (//(Token.TagName = ePre) or // hoisted
             (Token.TagName = eListing)) then
         begin
            if (StackOfOpenElementsHasInButtonScope(eP)) then
               CloseAPElement();
            InsertAnHTMLElementFor(Token);
            FOriginalInsertionMode := FInsertionMode;
            FInsertionMode := @TheSkipNewLineInsertionMode;
            FFramesetOkFlag := False;
         end
         else
         if (Token.TagName = eForm) then
         begin
            if (Assigned(FFormElementPointer) and (not StackOfOpenElementsHas(nsHTML, eTemplate))) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected form element start tag'); {$ENDIF}
            end
            else
            begin
               if (StackOfOpenElementsHasInButtonScope(eP)) then
                  CloseAPElement();
               NewElement := InsertAnHTMLElementFor(Token);
               if (not StackOfOpenElementsHas(nsHTML, eTemplate)) then
                  FFormElementPointer := NewElement;
            end;
         end
         else
         if (Token.TagName = ePlaintext) then
         begin
            if (StackOfOpenElementsHasInButtonScope(eP)) then
               CloseAPElement();
            InsertAnHTMLElementFor(Token);
            FTokeniserState := tsPlaintextState;
         end
         else
         if (Token.TagName = eButton) then
         begin
            if (StackOfOpenElementsHasInScope(eButton)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected button start tag'); {$ENDIF}
               GenerateImpliedEndTags();
               repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, eButton);
            end;
            ReconstructTheActiveFormattingElements();
            InsertAnHTMLElementFor(Token);
            FFramesetOkFlag := False;
         end
         else
         if ((Token.TagName = eB) or
             (Token.TagName = eBig) or
             //(Token.TagName = eCode) or // hoisted higher
             (Token.TagName = eEm) or
             (Token.TagName = eFont) or
             //(Token.TagName = eI) or // hoisted higher
             (Token.TagName = eS) or
             (Token.TagName = eSmall) or
             (Token.TagName = eStrike) or
             (Token.TagName = eStrong) or
             (Token.TagName = eTT) or
             (Token.TagName = eU)) then
         begin
            ReconstructTheActiveFormattingElements();
            PushOntoTheListOfActiveFormattingElements(InsertAnHTMLElementFor(Token));
         end
         else
         if (Token.TagName = eNoBr) then
         begin
            ReconstructTheActiveFormattingElements();
            if (StackOfOpenElementsHasInScope(eNoBr)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected nobr start tag'); {$ENDIF}
               CallAdoptionAgency(eNoBr);
               ReconstructTheActiveFormattingElements();
            end;
            PushOntoTheListOfActiveFormattingElements(InsertAnHTMLElementFor(Token));
         end
         else
         if ((Token.TagName = eApplet) or
             (Token.TagName = eMarquee) or
             (Token.TagName = eObject)) then
         begin
            ReconstructTheActiveFormattingElements();
            InsertAnHTMLElementFor(Token);
            InsertMarkerAtEndOfListOfActiveFormattingElements();
            FFramesetOkFlag := False;
         end
         else
         if (Token.TagName = eTable) then
         begin
            if ((FDocument.DocumentMode <> dmQuirksMode) and (StackOfOpenElementsHasInButtonScope(eP))) then
               CloseAPElement();
            InsertAnHTMLElementFor(Token);
            FFramesetOkFlag := False;
            FInsertionMode := @TheInTableInsertionMode;
         end
         else
         if ((Token.TagName = eArea) or
             (Token.TagName = eBr) or
             (Token.TagName = eEmbed) or
             (Token.TagName = eImg) or
             (Token.TagName = eKeygen) or
             (Token.TagName = eWBr)) then
         begin
            ReconstructTheActiveFormattingElements();
            InsertAnHTMLElementFor(Token);
            FStackOfOpenElements.Pop();
            {$IFDEF PARSEERROR} Token.AcknowledgeSelfClosingFlag(); {$ENDIF}
            FFramesetOkFlag := False;
         end
         else
         if (Token.TagName = eInput) then
         begin
            ReconstructTheActiveFormattingElements();
            NewElement := InsertAnHTMLElementFor(Token);
            FStackOfOpenElements.Pop();
            {$IFDEF PARSEERROR} Token.AcknowledgeSelfClosingFlag(); {$ENDIF}
            // XXX Should cache this on the element as a property when creating it
            if ((not NewElement.HasAttribute('type')) or (ASCIILowerCase(NewElement.GetAttribute('type').AsString) <> 'hidden')) then
               FFramesetOkFlag := False;
         end
         else
         if ((Token.TagName = eMenuItem) or
             (Token.TagName = eParam) or
             (Token.TagName = eSource) or
             (Token.TagName = eTrack)) then
         begin
            InsertAnHTMLElementFor(Token);
            FStackOfOpenElements.Pop();
            {$IFDEF PARSEERROR} Token.AcknowledgeSelfClosingFlag(); {$ENDIF}
         end
         else
         if (Token.TagName = eImage) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected image start tag'); {$ENDIF}
            Token.TagName := eImg;
            TreeConstructionDispatcher(Token);
         end
         else
         if (Token.TagName = eIsIndex) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected isindex start tag'); {$ENDIF}
            if ((not StackOfOpenElementsHas(nsHTML, eTemplate)) and (Assigned(FFormElementPointer))) then
               exit;
            {$IFDEF PARSEERROR} Token.AcknowledgeSelfClosingFlag(); {$ENDIF}
            FFramesetOkFlag := False;
            if (StackOfOpenElementsHasInButtonScope(eP)) then
               CloseAPElement();
            NewElement := InsertAnHTMLElement(ConstructHTMLElement(eForm));
            Assert(not Assigned(FFormElementPointer));
            if (not StackOfOpenElementsHas(nsHTML, eTemplate)) then
               FFormElementPointer := NewElement;
            Attributes := Token.TakeAttributes();
            if (Assigned(Attributes) and Attributes.Has('action')) then
            begin
               TemporaryAttributeValue := Attributes['action'];
               NewElement.SetAttributeDestructively('action', TemporaryAttributeValue);
               Attributes.Remove('action');
            end;
            InsertAnHTMLElement(ConstructHTMLElement(eHR));
            FStackOfOpenElements.Pop();
            ReconstructTheActiveFormattingElements();
            InsertAnHTMLElement(ConstructHTMLElement(eLabel));
            if (Assigned(Attributes) and Attributes.Has('prompt')) then
            begin
               TemporaryAttributeValue := Attributes['prompt'];
               // InsertCharacters(TemporaryAttributeValue);
               Assert(not FFosterParenting);
               Assert(CurrentNode.IsIdentity(nsHTML, eLabel));
               (CurrentNode as TElement).AppendChild(TText.CreateDestructively(TemporaryAttributeValue));
               Attributes.Remove('prompt');
            end
            else
               InsertCharacters('This is a searchable index. Enter search keywords: ');
            if (not Assigned(Attributes)) then
               Attributes := TElement.TAttributeHashTable.Create(1);
            TemporaryAttributeValue := Default(TParserString);
            TemporaryString := 'isindex';
            TemporaryAttributeValue.Append(@TemporaryString);
            Attributes['name'] := TemporaryAttributeValue;
            InsertAnHTMLElement(ConstructHTMLElement(eInput, Attributes));
            FStackOfOpenElements.Pop();
            // InsertCharacters('');
            Assert(CurrentNode.IsIdentity(nsHTML, eLabel));
            FStackOfOpenElements.Pop();
            InsertAnHTMLElement(ConstructHTMLElement(eHR));
            FStackOfOpenElements.Pop();
            Assert(CurrentNode = NewElement);
            FStackOfOpenElements.Pop();
            if (not StackOfOpenElementsHas(nsHTML, eTemplate)) then
            begin
               Assert(FFormElementPointer = NewElement);
               FFormElementPointer := nil;
            end;
         end
         else
         if (Token.TagName = eTextArea) then
         begin
            InsertAnHTMLElementFor(Token);
            FTokeniserState := tsRcdataState;
            FOriginalInsertionMode := FInsertionMode;
            FFramesetOkFlag := false;
            FInsertionMode := @TheSkipNewLineThenTextInsertionMode;
         end
         else
         if (Token.TagName = eXMP) then
         begin
            if (StackOfOpenElementsHasInButtonScope(eP)) then
               CloseAPElement();
            ReconstructTheActiveFormattingElements();
            FFramesetOkFlag := False;
            GenericRawTextElementParsingAlgorithm(Token);
         end
         else
         if (Token.TagName = eIFrame) then
         begin
            FFramesetOkFlag := False;
            GenericRawTextElementParsingAlgorithm(Token);
         end
         else
         if ((Token.TagName = eNoEmbed) or ((Token.TagName = eNoScript) and (FScriptingFlag))) then
         begin
            GenericRawTextElementParsingAlgorithm(Token);
         end
         else
         if (Token.TagName = eSelect) then
         begin
            ReconstructTheActiveFormattingElements();
            InsertAnHTMLElementFor(Token);
            FFramesetOkFlag := False;
            if ((FInsertionMode = @TheInTableInsertionMode) or
                (FInsertionMode = @TheInCaptionInsertionMode) or
                (FInsertionMode = @TheInTableBodyInsertionMode) or
                (FInsertionMode = @TheInRowInsertionMode) or
                (FInsertionMode = @TheInCellInsertionMode)) then
               FInsertionMode := @TheInSelectInTableInsertionMode
            else
               FInsertionMode := @TheInSelectInsertionMode;
         end
         else
         if ((Token.TagName = eOptGroup) or
             (Token.TagName = eOption)) then
         begin
            if (CurrentNode.IsIdentity(nsHTML, eOption)) then
               FStackOfOpenElements.Pop();
            ReconstructTheActiveFormattingElements();
            InsertAnHTMLElementFor(Token);
         end
         else
         if ((Token.TagName = eRP) or
             (Token.TagName = eRT)) then
         begin
            if (StackOfOpenElementsHasInScope(eRuby)) then
               GenerateImpliedEndTags();
            {$IFDEF PARSEERROR}
               if (not CurrentNode.IsIdentity(nsHTML, eRuby)) then
                   ParseError('unexpected start tag');
            {$ENDIF}
            InsertAnHTMLElementFor(Token);
         end
         else
         if ((Token.TagName = eMath)) then
         begin
            ReconstructTheActiveFormattingElements();
            Token.AdjustMathMLAttributes();
            Token.AdjustForeignAttributes();
            InsertAForeignElementFor(Token, nsMathML);
            if (Token.SelfClosingFlag) then
            begin
               FStackOfOpenElements.Pop();
               {$IFDEF PARSEERROR} Token.AcknowledgeSelfClosingFlag(); {$ENDIF}
            end;
         end
         else
         if ((Token.TagName = eSVG)) then
         begin
            ReconstructTheActiveFormattingElements();
            Token.AdjustSVGAttributes();
            Token.AdjustForeignAttributes();
            InsertAForeignElementFor(Token, nsSVG);
            if (Token.SelfClosingFlag) then
            begin
               FStackOfOpenElements.Pop();
               {$IFDEF PARSEERROR} Token.AcknowledgeSelfClosingFlag(); {$ENDIF}
            end;
         end
         else
         if ((Token.TagName = eCaption) or
             (Token.TagName = eCol) or
             (Token.TagName = eColGroup) or
             (Token.TagName = eFrame) or
             (Token.TagName = eHead) or
             (Token.TagName = eTBody) or
             (Token.TagName = eTD) or
             (Token.TagName = eTFoot) or
             (Token.TagName = eTH) or
             (Token.TagName = eTHead) or
             (Token.TagName = eTR)) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected start tag'); {$ENDIF}
         end
         else
         begin
            // any other start tag
            ReconstructTheActiveFormattingElements();
            InsertAnHTMLElementFor(Token);
            if (FProprietaryVoids.Contains(Token.TagName)) then
            begin
               FStackOfOpenElements.Pop();
               {$IFDEF PARSEERROR} Token.AcknowledgeSelfClosingFlag(); {$ENDIF}
            end;
         end;
      tkEndTag:
         // in this section things are hoisted also
         if (Token.TagName = eCode) then
         begin
            CallAdoptionAgency(Token.TagName);
         end
         else
         if (Token.TagName = eSpan) then
         begin
            // any other end tag
            AnyOtherEndTag();
         end
         else
         if (Token.TagName = eP) then
         begin
            if (not StackOfOpenElementsHasInButtonScope(eP)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected p end tag'); {$ENDIF}
               InsertAnHTMLElement(ConstructHTMLElement(eP));
            end;
            CloseAPElement();
         end
         else
         if (Token.TagName = eVar) then
         begin
            // any other end tag
            AnyOtherEndTag();
         end
         else
         if (Token.TagName = eLI) then
         begin
            if (not StackOfOpenElementsHasInListItemScope(eLI)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected li end tag'); {$ENDIF}
            end
            else
            begin
               GenerateImpliedEndTagsExceptFor(eLI);
               {$IFDEF PARSEERROR}
                  if (not CurrentNode.IsIdentity(nsHTML, eLI)) then
                     ParseError('unexpected li end tag');
               {$ENDIF}
               repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, eLI);
            end;
         end
         else
         if (Token.TagName = eDfn) then
         begin
            // any other end tag
            AnyOtherEndTag();
         end
         else
         if ((Token.TagName = eDD) or (Token.TagName = eDT)) then
         begin
            if (not StackOfOpenElementsHasInScope(Token.TagName)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected end tag'); {$ENDIF}
            end
            else
            begin
               GenerateImpliedEndTagsExceptFor(Token.TagName);
               {$IFDEF PARSEERROR}
                  if (not CurrentNode.IsIdentity(nsHTML, Token.TagName)) then
                     ParseError('unexpected end tag');
               {$ENDIF}
               repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, Token.TagName);
            end;
         end
         else
         if (Token.TagName = eI) then
         begin
            CallAdoptionAgency(Token.TagName);
         end
         else
         if ((Token.TagName = eDiv) or
             (Token.TagName = ePre) or
             (Token.TagName = eOL) or
             (Token.TagName = eDL)) then 
         begin
            if (not StackOfOpenElementsHasInScope(Token.TagName)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected end tag'); {$ENDIF}
            end
            else
            begin
               GenerateImpliedEndTags();
               {$IFDEF PARSEERROR}
                  if (not CurrentNode.IsIdentity(nsHTML, Token.TagName)) then
                     ParseError('unexpected end tag');
               {$ENDIF}
               repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, Token.TagName);
            end;
         end
         else
         if (Token.TagName = eA) then // this one really should be between div and pre above
         begin
            CallAdoptionAgency(Token.TagName);
         end
         else
         // remainder are in spec order
         if (Token.TagName = eTemplate) then
         begin
            TheInHeadInsertionMode(Token);
         end
         else
         if (Token.TagName = eBody) then
         begin
            if (not StackOfOpenElementsHasInScope(eBody)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected body end tag'); {$ENDIF}
            end
            else
            begin
               {$IFDEF PARSEERROR}
                  if (StackOfOpenElementsHasElementOtherThan(propEOFImpliedEndTag)) then
                     ParseError('unexpected body end tag');
               {$ENDIF}
               FInsertionMode := @TheAfterBodyInsertionMode;
            end;
         end
         else
         if (Token.TagName = eHTML) then
         begin
            if (not StackOfOpenElementsHasInScope(eBody)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected html end tag'); {$ENDIF}
            end
            else
            begin
               {$IFDEF PARSEERROR}
               if (StackOfOpenElementsHasElementOtherThan(propEOFImpliedEndTag)) then
                  ParseError('unexpected html end tag');
               {$ENDIF}
               FInsertionMode := @TheAfterBodyInsertionMode;
               TreeConstructionDispatcher(Token);
            end;
         end
         else
         if ((Token.TagName = eAddress) or
             (Token.TagName = eArticle) or
             (Token.TagName = eAside) or
             (Token.TagName = eBlockQuote) or
             (Token.TagName = eButton) or
             (Token.TagName = eCenter) or
             (Token.TagName = eDetails) or
             (Token.TagName = eDialog) or
             (Token.TagName = eDir) or
             //(Token.TagName = eDiv) or // hoisted
             //(Token.TagName = eDL) or // hoisted
             (Token.TagName = eFieldSet) or
             (Token.TagName = eFigCaption) or
             (Token.TagName = eFigure) or
             (Token.TagName = eFooter) or
             (Token.TagName = eHeader) or
             (Token.TagName = eHGroup) or
             (Token.TagName = eListing) or
             (Token.TagName = eMain) or
             (Token.TagName = eMenu) or
             (Token.TagName = eNav) or
             //(Token.TagName = eOL) or // hoisted
             //(Token.TagName = ePre) or // hoisted
             (Token.TagName = eSection) or
             (Token.TagName = eSummary) or
             (Token.TagName = eUL)) then 
         begin
            if (not StackOfOpenElementsHasInScope(Token.TagName)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected end tag'); {$ENDIF}
            end
            else
            begin
               GenerateImpliedEndTags();
               {$IFDEF PARSEERROR}
                  if (not CurrentNode.IsIdentity(nsHTML, Token.TagName)) then
                     ParseError('unexpected end tag');
               {$ENDIF}
               repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, Token.TagName);
            end;
         end
         else
         if (Token.TagName = eForm) then
         begin
            if (not StackOfOpenElementsHas(nsHTML, eTemplate)) then
            begin
               Node := FFormElementPointer;
               FFormElementPointer := nil;
               if ((not Assigned(Node)) or (not StackOfOpenElementsHasInScope(Node))) then
               begin
                  {$IFDEF PARSEERROR} ParseError('unexpected form end tag'); {$ENDIF}
                  exit;
               end;
               GenerateImpliedEndTags();
               {$IFDEF PARSEERROR}
                  if (CurrentNode <> Node) then
                     ParseError('unexpected form end tag');
               {$ENDIF}
               FStackOfOpenElements.Remove(Node);
            end
            else
            begin
               if (not StackOfOpenElementsHasInScope(eForm)) then
               begin
                  {$IFDEF PARSEERROR} ParseError('unexpected form end tag'); {$ENDIF}
                  exit;
               end;
               GenerateImpliedEndTags();
               {$IFDEF PARSEERROR}
                  if (not CurrentNode.IsIdentity(nsHTML, eTable)) then
                     ParseError('unexpected form end tag');
               {$ENDIF}
               repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, eTable);
            end;
         end
         else
         if ((Token.TagName = eH1) or
             (Token.TagName = eH2) or
             (Token.TagName = eH3) or
             (Token.TagName = eH4) or
             (Token.TagName = eH5) or
             (Token.TagName = eH6)) then
         begin
            if (not StackOfOpenElementsHasInScope(propHeading)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected heading end tag'); {$ENDIF}
            end
            else
            begin
               GenerateImpliedEndTags();
               {$IFDEF PARSEERROR}
                  if (not CurrentNode.IsIdentity(nsHTML, Token.TagName)) then
                     ParseError('unexpected heading end tag');
               {$ENDIF}
               repeat until FStackOfOpenElements.Pop().HasProperties(propHeading);
            end;
         end
         else
         // if (Token.TagName = eSarcasm) then
         // begin
         //    DeepBreath();
         //    AnyOtherEndTag();
         // end
         // else
         if (//(Token.TagName = eA) or // hoisted
             (Token.TagName = eB) or
             (Token.TagName = eBig) or
             //(Token.TagName = eCode) or // hoisted
             (Token.TagName = eEm) or
             (Token.TagName = eFont) or
             //(Token.TagName = eI) or // hoisted
             (Token.TagName = eNoBr) or
             (Token.TagName = eS) or
             (Token.TagName = eSmall) or
             (Token.TagName = eStrike) or
             (Token.TagName = eStrong) or
             (Token.TagName = eTT) or
             (Token.TagName = eU)) then
         begin
            CallAdoptionAgency(Token.TagName);
         end
         else
         if ((Token.TagName = eApplet) or
             (Token.TagName = eMarquee) or
             (Token.TagName = eObject)) then
         begin
            if (not StackOfOpenElementsHasInScope(Token.TagName)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected end tag'); {$ENDIF}
            end
            else
            begin
               GenerateImpliedEndTags();
               {$IFDEF PARSEERROR}
                  if (not CurrentNode.IsIdentity(nsHTML, Token.TagName)) then
                     ParseError('unexpected end tag');
               {$ENDIF}
               repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, Token.TagName);
               ClearTheListOfActiveFormattingElementsUpToTheLastMarker();
            end;
         end
         else
         if (Token.TagName = eBr) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected br end tag'); {$ENDIF}
            Token.TakeAttributes().Free();
            ReconstructTheActiveFormattingElements();
            InsertAnHTMLElementFor(Token);
            FStackOfOpenElements.Pop();
            FFramesetOkFlag := False;
         end
         else
         begin
            // any other end tag
            AnyOtherEndTag();
         end;
      tkExtraCharacters:
         begin
            ReconstructTheActiveFormattingElements();
            InsertCharacters(Token.ExtraChars);
            FFramesetOkFlag := False;
         end;
      tkComment:
         begin
            InsertAComment(Token);
         end;

      // remaining states didn't happen often enough to matter
      {$IFDEF PARSEERROR}
         tkNullCharacter:
            ParseError('unexpected null in text');
      {$ENDIF}
      tkExtraSpaceCharacter:
         begin
            ReconstructTheActiveFormattingElements();
            InsertCharacters(Token.ExtraChars);
         end;
      {$IFDEF PARSEERROR}
         tkDOCTYPE:
               ParseError('unexpected DOCTYPE');
      {$ENDIF}
      tkEOF:
         begin
            if (FStackOfTemplateInsertionModes.Length > 0) then
            begin
               TheInTemplateInsertionMode(Token);
            end
            else
            begin
               {$IFDEF PARSEERROR}
                  if (StackOfOpenElementsHasElementOtherThan(propEOFImpliedEndTag)) then
                     ParseError('unexpected end of file');
               {$ENDIF}
               // and then we stop parsing
            end;
         end;
      {$IFDEF PARSEERROR}
      else
         Assert(False);
      {$ENDIF}
   end;
end;

procedure THTMLParser.TheTextInsertionMode(var Token: TToken);
begin
   Assert(Assigned(FDocument));
   Assert(Assigned(FOriginalInsertionMode));
   case (Token.Kind) of
      tkSourceCharacters:
         begin
            InsertCharactersFor(Token);
         end;
      tkExtraCharacters,
      tkExtraSpaceCharacter:
         begin
            InsertCharacters(Token.ExtraChars);
         end;
      tkEOF:
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected EOF'); {$ENDIF}
            FStackOfOpenElements.Pop();
            FInsertionMode := FOriginalInsertionMode;
            {$IFOPT C+} FOriginalInsertionMode := nil; {$ENDIF}
            TreeConstructionDispatcher(Token); // http://bugs.freepascal.org/view.php?id=26403
         end;
      tkEndTag:
         begin
            FStackOfOpenElements.Pop();
            FInsertionMode := FOriginalInsertionMode;
            {$IFOPT C+} FOriginalInsertionMode := nil; {$ENDIF}
         end;
      else
         Assert(False); // tkComment, tkDOCTYPE, tkNullCharacter, tkStartTag
   end;
end;

procedure THTMLParser.TheInTableInsertionMode(var Token: TToken);

   procedure ClearTheStackBackToATableContext(); inline;
   begin
      while (not CurrentNode.HasProperties(propTableScope)) do
         FStackOfOpenElements.Pop();
   end;

begin
   {$IFDEF VERBOSETABLE} if (DebugNow) then Writeln('"in table" with token kind ', Token.Kind); {$ENDIF}
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkNullCharacter,
      tkSourceCharacters,
      tkExtraCharacters,
      tkExtraSpaceCharacter:
         begin
            if (CurrentNode.HasProperties(propFosterParent)) then
            begin
               FPendingTableCharacterTokensList := Default(TParserString);
               FPendingTableCharacterTokensListHasNonSpaces := False;
               FOriginalInsertionMode := FInsertionMode;
               FInsertionMode := @TheInTableTextInsertionMode;
               TreeConstructionDispatcher(Token); // http://bugs.freepascal.org/view.php?id=26403
               exit;
            end;
            // otherwise, treat as anything else below
         end;
      tkComment:
         begin
            InsertAComment(Token);
            exit;
         end;
      tkDOCTYPE:
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected DOCTYPE'); {$ENDIF}
            exit;
         end;
      tkStartTag:
         if (Token.TagName = eCaption) then
         begin
            ClearTheStackBackToATableContext();
            InsertMarkerAtEndOfListOfActiveFormattingElements();
            InsertAnHTMLElementFor(Token);
            FInsertionMode := @TheInCaptionInsertionMode;
            exit;
         end
         else
         if (Token.TagName = eColGroup) then
         begin
            ClearTheStackBackToATableContext();
            InsertAnHTMLElementFor(Token);
            FInsertionMode := @TheInColumnGroupInsertionMode;
            exit;
         end
         else
         if (Token.TagName = eCol) then
         begin
            ClearTheStackBackToATableContext();
            InsertAnHTMLElement(ConstructHTMLElement(eColGroup));
            FInsertionMode := @TheInColumnGroupInsertionMode;
            TreeConstructionDispatcher(Token);
            exit;
         end
         else
         if ((Token.TagName = eTBody) or (Token.TagName = eTFoot) or (Token.TagName = eTHead)) then
         begin
            ClearTheStackBackToATableContext();
            InsertAnHTMLElementFor(Token);
            FInsertionMode := @TheInTableBodyInsertionMode;
            exit;
         end
         else
         if ((Token.TagName = eTD) or (Token.TagName = eTH) or (Token.TagName = eTR)) then
         begin
            ClearTheStackBackToATableContext();
            InsertAnHTMLElement(ConstructHTMLElement(eTBody));
            FInsertionMode := @TheInTableBodyInsertionMode;
            TreeConstructionDispatcher(Token);
            exit;
         end
         else
         if (Token.TagName = eTable) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected table start tag in table'); {$ENDIF}
            if (StackOfOpenElementsHasInTableScope(eTable)) then
            begin
               repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, eTable);
               ResetTheInsertionModeAppropriately();
               TreeConstructionDispatcher(Token);
            end;
            exit;
         end
         else
         if ((Token.TagName = eStyle) or (Token.TagName = eScript) or (Token.TagName = eTemplate)) then
         begin
            TheInHeadInsertionMode(Token);
            exit;
         end
         else
         if (Token.TagName = eInput) then
         begin
            if (ASCIILowerCase(Token.GetAttribute('type')) = 'hidden') then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected input start tag in table'); {$ENDIF}
               InsertAnHTMLElementFor(Token);
               FStackOfOpenElements.Pop();
               {$IFDEF PARSEERROR} Token.AcknowledgeSelfClosingFlag(); {$ENDIF}
               exit;
            end;
         end
         else
         if (Token.TagName = eForm) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected form start tag in table'); {$ENDIF}
            if ((not StackOfOpenElementsHas(nsHTML, eTemplate)) and (not Assigned(FFormElementPointer))) then
            begin
               FFormElementPointer := InsertAnHTMLElementFor(Token);
               FStackOfOpenElements.Pop();
            end;
            exit;
         end;
      tkEndTag:
         if (Token.TagName = eTable) then
         begin
            if (not StackOfOpenElementsHasInTableScope(eTable)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected table end tag'); {$ENDIF}
            end
            else
            begin
               repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, eTable);
               ResetTheInsertionModeAppropriately();
            end;
            exit;
         end
         else
         if ((Token.TagName = eBody) or 
             (Token.TagName = eCaption) or
             (Token.TagName = eCol) or
             (Token.TagName = eColGroup) or
             (Token.TagName = eHTML) or
             (Token.TagName = eTBody) or
             (Token.TagName = eTD) or
             (Token.TagName = eTFoot) or
             (Token.TagName = eTH) or
             (Token.TagName = eTHead) or
             (Token.TagName = eTR)) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected end tag in table'); {$ENDIF}
            exit;
         end
         else
         if (Token.TagName = eTemplate) then
         begin
            TheInHeadInsertionMode(Token);
            exit;
         end;
      tkEOF:
         begin
            TheInBodyInsertionMode(Token);
            exit;
         end;
      else
         Assert(False);
   end;
   // anything else
   {$IFDEF PARSEERROR}
   case (Token.Kind) of
      tkSourceCharacters: ParseError('unexpected character token in table', Token.GetCharacterCount(FInputStream.Data));
      tkExtraSpaceCharacter,
      tkExtraCharacters: ParseError('unexpected character token in table', Length(Token.ExtraChars)); // $R-
      tkNullCharacter: ParseError('unexpected null in table');
      else
         ParseError('unexpected token in table - foster parenting');
   end;
   {$ENDIF}
   Assert(not FFosterParenting);
   FFosterParenting := True;
   TheInBodyInsertionMode(Token);
   Assert(FFosterParenting);
   FFosterParenting := False;
end;

procedure THTMLParser.TheInTableTextInsertionMode(var Token: TToken);
var
   Placeholder: TCutParserString;
begin
   Assert(Assigned(FDocument));
   Assert(Assigned(FOriginalInsertionMode));
   {$IFDEF VERBOSETABLE} if (DebugNow) then Writeln('"in table text" with token kind ', Token.Kind); {$ENDIF}
   case (Token.Kind) of
      tkNullCharacter: {$IFDEF PARSEERROR} ParseError('unexpected null in table') {$ENDIF};
      tkSourceCharacters:
         begin
            Placeholder := Token.ExtractSourceCharacters(FInputStream.Data);
            FPendingTableCharacterTokensList.AppendDestructively(Placeholder);
            if (ssHaveNonSpace in Token.SpaceState) then
               FPendingTableCharacterTokensListHasNonSpaces := True;
         end;
      tkExtraCharacters:
         begin
            FPendingTableCharacterTokensList.Append(Token.ExtraChars);
            FPendingTableCharacterTokensListHasNonSpaces := True;
         end;
      tkExtraSpaceCharacter:
         begin
            FPendingTableCharacterTokensList.Append(Token.ExtraChars);
         end;
      else
         begin
            {$IFDEF VERBOSETABLE} if (DebugNow) then Writeln('FPendingTableCharacterTokensList: "', FPendingTableCharacterTokensList, '"'); {$ENDIF}
            if (FPendingTableCharacterTokensListHasNonSpaces) then
            begin
               // inlined copy of the "anything else" entry in the "in table" insertion mode
               {$IFDEF PARSEERROR}
                  // XXX This should really be optimised somehow
                  ParseError('unexpected text in table', Length(FPendingTableCharacterTokensList.AsString)); // $R-
               {$ENDIF}
               Assert(not FFosterParenting);
               FFosterParenting := True;
               // inline copy of the "any other character token" entry in the "in body" insertion mode
               begin
                  ReconstructTheActiveFormattingElements();
                  InsertCharacters(FPendingTableCharacterTokensList);
                  Assert(not FFramesetOkFlag);
               end;
               // back to the "anything else" entry in the "in table" insertion mode
               Assert(FFosterParenting);
               FFosterParenting := False;
            end
            else
               InsertCharacters(FPendingTableCharacterTokensList);
            Assert(Assigned(FOriginalInsertionMode));
            FInsertionMode := FOriginalInsertionMode;
            {$IFOPT C+} FOriginalInsertionMode := nil; {$ENDIF}
            TreeConstructionDispatcher(Token);
         end;
   end;
end;

procedure THTMLParser.TheInCaptionInsertionMode(var Token: TToken);
begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkStartTag:
         if ((Token.TagName = eCaption) or
             (Token.TagName = eCol) or
             (Token.TagName = eColGroup) or
             (Token.TagName = eTBody) or
             (Token.TagName = eTD) or
             (Token.TagName = eTFoot) or
             (Token.TagName = eTH) or
             (Token.TagName = eTHead) or
             (Token.TagName = eTR)) then
         begin
            if (not StackOfOpenElementsHasInTableScope(eCaption)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected start tag in caption'); {$ENDIF}
               exit;
            end;
            GenerateImpliedEndTags();
            {$IFDEF PARSEERROR}
               if (not CurrentNode.IsIdentity(nsHTML, eCaption)) then
                  ParseError('unexpected start tag in caption');
            {$ENDIF}
            repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, eCaption);
            ClearTheListOfActiveFormattingElementsUpToTheLastMarker();
            FInsertionMode := @TheInTableInsertionMode;
            TreeConstructionDispatcher(Token); // http://bugs.freepascal.org/view.php?id=26403
            exit;
         end;
      tkEndTag:
         if (Token.TagName = eCaption) then
         begin
            if (not StackOfOpenElementsHasInTableScope(eCaption)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected caption end tag in caption'); {$ENDIF}
               exit;
            end;
            GenerateImpliedEndTags();
            {$IFDEF PARSEERROR}
               if (not CurrentNode.IsIdentity(nsHTML, eCaption)) then
                  ParseError('unexpected caption end tag in caption');
            {$ENDIF}
            repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, eCaption);
            ClearTheListOfActiveFormattingElementsUpToTheLastMarker();
            FInsertionMode := @TheInTableInsertionMode;
            exit;
         end
         else
         if (Token.TagName = eTable) then
         begin
            if (not StackOfOpenElementsHasInTableScope(eCaption)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected table end tag in caption'); {$ENDIF}
               exit;
            end;
            GenerateImpliedEndTags();
            {$IFDEF PARSEERROR}
               if (not CurrentNode.IsIdentity(nsHTML, eCaption)) then
                  ParseError('unexpected table end tag in caption');
            {$ENDIF}
            repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, eCaption);
            ClearTheListOfActiveFormattingElementsUpToTheLastMarker();
            FInsertionMode := @TheInTableInsertionMode;
            TreeConstructionDispatcher(Token);
            exit;
         end
         else
         if ((Token.TagName = eBody) or
             (Token.TagName = eCol) or
             (Token.TagName = eColGroup) or
             (Token.TagName = eHTML) or
             (Token.TagName = eTBody) or
             (Token.TagName = eTD) or
             (Token.TagName = eTFoot) or
             (Token.TagName = eTH) or
             (Token.TagName = eTHead) or
             (Token.TagName = eTR)) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected end tag in caption'); {$ENDIF}
            exit;
         end;
   end;
   // anything else
   TheInBodyInsertionMode(Token);
end;

procedure THTMLParser.TheInColumnGroupInsertionMode(var Token: TToken);
begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkSourceCharacters:
         begin
            if (ssHaveNonSpace in Token.SpaceState) then
            begin
               if (ssHaveLeadingSpace in Token.SpaceState) then
                  InsertLeadingSpacesFor(Token); // http://bugs.freepascal.org/view.php?id=26403
               // fall through to "anything else" clause
            end
            else
            begin
               InsertCharactersFor(Token);
               exit;
            end;
         end;
      tkExtraSpaceCharacter:
         begin
            InsertCharacters(Token.ExtraChars);
            exit;
         end;
      tkComment:
         begin
            InsertAComment(Token);
            exit;
         end;
      tkDOCTYPE:
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected DOCTYPE'); {$ENDIF}
            exit;
         end;
      tkStartTag:
         if (Token.TagName = eHTML) then
         begin
            TheInBodyInsertionMode(Token);
            exit;
         end
         else
         if (Token.TagName = eCol) then
         begin
            InsertAnHTMLElementFor(Token);
            FStackOfOpenElements.Pop();
            {$IFDEF PARSEERROR} Token.AcknowledgeSelfClosingFlag(); {$ENDIF}
            exit;
         end
         else
         if (Token.TagName = eTemplate) then
         begin
            TheInHeadInsertionMode(Token);
            exit;
         end;
      tkEndTag:
         if (Token.TagName = eColGroup) then
         begin
            if (not CurrentNode.IsIdentity(nsHTML, eColGroup)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected colgroup end tag'); {$ENDIF}
            end
            else
            begin
               FStackOfOpenElements.Pop();
               FInsertionMode := @TheInTableInsertionMode;
            end;
            exit;
         end
         else
         if (Token.TagName = eCol) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected col end tag'); {$ENDIF}
            exit;
         end
         else
         if (Token.TagName = eTemplate) then
         begin
            TheInHeadInsertionMode(Token);
            exit;
         end;
      tkEOF:
         begin
            TheInBodyInsertionMode(Token);
            exit;
         end;
      else
         Assert(False);
   end;
   // anything else
   if (not CurrentNode.IsIdentity(nsHTML, eColGroup)) then
   begin
      {$IFDEF PARSEERROR} ParseError('unexpected token'); {$ENDIF}
   end
   else
   begin
      FStackOfOpenElements.Pop();
      FInsertionMode := @TheInTableInsertionMode;
      TreeConstructionDispatcher(Token);
   end;
end;

procedure THTMLParser.TheInTableBodyInsertionMode(var Token: TToken);

   procedure ClearTheStackBackToATableBodyContext(); inline;
   begin
      while (not CurrentNode.HasProperties(propTBodyContext)) do
         FStackOfOpenElements.Pop();
   end;

begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkStartTag:
         if (Token.TagName = eTR) then
         begin
            ClearTheStackBackToATableBodyContext();
            InsertAnHTMLElementFor(Token);
            FInsertionMode := @TheInRowInsertionMode;
            exit;
         end
         else
         if ((Token.TagName = eTH) or (Token.TagName = eTD)) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected cell start tag in table body'); {$ENDIF}
            ClearTheStackBackToATableBodyContext();
            InsertAnHTMLElement(ConstructHTMLElement(eTR));
            FInsertionMode := @TheInRowInsertionMode;
            TreeConstructionDispatcher(Token); // http://bugs.freepascal.org/view.php?id=26403
            exit;
         end
         else
         if ((Token.TagName = eCaption) or
             (Token.TagName = eCol) or
             (Token.TagName = eColGroup) or
             (Token.TagName = eTBody) or
             (Token.TagName = eTFoot) or
             (Token.TagName = eTHead)) then
         begin
            if (not StackOfOpenElementsHasInTableScope(propTableSection)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected start tag in table body'); {$ENDIF}
               exit;
            end;
            ClearTheStackBackToATableBodyContext();
            Assert(CurrentNode.HasProperties(propTableSection));
            FStackOfOpenElements.Pop();
            FInsertionMode := @TheInTableInsertionMode;
            TreeConstructionDispatcher(Token);
            exit;
         end;
      tkEndTag:
         if ((Token.TagName = eTBody) or
             (Token.TagName = eTFoot) or
             (Token.TagName = eTHead)) then
         begin
            if (not StackOfOpenElementsHasInTableScope(Token.TagName)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected end tag in table body'); {$ENDIF}
               exit;
            end;
            ClearTheStackBackToATableBodyContext();
            Assert(CurrentNode.IsIdentity(nsHTML, Token.TagName));
            FStackOfOpenElements.Pop();
            FInsertionMode := @TheInTableInsertionMode;
            exit;
         end
         else
         if (Token.TagName = eTable) then
         begin
            if (not StackOfOpenElementsHasInTableScope(propTableSection)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected table end tag in table body'); {$ENDIF}
               exit;
            end;
            ClearTheStackBackToATableBodyContext();
            Assert(CurrentNode.HasProperties(propTableSection));
            FStackOfOpenElements.Pop();
            FInsertionMode := @TheInTableInsertionMode;
            TreeConstructionDispatcher(Token);
            exit;
         end
         else
         if ((Token.TagName = eBody) or
             (Token.TagName = eCaption) or
             (Token.TagName = eCol) or
             (Token.TagName = eColGroup) or
             (Token.TagName = eHTML) or
             (Token.TagName = eTD) or
             (Token.TagName = eTH) or
             (Token.TagName = eTR)) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected end tag in table body'); {$ENDIF}
            exit;
         end;
   end;
   // anything else
   TheInTableInsertionMode(Token);
end;

procedure THTMLParser.TheInRowInsertionMode(var Token: TToken);

   procedure ClearTheStackBackToATableRowContext(); inline;
   begin
      while (not CurrentNode.HasProperties(propTRContext)) do
         FStackOfOpenElements.Pop();
   end;

   procedure CloseRow();
   begin
      ClearTheStackBackToATableRowContext();
      Assert(CurrentNode.IsIdentity(nsHTML, eTR));
      FStackOfOpenElements.Pop();
      FInsertionMode := @TheInTableBodyInsertionMode;
   end;

begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkStartTag:
         if ((Token.TagName = eTH) or (Token.TagName = eTD)) then
         begin
            ClearTheStackBackToATableRowContext();
            InsertAnHTMLElementFor(Token);
            FInsertionMode := @TheInCellInsertionMode;
            InsertMarkerAtEndOfListOfActiveFormattingElements();
            exit;
         end
         else
         if ((Token.TagName = eCaption) or
             (Token.TagName = eCol) or
             (Token.TagName = eColGroup) or
             (Token.TagName = eTBody) or
             (Token.TagName = eTFoot) or
             (Token.TagName = eTHead) or
             (Token.TagName = eTR)) then
         begin
            if (not StackOfOpenElementsHasInTableScope(eTR)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected start tag in table row'); {$ENDIF}
            end
            else
            begin
               CloseRow();
               TreeConstructionDispatcher(Token); // http://bugs.freepascal.org/view.php?id=26403
            end;
            exit;
         end;
      tkEndTag:
         if (Token.TagName = eTR) then
         begin
            if (not StackOfOpenElementsHasInTableScope(eTR)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected tr end tag in table row'); {$ENDIF}
            end
            else
            begin
               CloseRow();
            end;
            exit;
         end
         else
         if (Token.TagName = eTable) then
         begin
            if (not StackOfOpenElementsHasInTableScope(eTR)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected table end tag in table row'); {$ENDIF}
            end
            else
            begin
               CloseRow();
               TreeConstructionDispatcher(Token);
            end;
            exit;
         end
         else
         if ((Token.TagName = eTBody) or
             (Token.TagName = eTFoot) or
             (Token.TagName = eTHead)) then
         begin
            if (not StackOfOpenElementsHasInTableScope(Token.TagName)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected end tag in table row'); {$ENDIF}
            end
            else
            if (StackOfOpenElementsHasInTableScope(eTR)) then
            begin
               CloseRow();
               TreeConstructionDispatcher(Token);
            end;
            exit;
         end
         else
         if ((Token.TagName = eBody) or
             (Token.TagName = eCaption) or
             (Token.TagName = eCol) or
             (Token.TagName = eColGroup) or
             (Token.TagName = eHTML) or
             (Token.TagName = eTD) or
             (Token.TagName = eTH)) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected end tag in table row'); {$ENDIF}
            exit;
         end;
   end;
   // anything else
   TheInTableBodyInsertionMode(Token);
end;

procedure THTMLParser.TheInCellInsertionMode(var Token: TToken);

   procedure CloseTheCell(); inline;
   begin
      Assert(StackOfOpenElementsHasInTableScope(eTD) xor StackOfOpenElementsHasInTableScope(eTH));
      GenerateImpliedEndTags();
      {$IFDEF PARSEERROR}
      if (not CurrentNode.HasProperties(propTableCell)) then
         ParseError('forcibly closing cell');
      {$ENDIF}
      repeat until FStackOfOpenElements.Pop().HasProperties(propTableCell);
      ClearTheListOfActiveFormattingElementsUpToTheLastMarker();
      FInsertionMode := @TheInRowInsertionMode;
   end;

begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkStartTag:
         if ((Token.TagName = eCaption) or
             (Token.TagName = eCol) or
             (Token.TagName = eColGroup) or
             (Token.TagName = eTBody) or
             (Token.TagName = eTD) or
             (Token.TagName = eTFoot) or
             (Token.TagName = eTH) or
             (Token.TagName = eTHead) or
             (Token.TagName = eTR)) then
         begin
            if (not StackOfOpenElementsHasInTableScope(propTableCell)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected start tag in table cell'); {$ENDIF}
            end
            else
            begin
               CloseTheCell();
               TreeConstructionDispatcher(Token); // http://bugs.freepascal.org/view.php?id=26403
            end;
            exit;
         end;
      tkEndTag:
         if ((Token.TagName = eTD) or
             (Token.TagName = eTH)) then
         begin
            if (not StackOfOpenElementsHasInTableScope(Token.TagName)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected end tag in table cell'); {$ENDIF}
            end
            else
            begin
               GenerateImpliedEndTags();
               {$IFDEF PARSEERROR}
                  if (not CurrentNode.IsIdentity(nsHTML, Token.TagName)) then
                     ParseError('unexpected end tag in table cell');
               {$ENDIF}
               repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, Token.TagName);
               ClearTheListOfActiveFormattingElementsUpToTheLastMarker();
               FInsertionMode := @TheInRowInsertionMode;
            end;
            exit;
         end
         else
         if ((Token.TagName = eBody) or
             (Token.TagName = eCaption) or
             (Token.TagName = eCol) or
             (Token.TagName = eColGroup) or
             (Token.TagName = eHTML)) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected end tag in table cell'); {$ENDIF}
            exit;
         end
         else
         if ((Token.TagName = eTable) or
             (Token.TagName = eTBody) or
             (Token.TagName = eTFoot) or
             (Token.TagName = eTHead) or
             (Token.TagName = eTR)) then
         begin
            if (not StackOfOpenElementsHasInTableScope(Token.TagName)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected end tag in table cell'); {$ENDIF}
            end
            else
            begin
               CloseTheCell();
               TreeConstructionDispatcher(Token);
            end;
            exit;
         end;
   end;
   // anything else
   TheInBodyInsertionMode(Token);
end;

procedure THTMLParser.TheInSelectInsertionMode(var Token: TToken);
begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkNullCharacter:
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected null in select'); {$ENDIF}
            exit;
         end;
      tkSourceCharacters:
         begin
            InsertCharactersFor(Token);
            exit;
         end;
      tkExtraCharacters,
      tkExtraSpaceCharacter:
         begin
            InsertCharacters(Token.ExtraChars);
            exit;
         end;
      tkComment:
         begin
            InsertAComment(Token); // http://bugs.freepascal.org/view.php?id=26403
            exit;
         end;
      tkDOCTYPE:
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected DOCTYPE'); {$ENDIF}
            exit;
         end;
      tkStartTag:
         if (Token.TagName = eHTML) then
         begin
            TheInBodyInsertionMode(Token);
            exit;
         end
         else
         if (Token.TagName = eOption) then
         begin
            if (CurrentNode.IsIdentity(nsHTML, eOption)) then
               FStackOfOpenElements.Pop();
            InsertAnHTMLElementFor(Token);
            exit;
         end
         else
         if (Token.TagName = eOptGroup) then
         begin
            if (CurrentNode.IsIdentity(nsHTML, eOption)) then
               FStackOfOpenElements.Pop();
            if (CurrentNode.IsIdentity(nsHTML, eOptGroup)) then
               FStackOfOpenElements.Pop();
            InsertAnHTMLElementFor(Token);
            exit;
         end
         else
         if (Token.TagName = eSelect) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected select start tag in select'); {$ENDIF}
            if (StackOfOpenElementsHasInSelectScope(eSelect)) then
            begin
               repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, eSelect);
               ResetTheInsertionModeAppropriately();
            end;
            exit;
         end
         else
         if ((Token.TagName = eInput) or (Token.TagName = eKeygen) or (Token.TagName = eTextArea)) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected start tag in select'); {$ENDIF}
            if (StackOfOpenElementsHasInSelectScope(eSelect)) then
            begin
               repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, eSelect);
               ResetTheInsertionModeAppropriately();
               TreeConstructionDispatcher(Token);
            end;
            exit;
         end
         else
         if ((Token.TagName = eScript) or (Token.TagName = eTemplate)) then
         begin
            TheInHeadInsertionMode(Token);
            exit;
         end;
      tkEndTag:
         if (Token.TagName = eOptGroup) then
         begin
            Assert(FStackOfOpenElements.Length >= 2);
            if (CurrentNode.IsIdentity(nsHTML, eOption) and FStackOfOpenElements[FStackOfOpenElements.Length-2].IsIdentity(nsHTML, eOptGroup)) then // $R-
               FStackOfOpenElements.Pop();
            if (CurrentNode.IsIdentity(nsHTML, eOptGroup)) then
               FStackOfOpenElements.Pop()
            {$IFDEF PARSEERROR}
            else
               ParseError('unexpected optgroup end tag in select')
            {$ENDIF};
            exit;
         end
         else
         if (Token.TagName = eOption) then
         begin
            if (CurrentNode.IsIdentity(nsHTML, eOption)) then
               FStackOfOpenElements.Pop()
            {$IFDEF PARSEERROR}
            else
               ParseError('unexpected option end tag in select')
            {$ENDIF};
            exit;
         end
         else
         if (Token.TagName = eSelect) then
         begin
            if (not StackOfOpenElementsHasInSelectScope(eSelect)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected select start tag in select') {$ENDIF}
            end
            else
            begin
               repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, eSelect);
               ResetTheInsertionModeAppropriately();
            end;
            exit;
         end
         else
         if (Token.TagName = eTemplate) then
         begin
            TheInHeadInsertionMode(Token);
            exit;
         end;
      tkEOF:
         begin
            TheInBodyInsertionMode(Token);
            exit;
         end;
      else
         Assert(False);
   end;
   // anything else
   {$IFDEF PARSEERROR} ParseError('unexpected token in select'); {$ENDIF}
end;

procedure THTMLParser.TheInSelectInTableInsertionMode(var Token: TToken);
begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkStartTag:
         if ((Token.TagName = eCaption) or
             (Token.TagName = eTable) or
             (Token.TagName = eTBody) or
             (Token.TagName = eTFoot) or
             (Token.TagName = eTHead) or
             (Token.TagName = eTR) or
             (Token.TagName = eTD) or
             (Token.TagName = eTH)) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected start tag in select'); {$ENDIF}
            repeat until FStackOfOpenElements.Pop.IsIdentity(nsHTML, eSelect);
            ResetTheInsertionModeAppropriately();
            TreeConstructionDispatcher(Token); // http://bugs.freepascal.org/view.php?id=26403
            exit;
         end;
      tkEndTag:
         if ((Token.TagName = eCaption) or
             (Token.TagName = eTable) or
             (Token.TagName = eTBody) or
             (Token.TagName = eTFoot) or
             (Token.TagName = eTHead) or
             (Token.TagName = eTR) or
             (Token.TagName = eTD) or
             (Token.TagName = eTH)) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected end tag in select'); {$ENDIF}
            if (StackOfOpenElementsHasInTableScope(Token.TagName)) then
            begin
               repeat until FStackOfOpenElements.Pop.IsIdentity(nsHTML, eSelect);
               ResetTheInsertionModeAppropriately();
               TreeConstructionDispatcher(Token);
            end;
            exit;
         end;
   end;
   // anything else
   TheInSelectInsertionMode(Token);
end;

procedure THTMLParser.TheInTemplateInsertionMode(var Token: TToken);
begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkNullCharacter, tkSourceCharacters, tkExtraSpaceCharacter, tkExtraCharacters, tkComment, tkDOCTYPE:
         TheInBodyInsertionMode(Token); // http://bugs.freepascal.org/view.php?id=26403
      tkStartTag:
         if ((Token.TagName = eBase) or (Token.TagName = eBaseFont) or (Token.TagName = eBGSound) or (Token.TagName = eLink) or
             (Token.TagName = eMeta) or (Token.TagName = eNoFrames) or (Token.TagName = eScript) or (Token.TagName = eStyle) or
             (Token.TagName = eTemplate) or (Token.TagName = eTitle)) then
         begin
            TheInHeadInsertionMode(Token);
         end
         else
         if ((Token.TagName = eCaption) or
             (Token.TagName = eColGroup) or
             (Token.TagName = eTBody) or
             (Token.TagName = eTFoot) or
             (Token.TagName = eTHead)) then
         begin
            FStackOfTemplateInsertionModes.Pop();
            FStackOfTemplateInsertionModes.Push(@TheInTableInsertionMode);
            FInsertionMode := @TheInTableInsertionMode;
            TreeConstructionDispatcher(Token);
         end
         else
         if (Token.TagName = eCol) then
         begin
            FStackOfTemplateInsertionModes.Pop();
            FStackOfTemplateInsertionModes.Push(@TheInColumnGroupInsertionMode);
            FInsertionMode := @TheInColumnGroupInsertionMode;
            TreeConstructionDispatcher(Token);
         end
         else
         if (Token.TagName = eTR) then
         begin
            FStackOfTemplateInsertionModes.Pop();
            FStackOfTemplateInsertionModes.Push(@TheInTableBodyInsertionMode);
            FInsertionMode := @TheInTableBodyInsertionMode;
            TreeConstructionDispatcher(Token);
         end
         else
         if ((Token.TagName = eTD) or (Token.TagName = eTH)) then
         begin
            FStackOfTemplateInsertionModes.Pop();
            FStackOfTemplateInsertionModes.Push(@TheInRowInsertionMode);
            FInsertionMode := @TheInRowInsertionMode;
            TreeConstructionDispatcher(Token);
         end
         else
         begin
            FStackOfTemplateInsertionModes.Pop();
            FStackOfTemplateInsertionModes.Push(@TheInBodyInsertionMode);
            FInsertionMode := @TheInBodyInsertionMode;
            TreeConstructionDispatcher(Token);
         end;
      tkEndTag:
         if (Token.TagName = eTemplate) then
         begin
            TheInHeadInsertionMode(Token);
         end
         else
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected token in template'); {$ENDIF}
         end;
      tkEOF:
         begin
            if (StackOfOpenElementsHas(nsHTML, eTemplate)) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected end of file in template'); {$ENDIF}
               repeat until FStackOfOpenElements.Pop().IsIdentity(nsHTML, eTemplate);
               ClearTheListOfActiveFormattingElementsUpToTheLastMarker();
               FStackOfTemplateInsertionModes.Pop();
               ResetTheInsertionModeAppropriately();
               TreeConstructionDispatcher(Token);
            end;
            // else we just stop parsing
         end;
      else
         Assert(False);
   end;
end;

procedure THTMLParser.TheAfterBodyInsertionMode(var Token: TToken);
begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkSourceCharacters:
         begin
            if (ssHaveNonSpace in Token.SpaceState) then
            begin
               if (ssHaveLeadingSpace in Token.SpaceState) then
               begin
                  ReconstructTheActiveFormattingElements();
                  InsertLeadingSpacesFor(Token); // http://bugs.freepascal.org/view.php?id=26403
               end;
               // fall through to "anything else" clause
            end
            else
            begin
               ReconstructTheActiveFormattingElements();
               InsertCharactersFor(Token);
               exit;
            end;
         end;
      tkExtraSpaceCharacter:
         begin
            ReconstructTheActiveFormattingElements();
            InsertCharacters(Token.ExtraChars);
            exit;
         end;
      tkComment:
         begin
            FStackOfOpenElements[0].AppendChild(CreateACommentFor(Token));
            exit;
         end;
      tkDOCTYPE:
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected DOCTYPE'); {$ENDIF}
            exit;
         end;
      tkStartTag:
         if (Token.TagName = eHTML) then
         begin
            TheInBodyInsertionMode(Token);
            exit;
         end;
      tkEndTag:
         if (Token.TagName = eHTML) then
         begin
            if (FFragmentParsingMode) then
            begin
               {$IFDEF PARSEERROR} ParseError('unexpected html end tag in fragment parser') {$ENDIF}
            end
            else
               FInsertionMode := @TheAfterAfterBodyInsertionMode;
            exit;
         end;
      tkEOF:
         begin
            // and then we stop parsing
            exit;
         end;
      else
         Assert(False);
   end;
   // anything else
   {$IFDEF PARSEERROR} ParseError('unexpected token after html'); {$ENDIF}
   FInsertionMode := @TheInBodyInsertionMode;
   TreeConstructionDispatcher(Token);
end;

procedure THTMLParser.TheInFramesetInsertionMode(var Token: TToken);
begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      {$IFDEF PARSEERROR} tkNullCharacter: ParseError('unexpected null'); {$ENDIF}
      tkSourceCharacters:
         begin
            if (ssHaveNonSpace in Token.SpaceState) then
            begin
               if (ssHaveLeadingSpace in Token.SpaceState) then
                  InsertLeadingSpacesFor(Token); // http://bugs.freepascal.org/view.php?id=26403
               repeat
                  {$IFDEF PARSEERROR}
                     ParseError('unexpected character token', Token.SkipLeadingNonSpaces(FInputStream.Data));
                  {$ELSE}
                     Token.SkipLeadingNonSpaces(FInputStream.Data);
                  {$ENDIF}
                  if (ssHaveLeadingSpace in Token.SpaceState) then
                  begin
                     InsertLeadingSpacesFor(Token);
                  end
                  else
                     break;
               until forever;
            end
            else
            begin
               InsertCharactersFor(Token);
            end;
         end;
      tkExtraSpaceCharacter: InsertCharacters(Token.ExtraChars);
      {$IFDEF PARSEERROR} tkExtraCharacters: ParseError('unexpected character token', Length(Token.ExtraChars)); {$ENDIF} // $R-
      tkComment: InsertAComment(Token);
      {$IFDEF PARSEERROR} tkDOCTYPE: ParseError('unexpected DOCTYPE'); {$ENDIF}
      tkStartTag:
         if (Token.TagName = eHTML) then
         begin
            TheInBodyInsertionMode(Token);
         end
         else
         if (Token.TagName = eFrameSet) then
         begin
            InsertAnHTMLElementFor(Token);
         end
         else
         if (Token.TagName = eFrame) then
         begin
            InsertAnHTMLElementFor(Token);
            FStackOfOpenElements.Pop();
            {$IFDEF PARSEERROR} Token.AcknowledgeSelfClosingFlag(); {$ENDIF}
         end
         else
         if (Token.TagName = eNoFrames) then
         begin
            TheInHeadInsertionMode(Token);
         end
         else
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected start tag token'); {$ENDIF}
         end;
      tkEndTag:
         if (Token.TagName = eFrameSet) then
         begin
            if (FStackOfOpenElements.Length = 1) then
            begin
               Assert(CurrentNode.IsIdentity(nsHTML, eHTML));
               {$IFDEF PARSEERROR} ParseError('unexpected frameset end tag'); {$ENDIF}
            end
            else
            begin
               FStackOfOpenElements.Pop();
               if ((not FFragmentParsingMode) and (not CurrentNode.IsIdentity(nsHTML, eFrameSet))) then
                  FInsertionMode := @TheAfterFramesetInsertionMode;
            end;
         end
         else
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected end tag token'); {$ENDIF}
         end;
      tkEOF:
         begin
            {$IFDEF PARSEERROR}
               if (FStackOfOpenElements.Length <> 1) then
                  ParseError('unexpected end of file')
               else
                  Assert(CurrentNode.IsIdentity(nsHTML, eHTML));
            {$ENDIF}
            // and then we stop parsing
         end;
      {$IFDEF PARSEERROR}
      else
         Assert(False);
      {$ENDIF}
   end;
end;

procedure THTMLParser.TheAfterFramesetInsertionMode(var Token: TToken);
begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      {$IFDEF PARSEERROR} tkNullCharacter: ParseError('unexpected character token after frameset'); {$ENDIF}
      tkSourceCharacters:
         begin
            if (ssHaveNonSpace in Token.SpaceState) then
            begin
               if (ssHaveLeadingSpace in Token.SpaceState) then
                  InsertLeadingSpacesFor(Token); // http://bugs.freepascal.org/view.php?id=26403
               repeat
                  {$IFDEF PARSEERROR}
                     ParseError('unexpected character token after frameset', Token.SkipLeadingNonSpaces(FInputStream.Data));
                  {$ELSE}
                     Token.SkipLeadingNonSpaces(FInputStream.Data);
                  {$ENDIF}
                  if (ssHaveLeadingSpace in Token.SpaceState) then
                     InsertLeadingSpacesFor(Token)
                  else
                     break;
               until forever;
            end
            else
            begin
               InsertCharactersFor(Token);
            end;
         end;
      tkExtraSpaceCharacter: InsertCharacters(Token.ExtraChars);
      {$IFDEF PARSEERROR} tkExtraCharacters: ParseError('unexpected character token after frameset', Length(Token.ExtraChars)); {$ENDIF} // $R- 
      tkComment: InsertAComment(Token);
      {$IFDEF PARSEERROR} tkDOCTYPE: ParseError('unexpected DOCTYPE'); {$ENDIF}
      tkStartTag:
         if (Token.TagName = eHTML) then
         begin
            TheInBodyInsertionMode(Token);
         end
         else
         if (Token.TagName = eNoFrames) then
         begin
            TheInHeadInsertionMode(Token);
         end
         else
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected start tag token after frameset'); {$ENDIF}
         end;
      tkEndTag:
         if (Token.TagName = eHTML) then
         begin
            FInsertionMode := @TheAfterAfterFramesetInsertionMode;
         end
         else
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected end tag token after frameset'); {$ENDIF}
         end;
      tkEOF: ; // and then we stop parsing
      {$IFDEF PARSEERROR}
      else
         Assert(False);
      {$ENDIF}
   end;
end;

procedure THTMLParser.TheAfterAfterBodyInsertionMode(var Token: TToken);
begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkSourceCharacters:
         begin
            if (ssHaveNonSpace in Token.SpaceState) then
            begin
               if (ssHaveLeadingSpace in Token.SpaceState) then
               begin
                  ReconstructTheActiveFormattingElements();
                  InsertLeadingSpacesFor(Token); // http://bugs.freepascal.org/view.php?id=26403
               end;
               // fall through to "anything else" clause
            end
            else
            begin
               ReconstructTheActiveFormattingElements();
               InsertCharactersFor(Token);
               exit;
            end;
         end;
      tkExtraSpaceCharacter:
         begin
            ReconstructTheActiveFormattingElements();
            InsertCharacters(Token.ExtraChars);
            exit;
         end;
      tkComment:
         begin
            FDocument.AppendChild(CreateACommentFor(Token));
            exit;
         end;
      tkDOCTYPE:
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected DOCTYPE'); {$ENDIF}
            exit;
         end;
      tkStartTag:
         if (Token.TagName = eHTML) then
         begin
            TheInBodyInsertionMode(Token);
            exit;
         end
         else
         if (Token.TagName = eNoFrames) then
         begin
            TheInHeadInsertionMode(Token);
            exit;
         end;
      tkEOF:
         begin
            // and then we stop parsing
            exit;
         end;
      else
         Assert(Token.Kind = tkEndTag);
   end;
   // anything else
   {$IFDEF PARSEERROR} ParseError('unexpected token after html'); {$ENDIF}
   FInsertionMode := @TheInBodyInsertionMode;
   TreeConstructionDispatcher(Token);
end;

procedure THTMLParser.TheAfterAfterFramesetInsertionMode(var Token: TToken);
begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkSourceCharacters:
         begin
            if (ssHaveNonSpace in Token.SpaceState) then
            begin
               if (ssHaveLeadingSpace in Token.SpaceState) then
                  InsertLeadingSpacesFor(Token); // http://bugs.freepascal.org/view.php?id=26403
               repeat
                  {$IFDEF PARSEERROR}
                     ParseError('unexpected character token after html after frameset', Token.SkipLeadingNonSpaces(FInputStream.Data));
                  {$ELSE}
                     Token.SkipLeadingNonSpaces(FInputStream.Data);
                  {$ENDIF}
                  if (ssHaveLeadingSpace in Token.SpaceState) then
                     InsertLeadingSpacesFor(Token)
                  else
                     exit;
               until forever;
            end
            else
            begin
               InsertCharactersFor(Token);
               exit;
            end;
         end;
      tkExtraSpaceCharacter:
         begin
            ReconstructTheActiveFormattingElements();
            InsertCharacters(Token.ExtraChars);
            exit;
         end;
      tkComment:
         begin
            FDocument.AppendChild(CreateACommentFor(Token));
            exit;
         end;
      tkDOCTYPE:
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected DOCTYPE'); {$ENDIF}
            exit;
         end;
      tkStartTag:
         if (Token.TagName = eHTML) then
         begin
            TheInBodyInsertionMode(Token);
            exit;
         end
         else
         if (Token.TagName = eNoFrames) then
         begin
            TheInHeadInsertionMode(Token);
            exit;
         end;
      tkEOF:
         begin
            // and then we stop parsing
            exit;
         end;
      else
         Assert(Token.Kind = tkEndTag);
   end;
   // anything else
   {$IFDEF PARSEERROR} ParseError('unexpected token after html after frameset'); {$ENDIF}
end;

procedure THTMLParser.TheSkipNewLineInsertionMode(var Token: TToken);
begin
   Assert(Assigned(FDocument));
   FInsertionMode := FOriginalInsertionMode;
   case (Token.Kind) of
      tkSourceCharacters:
         begin
            if (not Token.SkipOneLeadingNewline(FInputStream.Data)) then
               exit;
         end;
      tkExtraSpaceCharacter:
         begin
            Assert(Length(Token.ExtraChars) = 1);
            if (Token.ExtraChars[0] = $000A) then
               exit; // skip the one newline character
         end;
   end;
   TreeConstructionDispatcher(Token);
end;

procedure THTMLParser.TheSkipNewLineThenTextInsertionMode(var Token: TToken);
begin
   Assert(Assigned(FDocument));
   FInsertionMode := @TheTextInsertionMode;
   case (Token.Kind) of
      tkSourceCharacters:
         begin
            if (not Token.SkipOneLeadingNewline(FInputStream.Data)) then
               exit;
         end;
      tkExtraSpaceCharacter:
         begin
            Assert(Length(Token.ExtraChars) = 1);
            if (Token.ExtraChars[0] = $000A) then
               exit; // skip the one newline character
         end;
   end;
   TreeConstructionDispatcher(Token);
end;

procedure THTMLParser.TheRulesForParsingTokensInForeignContent(var Token: TToken);

   procedure AnyOtherStartTag();
   begin
      if (AdjustedCurrentNode.NamespaceURL = nsMathML) then
      begin
         Token.AdjustMathMLAttributes();
      end
      else
      if (AdjustedCurrentNode.NamespaceURL = nsSVG) then
      begin
         // tag name adjustment is done by ConstructSVGElement() in webdom.pas
         Token.AdjustSVGAttributes();
      end;
      Token.AdjustForeignAttributes();
      InsertAForeignElementFor(Token, AdjustedCurrentNode.NamespaceURL);
      if (Token.SelfClosingFlag) then
      begin
         FStackOfOpenElements.Pop();
         {$IFDEF PARSEERROR} Token.AcknowledgeSelfClosingFlag(); {$ENDIF}
         // note: <script/> here is handled like any other foreign void start tag, since we don't support scripts
      end;
   end;

var
   NodeIndex: Cardinal;
   Node: TXMLElement;
   Placeholder: TUnicodeCodepointArray;
begin
   Assert(Assigned(FDocument));
   case (Token.Kind) of
      tkNullCharacter:
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected null in foreign content'); {$ENDIF}
            SetLength(Placeholder, 1);
            Placeholder[0] := $FFFD;
            InsertCharacters(Placeholder);
         end;
      tkSourceCharacters:
         begin
            InsertCharactersFor(Token);
            if (ssHaveNonSpace in Token.SpaceState) then
               FFramesetOkFlag := False;
         end;
      tkExtraCharacters:
         begin
            InsertCharacters(Token.ExtraChars);
            FFramesetOkFlag := False;
         end;
      tkExtraSpaceCharacter:
         begin
            InsertCharacters(Token.ExtraChars);
         end;
      tkComment: InsertAComment(Token); // http://bugs.freepascal.org/view.php?id=26403
      {$IFDEF PARSEERROR} tkDOCTYPE: ParseError('unexpected DOCTYPE'); {$ENDIF}
      tkStartTag:
         if ((Token.TagName = eB) or 
             (Token.TagName = eBig) or 
             (Token.TagName = eBlockQuote) or 
             (Token.TagName = eBody) or 
             (Token.TagName = eBr) or 
             (Token.TagName = eCenter) or 
             (Token.TagName = eCode) or 
             (Token.TagName = eDD) or 
             (Token.TagName = eDiv) or 
             (Token.TagName = eDL) or 
             (Token.TagName = eDT) or 
             (Token.TagName = eEm) or 
             (Token.TagName = eEmbed) or 
             (Token.TagName = eH1) or 
             (Token.TagName = eH2) or 
             (Token.TagName = eH3) or 
             (Token.TagName = eH4) or 
             (Token.TagName = eH5) or 
             (Token.TagName = eH6) or 
             (Token.TagName = eHead) or 
             (Token.TagName = eHR) or 
             (Token.TagName = eI) or 
             (Token.TagName = eImg) or 
             (Token.TagName = eLI) or 
             (Token.TagName = eListing) or 
             (Token.TagName = eMenu) or 
             (Token.TagName = eMeta) or 
             (Token.TagName = eNoBr) or 
             (Token.TagName = eOL) or 
             (Token.TagName = eP) or 
             (Token.TagName = ePre) or 
             (Token.TagName = eRuby) or 
             (Token.TagName = eS) or 
             (Token.TagName = eSmall) or 
             (Token.TagName = eSpan) or 
             (Token.TagName = eStrong) or 
             (Token.TagName = eStrike) or 
             (Token.TagName = eSub) or 
             (Token.TagName = eSup) or 
             (Token.TagName = eTable) or 
             (Token.TagName = eTT) or 
             (Token.TagName = eU) or 
             (Token.TagName = eUL) or 
             (Token.TagName = eVar) or
             ((Token.TagName = eFont) and (Token.HasAttributes(['color', 'face', 'size'])))) then
         begin
            {$IFDEF PARSEERROR} ParseError('unexpected HTML-like start tag token in foreign content'); {$ENDIF}
            if (FFragmentParsingMode) then
            begin
               AnyOtherStartTag();
            end
            else
            begin
               repeat
                  FStackOfOpenElements.Pop();
               until CurrentNode.HasSomeProperties(propMathMLTextIntegrationPoint or propHTMLIntegrationPoint or propHTML);
               TreeConstructionDispatcher(Token);
            end;
         end
         else
            AnyOtherStartTag();
      tkEndTag:
         begin
            // note: </script> is handled like any other end tag, since we don't support scripts
            NodeIndex := FStackOfOpenElements.Length-1; // $R-
            Assert(FStackOfOpenElements[NodeIndex] is TXMLElement);
            Node := FStackOfOpenElements[NodeIndex] as TXMLElement;
            {$IFDEF PARSEERROR}
               if (Node.LowerCaseTagName <> Token.TagName) then
                  ParseError('unexpected end tag');
            {$ENDIF}
            repeat
               if (NodeIndex = 0) then
                  break;
               if (Node.LowerCaseTagName = Token.TagName) then
               begin
                  repeat until FStackOfOpenElements.Pop() = Node;
                  exit;
               end;
               Dec(NodeIndex);
               if (FStackOfOpenElements[NodeIndex].NamespaceURL = nsHTML) then
               begin
                  FInsertionMode(Token);
                  break;
               end;
               Assert(FStackOfOpenElements[NodeIndex] is TXMLElement);
               Node := FStackOfOpenElements[NodeIndex] as TXMLElement;
            until forever;
         end;
      {$IFDEF PARSEERROR}
      else
         Assert(False);
      {$ENDIF}
   end;
end;

end.
