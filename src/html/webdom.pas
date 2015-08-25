{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit webdom;

// This library is based on: http://whatwg.org/c

// Apart from being a subset and using FreePascal conventions where
// appropriate, it differs from the spec in the following ways:
//  - <template> elements store their contents in the normal DOM, not
//    in a separate FragmentDocument

interface

uses
   dom, canonicalstrings;

var
   nsHTML, nsMathML, nsSVG: TCanonicalString;
   eA, eAbbr, eAddress, eApplet, eArea, eArticle, eAside, eAudio, eB,
   eBDI, eBDO, eBGSound, eBr, eBase, eBaseFont, eBig, eBlink,
   eBlockQuote, eBody, eButton, eCanvas, eCaption, eCenter, eCite,
   eCode, eCol, eColGroup, eDD, eDL, eDT, eData, eDataList, eDel,
   eDetails, eDfn, eDialog, eDir, eDiv, eEm, eEmbed, eFieldSet,
   eFigCaption, eFigure, eFont, eFooter, eForm, eFrame, eFrameSet,
   eH1, eH2, eH3, eH4, eH5, eH6, eHGroup, eHR, eHTML, eHead, eHeader,
   eI, eIFrame, eImg, eInput, eIns, eIsIndex, eKbd, eKeyGen, eLI,
   eLabel, eLegend, eLink, eListing, eMain, eMap, eMark, eMarquee,
   eMenu, eMenuItem, eMeta, eMeter, eMultiCol, eNav, eNextID, eNoBr,
   eNoEmbed, eNoFrames, eNoScript, eOL, eObject, eOptGroup, eOption,
   eOutput, eP, eParam, ePlaintext, ePre, eProgress, eQ, eRB, eRP,
   eRT, eRuby, eS, eSamp, eScript, eSection, eSelect, eSmall, eSource,
   eSpacer, eSpan, eStrike, eStrong, eStyle, eSub, eSummary, eSup,
   eTBody, eTD, eTFoot, eTH, eTHead, eTR, eTT, eTable, eTemplate,
   eTextArea, eTime, eTitle, eTrack, eU, eUL, eVar, eVideo, eWBr,
   eXMP: TCanonicalString;
   eImage, eMath, eSVG, eMGlyph, eMAlignMark, eAnnotationXML: TCanonicalString; // not HTML elements

const
   // generally useful properties
   propHTML                              : TElementProperties = %00000000000000000000000000000001; // equivalent to NamespaceURL = nsHTML
   propTableSection                      : TElementProperties = %00000000000000000000000000000010; // tbody, tfoot, thead
   propTableCell                         : TElementProperties = %00000000000000000000000000000100; // td, th
   propHeading                           : TElementProperties = %00000000000000000000000000001000; // h1-h6
   propVoidElement                       : TElementProperties = %00000000000000000000000000010000; // <foo/>
   propRawTextElement                    : TElementProperties = %00000000000000000000000000100000; // script, style
   propEscapableRawTextElement           : TElementProperties = %00000000000000000000000001000000; // textarea, title
   propOptionalStartTag                  : TElementProperties = %00000000000000000000000010000000; // see the spec for precisely when they can be omitted
   propOptionalEndTag                    : TElementProperties = %00000000000000000000000100000000; // see the spec for precisely when they can be omitted
   propCannotContainPalpableText         : TElementProperties = %00000000000000000000001000000000; // e.g. <ul>, <dl>
   // rather parser-specific properties
   propAutoclosesP                       : TElementProperties = %00000000000000010000000000000000;
   propFosterParent                      : TElementProperties = %00000000000000100000000000000000;
   propGenericScope                      : TElementProperties = %00000000000001000000000000000000;
   propListItemScope                     : TElementProperties = %00000000000010000000000000000000;
   propButtonScope                       : TElementProperties = %00000000000100000000000000000000;
   propTableScope                        : TElementProperties = %00000000001000000000000000000000;
   propSelectScope                       : TElementProperties = %00000000010000000000000000000000;
   propTBodyContext                      : TElementProperties = %00000000100000000000000000000000;
   propTRContext                         : TElementProperties = %00000001000000000000000000000000;
   propSpecial                           : TElementProperties = %00000010000000000000000000000000;
   propSpecialish                        : TElementProperties = %00000100000000000000000000000000;
   propMathMLTextIntegrationPoint        : TElementProperties = %00001000000000000000000000000000;
   propHTMLIntegrationPoint              : TElementProperties = %00010000000000000000000000000000;
   propBodyImpliedEndTag                 : TElementProperties = %00100000000000000000000000000000; // for </div> and company
   propEOFImpliedEndTag                  : TElementProperties = %01000000000000000000000000000000; // for </html>, </body>, EOF in body
   propTemplateImpliedEndTag             : TElementProperties = %10000000000000000000000000000000; // for </template>

type
   THTMLElement = class(TElement)
    public
      constructor Create(const NewLocalName: TCanonicalString; const NewAttributes: TAttributeHashTable; const NewChildren: array of TNode; const NewProperties: TElementProperties);
      {$IFOPT C+} procedure AfterConstruction(); override; {$ENDIF}
   end;
   THTMLElementClass = class of THTMLElement;

   THTMLMediaElement = class(THTMLElement)
   end;

   THTMLAnchorElement = class(THTMLElement)
   end;

   THTMLAreaElement = class(THTMLElement)
   end;

   THTMLAudioElement = class(THTMLMediaElement)
   end;

   THTMLBaseElement = class(THTMLElement)
   end;

   THTMLBodyElement = class(THTMLElement)
   end;

   THTMLBRElement = class(THTMLElement)
   end;

   THTMLButtonElement = class(THTMLElement)
   end;

   THTMLCanvasElement = class(THTMLElement)
   end;

   THTMLDataElement = class(THTMLElement)
   end;

   THTMLDataListElement = class(THTMLElement)
   end;

   THTMLDetailsElement = class(THTMLElement)
   end;

   THTMLDialogElement = class(THTMLElement)
   end;

   THTMLDivElement = class(THTMLElement)
   end;

   THTMLDListElement = class(THTMLElement)
   end;

   THTMLEmbedElement = class(THTMLElement)
   end;

   THTMLFieldSetElement = class(THTMLElement)
   end;

   THTMLFormElement = class(THTMLElement)
   end;

   THTMLHeadElement = class(THTMLElement)
   end;

   THeadingRank = 1..6;
   THTMLHeadingElement = class(THTMLElement)
    private
      FRank: THeadingRank;
    public
      procedure AfterConstruction(); override;
      property Rank: THeadingRank read FRank;
   end;

   THTMLHRElement = class(THTMLElement)
   end;

   THTMLHTMLElement = class(THTMLElement)
   end;

   THTMLIFrameElement = class(THTMLElement)
   end;

   THTMLImageElement = class(THTMLElement)
   end;

   THTMLInputElement = class(THTMLElement)
   end;

   THTMLKeygenElement = class(THTMLElement)
   end;

   THTMLLabelElement = class(THTMLElement)
   end;

   THTMLLegendElement = class(THTMLElement)
   end;

   THTMLLIElement = class(THTMLElement)
   end;

   THTMLLinkElement = class(THTMLElement)
   end;

   THTMLMapElement = class(THTMLElement)
   end;

   THTMLMenuElement = class(THTMLElement)
   end;

   THTMLMenuItemElement = class(THTMLElement)
   end;

   THTMLMetaElement = class(THTMLElement)
   end;

   THTMLMeterElement = class(THTMLElement)
   end;

   THTMLModElement = class(THTMLElement)
   end;

   THTMLObjectElement = class(THTMLElement)
   end;

   THTMLOListElement = class(THTMLElement)
   end;

   THTMLOptGroupElement = class(THTMLElement)
   end;

   THTMLOptionElement = class(THTMLElement)
   end;

   THTMLOutputElement = class(THTMLElement)
   end;

   THTMLParagraphElement = class(THTMLElement)
   end;

   THTMLParamElement = class(THTMLElement)
   end;

   THTMLPreElement = class(THTMLElement)
   end;

   THTMLProgressElement = class(THTMLElement)
   end;

   THTMLQuoteElement = class(THTMLElement)
   end;

   THTMLScriptElement = class(THTMLElement)
   end;

   THTMLSelectElement = class(THTMLElement)
   end;

   THTMLSourceElement = class(THTMLElement)
   end;

   THTMLSpanElement = class(THTMLElement)
   end;

   THTMLStyleElement = class(THTMLElement)
   end;

   THTMLTableCaptionElement = class(THTMLElement)
   end;

   THTMLTableColElement = class(THTMLElement)
   end;

   THTMLTableCellElement = class(THTMLElement)
   end;

   THTMLTableDataCellElement = class(THTMLTableCellElement)
   end;

   THTMLTableElement = class(THTMLElement)
   end;

   THTMLTableHeaderCellElement = class(THTMLTableCellElement)
   end;

   THTMLTableRowElement = class(THTMLElement)
   end;

   THTMLTableSectionElement = class(THTMLElement)
   end;

   THTMLTemplateElement = class(THTMLElement)
   end;

   THTMLTextAreaElement = class(THTMLElement)
   end;

   THTMLTimeElement = class(THTMLElement)
   end;

   THTMLTitleElement = class(THTMLElement)
   end;

   THTMLTrackElement = class(THTMLElement)
   end;

   THTMLUListElement = class(THTMLElement)
   end;

   THTMLVideoElement = class(THTMLMediaElement)
   end;

   // obsolete elements:

   THTMLUnknownElement = class(THTMLElement)
   end;

   THTMLAppletElement = class(THTMLElement)
   end;

   THTMLDirectoryElement = class(THTMLElement)
   end;

   THTMLFontElement = class(THTMLElement)
   end;

   THTMLFrameElement = class(THTMLElement)
   end;

   THTMLFrameSetElement = class(THTMLElement)
   end;

   THTMLMarqueeElement = class(THTMLElement)
   end;

   TXMLElement = class(TElement)
    private
      FLowerCaseTagName: TCanonicalString;
    public
      property LowerCaseTagName: TCanonicalString read FLowerCaseTagName; // only valid if constructed via ConstructSVGElement() or ConstructMathMLElement()
   end;

   TMathMLElement = class(TXMLElement)
    public
      constructor Create(const NewLocalName: TCanonicalString; const NewAttributes: TAttributeHashTable; const NewChildren: array of TNode; const NewProperties: TElementProperties);
      {$IFOPT C+} procedure AfterConstruction(); override; {$ENDIF}
   end;

   TSVGElement = class(TXMLElement)
    public
      constructor Create(const NewLocalName: TCanonicalString; const NewAttributes: TAttributeHashTable; const NewChildren: array of TNode; const NewProperties: TElementProperties);
      {$IFOPT C+} procedure AfterConstruction(); override; {$ENDIF}
   end;

procedure RegisterNamespace(const Namespace: UTF8String; var CanonicalNamespace: TCanonicalString); inline;
procedure RegisterHTMLElement(const TagName: UTF8String; var CanonicalTagName: TCanonicalString; const ElementClass: THTMLElementClass; const ElementProperties: TElementProperties); inline;
procedure RegisterMathMLElement(const TagName: UTF8String; const ElementProperties: TElementProperties); inline;
procedure RegisterSVGElement(const RealTagName: UTF8String; const ElementProperties: TElementProperties); inline;

function ConstructHTMLElement(const TagName: TCanonicalString; const NewAttributes: TElement.TAttributeHashTable = nil): THTMLElement;
function ConstructMathMLElement(const TagName: TCanonicalString; const NewAttributes: TElement.TAttributeHashTable = nil): TMathMLElement;
function ConstructSVGElement(const TagName: TCanonicalString; const NewAttributes: TElement.TAttributeHashTable = nil): TSVGElement;

function E(const Namespace: TCanonicalString; const TagName: TCanonicalString; const AttributeNameValuePairs: array of UTF8String; const Doc: TDocument; const Children: array of TNode): TElement; overload;
function E(const Namespace: TCanonicalString; const TagName: TCanonicalString; const AttributeNameValuePairs: array of UTF8String; const Children: array of TNode): TElement; inline; overload;

// these default to nsHTML:
function E(const TagName: TCanonicalString; const AttributeNameValuePairs: array of UTF8String; const Doc: TDocument; const Children: array of TNode): TElement; inline; overload;
function E(const TagName: TCanonicalString; const AttributeNameValuePairs: array of UTF8String; const Children: array of TNode): TElement; inline; overload;
function E(const TagName: TCanonicalString; const Children: array of TNode): TElement; inline; overload;
function E(const TagName: TCanonicalString; const AttributeNameValuePairs: array of UTF8String; const Doc: TDocument): TElement; inline; overload;
function E(const TagName: TCanonicalString; const AttributeNameValuePairs: array of UTF8String): TElement; inline; overload;
function E(const TagName: TCanonicalString): TElement; inline; overload;

function T(constref Text: UTF8String; const Doc: TDocument): TText; overload;
function T(constref Text: UTF8String): TText; inline; overload;

{$IFDEF DEBUG} function SerialiseDOMForTestOutput(const Document: TDocument): UTF8String; {$ENDIF}

implementation

uses
   hashtable, specutils, sysutils, {$IFDEF USEROPES} ropes {$ELSE} stringutils {$ENDIF}, rtlutils;

type
   THTMLElementData = record
      ElementClass: THTMLElementClass;
      Properties: TElementProperties;
   end;
   THTMLElementDataHashTable = specialize THashTable <TCanonicalString, THTMLElementData, TCanonicalString>;

type
   TMathMLElementDataHashTable = specialize THashTable <TCanonicalString, TElementProperties, TCanonicalString>;

type
   TSVGElementData = record
      RealLocalName: TCanonicalString;
      Properties: TElementProperties;
   end;
   TSVGElementDataHashTable = specialize THashTable <TCanonicalString, TSVGElementData, TCanonicalString>;

var
   HTMLElements: THTMLElementDataHashTable;
   MathMLElements: TMathMLElementDataHashTable;
   SVGElements: TSVGElementDataHashTable;

function ConstructHTMLElement(const TagName: TCanonicalString; const NewAttributes: TElement.TAttributeHashTable): THTMLElement;
var
   ElementData: THTMLElementData;
begin
   ElementData := HTMLElements[TagName];
   if (Assigned(ElementData.ElementClass)) then
      Result := ElementData.ElementClass.Create(TagName, NewAttributes, [], ElementData.Properties)
   else
      Result := THTMLUnknownElement.Create(TagName, NewAttributes, [], propHTML);
end;

function ConstructMathMLElement(const TagName: TCanonicalString; const NewAttributes: TElement.TAttributeHashTable): TMathMLElement;
begin
   Assert(ASCIILowerCase(TagName.AsString) = TagName.AsString);
   Result := TMathMLElement.Create(TagName, NewAttributes, [], MathMLElements[TagName]);
   Result.FLowerCaseTagName := TagName;
end;

function ConstructSVGElement(const TagName: TCanonicalString; const NewAttributes: TElement.TAttributeHashTable): TSVGElement;
var
   ElementData: TSVGElementData;
begin
   Assert(ASCIILowerCase(TagName.AsString) = TagName.AsString);
   if (SVGElements.Has(TagName)) then
   begin
      ElementData := SVGElements[TagName];
      Result := TSVGElement.Create(ElementData.RealLocalName, NewAttributes, [], ElementData.Properties);
   end
   else
   begin
      Result := TSVGElement.Create(TagName, NewAttributes, [], 0);
   end;
   Result.FLowerCaseTagName := TagName;
end;

procedure RegisterNamespace(const Namespace: UTF8String; var CanonicalNamespace: TCanonicalString); inline;
begin
   CanonicalNamespace := Intern(Namespace);
end;

procedure RegisterHTMLElement(const TagName: UTF8String; var CanonicalTagName: TCanonicalString; const ElementClass: THTMLElementClass; const ElementProperties: TElementProperties); inline;
var
   ElementData: THTMLElementData;
begin
   CanonicalTagName := Intern(TagName);
   Assert(not HTMLElements.Has(CanonicalTagName));
   ElementData.ElementClass := ElementClass;
   ElementData.Properties := propHTML or ElementProperties;
   HTMLElements.Add(CanonicalTagName, ElementData);
end;

procedure RegisterMathMLElement(const TagName: UTF8String; const ElementProperties: TElementProperties); inline;
var
   CanonicalTagName: TCanonicalString;
begin
   CanonicalTagName := Intern(TagName);
   Assert(not MathMLElements.Has(CanonicalTagName));
   MathMLElements.Add(CanonicalTagName, ElementProperties);
end;

procedure RegisterSVGElement(const RealTagName: UTF8String; const ElementProperties: TElementProperties); inline;
var
   CanonicalTagName: TCanonicalString;
   ElementData: TSVGElementData;
begin
   CanonicalTagName := Intern(ASCIILowerCase(RealTagName));
   Assert(not SVGElements.Has(CanonicalTagName));
   ElementData.RealLocalName := Intern(RealTagName);
   ElementData.Properties := ElementProperties;
   SVGElements.Add(CanonicalTagName, ElementData);
end;


constructor THTMLElement.Create(const NewLocalName: TCanonicalString; const NewAttributes: TAttributeHashTable; const NewChildren: array of TNode; const NewProperties: TElementProperties);
begin
   inherited Create(nsHTML, NewLocalName, NewAttributes, NewChildren);
   FProperties := NewProperties;
end;

{$IFOPT C+}
procedure THTMLElement.AfterConstruction();
var
   ElementData: THTMLElementData;
begin
   inherited;
   Assert(FNamespaceURL = nsHTML);
   if (HTMLElements.Has(LocalName)) then
   begin
      ElementData := HTMLElements[LocalName];
      Assert(Assigned(ElementData.ElementClass));
      Assert(ClassType = ElementData.ElementClass);
      Assert(FProperties = ElementData.Properties, LocalName.AsString + ' has bogus properties? have ' + IntToStr(FProperties) + ' but expected ' + IntToStr(ElementData.Properties));
   end
   else
   begin
      Assert(ClassType = THTMLUnknownElement);
      Assert(FProperties = propHTML);
   end;
end;
{$ENDIF}


procedure THTMLHeadingElement.AfterConstruction(); 
begin
   inherited;
   if (LocalName = eH1) then
      FRank := 1
   else
   if (LocalName = eH2) then
      FRank := 2
   else
   if (LocalName = eH3) then
      FRank := 3
   else
   if (LocalName = eH4) then
      FRank := 4
   else
   if (LocalName = eH5) then
      FRank := 5
   else
   if (LocalName = eH6) then
      FRank := 6
   else
   begin
      Assert(False);
      FRank := 1;
   end;
end;


constructor TMathMLElement.Create(const NewLocalName: TCanonicalString; const NewAttributes: TAttributeHashTable; const NewChildren: array of TNode; const NewProperties: TElementProperties);
begin
   inherited Create(nsMathML, NewLocalName, NewAttributes, NewChildren);
   FProperties := NewProperties;
end;

{$IFOPT C+}
procedure TMathMLElement.AfterConstruction();
begin
   inherited;
   Assert(FNamespaceURL = nsMathML);
   Assert(FProperties = MathMLElements[LocalName]);
end;
{$ENDIF}


constructor TSVGElement.Create(const NewLocalName: TCanonicalString; const NewAttributes: TAttributeHashTable; const NewChildren: array of TNode; const NewProperties: TElementProperties);
begin
   inherited Create(nsSVG, NewLocalName, NewAttributes, NewChildren);
   FProperties := NewProperties;
end;

{$IFOPT C+}
procedure TSVGElement.AfterConstruction();
var
   ElementData: TSVGElementData;
   LowercaseName: TCanonicalString;
begin
   inherited;
   Assert(FNamespaceURL = nsSVG);
   LowercaseName := Intern(ASCIILowerCase(FLocalName.AsString));
   if (SVGElements.Has(LowercaseName)) then
   begin
      ElementData := SVGElements[LowercaseName];
      Assert(FLocalName = ElementData.RealLocalName);
      Assert(FProperties = ElementData.Properties);
   end;
end;
{$ENDIF}


function E(const Namespace: TCanonicalString; const TagName: TCanonicalString; const AttributeNameValuePairs: array of UTF8String; const Doc: TDocument; const Children: array of TNode): TElement;
var
   Attributes: TElement.TAttributeHashTable;
   Index: Cardinal;
   Scratch: Rope;
   StringStore: TStringStore;

   procedure StoreString(constref S: UTF8String);
   begin
      if (GetRefCount(S) <> -1) then
      begin
         if (not Assigned(StringStore)) then
         begin
            if (not Assigned(Doc)) then
               raise Exception.Create('non-constant string used in attribute value, but no owner document provided');
            StringStore := TStringStore.Create();
            Doc.TakeOwnership(StringStore);
         end;
         StringStore.Push(S);
      end;
   end;

begin
   Assert(Length(AttributeNameValuePairs) mod 2 = 0);
   StringStore := nil;
   if (Length(AttributeNameValuePairs) > 0) then
   begin
      Attributes := TElement.TAttributeHashTable.Create(Length(AttributeNameValuePairs) div 2); // $R-
      Index := 0;
      repeat
         Scratch := Default(Rope);
         if (AttributeNameValuePairs[Index+1] <> '') then
         begin
            StoreString(AttributeNameValuePairs[Index+1]);
            Scratch.Append(@AttributeNameValuePairs[Index+1][1], Length(AttributeNameValuePairs[Index+1])); // $R-
         end;
         Attributes[AttributeNameValuePairs[Index]] := Scratch;
         Inc(Index, 2);
      until Index = Length(AttributeNameValuePairs);
   end
   else
      Attributes := nil;
   if (Namespace = nsHTML) then
      Result := ConstructHTMLElement(TagName, Attributes)
   else
   if (Namespace = nsMathML) then
      Result := ConstructMathMLElement(TagName, Attributes)
   else
   if (Namespace = nsSVG) then
      Result := ConstructSVGElement(TagName, Attributes)
   else
      Result := TElement.Create(Namespace, TagName, Attributes);
   if (Length(Children) > 0) then
      for Index := Low(Children) to High(Children) do
         Result.AppendChild(Children[Index]);
end;

function E(const Namespace: TCanonicalString; const TagName: TCanonicalString; const AttributeNameValuePairs: array of UTF8String; const Children: array of TNode): TElement;
begin
   Result := E(Namespace, TagName, AttributeNameValuePairs, nil, Children);
end;

function E(const TagName: TCanonicalString; const AttributeNameValuePairs: array of UTF8String; const Doc: TDocument; const Children: array of TNode): TElement;
begin
   Result := E(nsHTML, TagName, AttributeNameValuePairs, Doc, Children);
end;

function E(const TagName: TCanonicalString; const AttributeNameValuePairs: array of UTF8String; const Children: array of TNode): TElement;
begin
   Result := E(nsHTML, TagName, AttributeNameValuePairs, nil, Children);
end;

function E(const TagName: TCanonicalString; const Children: array of TNode): TElement;
begin
   Result := E(nsHTML, TagName, [], nil, Children);
end;

function E(const TagName: TCanonicalString; const AttributeNameValuePairs: array of UTF8String; const Doc: TDocument): TElement;
begin
   Result := E(nsHTML, TagName, AttributeNameValuePairs, Doc, []);
end;

function E(const TagName: TCanonicalString; const AttributeNameValuePairs: array of UTF8String): TElement;
begin
   Result := E(nsHTML, TagName, AttributeNameValuePairs, nil, []);
end;

function E(const TagName: TCanonicalString): TElement;
begin
   Result := E(nsHTML, TagName, [], nil, []);
end;

function T(constref Text: UTF8String; const Doc: TDocument): TText;
var
   Scratch: Rope;
   StringStore: TStringStore;
begin
   if (Text = '') then
   begin
      Result := TText.Create('');
      exit;
   end;
   if (GetRefCount(Text) <> -1) then
   begin
      if (not Assigned(Doc)) then
         raise Exception.Create('non-constant string used in text node value, but no owner document provided');
      StringStore := TStringStore.Create();
      Doc.TakeOwnership(StringStore);
      StringStore.Push(Text);
   end;
   Scratch := Default(Rope);
   Scratch.Append(@Text[1], Length(Text)); // $R-
   Result := TText.CreateDestructively(Scratch);
end;

function T(constref Text: UTF8String): TText;
begin
   Result := T(Text, nil);
end;

{$IFDEF DEBUG}
function SerialiseDOMForTestOutput(const Document: TDocument): UTF8String;
type
   UTF8StringArray = array of UTF8String;

   procedure QuickSort(var List: UTF8StringArray; L, R: Integer);
   var
      I, J : Integer;
      P, Q : AnsiString;
   begin
      // based on QuickSort in rtl/objpas/classes/lists.inc
      repeat
         I := L;
         J := R;
         P := List[(L + R) div 2];
         repeat
            while (P > List[I]) do
               I := I + 1; // $R-
            while (P < List[J]) do
               J := J - 1; // $R-
            if (I <= J) then
            begin
               Q := List[I];
               List[I] := List[J]; // http://bugs.freepascal.org/view.php?id=26403
               List[J] := Q;
               I := I + 1; // $R-
               J := J - 1; // $R-
            end;
         until I > J;
         if (L < J) then
            QuickSort(List, L, J);
         L := I;
      until I >= R;
   end;

   procedure QuickSort(var List: UTF8StringArray);
   begin
      Assert(Low(List) >= Low(Integer));
      Assert(High(List) <= High(Integer));
      if (Length(List) > 1) then
         QuickSort(List, Low(List), High(List)); // $R- // http://bugs.freepascal.org/view.php?id=26403
   end;

var
   Indent: Integer;

   function IndentString(): UTF8String;
   begin
      Assert(Indent >= 0);
      SetLength(Result, Indent * 2);
      if (Length(Result) > 0) then
         FillChar(Result[1], Length(Result), ' ');
   end;

const
   LinePrefix = '| ';

   procedure WalkOut(const Element: TElement);
   begin
      if (Element.IsIdentity(nsHTML, eTemplate)) then
         Dec(Indent, 2)
      else
         Dec(Indent);
   end;

var
   Pos: TNode;
   Element: TElement;
   AttributeLines: UTF8StringArray;
   AttributeName: UTF8String;
   AttributeIndex: Cardinal;
begin
   Result := '';
   Indent := 0;
   Assert(Assigned(Document));
   Pos := Document;
   while (WalkToNext(Pos, Document, @WalkOut)) do
   begin
      if (Result <> '') then
         Result := Result + #$0A;
      Result := Result + LinePrefix + IndentString();
      if (Pos is TElement) then
      begin
         Element := Pos as TElement;
         Result := Result + '<';
         if (Element.NamespaceURL <> nsHTML) then
         begin
            if (Element.NamespaceURL = nsSVG) then
               Result := Result + 'svg '
            else
            if (Element.NamespaceURL = nsMathML) then
               Result := Result + 'math '
            else
               Result := Result + 'unknown-namespace ';
         end;
         Result := Result + Element.LocalName.AsString + '>';
         if (Assigned(Element.Attributes) and (Element.Attributes.Count > 0)) then
         begin
            SetLength(AttributeLines, Element.Attributes.Count);
            AttributeIndex := 0;
            Inc(Indent);
            for AttributeName in Element.Attributes do
            begin
               AttributeLines[AttributeIndex] := #$0A + LinePrefix + IndentString() +
                                                 AttributeName + '="' + Element.Attributes[AttributeName].AsString + '"';
               Inc(AttributeIndex);
            end;
            Assert(AttributeIndex = Element.Attributes.Count);
            QuickSort(AttributeLines);
            Assert(High(AttributeLines) >= 0);
            Assert(High(AttributeLines) = Element.Attributes.Count-1);
            for AttributeIndex := Low(AttributeLines) to High(AttributeLines) do // $R-
               Result := Result + AttributeLines[AttributeIndex];
            Dec(Indent);
         end;
         if (Element.IsIdentity(nsHTML, eTemplate)) then
         begin
            Inc(Indent);
            Result := Result + #$0A + LinePrefix + IndentString() + 'content';
         end;
         Inc(Indent);
      end
      else
      if (Pos is TText) then
      begin
         Result := Result + '"' + (Pos as TText).Data.AsString + '"';
      end
      else
      if (Pos is TComment) then
      begin
         Result := Result + '<!-- ' + (Pos as TComment).Data.AsString + ' -->';
      end
      else
      if (Pos is TDocumentType) then
      begin
         Result := Result + IndentString() + '<!DOCTYPE ' + (Pos as TDocumentType).Name.AsString;
         if ((not (Pos as TDocumentType).PublicID.IsEmpty) or (not (Pos as TDocumentType).SystemID.IsEmpty)) then
            Result := Result + ' "' + (Pos as TDocumentType).PublicID.AsString + '" "' + (Pos as TDocumentType).SystemID.AsString + '"';
         Result := Result + '>';
      end
      else
      begin
         Assert(False);
         Result := Result + 'ERROR';
      end;
   end;
end;
{$ENDIF}

initialization
   // Namespaces
   RegisterNamespace('http://www.w3.org/1999/xhtml', nsHTML);
   RegisterNamespace('http://www.w3.org/1998/Math/MathML', nsMathML);
   RegisterNamespace('http://www.w3.org/2000/svg', nsSVG);

   // HTML elements
   HTMLElements := THTMLElementDataHashTable.Create(@CanonicalStringHash32, 136);
   RegisterHTMLElement('a', eA, THTMLAnchorElement, 0);
   RegisterHTMLElement('abbr', eAbbr, THTMLElement, 0);
   RegisterHTMLElement('address', eAddress, THTMLElement, propSpecial or propAutoclosesP);
   RegisterHTMLElement('applet', eApplet, THTMLAppletElement, propGenericScope or propListItemScope or propButtonScope or propSpecial or propSpecialish);
   RegisterHTMLElement('area', eArea, THTMLAreaElement, propSpecial or propSpecialish or propVoidElement or propCannotContainPalpableText);
   RegisterHTMLElement('article', eArticle, THTMLElement, propSpecial or propSpecialish or propAutoclosesP);
   RegisterHTMLElement('aside', eAside, THTMLElement, propSpecial or propSpecialish or propAutoclosesP);
   RegisterHTMLElement('audio', eAudio, THTMLAudioElement, 0);
   RegisterHTMLElement('b', eB, THTMLElement, 0);
   RegisterHTMLElement('base', eBase, THTMLBaseElement, propSpecial or propSpecialish or propVoidElement or propCannotContainPalpableText);
   RegisterHTMLElement('basefont', eBaseFont, THTMLElement, propSpecial or propSpecialish);
   RegisterHTMLElement('bdi', eBDI, THTMLElement, 0);
   RegisterHTMLElement('bdo', eBDO, THTMLElement, 0);
   RegisterHTMLElement('bgsound', eBGSound, THTMLUnknownElement, propSpecial or propSpecialish);
   RegisterHTMLElement('big', eBig, THTMLElement, 0);
   RegisterHTMLElement('blink', eBlink, THTMLUnknownElement, 0);
   RegisterHTMLElement('blockquote', eBlockQuote, THTMLQuoteElement, propSpecial or propSpecialish or propAutoclosesP);
   RegisterHTMLElement('body', eBody, THTMLBodyElement, propSpecial or propSpecialish or propEOFImpliedEndTag or propOptionalStartTag or propOptionalEndTag);
   RegisterHTMLElement('br', eBr, THTMLBRElement, propSpecial or propSpecialish or propVoidElement or propCannotContainPalpableText);
   RegisterHTMLElement('button', eButton, THTMLButtonElement, propButtonScope or propSpecial or propSpecialish);
   RegisterHTMLElement('canvas', eCanvas, THTMLCanvasElement, 0);
   RegisterHTMLElement('caption', eCaption, THTMLTableCaptionElement, propGenericScope or propListItemScope or propButtonScope or propSpecial or propSpecialish or propTemplateImpliedEndTag);
   RegisterHTMLElement('center', eCenter, THTMLElement, propSpecial or propSpecialish);
   RegisterHTMLElement('cite', eCite, THTMLElement, 0);
   RegisterHTMLElement('code', eCode, THTMLElement, 0);
   RegisterHTMLElement('col', eCol, THTMLTableColElement, propSpecial or propSpecialish or propVoidElement or propCannotContainPalpableText);
   RegisterHTMLElement('colgroup', eColGroup, THTMLTableColElement, propSpecial or propSpecialish or propTemplateImpliedEndTag or propCannotContainPalpableText);
   RegisterHTMLElement('data', eData, THTMLDataElement, 0);
   RegisterHTMLElement('datalist', eDataList, THTMLDataListElement, 0);
   RegisterHTMLElement('dd', eDD, THTMLElement, propBodyImpliedEndTag or propEOFImpliedEndTag or propSpecial or propSpecialish or propOptionalEndTag or propTemplateImpliedEndTag);
   RegisterHTMLElement('del', eDel, THTMLModElement, 0);
   RegisterHTMLElement('details', eDetails, THTMLDetailsElement, propSpecial or propSpecialish);
   RegisterHTMLElement('dfn', eDfn, THTMLElement, 0);
   RegisterHTMLElement('dialog', eDialog, THTMLDialogElement, 0);
   RegisterHTMLElement('dir', eDir, THTMLDirectoryElement, propSpecial or propSpecialish);
   RegisterHTMLElement('div', eDiv, THTMLDivElement, propSpecial or propAutoclosesP);
   RegisterHTMLElement('dl', eDL, THTMLDListElement, propSpecial or propSpecialish or propAutoclosesP or propCannotContainPalpableText);
   RegisterHTMLElement('dt', eDT, THTMLElement, propBodyImpliedEndTag or propEOFImpliedEndTag or propSpecial or propSpecialish or propOptionalEndTag or propTemplateImpliedEndTag);
   RegisterHTMLElement('em', eEm, THTMLElement, 0);
   RegisterHTMLElement('embed', eEmbed, THTMLEmbedElement, propSpecial or propSpecialish or propVoidElement or propCannotContainPalpableText);
   RegisterHTMLElement('fieldset', eFieldSet, THTMLFieldSetElement, propSpecial or propSpecialish or propAutoclosesP);
   RegisterHTMLElement('figcaption', eFigCaption, THTMLElement, propSpecial or propSpecialish);
   RegisterHTMLElement('figure', eFigure, THTMLElement, propSpecial or propSpecialish);
   RegisterHTMLElement('font', eFont, THTMLFontElement, 0);
   RegisterHTMLElement('footer', eFooter, THTMLElement, propSpecial or propSpecialish or propAutoclosesP);
   RegisterHTMLElement('form', eForm, THTMLFormElement, propSpecial or propSpecialish or propAutoclosesP);
   RegisterHTMLElement('frame', eFrame, THTMLFrameElement, propSpecial or propSpecialish);
   RegisterHTMLElement('frameset', eFrameSet, THTMLFrameSetElement, propSpecial or propSpecialish);
   RegisterHTMLElement('h1', eH1, THTMLHeadingElement, propHeading or propSpecial or propSpecialish or propAutoclosesP);
   RegisterHTMLElement('h2', eH2, THTMLHeadingElement, propHeading or propSpecial or propSpecialish or propAutoclosesP);
   RegisterHTMLElement('h3', eH3, THTMLHeadingElement, propHeading or propSpecial or propSpecialish or propAutoclosesP);
   RegisterHTMLElement('h4', eH4, THTMLHeadingElement, propHeading or propSpecial or propSpecialish or propAutoclosesP);
   RegisterHTMLElement('h5', eH5, THTMLHeadingElement, propHeading or propSpecial or propSpecialish or propAutoclosesP);
   RegisterHTMLElement('h6', eH6, THTMLHeadingElement, propHeading or propSpecial or propSpecialish or propAutoclosesP);
   RegisterHTMLElement('head', eHead, THTMLHeadElement, propSpecial or propSpecialish or propOptionalStartTag or propOptionalEndTag or propCannotContainPalpableText);
   RegisterHTMLElement('header', eHeader, THTMLElement, propSpecial or propSpecialish or propAutoclosesP);
   RegisterHTMLElement('hgroup', eHGroup, THTMLElement, propSpecial or propSpecialish or propAutoclosesP or propCannotContainPalpableText);
   RegisterHTMLElement('hr', eHR, THTMLHRElement, propSpecial or propSpecialish or propVoidElement or propAutoclosesP or propCannotContainPalpableText);
   RegisterHTMLElement('html', eHTML, THTMLHTMLElement, propGenericScope or propListItemScope or propButtonScope or propTableScope or propTBodyContext or propTRContext or propSpecial or propSpecialish or propEOFImpliedEndTag or propBodyImpliedEndTag or propOptionalStartTag or propOptionalEndTag or propCannotContainPalpableText);
   RegisterHTMLElement('i', eI, THTMLElement, 0);
   RegisterHTMLElement('iframe', eIFrame, THTMLIFrameElement, propSpecial or propSpecialish);
   RegisterHTMLElement('img', eImg, THTMLImageElement, propSpecial or propSpecialish or propVoidElement or propCannotContainPalpableText);
   RegisterHTMLElement('input', eInput, THTMLInputElement, propSpecial or propSpecialish or propVoidElement or propCannotContainPalpableText);
   RegisterHTMLElement('ins', eIns, THTMLModElement, 0);
   RegisterHTMLElement('isindex', eIsIndex, THTMLUnknownElement, propSpecial or propSpecialish);
   RegisterHTMLElement('kbd', eKbd, THTMLElement, 0);
   RegisterHTMLElement('keygen', eKeyGen, THTMLKeygenElement, propVoidElement or propCannotContainPalpableText);
   RegisterHTMLElement('label', eLabel, THTMLLabelElement, 0);
   RegisterHTMLElement('legend', eLegend, THTMLLegendElement, 0);
   RegisterHTMLElement('li', eLI, THTMLLIElement, propBodyImpliedEndTag or propEOFImpliedEndTag or propSpecial or propSpecialish or propOptionalEndTag or propTemplateImpliedEndTag);
   RegisterHTMLElement('link', eLink, THTMLLinkElement, propSpecial or propSpecialish or propVoidElement or propCannotContainPalpableText);
   RegisterHTMLElement('listing', eListing, THTMLElement, propSpecial or propSpecialish);
   RegisterHTMLElement('main', eMain, THTMLElement, propSpecial or propSpecialish or propAutoclosesP);
   RegisterHTMLElement('map', eMap, THTMLMapElement, 0);
   RegisterHTMLElement('mark', eMark, THTMLElement, 0);
   RegisterHTMLElement('marquee', eMarquee, THTMLMarqueeElement, propGenericScope or propListItemScope or propButtonScope or propSpecial or propSpecialish);
   RegisterHTMLElement('menu', eMenu, THTMLMenuElement, propSpecial or propSpecialish or propAutoclosesP);
   RegisterHTMLElement('menuitem', eMenuItem, THTMLMenuItemElement, propSpecial or propSpecialish or propVoidElement or propCannotContainPalpableText);
   RegisterHTMLElement('meta', eMeta, THTMLMetaElement, propSpecial or propSpecialish or propVoidElement or propCannotContainPalpableText);
   RegisterHTMLElement('meter', eMeter, THTMLMeterElement, 0);
   RegisterHTMLElement('multicol', eMultiCol, THTMLUnknownElement, 0);
   RegisterHTMLElement('nav', eNav, THTMLElement, propSpecial or propSpecialish or propAutoclosesP);
   RegisterHTMLElement('nextid', eNextID, THTMLUnknownElement, 0);
   RegisterHTMLElement('nobr', eNoBr, THTMLElement, 0);
   RegisterHTMLElement('noembed', eNoEmbed, THTMLElement, propSpecial or propSpecialish);
   RegisterHTMLElement('noframes', eNoFrames, THTMLElement, propSpecial or propSpecialish);
   RegisterHTMLElement('noscript', eNoScript, THTMLElement, propSpecial or propSpecialish);
   RegisterHTMLElement('object', eObject, THTMLObjectElement, propGenericScope or propListItemScope or propButtonScope or propSpecial or propSpecialish);
   RegisterHTMLElement('ol', eOL, THTMLOListElement, propListItemScope or propSpecial or propSpecialish or propAutoclosesP or propCannotContainPalpableText);
   RegisterHTMLElement('optgroup', eOptGroup, THTMLOptGroupElement, propSelectScope or propBodyImpliedEndTag or propEOFImpliedEndTag or propTemplateImpliedEndTag or propOptionalEndTag or propCannotContainPalpableText);
   RegisterHTMLElement('option', eOption, THTMLOptionElement, propSelectScope or propBodyImpliedEndTag or propOptionalEndTag or propTemplateImpliedEndTag);
   RegisterHTMLElement('output', eOutput, THTMLOutputElement, 0);
   RegisterHTMLElement('p', eP, THTMLParagraphElement, propBodyImpliedEndTag or propEOFImpliedEndTag or propSpecial or propOptionalEndTag or propAutoclosesP or propTemplateImpliedEndTag);
   RegisterHTMLElement('param', eParam, THTMLParamElement, propSpecial or propSpecialish or propVoidElement or propCannotContainPalpableText);
   RegisterHTMLElement('plaintext', ePlaintext, THTMLElement, propSpecial or propSpecialish);
   RegisterHTMLElement('pre', ePre, THTMLPreElement, propSpecial or propSpecialish or propAutoclosesP);
   RegisterHTMLElement('progress', eProgress, THTMLProgressElement, 0);
   RegisterHTMLElement('q', eQ, THTMLQuoteElement, 0);
   RegisterHTMLElement('rb', eRB, THTMLUnknownElement, 0);
   RegisterHTMLElement('rp', eRP, THTMLElement, propBodyImpliedEndTag or propOptionalEndTag or propTemplateImpliedEndTag);
   RegisterHTMLElement('rt', eRT, THTMLElement, propBodyImpliedEndTag or propOptionalEndTag or propTemplateImpliedEndTag);
   RegisterHTMLElement('ruby', eRuby, THTMLElement, 0);
   RegisterHTMLElement('s', eS, THTMLElement, 0);
   RegisterHTMLElement('samp', eSamp, THTMLElement, 0);
   RegisterHTMLElement('script', eScript, THTMLScriptElement, propSpecial or propSpecialish or propRawTextElement);
   RegisterHTMLElement('section', eSection, THTMLElement, propSpecial or propSpecialish or propAutoclosesP);
   RegisterHTMLElement('select', eSelect, THTMLSelectElement, propSpecial or propSpecialish or propCannotContainPalpableText);
   RegisterHTMLElement('small', eSmall, THTMLElement, 0);
   RegisterHTMLElement('source', eSource, THTMLSourceElement, propSpecial or propSpecialish or propVoidElement or propCannotContainPalpableText);
   RegisterHTMLElement('spacer', eSpacer, THTMLUnknownElement, 0);
   RegisterHTMLElement('span', eSpan, THTMLSpanElement, 0);
   RegisterHTMLElement('strike', eStrike, THTMLElement, 0);
   RegisterHTMLElement('strong', eStrong, THTMLElement, 0);
   RegisterHTMLElement('style', eStyle, THTMLStyleElement, propSpecial or propSpecialish or propRawTextElement);
   RegisterHTMLElement('sub', eSub, THTMLElement, 0);
   RegisterHTMLElement('summary', eSummary, THTMLElement, propSpecial or propSpecialish);
   RegisterHTMLElement('sup', eSup, THTMLElement, 0);
   RegisterHTMLElement('table', eTable, THTMLTableElement, propFosterParent or propGenericScope or propListItemScope or propButtonScope or propTableScope or propSpecial or propSpecialish or propAutoclosesP or propCannotContainPalpableText);
   RegisterHTMLElement('tbody', eTBody, THTMLTableSectionElement, propFosterParent or propTableSection or propTBodyContext or propSpecial or propSpecialish or propEOFImpliedEndTag or propTemplateImpliedEndTag or propOptionalEndTag or propCannotContainPalpableText);
   RegisterHTMLElement('td', eTD, THTMLTableDataCellElement, propTableCell or propGenericScope or propListItemScope or propButtonScope or propSpecial or propSpecialish or propEOFImpliedEndTag or propTemplateImpliedEndTag or propOptionalEndTag);
   RegisterHTMLElement('template', eTemplate, THTMLTemplateElement, propGenericScope or propListItemScope or propButtonScope or propTableScope or propTBodyContext or propTRContext or propSpecial or propSpecialish);
   RegisterHTMLElement('textarea', eTextArea, THTMLTextAreaElement, propSpecial or propSpecialish or propEscapableRawTextElement);
   RegisterHTMLElement('tfoot', eTFoot, THTMLTableSectionElement, propFosterParent or propTableSection or propTBodyContext or propSpecial or propSpecialish or propEOFImpliedEndTag or propTemplateImpliedEndTag or propOptionalEndTag or propCannotContainPalpableText);
   RegisterHTMLElement('th', eTH, THTMLTableHeaderCellElement, propTableCell or propGenericScope or propListItemScope or propButtonScope or propSpecial or propSpecialish or propEOFImpliedEndTag or propTemplateImpliedEndTag or propOptionalEndTag);
   RegisterHTMLElement('thead', eTHead, THTMLTableSectionElement, propFosterParent or propTableSection or propTBodyContext or propSpecial or propSpecialish or propEOFImpliedEndTag or propTemplateImpliedEndTag or propOptionalEndTag or propCannotContainPalpableText);
   RegisterHTMLElement('time', eTime, THTMLTimeElement, 0);
   RegisterHTMLElement('title', eTitle, THTMLTitleElement, propSpecial or propSpecialish or propEscapableRawTextElement);
   RegisterHTMLElement('tr', eTR, THTMLTableRowElement, propFosterParent or propTRContext or propSpecial or propSpecialish or propEOFImpliedEndTag or propTemplateImpliedEndTag or propOptionalEndTag or propCannotContainPalpableText);
   RegisterHTMLElement('track', eTrack, THTMLTrackElement, propSpecial or propSpecialish or propVoidElement or propCannotContainPalpableText);
   RegisterHTMLElement('tt', eTT, THTMLElement, 0);
   RegisterHTMLElement('u', eU, THTMLElement, 0);
   RegisterHTMLElement('ul', eUL, THTMLUListElement, propListItemScope or propSpecial or propSpecialish or propAutoclosesP or propCannotContainPalpableText);
   RegisterHTMLElement('var', eVar, THTMLElement, 0);
   RegisterHTMLElement('video', eVideo, THTMLVideoElement, 0);
   RegisterHTMLElement('wbr', eWBr, THTMLElement, propSpecial or propSpecialish or propVoidElement or propCannotContainPalpableText);
   RegisterHTMLElement('xmp', eXMP, THTMLElement, propSpecial or propSpecialish);
   Assert(HTMLElements.Count = 136); // make sure this matches constructor above
   eImage := Intern('image'); // don't ask

   // MathML
   eMath := Intern('math');
   eMGlyph := Intern('mglyph');
   eMAlignMark := Intern('malignmark');
   eAnnotationXML := Intern('annotation-xml');
   MathMLElements := TMathMLElementDataHashTable.Create(@CanonicalStringHash32, 6);
   RegisterMathMLElement('mi', propGenericScope or propListItemScope or propButtonScope or propSpecial or propSpecialish or propMathMLTextIntegrationPoint);
   RegisterMathMLElement('mo', propGenericScope or propListItemScope or propButtonScope or propSpecial or propSpecialish or propMathMLTextIntegrationPoint);
   RegisterMathMLElement('mn', propGenericScope or propListItemScope or propButtonScope or propSpecial or propSpecialish or propMathMLTextIntegrationPoint);
   RegisterMathMLElement('ms', propGenericScope or propListItemScope or propButtonScope or propSpecial or propSpecialish or propMathMLTextIntegrationPoint);
   RegisterMathMLElement('mtext', propGenericScope or propListItemScope or propButtonScope or propSpecial or propSpecialish or propMathMLTextIntegrationPoint);
   RegisterMathMLElement('annotation-xml', propGenericScope or propListItemScope or propButtonScope or propSpecial or propSpecialish);
   Assert(MathMLElements.Count = 6); // make sure this matches constructor above

   // SVG
   eSVG := Intern('svg');
   SVGElements := TSVGElementDataHashTable.Create(@CanonicalStringHash32, 39);
   RegisterSVGElement('altGlyph', 0);
   RegisterSVGElement('altGlyphDef', 0);
   RegisterSVGElement('altGlyphItem', 0);
   RegisterSVGElement('animateColor', 0);
   RegisterSVGElement('animateMotion', 0);
   RegisterSVGElement('animateTransform', 0);
   RegisterSVGElement('clipPath', 0);
   RegisterSVGElement('desc', propGenericScope or propListItemScope or propButtonScope or propSpecial or propSpecialish or propHTMLIntegrationPoint);
   RegisterSVGElement('feBlend', 0);
   RegisterSVGElement('feColorMatrix', 0);
   RegisterSVGElement('feComponentTransfer', 0);
   RegisterSVGElement('feComposite', 0);
   RegisterSVGElement('feConvolveMatrix', 0);
   RegisterSVGElement('feDiffuseLighting', 0);
   RegisterSVGElement('feDisplacementMap', 0);
   RegisterSVGElement('feDistantLight', 0);
   RegisterSVGElement('feDropShadow', 0);
   RegisterSVGElement('feFlood', 0);
   RegisterSVGElement('feFuncA', 0);
   RegisterSVGElement('feFuncB', 0);
   RegisterSVGElement('feFuncG', 0);
   RegisterSVGElement('feFuncR', 0);
   RegisterSVGElement('feGaussianBlur', 0);
   RegisterSVGElement('feImage', 0);
   RegisterSVGElement('feMerge', 0);
   RegisterSVGElement('feMergeNode', 0);
   RegisterSVGElement('feMorphology', 0);
   RegisterSVGElement('feOffset', 0);
   RegisterSVGElement('fePointLight', 0);
   RegisterSVGElement('feSpecularLighting', 0);
   RegisterSVGElement('feSpotLight', 0);
   RegisterSVGElement('feTile', 0);
   RegisterSVGElement('feTurbulence', 0);
   RegisterSVGElement('foreignObject', propGenericScope or propListItemScope or propButtonScope or propSpecial or propSpecialish or propHTMLIntegrationPoint);
   RegisterSVGElement('glyphRef', 0);
   RegisterSVGElement('linearGradient', 0);
   RegisterSVGElement('radialGradient', 0);
   RegisterSVGElement('textPath', 0);
   RegisterSVGElement('title', propGenericScope or propListItemScope or propButtonScope or propSpecial or propSpecialish or propHTMLIntegrationPoint);
   Assert(SVGElements.Count = 39); // make sure this matches constructor above

   // XXX check to see how balanced the hash tables are
finalization
   HTMLElements.Free();
   SVGElements.Free();
   MathMLElements.Free();
end.
