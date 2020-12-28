{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
program wattsi;

(************ XXX ***********

  XXX parse all idl blocks and get:
       - interface name
       - whether it's primary or partial
       - link back to the <dfn> element for that interface
       - whether it has a <!-- not obsolete --> comment at the end of the interface line
       - link to the text node that contains the interface's final } character
      then where you see <!--INTERFACES-->, insert a:
         <ul class="brief">
           <li><code><a href="#primary">InterfaceName</a></code>
      ...followed by, for each partial, if more than one:
           <a href="#partial1">partial N</a>
           <a href="#partial2">2</a>
           <a href="#partial3">3</a>
      ...or if there's only one:
           <a href="#partial">partial</a>
      Finally, go back to each interface that also has a partial and add as applicable:
           // <a href="#partialN">also has obsolete members</a>
           // <a href="#partialN">also has additional members in a partial interface</a>
      Also, hyperlink types like DOMString, etc, and remove the need
      for <span>s around other interface names in the IDL blocks


DFN
 +-> override link in other spec

<ref> should have a popup of the <dd> when hovered

use <a [lt]> instead of <span [data-x]>

 ****************************)

{$IFDEF DEBUG} {$DEFINE TIMINGS} {$ENDIF}

uses
   classes, fgl, strutils, // for TStringList, TFPGMap, DelSpace1
   sysutils, {$IFOPT C+} rtlutils, {$ENDIF} fileutils, stringutils,
   dateutils, genericutils, hashfunctions, hashtable, hashset,
   plasticarrays, exceptions, unicode, ropes, wires, canonicalstrings,
   dom, webdom, htmlparser, json, fphttpclient;

var
   Quiet: Boolean = false;
   SinglePageOnly: Boolean = false;
   Version: Word = (*$I version.inc *); // unsigned integer from 0 .. 65535
   HighlightServerURL: AnsiString = '';
   OutputDirectory: AnsiString;
   SearchIndexJsonFile: Text;
   IsFirstSearchIndexItem: Boolean = true;

type
   TAllVariants = (vHTML, vDEV, vSnap, vReview, vSplit);
   TVariants = vHTML..vReview;
   TStringMap = specialize THashTable <UTF8String, UTF8String, UTF8StringUtils>;
   TMDNBrowsers = specialize TFPGMap <UTF8String, UTF8String>; // preserves order

var
   HighlighterOutputByJSONContents: TStringMap;
   MDNJSONData: TJSON;
   MDNBrowsers: TMDNBrowsers;
   MDNBrowsersProvidingCurrentEngines:
      array [0..2] of UTF8String = ('firefox', 'safari', 'chrome');
   MDNBrowsersWithBorrowedEngines:
      array [0..1] of UTF8String = ('opera', 'edge_blink');
   MDNBrowsersWithRetiredEngines:
      array [0..1] of UTF8String = ('edge', 'ie');
   MDNBrowsersForMobileDevices:
      array [0..5] of UTF8String = ('firefox_android', 'safari_ios',
                                    'chrome_android', 'webview_android',
                                    'samsunginternet_android', 'opera_android');
   CurrentVariant: TAllVariants;

const
   kSuffixes: array[TVariants] of UTF8String = ('html', 'dev', 'snap', 'review');
   kExcludingAttribute: array[TAllVariants] of UTF8String = ('w-nohtml', 'w-nodev', 'w-nosnap', 'w-noreview', 'w-nosplit');
   kDEVAttribute = 'w-dev';
   kCrossRefAttribute = 'data-x';
   kCrossSpecRefAttribute = 'data-x-href';
   kCrossRefInternalLinkAttribute = 'data-x-internal';
   kSubDFNAttribute = 'subdfn';
   kUndefinedAttribute = 'undefined';
   kSplitFilenameAttribute = 'split-filename';
   kSplitFilenameTargetAttribute = 'split-filename-target';
   kLTAttribute = 'lt';
   kHrefAttribute = 'href';
   kNonNormative = '本节是非规范的。';
   Months: array[1..12] of UTF8String = ('一月', '二月', '三月', '四月',
                                         '五月', '六月', '七月', '八月', '九月',
                                         '十月', '十一月', '十二月');

type
   TElementMap = specialize THashTable <UTF8String, TElement, UTF8StringUtils>;

var
   nsNone: TCanonicalString;
   eRef, eList, eChapter: TCanonicalString;

procedure Inform(Message: UTF8String);
begin
   if (not Quiet) then
      Writeln(Message);
end;

function IsHighlighterTarget(const Element: TElement; const ClassValue: String): Boolean;
begin
   Result := ((HighlightServerURL <> '')
      and (Element.IsIdentity(nsHTML, ePre)
         or (Element.IsIdentity(nsHTML, eCode)
            and (Element.ParentNode is TElement)
            and TElement(Element.ParentNode).IsIdentity(nsHTML, ePre)))
      and Element.HasAttribute('class')
      and (AnsiContainsStr(ClassValue, 'idl')
         or AnsiContainsStr(ClassValue, 'css')
         or AnsiContainsStr(ClassValue, 'js')
         or AnsiContainsStr(ClassValue, 'html')
         or AnsiContainsStr(ClassValue, 'abnf')));
end;

function HasAncestor(Element: TNode; const TestNamespaceURL, TestLocalName: TCanonicalString): Boolean;
begin
   repeat
      Element := Element.ParentNode;
      if (not (Element is TElement)) then
      begin
         Result := False;
         exit;
      end;
   until (TElement(Element).IsIdentity(TestNamespaceURL, TestLocalName));
   Result := True;
end;

function HasAncestorWithProperties(Element: TNode; const TestProperties: TElementProperties): Boolean;
begin
   repeat
      Element := Element.ParentNode;
      if (not (Element is TElement)) then
      begin
         Result := False;
         exit;
      end;
   until (TElement(Element).HasProperties(TestProperties));
   Result := True;
end;

procedure AddMDNBrowserRow(const SupportTable: TElement;
                           const BrowserID: UTF8String;
                           const YesNoUnknown: UTF8String;
                           const Version: UTF8String;
                           const IsPartial: Boolean;
                           const NeedsFlag: Boolean;
                           const NeedsPrefixOrAltName: Boolean;
                           const Document: TDocument);
type
   AttributesArray = array of UTF8String;
var
   FlagSymbol, BrowserClass: UTF8String;
   BrowserVersionAttributes: AttributesArray;
   BrowserRow: TElement;

begin
   BrowserClass := BrowserID + ' ' + YesNoUnknown;
   FlagSymbol := '';
   BrowserVersionAttributes := Default(AttributesArray);
   BrowserVersionAttributes := AttributesArray.Create('data-x', '');
   if (IsPartial or NeedsFlag or NeedsPrefixOrAltName) then
      FlagSymbol := UTF8String(#$F0#$9F#$94#$B0) + ' ';
   if (NeedsPrefixOrAltName) then
      BrowserVersionAttributes := AttributesArray
        .Create('data-x', '', 'title', 'Requires a prefix or alternative name.');
   if (NeedsFlag) then
      BrowserVersionAttributes := AttributesArray
        .Create('data-x', '', 'title', 'Requires setting a user preference or runtime flag.');
   if (IsPartial) then
      BrowserVersionAttributes := AttributesArray
        .Create('data-x', '', 'title', 'Partial implementation.');
   if (BrowserID = 'edge_blink') then
      BrowserClass := 'edge_blink ' + YesNoUnknown
   else
   if (BrowserID = 'edge') then
      BrowserClass := 'edge ' + YesNoUnknown;
   BrowserRow := E(eSpan, ['data-x', '', 'class', BrowserClass], Document);
   BrowserRow.AppendChild(E(eSpan, ['data-x', ''], [T(MDNBrowsers[BrowserID], Document)]));
   BrowserRow.AppendChild(E(eSpan, BrowserVersionAttributes,
                             [T(FlagSymbol + Version, Document)]));
   SupportTable.AppendChild(BrowserRow);
end;

function GetVersionAsString(const Version: TJSON): UTF8String;
begin
   if (not Assigned(Version)) then exit('');
   if (Version is TJSONBoolean) then exit(IfThen(Version, 'True', 'False'));
   exit(UTF8String(Version));
end;

function GetVersionOrRange(const BrowserID: UTF8String;
                           const VersionAdded: UTF8String;
                           const VersionRemoved: UTF8String;
                           const YesNoUnknown: UTF8String): UTF8String;
begin
   if (YesNoUnknown = 'unknown') then
      exit('?');
   if (VersionAdded = 'False') then
      exit('No');
   if ((VersionRemoved = '') or (VersionRemoved = 'False')) then
   begin
      if (YesNoUnknown = 'no') then
         exit('No');
      if (VersionAdded = 'True') then
         exit('Yes');
      if ((BrowserID = 'edge') and (VersionAdded = '18')) then
         exit('18');
      if ((BrowserID = 'ie') and (VersionAdded = '11')) then
         exit('11');
      exit(VersionAdded + '+');
   end;
   // else VersionRemoved is either 'True' or an actual version number
   if (VersionAdded = '') then
      // can't show range if VersionAdded is empty
      exit('No');
   if ((VersionRemoved = 'True') or (VersionAdded = 'True')) then
      // can't show range if VersionRemoved & VersionAdded aren't both numbers
      exit('No');
   exit(VersionAdded + UTF8Encode(#$2013) + VersionRemoved);
end;

procedure ProcessBrowserData(const BrowserID: UTF8String;
                             const VersionData: TJSON;
                             const SupportTable: TElement;
                             const Document: TDocument);
var
   NeedsFlag, NeedsPrefixOrAltName, IsPartial: Boolean;
   VersionDetails: TJSON;
   YesNoUnknown, Version, VersionAdded, VersionRemoved: UTF8String;
begin
   NeedsFlag := False;
   NeedsPrefixOrAltName := False;
   IsPartial := False;
   YesNoUnknown := 'unknown';
   Version := '?';
   VersionAdded := '';
   VersionRemoved := '';

   // MDN support data for a given browser can be an array of objects.
   if (VersionData is TJSONArray) then
   begin
      for VersionDetails in VersionData do
      begin
         NeedsFlag := False;
         VersionAdded := GetVersionAsString(VersionDetails['version_added']);
         VersionRemoved := GetVersionAsString(VersionDetails['version_removed']);
         if ((VersionRemoved <> '') and (VersionRemoved <> 'False')) then
         begin
            YesNoUnknown := 'no';
            continue;
         end;
         if (VersionAdded <> '') then
         begin
            YesNoUnknown := IfThen((VersionAdded = 'False'), 'no', 'yes');
            if (Assigned(VersionDetails['partial_implementation'])) then
               IsPartial := True;
            if (Assigned(VersionDetails['flags'])) then
               NeedsFlag := True;
            if (Assigned(VersionDetails['prefix']) or
                Assigned(VersionDetails['alternative_name'])) then
               NeedsPrefixOrAltName := True;
            if ((NeedsFlag = False) and
                (IsPartial = False) and
                (NeedsPrefixOrAltName = False)) then
               break;
            if (IsPartial = True) then
               break;
            if (NeedsFlag = True) then
               break;
         end;
      end;
   end
   else
   if (VersionData is TJSONObject) then
   begin
      VersionAdded := GetVersionAsString(VersionData['version_added']);
      VersionRemoved := GetVersionAsString(VersionData['version_removed']);
      if ((VersionRemoved <> '') and (VersionRemoved <> 'False')) then
         YesNoUnknown := 'no'
      else
      begin
         if (VersionAdded <> '') then
         begin
            YesNoUnknown := IfThen((VersionAdded = 'False'), 'no', 'yes');
            if (Assigned(VersionData['partial_implementation'])) then
               IsPartial := True;
            if (Assigned(VersionData['flags'])) then
               NeedsFlag := True;
            if (Assigned(VersionData['prefix']) or
                Assigned(VersionData['alternative_name'])) then
               NeedsPrefixOrAltName := True;
         end;
      end;
   end;
   Version :=
      GetVersionOrRange(BrowserID, VersionAdded, VersionRemoved, YesNoUnknown);
   AddMDNBrowserRow(SupportTable, BrowserID, YesNoUnknown, Version, IsPartial,
                    NeedsFlag, NeedsPrefixOrAltName, Document);
end;

procedure AddMDNBox(const MDNBox: TElement;
                    const ID: UTF8String;
                    const Document: TDocument;
                          IsFirst: Boolean);
var
   MDNData, MDNSupport: TJSONObject;
   MDNButton, MDNFeature, SupportTable, CanIUseRow: TElement;
   MDNEngines: TJSONArray;
   MDNFilename, MDNName, MDNSlug, MDNSubpath, MDNSummary, BrowserID: UTF8String;
   FlagClassName, FlagSymbol, FlagTitle: UTF8String;
   EnginesClassName, EnginesText: UTF8String;
   EngineCount, i, j: Integer;
const
   kMDNURLBase = 'https://developer.mozilla.org/en-US/docs/Web/';
   kCanIUseURLBase = 'https://caniuse.com/#feat=';
   kCanIUseText = 'caniuse.com table';
   kFlagClassLessThanTwo = 'less-than-two-engines-flag';
   kFlagClassAll = 'all-engines-flag';
   kEnginesClassLessThanTwo = 'less-than-two-engines-text';
   kEnginesClassAll = 'all-engines-text';
   kInNone = 'No support in current engines.';
   kInOne = 'Support in one engine only.';
   kInAll = 'Support in all current engines.';
   kAltNameInOne = 'Support in one engine under other name.';
   kAltNameInSome = 'Support in some engines under other name.';
   kPrefixInOne = 'Prefixed support in one engine.';
   kPrefixInSome = 'Prefixed support in some engines.';
   kPartialInOne = 'Partial support in one engine.';
   kPartialInSome = 'Partial support in some engines.';

begin
   // Get the MDN details for this annotation.
   for MDNData in TJSONArray(MDNJSONData[ID]) do
   begin
      // MDNJSONData[ID] is an array of objects, where each object has data
      // associated with a particular MDN article which links to the given ID in
      // the HTML spec. We loop through those objects and assign each to an
      // MDNData, which contains data for a particular spec feature: the set of
      // engines with support for the feature, the browser-compat-data (BCD)
      // filename which contains data for the feature, the feature name, the
      // slug for the associated MDN article, the MDN article summary, the BCD
      // browser-compat support data for the feature, and the MDN article title.
      //
      // Example showing the structure of the JSON data:
      //
      // "sharedworker": [                                <= HTML spec ID
      //  {
      //    "engines": [ "blink", "gecko" ],              <= supporting engines
      //    "partial": [ "safari" ],                      <= partial support
      //    "prefixed": [ "gecko", "safari" ],            <= prefixed support
      //    "altname": [ "gecko" ],                       <= alternative name
      //    "filename": "api/SharedWorker.json",          <= BCD filename
      //    "name": "SharedWorker",                       <= BCD feature name
      //    "slug":    "API/SharedWorker",                <= MDN article slug
      //    "summary": "The SharedWorker interface ...",  <= MDN article summary
      //    "support": {"chrome":{"version_added":"4"},.. <= MDN support data
      //    "caniuse": {
      //        "feature": "sharedworkers",               <= caniuse.com anchor
      //        "title": "Shared Web Workers"             <= caniuse.com title
      //    },
      //    "title":   "SharedWorker"                     <= MDN article title
      //  },
      //  {
      //    /* data from another MDN article associated with this spec ID */
      //  }
      // ],
      //
      // Note that, in the browser-support data, the value for a particular
      // browser-ID key (e.g., "chrome") can optionally be an array of objects
      // (instead of just a single object as shown in example above).
      // See https://goo.gl/uejWa4 for documentation on the structure.
      MDNSlug := MDNData['slug'];
      MDNSummary := MDNData['summary'];
      MDNSubpath := Copy(MDNSlug, Pos('/', MDNSlug) + 1);
      MDNSupport := MDNData['support'];
      if (MDNData['engines'] is TJSONArray) then
         EngineCount := MDNData['engines'].Length
      else
         EngineCount := -1;
      if (IsFirst) then
      begin
         MDNButton := E(eButton, ['class', 'mdn-anno-btn',
            'onclick', 'toggleStatus(this)']);
         if (EngineCount = 0) then
         begin
            FlagClassName := kFlagClassLessThanTwo;
            FlagSymbol := #$26A0;
            FlagTitle := kInNone;
            if (Assigned(MDNData['altname'])) then
            begin
               FlagTitle := kAltNameInOne;
               if (MDNData['altname'].Length > 1) then
                  FlagTitle := kAltNameInSome;
            end
            else
            if (Assigned(MDNData['prefixed'])) then
            begin
               FlagTitle := kPrefixInOne;
               if (MDNData['Prefixed'].Length > 1) then
                  FlagTitle := kPrefixInSome;
            end
            else
            if (Assigned(MDNData['partial'])) then
            begin
               FlagTitle := kPartialInOne;
               if (MDNData['partial'].Length > 1) then
                  FlagTitle := kPartialInSome;
            end
         end
         else
         if (EngineCount = 1) then
         begin
            FlagClassName := kFlagClassLessThanTwo;
            FlagSymbol := #$26A0;
            FlagTitle := kInOne;
         end
         else
         if (EngineCount >= Length(MDNBrowsersProvidingCurrentEngines)) then
         begin
            FlagClassName := kFlagClassAll;
            FlagSymbol := #$2714;
            FlagTitle := kInAll;
         end;
         if ((EngineCount <> 2) and (MDNSupport <> nil)) then
            MDNButton.AppendChild(E(eB, ['class', FlagClassName,
                  'title', FlagTitle], Document, [T(FlagSymbol, Document)]));
         MDNButton.AppendChild(E(eSpan, ['data-x', ''], [T('MDN')]));
         MDNBox.AppendChild(MDNButton);
      end;
      IsFirst := False;
      MDNFeature := E(eDiv, ['class', 'feature']);
      MDNFeature.AppendChild(E(eP, [
         E(eA, ['href', kMDNURLBase + MDNSlug, 'title', MDNSummary],
            Document, [T(MDNSubpath, Document)])]));
      MDNBox.AppendChild(MDNFeature);
      if (EngineCount = 0) then
      begin
         EnginesClassName := kEnginesClassLessThanTwo;
         EnginesText := kInNone;
         if (Assigned(MDNData['altname'])) then
         begin
            EnginesText := kAltNameInOne;
            if (MDNData['altname'].Length > 1) then
               EnginesText := kAltNameInSome;
         end
         else
         if (Assigned(MDNData['prefixed'])) then
         begin
            EnginesText := kPrefixInOne;
            if (MDNData['prefixed'].Length > 1) then
               EnginesText := kPrefixInSome;
         end
         else
         if (Assigned(MDNData['partial'])) then
         begin
            EnginesText := kPartialInOne;
            if (MDNData['partial'].Length > 1) then
               EnginesText := kPartialInSome;
         end
      end
      else
      if (EngineCount = 1) then
      begin
         EnginesClassName := kEnginesClassLessThanTwo;
         EnginesText := kInOne;
      end
      else
      if (EngineCount >= Length(MDNBrowsersProvidingCurrentEngines)) then
      begin
         EnginesClassName := kEnginesClassAll;
         EnginesText := kInAll;
      end;
      if (MDNSupport = nil) then continue;
      if (EngineCount <> 2) then
         MDNFeature.AppendChild(E(eP, ['class', EnginesClassName],
            Document, [T(EnginesText, Document)]));
      SupportTable := E(eDiv, ['class', 'support']);
      MDNFeature.AppendChild(SupportTable);
      for BrowserID in MDNBrowsersProvidingCurrentEngines do
         ProcessBrowserData(BrowserID, MDNSupport[BrowserID], SupportTable,
                            Document);
      SupportTable.AppendChild(E(eHR));
      for BrowserID in MDNBrowsersWithBorrowedEngines do
         ProcessBrowserData(BrowserID, MDNSupport[BrowserID], SupportTable,
                            Document);
      SupportTable.AppendChild(E(eHR));
      for BrowserID in MDNBrowsersWithRetiredEngines do
         ProcessBrowserData(BrowserID, MDNSupport[BrowserID], SupportTable,
                            Document);
      SupportTable.AppendChild(E(eHR));
      for BrowserID in MDNBrowsersForMobileDevices do
         ProcessBrowserData(BrowserID, MDNSupport[BrowserID], SupportTable,
                            Document);
      if (Assigned(MDNData['caniuse'])) then
      begin
         SupportTable.AppendChild(E(eHR));
         CanIUseRow := E(eSpan, ['data-x', '', 'class', 'caniuse'], Document);
         CanIUseRow.AppendChild(E(eSpan, ['data-x', ''],
            [E(eA, ['href', kCanIUseURLBase + UTF8String(MDNData['caniuse']['feature']),
               'title', MDNData['caniuse']['title']], Document, [T(kCanIUseText)])]));
         SupportTable.AppendChild(CanIUseRow);
      end;
   end;
end;

procedure ProcessDocument(const Document: TDocument; const Variant: TVariants; out BigTOC: TElement; const SourceGitSHA: AnsiString);
type
   PElementListNode = ^TElementListNode;
   TElementListNode = record
      Value: TElement;
      Next: PElementListNode;
   end;

type
   TDFNEntry = record
      DFNElement: TElement;
      SubDFNElement: TElement;
      UsageCount: Cardinal;
   end;
   TDFNTable = specialize THashTable <UTF8String, TDFNEntry, UTF8StringUtils>;
   TCrossRefKind = (crExplicit, crImplicit);
   PCrossReferenceListNode = ^TCrossReferenceListNode;
   TCrossReferenceListNode = record
      LastHeadingText, SplitFilename: UTF8String;
      Topic, LastHeading: UTF8String;
      Element: TElement;
      Kind: TCrossRefKind;
      Next: PCrossReferenceListNode;
   end;
   TCrossReferences = record
      DFNs: TDFNTable;
      First, Last: PCrossReferenceListNode;
   end;

type
   TReferences = specialize THashTable <UTF8String, PElementListNode, UTF8StringUtils>;
   // Because we need to output lists of section names in order and lists of
   // xref anchors in order, for those instead of THashTable (which doesn't
   // preserve order), we use TFPGMap & TStringList (which both preserve order).
   TXrefAnchorsBySectionName = specialize TFPGMap <UTF8String, TStringList>;
   TXrefsByDFNAnchor = specialize THashTable <UTF8String, TXrefAnchorsBySectionName, UTF8StringUtils>;

var
   IDs: TElementMap; // The keys in these hashtables must outlive the DOM, since the DOM points to those strings
   CrossReferences: TCrossReferences;
   References: TReferences;
   MissingReferences: TReferences;
   SmallTOC: TElement;
   SmallTOCBookmark, BigTOCBookmark: TNode;
   LastHeadingRank: THeadingRank;
   LastTOCOL, LastTOCLI: TElement;
   CurrentSectionNumber: array[THeadingRank] of Cardinal;
   StringStore: TStringStore;
   Errors: Cardinal;
   XrefsByDFNAnchor: TXrefsByDFNAnchor;
   HeadingTextBySectionNumber: TStringMap;

   function NewXrefsByDFNAnchor(SectionName: UTF8String; Anchor: UTF8String): TXrefAnchorsBySectionName;
   var
      Anchors: TStringList;
      XrefAnchorsBySectionName: TXrefAnchorsBySectionName;
   begin
      Anchors := TStringList.Create;
      Anchors.Add(Anchor);
      XrefAnchorsBySectionName := TXrefAnchorsBySectionName.Create;
      XrefAnchorsBySectionName.Add(SectionName, Anchors);
      Result := XrefAnchorsBySectionName;
   end;

   function NewXrefAnchorsBySectionName(SectionName: UTF8String; Anchor: UTF8String): TXrefAnchorsBySectionName;
   var
      Anchors: TStringList;
      XrefAnchorsBySectionName: TXrefAnchorsBySectionName;
   begin
      Anchors := TStringList.Create;
      Anchors.Add(Anchor);
      XrefAnchorsBySectionName := TXrefAnchorsBySectionName.Create;
      XrefAnchorsBySectionName.Add(SectionName, Anchors);
      Result := XrefAnchorsBySectionName;
   end;

   procedure XrefsToJSON(XrefsByDFNAnchor: TXrefsByDFNAnchor);
   var
      DFNAnchor, SectionName, Anchor: UTF8String;
      IsFirstDFN, IsFirstSectionName, IsFirstAnchor: boolean;
      Xrefs: Rope;
      XrefsFile: Text;
      i: integer;
   begin
      Xrefs := Default(Rope);
      Xrefs.Append('{');
      IsFirstDFN := True;
      for DFNAnchor in XrefsByDFNAnchor do
         begin
            if (not IsFirstDFN) then
               Xrefs.Append(',');
            IsFirstDFN := False;
            Xrefs.Append('"');
            Xrefs.Append(@DFNAnchor);
            Xrefs.Append('":{');
            IsFirstSectionName := True;
            for i := 0 to XrefsByDFNAnchor[DFNAnchor].Count - 1 do
            begin
               if (not IsFirstSectionName) then
                  Xrefs.Append(',');
               IsFirstSectionName := False;
               Xrefs.Append('"');
               SectionName := XrefsByDFNAnchor[DFNAnchor].Keys[i];
               Xrefs.Append(@SectionName);
               Xrefs.Append('":[');
               IsFirstAnchor := True;
               for Anchor in XrefsByDFNAnchor[DFNAnchor][SectionName] do
               begin
                  if (not IsFirstAnchor) then
                     Xrefs.Append(',');
                  IsFirstAnchor := False;
                  Xrefs.Append('"');
                  Xrefs.Append(@Anchor);
                  Xrefs.Append('"');
               end;
               Xrefs.Append(']');
            end;
            Xrefs.Append('}');
         end;
      Xrefs.Append('}');
      Assign(XrefsFile, OutputDirectory + '/xrefs.json');
      Rewrite(XrefsFile);
      Write(XrefsFile, Xrefs.AsString);
      Close(XrefsFile);
   end;

   function NewAnchorSet(Anchor: UTF8String): TStringList;
   var
      Anchors: TStringList;
   begin
      Anchors := TStringList.Create;
      Anchors.Add(Anchor);
      Result := Anchors;
   end;

   procedure Fail(Message: UTF8String);
   var
      Index: Cardinal;
   begin
      for Index := 1 to Length(Message) do // $R-
         if (Message[Index] = #$0A) then
            Message[Index] := ' ';
      Writeln('Error: ', Message);
      Inc(Errors);
   end;

   procedure Warn(Message: UTF8String);
   var
      Index: Cardinal;
   begin
      for Index := 1 to Length(Message) do // $R-
         if (Message[Index] = #$0A) then
            Message[Index] := ' ';
      Writeln('Warning: ', Message);
   end;

   function Describe(const Element: TElement): UTF8String;
   var
      HeadingContents: UTF8String;
      Current: TNode;
   begin
      Current := Element;
      while (Assigned(Current) and ((not (Current is TElement)) or (not TElement(Current).HasProperties(propHeading)))) do
      begin
         if (Assigned(Current.PreviousSibling)) then
            Current := Current.PreviousSibling
         else
            Current := Current.ParentNode;
      end;
      if (Assigned(Current)) then
         HeadingContents := Current.TextContent.AsString
      else
         HeadingContents := ' - no heading found - ';
      Result := '<' + Element.LocalName.AsString + '> element containing "' + Element.TextContent.AsString + '"; previous heading contents are "' + HeadingContents + '"';
   end;

   procedure FirstPass();
   var
      Current: TNode;

      procedure DropNode(); inline;
      var
         Last: TNode;
      begin
         Last := Current;
         WalkToNextSkippingChildren(Current, Document, nil);
         Assert(Last <> Current);
         Last.Remove();
         Last.Free();
      end;

   var
      IDString: UTF8String;
      SubVariant: TAllVariants;
      AttrCount: Cardinal;
      Next: TNode;
   begin
      Current := Document;
      repeat
         if (Current is TElement) then
         begin
            if ((TElement(Current).HasAttribute(kExcludingAttribute[Variant])) or
                ((Variant <> vDEV) and
                 (TElement(Current).HasAttribute(kDEVAttribute)))) then
            begin
               DropNode();
               continue;
            end
            else
            if (TElement(Current).HasAttribute('id')) then
            begin
               IDString := TElement(Current).GetAttribute('id').AsString;
               if (IDs.Has(IDString)) then
                  Fail('Multiple elements found with ID "' + IDString + '"');
               IDs[IDString] := TElement(Current);
            end
            else
            if (Assigned(TElement(Current).Attributes)) then
            begin
               AttrCount := 0;
               for SubVariant in TVariants do
                  if (TElement(Current).HasAttribute(kExcludingAttribute[SubVariant])) then
                     Inc(AttrCount);
               if ((AttrCount > 0) and (AttrCount = TElement(Current).Attributes.Count) and
                   (TElement(Current).IsIdentity(nsHTML, eDiv) or TElement(Current).IsIdentity(nsHTML, eSpan))) then
               begin
                  Assert(not TElement(Current).HasAttribute(kCrossRefAttribute));
                  if (TElement(Current).ParentNode is TElement) then
                  begin
                     if (TElement(Current).HasChildNodes()) then
                     begin
                        Next := TElement(Current).FirstChild;
                        TElement(TElement(Current).ParentNode).ReplaceChildWithChildren(TElement(Current));
                        Current.Free();
                        Current := Next;
                        continue;
                     end
                     else
                     begin
                        DropNode();
                        continue;
                     end;
                  end;
               end;
            end;
         end
         else
         if (Current is TText) then
         begin
            Assert(Assigned(Current.ParentNode));
            Assert(Current.ParentNode is TElement);
            if (TElement(Current.ParentNode).HasProperties(propCannotContainPalpableText)) then
            begin
               // XXX Assert() that the text is whitespace only
               DropNode();
               continue;
            end;
         end;
         WalkToNext(Current, Document, nil);
      until Current = Document;
   end;

   function MungeForJsonOutput(const Original: UTF8String): UTF8String;
   begin
      // Replace all `"` (#$22) with `\"` and also while we're at it, replace
      // any newline (#$0A) with a space, and squash (DelSpace1) all sequences
      // of multiple space into a single space.
      Result := DelSpace1(StringReplace(StringReplace(Original, #$0A, ' ', [rfReplaceAll]), #$22, '\' + #$22, [rfReplaceAll]));
   end;

   function MungeTopicToID(const Original: UTF8String): UTF8String;
   var
      IndexSource, IndexTarget: Cardinal;
      HadSpace: Boolean;
   begin
      Result := Original;
      if (Result = '') then
         exit;
      IndexTarget := 0;
      HadSpace := True;
      for IndexSource := 1 to Length(Original) do // $R-
      begin
         case Original[IndexSource] of
            ' ', #$0A, '<', '>', '[', #$5C, ']', '^', '{', '|', '}', '%':
               begin
                  if (not HadSpace) then
                  begin
                     Inc(IndexTarget);
                     Result[IndexTarget] := '-';
                     HadSpace := True;
                  end;
               end;
            '"', '?', '`': ; // skipped silently
            'A'..'Z':
               begin
                  Inc(IndexTarget);
                  Result[IndexTarget] := Chr(Ord(Original[IndexSource])+$20); // $R-
                  HadSpace := False;
               end;
         else
            begin
               Inc(IndexTarget);
               if (IndexTarget <> IndexSource) then
               begin
                  // this implies UniqueString(Result)
                  // so we only call it if we need to
                  Result[IndexTarget] := Original[IndexSource];
               end;
               HadSpace := False;
            end;
         end;
      end;
      if ((HadSpace) and (IndexTarget > 0)) then
         Dec(IndexTarget);
      if (IndexTarget <> Length(Result)) then
         SetLength(Result, IndexTarget);
   end;

   function MungeStringToTopic(const Original: UTF8String): UTF8String;
   var
      IndexSource, IndexTarget: Cardinal;
      HadSpace: Boolean;
   begin
      Result := Original;
      if (Result = '') then
         exit;
      IndexTarget := 0;
      HadSpace := False;
      for IndexSource := 1 to Length(Original) do // $R-
      begin
         case Original[IndexSource] of
            '#': ; // so that "#text" is the same as "Text"
            ' ', #$0A:
               begin
                  if (not HadSpace) then
                  begin
                     Inc(IndexTarget);
                     Result[IndexTarget] := ' ';
                     HadSpace := True;
                  end;
               end;
            'A'..'Z':
               begin
                  Inc(IndexTarget);
                  Result[IndexTarget] := Chr(Ord(Original[IndexSource])+$20); // $R-
                  HadSpace := False;
               end;
         else
            begin
               Inc(IndexTarget);
               if (IndexTarget <> IndexSource) then
               begin
                  // this implies UniqueString(Result)
                  // so we only call it if we need to
                  Result[IndexTarget] := Original[IndexSource];
               end;
               HadSpace := False;
            end;
         end;
      end;
      if (HadSpace) then
         Dec(IndexTarget);
      if (IndexTarget <> Length(Result)) then
         SetLength(Result, IndexTarget);
   end;

   function GetTopicIdentifier(const Element: TElement): UTF8String;
   begin
      if (Element.HasAttribute(kCrossRefAttribute)) then
         Result := Element.GetAttribute(kCrossRefAttribute).AsString
      else
      begin
         if ((Element.ChildNodes.Length = 1) and (Element.FirstChild is TElement)) then
            Result := GetTopicIdentifier(TElement(Element.FirstChild))
         else
            Result := Element.TextContent.AsString;
      end;
      Result := MungeStringToTopic(Result);
   end;

   procedure GenerateID(const Base: UTF8String; const Element: TElement);
   var
      Index: Cardinal;
      ScratchRope: Rope;
      Candidate: UTF8String;
   begin
      if (IDs.Has(Base)) then
      begin
         Index := 2;
         repeat
            Candidate := Base + '-' + IntToStr(Index);
            Inc(Index);
         until not IDs.Has(Candidate);
      end
      else
         Candidate := Base;
      IDs[Candidate] := Element;
      {$IFOPT C+} AssertStringIsReffed(Candidate, 2); {$ENDIF} // once by us, once by the ID hashtable
      ScratchRope.Append(@Candidate[1], Length(Candidate)); // $R-
      Element.SetAttributeDestructively('id', ScratchRope);
   end;

   function EnsureID(const Element: TElement; const SuggestedID: UTF8String): CutRope;
   begin
      if (not Element.HasAttribute('id')) then
         GenerateID(SuggestedID, Element);
      Result := Element.GetAttribute('id');
      Assert(not Result.IsEmpty);
   end;

   var
      SplitFilename, SplitFilenameClassName, LastSeenHeadingText: UTF8String;
      LastSeenHeadingID, LastSeenReferenceName: UTF8String;
      InHeading, InReferences, InDFN: TElement;

   procedure SaveCrossReference(const Element: TElement);
   var
      CrossReferenceName: UTF8String;
      CrossRefListNode: PCrossReferenceListNode;
      DFNEntry: TDFNEntry;
      SectionNumber: Rope;
      CurrentHeadingRank: THeadingRank;
   begin
      CrossReferenceName := GetTopicIdentifier(Element);
      if (not CrossReferenceName.IsEmpty) then
      begin
         DFNEntry := CrossReferences.DFNs[CrossReferenceName];
         if (Element.HasAttribute(kSubDFNAttribute)) then
         begin
            if (Assigned(DFNEntry.SubDFNElement)) then
            begin
               Fail('Multiple secondary definitions (subdfn) for term "' + CrossReferenceName + '"' + #$0A +
                       'Parent of first says: "' + DFNEntry.SubDFNElement.ParentNode.TextContent.AsString + '", parent of second says: "' + Element.ParentNode.TextContent.AsString + '"');
            end;
            DFNEntry.SubDFNElement := Element;

            // Need to ensure an ID because it is normally only ensured for <dfn>s or elements that
            // are cross-referenced at least once. But subdfns can be linked to from outside, so
            // they need an ID even if they are not referenced.
            if (Variant <> vDEV) then
               // If this isn't the dev edition, then we need to adjust the ID value to prevent the
               // issue reported at https://github.com/whatwg/html-build/issues/121
               EnsureID(Element, MungeTopicToID(CrossReferenceName + '-dev'))
            else
               EnsureID(Element, MungeTopicToID(CrossReferenceName));
         end;
         SectionNumber := Default(Rope);
         for CurrentHeadingRank := 2 to LastHeadingRank do
         begin
            if (CurrentHeadingRank > 2) then
               SectionNumber.Append($002E);
            SectionNumber.Append(IntToStr(CurrentSectionNumber[CurrentHeadingRank]));
         end;
         Inc(DFNEntry.UsageCount);
         New(CrossRefListNode);
         CrossRefListNode^.Topic := CrossReferenceName;
         CrossRefListNode^.LastHeading := LastSeenHeadingID;
         if (SplitFilenameClassName = 'no-num') then
            CrossRefListNode^.LastHeadingText := LastSeenHeadingText
         else
            CrossRefListNode^.LastHeadingText := SectionNumber.AsString + ' ' + LastSeenHeadingText;
         CrossRefListNode^.SplitFilename := SplitFilename;
         CrossRefListNode^.Element := Element;
         if (Element.HasAttribute(kCrossRefAttribute) or Element.IsIdentity(nsHTML, eCode) or (Element.IsIdentity(nsHTML, eSpan) and not Assigned(Element.Attributes))) then
            CrossRefListNode^.Kind := crExplicit
         else
            CrossRefListNode^.Kind := crImplicit;
         CrossRefListNode^.Next := nil;
         if (not Assigned(CrossReferences.First)) then
            CrossReferences.First := CrossRefListNode
         else
            CrossReferences.Last^.Next := CrossRefListNode;
         CrossReferences.Last := CrossRefListNode;
         CrossReferences.DFNs[CrossReferenceName] := DFNEntry;
      end;
   end;

   procedure InsertMDNAnnotationForElement(const Element: TElement);
   var
      Candidate: TNode;
      ID, ClassName: UTF8String;
      TargetAncestor, MDNBox: TElement;
      IsFirst: Boolean;
   begin
      if (not(Element.HasAttribute('id'))) then
         exit;
      ID := Element.GetAttribute('id').AsString;
      if (not(MDNJSONData[ID] is TJSONArray)) then
         // No MDN article has a link to this ID.
         exit;

      MDNBox := E(eDiv);

      if (Element.HasProperties(propHeading) or
         HasAncestorWithProperties(Element, propHeading)) then
      begin
         // If the element we want to annotate is either (1) a descendant of
         // heading, or else (2) is itself a heading, we insert the annotation
         // after the heading. When generating multipage output, inserting the
         // annotation after the heading ensures that it ends up in the right
         // file. It can otherwise incorrectly end up in the previous split.
         ClassName := 'mdn-anno wrapped';
         MDNBox.SetAttribute('class', ClassName);
         Candidate := Element;
         while not TElement(Candidate).HasProperties(propHeading) do
            Candidate := Candidate.ParentNode;
         TargetAncestor := TElement(Candidate);
         if (TargetAncestor.NextElementSibling().GetAttribute('class').AsString = ClassName) then
         begin
            // If there's already an MDN box at the point where we want this,
            // then just re-use it (instead of creating another one).
            MDNBox := TargetAncestor.NextElementSibling();
            IsFirst := False;
         end
         else
         begin
            TElement(TargetAncestor.ParentNode).InsertBefore(MDNBox, TargetAncestor.NextSibling);
            IsFirst := True;
         end
      end
      else
      if (HasAncestor(Element, nsHTML, eTD) or HasAncestor(Element, nsHTML, eDT)) then
      begin
         // For elements we're annotating inside a <td> or <dt>, we append the
         // annotation directly to the <td> or <dt> as the last child. Otherwise
         // in cases where we have a long table (e.g., the list of events in the
         // index) or long <dl> list (e.g., the list of pseudo-classes), all the
         // annos for everything in that table or list end up being merged into
         // a single annotation way up at the beginning of the table or list.
         ClassName := 'mdn-anno wrapped before';
         Candidate := Element;
         while not TElement(Candidate).IsIdentity(nsHTML, eTD) and
               not TElement(Candidate).IsIdentity(nsHTML, eDT) do
            Candidate := Candidate.ParentNode;
         TargetAncestor := TElement(Candidate);
         if (TargetAncestor.LastElementChild().GetAttribute('class').AsString = ClassName) then
         begin
            // If there's already an MDN box at the point where we want this,
            // then just re-use it (instead of creating another one).
            MDNBox := TargetAncestor.LastElementChild();
            IsFirst := False;
         end
         else
         begin
            TargetAncestor.AppendChild(MDNBox);
            IsFirst := True;
         end;
      end
      else
      begin
         ClassName := 'mdn-anno wrapped before';

         // Find the furthest ancestor that is a direct child of <body>
         Candidate := Element;
         while not TElement(Candidate.ParentNode).IsIdentity(nsHTML, eBody) do
            Candidate := Candidate.ParentNode;
         TargetAncestor := TElement(Candidate);
         if (TargetAncestor.PreviousElementSibling().GetAttribute('class').AsString = ClassName) then
            // If there's already an MDN box at the point where we want this,
            // then just re-use it (instead of creating another one).
         begin
            MDNBox := TElement(TargetAncestor.PreviousElementSibling());
            IsFirst := False;
         end
         else
         begin
            TElement(TargetAncestor.ParentNode).InsertBefore(MDNBox, TargetAncestor);
            IsFirst := True;
         end
      end;

      MDNBox.SetAttribute('class', ClassName);
      AddMDNBox(MDNBox, ID, Document, IsFirst);
   end;

   function ProcessNode(var Node: TNode): Boolean; // return True if we are to keep this node, False if we drop it
   const
      CommitSnapshotBaseURL: AnsiString = '/commit-snapshots/';
      SourceGitBaseURL: AnsiString = 'https://github.com/whatwg/html/commit/';
      kExport = 'export';
      kDataExport = 'data-export';
      kNoExport = 'noexport';
      kDataNoExport = 'data-noexport';
      kFor = 'for';
      kDataDFNFor = 'data-dfn-for';
      kDataDFNType = 'data-dfn-type';
      // From https://github.com/tabatkins/bikeshed/blob/master/bikeshed/config/dfnTypes.py#L7
      // Plus the default "dfn" type (which we use on headings)
      kDFNTypes: array[1..41] of UTF8String =
         ('abstract-op',
          'property',
          'value',
          'at-rule',
          'descriptor',
          'type',
          'function',
          'selector',
          'element',
          'element-attr',
          'attr-value',
          'element-state',
          'event',
          'interface',
          'namespace',
          'extended-attribute',
          'constructor',
          'method',
          'argument',
          'attribute',
          'callback',
          'dictionary',
          'dict-member',
          'enum',
          'enum-value',
          'exception',
          'const',
          'typedef',
          'stringifier',
          'serializer',
          'iterator',
          'maplike',
          'setlike',
          'grammar',
          'scheme',
          'state',
          'mode',
          'context',
          'facet',
          'http-header',
          'dfn');
   var
      CandidateChild, SelectedForTransfer: TNode;
      CurrentHeadingRank: THeadingRank;
      Element, HeadingSelfLink, NewLI, SecondLI, NewLink, NewP, NewI, NewSpan, TempElement: TElement;
      Scratch: Rope;
      ExtractedData: CutRope;
      ClassName, Instruction, CrossReferenceName, ReferenceName: UTF8String;
      TodayYear, TodayMonth, TodayDay: Word;
      InSkippedNode, UsedLI: Boolean;
      ListNode: PElementListNode;
      DFNEntry: TDFNEntry;
      ID, HeadingText, ParentHeadingText, SectionNumber, ParentSectionNumber: UTF8String;
      ClassValue: String = '';
      DFNType: UTF8String = '';

   procedure TranslateBikeshedSyntax(const Node: TElement);
   begin
      if (Element.HasAttribute(kLTAttribute)) then
         Fail('Element with lt="" found, use data-x="" instead; dfn is ' + Describe(Element));
      if (Element.HasAttribute(kFor)) then
      begin
         ExtractedData := Element.GetAttribute(kFor);
         Element.SetAttributeDestructively(kDataDFNFor, ExtractedData);
         Element.RemoveAttribute(kFor);
      end;
      if (Element.HasAttribute(kExport)) then
      begin
         Element.SetAttribute(kDataExport, '');
         Element.RemoveAttribute(kExport);
      end;
      if (Element.HasAttribute(kNoExport)) then
      begin
         Element.SetAttribute(kDataNoExport, '');
         Element.RemoveAttribute(kNoExport);
      end;
      for DFNType in kDFNTypes do
         if (Element.HasAttribute(DFNType)) then
         begin
            if ((DFNType = 'dfn') and
               Element.IsIdentity(nsHTML, eDFN)) then
            begin
               Fail('<dfn> element found with redundant dfn="" '
                  + 'attribute; dfn is ' + Describe(Element));
            end;
            Element.SetAttribute(kDataDFNType, DFNType);
            Element.RemoveAttribute(DFNType);
         end;
      if (Element.HasAttribute(kDataDFNType)
         and Element.HasAttribute(kDataExport)) then
      begin
         Fail('Element found with dfn type name and redundant'
            + ' export attribute; dfn is ' + Describe(Element));
      end;
   end;

   begin
      Result := True;
      if (Node is TElement) then
      begin
         Element := TElement(Node);
         if (Element.HasAttribute('class')) then
            ClassValue := Element.GetAttribute('class').AsString;
         if (IsHighlighterTarget(Element, ClassValue) and AnsiContainsStr(ClassValue, 'idl') and (Variant = vDEV)) then
         begin
            Result := False;
         end
         else
         if (Element.HasProperties(propHeading)) then
         begin
            TranslateBikeshedSyntax(Element);
            ClassName := Element.GetAttribute('class').AsString;
            CurrentHeadingRank := (Element as THTMLHeadingElement).Rank;
            if (CurrentHeadingRank > 1) then
            begin
               InHeading := Element;
               if (Element.HasAttribute('split-filename')) then
               begin
                  SplitFilename := Element.GetAttribute('split-filename').AsString;
                  SplitFilenameClassName := Element.GetAttribute('class').AsString;
               end;
               LastSeenHeadingText := Element.TextContent.AsString;
               LastSeenHeadingID := MungeTopicToID(Element.TextContent.AsString);
               if (LastSeenHeadingID = '') then
                  LastSeenHeadingID := 'blank-heading';
               ExtractedData := EnsureID(Element, LastSeenHeadingID);
               Assert(not ExtractedData.IsEmpty);
               LastSeenHeadingID := ExtractedData.AsString;

               // Append a self-link to each header
               if (ClassName <> 'no-num no-toc') then
               begin
                  HeadingSelfLink := ConstructHTMLElement(eA);
                  HeadingSelfLink.SetAttribute('class', 'self-link');

                  Scratch := Default(Rope);
                  Scratch.Append('#');
                  Scratch.AppendDestructively(ExtractedData);
                  HeadingSelfLink.SetAttributeDestructively('href', Scratch);

                  Element.AppendChild(HeadingSelfLink);
               end;

               // ExtractedData is no longer valid

               if (CurrentHeadingRank > LastHeadingRank) then
               begin
                  Inc(LastHeadingRank);
                  if (CurrentHeadingRank > LastHeadingRank) then
                  begin
                     Fail('heading rank failure - jumped a heading with the heading with text "' + Element.TextContent.AsString + '"');
                     LastHeadingRank := CurrentHeadingRank;
                  end;
                  Assert(CurrentHeadingRank = LastHeadingRank);
                  CurrentSectionNumber[CurrentHeadingRank] := 0;
                  if ((ClassName <> 'no-num no-toc') and (CurrentHeadingRank > 2)) then
                  begin
                     if (Assigned(LastTOCOL)) then
                     begin
                        if (not Assigned(LastTOCLI)) then
                        begin
                           Fail('First heading after Table of Contents must be an <h2> also; but this one is rank ' + IntToStr(CurrentHeadingRank) + ' (with text "' + Element.TextContent.AsString + '")');
                        end
                        else
                        begin
                           LastTOCOL := ConstructHTMLElement(eOL);
                           LastTOCLI.AppendChild(LastTOCOL);
                        end;
                     end;
                  end;
               end
               else
               while (CurrentHeadingRank < LastHeadingRank) do
               begin
                  Dec(LastHeadingRank);
                  if ((ClassName <> 'no-num no-toc') and (Assigned(LastTOCOL))) then
                  begin
                     if (Assigned(LastTOCOL.ParentNode) and Assigned(LastTOCOL.ParentNode.ParentNode)) then
                     begin
                        LastTOCOL := LastTOCOL.ParentNode.ParentNode as TElement;
                        Assert(LastTOCOL.IsIdentity(nsHTML, eOL));
                     end
                     else
                        Fail('Weird heading structure near ' + Describe(Element));
                  end;
               end;
               Assert(LastHeadingRank = CurrentHeadingRank);
               if (ClassName <> 'no-num no-toc') then
               begin
                  if (ClassName <> 'no-num') then
                  begin
                     Inc(CurrentSectionNumber[LastHeadingRank]);
                     Scratch := Default(Rope);
                     for CurrentHeadingRank := 2 to LastHeadingRank do
                     begin
                        if (CurrentHeadingRank > 2) then
                           Scratch.Append($002E);
                        Scratch.Append(IntToStr(CurrentSectionNumber[CurrentHeadingRank]));
                     end;
                     if (Variant = vDEV) then
                     begin
                        ID := Element.GetAttribute('id').AsString;
                        SectionNumber := Scratch.AsString;
                        HeadingText := MungeForJsonOutput(Element.TextContent.AsString);
                        if (not IsFirstSearchIndexItem) then
                           Write(SearchIndexJsonFile, ',');
                        IsFirstSearchIndexItem := False;
                        Write(SearchIndexJsonFile, '{');
                        Write(SearchIndexJsonFile, '"url":"' + SplitFilename + '.html#' + ID + '",');
                        Write(SearchIndexJsonFile, '"text":"' + HeadingText + '",');
                        if (LastDelimiter('.', SectionNumber) <> 0) then
                        begin
                           ParentSectionNumber := Copy(SectionNumber, 1, LastDelimiter('.', SectionNumber) - 1);
                           ParentHeadingText := HeadingTextBySectionNumber[ParentSectionNumber];
                           Write(SearchIndexJsonFile, '"section":"' + SectionNumber + ' ' + UTF8Encode(#$2014) + ' ' + ParentHeadingText + '"');
                        end
                        else
                        begin
                           Write(SearchIndexJsonFile, '"section":"' + SectionNumber + '"');
                        end;
                        Write(SearchIndexJsonFile, '}');
                        HeadingTextBySectionNumber[SectionNumber] := HeadingText;
                     end;
                     NewSpan := ConstructHTMLElement(eSpan);
                     NewSpan.SetAttribute('class', 'secno');
                     NewSpan.appendChild(TText.CreateDestructively(Scratch));
                     Element.InsertBefore(TText.Create(#$0020), Element.FirstChild);
                     Element.InsertBefore(NewSpan, Element.FirstChild);
                  end
                  else
                  begin
                     // TODO Find a more robust way to populate the search index
                     // for this no-num backmatter sections (Index, References,
                     // Acknowledgments) because the below is brittle. (It's
                     // necessary to handle the backmatter sections differently
                     // because they don't have section numbers, so we can't use
                     // HeadingTextBySectionNumber to identify parent sections.)
                     if (Variant = vDEV) then
                     begin
                        ID := Element.GetAttribute('id').AsString;
                        HeadingText := MungeForJsonOutput(Element.TextContent.AsString);
                        Write(SearchIndexJsonFile, ',{');
                        Write(SearchIndexJsonFile, '"url": "' + SplitFilename + '.html#' + ID + '",');
                        Write(SearchIndexJsonFile, '"text": "' + HeadingText + '",');
                        // TODO Especially find a better way for this particular
                        // bit, given it'll break if there's a change to either
                        // the split-filename or id value for the Index section.
                        // It's possible that LastTOCLI could help here.
                        if ((SplitFilename = 'indices') and (ID <> 'index')) then
                        begin
                          Write(SearchIndexJsonFile, '"section": "' + ' ' + UTF8Encode(#$2014) + ' Index"');
                        end
                        else
                        begin
                          Write(SearchIndexJsonFile, '"section": ""');
                        end;
                        Write(SearchIndexJsonFile, '}');
                     end;
                  end;
                  if (Assigned(LastTOCOL) or Assigned(SmallTOC)) then
                  begin
                     NewLI := ConstructHTMLElement(eLI);
                     NewLink := ConstructHTMLElement(eA);
                     Scratch := Default(Rope);
                     Scratch.Append($0023);
                     ExtractedData := Element.GetAttribute('id');
                     Assert(not ExtractedData.IsEmpty);
                     Scratch.AppendDestructively(ExtractedData);
                     NewLink.SetAttributeDestructively('href', Scratch);
                     TempElement := Element.CloneNode(True);
                     CandidateChild := TempElement.FirstChild;
                     CandidateChild.Remove();
                     NewLink.AppendChild(CandidateChild);
                     CandidateChild := TempElement.FirstChild;
                     InSkippedNode := False;
                     while (Assigned(CandidateChild) and ((not (CandidateChild is TElement)) or (TElement(CandidateChild).GetAttribute('class').AsString <> 'self-link'))) do
                     begin
                        if ((CandidateChild is TElement) and ((TElement(CandidateChild).IsIdentity(nsHTML, eDFN)) or (TElement(CandidateChild).IsIdentity(nsHTML, eSpan)))) then
                        begin
                           InSkippedNode := True;
                           if (CandidateChild.HasChildNodes()) then
                           begin
                              CandidateChild := (CandidateChild as TElement).FirstChild;
                              SelectedForTransfer := CandidateChild;
                           end
                           else
                              SelectedForTransfer := nil;
                        end
                        else
                        begin
                           SelectedForTransfer := CandidateChild;
                        end;
                        if (Assigned(CandidateChild.NextSibling)) then
                        begin
                           CandidateChild := CandidateChild.NextSibling
                        end
                        else
                        if (InSkippedNode) then
                        begin
                           CandidateChild := CandidateChild.ParentNode.NextSibling;
                           InSkippedNode := False;
                        end
                        else
                           CandidateChild := nil;
                        if (Assigned(SelectedForTransfer)) then
                        begin
                           SelectedForTransfer.Remove();
                           NewLink.AppendChild(SelectedForTransfer);
                        end;
                     end;
                     TempElement.Free();
                     NewLI.AppendChild(NewLink);
                     UsedLI := False;
                     if (Assigned(LastTOCOL)) then
                     begin
                        LastTOCOL.AppendChild(NewLI);
                        LastTOCLI := NewLI;
                        UsedLI := True;
                     end;
                     if (Assigned(SmallTOC) and (LastHeadingRank = 2)) then
                     begin
                        if (UsedLI) then
                        begin
                           SecondLI := NewLI.CloneNode(True);
                           ExtractedData := EnsureID(NewLI, 'toc-' + LastSeenHeadingID);
                           Assert(not ExtractedData.IsEmpty);
                           Scratch := Default(Rope);
                           Scratch.Append($0023);
                           Scratch.AppendDestructively(ExtractedData);
                           (SecondLI.FirstChild as TElement).SetAttributeDestructively('href', Scratch);
                        end
                        else
                           SecondLI := NewLI;
                        SmallTOC.AppendChild(SecondLI);
                     end;
                  end;
               end;
            end;
         end
         else
         if (Element.IsIdentity(nsHTML, eBody)) and (Variant = vSnap) then
         begin
            Element.SetAttribute('class', 'status-LS-COMMIT');
         end
         else
         if (Element.IsIdentity(nsHTML, eSpan)) then
         begin
            ClassName := Element.GetAttribute('class').AsString;
            if ((ClassName = 'pubdate') or (ClassName = 'pubyear')) then
            begin
               if ((not Element.HasChildNodes()) or (not (Element.FirstChild is TText))) then
               begin
                  Fail(ClassName + ' span must contain exactly one text node');
               end
               else
               begin
                  Scratch := Default(Rope);
                  DecodeDate(Date, TodayYear, TodayMonth, TodayDay);
                  if (ClassName = 'pubdate') then
                  begin
                     Scratch.Append(IntToStr(TodayDay));
                     Scratch.Append($0020);
                     Scratch.Append(@Months[TodayMonth][1], Length(Months[TodayMonth])); // $R-
                     Scratch.Append($0020);
                  end;
                  Scratch.Append(IntToStr(TodayYear));
                  TText(Element.FirstChild).Data := Scratch;
               end;
            end
            else
            begin
               if (Element.HasAttribute(kLTAttribute)) then
                  Fail('<span> with lt="" found, use data-x="" instead; span is ' + Describe(Element));
               if (Assigned(InDFN)) then
                  Fail('<span> inside <dfn>; span is ' + Describe(Element))
               else
                  SaveCrossReference(Element);
            end;
         end
         else
         // For vReview the title is already taken care of
         if (Element.IsIdentity(nsHTML, eTitle)) and (Variant = vSnap) then
         begin
            Element.AppendChild(TText.Create(' (Commit Snapshot '));
            Element.AppendChild(TText.Create(SourceGitSHA));
            Element.AppendChild(TText.Create(')'));
         end
         else
         if (Element.IsIdentity(nsHTML, eDFN)) then
         begin
            TranslateBikeshedSyntax(Element);
            CrossReferenceName := GetTopicIdentifier(Element);
            if (Assigned(InDFN)) then
               Fail('Nested <dfn>: ' + Describe(Element));
            InDFN := Element;
            if (not CrossReferenceName.IsEmpty) then
            begin
               DFNEntry := CrossReferences.DFNs[CrossReferenceName];
               if (Assigned(DFNEntry.DFNElement)) then
               begin
                  Fail('Multiple definitions for term "' + CrossReferenceName + '"' + #$0A +
                       'Parent of first says: "' + DFNEntry.DFNElement.ParentNode.TextContent.AsString + '", parent of second says: "' + Element.ParentNode.TextContent.AsString + '"');
               end;
               if (Assigned(InHeading)) then
               begin
                  DFNEntry.DFNElement := InHeading;
                  if (Element.HasAttribute(kCrossSpecRefAttribute)) then
                     Fail('A deferring definition can''t be in a heading, but this one is: ' + Describe(Element));
               end
               else
               begin
                  DFNEntry.DFNElement := Element;
                  EnsureID(Element, MungeTopicToID(CrossReferenceName));
                  if (Element.HasAttribute(kCrossSpecRefAttribute)) then
                  begin
                     ExtractedData := Element.GetAttribute(kCrossSpecRefAttribute);
                     NewLink := ConstructHTMLElement(eA);
                     NewLink.SetAttributeDestructively('href', ExtractedData);
                     Element.SwapChildNodes(NewLink);
                     Element.AppendChild(NewLink);
                  end;
               end;
               CrossReferences.DFNs[CrossReferenceName] := DFNEntry;
            end;
         end
         else
         if (Element.IsIdentity(nsHTML, eCode) and (not Assigned(InDFN))) then
         begin
            if (Element.HasAttribute(kLTAttribute)) then
               Fail('<code> with lt="" found, use data-x="" instead; code is ' + Describe(Element));
            SaveCrossReference(Element);
         end
         else
         if (Element.isIdentity(nsHTML, eA) and (Element.GetAttribute('href').AsString = '/commit-snapshots/[SHA]')) then
         begin
            Scratch := Default(Rope);
            Scratch.Append(@CommitSnapshotBaseURL);
            Scratch.Append(@SourceGitSHA);
            Scratch.Append('/');
            Element.SetAttributeDestructively('href', Scratch);
         end
         else
         if (Element.isIdentity(nsHTML, eA) and (Element.GetAttribute('class').AsString = 'sha-link') and (Variant = vSnap)) then
         begin
            Element.AppendChild(TText.Create(SourceGitSHA));
            Element.AppendChild(TText.Create(' commit'));
            Scratch := Default(Rope);
            Scratch.Append(@SourceGitBaseURL);
            Scratch.Append(@SourceGitSHA);
            Element.SetAttributeDestructively('href', Scratch);
         end
         else
         if (Element.isIdentity(nsHTML, eA) and (not Element.HasAttribute(kHrefAttribute))) then
         begin
            Fail('<a> without href found: ' + Describe(Element));
         end
         else
         if (Element.IsIdentity(nsHTML, eI) and (not Assigned(InDFN)) and (Element.HasAttribute(kCrossRefAttribute))) then
         begin
            if (Element.GetAttribute(kCrossRefAttribute).IsEmpty) then
               Fail('<i> with empty data-x="": ' + Describe(Element));
            SaveCrossReference(Element);
         end
         else
         if (Element.IsIdentity(nsHTML, eVar) and (not Assigned(InDFN)) and (Element.HasAttribute(kCrossRefAttribute))) then
         begin
            if (Element.GetAttribute(kCrossRefAttribute).IsEmpty) then
               Fail('<var> with empty data-x="": ' + Describe(Element));
            SaveCrossReference(Element);
         end
         else
         if (Element.IsIdentity(nsHTML, eRef)) then
         begin
            ExtractedData := Element.GetAttribute('spec');
            ReferenceName := ExtractedData.AsString;
            New(ListNode);
            ListNode^.Value := Element;
            ListNode^.Next := References[ReferenceName];
            References[ReferenceName] := ListNode;
            MissingReferences[ReferenceName] := ListNode;
            NewLink := ConstructHTMLElement(eA);
            Scratch := Default(Rope);
            Scratch.Append('#refs');
            Scratch.AppendDestructively(ExtractedData); // $R-
            NewLink.SetAttributeDestructively('href', Scratch);
            ExtractedData := Element.GetAttribute('spec');
            Scratch := Default(Rope);
            Scratch.Append('[');
            Scratch.AppendDestructively(ExtractedData); // $R-
            Scratch.Append(']');
            NewLink.AppendChild(TText.CreateDestructively(Scratch));
            (Node.ParentNode as TElement).ReplaceChild(NewLink, Node);
            Node.Free();
            Node := NewLink;
            Result := ProcessNode(Node);
         end
         else
         if (Element.IsIdentity(nsHTML, eDL) and (Element.GetAttribute('id').AsString = 'ref-list')) then
         begin
            InReferences := Element;
         end
         else
         if (Assigned(InReferences) and (Element.IsIdentity(nsHTML, eDT))) then
         begin
            LastSeenReferenceName := Element.GetAttribute('id').AsString;
            if (Length(LastSeenReferenceName) > 4) then
            begin
               LastSeenReferenceName := Copy(LastSeenReferenceName, 5, Length(LastSeenReferenceName) - 4);
               if (not References.Has(LastSeenReferenceName)) then
               begin
                  if (Variant <> vDEV) then
                     Fail('Unused reference: [' + LastSeenReferenceName + ']');
                  Result := False;
               end
               else
                  MissingReferences.Remove(LastSeenReferenceName);
            end
            else
               LastSeenReferenceName := '';
         end
         else
         if (Assigned(InReferences) and (Element.IsIdentity(nsHTML, eDD))) then
         begin
            if (not References.Has(LastSeenReferenceName)) then
               Result := False;
         end
      end
      else
      if (Node is TComment) then
      begin
         Instruction := (Node as TComment).Data.AsString;
         if (Instruction = 'toc') then
         begin
            if (LastHeadingRank <> 2) then
            begin
               Fail('Table of Contents must be in the context of an <h2>; current rank is ' + IntToStr(LastHeadingRank));
            end
            else
            begin
               BigTOC := ConstructHTMLElement(eOL);
               BigTOC.SetAttribute('class', 'toc');
               BigTOCBookmark := Node;
               LastTOCOL := BigTOC;
            end;
         end
         else
         if ((Instruction = 'smalltoc') and (Variant <> vDEV)) then
         begin
            if (LastHeadingRank <> 2) then
            begin
               Fail('Small Table of Contents must be in the context of an <h2>; current rank is ' + IntToStr(LastHeadingRank));
            end
            else
            begin
               SmallTOC := ConstructHTMLElement(eOL);
               SmallTOC.SetAttribute('class', 'brief toc');
               SmallTOC.SetAttribute(kExcludingAttribute[vSplit], '');
               SmallTOCBookmark := Node;
            end;
         end
         else
         if ((Instruction = ' NON-NORMATIVE SECTION ') and (Variant <> vDEV)) then
         begin
            NewP := ConstructHTMLElement(eP);
            NewI := ConstructHTMLElement(eI);
            NewI.AppendChild(TText.Create(kNonNormative));
            NewP.AppendChild(NewI);
            (Node.ParentNode as TElement).ReplaceChild(NewP, Node);
            Node.Free();
            Node := NewP;
            Result := ProcessNode(Node);
         end
         else
         if ((Instruction = 'INSERT TRACKING') and (Variant <> vDEV)) then
         begin
            TempElement := E(eSpan, [kCrossRefAttribute, 'tracking vector',
                                     'title', 'There is a tracking vector here.',
                                     'class', 'tracking-vector'],
                                    [E(eImg, ['src', 'https://resources.whatwg.org/tracking-vector.svg',
                                              'alt', '(This is a tracking vector.)',
                                              'width', '46',
                                              'height', '64',
                                              'crossorigin', ''])]);
            (Node.ParentNode as TElement).ReplaceChild(TempElement, Node);
            Node.Free();
            Node := TempElement;
            Result := ProcessNode(Node);
         end
         else
            Result := False;
      end;
      // XXX remove boring whitespace (none in <pre>s, more than one space between phrasing elements, any spaces elsewhere)
   end;

   procedure ProcessNodeExit(const Node: TElement);
   begin
      if (Variant <> vReview) then
         InsertMDNAnnotationForElement(Node);
      if (Node = InHeading) then
         InHeading := nil
      else
      if (Node = InDFN) then
         InDFN := nil
      else
      if (Node = InReferences) then
         InReferences := nil;
   end;

   procedure SecondPass();
   var
      Current, Last: TNode;
      Done: Boolean;
   begin
      Current := Document;
      repeat
         if (not ProcessNode(Current)) then
         begin
            Last := Current;
            Done := not WalkToNextSkippingChildren(Current, Document, @ProcessNodeExit);
            Last.Remove();
            Last.Free();
         end
         else
            Done := not WalkToNext(Current, Document, @ProcessNodeExit);
      until Done;
   end;

var
   CrossRefNode, CrossRefNodeNext: PCrossReferenceListNode;
   ID, ExtractedData: CutRope;
   Scratch: Rope;
   NewLink, DFN: TElement;
   DFNEntry: TDFNEntry;
   ListNodeHead, ListNode, NextListNode: PElementListNode;
   Anchor, DFNAnchor, SectionName, MissingReferenceName: UTF8String;
begin
   HighlighterOutputByJSONContents := TStringMap.Create(@UTF8StringHash32);
   XrefsByDFNAnchor := TXrefsByDFNAnchor.Create(@UTF8StringHash32);
   HeadingTextBySectionNumber := TStringMap.Create(@UTF8StringHash32);
   IDs := TElementMap.Create(@UTF8StringHash32);
   Document.TakeOwnership(IDs);
   StringStore := TStringStore.Create();
   Document.TakeOwnership(StringStore);
   References := TReferences.Create(@UTF8StringHash32, 12);
   MissingReferences := TReferences.Create(@UTF8StringHash32, 12);
   CrossReferences.DFNs := TDFNTable.Create(@UTF8StringHash32);
   CrossReferences.First := nil;
   CrossReferences.Last := nil;
   CurrentSectionNumber[1] := 0;
   SmallTOC := nil;
   SmallTOCBookmark := nil;
   BigTOC := nil;
   LastTOCOL := nil;
   BigTOCBookmark := nil;
   LastHeadingRank := 1;
   InHeading := nil;
   InDFN := nil;
   InReferences := nil;
   Errors := 0;
   try
      try
         // First pass
         {$IFDEF DEBUG} Writeln('Finding IDs and stripping excluded sections...'); {$ENDIF}
         FirstPass();
         // Second pass - make the changes to the DOM that we need
         if (Variant = vDEV) then
         begin
            {$IFDEF DEBUG} Writeln('Adjusting headers, references, finding cross-references, creating search-index.json...'); {$ENDIF}
            Assign(SearchIndexJsonFile, OutputDirectory + '/multipage-dev/search-index.json');
            Rewrite(SearchIndexJsonFile);
            Write(SearchIndexJsonFile, '[');
            SecondPass();
            Write(SearchIndexJsonFile, ']');
            Close(SearchIndexJsonFile);
         end
         else
         begin
            {$IFDEF DEBUG} Writeln('Adjusting headers, references, finding cross-references...'); {$ENDIF}
            SecondPass();
         end;
         // Insert cross-references
         {$IFDEF DEBUG} Writeln('Inserting cross-references...'); {$ENDIF}
         CrossRefNode := CrossReferences.First;
         while (Assigned(CrossRefNode)) do
         begin
            Assert(Assigned(CrossRefNode^.Element));
            if (not HasAncestor(CrossRefNode^.Element, nsHTML, eA)) then
            begin
               DFNEntry := CrossReferences.DFNs[CrossRefNode^.Topic];
               if (Assigned(DFNEntry.DFNElement)) then
                  DFN := DFNEntry.DFNElement
               else
                  DFN := DFNEntry.SubDFNElement;
               if (DFN = CrossRefNode^.Element) then
                  DFN := nil;
               if (Assigned(DFN)) then
               begin
                  if (CrossRefNode^.Element.HasAttribute(kUndefinedAttribute)) then
                     Fail('Use of defined term "' + CrossRefNode^.Topic + '" marked as undefined: ' + Describe(CrossRefNode^.Element));
                  ID := EnsureID(DFN, MungeTopicToID(CrossRefNode^.Topic));
                  EnsureID(CrossRefNode^.Element, CrossRefNode^.LastHeading + ':' + ID.AsString);
                  NewLink := ConstructHTMLElement(eA);
                  CrossRefNode^.Element.SwapChildNodes(NewLink);
                  if (Variant <> vDEV) then
                  begin
                     DFNAnchor := ID.AsString;
                     SectionName := MungeForJsonOutput(CrossRefNode^.LastHeadingText);
                     Anchor := CrossRefNode^.SplitFilename.AsString + '.html#' + CrossRefNode^.Element.GetAttribute('id').AsString;
                     if (not XrefsByDFNAnchor.Has(DFNAnchor)) then
                     begin
                        XrefsByDFNAnchor.Add(DFNAnchor, NewXrefsByDFNAnchor(SectionName, Anchor));
                     end;
                     if (XrefsByDFNAnchor[DFNAnchor].IndexOf(SectionName) = -1) then
                     begin
                        XrefsByDFNAnchor[DFNAnchor].Add(SectionName, NewAnchorSet(Anchor));
                     end
                     else
                       if (XrefsByDFNAnchor[DFNAnchor][SectionName].IndexOf(Anchor) = -1) then
                          XrefsByDFNAnchor[DFNAnchor][SectionName].Add(Anchor);
                  end;
                  if (CrossRefNode^.Element.IsIdentity(nsHTML, eSpan)) then
                  begin
                     if (CrossRefNode^.Element.HasAttribute('id')) then
                        IDs[CrossRefNode^.Element.GetAttribute('id').AsString] := NewLink;
                     CrossRefNode^.Element.SwapAttributes(NewLink);
                     (CrossRefNode^.Element.ParentNode as TElement).ReplaceChild(NewLink, CrossRefNode^.Element);
                     CrossRefNode^.Element.Free();
                  end
                  else
                  begin
                     CrossRefNode^.Element.AppendChild(NewLink);
                  end;
                  if (DFN.HasAttribute(kCrossSpecRefAttribute)) then
                  begin
                     ExtractedData := DFN.GetAttribute(kCrossSpecRefAttribute);
                     NewLink.SetAttributeDestructively('href', ExtractedData);
                     NewLink.SetAttributeDestructively(kCrossRefInternalLinkAttribute, ID);
                  end
                  else
                  begin
                     Scratch := Default(Rope);
                     Scratch.Append($0023);
                     Scratch.AppendDestructively(ID);
                     NewLink.SetAttributeDestructively('href', Scratch);
                  end;
               end
               else
               if (Variant <> vDEV) and
                  not(CrossRefNode^.Element.IsIdentity(nsHTML, eCode)
                     and (CrossRefNode^.Element.ParentNode is TElement)
                     and (CrossRefNode^.Element.ParentNode as TElement).IsIdentity(nsHTML, ePre)) then
               begin
                  if (CrossRefNode^.Kind = crExplicit) then
                  begin
                     if (not CrossRefNode^.Element.HasAttribute(kUndefinedAttribute)) then
                     begin
                        if (DFNEntry.UsageCount = 1) then
                        begin
                           Fail('missing <dfn> for topic "' + CrossRefNode^.Topic + '" explicitly from ' + Describe(CrossRefNode^.Element))
                        end
                        else
                        begin
                           Fail('missing <dfn> for topic "' + CrossRefNode^.Topic + '" from ' + IntToStr(DFNEntry.UsageCount) + ' sites including explicitly from ' + Describe(CrossRefNode^.Element));
                        end;
                     end;
                  end
                  else
                  if (DFNEntry.UsageCount > 1) then
                     Fail('missing <dfn> for topic "' + CrossRefNode^.Topic + '" from ' + IntToStr(DFNEntry.UsageCount) + ' sites including implicitly from ' + Describe(CrossRefNode^.Element))
               end;
            end;
            CrossRefNode := CrossRefNode^.Next;
         end;
         if (Variant <> vDEV) then
         begin
            {$IFDEF DEBUG} Writeln('Writing xrefs.json...'); {$ENDIF}
            XrefsToJSON(XrefsByDFNAnchor);
         end;
         {$IFDEF DEBUG} Writeln('Inserting tables of contents...'); {$ENDIF}
         if (Assigned(BigTOCBookmark)) then
         begin
            Assert(Assigned(BigTOC));
            (BigTOCBookmark.ParentNode as TElement).ReplaceChild(BigTOC, BigTOCBookmark);
            FreeAndNil(BigTOCBookmark);
         end;
         if (Assigned(SmallTOCBookmark)) then
         begin
            Assert(Assigned(SmallTOC));
            (SmallTOCBookmark.ParentNode as TElement).ReplaceChild(SmallTOC, SmallTOCBookmark);
            FreeAndNil(SmallTOCBookmark);
         end;
      except
         FreeAndNil(BigTOC);
         FreeAndNil(SmallTOC);
         raise;
      end;
   finally
      CrossRefNode := CrossReferences.First;
      while (Assigned(CrossRefNode)) do
      begin
         CrossRefNodeNext := CrossRefNode^.Next;
         Dispose(CrossRefNode);
         CrossRefNode := CrossRefNodeNext;
      end;
      CrossReferences.DFNs.Free();
      for ListNodeHead in References.Values do
      begin
         ListNode := ListNodeHead;
         while (Assigned(ListNode)) do
         begin
            NextListNode := ListNode^.Next;
            Dispose(ListNode);
            ListNode := NextListNode;
         end;
      end;
      if (Variant <> vDEV) then
      begin
         for MissingReferenceName in MissingReferences.GetEnumerator do
         begin
            MissingReferences.Remove(MissingReferenceName);
            Fail('Missing reference: [' + MissingReferenceName + ']');
         end;
      end;
      References.Free();
      MissingReferences.Free();
   end;
   if (Errors > 0) then
   begin
      Writeln('Error count: ', Errors);
      if (ExitCode = 0) then
         // only set ExitCode to 1 if some other part of the code has not already set to other value
         ExitCode := 1;
      // raise EAbort.Create(IntToStr(Errors) + ' errors found.');
   end;
end;

// ========================================================================================================================
// SERIALISER
// ========================================================================================================================

type
   TQuoteType = (qtNone, qtSingle, qtDouble);

(*
function ForceEscapeAttributeSingleQuote(constref Value: Rope): TWire;
var
   Enumerator: RopeEnumerator;
begin
   Result.Init();
   Enumerator := RopeEnumerator.Create(@Value);
   while (Enumerator.MoveNext()) do
      case (Enumerator.Current.Value) of
         $0027:
            begin
               Result.Append(Ord('&'));
               Result.Append(Ord('a'));
               Result.Append(Ord('p'));
               Result.Append(Ord('o'));
               Result.Append(Ord('s'));
               Result.Append(Ord(';'));
            end;
         $0026:
            begin
               Result.Append(Ord('&'));
               Result.Append(Ord('a'));
               Result.Append(Ord('m'));
               Result.Append(Ord('p'));
               Result.Append(Ord(';'));
            end;
      else
         Result.Append(Enumerator.Current);
      end;
   Enumerator.Free();
end;
*)

function ForceEscapeAttributeDoubleQuote(constref Value: Rope): TWire;
var
   Enumerator: RopeEnumerator;
begin
   Result.Init();
   Enumerator := RopeEnumerator.Create(@Value);
   while (Enumerator.MoveNext()) do
      case (Enumerator.Current.Value) of
         $0022:
            begin
               Result.Append(Ord('&'));
               Result.Append(Ord('q'));
               Result.Append(Ord('u'));
               Result.Append(Ord('o'));
               Result.Append(Ord('t'));
               Result.Append(Ord(';'));
            end;
         $0026:
            begin
               Result.Append(Ord('&'));
               Result.Append(Ord('a'));
               Result.Append(Ord('m'));
               Result.Append(Ord('p'));
               Result.Append(Ord(';'));
            end;
      else
         Result.Append(Enumerator.Current);
      end;
   Enumerator.Free();
end;

function EscapeAttribute(constref Value: Rope; out NeededQuotes: TQuoteType): UTF8String;
var
   Enumerator: RopeEnumerator;
   Count: Cardinal;
begin
   if (Value.IsEmpty) then
   begin
      Result := '';
      NeededQuotes := qtDouble;
      exit;
   end;
   Count := 0;
   Enumerator := RopeEnumerator.Create(@Value);
   while (Enumerator.MoveNext()) do
   begin
      Inc(Count);
      case (Enumerator.Current.Value) of
         $000A, $0020, $003D, $003C, $003E, $0060, $0026, $0027:
            begin
               Result := ForceEscapeAttributeDoubleQuote(Value).AsString;
               NeededQuotes := qtDouble;
               Enumerator.Free();
               exit;
            end;
      end;
   end;
   Enumerator.Free();
   Result := Value.AsString;
   NeededQuotes := qtNone;
end;

function ForceEscapeForJSON(constref Value: Rope): TWire;
var
   Enumerator: RopeEnumerator;
begin
   Result.Init();
   Enumerator := RopeEnumerator.Create(@Value);
   while (Enumerator.MoveNext()) do
      case (Enumerator.Current.Value) of
         $000A:
            begin
               Result.Append(Ord('\'));
               Result.Append(Ord('n'));
            end;
         $0022:
            begin
               Result.Append(Ord('\'));
               Result.Append(Ord('"'));
            end;
         $005C:
            begin
               Result.Append(Ord('\'));
               Result.Append(Ord('\'));
            end;
         $0026:
            begin
               Result.Append(Ord('&'));
               Result.Append(Ord('a'));
               Result.Append(Ord('m'));
               Result.Append(Ord('p'));
               Result.Append(Ord(';'));
            end;
         $003C:
            begin
               Result.Append(Ord('&'));
               Result.Append(Ord('l'));
               Result.Append(Ord('t'));
               Result.Append(Ord(';'));
            end;
      else
         Result.Append(Enumerator.Current);
      end;
   Enumerator.Free();
end;

function EscapeForJSON(constref Value: Rope): UTF8String;
var
   Enumerator: RopeEnumerator;
begin
   Enumerator := RopeEnumerator.Create(@Value);
   while (Enumerator.MoveNext()) do

      case (Enumerator.Current.Value) of
         $000A, $0022, $005C, $0026, $003C:
            begin
               Result := ForceEscapeForJSON(Value).AsString;
               Enumerator.Free();
               exit;
            end;
      end;
   Enumerator.Free();
   Result := Value.AsString;
end;

function ForceEscapeText(constref Value: Rope): TWire;
var
   Enumerator: RopeEnumerator;
begin
   Result.Init();
   Enumerator := RopeEnumerator.Create(@Value);
   while (Enumerator.MoveNext()) do
      case (Enumerator.Current.Value) of
         $0026:
            begin
               Result.Append(Ord('&'));
               Result.Append(Ord('a'));
               Result.Append(Ord('m'));
               Result.Append(Ord('p'));
               Result.Append(Ord(';'));
            end;
         $003C:
            begin
               Result.Append(Ord('&'));
               Result.Append(Ord('l'));
               Result.Append(Ord('t'));
               Result.Append(Ord(';'));
            end;
    //     $003E:
    //        begin
    //           Result.Append(Ord('&'));
    //           Result.Append(Ord('g'));
    //           Result.Append(Ord('t'));
    //           Result.Append(Ord(';'));
    //        end;
      else
         Result.Append(Enumerator.Current);
      end;
   Enumerator.Free();
end;

function EscapeText(constref Value: Rope): UTF8String;
var
   Enumerator: RopeEnumerator;
begin
   Enumerator := RopeEnumerator.Create(@Value);
   while (Enumerator.MoveNext()) do
      case (Enumerator.Current.Value) of
         $0026, $003C:
            begin
               Result := ForceEscapeText(Value).AsString;
               Enumerator.Free();
               exit;
            end;
      end;
   Enumerator.Free();
   Result := Value.AsString;
end;

procedure Save(const Document: TDocument; const FileName: AnsiString; const InSplit: Boolean = False);
var
   F: Text;
   CurrentElement: TElement;
   CurrentlyInHighlightedElement, StartingNewJSONObject: Boolean;
   CurrentHighlightedElementJSON: UTF8String;
   CurrentHighlightedElementFallbackHTML: UTF8String;

   function AutoclosedBy(const Before: TElement; const After: TNode): Boolean;
   begin
Result := False;
      if (((Before.IsIdentity(nsHTML, eDT) or Before.IsIdentity(nsHTML, eDD)) and
              (After is TElement) and (TElement(After).IsIdentity(nsHTML, eDT) or TElement(After).IsIdentity(nsHTML, eDD))) or
          // ruby-related, select-related, colgroup ...
          (Before.IsIdentity(nsHTML, eP) and (After is TElement) and TElement(After).HasProperties(propAutoclosesP)) or
          (Before.HasProperties(propTableSection) and (After is TElement) and TElement(After).HasProperties(propTableSection)) or
          (Before.HasProperties(propTableCell) and (After is TElement) and TElement(After).HasProperties(propTableCell)) or
          (Before.IsIdentity(nsHTML, eLI) and (After is TElement) and TElement(After).IsIdentity(nsHTML, eLI)) or
          (Before.IsIdentity(nsHTML, eTR) and (After is TElement) and TElement(After).IsIdentity(nsHTML, eTR)) or
          (Before.IsIdentity(nsHTML, eHTML) or Before.IsIdentity(nsHTML, eHead) or Before.IsIdentity(nsHTML, eBody))) then
         Result := True
      else
         Result := False;
   end;

   function SkippableTBodyStartTag(const Element: TElement): Boolean;
   begin
Result := False;
      if (Element.IsIdentity(nsHTML, eTBody)) then
      begin
         if ((not Assigned(Element.FirstChild)) or (not (Element.FirstChild is TElement)) or (not TElement(Element.FirstChild).IsIdentity(nsHTML, eTR))) then
         begin
            Result := False; // empty tbody elements or tbody elements whose first child is not a <tr> can't be omitted
         end
         else
         if (not Assigned(Element.PreviousSibling)) then
         begin
            Result := True; // first node in parent
         end
         else
         if (not (Element.PreviousSibling is TElement)) then
         begin
            Result := True; // previous sibling isn't a <tbody>, <thead>, or <tfoot>
         end
         else
         if (TElement(Element.PreviousSibling).HasProperties(propTableSection)) then
         begin
            Result := False; // previous sibling is a <tbody>, <thead>, or <tfoot>, and we skip their end tags
         end
         else
         begin
            Result := True; // previous sibling isn't a <tbody>, <thead>, or <tfoot>
         end;
      end
      else
         Result := False; // not tbody
   end;

   function AttributeCompare(const A, B: UTF8String): Integer;
   begin
      if A < B then
         Result := -1
      else
      if A > B then
         Result := 1
      else
         Result := 0;
   end;

   function DetermineIsExcluder(const Element: TElement; out AttributeCount: Cardinal): Boolean;
   var
      Variant: TAllVariants;
   begin
      Result := False;
      if (Assigned(Element.Attributes)) then
      begin
         AttributeCount := Element.Attributes.Count;
         for Variant in TAllVariants do
            if (Element.Attributes.Has(kExcludingAttribute[Variant])) then
            begin
               Dec(AttributeCount);
               Result := True;
            end;
         if (Element.Attributes.Has(kCrossRefAttribute) and Element.Attributes[kCrossRefAttribute].IsEmpty) then
            Dec(AttributeCount);
         Result := Result and (AttributeCount = 0) and (Element.IsIdentity(nsHTML, eDiv) or Element.IsIdentity(nsHTML, eSpan));
         if (not Element.Attributes[kCrossRefAttribute].IsEmpty) then
            Dec(AttributeCount);
         if (Element.Attributes.Has(kSubDFNAttribute)) then
            Dec(AttributeCount);
         if (Element.Attributes.Has(kCrossSpecRefAttribute)) then
            Dec(AttributeCount);
         if (Element.Attributes.Has(kUndefinedAttribute)) then
            Dec(AttributeCount);
         if (Element.Attributes.Has(kSplitFilenameAttribute)) then
            Dec(AttributeCount);
         if (Element.Attributes.Has(kSplitFilenameTargetAttribute)) then
            Dec(AttributeCount);
      end
      else
      begin
         AttributeCount := 0;
      end;
   end;

   procedure WriteJSONIfInHighlightedElement(const JSONFragment: UTF8String);
   begin
      if (CurrentlyInHighlightedElement) then
         CurrentHighlightedElementJSON := CurrentHighlightedElementJSON + JSONFragment;
   end;

   procedure WriteHTML(const HTMLFragment: UTF8String);
   begin
      if (CurrentlyInHighlightedElement) then
         CurrentHighlightedElementFallbackHTML := CurrentHighlightedElementFallbackHTML + HTMLFragment
      else
         Write(F, HTMLFragment);
   end;

   procedure InsertWPTTestsBlock(const Element: TElement);
   var
      WPTPaths, WPTOutput: TStrings;
      WPTPath, WPTSubPath, WPTLiveURLScheme, WPTFilename: String;
      WPTPathPrefix: String = '/html/';
   begin
      if ((CurrentVariant = vDev) or (CurrentVariant = vReview)) then
         exit;
      if (Element.HasAttribute('pathprefix')) then
         WPTPathPrefix := Trim(Element.GetAttribute('pathprefix').AsString);
      WPTOutput := TStringList.Create;
      WPTOutput.Add('<div class=wpt-tests-margin>');
      WPTOutput.Add('<input onclick="toggleStatus(this)" value="⋰" type="button">');
      WPTOutput.Add('<dl>');
      WPTPaths := TStringList.Create;
      WPTPaths.Text := Element.TextContent.AsString;
      for WPTSubPath in WPTPaths do
      begin
         WPTLiveURLScheme := 'http';
         if (Trim(WPTSubPath) = '') then
            continue;
         WPTPath := WPTPathPrefix + Trim(WPTSubPath);
         WPTFilename := ExtractFileName(WPTPath);
         if (AnsiContainsStr(WPTFilename, '.https.')
               or AnsiContainsStr(WPTFilename, '.serviceworker.')) then
            WPTLiveURLScheme := 'https';
         WPTOutput.Add('<dt>');
         WPTOutput.Add('<a title="' + WPTFilename + '"'
            + ' href="https://wpt.fyi/results'
            + WPTPath + '">' + WPTFilename + '</a></dt>');
         WPTOutput.Add('<dd><a href="' + WPTLiveURLScheme + '://web-platform-tests.live'
            + WPTPath + '">(live test)</a>');
         WPTOutput.Add(' <a href="https://github.com/web-platform-tests/wpt/blob/master'
            + WPTPath + '">(source)</a></dd>');
      end;
      WPTOutput.Add('</dl>');
      WPTOutput.Add('</div>');
      Write(F, WPTOutput.Text);
   end;

   procedure WalkIn(const Element: TElement);
   var
      IsExcluder, Skip, NotFirstAttribute: Boolean;
      AttributeCount, Index: Cardinal;
      AttributeName, EscapedAttributeName, EscapedAttributeValue: UTF8String;
      Quotes: TQuoteType;
      Variant: TAllVariants;
   begin
      // The following causes the <pre> start tag for any empty pre elements to
      // be dropped. We can end up with empty pre elements because the first
      // pass may drop <code class="idl"> elements from the dev edition, leaving
      // behind the (now-empty) pre parents of the <code class="idl"> elements.
      if Element.IsIdentity(nsHTML, ePre) and (Element.TextContent.AsString = '') then
         exit;
      IsExcluder := DetermineIsExcluder(Element, AttributeCount);
      if ((not IsExcluder) and ((AttributeCount > 0) or (not (Element.HasProperties(propOptionalStartTag) or SkippableTBodyStartTag(Element))))) then
      begin
         if (not StartingNewJSONObject) then
            WriteJSONIfInHighlightedElement(',');
         WriteJSONIfInHighlightedElement('["' + Element.LocalName.AsString + '"');
         WriteHTML('<' + Element.LocalName.AsString);
         NotFirstAttribute := False;
         if (AttributeCount > 0) then
         begin
            WriteJSONIfInHighlightedElement(',{');
            for AttributeName in Element.Attributes do
            begin
               Skip := False;
               for Variant in TAllVariants do
                  if (AttributeName = kExcludingAttribute[Variant]) then
                     Skip := True;
               if (Skip or (AttributeName = kDEVAttribute) or
                           (AttributeName = kCrossRefAttribute) or
                           (AttributeName = kSubDFNAttribute) or
                           (AttributeName = kCrossSpecRefAttribute) or
                           (AttributeName = kUndefinedAttribute) or
                           (AttributeName = kSplitFilenameAttribute) or
                           (AttributeName = kSplitFilenameTargetAttribute)) then
                  continue;
               EscapedAttributeValue := EscapeAttribute(Element.Attributes[AttributeName], Quotes).AsString;
               EscapedAttributeName := AttributeName;
               if (not Element.HasProperties(propHTML)) then
               begin
                  Assert(Length(AttributeName) > 0);
                  for Index := 1 to Length(EscapedAttributeName) do // $R-
                     if (EscapedAttributeName[Index] = ' ') then
                        EscapedAttributeName[Index] := ':';
               end;
               if (NotFirstAttribute) then
                  WriteJSONIfInHighlightedElement(',')
               else
                  NotFirstAttribute := True;
               if (Element.LocalName.AsString = 'pre') and (AttributeName = 'class') then
                  WriteJSONIfInHighlightedElement('"class": "' + EscapedAttributeValue + ' highlight"')
               else
                  WriteJSONIfInHighlightedElement('"' + EscapedAttributeName + '": "' + EscapedAttributeValue + '"');
               case (Quotes) of
                  qtSingle: WriteHTML(' ' + EscapedAttributeName + '=''' + EscapedAttributeValue + '''');
                  qtDouble: WriteHTML(' ' + EscapedAttributeName + '="' + EscapedAttributeValue + '"');
               else WriteHTML(' ' + EscapedAttributeName + '=' + EscapedAttributeValue);
               end;
            end;
            if (Element.LocalName.AsString = 'pre') and (not (Element.HasAttribute('class'))) then
               WriteJSONIfInHighlightedElement(',"class":"highlight"');
            WriteJSONIfInHighlightedElement('}')
         end
         else
            if (Element.LocalName.AsString = 'pre') then
               WriteJSONIfInHighlightedElement(',{"class":"highlight"}')
            else
               WriteJSONIfInHighlightedElement(',{}');
         if (Element.HasProperties(propVoidElement)) then
            WriteJSONIfInHighlightedElement(']');
         WriteHTML('>');
      end;
      CurrentElement := Element;
   end;

   procedure WalkOut(const Element: TElement);
   var
      IsExcluder: Boolean;
      AttributeCount: Cardinal;
      URLEncodedJSONContents: String;
      HighlighterOutput: String;
      ClassValue: String = '';
      HTTPClient: TFPHTTPClient;
      Ss: TStringStream;
   begin
      // The following causes the </pre> end tag for any empty pre elements to
      // be dropped. We can end up with empty pre elements because the first
      // pass may drop <code class="idl"> elements from the dev edition, leaving
      // behind the (now-empty) pre parents of the <code class="idl"> elements.
      if Element.IsIdentity(nsHTML, ePre) and (Element.TextContent.AsString = '') then
         exit;
      if (Element.LocalName.AsString = 'wpt') then
         exit;
      if (InSplit and Element.HasAttribute(kExcludingAttribute[vSplit])) then
         exit;
      IsExcluder := DetermineIsExcluder(Element, AttributeCount);
      if (not (IsExcluder or Element.HasProperties(propVoidElement) or
               (Element.HasProperties(propOptionalEndTag) and ((not Assigned(Element.NextSibling)) or (AutoclosedBy(Element, Element.NextSibling)))))) then
      begin
         WriteJSONIfInHighlightedElement(']');
         WriteHTML('</' + Element.LocalName.AsString + '>');
         if (Element.HasAttribute('class')) then
            ClassValue := Element.GetAttribute('class').AsString;
         if (IsHighlighterTarget(Element, ClassValue)) then
         begin
            URLEncodedJSONContents := EncodeURLElement(CurrentHighlightedElementJSON);
            try
               if (HighlighterOutputByJSONContents.Has(CurrentHighlightedElementJSON)) then
                  Write(F, HighlighterOutputByJSONContents[CurrentHighlightedElementJSON])
               else
               begin
                  try
                     HTTPClient := TFPHTTPClient.Create(nil);
                     Ss := TStringStream.Create('');
                     if (AnsiContainsStr(ClassValue, 'idl')) then
                        HTTPClient.HTTPMethod('GET', HighlightServerURL + '/webidl?' + URLEncodedJSONContents, Ss, [200,400])
                     else
                     if (AnsiContainsStr(ClassValue, 'css')) then
                        HTTPClient.HTTPMethod('GET', HighlightServerURL + '/css?' + URLEncodedJSONContents, Ss, [200,400])
                     else
                     if (AnsiContainsStr(ClassValue, 'js')) then
                        HTTPClient.HTTPMethod('GET', HighlightServerURL + '/js?' + URLEncodedJSONContents, Ss, [200,400])
                     else
                     if (AnsiContainsStr(ClassValue, 'abnf')) then
                        HTTPClient.HTTPMethod('GET', HighlightServerURL + '/abnf?' + URLEncodedJSONContents, Ss, [200,400])
                     else
                     if (AnsiContainsStr(ClassValue, 'html')) then
                        HTTPClient.HTTPMethod('GET', HighlightServerURL + '/html?' + URLEncodedJSONContents, Ss, [200,400]);
                     HighlighterOutput := Ss.Datastring;
                     Ss.Free;
                     if HTTPClient.ResponseStatusCode = 400 then
                     begin
                        Write(Trim(HighlighterOutput));
                        Halt(1);
                     end;
                     HighlighterOutputByJSONContents[CurrentHighlightedElementJSON] := HighlighterOutput;
                     Write(F, Trim(HighlighterOutput));
                  finally
                     HTTPClient.Free;
                  end;
               end;
            except
              on E: EHTTPClient do
                  Write(F, CurrentHighlightedElementFallbackHTML);
            end;
            CurrentHighlightedElementJSON := '';
            CurrentHighlightedElementFallbackHTML := '';
            CurrentlyInHighlightedElement := False;
         end;
      end;
      if (Element.ParentNode is TElement) then
         CurrentElement := TElement(Element.ParentNode)
      else
         CurrentElement := nil;
   end;

var
   Current: TNode;
   ClassValue: String = '';
   Element: TElement;
begin
   Assign(F, FileName);
   Rewrite(F);
   Write(F, '<!DOCTYPE html>');
   Current := Document.GetDocumentElement();
   CurrentElement := nil;
   CurrentlyInHighlightedElement := False;
   CurrentHighlightedElementJSON := '';
   CurrentHighlightedElementFallbackHTML := '';

   if (HighlightServerURL <> '') then
      Inform('Highlighting and saving ' + ExtractFileName(FileName))
   else
      Inform('Saving ' + ExtractFileName(FileName));

   repeat
      Assert(Assigned(Current));
      StartingNewJSONObject := False;
      if (Current is TElement) then
      begin
         Element := TElement(Current);
         if (Element.LocalName.AsString = 'wpt') then
         begin
            InsertWPTTestsBlock(Element);
            WalkToNextSkippingChildren(Current, Document, @WalkOut);
            continue;
         end;
         if (Element.HasAttribute('class')) then
            ClassValue := Element.GetAttribute('class').AsString;
         if (IsHighlighterTarget(Element, ClassValue)) then
         begin
            CurrentlyInHighlightedElement := True;
            StartingNewJSONObject := True;
         end;
         if (InSplit and TElement(Current).HasAttribute(kExcludingAttribute[vSplit])) then
         begin
            WalkToNextSkippingChildren(Current, Document, @WalkOut);
            continue;
         end
         else
            WalkIn(TElement(Current));
      end
      else
      if (Current is TText) then
      begin
         Assert(Assigned(CurrentElement));
         WriteJSONIfInHighlightedElement(',"');
         if (CurrentElement.HasProperties(propRawTextElement)) then
         begin
            WriteJSONIfInHighlightedElement(EscapeForJSON(TText(Current).Data).AsString);
            WriteHTML(TText(Current).Data.AsString);
         end
         else
         begin
            WriteJSONIfInHighlightedElement(EscapeForJSON(TText(Current).Data).AsString);
            WriteHTML(EscapeText(TText(Current).Data).AsString);
         end;
         WriteJSONIfInHighlightedElement('"');
      end;
      if (not WalkToNext(Current, Document, @WalkOut)) then
         break;
   until False;
   Writeln(F);
   Close(F);
end;

function Split(const Document: TDocument; var BigTOC: TElement; const Base: AnsiString): Boolean; // True = success, False = failed

   procedure SaveFragmentLinks(const Data: Rope);
   var
      FragmentLinksFile: Text;
   begin
      // fragment-links.json
      Assign(FragmentLinksFile, Base + 'fragment-links.json');
      Rewrite(FragmentLinksFile);
      Write(FragmentLinksFile, '{');
      Write(FragmentLinksFile, Data.AsString);
      Write(FragmentLinksFile, '}');
      Close(FragmentLinksFile);
   end;

   procedure Fail(const Message: UTF8String);
   begin
      Writeln('Error: ', Message);
      Result := False;
   end;

   procedure Trim(const NewTOC: TElement; const SectionName: UTF8String);

      procedure TrimOut(const Candidate: TElement);
      begin
         if (not Candidate.HasChildNodes()) then
         begin
            Candidate.Remove();
            Candidate.Free();
         end;
      end;

   var
      Current, Victim: TNode;
   begin
      Current := NewTOC;
      repeat
         if ((Current is TElement) and
             (TElement(Current).IsIdentity(nsHTML, eA)) and
             (TElement(Current).GetAttribute(kSplitFilenameTargetAttribute).AsString <> SectionName)) then
            while (TElement(Current).HasChildNodes()) do
            begin
               Victim := TElement(Current).FirstChild;
               Victim.Remove();
               Victim.Free();
            end;
      until not WalkToNext(Current, NewTOC, @TrimOut);
   end;

type
   TElementUtils = specialize DefaultUnorderedUtils <TElement>;
   TStringSet = specialize THashSet <UTF8String, UTF8StringUtils>;
const
   kFileNameAttribute = 'filename';
   kHeadingAttribute = 'heading';
   kTableOfContents = 'Table of Contents';
   kIndexFilename = 'index.html';
var
   SectionDoc: TDocument;
   SectionName, ID: UTF8String;
   FirstChild, Body, Sections, CurrentSection, Link, LinkHome, Target, TargetHome, NewTOC: TElement;
   Next, Current: TNode;
   Adjust: Boolean;
   Targets: TElementMap;
   TargetIndex: Integer;
   Links: specialize PlasticArray <TElement, TElementUtils>;
   Scratch, FragmentLinks: Rope;
   ExtractedData: CutRope;
   Enumerator: RopeEnumerator;
   SectionNames: TStringSet;
begin
   Result := True;
   Sections := TElement.Create(nsNone, eList);
   SectionNames := TStringSet.Create(@UTF8StringHash32);
   SectionNames.Add('index');
   Link := ConstructHTMLElement(eScript);
   Link.SetAttribute('src', '/link-fixup.js');
   Link.SetAttribute('defer', '');
   FirstChild := Document.GetDocumentElement().FirstChild as TElement;
   Document.GetDocumentElement().InsertBefore(Link, FirstChild);
   // find body
   Current := Document;
   Inform('Splitting...');
   repeat
      WalkToNext(Current, Document, nil)
   until (Current is TElement) and TElement(Current).IsIdentity(nsHTML, eBody);
   Assert(Assigned(Current));
   Assert(Current <> Document);
   Body := Current as TElement;
   // find links and targets
   Targets := TElementMap.Create(@UTF8StringHash32);
   Links.Init();
   Current := Document;
   repeat
      if (Current is TElement) then
      begin
         if (TElement(Current).IsIdentity(nsHTML, eA)) then
            Links.Push(TElement(Current));
         if (TElement(Current).HasAttribute('id')) then
         begin
            Assert(not TElement(Current).GetAttribute('id').IsEmpty);
            Assert(TElement(Current).GetAttribute('id').AsString <> '');
            Targets[TElement(Current).GetAttribute('id').AsString] := TElement(Current);
         end;
      end;
   until not WalkToNext(Current, Document, nil);
   // extract out sections
   CurrentSection := nil;
   Next := Body.FirstChild;
   while (Assigned(Next)) do
   begin
      Current := Next;
      Next := Next.NextSibling;
      Assert(Assigned(Current));
      if ((Current is TElement) and TElement(Current).HasAttribute(kSplitFilenameAttribute)) then
      begin
         ExtractedData := TElement(Current).GetAttribute(kSplitFilenameAttribute);
         SectionName := ExtractedData.AsString;
         if (SectionName = 'index') then
            Fail('split-filename "index" specified (that filename is used for the main page)')
         else
         if (SectionNames.Has(SectionName)) then
            Fail('duplicate split-filename specified: ' + SectionName);
         SectionNames.Add(SectionName);
         CurrentSection := TElement.Create(nsNone, eChapter);
         Scratch := Default(Rope);
         Scratch.AppendDestructively(ExtractedData);
         CurrentSection.SetAttributeDestructively(kFileNameAttribute, Scratch);
         Scratch := TElement(Current).TextContent;
         CurrentSection.SetAttributeDestructively(kHeadingAttribute, Scratch);
         Sections.AppendChild(CurrentSection);
      end;
      if (Assigned(CurrentSection)) then
      begin
         Current.Remove();
         CurrentSection.AppendChild(Current);
      end;
   end;
   // update all the hyperlinks
   FragmentLinks := Default(Rope);
   TargetIndex := 0;
   for ID in Targets do
   begin
      Assert(Length(ID) > 0);
      // emit "," only if ID is not first one to make FragmentLinks JSON compatible.
      if (TargetIndex > 0) then
      begin
         FragmentLinks.Append($002C);
      end;
      FragmentLinks.Append($0022);
      FragmentLinks.Append(@ID[1], Length(ID)); // $R-
      FragmentLinks.Append($0022);
      FragmentLinks.Append($003A);
      FragmentLinks.Append($0022);
      LinkHome := Targets[ID];
      repeat
         LinkHome := LinkHome.ParentNode as TElement;
      until (not (LinkHome.ParentNode is TElement)) or LinkHome.IsIdentity(nsNone, eChapter);
      ExtractedData := LinkHome.GetAttribute(kFileNameAttribute);
      if (not ExtractedData.IsEmpty) then
         FragmentLinks.AppendDestructively(ExtractedData);
      // else the empty string
      FragmentLinks.Append($0022);
      Inc(TargetIndex);
   end;
   // save fragment-links.json
   SaveFragmentLinks(FragmentLinks);
   for Link in Links do
   begin
      if (Link.HasAttribute('href')) then
      begin
         Scratch := Link.Attributes['href'];
         Enumerator := Scratch.GetEnumerator();
         Adjust := (Enumerator.MoveNext()) and (Enumerator.Current = $0023);
         if (Adjust) then
         begin
            Enumerator.MoveNext();
            ID := Scratch.ExtractToEnd(Enumerator.GetPointer()).AsString;
         end;
         Enumerator.Free();
         if (Adjust and Targets.Has(ID)) then
         begin
            LinkHome := Link;
            repeat
               LinkHome := LinkHome.ParentNode as TElement;
            until (not (LinkHome.ParentNode is TElement)) or LinkHome.IsIdentity(nsNone, eChapter);
            Target := Targets[ID];
            TargetHome := Target;
            Assert(TargetHome.GetAttribute('id').AsString = ID, TargetHome.GetAttribute('id').AsString + ' -vs- ' + ID);
            repeat
               Assert(Assigned(TargetHome.ParentNode));
               Assert(TargetHome.ParentNode is TElement, TargetHome.ParentNode.ClassName + ' reached from ' + Target.ClassName);
               TargetHome := TargetHome.ParentNode as TElement;
            until (not (TargetHome.ParentNode is TElement)) or TargetHome.IsIdentity(nsNone, eChapter);
            if (LinkHome <> TargetHome) then
            begin
               Scratch := Default(Rope);
               ExtractedData := TargetHome.GetAttribute(kFileNameAttribute);
               Scratch.AppendDestructively(ExtractedData);
               Scratch.Append('.html');
               ExtractedData := Link.GetAttribute('href');
               Scratch.AppendDestructively(ExtractedData);
               Link.SetAttributeDestructively('href', Scratch);
               ExtractedData := TargetHome.GetAttribute(kFileNameAttribute);
               Link.SetAttributeDestructively(kSplitFilenameTargetAttribute, ExtractedData);
            end;
         end;
      end;
   end;
   // save table of contents section
   SectionDoc := Document.CloneNode(True);
   SectionDoc.GetDocumentElement().SetAttribute('class', 'split index');
   Save(SectionDoc, Base + kIndexFilename);
   SectionDoc.Free();
   BigTOC.Remove();
   // save sections
   CurrentSection := Sections.FirstChild as TElement;
   while (Assigned(CurrentSection)) do
   begin
      SectionName := CurrentSection.GetAttribute(kFileNameAttribute).AsString;
      SectionDoc := Document.CloneNode(True);
      SectionDoc.GetDocumentElement().SetAttribute('class', 'split');
      // find body
      Current := SectionDoc;
      repeat
         WalkToNext(Current, SectionDoc, nil);
      until (Current is TElement) and TElement(Current).IsIdentity(nsHTML, eBody);
      Assert(Assigned(Current));
      Assert(Current <> Document);
      Body := Current as TElement;
      // insert next/previous links
      LinkHome := ConstructHTMLElement(eNav);
      if (Assigned(CurrentSection.PreviousSibling)) then
      begin
         Link := ConstructHTMLElement(eA);
         Scratch := Default(Rope);
         Scratch.Append($2190);
         Scratch.Append($0020);
         Link.AppendChild(TText.CreateDestructively(Scratch));
         ExtractedData := TElement(CurrentSection.PreviousSibling).GetAttribute(kHeadingAttribute);
         Link.AppendChild(TText.CreateDestructively(ExtractedData));
         ExtractedData := TElement(CurrentSection.PreviousSibling).GetAttribute(kFilenameAttribute);
         Scratch := Default(Rope);
         Scratch.AppendDestructively(ExtractedData);
         Scratch.Append('.html');
         Link.SetAttributeDestructively('href', Scratch);
         LinkHome.AppendChild(Link);
         Scratch := Default(Rope);
         Scratch.Append($0020);
         Scratch.Append($2014);
         Scratch.Append($0020);
         LinkHome.AppendChild(TText.CreateDestructively(Scratch));
      end;
      Link := ConstructHTMLElement(eA);
      Scratch := Default(Rope);
      Scratch.Append(@kTableOfContents[1], Length(kTableOfContents));
      Link.AppendChild(TText.CreateDestructively(Scratch));
      Scratch := Default(Rope);
      Scratch.Append('./'); // The shorter URL is the canonical one; no kIndexFilename here.
      Link.SetAttributeDestructively('href', Scratch);
      LinkHome.AppendChild(Link);
      if (Assigned(CurrentSection.NextSibling)) then
      begin
         Scratch := Default(Rope);
         Scratch.Append($0020);
         Scratch.Append($2014);
         Scratch.Append($0020);
         LinkHome.AppendChild(TText.CreateDestructively(Scratch));
         Link := ConstructHTMLElement(eA);
         ExtractedData := TElement(CurrentSection.NextSibling).GetAttribute(kHeadingAttribute);
         Link.AppendChild(TText.CreateDestructively(ExtractedData));
         ExtractedData := TElement(CurrentSection.NextSibling).GetAttribute(kFilenameAttribute);
         Scratch := Default(Rope);
         Scratch.AppendDestructively(ExtractedData);
         Scratch.Append('.html');
         Link.SetAttributeDestructively('href', Scratch);
         Scratch := Default(Rope);
         Scratch.Append($0020);
         Scratch.Append($2192);
         Link.AppendChild(TText.CreateDestructively(Scratch));
         LinkHome.AppendChild(Link);
      end;
      Body.AppendChild(LinkHome);
      // insert ToC
      if (Assigned(BigTOC)) then
      begin
         // insert trimmed NewTOC
         NewTOC := BigTOC.CloneNode(True);
         Trim(NewTOC, SectionName);
         Body.AppendChild(NewTOC);
      end;
      // insert section content
      while (Assigned(CurrentSection.FirstChild)) do
      begin
         Next := CurrentSection.FirstChild;
         Next.Remove();
         Body.AppendChild(Next);
      end;
      // insert another link/previous link pair
      Body.AppendChild(LinkHome.CloneNode(True));
      // save output
      Save(SectionDoc, Base + SectionName + '.html', True);
      SectionDoc.Free();
      CurrentSection := CurrentSection.NextSibling as TElement;
   end;
   FreeAndNil(BigTOC);
   Targets.Free();
   Sections.Free();
   SectionNames.Free();
end;

function Main(): Boolean;
const
   OtherVariants = [Low(TVariants)..High(TVariants)] - [Low(TVariants)];
var
   SourceFile: AnsiString;
   SourceGitSHA: AnsiString;
   BuildType: AnsiString;
   MDNJSONFilename: AnsiString;
   Source: TFileData;
   Parser: THTMLParser;
   BigTOC: TElement;
   Documents: array[TVariants] of TDocument;
   {$IFDEF TIMINGS} StartTime: TDateTime; {$ENDIF}
   Variant: TAllVariants;
   i: integer;
begin
   Result := False;
   if ((ParamCount() = 1) and (ParamStr(1) = '--version')) then
   begin
      Writeln('wattsi ' + IntToStr(Version));
      exit;
   end;
   for i := 1 to ParamCount() do
   begin
      if (ParamStr(i) = '--quiet') then
      begin
         Quiet := true;
         continue;
      end
      else
      if (ParamStr(i) = '--single-page-only') then
      begin
         SinglePageOnly := true;
         continue;
      end
      else
      begin
         if ((ParamCount() - i) < 4) then
         begin
            Writeln('wattsi: invalid arguments');
            Writeln('syntax:');
            Writeln('  wattsi [--quiet] [--single-page-only] <source-file> <source-git-sha> <output-directory> <default-or-review> <mdn-spec-links/html.json> [<highlight-server-url>]');
            Writeln('  wattsi --version');
            exit;
         end;
         SourceFile := ParamStr(i);
         SourceGitSHA := ParamStr(i + 1);
         OutputDirectory := ParamStr(i + 2);
         BuildType := ParamStr(i + 3);
         MDNJSONFilename := ParamStr(i + 4);
         if (ParamCount() = (i + 5)) then
            HighlightServerURL := ParamStr(i + 5);
         break;
      end;
   end;

   if (not IsEmptyDirectory(OutputDirectory)) then
   begin
      // only act if, when we start, the output directory is empty, to make sure that the
      // caller is indeed expecting us to put the data there
      Writeln('wattsi: output directory (second argument) must be an existing empty directory');
      exit;
   end;
   Inform('Parsing MDN data...');
   MDNJSONData := ParseJSON(ReadTextFile(MDNJSONFilename));
   MDNBrowsers := TMDNBrowsers.Create;
   // The browser IDs here must match the ones in the imported JSON data.
   // See the list of browser IDs at https://goo.gl/iDacWP.
   MDNBrowsers['chrome'] := 'Chrome';
   MDNBrowsers['chrome_android'] := 'Chrome Android';
   MDNBrowsers['edge_blink'] := 'Edge';
   MDNBrowsers['edge'] := 'Edge (Legacy)';
   MDNBrowsers['firefox'] := 'Firefox';
   MDNBrowsers['firefox_android'] := 'Firefox Android';
   MDNBrowsers['ie'] := 'Internet Explorer';
   // MDNBrowsers['nodejs'] := 'Node.js'; // no data for features in HTML
   MDNBrowsers['opera'] := 'Opera';
   MDNBrowsers['opera_android'] := 'Opera Android';
   // MDNBrowsers['qq_android'] := 'QQ Browser'; // not enough data for features in HTML
   MDNBrowsers['safari'] := 'Safari';
   MDNBrowsers['safari_ios'] := 'Safari iOS';
   MDNBrowsers['samsunginternet_android'] := 'Samsung Internet';
   // MDNBrowsers['uc_android'] := 'UC Browser'; // not enough data for features in HTML
   // MDNBrowsers['uc_chinese_android'] := 'Chinese UC Browser'; // not enough data for features in HTML
   MDNBrowsers['webview_android'] := 'WebView Android';
   nsNone := Intern('');
   eList := Intern('list');
   eChapter := Intern('chapter');
   RegisterHTMLElement('ref', eRef, THTMLElement, 0);
   Inform('Parsing...');
   {$IFDEF TIMINGS} StartTime := Now(); {$ENDIF}
   Source := ReadFile(SourceFile);
   try
      Parser := THTMLParser.Create();
      Parser.RegisterProperietaryVoidElements([eRef]);
      try
         try
            Parser.SpoonFeed(Source.Start, Source.Length);
            Documents[Low(TVariants)] := Parser.Parse();
         except
            on E: ESyntaxError do
            begin
               Writeln('Parse Error: ', E.Message);
               exit;
            end
            else
            begin
               ReportCurrentException();
               exit;
            end;
         end;
      finally
         Parser.Free();
      end;
      {$IFDEF TIMINGS} Writeln('Elapsed time: ', MillisecondsBetween(StartTime, Now()), 'ms'); {$ENDIF}
      if (BuildType = 'default') then
      begin
         {$IFDEF TIMINGS} Writeln('Cloning...'); {$ENDIF}
         {$IFDEF TIMINGS} StartTime := Now(); {$ENDIF}
         for Variant in OtherVariants do
            Documents[Variant] := Documents[Low(TVariants)].CloneNode(True);
         {$IFDEF TIMINGS} Writeln('Elapsed time: ', MillisecondsBetween(StartTime, Now()), 'ms'); {$ENDIF}
      end;
      try
         try
            // gen...
            if (BuildType = 'default') then
            begin
               for Variant in TVariants do
               begin
                  if ((SinglePageOnly) and (Variant <> vHTML)) then
                     exit;
                  CurrentVariant := Variant;
                  if (Variant = vReview) then
                  begin
                     continue;
                  end;
                  // Create this directory early as ProcessDocument relies on it to store
                  // /multipage-dev/search-index.json
                  if (Variant <> vSnap) then
                  begin
                     MkDir(OutputDirectory + '/multipage-' + kSuffixes[Variant]);
                  end;
                  Inform('Generating ' + Uppercase(kSuffixes[Variant]) + ' variant...');
                  {$IFDEF TIMINGS} StartTime := Now(); {$ENDIF}
                  ProcessDocument(Documents[Variant], Variant, BigTOC, SourceGitSHA); // $R-
                  {$IFDEF TIMINGS} Writeln('Elapsed time: ', MillisecondsBetween(StartTime, Now()), 'ms'); {$ENDIF}
                  // output...
                  if (Variant <> vDEV) then
                  begin
                     {$IFDEF TIMINGS} Writeln('Saving single-page version...'); {$ENDIF}
                     {$IFDEF TIMINGS} StartTime := Now(); {$ENDIF}
                     Save(Documents[Variant], OutputDirectory + '/index-' + kSuffixes[Variant]);
                     {$IFDEF TIMINGS} Writeln('Elapsed time: ', MillisecondsBetween(StartTime, Now()), 'ms'); {$ENDIF}
                  end;
                  // multipage...
                  if (Variant <> vSnap) then
                  begin
                     if (SinglePageOnly) then
                        exit;
                     {$IFDEF TIMINGS} Writeln('Splitting spec...'); {$ENDIF}
                     {$IFDEF TIMINGS} StartTime := Now(); {$ENDIF}
                     if (not Split(Documents[Variant], BigTOC, OutputDirectory + '/multipage-' + kSuffixes[Variant] + '/')) then
                        raise EAbort.Create('Could not split specification');
                     {$IFDEF TIMINGS} Writeln('Elapsed time: ', MillisecondsBetween(StartTime, Now()), 'ms'); {$ENDIF}
                  end;
               end;
            end
            else
            begin
               Assert(BuildType = 'review');
               // Skip timing information here as it should be roughly equivalent
               Inform('Generating ' + Uppercase(kSuffixes[vReview]) + ' exclusively...');
               ProcessDocument(Documents[Low(TVariants)], vReview, BigTOC, SourceGitSHA);
               Save(Documents[Low(TVariants)], OutputDirectory + '/index-' + kSuffixes[vReview]);
            end;
            Result := True;
         except
            on E: EAbort do
               Writeln('Aborting.');
            else
            begin
               ReportCurrentException();
            end;
         end;
      finally
         try
            if (BuildType = 'default') then
            begin
               for Variant in TVariants do
                  Documents[Variant].Free();
            end
            else
            begin
               Documents[Low(TVariants)].Free();
            end;
         except
            ReportCurrentException();
         end;
      end;
   finally
      Source.Destroy();
   end;
end;

begin
   Main();
end.
