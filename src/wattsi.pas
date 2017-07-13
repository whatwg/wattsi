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
   dom, webdom, htmlparser, json;

var
   Quiet: Boolean = false;
   Version: Word = (*$I version.inc *); // unsigned integer from 0 .. 65535
   OutputDirectory: AnsiString;

type
   TAllVariants = (vHTML, vDEV, vSplit);
   TVariants = vHTML..vDEV;

const
   kSuffixes: array[TVariants] of UTF8String = ('html', 'dev');
   kExcludingAttribute: array[TAllVariants] of UTF8String = ('w-nohtml', 'w-nodev', 'w-nosplit');
   kCrossRefAttribute = 'data-x';
   kCrossSpecRefAttribute = 'data-x-href';
   kCrossRefInternalLinkAttribute = 'data-x-internal';
   kSubDFNAttribute = 'subdfn';
   kUndefinedAttribute = 'undefined';
   kSplitFilenameAttribute = 'split-filename';
   kSplitFilenameTargetAttribute = 'split-filename-target';
   kLTAttribute = 'lt';
   kHrefAttribute = 'href';
   kNonNormative = 'This section is non-normative.';
   kEllipsis = #$22F0;
   Months: array[1..12] of UTF8String = ('January', 'February', 'March', 'April',
                                         'May', 'June', 'July', 'August', 'September',
                                         'October', 'November', 'December');
   InterestingBrowserCount = 12;

type
   TBrowser = record
      Code, Name: UTF8String;
      TotalUsage: Double;
      Versions: array of UTF8String;
   end;
   TBrowserIndex = 1..InterestingBrowserCount;
   TBug = record
      ID, URL, Summary: UTF8String;
   end;
   TImplState = (sYes, sAlmost, sNo, sRemoved, sPolyfill, sUnknown, sPrefix, sDisabled, sNotes);
   TImplGoodState = sYes..sRemoved;
   TVersionedState = record
      State: TImplGoodState;
      Version: UTF8String;
      LastVersion: UTF8String;
   end;
   TFeature = record
      CanIUseCode: UTF8String;
      FirstGoodVersion: array[TBrowserIndex] of TVersionedState;
      Bugs: array of TBug;
      procedure Reset();
   end;

type
   TElementMap = specialize THashTable <UTF8String, TElement, UTF8StringUtils>;
   TFeatureMap = specialize THashTable <UTF8String, TFeature, UTF8StringUtils>;

procedure TFeature.Reset();
var
   BrowserIndex: TBrowserIndex;
begin
   CanIUseCode := '';
   for BrowserIndex in TBrowserIndex do
   begin
      FirstGoodVersion[BrowserIndex].State := sNo;
      FirstGoodVersion[BrowserIndex].Version := '';
   end;
   SetLength(Bugs, 0);
end;

var
   nsNone: TCanonicalString;
   eRef, eList, eChapter: TCanonicalString;
   Browsers: array[TBrowserIndex] of TBrowser;
   Features: TFeatureMap;

procedure ProcessDocument(const Document: TDocument; const Variant: TVariants; out BigTOC: TElement);
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
   SmallTOC: TElement;
   SmallTOCBookmark, BigTOCBookmark: TNode;
   LastHeadingRank: THeadingRank;
   LastTOCOL, LastTOCLI: TElement;
   CurrentSectionNumber: array[THeadingRank] of Cardinal;
   StringStore: TStringStore;
   Errors: Cardinal;
   XrefsByDFNAnchor: TXrefsByDFNAnchor;

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
            if (TElement(Current).HasAttribute(kExcludingAttribute[Variant])) then
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
            // they need an ID even if they are not referenced. Additionally, the linkage to the
            // caniuse annotations depends on the ID existing.
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

   function ProcessNode(var Node: TNode): Boolean; // return True if we are to keep this node, False if we drop it
   var
      CandidateChild, SelectedForTransfer: TNode;
      CurrentHeadingRank: THeadingRank;
      Element, HeadingSelfLink, NewLI, SecondLI, NewLink, NewP, NewI, NewSpan, TempElement: TElement;
      Scratch, ImageSrc: Rope;
      ExtractedData: CutRope;
      ClassName, Instruction, CrossReferenceName, Revision, ReferenceName: UTF8String;
      Index: Cardinal;
      TodayYear, TodayMonth, TodayDay: Word;
      InSkippedNode, Matching, UsedLI: Boolean;
      Enumerator: RopeEnumerator;
      ListNode: PElementListNode;
      DFNEntry: TDFNEntry;
   begin
      Result := True;
      if (Node is TElement) then
      begin
         Element := TElement(Node);
         if (Element.HasProperties(propHeading)) then
         begin
            ClassName := Element.GetAttribute('class').AsString;
            CurrentHeadingRank := (Element as THTMLHeadingElement).Rank;
            if (CurrentHeadingRank > 1) then
            begin
               InHeading := Element;
               if (Element.HasAttribute('split-filename')) then
                  SplitFilename := Element.GetAttribute('split-filename').AsString;
                  SplitFilenameClassName := Element.GetAttribute('class').AsString;
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
                     NewSpan := ConstructHTMLElement(eSpan);
                     NewSpan.SetAttribute('class', 'secno');
                     NewSpan.appendChild(TText.CreateDestructively(Scratch));
                     Element.InsertBefore(TText.Create(#$0020), Element.FirstChild);
                     Element.InsertBefore(NewSpan, Element.FirstChild);
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
                           if (InSkippedNode) then
                              Fail('Nested <dfn> or <span> elements in heading ' + LastSeenHeadingID);
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
         if ((Element.IsIdentity(nsHTML, eSpan)) and (Element.GetAttribute('class').AsString = 'pubdate')) then
         begin
            if ((not Element.HasChildNodes()) or (not (Element.FirstChild is TText))) then
            begin
               Fail('pubdate span must contain exactly one text node');
            end
            else
            begin
               Scratch := Default(Rope);
               DecodeDate(Date, TodayYear, TodayMonth, TodayDay);
               Scratch.Append(IntToStr(TodayDay));
               Scratch.Append($0020);
               Scratch.Append(@Months[TodayMonth][1], Length(Months[TodayMonth])); // $R-
               Scratch.Append($0020);
               Scratch.Append(IntToStr(TodayYear));
               TText(Element.FirstChild).Data := Scratch;
            end;
         end
         else
         if (Element.IsIdentity(nsHTML, eDFN)) then
         begin
            if (Element.HasAttribute(kLTAttribute)) then
               Fail('<dfn> with lt="" found, use data-x="" instead; dfn is ' + Describe(Element));
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
         if (Element.IsIdentity(nsHTML, ePre) and (Element.GetAttribute('class').AsString = 'idl') and (Variant = vDEV)) then
         begin
            Result := False;
         end
         else
         if (Element.IsIdentity(nsHTML, eSpan)) then
         begin
            if (Element.HasAttribute(kLTAttribute)) then
               Fail('<span> with lt="" found, use data-x="" instead; span is ' + Describe(Element));
            if (Assigned(InDFN)) then
               Fail('<span> inside <dfn>; span is ' + Describe(Element))
            else
               SaveCrossReference(Element);
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
                     Warn('Unused reference: [' + LastSeenReferenceName + ']');
                  Result := False;
               end
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
         if ((Instruction = 'INSERT FINGERPRINT') and (Variant <> vDEV)) then
         begin
            TempElement := E(eSpan, [kCrossRefAttribute, 'fingerprinting vector',
                                     'title', 'There is a potential fingerprinting vector here.',
                                     'class', 'fingerprint'],
                                    [E(eImg, ['src', '/images/fingerprint.png',
                                              'alt', '(This is a fingerprinting vector.)',
                                              'width', '46',
                                              'height', '64'])]);
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

   procedure InsertAnnotations();
   var
      ID: UTF8String;
      Feature: TFeature;
      Bug: TBug;
      Container: TElement;
      Context, Ancestor: TNode;
      Status, P: TElement;
      First, Found: Boolean;
      BrowserIndex: TBrowserIndex;
   begin
{$IFDEF VERBOSE_ANNOTATIONS} Writeln('START OF ANNOTATIONS'); {$ENDIF}
      for ID in Features do
      begin
{$IFDEF VERBOSE_ANNOTATIONS} Writeln('  considering ', ID); {$ENDIF}
         Feature := Features[ID];
         if (IDs.Has(ID)) then
         begin
{$IFDEF VERBOSE_ANNOTATIONS} Writeln('    found'); {$ENDIF}
            Container := IDs[ID];
{$IFDEF VERBOSE_ANNOTATIONS} Writeln('      Container = @', PtrUInt(Container)); {$ENDIF}
{$IFDEF VERBOSE_ANNOTATIONS} Writeln('      Container.ParentNode = @', PtrUInt(Container.ParentNode)); {$ENDIF}
            Context := nil;
            // if you get a crash here, check if there's a place where
            // we Replace the original element for this ID with a new
            // element somehow
            while not Container.IsIdentity(nsHTML, eDiv) and
                  not Container.IsIdentity(nsHTML, eTD) and
                  not Container.IsIdentity(nsHTML, eDD) and
                  not Container.IsIdentity(nsHTML, eLI) and
                  not Container.IsIdentity(nsHTML, eBody) do
            begin
{$IFDEF VERBOSE_ANNOTATIONS} Writeln('      Moving up one...'); {$ENDIF}
{$IFDEF VERBOSE_ANNOTATIONS} Writeln('        Container.ParentNode = @', PtrUInt(Container.ParentNode), ' (unchanged)'); {$ENDIF}
               Context := Container;
{$IFDEF VERBOSE_ANNOTATIONS} Writeln('        Context = @', PtrUInt(Context), ' (old Container)'); {$ENDIF}
{$IFDEF VERBOSE_ANNOTATIONS} Writeln('        Container.ParentNode = @', PtrUInt(Container.ParentNode)); {$ENDIF}
{$IFDEF VERBOSE_ANNOTATIONS} Writeln('        = Container := Container.ParentNode ='); {$ENDIF}
               Container := Container.ParentNode as TElement;
{$IFDEF VERBOSE_ANNOTATIONS} Writeln('        New Container = @', PtrUInt(Container)); {$ENDIF}
            end;
{$IFDEF VERBOSE_ANNOTATIONS} Writeln('    examining ancestors'); {$ENDIF}
            Found := False;
            Ancestor := Container;
            while (Assigned(Ancestor)) do
            begin
               if ((Ancestor = BigTOC) or (Ancestor = SmallTOC)) then
               begin
                  Warn('Found ID ' + ID + ' in a table of contents for annotation that uses URLs:');
                  for Bug in Feature.Bugs do
                     Writeln('   ', Bug.URL);
                  if (Feature.CanIUseCode <> '') then
                     Writeln('   https://caniuse.com/#feat=', Feature.CanIUseCode);
                  Found := True;
               end;
               Ancestor := Ancestor.ParentNode;
            end;
{$IFDEF VERBOSE_ANNOTATIONS} Writeln('    found=', Found); {$ENDIF}
            if (Found) then
               continue;
{$IFDEF VERBOSE_ANNOTATIONS} Writeln('    building...'); {$ENDIF}

            if (not Assigned(Context)) then
               Context := Container.FirstChild
            else
               Context := Context.NextSibling;
            if ((Context is TElement) and (TElement(Context).GetAttribute('class').AsString = 'status')) then
               Status := TElement(Context)
            else
            begin
               Status := E(eDiv, ['class', 'status'], [E(eInput, ['type', 'button', 'value', kEllipsis, 'onclick', 'toggleStatus(this)'])]);
               Container.InsertBefore(Status, Context);
            end;

            if ((Length(Feature.Bugs) > 0) and (Variant <> vDEV)) then
            begin
               P := E(eP, ['class', 'bugs'], [E(eStrong, [T('Spec bugs:')]), T(' ')]);
               First := True;
               for Bug in Feature.Bugs do
               begin
                  if (not First) then
                     P.AppendChild(T(', '));
                  P.AppendChild(E(eA, ['href', Bug.URL, 'title', Bug.Summary], Document, [T(Bug.ID, Document)]));
                  First := False;
               end;
               Status.AppendChild(P);
            end;
{$IFDEF VERBOSE_ANNOTATIONS} Writeln('    built'); {$ENDIF}
            Found := False;
            for BrowserIndex in TBrowserIndex do
            begin
               if (Feature.FirstGoodVersion[BrowserIndex].Version <> '') then
               begin
                  Found := True;
                  break;
               end;
            end;
{$IFDEF VERBOSE_ANNOTATIONS} Writeln('    found=', Found); {$ENDIF}
            if (Found) then
            begin
               P := E(eP, ['class', 'support'], [E(eStrong, [T('Support:')]), T(' '), T(Feature.CanIUseCode, Document)]);
               for BrowserIndex in TBrowserIndex do
               begin
                  if (Feature.FirstGoodVersion[BrowserIndex].Version <> '') then
                     case (Feature.FirstGoodVersion[BrowserIndex].State) of
                        sYes:
                           begin
                              P.AppendChild(E(eSpan, ['class', Browsers[BrowserIndex].Code + ' yes'], Document,
                                              [E(eSpan, [T(Browsers[BrowserIndex].Name, Document)]),
                                               T(' '),
                                               E(eSpan, [T(Feature.FirstGoodVersion[BrowserIndex].Version, Document), T('+')])]));
                           end;
                        sAlmost:
                           begin
                              P.AppendChild(E(eSpan, ['class', Browsers[BrowserIndex].Code + ' partial'], Document,
                                              [E(eSpan, [T(Browsers[BrowserIndex].Name, Document), T(' (limited)')]),
                                               T(' '),
                                               E(eSpan, [T(Feature.FirstGoodVersion[BrowserIndex].Version, Document), T('+')])]));
                           end;
                        sNo:
                           begin
                              P.AppendChild(E(eSpan, ['class', Browsers[BrowserIndex].Code + ' no'], Document,
                                              [E(eSpan, [T(Browsers[BrowserIndex].Name, Document)]),
                                               T(' '),
                                               E(eSpan, [T('None')])]));
                           end;
                        sRemoved:
                           begin
                              P.AppendChild(E(eSpan, ['class', Browsers[BrowserIndex].Code + ' no'], Document,
                                              [E(eSpan, [T(Browsers[BrowserIndex].Name, Document), T(' (removed)')]),
                                               T(' '),
                                               E(eSpan, [T('-'), T(Feature.FirstGoodVersion[BrowserIndex].LastVersion, Document)])]));
                           end;
                     end;
               end;
               Status.AppendChild(P);
            end;
{$IFDEF VERBOSE_ANNOTATIONS} Writeln('    considering caniusecode'); {$ENDIF}
            if (Length(Feature.CanIUseCode) > 0) then
            begin
               if (Found) then
                  Status.AppendChild(E(eP, ['class', 'caniuse'], [T('Source: '), E(eA, ['href', 'https://caniuse.com/#feat=' + Feature.CanIUseCode], Document, [T('caniuse.com')])]))
               else
                  Status.AppendChild(E(eP, ['class', 'caniuse'], [T('See also: '), E(eA, ['href', 'https://caniuse.com/#feat=' + Feature.CanIUseCode], Document, [T('caniuse.com')])]));
            end;
         end
         else
         begin
            if (Variant <> vDEV) then
            begin
               Warn('Could not find ID ' + ID + ' for annotation that uses URLs:');
               for Bug in Feature.Bugs do
                  Writeln('   ', Bug.URL);
               if (Feature.CanIUseCode <> '') then
                  Writeln('   https://caniuse.com/#feat=', Feature.CanIUseCode);
            end
            else if (Feature.CanIUseCode <> '') then
            begin
               Warn('Could not find ID ' + ID + ' for annotation that uses URLs:');
               Writeln('   https://caniuse.com/#feat=', Feature.CanIUseCode);
            end;
         end;
      end;
{$IFDEF VERBOSE_ANNOTATIONS} Writeln('END OF ANNOTATIONS'); {$ENDIF}
   end;

var
   CrossRefNode, CrossRefNodeNext: PCrossReferenceListNode;
   ID, ExtractedData: CutRope;
   Scratch: Rope;
   NewLink, DFN: TElement;
   DFNEntry: TDFNEntry;
   ListNodeHead, ListNode, NextListNode: PElementListNode;
   Anchor, DFNAnchor, SectionName: UTF8String;
begin
   XrefsByDFNAnchor := TXrefsByDFNAnchor.Create(@UTF8StringHash32);
   IDs := TElementMap.Create(@UTF8StringHash32);
   Document.TakeOwnership(IDs);
   StringStore := TStringStore.Create();
   Document.TakeOwnership(StringStore);
   References := TReferences.Create(@UTF8StringHash32, 12);
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
         {$IFDEF DEBUG} Writeln('Adjusting headers, references, finding cross-references...'); {$ENDIF}
         SecondPass();
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
                     SectionName := DelSpace1(StringReplace(StringReplace(CrossRefNode^.LastHeadingText, #$0A, ' ', [rfReplaceAll]), #$22, '\' + #$22, [rfReplaceAll]));
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
               if (Variant <> vDEV) then
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
         {$IFDEF DEBUG} Writeln('Inserting annotations...'); {$ENDIF}
         InsertAnnotations();
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
      References.Free();
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

   procedure WalkIn(const Element: TElement);
   var
      IsExcluder, Skip: Boolean;
      AttributeCount, Index: Cardinal;
      AttributeName, EscapedAttributeName, EscapedAttributeValue: UTF8String;
      Quotes: TQuoteType;
      Variant: TAllVariants;
   begin
      IsExcluder := DetermineIsExcluder(Element, AttributeCount);
      if ((not IsExcluder) and ((AttributeCount > 0) or (not (Element.HasProperties(propOptionalStartTag) or SkippableTBodyStartTag(Element))))) then
      begin
         Write(F, '<', Element.LocalName.AsString);
         if (AttributeCount > 0) then
            for AttributeName in Element.Attributes do
            begin
               Skip := False;
               for Variant in TAllVariants do
                  if (AttributeName = kExcludingAttribute[Variant]) then
                     Skip := True;
               if (Skip or (AttributeName = kCrossRefAttribute) or
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
               case (Quotes) of
                  qtSingle: Write(F, ' ' + EscapedAttributeName + '=''' + EscapedAttributeValue + '''');
                  qtDouble: Write(F, ' ' + EscapedAttributeName + '="' + EscapedAttributeValue + '"');
               else Write(F, ' ' + EscapedAttributeName + '=' + EscapedAttributeValue);
               end;
            end;
         Write(F, '>');
      end;
      CurrentElement := Element;
   end;

   procedure WalkOut(const Element: TElement);
   var
      IsExcluder: Boolean;
      AttributeCount: Cardinal;
   begin
      if (InSplit and Element.HasAttribute(kExcludingAttribute[vSplit])) then
         exit;
      IsExcluder := DetermineIsExcluder(Element, AttributeCount);
      if (not (IsExcluder or Element.HasProperties(propVoidElement) or
               (Element.HasProperties(propOptionalEndTag) and ((not Assigned(Element.NextSibling)) or (AutoclosedBy(Element, Element.NextSibling)))))) then
      begin
         Write(F, '</', Element.LocalName.AsString, '>');
      end;
      if (Element.ParentNode is TElement) then
         CurrentElement := TElement(Element.ParentNode)
      else
         CurrentElement := nil;
   end;

var
   Current: TNode;
begin
   Assign(F, FileName);
   Rewrite(F);
   Write(F, '<!DOCTYPE html>');
   Current := Document.DocumentElement;
   CurrentElement := nil;
   repeat
      Assert(Assigned(Current));
      if (Current is TElement) then
      begin
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
         if (CurrentElement.HasProperties(propRawTextElement)) then
            Write(F, TText(Current).Data.AsString)
         else
            Write(F, EscapeText(TText(Current).Data).AsString);
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
   FirstChild := Document.DocumentElement.FirstChild as TElement;
   Document.DocumentElement.InsertBefore(Link, FirstChild);
   // find body
   Current := Document;
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
      if (ExtractedData.IsEmpty) then
         FragmentLinks.Append(kIndexFilename)
      else
         FragmentLinks.AppendDestructively(ExtractedData);
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
   SectionDoc.DocumentElement.SetAttribute('class', 'split index');
   Save(SectionDoc, Base + kIndexFilename);
   SectionDoc.Free();
   BigTOC.Remove();
   // save sections
   CurrentSection := Sections.FirstChild as TElement;
   while (Assigned(CurrentSection)) do
   begin
      SectionName := CurrentSection.GetAttribute(kFileNameAttribute).AsString;
      SectionDoc := Document.CloneNode(True);
      SectionDoc.DocumentElement.SetAttribute('class', 'split');
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
      Scratch.Append(kIndexFilename);
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

function CanIUseURLToID(const SpecURL: UTF8String; out ID: UTF8String): Boolean;
var
   HashIndex: Cardinal;
begin
   ID := '';
   Result := True;
   if (Pos('https://html.spec.whatwg.org/', SpecURL) = 1) then
   begin
      HashIndex := Pos('#', SpecURL); // $R-
      if (HashIndex > 0) then
      begin
         ID := Copy(SpecURL, HashIndex+1, Length(SpecURL)-HashIndex);
      end
      else
         Result := False;
   end
   else
   if (SpecURL = 'http://www.w3.org/TR/MathML/') then
   begin
      ID := 'mathml';
   end
   else
      Result := False;
   {$IFDEF VERBOSE_ID_FINDER}
      if (not Result) then
         Writeln('Could not find ID in: ', SpecURL);
   {$ENDIF}
end;

function BugzillaURLToID(const SpecURL: UTF8String; out ID: UTF8String): Boolean;
var
   HashIndex: Cardinal;
begin
   ID := '';
   Result := True;
   if ((Pos('http://www.w3.org/TR/html51/', SpecURL) = 1) or
       (Pos('http://www.w3.org/TR/html5/', SpecURL) = 1) or
       (Pos('http://www.w3.org/TR/html/', SpecURL) = 1) or
       (Pos('http://www.w3.org/html/wg/drafts/html/master/', SpecURL) = 1) or
       (Pos('http://dev.w3.org/html5/spec/', SpecURL) = 1) or
       (Pos('http://www.w3.org/TR/2011/WD-html5-20110525/', SpecURL) = 1) or
       (Pos('http://www.w3.org/TR/2014/PR-html5-20140916/', SpecURL) = 1) or
       (Pos('http://www.w3.org/TR/workers/', SpecURL) = 1) or
       (Pos('http://www.w3.org/TR/webstorage/', SpecURL) = 1) or
       (Pos('http://www.w3.org/TR/webmessaging/', SpecURL) = 1) or
       (Pos('https://html.spec.whatwg.org/', SpecURL) = 1) or
       (Pos('https://www.whatwg.org/specs/web-apps/current-work/', SpecURL) = 1) or
       (Pos('http://www.whatwg.org/specs/web-apps/current-work/', SpecURL) = 1) or
       (Pos('https://whatwg.org/specs/web-apps/current-work/', SpecURL) = 1) or
       (Pos('http://whatwg.org/specs/web-apps/current-work/', SpecURL) = 1)) then
   begin
      HashIndex := Pos('#', SpecURL); // $R-
      if (HashIndex > 0) then
      begin
         ID := Copy(SpecURL, HashIndex+1, Length(SpecURL)-HashIndex);
      end
      else
      if (SpecURL = 'http://www.w3.org/TR/workers/') then
      begin
         ID := 'workers';
      end
      else
         Result := False;
   end
   else
   if ((SpecURL = 'http://www.w3.org/TR/eventsource/') or
       (SpecURL = 'http://dev.w3.org/html5/eventsource/')) then
   begin
      ID := 'server-sent-events';
   end
   else
   if (SpecURL = 'http://www.w3.org/TR/xhtml1/') then
   begin
      ID := 'parsing-xhtml-documents';
   end
   else
   if ((SpecURL = 'http://www.w3.org/TR/websockets/') or
       (SpecURL = 'http://dev.w3.org/html5/websockets/')) then
   begin
      ID := 'network';
   end
   else
   if (SpecURL = 'http://www.w3.org/TR/html-markup/ruby.html') then
   begin
      ID := 'the-ruby-element';
   end
   else
      Result := False;
   if (Result) then
   begin
      if (ID = 'top') then
         Result := False;
   end;
   {$IFDEF VERBOSE_ID_FINDER}
      if (not Result) then
         Writeln('Could not find ID in: ', SpecURL);
   {$ENDIF}
end;

procedure Inform(Message: UTF8String);
begin
   if (not Quiet) then
      Writeln(Message);
end;

// http://wiki.freepascal.org/UTF8_strings_and_characters#Search_and_copy
// TODO: SplitInHalf is expensive, should use ropes. https://github.com/whatwg/wattsi/issues/40
function SplitInHalf(Txt, Separator: UTF8String; out Half1, Half2: UTF8String): Boolean;
var
  i: Integer;
begin
  i := Pos(Separator, Txt);
  Result := i > 0;
  if Result then
  begin
    Half1 := Copy(Txt, 1, i-1);
    Half2 := Copy(Txt, i+Length(Separator), Length(Txt));
  end;
end;

procedure PreProcessCanIUseData(const CanIUseJSONFilename: AnsiString);
var
   CanIUseData, Agent, Version, FeatureData: TJSON;
   Browser: TBrowser;
   BrowserCode, FeatureCode, SpecURL, RawState, ID, LowVersion, HighVersion: UTF8String;
   CurrentUsage: Double;
   BrowserIndex, CopyIndex: TBrowserIndex;
   VersionIndex, StateIndex: Cardinal;
   Feature: TFeature;
   States: set of TImplState;
   NewState: TImplGoodState;
begin
   Inform('Parsing caniuse.com data...');
   CanIUseData := ParseJSON(ReadTextFile(CanIUseJSONFilename));
   try
      Inform('Processing caniuse.com data...');
      for BrowserIndex := Low(Browsers) to High(Browsers) do
      begin
         Browsers[BrowserIndex].Code := '';
         Browsers[BrowserIndex].Name := '<>';
         Browsers[BrowserIndex].TotalUsage := 0.0;
         SetLength(Browsers[BrowserIndex].Versions, 0);
      end;
      if (not (CanIUseData['agents'] is TJSONObject)) then
         raise ESyntaxError.Create('caniuse.com json file is missing agents data');
      for BrowserCode in TJSONObject(CanIUseData['agents']).Keys do
      begin
         Agent := CanIUseData['agents'][BrowserCode];
         if (not (Agent is TJSONObject)) then
            raise ESyntaxError.Create('caniuse.com json file has bogus data for agent');
         if (not (Agent['versions'] is TJSONArray)) then
            raise ESyntaxError.Create('caniuse.com json file has bogus data for agent versions');
         if (Agent['versions'].Length > 0) then
         begin
            Browser.TotalUsage := 0.0;
            if (not (Agent['usage_global'] is TJSONObject)) then
               raise ESyntaxError.Create('caniuse.com json file has bogus data for agent usage');
            for CurrentUsage in Agent['usage_global'] do
               Browser.TotalUsage := Browser.TotalUsage + CurrentUsage;
            for BrowserIndex := Low(Browsers) to High(Browsers) do
            begin
               if (Browsers[BrowserIndex].TotalUsage < Browser.TotalUsage) then
               begin
                  Browser.Code := BrowserCode;
                  if (not (Agent['browser'] is TJSONString)) then
                     raise ESyntaxError.Create('caniuse.com json file has bogus data for agent name');
                  Browser.Name := Agent['browser'];
                  VersionIndex := 0;
                  SetLength(Browser.Versions, Agent['versions'].Length);
                  for Version in Agent['versions'] do
                     if (Assigned(Version)) then
                     begin
                        Browser.Versions[VersionIndex] := Version;
                        Inc(VersionIndex);
                     end;
                  SetLength(Browser.Versions, VersionIndex);
                  if (BrowserIndex < High(BrowserIndex)) then
                     for CopyIndex := High(BrowserIndex) downto Succ(BrowserIndex) do
                        Browsers[CopyIndex] := Browsers[Pred(CopyIndex)];
                  Browsers[BrowserIndex] := Browser;
                  break;
               end;
            end;
         end;
      end;
      {$IFDEF VERBOSE_CANIUSE_PARSE}
         for BrowserIndex := Low(Browsers) to High(Browsers) do
         begin
            Writeln('Browser #', BrowserIndex, ': ', Browsers[BrowserIndex].Name, ' ("', Browsers[BrowserIndex].Code, '"): usage: ', Browsers[BrowserIndex].TotalUsage:3:2, '%');
            if (Length(Browsers[BrowserIndex].Versions) > 0) then
               for VersionIndex := Low(Browsers[BrowserIndex].Versions) to High(Browsers[BrowserIndex].Versions) do // $R-
                  Writeln('  ', Browsers[BrowserIndex].Versions[VersionIndex]);
         end;
      {$ENDIF}
      if (not (CanIUseData['data'] is TJSONObject)) then
         raise ESyntaxError.Create('caniuse.com json file is missing feature data');
      for FeatureCode in TJSONObject(CanIUseData['data']).Keys do
      begin
         FeatureData := CanIUseData['data'][FeatureCode];
         SpecURL := FeatureData['spec'];
         if (not CanIUseURLToID(SpecURL, ID)) then
            continue;
         if (Features.Has(ID)) then
            Feature := Features[ID]
         else
            Feature.Reset();
         Feature.CanIUseCode := FeatureCode;
         for BrowserIndex in TBrowserIndex do
         begin
            Feature.FirstGoodVersion[BrowserIndex].Version := '';
            Feature.FirstGoodVersion[BrowserIndex].LastVersion := '';
            Browser := Browsers[BrowserIndex];
            for VersionIndex := High(Browsers[BrowserIndex].Versions) downto Low(Browsers[BrowserIndex].Versions) do // $R-
            begin
               RawState := FeatureData['stats'][Browser.Code][Browser.Versions[VersionIndex]];
               States := [];
               if (Length(RawState) > 0) then
                  for StateIndex := 1 to Length(RawState) do // $R-
                     case RawState[StateIndex] of
                       'y': Include(States, sYes);
                       'a': Include(States, sAlmost);
                       'n': Include(States, sNo);
                       'p': Include(States, sPolyfill);
                       'u': Include(States, sUnknown);
                       'x': Include(States, sPrefix);
                       'd': Include(States, sDisabled);
                       '#', '0'..'9': Include(States, sNotes);
                     end;
               if ((States * [sYes, sAlmost, sNo, sPrefix, sDisabled] <> []) and
                   (not (sUnknown in States))) then
               begin
                  if (States * [sYes, sAlmost, sNo, sPrefix, sDisabled] = [sYes]) then
                     NewState := sYes
                  else
                  if (States * [sAlmost, sNo, sPrefix, sDisabled] = [sAlmost]) then
                     NewState := sAlmost
                  else
                     NewState := sNo;
                  if (not SplitInHalf(Browser.Versions[VersionIndex], '-', LowVersion, HighVersion)) then
                  begin
                     LowVersion := Browser.Versions[VersionIndex];
                     HighVersion := LowVersion;
                  end;
                  if (Feature.FirstGoodVersion[BrowserIndex].Version = '') then
                  begin
                     Feature.FirstGoodVersion[BrowserIndex].LastVersion := HighVersion;
                     if ((VersionIndex <> High(Browsers[BrowserIndex].Versions)) and (NewState <> sNo)) then
                       NewState := sRemoved;
                  end;
                  if ((Feature.FirstGoodVersion[BrowserIndex].Version <> '') and (Feature.FirstGoodVersion[BrowserIndex].State <> NewState)) then
                     break;
                  Feature.FirstGoodVersion[BrowserIndex].Version := LowVersion;
                  Feature.FirstGoodVersion[BrowserIndex].State := NewState;
               end;
            end;
         end;
         Features[ID] := Feature;
      end;
   finally
      CanIUseData.Free();
   end;
end;

procedure PreProcessBugsData(const BugsFilename: AnsiString);
type
   TCSVParseMode = (pmRaw, pmQuoted, pmEscaped);
var
   BugsFile: Text;
   S, ID: UTF8String;
   Fields: array[0..2] of UTF8String;
   StringIndex, Field: Cardinal;
   Mode: TCSVParseMode;
   Bug: TBug;
   Feature: TFeature;
begin
   Inform('Parsing bugs data...');
   Assign(BugsFile, BugsFilename);
   Reset(BugsFile);
   Readln(BugsFile); // skip header
   while not eof(BugsFile) do
   begin
      Readln(BugsFile, S);
      StringIndex := 1;
      Mode := pmRaw;
      Field := 0;
      Fields[0] := '';
      Fields[1] := '';
      Fields[2] := '';
      while ((StringIndex < Length(S)) and (Field <= High(Fields))) do
      begin
         case (S[StringIndex]) of
            '"':
               begin
                  case (Mode) of
                     pmQuoted:
                        begin
                           Mode := pmEscaped;
                        end;
                     pmEscaped:
                        begin
                           Mode := pmQuoted;
                           Fields[Field] := Fields[Field] + '"';
                        end;
                     else
                        begin
                           Mode := pmQuoted;
                           // fail if field is not ''
                        end;
                  end;
               end;
            ',':
               begin
                  case (Mode) of
                     pmQuoted:
                        begin
                           Fields[Field] := Fields[Field] + ',';
                        end;
                     pmEscaped:
                        begin
                           Mode := pmRaw;
                           Inc(Field);
                        end;
                     else
                        begin
                           Inc(Field);
                        end;
                  end;
               end;
            else
               begin
                  Fields[Field] := Fields[Field] + S[StringIndex];
                  // fail if Mode is pmEscaped
               end;
         end;
         Inc(StringIndex);
      end;
      if (not BugzillaURLToID(Fields[1], ID)) then
         continue;
      if (Features.Has(ID)) then
         Feature := Features[ID]
      else
         Feature.Reset();
      Bug.ID := Fields[0];
      Bug.URL := 'https://www.w3.org/Bugs/Public/show_bug.cgi?id=' + Fields[0];
      Bug.Summary := Fields[2];
      SetLength(Feature.Bugs, Length(Feature.Bugs)+1);
      Feature.Bugs[High(Feature.Bugs)] := Bug;
      Features[ID] := Feature;
   end;
   Close(BugsFile);
end;

function Main(): Boolean;
const
   OtherVariants = [Low(TVariants)..High(TVariants)] - [Low(TVariants)];
var
   ParamOffset: Integer = 0;
   SourceFile: AnsiString;
   Source: TFileData;
   Parser: THTMLParser;
   BigTOC: TElement;
   Documents: array[TVariants] of TDocument;
   {$IFDEF TIMINGS} StartTime: TDateTime; {$ENDIF}
   Variant: TAllVariants;
begin
   Result := False;
   if (ParamCount() <> 4) then
      if ((ParamCount() = 5) and (ParamStr(1) = '--quiet')) then
      begin
         Quiet := true;
         ParamOffset := 1;
      end
      else
      if ((ParamCount() = 1) and (ParamStr(1) = '--version')) then
      begin
         Writeln('wattsi ' + IntToStr(Version));
         exit;
      end
      else
      begin
         Writeln('wattsi: invalid arguments');
         Writeln('syntax:');
         Writeln('  wattsi [--quiet] <source-file> <output-directory> <caniuse.json> <bugs.csv>');
         Writeln('  wattsi --version');
         exit;
      end;
   SourceFile := ParamStr(1 + ParamOffset);
   OutputDirectory := ParamStr(2 + ParamOffset);
   if (not IsEmptyDirectory(OutputDirectory)) then
   begin
      // only act if, when we start, the output directory is empty, to make sure that the
      // caller is indeed expecting us to put the data there
      Writeln('wattsi: output directory (second argument) must be an existing empty directory');
      exit;
   end;
   Features := TFeatureMap.Create(@UTF8StringHash32);
   try
      PreProcessCanIUseData(ParamStr(3 + ParamOffset));
      PreProcessBugsData(ParamStr(4 + ParamOffset));
      {$IFDEF VERBOSE_PREPROCESSORS}
         if (Assigned(Features)) then
            for ID in Features do
            begin
               Write('#', ID, ':');
               for BrowserIndex := Low(Features[ID].FirstGoodVersion) to High(Features[ID].FirstGoodVersion) do
                  if (Features[ID].FirstGoodVersion[BrowserIndex] <> '') then
                     Write(' ', Browsers[BrowserIndex].Name, ' ', Features[ID].FirstGoodVersion[BrowserIndex], '+ ');
               Writeln();
               if (Length(Features[ID].Bugs) > 0) then
                  for BugIndex := Low(Features[ID].Bugs) to High(Features[ID].Bugs) do // $R-
                     Writeln(' ', Features[ID].Bugs[BugIndex].ID, ' ', Features[ID].Bugs[BugIndex].Summary);
            end;
      {$ENDIF}
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
         {$IFDEF TIMINGS} Writeln('Cloning...'); {$ENDIF}
         {$IFDEF TIMINGS} StartTime := Now(); {$ENDIF}
         for Variant in OtherVariants do
            Documents[Variant] := Documents[Low(TVariants)].CloneNode(True);
         {$IFDEF TIMINGS} Writeln('Elapsed time: ', MillisecondsBetween(StartTime, Now()), 'ms'); {$ENDIF}
         try
            try
               // gen...
               for Variant in TVariants do
               begin
                  Inform('Generating ' + Uppercase(kSuffixes[Variant]) + ' variant...');
                  {$IFDEF TIMINGS} StartTime := Now(); {$ENDIF}
                  ProcessDocument(Documents[Variant], Variant, BigTOC); // $R-
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
                  {$IFDEF TIMINGS} Writeln('Splitting spec...'); {$ENDIF}
                  {$IFDEF TIMINGS} StartTime := Now(); {$ENDIF}
                  MkDir(OutputDirectory + '/multipage-' + kSuffixes[Variant]);
                  if (not Split(Documents[Variant], BigTOC, OutputDirectory + '/multipage-' + kSuffixes[Variant] + '/')) then
                     raise EAbort.Create('Could not split specification');
                  {$IFDEF TIMINGS} Writeln('Elapsed time: ', MillisecondsBetween(StartTime, Now()), 'ms'); {$ENDIF}
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
               for Variant in TVariants do
                  Documents[Variant].Free();
            except
               ReportCurrentException();
            end;
         end;
      finally
         Source.Destroy();
      end;
   finally
      Features.Free();
   end;
end;

begin
   Main();
end.
