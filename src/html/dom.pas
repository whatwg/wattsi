{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit dom;

// This library is based on: http://dom.spec.whatwg.org/

// Apart from being a subset and using FreePascal conventions where
// appropriate, it differs from the spec in the following ways:
//  - TNode doesn't have the members relating to children, those are
//    instead only available on TElement and TDocument.
//  - Anything that would throw an exception is not allowed; behaviour
//    for those cases is not defined (may assert, crash).
//  - TElement and TDocument have a series of convenience constructors
//  - TCharacterData has a constructor inherited by TText and TComment
//  - TCharacterData.AppendData() is called AppendDataDestructively()
//    since it takes ownership of the passed rope
//  - Anything in the API that was called "URI" is now "URL"
//  - Elements have:
//      - HasProperties(prop) (see webdom.html for the properties you can use)
//      - IsIdentity(ns, name)
//      - FindFirstMatchingChild(ns, name)
//      - SwapChildNodes(element) swaps the child nodes of two elements
//      - ReplaceChildWithChildren() which inserts a node's child nodes in place of the node
//  - TElement.Attributes is a hash table, not an array of Attr,
//    and it can be nil. Also, if you clone it, make sure you don't
//    Append() to any of the values, because that will cause one of the
//    two copies to get invalid pointers if you're using ropes
//  - Attributes have no namespaces. Instead, designate attribute
//    namespaces by prefixing their names by an identifying prefix
//    and a space; e.g. 'xlink href', 'xml lang'.
//  - Documents have a TakeOwnership() method so that they can own
//    objects that are owning the UTF8Strings to which Ropes point.

interface

uses
   hashtable, hashfunctions, canonicalstrings, {$IFDEF USEROPES} ropes, plasticarrays {$ELSE} stringutils {$ENDIF}, genericutils;

type
   {$IFDEF USEROPES}
      TDOMString = Rope; // so we can convert it to a rope later
      TCutDOMString = CutRope; // so we can convert it to a rope later
   {$ELSE}
      TDOMString = UTF8String; // so we can convert it to a rope later
      TCutDOMString = CutUTF8String; // so we can convert it to a rope later
   {$ENDIF}
   TNode = class;
   TDocument = class;
   TDocumentType = class;
   TElement = class;
   TComment = class;

   TNodeList = class abstract
    protected
      function GetLength(): Integer; virtual; abstract;
      function GetItem(const Index: Integer): TNode; virtual; abstract;
    public
      property Length: Integer read GetLength;
      property Item[Index: Integer]: TNode read GetItem; default;
   end;

   TNode = class abstract
    protected
      {$IFOPT C+} FDestroying: Boolean; {$ENDIF}
      FParentNode, FPreviousSibling, FNextSibling: TNode;
      function GetTextContent(): Rope;
    public
      {$IFOPT C+} destructor Destroy(); override; {$ENDIF}
      function HasChildNodes(): Boolean; virtual;
      procedure Remove(); inline;
      function CloneNode(const Deep: Boolean = False): TNode; virtual; abstract;
      function NextElementSibling(): TElement;
      function PreviousElementSibling(): TElement;
      property ParentNode: TNode read FParentNode;
      property PreviousSibling: TNode read FPreviousSibling;
      property NextSibling: TNode read FNextSibling;
      property TextContent: Rope read GetTextContent; // expensive
   end;

   TDocument = class(TNode)
    public
     type
      TDocumentMode = (dmQuirksMode, dmLimitedQuirksMode, dmNoQuirksMode);
    strict private
     type
      TDocumentNodeList = class(TNodeList)
       protected
        type
         TCommentArray = array of TComment;
        var
         FDocType: TDocumentType;
         FDocumentElement: TElement;
         FCommentsTop, FCommentsMiddle, FCommentsBottom: TCommentArray;
         function GetLength(): Integer; override;
         function GetItem(const Index: Integer): TNode; override;
         procedure AppendChild(const NewNode: TNode);
         procedure RemoveChild(const OldNode: TNode);
      end;
     var
      {$IFDEF USEROPES} FOwnedObjects: specialize PlasticArray <TObject, TObjectUtils>; {$ENDIF}
    protected
      FDocumentMode: TDocumentMode;
      FChildNodes: TDocumentNodeList;
      function GetFirstChild(): TNode; inline;
      function GetLastChild(): TNode; inline;
    public
      constructor Create();
      constructor Create(const NewDocType: TDocumentType; const NewDocumentElement: TElement; const NewDocumentMode: TDocumentMode = dmNoQuirksMode);
      destructor Destroy(); override;
      {$IFDEF USEROPES} procedure TakeOwnership(const NewObject: TObject); {$ENDIF}
      procedure SetDocumentMode(const NewDocumentMode: TDocumentMode); inline;
      function HasChildNodes(): Boolean; override;
      function CloneNode(const Deep: Boolean = False): TDocument; override;
      function AppendChild(const NewNode: TNode): TNode; inline;
      function RemoveChild(const OldNode: TNode): TNode; inline;
      property DocType: TDocumentType read FChildNodes.FDocType;
      property DocumentElement: TElement read FChildNodes.FDocumentElement;
      property DocumentMode: TDocumentMode read FDocumentMode;
      property FirstChild: TNode read GetFirstChild;
      property LastChild: TNode read GetLastChild;
   end;

   TDocumentType = class(TNode)
    protected
      FName, FPublicID, FSystemID: TDOMString;
    public
      constructor CreateDestructively(var NewName, NewPublicID, NewSystemID: TDOMString);
      function CloneNode(const Deep: Boolean = False): TDocumentType; override;
      property Name: TDOMString read FName;
      property PublicID: TDOMString read FPublicID;
      property SystemID: TDOMString read FSystemID;
   end;

   TElementProperties = type Cardinal;

   TElement = class(TNode)
    strict private
     type
      TElementChildrenNodeList = class (TNodeList)
       strict private
        FMaintainingArray: Boolean;
        // if FMaintainingArray is True:
        FCount, FStart: Integer;
        FNodes: array of TNode; // from FStart to FStart+FCount-1
        // if FMaintainingArray is False:
        FFirstNode, FLastNode: TNode;
        procedure EnsureMaintainingArray(const Margin: Integer = 0); // sets FMaintainingArray to true and converts accordingly; also ensures FNodes is big enough to increase by Margin without realloc
       protected
        FOwnerElement: TElement;
        function GetLength(): Integer; override;
        function GetItem(const Index: Integer): TNode; override;
        function GetFirstChild(): TNode; inline;
        function GetLastChild(): TNode; inline;
       public
        constructor Create(const ParentNode: TElement);
        procedure InsertAt(const NewNode: TNode; const Position: Integer);
        procedure RemoveAt(const Position: Integer);
        procedure ReplaceAt(const NewNode: TNode; const Position: Integer);
        procedure InsertBefore(const NewNode: TNode; const Child: TNode);
        procedure AppendChild(const NewNode: TNode);
        procedure ReplaceChild(const NewNode: TNode; const OldNode: TNode);
        procedure ReplaceChildWithChildren(const OldNode: TElement);
        procedure RemoveChild(const OldNode: TNode);
        property FirstChild: TNode read GetFirstChild;
        property LastChild: TNode read GetLastChild;
      end;
    public
     type
      TUTF8StringUtils = specialize DefaultUtils <UTF8String>;
      TAttributeHashTable = class (specialize THashTable <UTF8String, TDOMString, TUTF8StringUtils>)
       public
        constructor Create(PredictedCount: THashTableSizeInt = 1);
      end;
    protected
      FProperties: TElementProperties;
      FChildNodes: TElementChildrenNodeList;
      FAttributes: TAttributeHashTable;
      FNamespaceURL, FLocalName: TCanonicalString;
      function GetChildNodes(): TNodeList; inline;
      function GetFirstChild(): TNode; inline;
      function GetLastChild(): TNode; inline;
      procedure EnsureChildNodes();
      procedure EnsureAttributes();
    public
      constructor Create(const NewNamespaceURL: TCanonicalString; const NewLocalName: TCanonicalString; const NewAttributes: TAttributeHashTable; const NewChildren: array of TNode; const NewProperties: TElementProperties);
      constructor Create(const NewNamespaceURL, NewLocalName: TCanonicalString; const NewAttributes: TAttributeHashTable; const NewChildren: array of TNode);
      constructor Create(const NewNamespaceURL, NewLocalName: TCanonicalString; const NewAttributes: TAttributeHashTable = nil);
      constructor Create(const NewNamespaceURL, NewLocalName: TCanonicalString; const NewChildren: array of TNode);
      {$IFOPT C+} procedure AfterConstruction(); override; {$ENDIF}
      destructor Destroy(); override;
      procedure AddProperty(const NewProperty: TElementProperties); inline;
      function HasProperties(const TestProperties: TElementProperties): Boolean; inline;
      function HasSomeProperties(const TestProperties: TElementProperties): Boolean; inline;
      function IsIdentity(const TestNamespaceURL, TestLocalName: TCanonicalString): Boolean; inline;
      procedure SwapChildNodes(var OtherElement: TElement); inline;
      procedure SwapAttributes(var OtherElement: TElement); inline;
      function FindFirstMatchingChild(const TestNamespaceURL, TestLocalName: TCanonicalString): TElement;
     public
      function HasChildNodes(): Boolean; override;
      function InsertBefore(const NewNode: TNode; const Child: TNode): TNode; inline;
      function AppendChild(const NewNode: TNode): TNode; inline;
      function ReplaceChild(const NewNode: TNode; const OldNode: TNode): TNode; inline;
      procedure ReplaceChildWithChildren(const OldNode: TElement); inline;
      function RemoveChild(const OldNode: TNode): TNode; inline;
      function HasAttribute(const Name: UTF8String): Boolean; inline;
      function GetAttribute(const Name: UTF8String): CutRope; inline;
      procedure SetAttribute(const Name: UTF8String; const Value: UTF8String); inline; // must be a constant
      procedure SetAttributeDestructively(const Name: UTF8String; var Value: TDOMString); inline;
      procedure SetAttributeDestructively(const Name: UTF8String; var Value: TCutDOMString); inline;
      procedure RemoveAttribute(const Name: UTF8String); inline;
      function CloneNode(const Deep: Boolean = False): TElement; override;
      property Attributes: TAttributeHashTable read FAttributes; // WARNING - this is a foot gun. Use extreme care.
      property ChildNodes: TNodeList read GetChildNodes;
      property FirstChild: TNode read GetFirstChild;
      property LastChild: TNode read GetLastChild;
      property NamespaceURL: TCanonicalString read FNamespaceURL;
      property LocalName: TCanonicalString read FLocalName;
   end;
   TElementClass = class of TElement;

   TCharacterData = class abstract (TNode)
    protected
      FData: TDOMString;
    public
      constructor Create(const NewData: UTF8String); // only valid when called with constant string
      constructor CreateDestructively(var NewData: TDOMString);
      constructor CreateDestructively(var NewData: TCutDOMString);
      procedure AppendDataDestructively(var Data: TDOMString); inline;
      procedure AppendDataDestructively(var Data: TCutDOMString); inline;
      property Data: TDOMString read FData write FData;
   end;

   TText = class(TCharacterData)
      function CloneNode(const Deep: Boolean = False): TText; override;
   end;

   TComment = class(TCharacterData)
      function CloneNode(const Deep: Boolean = False): TComment; override;
   end;

type
   TWalkOutCallback = procedure (const Element: TElement) is nested;
   // Caller will not use Element again, so feel free to e.g. free it, move it out of the DOM, whatever

// These return False once the end (Top) is reached
function WalkToNext(var Current: TNode; const Top: TNode; const WalkOutCallback: TWalkOutCallback): Boolean; //inline;
function WalkToNextSkippingChildren(var Current: TNode; const Top: TNode; const WalkOutCallback: TWalkOutCallback): Boolean; //inline;

type
   TStringStore = class
      FStrings: array of UTF8String;
      procedure Push(constref Value: UTF8String); inline;
   end;

implementation

uses
   exceptions {$IFOPT C+}, rtlutils {$ENDIF} {$IFDEF DEBUG}, debug {$ENDIF};

{$IFOPT C+}
destructor TNode.Destroy();
begin
   try
      if (Assigned(RaiseList)) then
      begin
         Writeln('Node destroyed with active exception:');
         ReportCurrentException;
      end;
      if (Assigned(FParentNode)) then
         Assert(FParentNode.FDestroying, 'Node freed while still in DOM');
   except
      ReportCurrentException();
   end;
   FDestroying := True;
   inherited;
end;
{$ENDIF}

function TNode.HasChildNodes(): Boolean;
begin
   Result := False;
end;

procedure TNode.Remove();
begin
   if (Assigned(FParentNode)) then
   begin
      if (FParentNode is TElement) then
         (FParentNode as TElement).RemoveChild(Self)
      else
      if (FParentNode is TDocument) then
         (FParentNode as TDocument).RemoveChild(Self)
      else
         Assert(False);
   end;
end;

function TNode.GetTextContent(): Rope;
var
   Current: TNode;
   Fragment: CutRope;
begin
   Result := Default(Rope);
   Current := Self;
   while (WalkToNext(Current, Self, nil)) do
   begin
      if (Current is TText) then
      begin
         Fragment := TText(Current).FData.ExtractAll();
         Result.AppendDestructively(Fragment);
      end;
   end;
end;


function TNode.NextElementSibling(): TElement;
var
  Current: TNode;
begin
  Current := Self;
  while (Assigned(Current)) do
  begin
    Current := Current.NextSibling;
    if (Current is TElement) then
    begin
      Result := TElement(Current);
      exit;
    end;
  end;
end;

function TNode.PreviousElementSibling(): TElement;
var
  Current: TNode;
begin
  Current := Self;
  while (Assigned(Current)) do
  begin
    Current := Current.PreviousSibling;
    if (Current is TElement) then
    begin
      Result := TElement(Current);
      exit;
    end;
  end;
end;

function TDocument.TDocumentNodeList.GetLength(): Integer;
begin
   Assert(Assigned(Self));
   Assert(System.Length(FCommentsTop) + System.Length(FCommentsMiddle) + System.Length(FCommentsBottom) + 2 < High(Result));
   Result := System.Length(FCommentsTop) + System.Length(FCommentsMiddle) + System.Length(FCommentsBottom); // $R-
   if (Assigned(FDocType)) then
      Inc(Result);
   if (Assigned(FDocumentElement)) then
      Inc(Result);
end;

function TDocument.TDocumentNodeList.GetItem(const Index: Integer): TNode;
var
   Offset: Cardinal;
begin
   Assert(Index >= 0);
   Assert(Index < GetLength());
   if (Index < System.Length(FCommentsTop)-1) then
   begin
      Result := FCommentsTop[Index];
      exit;
   end;
   Offset := System.Length(FCommentsTop); // $R-
   if (Assigned(FDocType)) then
   begin
      if (Index-Offset = 0) then
      begin
         Result := FDocType;
         exit;
      end;
      Inc(Offset);
   end;
   if (Index < System.Length(FCommentsMiddle)-1) then
   begin
      Result := FCommentsMiddle[Index];
      exit;
   end;
   Inc(Offset, System.Length(FCommentsMiddle));
   if (Assigned(FDocumentElement)) then
   begin
      if (Index-Offset = 0) then
      begin
         Result := FDocumentElement;
         exit;
      end;
      Inc(Offset);
   end;
   Assert(Index < System.Length(FCommentsBottom)-1);
   Result := FCommentsBottom[Index];
end;

procedure TDocument.TDocumentNodeList.AppendChild(const NewNode: TNode);

   procedure AppendCommentTo(var Target: TCommentArray);
   begin
      SetLength(Target, System.Length(Target)+1);
      Target[High(Target)] := NewNode as TComment;
   end;

var
   NewPreviousSibling, NewNextSibling: TNode;

   procedure SetSiblingLinks();
   begin
      if (Assigned(NewPreviousSibling)) then
      begin
         NewNode.FPreviousSibling := NewPreviousSibling;
         Assert(NewPreviousSibling.FNextSibling = NewNextSibling);
         NewPreviousSibling.FNextSibling := NewNode;
      end;
      if (Assigned(NewNextSibling)) then
      begin
         NewNode.FNextSibling := NewNextSibling;
         Assert(NewNextSibling.FPreviousSibling = NewPreviousSibling);
         NewNextSibling.FPreviousSibling := NewNode;
      end;
   end;

begin
   Assert(System.Length(FCommentsTop) + System.Length(FCommentsMiddle) + System.Length(FCommentsBottom) + 3 < High(Length));
   Assert(not Assigned(NewNode.FParentNode));
   Assert(not Assigned(NewNode.FNextSibling));
   Assert(not Assigned(NewNode.FPreviousSibling));
   if (NewNode is TElement) then
   begin
      Assert(not Assigned(FDocumentElement));
      Assert(System.Length(FCommentsBottom) = 0);
      FDocumentElement := NewNode as TElement;
      if (System.Length(FCommentsMiddle) > 0) then
         NewPreviousSibling := FCommentsMiddle[High(FCommentsMiddle)]
      else
      if (Assigned(FDocType)) then
         NewPreviousSibling := FDocType
      else
      if (System.Length(FCommentsTop) > 0) then
         NewPreviousSibling := FCommentsTop[High(FCommentsTop)]
      else
         NewPreviousSibling := nil;
      if (System.Length(FCommentsBottom) > 0) then
         NewNextSibling := FCommentsBottom[0]
      else
         NewNextSibling := nil;
      SetSiblingLinks();
      exit;
   end;
   if (NewNode is TDocumentType) then
   begin
      Assert(not Assigned(FDocType));
      Assert(System.Length(FCommentsMiddle) = 0);
      Assert(not Assigned(FDocumentElement));
      Assert(System.Length(FCommentsBottom) = 0);
      FDocType := NewNode as TDocumentType;
      if (System.Length(FCommentsTop) > 0) then
         NewPreviousSibling := FCommentsTop[High(FCommentsTop)]
      else
         NewPreviousSibling := nil;
      if (System.Length(FCommentsMiddle) > 0) then
         NewNextSibling := FCommentsMiddle[0]
      else
      if (Assigned(FDocumentElement)) then
         NewNextSibling := FDocType
      else
      if (System.Length(FCommentsBottom) > 0) then
         NewNextSibling := FCommentsBottom[0]
      else
         NewNextSibling := nil;
      SetSiblingLinks();
      exit;
   end;
   Assert(NewNode is TComment);
   NewNextSibling := nil;
   if (Assigned(FDocumentElement)) then
   begin
      if (System.Length(FCommentsBottom) > 0) then
         NewPreviousSibling := FCommentsBottom[High(FCommentsBottom)]
      else
         NewPreviousSibling := FDocumentElement;
      AppendCommentTo(FCommentsBottom);
      SetSiblingLinks();
      exit;
   end;
   Assert(System.Length(FCommentsBottom) = 0);
   if (Assigned(FDocType)) then
   begin
      if (System.Length(FCommentsMiddle) > 0) then
         NewPreviousSibling := FCommentsMiddle[High(FCommentsMiddle)]
      else
         NewPreviousSibling := FDocType;
      AppendCommentTo(FCommentsMiddle);
      SetSiblingLinks();
      exit;
   end;
   Assert(System.Length(FCommentsMiddle) = 0);
   if (System.Length(FCommentsTop) > 0) then
      NewPreviousSibling := FCommentsTop[High(FCommentsTop)]
   else
      NewPreviousSibling := nil;
   AppendCommentTo(FCommentsTop);
   SetSiblingLinks();
end;

procedure TDocument.TDocumentNodeList.RemoveChild(const OldNode: TNode);

   procedure Merge(var Source, Dest: TCommentArray);
   var
      DestIndex: Cardinal;
   begin
      if (System.Length(Source) > 0) then
      begin
         if (System.Length(Dest) > 0) then
         begin
            DestIndex := System.Length(Dest); // $R-
            SetLength(Dest, System.Length(Dest) + System.Length(Source));
            Move(Source[0], Dest[DestIndex], System.Length(Source) * SizeOf(Source[0]));
         end
         else
         begin
            Source := Dest;
         end;
         SetLength(Dest, 0);
      end;
   end;

   function Check(var List: TCommentArray; const Index: Integer): Boolean; inline;
   begin
      //Writeln('Check(): Length(List)=', System.Length(List), ' Index=', Index);
      Assert(Index >= 0);
      Assert(Index >= Low(List));
      Assert(Index <= High(List));
      Assert(Index < System.Length(List));
      if (List[Index] = OldNode) then
      begin
         if (Index < High(List)) then
            Move(List[Index+1], List[Index], (High(List)-Index) * SizeOf(List[0])); // http://bugs.freepascal.org/view.php?id=26403
         SetLength(List, System.Length(List)-1);
         Result := True;
      end
      else
         Result := False;
   end;

var
   Index: Integer;
begin
   if (Assigned(OldNode.FPreviousSibling)) then
   begin
      OldNode.FPreviousSibling.FNextSibling := OldNode.FNextSibling;
      OldNode.FPreviousSibling := nil;
   end;
   if (Assigned(OldNode.FNextSibling)) then
   begin
      OldNode.FNextSibling.FPreviousSibling := OldNode.FPreviousSibling;
      OldNode.FNextSibling := nil;
   end;
   OldNode.FParentNode := nil;
   if (OldNode is TElement) then
   begin
      Assert(OldNode = FDocumentElement);
      FDocumentElement := nil;
      if (Assigned(FDocType)) then
      begin
         Merge(FCommentsBottom, FCommentsMiddle);
      end
      else
      begin
         Assert(System.Length(FCommentsMiddle) = 0);
         Merge(FCommentsBottom, FCommentsTop);
      end;
      exit;
   end;
   if (OldNode is TDocumentType) then
   begin
      Assert(OldNode = FDocType);
      FDocType := nil;
      Merge(FCommentsMiddle, FCommentsTop);
      exit;
   end;
   Assert(OldNode is TComment);
   for Index := Low(FCommentsTop) to High(FCommentsTop) do // $R-
      if (Check(FCommentsTop, Index)) then exit;
   for Index := Low(FCommentsMiddle) to High(FCommentsMiddle) do // $R-
      if (Check(FCommentsMiddle, Index)) then exit;
   for Index := Low(FCommentsBottom) to High(FCommentsBottom) do // $R-
      if (Check(FCommentsBottom, Index)) then exit;
   Assert(False);
end;


constructor TDocument.Create();
begin
   inherited Create();
   FChildNodes := TDocumentNodeList.Create();
end;

constructor TDocument.Create(const NewDocType: TDocumentType; const NewDocumentElement: TElement; const NewDocumentMode: TDocumentMode = dmNoQuirksMode);
begin
   Create();
   FChildNodes.FDocType := NewDocType;
   FChildNodes.FDocumentElement := NewDocumentElement;
   FDocumentMode := NewDocumentMode;
end;

destructor TDocument.Destroy();
var
   Index: Integer;
   O: TObject;
begin
   {$IFOPT C+} FDestroying := True; {$ENDIF}
   {$IFDEF DEBUG} try {$ENDIF}
   if (Assigned(FChildNodes.FDocType)) then
      FChildNodes.FDocType.Free();
   if (Assigned(FChildNodes.FDocumentElement)) then
      FChildNodes.FDocumentElement.Free();
   for Index := Low(FChildNodes.FCommentsTop) to High(FChildNodes.FCommentsTop) do // $R-
      FChildNodes.FCommentsTop[Index].Free();
   for Index := Low(FChildNodes.FCommentsMiddle) to High(FChildNodes.FCommentsMiddle) do // $R-
      FChildNodes.FCommentsMiddle[Index].Free();
   for Index := Low(FChildNodes.FCommentsBottom) to High(FChildNodes.FCommentsBottom) do // $R-
      FChildNodes.FCommentsBottom[Index].Free();
   FChildNodes.Free();
   for O in FOwnedObjects do
      O.Free();
   {$IFDEF DEBUG}
   except
      ReportCurrentException();
   end;
   {$ENDIF}
   inherited;
end;

{$IFDEF USEROPES}
procedure TDocument.TakeOwnership(const NewObject: TObject);
begin
   FOwnedObjects.Push(NewObject);
end;
{$ENDIF}

procedure TDocument.SetDocumentMode(const NewDocumentMode: TDocumentMode);
begin
   FDocumentMode := NewDocumentMode;
end;

function TDocument.HasChildNodes(): Boolean;
begin
   Assert(Assigned(FChildNodes));
   Result := FChildNodes.Length > 0;
end;

function TDocument.CloneNode(const Deep: Boolean = False): TDocument;
var
   Index: Cardinal;
begin
   Result := TDocument.Create();
   Result.FDocumentMode := FDocumentMode;
   if (Deep and (FChildNodes.Length > 0)) then
   begin
      // CLONE THE CHILDREN
      if (Length(FChildNodes.FCommentsTop) > 0) then
         for Index := Low(FChildNodes.FCommentsTop) to High(FChildNodes.FCommentsTop) do // $R-
            Result.AppendChild(FChildNodes.FCommentsTop[Index].CloneNode(True));
      if (Assigned(FChildNodes.FDocType)) then
         Result.AppendChild(FChildNodes.FDocType.CloneNode(True));
      if (Length(FChildNodes.FCommentsMiddle) > 0) then
         for Index := Low(FChildNodes.FCommentsMiddle) to High(FChildNodes.FCommentsMiddle) do // $R-
            Result.AppendChild(FChildNodes.FCommentsMiddle[Index].CloneNode(True));
      if (Assigned(FChildNodes.FDocumentElement)) then
         Result.AppendChild(FChildNodes.FDocumentElement.CloneNode(True));
      if (Length(FChildNodes.FCommentsBottom) > 0) then
         for Index := Low(FChildNodes.FCommentsBottom) to High(FChildNodes.FCommentsBottom) do // $R-
            Result.AppendChild(FChildNodes.FCommentsBottom[Index].CloneNode(True));
   end;
end;

function TDocument.GetFirstChild(): TNode;
begin
   if (Length(FChildNodes.FCommentsTop) > 0) then
      Result := FChildNodes.FCommentsTop[0]
   else
   if (Assigned(FChildNodes.FDocType)) then
      Result := FChildNodes.FDocType
   else
   if (Length(FChildNodes.FCommentsMiddle) > 0) then
      Result := FChildNodes.FCommentsMiddle[0]
   else
   if (Assigned(FChildNodes.FDocumentElement)) then
      Result := FChildNodes.FDocumentElement
   else
   if (Length(FChildNodes.FCommentsBottom) > 0) then
      Result := FChildNodes.FCommentsBottom[0]
   else
      Result := nil;
// with this I get an assertion failed elsewhere
// without it, the code runs fine
//   Assert(Assigned(Result) = (FChildNodes.Length > 0));
end;

function TDocument.GetLastChild(): TNode;
begin
   if (Length(FChildNodes.FCommentsBottom) > 0) then
      Result := FChildNodes.FCommentsBottom[High(FChildNodes.FCommentsBottom)]
   else
   if (Assigned(FChildNodes.FDocumentElement)) then
      Result := FChildNodes.FDocumentElement
   else
   if (Length(FChildNodes.FCommentsMiddle) > 0) then
      Result := FChildNodes.FCommentsMiddle[High(FChildNodes.FCommentsMiddle)]
   else
   if (Assigned(FChildNodes.FDocType)) then
      Result := FChildNodes.FDocType
   else
   if (Length(FChildNodes.FCommentsTop) > 0) then
      Result := FChildNodes.FCommentsTop[High(FChildNodes.FCommentsTop)]
   else
      Result := nil;
   Assert(Assigned(Result) = (FChildNodes.Length > 0));
end;

function TDocument.AppendChild(const NewNode: TNode): TNode;
begin
   FChildNodes.AppendChild(NewNode);
   NewNode.FParentNode := Self;
   Result := NewNode;
end;

function TDocument.RemoveChild(const OldNode: TNode): TNode;
begin
   FChildNodes.RemoveChild(OldNode);
   Result := OldNode;
end;


constructor TDocumentType.CreateDestructively(var NewName, NewPublicID, NewSystemID: TDOMString);
begin
   inherited Create();
   FName := NewName;
   NewName := Default(TDOMString);
   FPublicID := NewPublicID;
   NewPublicID := Default(TDOMString);
   FSystemID := NewSystemID;
   NewSystemID := Default(TDOMString);
end;

function TDocumentType.CloneNode(const Deep: Boolean = False): TDocumentType;
var
   Scratch: CutRope;
begin
   Result := TDocumentType.Create();
   Scratch := FName.ExtractAll();
   Result.FName.AppendDestructively(Scratch);
   Scratch := FPublicID.ExtractAll();
   Result.FPublicID.AppendDestructively(Scratch);
   Scratch := FSystemID.ExtractAll();
   Result.FSystemID.AppendDestructively(Scratch);
end;


constructor TElement.TAttributeHashTable.Create(PredictedCount: THashTableSizeInt = 1);
begin
   inherited Create(@UTF8StringHash32, PredictedCount);
end;

constructor TElement.TElementChildrenNodeList.Create(const ParentNode: TElement);
begin
   inherited Create();
   FOwnerElement := ParentNode;
end;

procedure TElement.TElementChildrenNodeList.EnsureMaintainingArray(const Margin: Integer = 0);
var
   TempNode: TNode;
   Index: Integer;
begin
   Assert(Margin >= 0);
   if (not FMaintainingArray) then
   begin
      Assert((FCount > 0) = Assigned(FFirstNode));
      Assert(Assigned(FFirstNode) = Assigned(FLastNode));
      Assert(System.Length(FNodes) = 0);
      Assert(FStart = 0);
      TempNode := FFirstNode;
      FMaintainingArray := True;
      SetLength(FNodes, FCount+Margin);
      FFirstNode := nil;
      Index := 0;
      while (Assigned(TempNode)) do
      begin
         Assert(TempNode.FParentNode = FOwnerElement);
         FNodes[Index] := TempNode;
         TempNode := TempNode.FNextSibling;
         Inc(Index);
      end;
      Assert((not Assigned(FLastNode)) or (FNodes[Index-1] = FLastNode));
      FLastNode := nil;
   end
   else
   if (Margin > 0) then
   begin
      Assert(System.Length(FNodes) > 0);
      Assert(System.Length(FNodes) >= FCount);
      if (System.Length(FNodes)-FStart < FCount+Margin) then
      begin
         if (FStart > 0) then
         begin
            Move(FNodes[FStart], FNodes[0], FCount * SizeOf(TNode));
            FStart := 0;
         end;
         Assert(FStart = 0);
         if (System.Length(FNodes) < FCount+Margin) then
         begin
            SetLength(FNodes, FCount+Margin);
         end;
      end;
   end;
   Assert(FMaintainingArray);
   Assert(System.Length(FNodes) >= FCount+Margin);
end;

procedure TElement.TElementChildrenNodeList.InsertAt(const NewNode: TNode; const Position: Integer);
begin
   Assert(Position >= 0);
   Assert(Position <= FCount);
   Assert(not Assigned(NewNode.FParentNode));
   Assert(not Assigned(NewNode.FPreviousSibling));
   Assert(not Assigned(NewNode.FNextSibling));
   if (FMaintainingArray or ((Position > 0) and (Position < FCount))) then
      EnsureMaintainingArray(1);
   if (FMaintainingArray) then
   begin
      Assert(FStart+Position <= High(FNodes));
      Assert(FStart+FCount <= High(FNodes));
      if ((FStart > 0) and (Position = 0)) then
         Dec(FStart)
      else
      if (Position < FCount) then
         Move(FNodes[FStart+Position], FNodes[FStart+Position+1], (FCount-Position) * SizeOf(TNode));
      FNodes[FStart+Position] := NewNode;
      if (Position > 0) then
      begin
         FNodes[FStart+Position-1].FNextSibling := NewNode;
         NewNode.FPreviousSibling := FNodes[FStart+Position-1];
      end;
      if (Position < FCount) then
      begin
         FNodes[FStart+Position+1].FPreviousSibling := NewNode;
         NewNode.FNextSibling := FNodes[FStart+Position+1];
      end;
   end
   else
   begin
      Assert((Position = 0) or (Position = FCount));
      if (Position = 0) then
      begin
         FFirstNode.FPreviousSibling := NewNode;
         NewNode.FNextSibling := FFirstNode;
         FFirstNode := NewNode;
      end;
      if (Position = FCount) then
      begin
         FLastNode.FNextSibling := NewNode;
         NewNode.FPreviousSibling := FFirstNode;
         FLastNode := NewNode;
      end;
   end;
   NewNode.FParentNode := FOwnerElement;
   Assert(Assigned(NewNode.FParentNode));
   Inc(FCount);
end;

procedure TElement.TElementChildrenNodeList.RemoveAt(const Position: Integer);
var
   OldNode: TNode;
begin
   Assert(Position >= 0);
   Assert(Position < FCount);
   if ((Position > 0) and (Position < FCount-1)) then
      EnsureMaintainingArray(0);
   if (FMaintainingArray) then
   begin
      Assert(FStart+FCount <= High(FNodes));
      Assert(FStart+Position <= High(FNodes));
      OldNode := FNodes[FStart+Position];
      if (Position = 0) then
         Inc(FStart)
      else
      if (Position < FCount) then
         Move(FNodes[FStart+Position+1], FNodes[FStart+Position], (FCount-Position) * SizeOf(TNode));
      if (Assigned(OldNode.FPreviousSibling)) then
      begin
         Assert(FNodes[FStart+Position-1] = OldNode.FPreviousSibling);
         OldNode.FPreviousSibling.FNextSibling := OldNode.FNextSibling;
         OldNode.FPreviousSibling := nil;
      end;
      if (Assigned(OldNode.FNextSibling)) then
      begin
         Assert(FNodes[FStart+Position] = OldNode.FNextSibling);
         OldNode.FNextSibling.FPreviousSibling := OldNode.FPreviousSibling;
         OldNode.FNextSibling := nil;
      end;
   end
   else
   begin
      Assert((FFirstNode = FLastNode) = (FCount = 1));
      Assert((Position = 0) or (Position = FCount-1));
      if (Position = 0) then
      begin
         OldNode := FFirstNode;
         Assert(not Assigned(OldNode.FPreviousSibling));
         FFirstNode := OldNode.FNextSibling;
         if (Assigned(FFirstNode)) then
         begin
            Assert(FLastNode.FPreviousSibling = OldNode);
            FFirstNode.FPreviousSibling := nil;
            OldNode.FNextSibling := nil;
         end;
      end;
      if (Position = FCount-1) then
      begin
         OldNode := FLastNode;
         Assert(not Assigned(OldNode.FNextSibling));
         FLastNode := OldNode.FPreviousSibling;
         if (Assigned(FLastNode)) then
         begin
            Assert(FLastNode.FNextSibling = OldNode);
            FLastNode.FNextSibling := nil;
            OldNode.FPreviousSibling := nil;
         end;
      end;
      Assert(Assigned(OldNode));
   end;
   Assert(Assigned(OldNode));
   OldNode.FParentNode := nil;
   Assert(not Assigned(OldNode.FParentNode));
   Assert(not Assigned(OldNode.FPreviousSibling));
   Assert(not Assigned(OldNode.FNextSibling));
   Dec(FCount);
end;

procedure TElement.TElementChildrenNodeList.ReplaceAt(const NewNode: TNode; const Position: Integer);
var
   OldNode: TNode;
begin
   Assert(Position >= 0);
   Assert(Position < FCount);
   Assert(not Assigned(NewNode.FParentNode));
   Assert(not Assigned(NewNode.FPreviousSibling));
   Assert(not Assigned(NewNode.FNextSibling));
   if ((Position > 0) and (Position < FCount-1)) then
      EnsureMaintainingArray(0);
   if (FMaintainingArray) then
   begin
      Assert(FStart+FCount <= High(FNodes));
      Assert(FStart+Position <= High(FNodes));
      OldNode := FNodes[FStart+Position];
      FNodes[FStart+Position] := NewNode;
   end
   else
   begin
      Assert((Position = 0) or (Position = FCount-1));
      if (Position = 0) then
      begin
         OldNode := FFirstNode;
         FFirstNode := NewNode;
      end;
      if (Position = FCount-1) then
      begin
         OldNode := FLastNode;
         FLastNode := NewNode;
      end;
      Assert(Assigned(OldNode));
   end;
   NewNode.FPreviousSibling := OldNode.FPreviousSibling;
   if (Assigned(NewNode.FPreviousSibling)) then
   begin
      Assert(NewNode.FPreviousSibling.FNextSibling = OldNode);
      NewNode.FPreviousSibling.FNextSibling := NewNode;
      OldNode.FPreviousSibling := nil;
   end;
   NewNode.FNextSibling := OldNode.FNextSibling;
   if (Assigned(NewNode.FNextSibling)) then
   begin
      Assert(NewNode.FNextSibling.FPreviousSibling = OldNode);
      NewNode.FNextSibling.FPreviousSibling := NewNode;
      OldNode.FNextSibling := nil;
   end;
   NewNode.FParentNode := FOwnerElement;
   OldNode.FParentNode := nil;
   Assert(not Assigned(OldNode.FParentNode));
   Assert(not Assigned(OldNode.FPreviousSibling));
   Assert(not Assigned(OldNode.FNextSibling));
   Assert(Assigned(NewNode.FParentNode));
end;

procedure TElement.TElementChildrenNodeList.InsertBefore(const NewNode: TNode; const Child: TNode);
var
   Position: Integer;
begin
   Assert(Assigned(NewNode));
   Assert(not Assigned(NewNode.FParentNode));
   Assert(not Assigned(NewNode.FPreviousSibling));
   Assert(not Assigned(NewNode.FNextSibling));
   if (not Assigned(Child)) then
   begin
      AppendChild(NewNode);
   end
   else
   if (FMaintainingArray) then
   begin
      Assert(Child.FParentNode = FOwnerElement);
      Position := FStart;
      while (FNodes[Position] <> Child) do
      begin
         Inc(Position);
         Assert(Position < FStart+FCount);
      end;
      Assert(Position >= FStart);
      InsertAt(NewNode, Position-FStart); // $R-
   end
   else
   begin
      Assert(Child.FParentNode = FOwnerElement);
      NewNode.FPreviousSibling := Child.FPreviousSibling;
      if (Assigned(NewNode.FPreviousSibling)) then
         NewNode.FPreviousSibling.FNextSibling := NewNode;
      NewNode.FNextSibling := Child;
      Child.FPreviousSibling := NewNode;
      NewNode.FParentNode := FOwnerElement;
      if (FFirstNode = Child) then
         FFirstNode := NewNode;
      Inc(FCount);
   end;
   Assert(Assigned(NewNode.FParentNode));
end;

procedure TElement.TElementChildrenNodeList.AppendChild(const NewNode: TNode);
begin
   Assert(Assigned(NewNode));
   Assert(not Assigned(NewNode.FParentNode));
   Assert(not Assigned(NewNode.FPreviousSibling));
   Assert(not Assigned(NewNode.FNextSibling));
   if (FMaintainingArray) then
   begin
      InsertAt(NewNode, FCount);
   end
   else
   begin
      NewNode.FPreviousSibling := FLastNode;
      if (Assigned(NewNode.FPreviousSibling)) then
         NewNode.FPreviousSibling.FNextSibling := NewNode;
      NewNode.FNextSibling := nil;
      NewNode.FParentNode := FOwnerElement;
      if (FCount = 0) then
      begin
         Assert(not Assigned(FFirstNode));
         Assert(not Assigned(FLastNode));
         FFirstNode := NewNode;
      end
      else
      begin
         Assert(FCount > 0);
         Assert(Assigned(FFirstNode));
         Assert(Assigned(FLastNode));
      end;
      FLastNode := NewNode;
      Inc(FCount);
   end;
   Assert(Assigned(NewNode.FParentNode));
end;

procedure TElement.TElementChildrenNodeList.ReplaceChild(const NewNode: TNode; const OldNode: TNode);
var
   Position: Integer;
begin
   Assert(Assigned(NewNode));
   Assert(Assigned(OldNode));
   Assert(not Assigned(NewNode.FParentNode));
   Assert(not Assigned(NewNode.FPreviousSibling));
   Assert(not Assigned(NewNode.FNextSibling));
   Assert(OldNode.FParentNode = FOwnerElement);
   if (FMaintainingArray) then
   begin
      Position := FStart;
      while (FNodes[Position] <> OldNode) do
      begin
         Inc(Position);
         Assert(Position < FStart+FCount);
      end;
      Assert(Position >= FStart);
      ReplaceAt(NewNode, Position-FStart); // $R-
   end
   else
   begin
      if (OldNode = FFirstNode) then
      begin
         Assert(not Assigned(OldNode.FPreviousSibling));
         FFirstNode := NewNode;
      end;
      if (OldNode = FLastNode) then
      begin
         Assert(not Assigned(OldNode.FNextSibling));
         FLastNode := NewNode;
      end;
      NewNode.FPreviousSibling := OldNode.FPreviousSibling;
      if (Assigned(NewNode.FPreviousSibling)) then
      begin
         Assert(NewNode.FPreviousSibling.FNextSibling = OldNode);
         NewNode.FPreviousSibling.FNextSibling := NewNode;
         OldNode.FPreviousSibling := nil;
      end;
      NewNode.FNextSibling := OldNode.FNextSibling;
      if (Assigned(NewNode.FNextSibling)) then
      begin
         Assert(NewNode.FNextSibling.FPreviousSibling = OldNode);
         NewNode.FNextSibling.FPreviousSibling := NewNode;
         OldNode.FNextSibling := nil;
      end;
      NewNode.FParentNode := FOwnerElement;
      OldNode.FParentNode := nil;
   end;
   Assert(not Assigned(OldNode.FParentNode));
   Assert(not Assigned(OldNode.FPreviousSibling));
   Assert(not Assigned(OldNode.FNextSibling));
   Assert(Assigned(NewNode.FParentNode));
end;

procedure TElement.TElementChildrenNodeList.ReplaceChildWithChildren(const OldNode: TElement);
var
   Position: Integer;
   Node: TNode;
begin
   Assert(Assigned(OldNode));
   Assert(OldNode.FParentNode = FOwnerElement);
   if ((not Assigned(OldNode.FChildNodes)) or (OldNode.FChildNodes.FCount = 0)) then
   begin
      RemoveChild(OldNode);
      exit;
   end;
   if (FMaintainingArray) then
   begin
      Position := FStart;
      while (FNodes[Position] <> OldNode) do
      begin
         Inc(Position);
         Assert(Position < FStart+FCount);
      end;
      Assert(Position >= FStart);
      // XXX if OldNode has more than one child:
         // XXX update the array length to accomodate the new children
         // XXX move the old children up
      // XXX place the new children in
      XXX;
   end;
   Dec(FCount);
   Inc(FCount, OldNode.FChildNodes.FCount);
   if (OldNode = FFirstNode) then
   begin
      Assert(not Assigned(OldNode.FPreviousSibling));
      FFirstNode := OldNode.FirstChild;
   end
   else
   begin
      Assert(Assigned(OldNode.FPreviousSibling));
      OldNode.FPreviousSibling.FNextSibling := OldNode.FirstChild;
      OldNode.FirstChild.FPreviousSibling := OldNode.FPreviousSibling;
      OldNode.FPreviousSibling := nil;
   end;
   if (OldNode = FLastNode) then
   begin
      Assert(not Assigned(OldNode.FNextSibling));
      FLastNode := OldNode.LastChild;
   end
   else
   begin
      Assert(Assigned(OldNode.FNextSibling));
      OldNode.FNextSibling.FPreviousSibling := OldNode.LastChild;
      OldNode.LastChild.FNextSibling := OldNode.FNextSibling;
      OldNode.FNextSibling := nil;
   end;
   Node := OldNode.FirstChild;
   repeat
      Node.FParentNode := FOwnerElement;
      Node := Node.FNextSibling;
   until Node = OldNode.FNextSibling;

   {$IFOPT C+}
   Position := 0;
   Node := FFirstNode;
   while (Assigned(Node)) do
   begin
      Inc(Position);
      Node := Node.FNextSibling
   end;
   Assert(Position = FCount);
   Position := 0;
   Node := FLastNode;
   while (Assigned(Node)) do
   begin
      Inc(Position);
      Node := Node.FPreviousSibling;
   end;
   Assert(Position = FCount);
   {$ENDIF}

   OldNode.FChildNodes.FMaintainingArray := False;
   OldNode.FChildNodes.FCount := 0;
   OldNode.FChildNodes.FStart := 0;
   SetLength(OldNode.FChildNodes.FNodes, 0);
   OldNode.FChildNodes.FFirstNode := nil;
   OldNode.FChildNodes.FLastNode := nil;
   OldNode.FParentNode := nil;
end;

procedure TElement.TElementChildrenNodeList.RemoveChild(const OldNode: TNode);
var
   Position: Integer;
begin
   Assert(OldNode.FParentNode = FOwnerElement);
   if (FMaintainingArray) then
   begin
      Position := FStart;
      while (FNodes[Position] <> OldNode) do
      begin
         Inc(Position);
         Assert(Position < FStart+FCount);
      end;
      Assert(Position >= FStart);
      RemoveAt(Position-FStart); // $R-
   end
   else
   begin
      if (OldNode = FFirstNode) then
      begin
         Assert(not Assigned(OldNode.FPreviousSibling));
         FFirstNode := OldNode.FNextSibling;
      end
      else
      begin
         Assert(Assigned(OldNode.FPreviousSibling));
         OldNode.FPreviousSibling.FNextSibling := OldNode.FNextSibling;
      end;
      if (OldNode = FLastNode) then
      begin
         Assert(not Assigned(OldNode.FNextSibling));
         FLastNode := OldNode.FPreviousSibling;
      end
      else
      begin
         Assert(Assigned(OldNode.FNextSibling));
         OldNode.FNextSibling.FPreviousSibling := OldNode.FPreviousSibling;
      end;
      OldNode.FPreviousSibling := nil;
      OldNode.FNextSibling := nil;
      OldNode.FParentNode := nil;
      Dec(FCount);
   end;
   Assert(not Assigned(OldNode.FParentNode));
   Assert(not Assigned(OldNode.FPreviousSibling));
   Assert(not Assigned(OldNode.FNextSibling));
end;

function TElement.TElementChildrenNodeList.GetLength(): Integer;
begin
   Result := FCount;
end;

function TElement.TElementChildrenNodeList.GetItem(const Index: Integer): TNode;
begin
   Assert(FCount > 0);
   Assert(Index >= 0);
   Assert(Index < FCount);
   if ((Index > 0) and (Index < FCount-1)) then
      EnsureMaintainingArray(0);
   if (FMaintainingArray) then
      Result := FNodes[Index]
   else
   if (Index = 0) then
      Result := FFirstNode
   else
   begin
      Assert(Index = FCount-1);
      Result := FLastNode;
   end;
   Assert(Assigned(Result));
end;

function TElement.TElementChildrenNodeList.GetFirstChild(): TNode;
begin
   Assert((not FMaintainingArray) or (not Assigned(FFirstNode)));
   if (FMaintainingArray and (FCount > 0)) then
   begin
      Result := FNodes[FStart]
   end
   else
   begin
      Result := FFirstNode;
      Assert(Assigned(Result) = (FCount > 0));
   end;
end;

function TElement.TElementChildrenNodeList.GetLastChild(): TNode;
begin
   Assert((not FMaintainingArray) or (not Assigned(FLastNode)));
   if (FMaintainingArray and (FCount > 0)) then
   begin
      Result := FNodes[FStart+FCount-1]
   end
   else
   begin
      Result := FLastNode;
      Assert(Assigned(Result) = (FCount > 0));
   end;
end;


constructor TElement.Create(const NewNamespaceURL: TCanonicalString; const NewLocalName: TCanonicalString; const NewAttributes: TAttributeHashTable; const NewChildren: array of TNode; const NewProperties: TElementProperties);
var
   Child: TNode;
begin
   inherited Create();
   FNamespaceURL := NewNamespaceURL;
   FLocalName := NewLocalName;
   FAttributes := NewAttributes;
   for Child in NewChildren do // http://bugs.freepascal.org/view.php?id=25703
      AppendChild(Child);
   FProperties := NewProperties;
end;

constructor TElement.Create(const NewNamespaceURL, NewLocalName: TCanonicalString; const NewAttributes: TAttributeHashTable; const NewChildren: array of TNode);
begin
   Create(NewNamespaceURL, NewLocalName, NewAttributes, NewChildren, 0);
end;

constructor TElement.Create(const NewNamespaceURL, NewLocalName: TCanonicalString; const NewAttributes: TAttributeHashTable = nil);
begin
   Create(NewNamespaceURL, NewLocalName, NewAttributes, [], 0);
end;

constructor TElement.Create(const NewNamespaceURL, NewLocalName: TCanonicalString; const NewChildren: array of TNode);
begin
   Create(NewNamespaceURL, NewLocalName, nil, NewChildren, 0);
end;

{$IFOPT C+}
procedure TElement.AfterConstruction();
begin
   inherited;
   if (FNamespaceURL.AsString = 'http://www.w3.org/1999/xhtml') then
      Assert(FProperties <> 0);
end;
{$ENDIF}

//{$DEFINE VERBOSEDESTROY}
destructor TElement.Destroy();
var
   Current, Next: TNode;
begin
   {$IFDEF DEBUG} try {$ENDIF}
{$IFDEF VERBOSEDESTROY} Writeln(ClassName, '.Destroy()'); {$ENDIF}
   {$IFOPT C+} FDestroying := True; {$ENDIF}
   if (Assigned(FChildNodes)) then
   begin
      if (FChildNodes.Length > 0) then
      begin
         Current := FChildNodes.FirstChild;
         while (Assigned(Current)) do
         begin
{$IFDEF VERBOSEDESTROY} Writeln('  ', ClassName, '.Destroy(): Current is: ', PtrUInt(Current)); {$ENDIF}
{$IFDEF VERBOSEDESTROY} if (Current is TText) then Writeln('    Text: "', (Current as TText).Data.AsString, '"'); {$ENDIF}
{$IFDEF VERBOSEDESTROY} Writeln('  ', ClassName, '.Destroy(): Going to read Current.FNextSibling'); {$ENDIF}
            Next := Current.FNextSibling;
{$IFDEF VERBOSEDESTROY} Writeln('  ', ClassName, '.Destroy(): NextSibling is: ', PtrUInt(Next)); {$ENDIF}
{$IFDEF VERBOSEDESTROY} Writeln('  ', ClassName, '.Destroy(): Current.ClassName is: ', Current.ClassName); {$ENDIF}
            Current.Free();
{$IFDEF VERBOSEDESTROY} Writeln('  ', ClassName, '.Destroy(): Current gone'); {$ENDIF}
            Current := Next;
         end;
      end;
{$IFDEF VERBOSEDESTROY} Writeln('  ', ClassName, '.Destroy(): FChildNodes'); {$ENDIF}
      FChildNodes.Free();
   end;
{$IFDEF VERBOSEDESTROY} Writeln('  ', ClassName, '.Destroy(): Attributes'); {$ENDIF}
   if (Assigned(FAttributes)) then
      FAttributes.Free();
   {$IFDEF DEBUG}
   except
      ReportCurrentException();
   end;
   {$ENDIF}
   inherited;
end;

procedure TElement.AddProperty(const NewProperty: TElementProperties);
begin
   Assert(not HasProperties(NewProperty));
   FProperties := FProperties or NewProperty;
end;

function TElement.HasProperties(const TestProperties: TElementProperties): Boolean;
begin
   Result := (FProperties and TestProperties) = TestProperties;
end;

function TElement.HasSomeProperties(const TestProperties: TElementProperties): Boolean;
begin
   Result := (FProperties and TestProperties) <> 0;
end;

function TElement.IsIdentity(const TestNamespaceURL, TestLocalName: TCanonicalString): Boolean;
begin
   Result := (FNamespaceURL = TestNamespaceURL) and (FLocalName = TestLocalName);
end;

procedure TElement.EnsureChildNodes();
begin
   if (not Assigned(FChildNodes)) then
      FChildNodes := TElementChildrenNodeList.Create(Self);
end;

procedure TElement.EnsureAttributes();
begin
   if (not Assigned(FAttributes)) then
      FAttributes := TAttributeHashTable.Create(1);
end;

procedure TElement.SwapChildNodes(var OtherElement: TElement);
var
   TemporaryChildNodes: TElementChildrenNodeList;
   NextNode: TNode;
begin
   Assert(Assigned(Self));
   Assert(Assigned(OtherElement));
   TemporaryChildNodes := OtherElement.FChildNodes;
   OtherElement.FChildNodes := FChildNodes;
   FChildNodes := TemporaryChildNodes;
   if (Assigned(FChildNodes)) then
   begin
      FChildNodes.FOwnerElement := Self;
      NextNode := FChildNodes.FirstChild;
      while (Assigned(NextNode)) do
      begin
         NextNode.FParentNode := Self;
         NextNode := NextNode.NextSibling;
      end;
   end;
   if (Assigned(OtherElement.FChildNodes)) then
   begin
      OtherElement.FChildNodes.FOwnerElement := OtherElement;
      NextNode := OtherElement.FChildNodes.FirstChild;
      while (Assigned(NextNode)) do
      begin
         NextNode.FParentNode := OtherElement;
         NextNode := NextNode.NextSibling;
      end;
   end;
end;

procedure TElement.SwapAttributes(var OtherElement: TElement);
var
   TempAttributes: TAttributeHashTable;
begin
   Assert(Assigned(Self));
   Assert(Assigned(OtherElement));
   TempAttributes := FAttributes;
   FAttributes := OtherElement.FAttributes;
   OtherElement.FAttributes := TempAttributes;
end;

function TElement.FindFirstMatchingChild(const TestNamespaceURL, TestLocalName: TCanonicalString): TElement;
var
   Current: TNode;
begin
   Current := FirstChild;
   while (Assigned(Current) and ((not (Current is TElement)) or (not (Current as TElement).IsIdentity(TestNamespaceURL, TestLocalName)))) do
      Current := Current.NextSibling;
   if (Current is TElement) then
      Result := Current as TElement
   else
      Result := nil;
end;

function TElement.HasChildNodes(): Boolean;
begin
   Result := Assigned(FChildNodes) and (FChildNodes.Length > 0);
end;

function TElement.GetChildNodes(): TNodeList;
begin
   EnsureChildNodes();
   Result := FChildNodes;
end;

function TElement.GetFirstChild(): TNode;
begin
   if (Assigned(FChildNodes)) then
   begin
      Result := FChildNodes.FirstChild;
   end
   else
      Result := nil;
end;

function TElement.GetLastChild(): TNode;
begin
   if (Assigned(FChildNodes)) then
   begin
      Result := FChildNodes.LastChild;
   end
   else
      Result := nil;
end;

function TElement.InsertBefore(const NewNode: TNode; const Child: TNode): TNode;
begin
   EnsureChildNodes();
   FChildNodes.InsertBefore(NewNode, Child);
   Result := NewNode;
end;

function TElement.AppendChild(const NewNode: TNode): TNode;
begin
   EnsureChildNodes();
   FChildNodes.AppendChild(NewNode);
   Result := NewNode;
end;

function TElement.ReplaceChild(const NewNode: TNode; const OldNode: TNode): TNode;
begin
   Assert(Assigned(OldNode));
   Assert(OldNode.ParentNode = Self);
   Assert(Assigned(FChildNodes));
   FChildNodes.ReplaceChild(NewNode, OldNode);
   Result := NewNode;
end;

procedure TElement.ReplaceChildWithChildren(const OldNode: TElement);
begin
   Assert(Assigned(OldNode));
   Assert(OldNode.ParentNode = Self);
   Assert(Assigned(FChildNodes));
   FChildNodes.ReplaceChildWithChildren(OldNode);
end;

function TElement.RemoveChild(const OldNode: TNode): TNode;
begin
   Assert(Assigned(FChildNodes));
   Assert(OldNode.FParentNode = Self);
   FChildNodes.RemoveChild(OldNode);
   Result := OldNode;
end;

function TElement.HasAttribute(const Name: UTF8String): Boolean;
begin
   if (not Assigned(FAttributes)) then
      Result := False
   else
      Result := FAttributes.Has(Name);
end;

function TElement.GetAttribute(const Name: UTF8String): CutRope;
begin
   if (not Assigned(FAttributes)) then
      Result := Default(CutRope)
   else
      Result := FAttributes[Name].ExtractAll();
end;

procedure TElement.SetAttribute(const Name: UTF8String; const Value: UTF8String);
var
   R: Rope;
begin
   EnsureAttributes();
   R := Default(Rope);
   if (Value <> '') then
   begin
      {$IFOPT C+} AssertStringIsConstant(Value); {$ENDIF}
      R.Append(@Value);
   end;
   FAttributes[Name] := R;
end;

procedure TElement.SetAttributeDestructively(const Name: UTF8String; var Value: TDOMString);
begin
   EnsureAttributes();
   FAttributes[Name] := Value;
   Value := Default(TDOMString);
end;

procedure TElement.SetAttributeDestructively(const Name: UTF8String; var Value: TCutDOMString);
var
   Scratch: TDOMString;
begin
   EnsureAttributes();
   Scratch := Default(TDOMString);
   Scratch.AppendDestructively(Value); // http://bugs.freepascal.org/view.php?id=26403
   FAttributes[Name] := Scratch;
end;

procedure TElement.RemoveAttribute(const Name: UTF8String);
begin
   if (Assigned(FAttributes)) then
      FAttributes.Remove(Name);
end;

function TElement.CloneNode(const Deep: Boolean = False): TElement;
var
   NewAttributes: TElement.TAttributeHashTable;
   Current: TNode;
begin
   if (Assigned(FAttributes)) then
      NewAttributes := FAttributes.Clone() as TElement.TAttributeHashTable
   else
      NewAttributes := nil;
   Result := TElementClass(ClassType).Create(FNamespaceURL, FLocalName, NewAttributes, [], FProperties);
   if (Deep and Assigned(FChildNodes) and (FChildNodes.Length > 0)) then
   begin
      Current := FChildNodes.FirstChild;
      while (Assigned(Current)) do
      begin
         Result.AppendChild(Current.CloneNode(True));
         Current := Current.FNextSibling;
      end;
   end;
end;


constructor TCharacterData.Create(const NewData: UTF8String);
begin
   inherited Create();
   Assert(FData.IsEmpty);
   if (NewData <> '') then
   begin
      {$IFOPT C+} AssertStringIsConstant(NewData); {$ENDIF}
      FData.Append(@NewData);
   end;
end;

constructor TCharacterData.CreateDestructively(var NewData: TDOMString);
begin
   inherited Create();
   Assert(FData.IsEmpty);
   FData.AppendDestructively(NewData); // http://bugs.freepascal.org/view.php?id=26403
end;

constructor TCharacterData.CreateDestructively(var NewData: TCutDOMString);
begin
   inherited Create();
   Assert(FData.IsEmpty);
   FData.AppendDestructively(NewData); // http://bugs.freepascal.org/view.php?id=26403
end;

procedure TCharacterData.AppendDataDestructively(var Data: TDOMString);
begin
   FData.AppendDestructively(Data); // http://bugs.freepascal.org/view.php?id=26403
end;

procedure TCharacterData.AppendDataDestructively(var Data: TCutDOMString);
begin
   FData.AppendDestructively(Data); // http://bugs.freepascal.org/view.php?id=26403
end;

function TText.CloneNode(const Deep: Boolean = False): TText;
var
   DataCopy: CutRope;
begin
   DataCopy := FData.ExtractAll();
   Result := TText.CreateDestructively(DataCopy);
end;

function TComment.CloneNode(const Deep: Boolean = False): TComment;
var
   DataCopy: CutRope;
begin
   DataCopy := FData.ExtractAll();
   Result := TComment.CreateDestructively(DataCopy);
end;


function WalkToNext(var Current: TNode; const Top: TNode; const WalkOutCallback: TWalkOutCallback): Boolean;
begin
   Assert(Assigned(Current));
   if (Current.HasChildNodes()) then
   begin
      if (Current is TElement) then
      begin
         Current := TElement(Current).FirstChild;
      end
      else
      begin
         Assert(Current is TDocument);
         Current := TDocument(Current).FirstChild;
      end;
      Result := True;
   end
   else
   begin
      Result := WalkToNextSkippingChildren(Current, Top, WalkOutCallback);
   end;
end;

function WalkToNextSkippingChildren(var Current: TNode; const Top: TNode; const WalkOutCallback: TWalkOutCallback): Boolean;

   procedure WalkOut(var Node: TNode); inline;
   begin
      if ((Node is TElement) and (Assigned(WalkOutCallback))) then
         WalkOutCallback(TElement(Node));
   end;

var
   Old: TNode;
begin
   Assert(Assigned(Current));
   if (Current = Top) then
   begin
      WalkOut(Current);
      Result := False;
      exit;
   end
   else
   if (not Assigned(Current.NextSibling)) then
   begin
      repeat
         Assert(Assigned(Current));
         Old := Current;
         Current := Current.ParentNode;
         WalkOut(Old);
         Assert(Assigned(Current));
         if (Current = Top) then
         begin
            WalkOut(Current);
            Result := False;
            exit;
         end;
         Assert(Assigned(Current));
      until Assigned(Current.NextSibling);
   end;
   Old := Current;
   Assert(Assigned(Current));
   Current := Current.NextSibling;
   WalkOut(Old);
   Result := True;
end;

procedure TStringStore.Push(constref Value: UTF8String);
begin
   SetLength(FStrings, Length(FStrings) + 1);
   FStrings[High(FStrings)] := Value;
end;

end.

