{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit storable;

interface

uses
   hashtable, sysutils;

type
   TStorable = class;
   StorableClass = class of TStorable;

   PPendingFixupItem = ^TPendingFixupItem;
   TPendingFixupItem = record
      Destination: PPointer;
      ObjectID: PtrUInt;
      Next: PPendingFixupItem;
   end;

   TReferenceFixerProcedure = procedure (const Data: Pointer; const Value: Pointer) of object;
   TMethodFixerProcedure = procedure (const Data: Pointer; const Value: TMethod) of object;
   TFinalFixerProcedure = procedure () of object;
   PMethod = ^TMethod;

   PPendingReferenceFixerItem = ^TPendingReferenceFixerItem;
   TPendingReferenceFixerItem = record
      Fixer: TReferenceFixerProcedure;
      Data, Value: Pointer;
      Next: PPendingReferenceFixerItem;
   end;

   PPendingMethodFixerItem = ^TPendingMethodFixerItem;
   TPendingMethodFixerItem = record
      Fixer: TMethodFixerProcedure;
      Data: Pointer;
      Value: TMethod;
      Next: PPendingMethodFixerItem;
   end;

   PFinalFixerItem = ^TFinalFixerItem;
   TFinalFixerItem = record
      Fixer: TFinalFixerProcedure;
      Next: PFinalFixerItem;
   end;

   TFixupHashTable = specialize THashTable<PtrUInt, Pointer>;

   TReadStream = class
    protected
     type
      PMethodFixupData = ^TMethodFixupData;
      TMethodFixupData = record
        Field: PMethod;
        MethodName: AnsiString;
      end;
     var
      FActive: Boolean;
      FObjectsRead: TFixupHashTable;
      FPendingFixups: PPendingFixupItem;
      FPendingReferenceFixers: PPendingReferenceFixerItem;
      FPendingMethodFixers: PPendingMethodFixerItem;
      FFinalFixers: PFinalFixerItem;
      FInput: File;
      FVersion: Cardinal;
      procedure VerifyFieldType(FieldType: Byte);
      function GetFilename(): AnsiString;
      procedure FixupMethod(const Data: Pointer; const Value: Pointer);
    public
      constructor Create(var AInput: File);
      destructor Destroy; override;
      procedure DisableChecks();
      function ReadByte: Byte;
      function ReadBoolean: Boolean;
      function ReadCardinal: Cardinal;
      function ReadInteger: Integer;
      function ReadPtrUInt: PtrUInt;
      function ReadDouble: Double;
      function ReadAnsiString: RawByteString;
      procedure ReadByteStream(var Buffer; Length: Cardinal);
      function ReadClass: StorableClass;
      function ReadObject: TStorable;
      procedure ReadReference(Destination: PPointer);
      procedure ReadReference(Fixer: TReferenceFixerProcedure; const Data: Pointer);
      procedure ReadMethod(Destination: PMethod);
      procedure ReadMethod(Fixer: TMethodFixerProcedure; const Data: Pointer);
      procedure AddFinalFixer(Fixer: TFinalFixerProcedure);
      procedure VerifySentinel();
      procedure FixupReferences();
      property Version: Cardinal read FVersion;
      property Filename: AnsiString read GetFilename;
   end;

   TWriteStream = class
    protected
      FOutput: File;
      procedure WriteFieldType(FieldType: Byte);
    public
      constructor Create(var AOutput: File; Version: Cardinal);
      destructor Destroy; override;
      procedure WriteByte(Value: Byte);
      procedure WriteBoolean(Value: Boolean);
      procedure WriteCardinal(Value: Cardinal);
      procedure WriteInteger(Value: Integer);
      procedure WritePtrUInt(Value: PtrUInt);
      procedure WriteDouble(Value: Double);
      procedure WriteAnsiString(Value: RawByteString);
      procedure WriteByteStream(var Buffer; Length: Cardinal);
      procedure WriteClass(Value: StorableClass);
      procedure WriteObject(Value: TStorable);
      procedure WriteReference(Value: Pointer);
      procedure WriteMethod(Value: TMethod);
      procedure WriteSentinel();
   end;

   TStorable = class abstract
    protected
      {$IFOPT C+} FDebugCalledInherited: Boolean; {$ENDIF}
    public
      constructor Create();
      constructor Read(Stream: TReadStream); virtual;
      procedure Write(Stream: TWriteStream); virtual;
   end;

   EStorageError = class(Exception)
   end;

procedure RegisterStorableClass(AClass: StorableClass);
procedure RegisterStorableClassSynonym(AClassName: AnsiString; RestoreClass: StorableClass);
function GetRegisteredClass(AClassName: AnsiString): StorableClass;

procedure StoreObjectToFile(FileName: AnsiString; Value: TStorable; Version: Cardinal);
function ReadObjectFromFile(FileName: AnsiString): TStorable;

implementation

uses
   exceptions;

type
   TStorableClassesHashTable = specialize THashTable<AnsiString, StorableClass>;

var
   RegisteredClasses: TStorableClassesHashTable;

const { values with bit patterns unlikely to be found in typical data (so e.g. not $00, $01, $FF) }
   btSignature  = $AA;
   btStream     = $BB;
   btStreamEnd  = $DD;

   btBoolean    = $60;
   btCardinal   = $61;
   btInteger    = $62;
   btPtrUInt    = $63;
   btDouble     = $63;
   btAnsiString = $65;
   btByteStream = $66;

   btClass      = $80;
   btReference  = $81;
   btMethod     = $82;

   btObject     = $51;
   btObjectData = $52;
   btObjectEnd  = $54;
   btSentinel   = $58;

const
   ciNoClass = 'nil';

procedure RegisterStorableClass(AClass: StorableClass);
begin
   Assert(not RegisteredClasses.Has(AClass.ClassName), AClass.ClassName + ' registered twice');
   RegisteredClasses[AClass.ClassName] := AClass;
end;

procedure RegisterStorableClassSynonym(AClassName: AnsiString; RestoreClass: StorableClass);
begin
   Assert(not RegisteredClasses.Has(AClassName), AClassName + ' registered twice');
   RegisteredClasses[AClassName] := RestoreClass;
end;

function GetRegisteredClass(AClassName: AnsiString): StorableClass;
begin
   Result := RegisteredClasses[AClassName];
end;


procedure StoreObjectToFile(FileName: AnsiString; Value: TStorable; Version: Cardinal);
var
   F, OldF: File;
   Stream: TWriteStream;
begin
   Assign(F, FileName + '.$$$');
   Stream := nil;
   try
      Rewrite(F, 1);
      Stream := TWriteStream.Create(F, Version);
      Stream.WriteObject(Value);
   finally
      Stream.Free();
      Close(F);
   end;
   Assign(OldF, FileName);
   if (FileExists(FileName)) then
      Erase(OldF);
   Rename(F, FileName);
end;

function ReadObjectFromFile(FileName: AnsiString): TStorable;
var
   F: File;
   Stream: TReadStream;
   IOResultValue: Word;
begin
   Assign(F, FileName);
   Stream := nil;
   try
      {$I-} Reset(F, 1); {$I+}
      IOResultValue := IOResult();
      if (IOResultValue = 2) then
      begin
         Result := nil;
         Exit;
      end;
      if (IOResultValue <> 0) then
         RunError(IOResultValue);
      Stream := TReadStream.Create(F);
      try
         Result := Stream.ReadObject();
         Stream.FixupReferences();
      except
         Stream.DisableChecks();
         raise;
      end;
   finally
      Stream.Free();
      Close(F);
   end;
end;


constructor TReadStream.Create(var AInput: File);
begin
   inherited Create();
   FActive := True;
   FInput := AInput;
   FObjectsRead := TFixupHashTable.Create(@PtrUIntHash32);
   VerifyFieldType(btStream);
   FVersion := ReadCardinal();
   if (ReadByte() <> SizeOf(PtrUInt)) then
      raise EStorageError.Create('Stream was written on a platform with a different pointer size.');
end;

destructor TReadStream.Destroy;
var
   NextFixup: PPendingFixupItem;
   NextReferenceFixer: PPendingReferenceFixerItem;
   NextMethodFixer: PPendingMethodFixerItem;
   NextFinalFixer: PFinalFixerItem;
begin
   if (FActive) then
      VerifyFieldType(btStreamEnd);
   while (Assigned(FPendingFixups)) do
   begin
      NextFixup := FPendingFixups^.Next;
      Dispose(FPendingFixups);
      FPendingFixups := NextFixup;
   end;
   while (Assigned(FPendingReferenceFixers)) do
   begin
      NextReferenceFixer := FPendingReferenceFixers^.Next;
      Dispose(FPendingReferenceFixers);
      FPendingReferenceFixers := NextReferenceFixer;
   end;
   while (Assigned(FPendingMethodFixers)) do
   begin
      NextMethodFixer := FPendingMethodFixers^.Next;
      Dispose(FPendingMethodFixers);
      FPendingMethodFixers := NextMethodFixer;
   end;
   while (Assigned(FFinalFixers)) do
   begin
      NextFinalFixer := FFinalFixers^.Next;
      Dispose(FFinalFixers);
      FFinalFixers := NextFinalFixer;
   end;
   FObjectsRead.Free();
   inherited;
end;

procedure TReadStream.DisableChecks();
begin
   FActive := False;
end;

procedure TReadStream.VerifyFieldType(FieldType: Byte);
var
   Signature: Byte;
begin 
   if (ReadByte() <> btSignature) then
      raise EStorageError.Create('Stream inconsistent - type signature marker not found');
   Signature := ReadByte();
   if (Signature <> FieldType) then
      raise EStorageError.Create('Stream inconsistent - expected type signature ' + IntToHex(FieldType, 2) + ' but found ' + IntToHex(Signature, 2));
end;

function TReadStream.ReadByte: Byte;
begin
   {$HINTS OFF} // The following statement is guaranteed to either set Result or throw an exception, but compiler doesn't know that
   BlockRead(FInput, Result, SizeOf(Result));
   {$HINTS ON}
end;

function TReadStream.ReadBoolean: Boolean;
begin
   VerifyFieldType(btBoolean);
   Result := ReadByte() = $01;
end;

function TReadStream.ReadCardinal: Cardinal;
begin
   VerifyFieldType(btCardinal);
   {$HINTS OFF} // The following statement is guaranteed to either set Result or throw an exception, but compiler doesn't know that
   BlockRead(FInput, Result, SizeOf(Result));
   {$HINTS ON}
end;

function TReadStream.ReadInteger: Integer;
begin
   VerifyFieldType(btInteger);
   {$HINTS OFF} // The following statement is guaranteed to either set Result or throw an exception, but compiler doesn't know that
   BlockRead(FInput, Result, SizeOf(Result));
   {$HINTS ON}
end;

function TReadStream.ReadPtrUInt: PtrUInt;
begin
   VerifyFieldType(btPtrUInt);
   {$HINTS OFF} // The following statement is guaranteed to either set Result or throw an exception, but compiler doesn't know that
   BlockRead(FInput, Result, SizeOf(Result));
   {$HINTS ON}
end;

function TReadStream.ReadDouble: Double;
begin
   VerifyFieldType(btDouble);
   {$HINTS OFF} // The following statement is guaranteed to either set Result or throw an exception, but compiler doesn't know that
   BlockRead(FInput, Result, SizeOf(Result));
   {$HINTS ON}
end;

function TReadStream.ReadAnsiString: RawByteString;
var
   Length: Cardinal;
begin
   VerifyFieldType(btAnsiString);
   Result := '';
   Length := ReadCardinal();
   // XXX should also read dynamic code page
   if (Length > 0) then
   begin
      SetLength(Result, Length);
      BlockRead(FInput, Result[1], Length);
   end;
end;

procedure TReadStream.ReadByteStream(var Buffer; Length: Cardinal);
begin
   VerifyFieldType(btByteStream);
   BlockRead(FInput, Buffer, Length);
end;

function TReadStream.ReadClass: StorableClass;
var
   RestoreClassName: AnsiString;
begin
   VerifyFieldType(btClass);
   RestoreClassName := ReadAnsiString();
   if (RestoreClassName <> ciNoClass) then
   begin
      Assert(RegisteredClasses.Has(RestoreClassName), 'Tried to restore unregistered class ' + RestoreClassName);
      Result := GetRegisteredClass(RestoreClassName);
   end
   else
   begin
      Result := nil;
   end;
end;

function TReadStream.ReadObject: TStorable;
var
   ClassValue: StorableClass;
   ObjectID: PtrUInt;
begin
   VerifyFieldType(btObject);
   ClassValue := ReadClass();
   if (not Assigned(ClassValue)) then
   begin
      Result := nil;
   end
   else
   begin
      ObjectID := ReadPtrUInt();
      Result := ClassValue.Read(Self);
      {$IFOPT C+} Assert(Result.FDebugCalledInherited); {$ENDIF} // it's initialised to false when the object is created
      FObjectsRead.Add(ObjectID, Result);
   end;
   VerifyFieldType(btObjectEnd);
end;

procedure TReadStream.ReadReference(Destination: PPointer);
var
   Item: PPendingFixupItem;
   ObjectID: PtrUInt;
begin
   VerifyFieldType(btReference);
   ObjectID := ReadPtrUInt();
   {$HINTS OFF} // Compiler thinks casting 'nil' to PtrUInt might be non-portable
   if (ObjectID <> PtrUInt(nil)) then
   {$HINTS ON}
   begin
      New(Item);
      Item^.Destination := Destination;
      Item^.ObjectID := ObjectID;
      Item^.Next := FPendingFixups;
      FPendingFixups := Item;
   end
   else
   begin
      Destination^ := nil;
   end;
end;

procedure TReadStream.ReadReference(Fixer: TReferenceFixerProcedure; const Data: Pointer);
var
   Item: PPendingReferenceFixerItem;
begin
   New(Item);
   Item^.Fixer := Fixer;
   Item^.Data := Data;
   ReadReference(@(Item^.Value));
   Item^.Next := FPendingReferenceFixers;
   FPendingReferenceFixers := Item;
end;

procedure TReadStream.ReadMethod(Destination: PMethod);
var
   MethodData: PMethodFixupData;
begin
   Assert(Assigned(Destination));
   VerifyFieldType(btMethod);
   New(MethodData);
   MethodData^.Field := Destination;
   MethodData^.MethodName := ReadAnsiString();
   ReadReference(@FixupMethod, MethodData);
end;

procedure TReadStream.FixupMethod(const Data: Pointer; const Value: Pointer);
var
   MethodData: PMethodFixupData;
   ResolvedMethodAddress: Pointer;
begin
   Assert(Assigned(Data));
   MethodData := PMethodFixupData(Data);
   Assert(Assigned(MethodData^.Field));
   if (Assigned(Value)) then
   begin
      ResolvedMethodAddress := TObject(Value).MethodAddress(MethodData^.MethodName);
      if (not Assigned(ResolvedMethodAddress)) then
         raise EStorageError.Create('Stream references non-existent method name "' + MethodData^.MethodName + '" for class ' + TObject(Value).ClassName);
      MethodData^.Field^.Data := Value;
      MethodData^.Field^.Code := ResolvedMethodAddress;
   end
   else
   begin
      if (MethodData^.MethodName <> '') then
         raise EStorageError.Create('Stream inconsistent - method name present ("'+ MethodData^.MethodName + '") but reference is missing');
      MethodData^.Field^.Data := nil;
      MethodData^.Field^.Code := nil;
   end;
   Dispose(MethodData);
end;

procedure TReadStream.ReadMethod(Fixer: TMethodFixerProcedure; const Data: Pointer);
var
   Item: PPendingMethodFixerItem;
begin
   New(Item);
   Item^.Fixer := Fixer;
   Item^.Data := Data;
   ReadMethod(@(Item^.Value));
   Item^.Next := FPendingMethodFixers;
   FPendingMethodFixers := Item;
end;

procedure TReadStream.AddFinalFixer(Fixer: TFinalFixerProcedure);
var
   Item: PFinalFixerItem;
begin
   New(Item);
   Item^.Fixer := Fixer;
   Item^.Next := FFinalFixers;
   FFinalFixers := Item;
end;

procedure TReadStream.VerifySentinel();
begin
   VerifyFieldType(btSentinel);
end;

procedure TReadStream.FixupReferences();
var
   NextFixup: PPendingFixupItem;
   NextReferenceFixer: PPendingReferenceFixerItem;
   NextMethodFixer: PPendingMethodFixerItem;
   NextFinalFixer: PFinalFixerItem;
begin
   while (Assigned(FPendingFixups)) do
   begin
      FPendingFixups^.Destination^ := FObjectsRead[FPendingFixups^.ObjectID];
      NextFixup := FPendingFixups^.Next;
      Dispose(FPendingFixups);
      FPendingFixups := NextFixup;
   end;
   while (Assigned(FPendingReferenceFixers)) do
   begin
      FPendingReferenceFixers^.Fixer(FPendingReferenceFixers^.Data, FPendingReferenceFixers^.Value);
      NextReferenceFixer := FPendingReferenceFixers^.Next;
      Dispose(FPendingReferenceFixers);
      FPendingReferenceFixers := NextReferenceFixer;
   end;
   while (Assigned(FPendingMethodFixers)) do
   begin
      FPendingMethodFixers^.Fixer(FPendingMethodFixers^.Data, FPendingMethodFixers^.Value);
      NextMethodFixer := FPendingMethodFixers^.Next;
      Dispose(FPendingMethodFixers);
      FPendingMethodFixers := NextMethodFixer;
   end;
   while (Assigned(FFinalFixers)) do
   begin
      FFinalFixers^.Fixer();
      NextFinalFixer := FFinalFixers^.Next;
      Dispose(FFinalFixers);
      FFinalFixers := NextFinalFixer;
   end;
end;

function TReadStream.GetFilename(): AnsiString;
begin
   Result := FileRec(FInput).Name;
end;


constructor TWriteStream.Create(var AOutput: File; Version: Cardinal);
begin
   inherited Create();
   FOutput := AOutput;
   WriteFieldType(btStream);
   WriteCardinal(Version);
   WriteByte(SizeOf(PtrUInt));
end;

destructor TWriteStream.Destroy;
begin
   WriteFieldType(btStreamEnd);
   inherited;
end;

procedure TWriteStream.WriteFieldType(FieldType: Byte);
begin 
   WriteByte(btSignature);
   WriteByte(FieldType);
end;

procedure TWriteStream.WriteByte(Value: Byte);
begin
   BlockWrite(FOutput, Value, SizeOf(Value));
end;

procedure TWriteStream.WriteBoolean(Value: Boolean);
begin
   WriteFieldType(btBoolean);
   if (Value) then
      WriteByte($01)
   else
      WriteByte($00);
end;

procedure TWriteStream.WriteCardinal(Value: Cardinal);
begin
   WriteFieldType(btCardinal);
   BlockWrite(FOutput, Value, SizeOf(Value));
end;

procedure TWriteStream.WriteInteger(Value: Integer);
begin
   WriteFieldType(btInteger);
   BlockWrite(FOutput, Value, SizeOf(Value));
end;

procedure TWriteStream.WritePtrUInt(Value: PtrUInt);
begin
   WriteFieldType(btPtrUInt);
   BlockWrite(FOutput, Value, SizeOf(Value));
end;

procedure TWriteStream.WriteDouble(Value: Double);
begin
   WriteFieldType(btDouble);
   BlockWrite(FOutput, Value, SizeOf(Value));
end;

procedure TWriteStream.WriteAnsiString(Value: RawByteString);
var
   Result: Cardinal;
begin
   WriteFieldType(btAnsiString);
   WriteCardinal(Length(Value));
   // XXX should also write dynamic code page
   if (Length(Value) > 0) then
   begin
      Result := 0;
      BlockWrite(FOutput, Value[1], Length(Value), Result);
      Assert(Result = Length(Value));
   end;
end;

procedure TWriteStream.WriteByteStream(var Buffer; Length: Cardinal);
begin
   WriteFieldType(btByteStream);
   BlockWrite(FOutput, Buffer, Length);
end;

procedure TWriteStream.WriteClass(Value: StorableClass);
begin
   WriteFieldType(btClass);
   if (not Assigned(Value)) then
   begin
      WriteAnsiString(ciNoClass);
   end
   else
   begin
      Assert(RegisteredClasses.Has(Value.ClassName), 'Tried to store unregistered class ' + Value.ClassName);
      WriteAnsiString(Value.ClassName);
   end;
end;

procedure TWriteStream.WriteObject(Value: TStorable);
begin
   WriteFieldType(btObject);
   if (not Assigned(Value)) then
   begin
      WriteClass(nil);
   end
   else
   begin
      WriteClass(StorableClass(Value.ClassType));
      WritePtrUInt(PtrUInt(Value));
      {$IFOPT C+} Value.FDebugCalledInherited := False; {$ENDIF}
      Value.Write(Self);
      {$IFOPT C+} Assert(Value.FDebugCalledInherited); {$ENDIF}
   end;
   WriteFieldType(btObjectEnd);
end;

procedure TWriteStream.WriteReference(Value: Pointer);
begin
   WriteFieldType(btReference);
   {$HINTS OFF} // Compiler thinks casting a pointer to PtrUInt might not be portable
   WritePtrUInt(PtrUInt(Value));
   {$HINTS ON}
end;

procedure TWriteStream.WriteMethod(Value: TMethod);
var
   ResolvedMethodName: AnsiString;
begin
   WriteFieldType(btMethod);
   if (Assigned(Value.Data)) then
   begin
      ResolvedMethodName := TObject(Value.Data).MethodName(Value.Code);
      Assert(ResolvedMethodName <> '', 'Methods to be saved by TStorable.WriteMethod() must be published');
      WriteAnsiString(ResolvedMethodName);
   end
   else
   begin
      WriteAnsiString('');
   end;
   WriteReference(Value.Data);
end;

procedure TWriteStream.WriteSentinel();
begin
   WriteFieldType(btSentinel);
end;


constructor TStorable.Create();
begin
   inherited;
   Assert(RegisteredClasses.Has(ClassName), 'Class ' + ClassName + ' has not been registered with RegisterStorableClass().');
end;

constructor TStorable.Read(Stream: TReadStream);
begin
   {$IFOPT C+} FDebugCalledInherited := True; {$ENDIF}
   Stream.VerifyFieldType(btObjectData);
end;

procedure TStorable.Write(Stream: TWriteStream);
begin
   {$IFOPT C+} FDebugCalledInherited := True; {$ENDIF}
   Stream.WriteFieldType(btObjectData);
end;


initialization
   RegisteredClasses := TStorableClassesHashTable.Create(@AnsiStringHash32);
finalization
   RegisteredClasses.Free();
end.
