{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit autostorable;

interface

uses
   storable, typinfo;

type
   {$TYPEINFO ON}
   TAutoStorable = class abstract (TStorable)
    private
      procedure FixupReferenceProperty(const PropInfo: Pointer; const Value: Pointer);
      procedure FixupMethodProperty(const PropInfo: Pointer; const Value: TMethod);
    public
      constructor Read(Stream: TReadStream); override;
      procedure Write(Stream: TWriteStream); override;
   end;
   {$TYPEINFO OFF}

implementation

uses
   {$IFDEF DEBUG} debug, {$ENDIF} exceptions;

{$IF SizeOf(Int64) < SizeOf(TClass)} {$FATAL GetOrdProp returns data too small for a pointer} {$ENDIF}

constructor TAutoStorable.Read(Stream: TReadStream);

   function GetNextPropertyName(out Name: AnsiString): Boolean;
   begin
      Name := Stream.ReadAnsiString();
      Result := Name <> '';
   end;

var
   Name, Value: AnsiString;
   AClass: TClass;
   PropInfo: PPropInfo;
   {$IFDEF DEBUG} OldHeapInfo: THeapInfo; {$ENDIF}
begin
   inherited;
   while (GetNextPropertyName(Name)) do
   begin
      {$IFDEF DEBUG} OldHeapInfo := SetHeapInfo('TAutoStorable.Read() reading ' + ClassName + '.' + Name); {$ENDIF}
      PropInfo := GetPropInfo(Self, Name);
      if (IsStoredProp(Self, PropInfo)) then
         case PropInfo^.PropType^.Kind of
          tkInteger, tkChar, tkBool: SetOrdProp(Self, PropInfo, Stream.ReadCardinal());
          tkSString, tkAString: SetStrProp(Self, PropInfo, Stream.ReadAnsiString());
          tkEnumeration: SetEnumProp(Self, PropInfo, Stream.ReadAnsiString());
          tkFloat: SetFloatProp(Self, PropInfo, Stream.ReadDouble());
          tkClass: 
            begin
               AClass := GetTypeData(PropInfo^.PropType)^.ClassType;
               if (AClass.InheritsFrom(TAutoStorable)) then
                  SetObjectProp(Self, PropInfo, Stream.ReadObject())
               else
                  Stream.ReadReference(@FixupReferenceProperty, PropInfo);
            end;
          tkClassRef:
            begin
               Assert(GetTypeData(GetTypeData(PropInfo^.PropType)^.InstanceType)^.ClassType.InheritsFrom(TStorable));
               Value := Stream.ReadAnsiString();
               if (Value = '') then
               begin
                  {$HINTS OFF} // "Conversion between ordinals and pointers is not portable" (but PtrUInt is designed for that purpose)
                  SetOrdProp(Self, PropInfo, PtrUInt(nil));
                  {$HINTS ON}
               end
               else
               begin
                  AClass := GetRegisteredClass(Value);
                  Assert(Assigned(AClass), 'Couldn''t find class ' + Value);
                  Assert(AClass.InheritsFrom(GetTypeData(GetTypeData(PropInfo^.PropType)^.InstanceType)^.ClassType));
                  SetOrdProp(Self, PropInfo, PtrUInt(AClass));
               end;
            end;
          tkMethod:
            begin
               Stream.ReadMethod(@FixupMethodProperty, PropInfo);
            end;
          else
            Assert(False, 'Property ' + PropInfo^.Name + ' uses a type (' + GetEnumName(TypeInfo(TTypeKind), Ord(PropInfo^.PropType^.Kind)) + ') that TAutoStorable does not yet support');
         end;
      {$IFDEF DEBUG} SetHeapInfo(OldHeapInfo); {$ENDIF}
   end;
end;

procedure TAutoStorable.FixupReferenceProperty(const PropInfo: Pointer; const Value: Pointer);
begin
   {$HINTS OFF} // "Conversion between ordinals and pointers is not portable" (but PtrUInt is designed for that purpose)
   SetOrdProp(Self, PPropInfo(PropInfo), PtrUInt(Value));
   {$HINTS ON}
end;

procedure TAutoStorable.FixupMethodProperty(const PropInfo: Pointer; const Value: TMethod);
begin
   SetMethodProp(Self, PPropInfo(PropInfo), Value);
end;

procedure TAutoStorable.Write(Stream: TWriteStream);
var
   OurTypeInfo: PTypeInfo;
   OurTypeData: PTypeData;
   PropertyList: PPropList;
   PropIndex: Cardinal;
   PropInfo: PPropInfo;
   AClass: TClass;
begin
   inherited;
   OurTypeInfo := ClassInfo();
   OurTypeData := GetTypeData(OurTypeInfo);
   if (OurTypeData^.PropCount > 0) then
   begin
      GetMem(PropertyList, OurTypeData^.PropCount * SizeOf(Pointer));
      GetPropInfos(OurTypeInfo, PropertyList);
      for PropIndex := 0 to OurTypeData^.PropCount-1 do
      begin
         PropInfo := PropertyList^[PropIndex];
         if (IsStoredProp(Self, PropInfo)) then
         begin
            Stream.WriteAnsiString(PropInfo^.Name);
            case PropInfo^.PropType^.Kind of
             tkInteger, tkChar, tkBool: Stream.WriteCardinal(Cardinal(GetOrdProp(Self, PropInfo)));
             tkSString, tkAString: Stream.WriteAnsiString(GetStrProp(Self, PropInfo));
             tkEnumeration: Stream.WriteAnsiString(GetEnumProp(Self, PropInfo));
             tkFloat: Stream.WriteDouble(GetFloatProp(Self, PropInfo));
             tkClass: 
               begin
                  AClass := GetTypeData(PropInfo^.PropType)^.ClassType;
                  if (AClass.InheritsFrom(TAutoStorable)) then
                     Stream.WriteObject(GetObjectProp(Self, PropInfo) as TAutoStorable)
                  else
                     Stream.WriteReference(GetObjectProp(Self, PropInfo));
               end;
             tkClassRef:
               begin
                  {$IFOPT C+}
                  AClass := GetTypeData(GetTypeData(PropInfo^.PropType)^.InstanceType)^.ClassType;
                  Assert(AClass.InheritsFrom(TStorable));
                  {$ENDIF}
                  if (not Assigned(TClass(GetOrdProp(Self, PropInfo)))) then {BOGUS Hint: Conversion between ordinals and pointers is not portable} // bogus only because we do a test higher up so we'll hit a $FATAL if this is a problem
                  begin
                     Stream.WriteAnsiString('');
                  end
                  else
                  begin
                     Assert(GetTypeData(PropInfo^.PropType)^.InstanceType^.Kind = tkClass);
                     Assert(Assigned(TClass(GetOrdProp(Self, PropInfo)))); {BOGUS Hint: Conversion between ordinals and pointers is not portable} // bogus only because we do a test higher up so we'll hit a $FATAL if this is a problem
                     Stream.WriteAnsiString(TClass(GetOrdProp(Self, PropInfo)).ClassName); {BOGUS Hint: Conversion between ordinals and pointers is not portable} // bogus only because we do a test higher up so we'll hit a $FATAL if this is a problem
                  end;
               end;
             tkMethod:
               begin
                  Stream.WriteMethod(GetMethodProp(Self, PropInfo));
               end;
             else
               Assert(False, 'Property ' + PropInfo^.Name + ' uses a type (' + GetEnumName(TypeInfo(TTypeKind), Ord(PropInfo^.PropType^.Kind)) + ') that TAutoStorable does not yet support');
            end;
         end;
      end;
      FreeMem(PropertyList);
   end;
   Stream.WriteAnsiString('');
end;

end.
