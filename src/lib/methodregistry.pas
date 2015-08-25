{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit methodregistry;

interface

uses
   typinfo;

procedure RegisterMethod(Address: Pointer; Signature: PTypeInfo);
{$IFOPT C+} procedure AssertMethodRegistered(Address: Pointer); {$ENDIF}
function GetTypeInfo(Address: Pointer): PTypeInfo;

implementation

uses
   hashtable;

type
   TMethodHashTable = specialize THashTable<Pointer, PTypeInfo>;

var
   Registry: TMethodHashTable;

procedure RegisterMethod(Address: Pointer; Signature: PTypeInfo);
begin
   Assert(Assigned(Address));
   Assert(Assigned(Signature));
   Assert(not Registry.Has(Address));
   Assert(Signature^.Kind = tkMethod);
   Registry.Add(Address, Signature);
end;

{$IFOPT C+}
procedure AssertMethodRegistered(Address: Pointer);
begin
   Assert(Assigned(Address));
   Assert(Registry.Has(Address));
end;
{$ENDIF}

function GetTypeInfo(Address: Pointer): PTypeInfo;
begin
   Assert(Assigned(Address));
   Assert(Registry.Has(Address));
   Result := Registry[Address];
end;

initialization
   Registry := TMethodHashTable.Create(@PointerHash32);
finalization
   Registry.Free();
end.
