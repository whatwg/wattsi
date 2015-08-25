{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit hashset;

interface

uses
   hashfunctions;

type
   generic THashSet <T, Utils> = class
    strict protected
     type
      PPHashSetEntry = ^PHashSetEntry;
      PHashSetEntry = ^THashSetEntry;
      THashSetEntry = record
        Value: T;
        Next: PHashSetEntry;
      end;
      THashFunction = function (const Value: T): DWord;
      THashSetSizeInt = THashTableSizeInt;
     const
      kMaxLoad = 0.7; // Wikipedia: "With a good hash function, the average lookup cost is nearly constant as the load factor increases from 0 up to 0.7 or so"
     var
      FTable: array of PHashSetEntry;
      FCount: THashSetSizeInt;
      FHashFunction: THashFunction;
      procedure DoubleSize();
      procedure Resize(const NewSize: THashSetSizeInt);
      procedure PrepareForSize(PredictedCount: THashSetSizeInt);
      procedure InternalAdd(var Table: array of PHashSetEntry; const Value: T);
    public
      constructor Create(const AHashFunction: THashFunction; const PredictedCount: THashSetSizeInt = 8);
      destructor Destroy(); override;
      procedure Reset();
      procedure Add(const Value: T);
        // Add() should only be called for values that are not in the table (as checked by Has()).
      procedure Intern(const Value: T);
      function Intern(const Value: T; out Hash: DWord): T;
        // Intern() first checks if the value is already in the table, and if it is, it returns that previous value;
        // otherwise, it adds it to the table. The returned value (and hash code) can be useful for types where
        // Utils.Equals() can return true even for values that are not pointer-equal, e.g. strings.
      procedure Remove(const Value: T);
      function Has(const Value: T): Boolean;
    public
     type
       TEnumerator = class
        strict private
          FOwner: THashSet;
          FIndex: THashSetSizeInt;
          FCurrent: PHashSetEntry;
          function GetCurrent(): T;
        public
          constructor Create(const Owner: THashSet);
          function MoveNext(): Boolean;
          property Current: T read GetCurrent;
       end;
      function GetEnumerator(): TEnumerator;
   end;

implementation

// a lot of this is just copied from hashtable.pas

uses
   sysutils;

constructor THashSet.Create(const AHashFunction: THashFunction; const PredictedCount: THashSetSizeInt = 8);
begin
   inherited Create();
   Assert(Assigned(AHashFunction));
   FHashFunction := AHashFunction;
   Assert(PredictedCount > 0);
   PrepareForSize(PredictedCount);
end;

destructor THashSet.Destroy();
begin
   Reset();
   inherited;
end;

procedure THashSet.Reset();
var
   Index: THashSetSizeInt;
   Item, LastItem: PHashSetEntry;
begin
   if (Length(FTable) > 0) then
      for Index := Low(FTable) to High(FTable) do
      begin
         Item := FTable[Index];
         while (Assigned(Item)) do
         begin
            LastItem := Item;
            Item := Item^.Next;
            Dispose(LastItem);
         end;
         FTable[Index] := nil;
      end;
   FCount := 0;
end;

procedure THashSet.DoubleSize();
begin
   Assert(Length(FTable) > 0);
   if (Length(FTable)*2 < High(THashSetSizeInt)) then
      Resize(Length(FTable) * 2) // $R-
   else
   if (Length(FTable) < High(THashSetSizeInt)) then
      Resize(High(THashSetSizeInt));
end;

procedure THashSet.PrepareForSize(PredictedCount: THashSetSizeInt);
const
   LoadFactorLimit = 1/kMaxLoad;
begin
   Assert(PredictedCount > 0);
   if (PredictedCount * LoadFactorLimit < High(THashSetSizeInt)) then
      PredictedCount := Trunc(PredictedCount * LoadFactorLimit) // $R-
   else
      PredictedCount := High(THashSetSizeInt);
   if (FCount > 0) then
      Resize(PredictedCount)
   else
      SetLength(FTable, PredictedCount);
end;

procedure THashSet.Resize(const NewSize: THashSetSizeInt);
var
   NewSet: array of PHashSetEntry;
   Index: THashSetSizeInt;
   Item, LastItem: PHashSetEntry;
begin
   Assert(NewSize > 0);
   if (NewSize <> Length(FTable)) then
   begin
      SetLength(NewSet, NewSize);
      Assert(Length(FTable) > 0);
      for Index := Low(FTable) to High(FTable) do // $R-
      begin
         Item := FTable[Index];
         while (Assigned(Item)) do
         begin
            InternalAdd(NewSet, Item^.Value);
            LastItem := Item;
            Item := Item^.Next;
            Dispose(LastItem);
         end;
      end;
      FTable := NewSet;
   end;
end;

procedure THashSet.InternalAdd(var Table: array of PHashSetEntry; const Value: T);
var
   Hash: DWord;
   Entry: PHashSetEntry;
begin
   { This is safe because Length(table) is positive and 'mod' will only ever return a smaller value }
   Hash := FHashFunction(Value) mod Length(Table); // $R-
   New(Entry);
   Entry^.Value := Value;
   Entry^.Next := Table[Hash];
   Table[Hash] := Entry;
end;

procedure THashSet.Add(const Value: T);
begin
   Assert(not Has(Value));
   Inc(FCount);
   if (FCount/Length(FTable) > kMaxLoad) then
      DoubleSize();
   InternalAdd(FTable, Value);
end;

procedure THashSet.Intern(const Value: T);
var
   Entry: PHashSetEntry;
begin
   { This is safe because Length(table) is positive and 'mod' will only ever return a smaller value }
   Entry := FTable[FHashFunction(Value) mod Length(FTable)];
   while (Assigned(Entry)) do
   begin
      if (Utils.Equals(Entry^.Value, Value)) then
         exit;
      Entry := Entry^.Next;
   end;
   Add(Value);
end;

function THashSet.Intern(const Value: T; out Hash: DWord): T;
var
   Entry: PHashSetEntry;
begin
   Hash := FHashFunction(Value);
   { This is safe because Length(table) is positive and 'mod' will only ever return a smaller value }
   Entry := FTable[Hash mod Length(FTable)];
   while (Assigned(Entry)) do
   begin
      if (Utils.Equals(Entry^.Value, Value)) then
      begin
         Result := Entry^.Value;
         exit;
      end;
      Entry := Entry^.Next;
   end;
   Add(Value);
   Result := Value;
end;

procedure THashSet.Remove(const Value: T);
var
   Hash: DWord;
   Entry: PHashSetEntry;
   LastEntry: PPHashSetEntry;
begin
   { This is safe because Length(table) is positive and 'mod' will only ever return a smaller value }
   Hash := FHashFunction(Value) mod Length(FTable); // $R-
   Entry := FTable[Hash];
   LastEntry := @FTable[Hash];
   while (Assigned(Entry)) do
   begin
      if (Utils.Equals(Entry^.Value, Value)) then
      begin
         LastEntry^ := Entry^.Next;
         Dispose(Entry);
         Dec(FCount);
         exit;
      end;
      LastEntry := @Entry^.Next;
      Entry := Entry^.Next;
   end;
end;

function THashSet.Has(const Value: T): Boolean;
var
   Entry: PHashSetEntry;
begin
   { This is safe because Length(table) is positive and 'mod' will only ever return a smaller value }
   Entry := FTable[FHashFunction(Value) mod Length(FTable)];
   while (Assigned(Entry)) do
   begin
      if (Utils.Equals(Entry^.Value, Value)) then
      begin
         Result := True;
         exit;
      end;
      Entry := Entry^.Next;
   end;
   Result := False;
end;


constructor THashSet.TEnumerator.Create(const Owner: THashSet);
begin
   FOwner := Owner;
   FIndex := 0;
   FCurrent := nil;
end;

function THashSet.TEnumerator.GetCurrent(): T;
begin
   Result := FCurrent^.Value;
end;

function THashSet.TEnumerator.MoveNext(): Boolean;
begin
   if (Assigned(FCurrent)) then
   begin // advance
      FCurrent := FCurrent^.Next;
   end
   else
   begin // just started
      Assert(FIndex = 0);
      FCurrent := FOwner.FTable[FIndex];
   end;
   while ((not Assigned(FCurrent)) and (FIndex < High(FOwner.FTable))) do
   begin
      Inc(FIndex);
      FCurrent := FOwner.FTable[FIndex];
   end;
   Result := Assigned(FCurrent);
end;

function THashSet.GetEnumerator(): TEnumerator;
begin
   Result := TEnumerator.Create(Self);
end;

end.
