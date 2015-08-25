{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit hashtable;

// warning: if you edit the implementation section of this unit but
// not its interface, dependent units won't be recompiled, so force it
// with -B

interface

uses
   hashfunctions;

(* How to use THashTable ******************************************************************
 * This generates a TFoo to TBar hash table with name TFooHashTable.
 * Replace FooHash32 with one of the functions immediately below, based on TFoo's type
 * If TFoo is a type that doesn't have built-in =/</> operators, you'll need to define
 * your own utility record type instead of using DefaultUtils; see genericutils.pas

   type
      TFooUtils = specialize DefaultUtils <TFoo>;
      TFooHashTable = class(specialize THashTable <TFoo, TBar, TFooUtils>)
       public
        constructor Create(PredictedCount: THashTableSizeInt = 8);
      end;

   constructor TFooHashTable.Create(PredictedCount: THashTableSizeInt = 8);
   begin
      inherited Create(@FooHash32, PredictedCount);
   end;

 * If you just have a one-off instance, you can skip defining a convenience constructor
 * and just do it like this instead:

   type
      TFooUtils = specialize DefaultUtils <TFoo>;
      TFooHashTable = specialize THashTable <TFoo, TBar, TFooUtils>;

   var
      Hash: TFooHashTable;

   Hash := TFooHashTable.Create(@FooHash32, PredictedCount);

 *****************************************************************************************)

type
   generic THashTable <TKey, TValue, Utils> = class
    strict protected
     type
      PPHashTableEntry = ^PHashTableEntry;
      PHashTableEntry = ^THashTableEntry;
      THashTableEntry = record
        Key: TKey;
        Value: TValue;
        Next: PHashTableEntry;
      end;
      THashFunction = function (const Key: TKey): DWord;
     const
      kMaxLoadFactor = 0.7; // Wikipedia: "With a good hash function, the average lookup cost is nearly constant as the load factor increases from 0 up to 0.7 or so";
     var
      FTable: array of PHashTableEntry;
      FCount: THashTableSizeInt;
      FHashFunction: THashFunction;
      procedure DoubleSize();
      procedure Resize(const NewSize: THashTableSizeInt);
      procedure PrepareForSize(PredictedCount: THashTableSizeInt);
      procedure InternalAdd(var Table: array of PHashTableEntry; const Key: TKey; const Value: TValue);
      procedure Update(const Key: TKey; const Value: TValue); // will call Add() if the key isn't already present
      function Get(const Key: TKey): TValue;
      function GetKeyForEntry(const Entry: Pointer): TKey;
      function GetValueForEntry(const Entry: Pointer): TValue;
      procedure AdvanceEnumerator(var Current: Pointer; var Index: THashTableSizeInt);
    public
      constructor Create(const AHashFunction: THashFunction; const PredictedCount: THashTableSizeInt = 8);
      destructor Destroy(); override;
      procedure Empty();
      procedure Remove(const Key: TKey);
      function Has(const Key: TKey): Boolean;
      procedure Add(const Key: TKey; const Value: TValue);
      function Clone(): THashTable;
      property Items[Key: TKey]: TValue read Get write Update; default;
      {$IFDEF DEBUG} procedure Histogram(var F: Text); {$ENDIF}
      property Count: THashTableSizeInt read FCount;
    public
     type
       TKeyEnumerator = class
        strict private
          FOwner: THashTable;
          FIndex: THashTableSizeInt;
          FCurrent: Pointer;
          function GetCurrent(): TKey;
        public
          constructor Create(const Owner: THashTable);
          function MoveNext(): Boolean;
          property Current: TKey read GetCurrent;
          function GetEnumerator(): TKeyEnumerator;
       end;
      function GetEnumerator(): TKeyEnumerator;
    public
     type
       TValueEnumerator = class
        strict private
          FOwner: THashTable;
          FIndex: THashTableSizeInt;
          FCurrent: Pointer;
          function GetCurrent(): TValue;
        public
          constructor Create(const Owner: THashTable);
          function MoveNext(): Boolean;
          property Current: TValue read GetCurrent;
          function GetEnumerator(): TValueEnumerator;
       end;
      function Values(): TValueEnumerator;
   end;

   // XXX would be good to see if we can cache the enumerators mentioned above
   // e.g. by tracking if it's still in use, and having a "master" enumerator (cached the first time it's created) which
   // we only free when it's done, and whose .Free doesn't do anything if the instance is a master, or something
   // (assuming for..in implicitly calls .Free)

implementation

uses
   sysutils;

constructor THashTable.Create(const AHashFunction: THashFunction; const PredictedCount: THashTableSizeInt = 8);
begin
   inherited Create();
   Assert(Assigned(AHashFunction));
   FHashFunction := AHashFunction;
   Assert(PredictedCount > 0);
   PrepareForSize(PredictedCount);
end;

destructor THashTable.Destroy();
begin
   Empty();
   inherited;
end;

procedure THashTable.Empty();
var
   Index: THashTableSizeInt;
   Item, LastItem: PHashTableEntry;
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

procedure THashTable.DoubleSize();
begin
   Assert(Length(FTable) > 0);
   if (Length(FTable)*2 < High(THashTableSizeInt)) then
      Resize(Length(FTable) * 2) // $R-
   else
   if (Length(FTable) < High(THashTableSizeInt)) then
      Resize(High(THashTableSizeInt));
end;

procedure THashTable.PrepareForSize(PredictedCount: THashTableSizeInt);
const
   LoadFactorLimit = 1/kMaxLoadFactor;
begin
   Assert(PredictedCount > 0);
   if (PredictedCount * LoadFactorLimit < High(THashTableSizeInt)) then
      PredictedCount := Trunc(PredictedCount * LoadFactorLimit) // $R-
   else
      PredictedCount := High(THashTableSizeInt);
   if (FCount > 0) then
      Resize(PredictedCount)
   else
      SetLength(FTable, PredictedCount);
end;

procedure THashTable.Resize(const NewSize: THashTableSizeInt);
var
   NewTable: array of PHashTableEntry;
   Index: THashTableSizeInt;
   Item, LastItem: PHashTableEntry;
begin
   Assert(NewSize > 0);
   if (NewSize <> Length(FTable)) then
   begin
      SetLength(NewTable, NewSize);
      Assert(Length(FTable) > 0);
      for Index := Low(FTable) to High(FTable) do // $R-
      begin
         Item := FTable[Index];
         while (Assigned(Item)) do
         begin
            InternalAdd(NewTable, Item^.Key, Item^.Value);
            LastItem := Item;
            Item := Item^.Next;
            Dispose(LastItem);
         end;
      end;
      FTable := NewTable;
   end;
end;

procedure THashTable.InternalAdd(var Table: array of PHashTableEntry; const Key: TKey; const Value: TValue);
var
   Hash: DWord;
   Entry: PHashTableEntry;
begin
   { This is safe because Length(table) is positive and 'mod' will only ever return a smaller value }
   Hash := FHashFunction(Key) mod Length(Table); // $R-
   New(Entry);
   Entry^.Key := Key;
   Entry^.Value := Value;
   Entry^.Next := Table[Hash];
   Table[Hash] := Entry;
end;

procedure THashTable.Add(const Key: TKey; const Value: TValue);
begin
   Inc(FCount);
   if (FCount/Length(FTable) > kMaxLoadFactor) then
   begin
      { Wikipedia: "With a good hash function, the average lookup cost is nearly constant as the load factor increases from 0 up to 0.7 or so" }
      DoubleSize();
   end;
   InternalAdd(FTable, Key, Value);
end;

procedure THashTable.Remove(const Key: TKey);
var
   Hash: DWord;
   Entry: PHashTableEntry;
   LastEntry: PPHashTableEntry;
begin
   { This is safe because Length(table) is positive and 'mod' will only ever return a smaller value }
   Hash := FHashFunction(Key) mod Length(FTable); // $R-
   Entry := FTable[Hash];
   LastEntry := @FTable[Hash];
   while (Assigned(Entry)) do
   begin
      if (Utils.Equals(Entry^.Key, Key)) then
      begin
         LastEntry^ := Entry^.Next;
         Dispose(Entry);
         Dec(FCount);
         Exit;
      end;
      LastEntry := @Entry^.Next;
      Entry := Entry^.Next;
   end;
end;

function THashTable.Get(const Key: TKey): TValue;
var
   Entry: PHashTableEntry;
begin
   { This is safe because Length(table) is positive and 'mod' will only ever return a smaller value }
   Entry := FTable[FHashFunction(Key) mod Length(FTable)];
   while (Assigned(Entry)) do
   begin
      if (Utils.Equals(Entry^.Key, Key)) then
      begin
         Result := Entry^.Value;
         Exit;
      end;
      Entry := Entry^.Next;
   end;
   Result := Default(TValue);
end;

function THashTable.Has(const Key: TKey): Boolean;
var
   Entry: PHashTableEntry;
begin
   { This is safe because Length(table) is positive and 'mod' will only ever return a smaller value }
   Entry := FTable[FHashFunction(Key) mod Length(FTable)];
   while (Assigned(Entry)) do
   begin
      if (Utils.Equals(Entry^.Key, Key)) then
      begin
         Result := True;
         Exit;
      end;
      Entry := Entry^.Next;
   end;
   Result := False;
end;

procedure THashTable.Update(const Key: TKey; const Value: TValue);
var
   Entry: PHashTableEntry;
begin
   { This is safe because Length(table) is positive and 'mod' will only ever return a smaller value }
   Entry := FTable[FHashFunction(Key) mod Length(FTable)];
   while (Assigned(Entry)) do
   begin
      if (Utils.Equals(Entry^.Key, Key)) then
      begin
         Entry^.Value := Value;
         Exit;
      end;
      Entry := Entry^.Next;
   end;
   Add(Key, Value);
end;

{$IFDEF DEBUG}
procedure THashTable.Histogram(var F: Text);
var
   Index: THashTableSizeInt;
   Item: PHashTableEntry;
begin
   Assert(Length(FTable) > 0);
   Writeln(F, 'THashTable histogram:'); // $DFA- for F
   for Index := Low(FTable) to High(FTable) do // $R-
   begin
      System.Write(F, Index: 5, ': ');
      Item := FTable[Index];
      while (Assigned(Item)) do
      begin
         System.Write(F, '#');
         Item := Item^.Next;
      end;
      Writeln(F);
   end;
   Writeln(F, 'Size: ' + IntToStr(Length(FTable)) + '; Count: ' + IntToStr(FCount));
end;
{$ENDIF}

function THashTable.GetKeyForEntry(const Entry: Pointer): TKey;
begin
   if (Assigned(Entry)) then
   begin
      Result := PHashTableEntry(Entry)^.Key;
   end
   else
   begin
      Result := Default(TKey);
   end;
end;

function THashTable.GetValueForEntry(const Entry: Pointer): TValue;
begin
   if (Assigned(Entry)) then
   begin
      Result := PHashTableEntry(Entry)^.Value;
   end
   else
   begin
      Result := Default(TValue);
   end;
end;

procedure THashTable.AdvanceEnumerator(var Current: Pointer; var Index: THashTableSizeInt);
begin
   if (Assigned(Current)) then
   begin // advance
      Current := PHashTableEntry(Current)^.Next
   end
   else
   begin // just started
      Assert(Index = 0);
      Current := FTable[Index];
   end;
   while ((not Assigned(Current)) and (Index < High(FTable))) do
   begin
      Inc(Index);
      Current := FTable[Index];
   end;
end;

function THashTable.Clone(): THashTable;
var
   Index: Cardinal;
   Current: PHashTableEntry;
begin
   Assert(Assigned(Self));
   Result := ClassType.Create() as THashTable;
   Result.FHashFunction := FHashFunction;
   Result.PrepareForSize(FCount);
   if (FCount > 0) then
   begin
      Assert(Length(FTable) > 0);
      for Index := Low(FTable) to High(FTable) do // $R-
      begin
         Current := FTable[Index];
         while (Assigned(Current)) do
         begin
            Result.Add(Current^.Key, Current^.Value);
            Current := Current^.Next;
         end;
      end;
   end;
   Assert(Result.Count = FCount);
end;


constructor THashTable.TKeyEnumerator.Create(const Owner: THashTable);
begin
   FOwner := Owner;
   FIndex := 0;
   FCurrent := nil;
end;

function THashTable.TKeyEnumerator.GetCurrent(): TKey;
begin
   Result := FOwner.GetKeyForEntry(FCurrent);
end;

function THashTable.TKeyEnumerator.MoveNext(): Boolean;
begin
   FOwner.AdvanceEnumerator(FCurrent, FIndex);
   Result := Assigned(FCurrent);
end;

function THashTable.TKeyEnumerator.GetEnumerator(): TKeyEnumerator;
begin
   Result := Self;
end;

function THashTable.GetEnumerator(): TKeyEnumerator;
begin
   Result := TKeyEnumerator.Create(Self);
end;


constructor THashTable.TValueEnumerator.Create(const Owner: THashTable);
begin
   FOwner := Owner;
   FIndex := 0;
   FCurrent := nil;
end;

function THashTable.TValueEnumerator.GetCurrent(): TValue;
begin
   Result := FOwner.GetValueForEntry(FCurrent);
end;

function THashTable.TValueEnumerator.MoveNext(): Boolean;
begin
   FOwner.AdvanceEnumerator(FCurrent, FIndex);
   Result := Assigned(FCurrent);
end;

function THashTable.TValueEnumerator.GetEnumerator(): TValueEnumerator;
begin
   Result := Self;
end;

function THashTable.Values(): TValueEnumerator;
begin
   Result := TValueEnumerator.Create(Self);
end;

end.
