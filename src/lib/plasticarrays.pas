{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit plasticarrays;

interface

type
   generic PlasticArray <T, Utils> = record
    private
     type
      PPlasticArray = ^PlasticArray;
      TArray = array of T;
     var
      FArray: TArray;
      FFilledLength: Cardinal;
      function GetItem(const Index: Cardinal): T; inline;
      procedure SetItem(const Index: Cardinal; const Item: T); inline;
      function GetLast(): T; inline;
      procedure SetFilledLength(const NewFilledLength: Cardinal); inline;
    public
      // these calls are all O(1) except as noted
      procedure Init(); // call this if the PlasticArray is not pre-zeroed (i.e. class member is fine; but if you use this in a procedure, call Init() first)
      procedure Push(const Item: T); inline; // expensive if it requires the length to be increased
      function Pop(): T; inline;
      procedure Empty(); inline;
      property Length: Cardinal read FFilledLength write SetFilledLength; // expensive if it requires the length to be increased
      property Items[Index: Cardinal]: T read GetItem write SetItem; default;
      property Last: T read GetLast;
    public
      // The following calls are relatively expensive for various reasons
      procedure Squeeze(); inline; // reduces memory usage to minimum required
      procedure RemoveAt(const Index: Cardinal); // does a memory move
      procedure Remove(const Value: T); // does a linear search, then memory move
      function Contains(const Value: T): Boolean; // linear search
      function Contains(const Value: T; out IndexResult: Cardinal): Boolean; // linear search; IndexResult is only valid if result is True
      procedure RemoveShiftLeftInsert(const RemoveIndex, InsertIndex: Cardinal; NewValue: T);
    public
     type
      TCompareFunc = function (const A, B: T): Integer is nested;
      procedure Sort(const CompareFunc: TCompareFunc);
      procedure Sort();
    strict private
      // these aren't nested procs just because generics can't have nested procs
      procedure QuickSort(L, R: Integer; const CompareFunc: TCompareFunc);
      procedure QuickSort(L, R: Integer);
    public
     type
       TEnumerator = class
        strict private
          FTarget: PPlasticArray;
          FIndex: Cardinal;
          function GetCurrent(): T;
        public
          constructor Create(const Target: PPlasticArray);
          function MoveNext(): Boolean;
          property Current: T read GetCurrent;
       end;
      function GetEnumerator(): TEnumerator;
   end;

implementation

procedure PlasticArray.Init();
begin
   FFilledLength := 0;
end;

function PlasticArray.GetItem(const Index: Cardinal): T;
begin
   Assert(Index < FFilledLength);
   Result := FArray[Index];
end;

procedure PlasticArray.SetItem(const Index: Cardinal; const Item: T);
begin
   Assert(Index < FFilledLength);
   FArray[Index] := Item;
end;

function PlasticArray.GetLast(): T;
begin
   Assert(FFilledLength > 0);
   Result := FArray[FFilledLength-1]; // $R-
end;

procedure PlasticArray.SetFilledLength(const NewFilledLength: Cardinal);
begin
   FFilledLength := NewFilledLength;
   if (FFilledLength > System.Length(FArray)) then
      SetLength(FArray, FFilledLength);
end;

procedure PlasticArray.Squeeze();
begin
   SetLength(FArray, FFilledLength);
end;

procedure PlasticArray.Empty();
begin
   FFilledLength := 0;
end;

procedure PlasticArray.Push(const Item: T);
begin
   Assert(FFilledLength < High(Cardinal));
   SetFilledLength(FFilledLength + 1); // $R-
   FArray[FFilledLength-1] := Item;
end;

function PlasticArray.Pop(): T;
begin
   Assert(FFilledLength > 0);
   Dec(FFilledLength);
   Result := FArray[FFilledLength];
end;

procedure PlasticArray.RemoveAt(const Index: Cardinal);
begin
   Assert(FFilledLength > 0);
   Assert(Index < FFilledLength);
   Dec(FFilledLength);
   if (Index < FFilledLength) then
      Move(FArray[Index+1], FArray[Index], (FFilledLength-Index+1)*SizeOf(T));
end;

procedure PlasticArray.Remove(const Value: T);
var
   Index: Cardinal;
begin
   if (FFilledLength > 0) then
   begin
      Index := FFilledLength;
      repeat
         Dec(Index);
         if (Utils.Equals(FArray[Index], Value)) then
         begin
            RemoveAt(Index);
            exit;
         end;
      until Index = Low(FArray);
   end;
end;

function PlasticArray.Contains(const Value: T): Boolean;
var
   Index: Cardinal;
begin
   if (FFilledLength > 0) then
      for Index := FFilledLength-1 downto Low(FArray) do // $R-
         if (Utils.Equals(FArray[Index], Value)) then
         begin
            Result := True;
            exit;
         end;
   Result := False;
end;

function PlasticArray.Contains(const Value: T; out IndexResult: Cardinal): Boolean;
var
   Index: Cardinal;
begin
   if (FFilledLength > 0) then
      for Index := FFilledLength-1 downto Low(FArray) do // $R-
         if (Utils.Equals(FArray[Index], Value)) then
         begin
            Result := True;
            IndexResult := Index;
            exit;
         end;
   Result := False;
   {$IFOPT C+} IndexResult := High(IndexResult); {$ENDIF}
end;

procedure PlasticArray.RemoveShiftLeftInsert(const RemoveIndex, InsertIndex: Cardinal; NewValue: T);
begin
   Assert(RemoveIndex <= InsertIndex);
   Assert(InsertIndex < FFilledLength);
   Assert(System.Length(FArray) >= FFilledLength);
   if (InsertIndex = RemoveIndex) then
   begin
      FArray[InsertIndex] := NewValue;
   end
   else
   begin
      Move(FArray[RemoveIndex+1], FArray[RemoveIndex], (InsertIndex-RemoveIndex)*SizeOf(T));
      FArray[InsertIndex] := NewValue;
   end;
end;

procedure PlasticArray.QuickSort(L, R: Integer; const CompareFunc: TCompareFunc);
var
   I, J : Integer;
   P, Q : T;
begin
   // based on QuickSort in rtl/objpas/classes/lists.inc
   repeat
      I := L;
      J := R;
      P := FArray[(L + R) div 2];
      repeat
         while (CompareFunc(P, FArray[I]) > 0) do
            I := I + 1; // $R-
         while (CompareFunc(P, FArray[J]) < 0) do
            J := J - 1; // $R-
         if (I <= J) then
         begin
            Q := FArray[I];
            FArray[I] := FArray[J];
            FArray[J] := Q;
            I := I + 1; // $R-
            J := J - 1; // $R-
         end;
      until I > J;
      if (L < J) then
         QuickSort(L, J, CompareFunc);
      L := I;
   until I >= R;
end;

procedure PlasticArray.Sort(const CompareFunc: TCompareFunc);
begin
   Assert(FFilledLength < High(Integer));
   if (FFilledLength > 1) then
      QuickSort(Low(FArray), FFilledLength-1, CompareFunc); // $R-
end;

procedure PlasticArray.QuickSort(L, R: Integer);
var
   I, J : Integer;
   P, Q : T;
begin
   // based on QuickSort in rtl/objpas/classes/lists.inc
   repeat
      I := L;
      J := R;
      P := FArray[(L + R) div 2];
      repeat
         while (Utils.GreaterThan(P, FArray[I])) do
            I := I + 1; // $R-
         while (Utils.LessThan(P, FArray[J])) do
            J := J - 1; // $R-
         if (I <= J) then
         begin
            Q := FArray[I];
            FArray[I] := FArray[J];
            FArray[J] := Q;
            I := I + 1; // $R-
            J := J - 1; // $R-
         end;
      until I > J;
      if (L < J) then
         QuickSort(L, J);
      L := I;
   until I >= R;
end;

procedure PlasticArray.Sort();
begin
   Assert(FFilledLength < High(Integer));
   if (FFilledLength > 1) then
      QuickSort(Low(FArray), FFilledLength-1); // $R-
end;

constructor PlasticArray.TEnumerator.Create(const Target: PPlasticArray);
begin
   inherited Create();
   Assert(Assigned(Target));
   FTarget := Target;
end;

function PlasticArray.TEnumerator.GetCurrent(): T;
begin
   Assert(FIndex > 0);
   Result := FTarget^[FIndex-1]; // $R-
end;

function PlasticArray.TEnumerator.MoveNext(): Boolean;
begin
   Result := FIndex < FTarget^.Length;
   Inc(FIndex);
end;

function PlasticArray.GetEnumerator(): TEnumerator;
begin
   Result := TEnumerator.Create(@Self);
end;

end.
