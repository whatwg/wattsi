{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
program preprocessor;

uses
   unicode, json, fileutils;

type
   PEntityTreeNode = ^TEntityTreeNode;

   TSubNodeEntry = record
      Prefix: Cardinal;
      SubNode: PEntityTreeNode;
   end;

   TEntityTreeNode = record
      StateID: Cardinal;
      Children: array of TSubNodeEntry;
      Value: TUnicodeCodepointArray;
   end;

var
   LastStateID: Cardinal = 0;

procedure Add(const Entity: AnsiString; const Index: Cardinal; const Value: TJSONArray; const Node: PEntityTreeNode);
var
   SearchIndex: Cardinal;
   CurrentCharacter: Cardinal;
   CurrentValue: Double;
begin
   Assert(Assigned(Node));
   Assert(Index > 1);
   if (Index > Length(Entity)) then
   begin
      Assert(Length(Node^.Value) = 0);
      Assert(Value.Length >= 1);
      Assert(Value.Length <= 2);
      SetLength(Node^.Value, Value.Length);
      for SearchIndex := 0 to Value.Length-1 do // $R-
      begin
         CurrentValue := Value[SearchIndex];
         Assert(Frac(CurrentValue) = 0.0);
         Assert(CurrentValue >= $0000);
         Assert(CurrentValue <= $10FFFF);
         Node^.Value[SearchIndex] := Trunc(CurrentValue); // $R-
      end;
   end
   else
   begin
      // walk the tree
      Assert(Index <= Length(Entity));
      CurrentCharacter := Ord(Entity[Index]);
      SearchIndex := Low(Node^.Children);
      while (SearchIndex <= High(Node^.Children)) do
      begin
         if (Node^.Children[SearchIndex].Prefix = CurrentCharacter) then
            Break;
         Inc(SearchIndex);
      end;
      if (SearchIndex > High(Node^.Children)) then
      begin
         // we don't have a node for this yet
         SetLength(Node^.Children, Length(Node^.Children)+1);
         Assert(SearchIndex = High(Node^.Children));
         Node^.Children[SearchIndex].Prefix := Ord(Entity[Index]);
         New(Node^.Children[SearchIndex].SubNode);
         Inc(LastStateID);
         Node^.Children[SearchIndex].SubNode^.StateID := LastStateID;
      end;
      Assert(Index < High(Index)); // we'd presumably crash long before we reach this...
      Add(Entity, Index+1, Value, Node^.Children[SearchIndex].SubNode); // $R-
   end;
end;

procedure PrintStates(const Root: PEntityTreeNode; const LastDefault: PEntityTreeNode = nil);
var
   Index: Cardinal;
   NewDefault: PEntityTreeNode;
begin
   if (Length(Root^.Children) > 0) then
   begin
      Writeln('    ', Root^.StateID, ': case (Character.Value) of');
      for Index := Low(Root^.Children) to High(Root^.Children) do // $R-
      begin
         Write('         ', Root^.Children[Index].Prefix, ': ');
         if (Length(Root^.Children[Index].SubNode^.Children) = 0) then
         begin
            Assert(Root^.Children[Index].Prefix = Ord(';'));
            Assert(Length(Root^.Children[Index].SubNode^.Value) > 0);
            Assert(Length(Root^.Children[Index].SubNode^.Value) <= 2);
            Write('FinishedWithSemicolon(', Root^.Children[Index].SubNode^.Value[0].Value);
            if (Length(Root^.Children[Index].SubNode^.Value) = 2) then
               Write(', ', Root^.Children[Index].SubNode^.Value[1].Value);
            Writeln(');');
         end
         else
         if (Length(Root^.Children[Index].SubNode^.Value) > 0) then
         begin
            Writeln('IncompleteButBookmark(', Root^.Children[Index].SubNode^.StateID, ');');
         end
         else
         begin
            Writeln('Incomplete(', Root^.Children[Index].SubNode^.StateID, ');');
         end;
      end;
      if (Length(Root^.Value) > 0) then
      begin
         Assert(Length(Root^.Value) = 1);
         Writeln('      else FailButBacktrack(', Root^.Value[0].Value, ');')
      end
      else
      if (Assigned(LastDefault)) then
      begin
         Assert(Length(LastDefault^.Value) = 1);
         Writeln('      else FailButBacktrack(', LastDefault^.Value[0].Value, ');')
      end
      else
      begin
         Writeln('      else Fail();');
      end;
      Writeln('    end;');
   end;
   if (Length(Root^.Value) > 0) then
      NewDefault := Root
   else
      NewDefault := LastDefault;
   if (Length(Root^.Children) > 0) then
      for Index := Low(Root^.Children) to High(Root^.Children) do
         PrintStates(Root^.Children[Index].SubNode, NewDefault);
end;

procedure DisposeTree(const Node: PEntityTreeNode);
var
   Index: Cardinal;
begin
   if (Length(Node^.Children) > 0) then
      for Index := Low(Node^.Children) to High(Node^.Children) do
         DisposeTree(Node^.Children[Index].SubNode);
   Dispose(Node);
end;

var
   Data: TJSON;
   Tree: PEntityTreeNode;
   EntityName: AnsiString;
begin
   Tree := New(PEntityTreeNode);
   Tree^.StateID := 0;
   Data := ParseJSON(ReadTextFile('src/entities/entities.json'));
   Assert(Data is TJSONObject);
   try
      for EntityName in (Data as TJSONObject).Keys do
         Add(EntityName, 2, Data[EntityName]['codepoints'], Tree);
      PrintStates(Tree);
   finally
      Data.Free();
      DisposeTree(Tree);
   end;
end.
