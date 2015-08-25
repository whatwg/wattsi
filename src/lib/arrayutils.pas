{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit arrayutils;

interface

// FisherYatesShuffle(ArrayToShuffle[Low(ArrayToShuffle)], Length(ArrayToShuffle), SizeOf(ArrayToShuffle[0]));
procedure FisherYatesShuffle(var Buffer; const Count: Cardinal; const ElementSize: Cardinal);

implementation

procedure FisherYatesShuffle(var Buffer; const Count: Cardinal; const ElementSize: Cardinal);
var
   Index, Subindex: Cardinal;
   Temp: Pointer;
begin
   if (Count < 2) then
      Exit;
   GetMem(Temp, ElementSize);
   for Index := Count-1 downto 1 do
   begin
      Subindex := Random(Index+1);
      {$POINTERMATH ON}
      Move((@Buffer+Subindex*ElementSize)^, Temp^, ElementSize);
      Move((@Buffer+Index*ElementSize)^, (@Buffer+Subindex*ElementSize)^, ElementSize);
      Move(Temp^, (@Buffer+Index*ElementSize)^, ElementSize);
      {$POINTERMATH OFF}
   end;
   FreeMem(Temp, ElementSize);
end;

end.
