{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit specutils;

interface

function ASCIILowerCase(const Data: UTF8String): UTF8String;

implementation

function ASCIILowerCase(const Data: UTF8String): UTF8String;
var
   Index: Cardinal;
begin
   Result := Data;
   if (Length(Result) > 0) then
      for Index := 1 to Length(Result) do
         if (Result[Index] in ['A'..'Z']) then
            Result[Index] := Chr(Ord(Result[Index]) + $0020); // $R-
end;

end.
