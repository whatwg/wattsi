{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit wires;

// A mostly rigid string
// This is a ShortString/UTF8String hybrid.
// It uses a ShortString until you hit 255 characters, at which point it starts using a regular UTF8String.

interface

uses
   unicode, utf8;

type
   TWire = record
    private
     const
      kSafeThreshold = High(ShortString) - High(TUTF8SequenceLength) + 1;
     var
      FShortString: ShortString;
      FLongString: UTF8String;
      function GetAsString(): UTF8String; inline;
      function GetIsEmpty(): Boolean; inline;
    public
      procedure Init(); inline;
      procedure Append(const Codepoint: TUnicodeCodepoint); inline;
      property AsString: UTF8String read GetAsString;
      property IsEmpty: Boolean read GetIsEmpty;
   end;

   operator = (const Op1: TWire; const Op2: UTF8String): Boolean; inline;

implementation

procedure TWire.Init();
begin
   FShortString := '';
   FLongString := '';
end;

procedure TWire.Append(const Codepoint: TUnicodeCodepoint);
var
   AsUTF8: TUTF8Sequence;
begin
   AsUTF8 := CodepointToUTF8(CodePoint);
   if (Length(FShortString) < kSafeThreshold) then
   begin
      {$PUSH}
      {$RANGECHECKS OFF}
      FShortString := FShortString + AsUTF8.AsString;
      {$POP}
   end
   else
   begin
      FLongString := FLongString + AsUTF8.AsString;
   end;
end;

function TWire.GetAsString(): UTF8String;
begin
   Result := FShortString + FLongString;
end;

function TWire.GetIsEmpty(): Boolean;
begin
   Result := FShortString[0] = #0;
end;

operator = (const Op1: TWire; const Op2: UTF8String): Boolean; inline;
begin
   if (Length(Op2) < Length(Op1.FShortString)) then
   begin
      Result := False;
   end
   else
   if (Length(Op1.FShortString) < Op1.kSafeThreshold) then
   begin
      Result := Op1.FShortString = Op2;
   end
   else
   begin
      Result := (Op1.FShortString + Op1.FLongString) = Op2;
   end;
end;

end.