{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit unicode;

interface

const
   kEOF = -1;
   {$IFOPT C+} kNone = -2; {$ENDIF}

type
   TUnicodeCodepointRange = type {$IFOPT C+} -2 {$ELSE} -1 {$ENDIF} ..$10FFFF;
   TUnicodeCodepoint = record
    private
     FValue: TUnicodeCodepointRange;
    public
     property Value: TUnicodeCodepointRange read FValue;
     {$IFOPT C+} function GetDebugDescription(): UTF8String; {$ENDIF}
   end;
   operator := (const Value: TUnicodeCodepointRange): TUnicodeCodepoint; inline;
   operator = (const Op1: TUnicodeCodepoint; const Op2: TUnicodeCodepointRange): Boolean; inline;
   operator = (const Op1, Op2: TUnicodeCodepoint): Boolean; inline;

type
   TUnicodeCodepointArray = array of TUnicodeCodepoint;

implementation

uses exceptions, sysutils {$IFOPT C+}, utf8 {$ENDIF};

operator := (const Value: TUnicodeCodepointRange): TUnicodeCodepoint;
begin
   Assert((Value < $D800) or (Value > $DFFF)); // exclude surrogates
   Result.FValue := Value;
end;

operator = (const Op1: TUnicodeCodepoint; const Op2: TUnicodeCodepointRange): Boolean;
begin
   Assert((Op2 < $D800) or (Op2 > $DFFF)); // exclude surrogates
   Result := Op1.Value = Op2;
end;

operator = (const Op1, Op2: TUnicodeCodepoint): Boolean;
begin
   Result := Op1.Value = Op2.Value;
end;

{$IFOPT C+}
function TUnicodeCodepoint.GetDebugDescription(): UTF8String;
begin
   if (FValue = kEOF) then
      Result := 'EOF'
   else
   if (FValue = kNone) then
      Result := 'NO CHARACTER'
   else
   if (FValue = $0000) then
      Result := 'U+0000 NULL'
   else
   if (FValue = $000A) then
      Result := 'U+000A LF'
   else
   if (FValue = $000D) then
      Result := 'U+000D CR'
   else
   if (FValue = $0020) then
      Result := 'U+0020 SPACE'
   else
   if (FValue < $0020) then
      Result := 'U+' + IntToHex(FValue, 4) + ' (<control>)'
   else
      Result := 'U+' + IntToHex(FValue, 4) + ' (' + CodepointToUTF8(FValue) + ')';
end;
{$ENDIF}

end.