{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit canonicalstrings;

interface

{$IFDEF CPU64}
// if we are on CPU64, there's a DWord of padding in the TAnsiRec header
// we could in theory use this to store the hash
// unfortunately, this only works if we don't use constants, since we can't
// modify the constant's header (it's on disk)
// if you want to use this, uncomment the following line:
// {$DEFINE PACKHASH}
{$ENDIF}

type
   TCanonicalString = record
    private
      FValue: Pointer;
      {$IFNDEF PACKHASH} FHashCode: DWord; {$ENDIF}
      function GetHashCode(): DWord; inline;
      procedure SetHashCode(const Value: DWord);
      function GetValue(): UTF8String; inline;
    public
      property AsString: UTF8String read GetValue;
      property HashCode: DWord read GetHashCode;
      class function Equals(const A, B: TCanonicalString): Boolean; static; inline;
      class function LessThan(const A, B: TCanonicalString): Boolean; static; inline;
      class function GreaterThan(const A, B: TCanonicalString): Boolean; static; inline;
   end;
   operator = (const Op1, Op2: TCanonicalString): Boolean; inline;

function CanonicalStringHash32(const Key: TCanonicalString): DWord; inline;

function Intern(const S: UTF8String): TCanonicalString; inline;

implementation

uses
   hashset, hashfunctions, stringutils;

{$IFDEF PACKHASH}
type
   PAnsiRec = ^TAnsiRec;
   TAnsiRec = record
      // based on TAnsiRec in astrings.inc
      // hopefully the layout won't change any time soon
      CodePage: TSystemCodePage;
      ElementSize: Word;
      HashCode: DWord; // this is Dummy in astrings.inc's record definition
      Ref: SizeInt;
      Len: SizeInt;
      Data: record end; // this is where the string data goes
   end;
   {$IF SizeOf(TAnsiRec) <> SizeOf(TSystemCodePage) +
                            SizeOf(Word) +
                            SizeOf(DWord) +
                            SizeOf(SizeInt) +
                            SizeOf(SizeInt) } {$FATAL TAnsiRec size is unexpected} {$ENDIF}
{$ENDIF}

function TCanonicalString.GetHashCode(): DWord;
begin
   {$IFDEF PACKHASH}
      Result := PAnsiRec(FValue-SizeOf(TAnsiRec))^.HashCode;
   {$ELSE}
      Result := FHashCode;
   {$ENDIF}
end;

procedure TCanonicalString.SetHashCode(const Value: DWord);
begin
   {$IFDEF PACKHASH}
      Assert(FValue <> nil);
      Assert(PAnsiRec(FValue-SizeOf(TAnsiRec))^.Ref > 0);
      PAnsiRec(FValue-SizeOf(TAnsiRec))^.HashCode := Value;
   {$ELSE}
      FHashCode := Value;
   {$ENDIF}
end;

function TCanonicalString.GetValue(): UTF8String;
begin
   Result := UTF8String(FValue);
end;

class function TCanonicalString.Equals(const A, B: TCanonicalString): Boolean;
begin
   Result := A.FValue = B.FValue;
   Assert((Result) = (UTF8String(A.FValue) = UTF8String(B.FValue)));
end;

class function TCanonicalString.LessThan(const A, B: TCanonicalString): Boolean;
begin
   Result := A.FValue < B.FValue;
end;

class function TCanonicalString.GreaterThan(const A, B: TCanonicalString): Boolean;
begin
   Result := A.FValue > B.FValue;
end;

operator = (const Op1, Op2: TCanonicalString): Boolean;   
begin
   Result := TCanonicalString.Equals(Op1, Op2);
end;

function CanonicalStringHash32(const Key: TCanonicalString): DWord;
begin
   Result := Key.HashCode;
end;

type
   TUTF8StringHashSet = specialize THashSet<UTF8String, UTF8StringUtils>;

var
   Strings: TUTF8StringHashSet;

function Intern(const S: UTF8String): TCanonicalString;
var
   Hash: DWord;
begin
   Result.FValue := Pointer(Strings.Intern(S, Hash));
   Result.SetHashCode(Hash);
end;

initialization
   Strings := TUTF8StringHashSet.Create(@UTF8StringHash32, 8);
finalization
   Strings.Free();
   // after this point, all TCanonicalString instances are going to be bogus
   // since they all point to UTF8Strings that have been dereffed
end.
