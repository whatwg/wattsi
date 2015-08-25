{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit genericutils;

interface

type
   generic DefaultUtils <T> = record
      class function Equals(const A, B: T): Boolean; static; inline;
      class function LessThan(const A, B: T): Boolean; static; inline;
      class function GreaterThan(const A, B: T): Boolean; static; inline;
   end;

   generic DefaultUnorderedUtils <T> = record
      class function Equals(const A, B: T): Boolean; static; inline;
      class function LessThan(const A, B: T): Boolean; static; inline;
      class function GreaterThan(const A, B: T): Boolean; static; inline;
   end;

   generic IncomparableUtils <T> = record
      class function Equals(const A, B: T): Boolean; static; inline;
      class function LessThan(const A, B: T): Boolean; static; inline;
      class function GreaterThan(const A, B: T): Boolean; static; inline;
   end;

   TObjectUtils = specialize DefaultUnorderedUtils <TObject>;

implementation

uses
   sysutils;

class function DefaultUtils.Equals(const A, B: T): Boolean;
begin
   Result := A = B;
end;

class function DefaultUtils.LessThan(const A, B: T): Boolean;
begin
   Result := A < B;
end;

class function DefaultUtils.GreaterThan(const A, B: T): Boolean;
begin
   Result := A > B;
end;

class function DefaultUnorderedUtils.Equals(const A, B: T): Boolean;
begin
   Result := A = B;
end;

class function DefaultUnorderedUtils.LessThan(const A, B: T): Boolean;
begin
   raise Exception.Create('tried to compare unordered data');
   Result := False;
end;

class function DefaultUnorderedUtils.GreaterThan(const A, B: T): Boolean;
begin
   raise Exception.Create('tried to compare unordered data');
   Result := False;
end;

class function IncomparableUtils.Equals(const A, B: T): Boolean;
begin
   Result := False;
end;

class function IncomparableUtils.LessThan(const A, B: T): Boolean;
begin
   raise Exception.Create('tried to compare unordered data');
   Result := False;
end;

class function IncomparableUtils.GreaterThan(const A, B: T): Boolean;
begin
   raise Exception.Create('tried to compare unordered data');
   Result := False;
end;

end.
