{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit hashfunctions;

interface

type
   THashTableSizeInt = 0..MaxInt;

function Integer32Hash32(const Key: DWord): DWord; inline;
function Integer64Hash32(const Key: QWord): DWord; inline;
function PtrUIntHash32(const Key: PtrUInt): DWord; inline;
function PointerHash32(const Key: Pointer): DWord; inline;
function TMethodHash32(const Key: TMethod): DWord; inline;
function RawByteStringHash32(const Key: RawByteString): DWord; inline;
function AnsiStringHash32(const Key: AnsiString): DWord; inline;
function UTF8StringHash32(const Key: UTF8String): DWord; inline;

implementation

{$IF SIZEOF(DWord) <> 4} {$ERROR DWord must be 32 bits wide.} {$ENDIF}
{$IF SIZEOF(QWord) <> 8} {$ERROR QWord must be 64 bits wide.} {$ENDIF}

uses
   sysutils;

function Integer32Hash32(const Key: DWord): DWord;
begin
   Assert(SizeOf(DWord) * 8 = 32);
   Result := Key;
   { Robert Jenkins 32bit Integer Hash - http://burtleburtle.net/bob/hash/integer.html }
   {$PUSH}
   {$OVERFLOWCHECKS OFF}
   {$RANGECHECKS OFF}
   {$HINTS OFF} // because all this intentionally overflows
   Result := (Result  +  $7ed55d16)  +  (Result shl 12);
   Result := (Result xor $c761c23c) xor (Result shr 19);
   Result := (Result  +  $165667b1)  +  (Result shl  5);
   Result := (Result  +  $d3a2646c) xor (Result shl  9);
   Result := (Result  +  $fd7046c5)  +  (Result shl  3);
   Result := (Result xor $b55a4f09) xor (Result shr 16);
   {$POP}
end;

function Integer64Hash32(const Key: QWord): DWord;
var
   Scratch: QWord;
begin
   Assert(SizeOf(QWord) * 8 = 64);
   {$PUSH}
   {$OVERFLOWCHECKS OFF}
   Scratch := Key;
   { Thomas Wang's hash6432shift - http://www.concentric.net/~Ttwang/tech/inthash.htm }
   Scratch := (not Scratch) + (Scratch shl 18);
   Scratch := Scratch xor (Scratch shr 31);
   Scratch := Scratch * 21;
   Scratch := Scratch xor (Scratch shr 11);
   Scratch := Scratch + (Scratch shl 6);
   Scratch := Scratch xor (Scratch shr 22);
   Result := DWord(Scratch);
   {$POP}
end;

function PtrUIntHash32(const Key: PtrUInt): DWord;
begin
   {$PUSH}
   {$OVERFLOWCHECKS OFF}
   {$IF SizeOf(PtrUInt) = SizeOf(DWord) }
     Result := Integer32Hash32(Key);
   {$ELSEIF SizeOf(PtrUInt) = SizeOf(QWord) }
     Result := Integer64Hash32(Key);
   {$ELSE}
     {$FATAL No hash function for pointer size on this platform. }
   {$ENDIF}
   {$POP}
end;

function PointerHash32(const Key: Pointer): DWord;
begin
   {$HINTS OFF} // Otherwise it complains that casting Pointer to PtrUInt is not portable, but it is portable, by definition
   Result := PtrUIntHash32(PtrUInt(Key));
   {$HINTS ON}
end;

function TMethodHash32(const Key: TMethod): DWord;
begin
   {$IF SizeOf(PtrUInt) = SizeOf(DWord) }
     Assert(SizeOf(Key) = SizeOf(QWord));
     Result := Integer64Hash32(QWord(Key));
   {$ELSEIF SizeOf(Pointer) = SizeOf(QWord) }
     // XXX no idea if this is an acceptable hash function
     // XXX should print out the hashtable histogram once there's a number of watchers
     {$HINTS OFF} // Otherwise it complains that casting Pointer to QWord is not portable, but we only go down this path if it is ok for this platform
     Result := Integer64Hash32(QWord(TMethod(Key).Code)) xor Integer64Hash32(QWord(TMethod(Key).Data));
     {$HINTS ON}
   {$ELSE}
     Result := PointerHash32(TMethod.Code) xor PointerHash32(TMethod.Data);
   {$ENDIF}
end;

function RawByteStringHash32(const Key: RawByteString): DWord;
var
   Index: Cardinal;
begin
   {$PUSH}
   {$RANGECHECKS OFF}

{$HINTS OFF} // not sure if the four hints for the next few lines are valid or not, but I'm guessing not.
   // djb2 from http://www.cse.yorku.ca/~oz/hash.html:
   Result := 5381;
   if (Length(Key) > 0) then
      for Index := 1 to Length(Key) do
         Result := Result shl 5 + Result + Ord(Key[Index]);
{$HINTS ON}

   // djb2 bis from http://www.cse.yorku.ca/~oz/hash.html:
   //Result := 5381;
   //if (Length(Key) > 0) then
   //   for Index := 1 to Length(Key) do
   //      Result := Result * 33 xor Ord(Key[Index]);

   // sdbm from http://www.cse.yorku.ca/~oz/hash.html:
   //Result := 0;
   //if (Length(Key) > 0) then
   //   for Index := 1 to Length(Key) do
   //      Result := Ord(Key[Index]) + (Result shl 6) + (Result shl 16) - Result;

   {$POP}
end;

function AnsiStringHash32(const Key: AnsiString): DWord;
begin
   Result := RawByteStringHash32(Key);
end;

function UTF8StringHash32(const Key: UTF8String): DWord;
begin
   Result := RawByteStringHash32(Key);
end;

end.