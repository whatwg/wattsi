{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
{
    This file is based on base64.pp in the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl
    base64 encoder & decoder (c) 1999 Sebastian Guenther
    Simplified for use without the FCL by Ian Hickson, 2012

    The source code of the Free Pascal Runtime Libraries and packages,
    and thus this derivative work, are distributed under the Library
    GNU General Public License (see the file COPYING.LGPL) with the
    following modification:

    As a special exception, the copyright holders of this library give
    you permission to link this library with independent modules to
    produce an executable, regardless of the license terms of these
    independent modules, and to copy and distribute the resulting
    executable under terms of your choice, provided that you also
    meet, for each linked independent module, the terms and conditions
    of the license of that module. An independent module is a module
    which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the
    library, but you are not obligated to do so. If you do not wish to
    do so, delete this exception statement from your version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit base64encoder;

interface

function Base64(const S: RawByteString): RawByteString;

implementation

const
  EncodingTable: String[64] = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

function Base64(const S: RawByteString): RawByteString;
var
   Index, Offset: Cardinal;
begin
   Result := '';
   Index := 1;
   while (Index+2 <= Length(S)) do
   begin
      Offset := Length(Result); {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
      SetLength(Result, Length(Result)+4);
      Result[Offset+1] := EncodingTable[1+(Ord(S[Index]) shr 2)];
      Result[Offset+2] := EncodingTable[1+((Ord(S[Index]) and 3) shl 4 or (Ord(S[Index+1]) shr 4))];
      Result[Offset+3] := EncodingTable[1+((Ord(S[Index+1]) and 15) shl 2 or (Ord(S[Index+2]) shr 6))];
      Result[Offset+4] := EncodingTable[1+(Ord(S[Index+2]) and 63)];
      Inc(Index, 3);
   end;
   if (Index <= Length(S)) then
   begin
      Assert(Length(S) - Index <= 2);
      Offset := Length(Result); {BOGUS Warning: Type size mismatch, possible loss of data / range check error}
      SetLength(Result, Length(Result)+4);
      case (Length(S)-Index) of
       0: begin
             Result[Offset+1] := EncodingTable[1+(Ord(S[Index]) shr 2)];
             Result[Offset+2] := EncodingTable[1+((Ord(S[Index]) and 3) shl 4)];
             Result[Offset+3] := '=';
             Result[Offset+4] := '=';
          end;
       1: begin
             Result[Offset+1] := EncodingTable[1+(Ord(S[Index]) shr 2)];
             Result[Offset+2] := EncodingTable[1+((Ord(S[Index]) and 3) shl 4 or (Ord(S[Index+1]) shr 4))];
             Result[Offset+3] := EncodingTable[1+((Ord(S[Index+1]) and 15) shl 2)];
             Result[Offset+4] := '=';
          end;
      end;
   end;
end;

end.
