{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit fileutils;

interface

uses
   baseunix;

type
   TFileData = record
      Start: Pointer;
      Length: size_t;
      procedure Destroy();
   end;

function ReadFile(const FileName: AnsiString): TFileData; // This is efficient
function ReadTextFile(const FileName: AnsiString): UTF8String; // This is not

function IsEmptyDirectory(const Path: AnsiString): Boolean;

implementation

uses
   exceptions, sysutils;

function ReadFile(const FileName: AnsiString): TFileData;
var
   FileDescriptor: CInt;
   StatInfo: Stat;
   MapResult: Pointer;
begin
   FileDescriptor := fpOpen(FileName, O_RDONLY);
   if (FileDescriptor < 0) then
      raise EKernelError.Create(fpGetErrNo);
   if (fpFStat(FileDescriptor, StatInfo) <> 0) then // $DFA- for StatInfo
      raise EKernelError.Create(fpGetErrNo);
   MapResult := fpMMap(nil, StatInfo.st_size+1, PROT_READ, MAP_PRIVATE, FileDescriptor, 0); // $R-
   {$PUSH}
   {$WARNINGS OFF} {$HINTS OFF}
   if (PtrInt(MapResult) < 0) then
      raise EKernelError.Create(fpGetErrNo);
   {$POP}
   fpClose(FileDescriptor);
   Result.Length := StatInfo.st_size; // $R-
   Result.Start := Pointer(MapResult);
end;

procedure TFileData.Destroy();
begin
  if (fpMUnMap(Self.Start, Self.Length) <> 0) Then
     raise EKernelError.Create(fpGetErrNo);
end;

function ReadTextFile(const FileName: AnsiString): UTF8String;
var
   Source: TFileData;
begin
   Source := ReadFile(FileName);
   if (Source.Length > High(Length(Result))) then
      raise Exception.Create('text file too big');
   SetLength(Result, Source.Length);
   Move(Source.Start^, Result[1], Source.Length); // $R-
   Source.Destroy();
end;

function IsEmptyDirectory(const Path: AnsiString): Boolean;
var
   FileRecord: TSearchRec;
   GotOneDot, GotTwoDots, GotOther: Boolean;
begin
   if (DirectoryExists(Path)) then
   begin
      GotOneDot := False;
      GotTwoDots := False;
      GotOther := False;
      if (FindFirst(Path + '/*', faDirectory, FileRecord) = 0) then
         repeat
            if (FileRecord.Name = '.') then
               GotOneDot := True
            else
            if (FileRecord.Name = '..') then
               GotTwoDots := True
            else
            begin
               GotOther := True;
               break;
            end;
         until (FindNext(FileRecord) <> 0);
      Result := GotOneDot and GotTwoDots and not GotOther;
      FindClose(FileRecord);
   end
   else
      Result := False;
end;

end.