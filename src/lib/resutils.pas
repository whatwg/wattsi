{$MODE OBJFPC} { -*- delphi -*- }
{$INCLUDE settings.inc}
unit resutils;

interface

type
   TResourceCallback = procedure (const Data: Pointer; const Size: Cardinal);
   TResourceCallbackNested = procedure (const Data: Pointer; const Size: Cardinal) is nested;
   TResourceCallbackMethod = procedure (const Data: Pointer; const Size: Cardinal) of object;

procedure ReadFromResources(const SectionName, ResourceName: AnsiString; const Callback: TResourceCallback);
procedure ReadFromResources(const SectionName, ResourceName: AnsiString; const Callback: TResourceCallbackNested);
procedure ReadFromResources(const SectionName, ResourceName: AnsiString; const Callback: TResourceCallbackMethod);

implementation

procedure ReadFromResources(const SectionName, ResourceName: AnsiString; const Callback: TResourceCallback);
var
   FoundResourceHandle: TFPResourceHandle;
   LoadedResourceHandle: TFPResourceHGlobal;
   Data: Pointer;
   Size: Cardinal;
begin
   FoundResourceHandle := FindResource(HInstance, ResourceName, SectionName);
   Assert(FoundResourceHandle <> 0, 'failed to load resource ' + SectionName + ':' + ResourceName);
   LoadedResourceHandle := LoadResource(HInstance, FoundResourceHandle);
   Assert(LoadedResourceHandle <> 0);
   Data := LockResource(LoadedResourceHandle);
   Size := SizeOfResource(HInstance, FoundResourceHandle);
   Callback(Data, Size);
   UnlockResource(LoadedResourceHandle);
   FreeResource(LoadedResourceHandle);
end;

procedure ReadFromResources(const SectionName, ResourceName: AnsiString; const Callback: TResourceCallbackNested);
var
   FoundResourceHandle: TFPResourceHandle;
   LoadedResourceHandle: TFPResourceHGlobal;
   Data: Pointer;
   Size: Cardinal;
begin
   FoundResourceHandle := FindResource(HInstance, ResourceName, SectionName);
   Assert(FoundResourceHandle <> 0, 'failed to load resource ' + SectionName + ':' + ResourceName);
   LoadedResourceHandle := LoadResource(HInstance, FoundResourceHandle);
   Assert(LoadedResourceHandle <> 0);
   Data := LockResource(LoadedResourceHandle);
   Size := SizeOfResource(HInstance, FoundResourceHandle);
   Callback(Data, Size);
   UnlockResource(LoadedResourceHandle);
   FreeResource(LoadedResourceHandle);
end;

procedure ReadFromResources(const SectionName, ResourceName: AnsiString; const Callback: TResourceCallbackMethod);
var
   FoundResourceHandle: TFPResourceHandle;
   LoadedResourceHandle: TFPResourceHGlobal;
   Data: Pointer;
   Size: Cardinal;
begin
   FoundResourceHandle := FindResource(HInstance, ResourceName, SectionName);
   Assert(FoundResourceHandle <> 0, 'failed to load resource ' + SectionName + ':' + ResourceName);
   LoadedResourceHandle := LoadResource(HInstance, FoundResourceHandle);
   Assert(LoadedResourceHandle <> 0);
   Data := LockResource(LoadedResourceHandle);
   Size := SizeOfResource(HInstance, FoundResourceHandle);
   Callback(Data, Size);
   UnlockResource(LoadedResourceHandle);
   FreeResource(LoadedResourceHandle);
end;

{$IFDEF ENABLETESTS}
{$IFOPT C+} {$ELSE} {$FATAL Can't run tests without assertion support} {$ENDIF}
{$RESOURCE tests/resutils.rc}
procedure TestRes();

   procedure Check(const Data: Pointer; const Size: Cardinal);
   var
      S: AnsiString;
   begin
      SetLength(S, Size);
      Move(Data^, S[1], Size);
      Assert(S = 'This is test data.'#$0A);
   end;

begin
   ReadFromResources('testdata', 'resutils', @Check);
end;
{$ENDIF}

initialization
   {$IFDEF ENABLETESTS} TestRes(); {$ENDIF}
end.
// if you need windres: sudo apt-get install mingw32-binutils
// make sure you have a symlink to /usr/bin/*windres* in your path as "windres"
