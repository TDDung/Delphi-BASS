{
  Bass_OPUS 2.4 Delphi unit
  Copyright (c) 2012-2020 Un4seen Developments Ltd.

  See the BASS_OPUS.CHM file for more detailed documentation

  NOTE: Delphi users should use the BASS_UNICODE flag where possible
}

{
TDDung - Vietnam, May 2021
}

unit BassOPUS;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Bass;

const
{$IFDEF MSWINDOWS}
  BassOpusDLL = 'bassopus.dll';
{$ENDIF}

{$IFDEF LINUX}
  BassOpusDLL = 'libbassopus.so';
{$ENDIF}

{$IFDEF MACOS}
  {$IFDEF IOS} // Needed frameworks: AudioToolbox, SystemConfiguration, CFNetwork, Accelerate, CoreMIDI (if using BASSMIDI)
    BassOpusDLL = 'Bass\Add-ons\Libs\iOS\libbassopus.a'; // Full path for static linking
  {$ELSE}
    BassOpusDLL = 'libbassopus.dylib';
  {$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
  BassOpusDLL = 'libbassopus.so';
{$ENDIF}

const
  // BASS_CHANNELINFO type
  BASS_CTYPE_STREAM_OPUS        = $11200;

  // Additional attributes
  BASS_ATTRIB_OPUS_ORIGFREQ     = $13000;
  BASS_ATTRIB_OPUS_GAIN         = $13001;

{$IFDEF IOS}
  procedure BASSOPUSplugin; cdecl; external BassOpusDLL;
  function BASS_OPUS_StreamCreateFile(mem:BOOL; f:Pointer; offset,length:QWORD; flags:Cardinal): HSTREAM; cdecl; external BassOpusDLL;
  function BASS_OPUS_StreamCreateFileUser(system,flags:Cardinal; var procs:BASS_FILEPROCS; user:Pointer): HSTREAM; cdecl; external BassOpusDLL;
  function BASS_OPUS_StreamCreateURL(URL:PAnsiChar; offset, flags:Cardinal; proc:DOWNLOADPROC; user:Pointer): HSTREAM; cdecl; external BassOpusDLL;
{$ELSE}
var
  BASS_OPUS_StreamCreateFile:function(mem:BOOL; f:Pointer; offset,length:QWORD; flags:Cardinal): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_OPUS_StreamCreateFileUser:function(system,flags:Cardinal; var procs:BASS_FILEPROCS; user:Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_OPUS_StreamCreateURL:function(URL:PAnsiChar; offset, flags:Cardinal; proc:DOWNLOADPROC; user:Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

var
  FBassOpusDLL: THandle = 0;
{$ENDIF}

function BassOPUS_IsAvailable: Boolean;

implementation

function BassOPUS_IsAvailable: Boolean;
begin
  {$IFDEF IOS}
  Result := True;
  {$ELSE}
  Result := FBassOpusDLL <> 0;
  {$ENDIF}
end;

{$IFNDEF IOS}
procedure LoadBassOpusDLL;
begin
  FBassOpusDLL := LoadLibrary(PChar(BassOpusDLL));
  if FBassOpusDLL = 0 then
    Exit;

  BASS_OPUS_StreamCreateFile:= GetProcAddress(FBassOpusDLL, PChar('BASS_OPUS_StreamCreateFile'));
  BASS_OPUS_StreamCreateFileUser:= GetProcAddress(FBassOpusDLL, PChar('BASS_OPUS_StreamCreateFileUser'));
  BASS_OPUS_StreamCreateURL:= GetProcAddress(FBassOpusDLL, PChar('BASS_OPUS_StreamCreateURL'));
end;

procedure UnloadBassOpusDLL;
begin
  FreeLibrary(FBassOpusDLL);
end;
{$ENDIF}

initialization
  {$IFNDEF IOS}
  LoadBassOpusDLL;
  {$ENDIF}
finalization
  {$IFNDEF IOS}
  UnloadBassOpusDLL;
  {$ENDIF}
end.
