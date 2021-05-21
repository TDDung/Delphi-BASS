{
  BassEnc_MP3 2.4 Delphi unit
  Copyright (c) 2018-2020 Un4seen Developments Ltd.

  See the BASSENC_MP3.CHM file for more detailed documentation

  NOTE: Delphi users should use the BASS_UNICODE flag where possible
}

{
TDDung - Vietnam, May 2021
}

Unit BassEnc_MP3;

interface

uses
 {$IFDEF MSWINDOWS}
  Windows,
 {$ENDIF}
  SysUtils, {Bass, }BassEnc;

const
{$IFDEF MSWINDOWS}
  BassEncMp3DLL = 'bassenc_mp3.dll';
{$ENDIF}

{$IFDEF LINUX}
  BassEncMp3DLL = 'libbassenc_mp3.so';
{$ENDIF}

{$IFDEF MACOS}
  {$IFDEF IOS} // Needed frameworks: AudioToolbox, SystemConfiguration, CFNetwork, Accelerate, CoreMIDI (if using BASSMIDI)
    BassEncMp3DLL = 'Bass\Add-ons\Libs\iOS\libbassenc_mp3.a'; // Full path for static linking
  {$ELSE}
    BassEncMp3DLL = 'libbassenc_mp3.dylib';
  {$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
  BassEncMp3DLL = 'libbassenc_mp3.so';
{$ENDIF}

{$IFDEF IOS}
  function BASS_Encode_MP3_GetVersion: Cardinal; cdecl; external BassEncMp3DLL;
  function BASS_Encode_MP3_Start(handle:Cardinal; options:PChar; flags:Cardinal; proc:ENCODEPROC; user:Pointer): HENCODE; cdecl; external BassEncMp3DLL;
  function BASS_Encode_MP3_StartFile(handle:Cardinal; options:PChar; flags:Cardinal; filename:PChar): HENCODE; cdecl; external BassEncMp3DLL;
{$ELSE}
var
  BASS_Encode_MP3_GetVersion:function:Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_MP3_Start:function(handle:Cardinal; options:PChar; flags:Cardinal; proc:ENCODEPROC; user:Pointer): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_MP3_StartFile:function(handle:Cardinal; options:PChar; flags:Cardinal; filename:PChar): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

var
  FBassEncMp3DLL : THandle = 0;
{$ENDIF}

function BassEncMP3_IsAvailable: Boolean;

implementation

function BassEncMP3_IsAvailable: Boolean;
begin
  {$IFDEF IOS}
  Result := True;
  {$ELSE}
  Result := FBassEncMp3DLL <> 0;
  {$ENDIF}
end;

{$IFNDEF IOS}
procedure LoadBassEncMp3DLL;
begin
  FBassEncMp3DLL := LoadLibrary(PChar(BassEncMp3DLL));
  if FBassEncMp3DLL = 0 then
    Exit;

  BASS_Encode_MP3_GetVersion:= GetProcAddress(FBassEncMp3DLL, PChar('BASS_Encode_MP3_GetVersion'));
  BASS_Encode_MP3_Start:= GetProcAddress(FBassEncMp3DLL, PChar('BASS_Encode_MP3_Start'));
  BASS_Encode_MP3_StartFile:= GetProcAddress(FBassEncMp3DLL, PChar('BASS_Encode_MP3_StartFile'));
end;

procedure UnloadBassEncMp3DLL;
begin
  FreeLibrary(FBassEncMp3DLL);
end;
{$ENDIF}

initialization
  {$IFNDEF IOS}
  LoadBassEncMp3DLL;
  {$ENDIF}
finalization
  {$IFNDEF IOS}
  UnloadBassEncMp3DLL;
  {$ENDIF}
end.