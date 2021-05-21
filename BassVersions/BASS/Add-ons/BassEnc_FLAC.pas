{
  BassEnc_FLAC 2.4 Delphi unit
  Copyright (c) 2017-2020 Un4seen Developments Ltd.

  See the BASSENC_FLAC.CHM file for more detailed documentation

  NOTE: Delphi users should use the BASS_UNICODE flag where possible
}

{
TDDung - Vietnam, May 2021
}

unit BassEnc_FLAC;

interface

uses
 {$IFDEF MSWINDOWS}
  Windows,
 {$ENDIF}
  SysUtils, Bass, BassEnc;

const
  // Additional BASS_Encode_FLAC_StartFile flags
  BASS_ENCODE_FLAC_NOCOUNT = $1000000;

{$IFDEF MSWINDOWS}
  BassEncFlacDLL = 'bassenc_flac.dll';
{$ENDIF}

{$IFDEF LINUX}
  BassEncFlacDLL = 'libbassenc_flac.so';
{$ENDIF}


{$IFDEF MACOS}
  {$IFDEF IOS} // Needed frameworks: AudioToolbox, SystemConfiguration, CFNetwork, Accelerate, CoreMIDI (if using BASSMIDI)
    BassEncFlacDLL = 'Bass\Add-ons\Libs\iOS\libbassenc_flac.a'; // Full path for static linking
  {$ELSE}
    BassEncFlacDLL = 'libbassenc_flac.dylib';
  {$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
  BassEncFlacDLL = 'libbassenc_flac.so';
{$ENDIF}

{$IFDEF IOS}
  function BASS_Encode_FLAC_GetVersion: Cardinal; cdecl; external BassEncFlacDLL;
  function BASS_Encode_FLAC_NewStream(handle: HENCODE; options: PChar; flags: Cardinal): BOOL; cdecl; external BassEncFlacDLL;
  function BASS_Encode_FLAC_Start(handle:Cardinal; options:PChar; flags:Cardinal; proc:ENCODEPROC; user:Pointer): HENCODE; cdecl; external BassEncFlacDLL;
  function BASS_Encode_FLAC_StartFile(handle:Cardinal; options:PChar; flags:Cardinal; filename:PChar): HENCODE; cdecl; external BassEncFlacDLL;
{$ELSE}
var
  BASS_Encode_FLAC_GetVersion:function:Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_FLAC_NewStream:function(handle: HENCODE; options: PChar; flags: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_FLAC_Start:function(handle:Cardinal; options:PChar; flags:Cardinal; proc:ENCODEPROC; user:Pointer): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_FLAC_StartFile:function(handle:Cardinal; options:PChar; flags:Cardinal; filename:PChar): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

var
  FBassEncFlacDLL : THandle = 0;
{$ENDIF}

function BassEncFLAC_IsAvailable: Boolean;

implementation

function BassEncFLAC_IsAvailable: Boolean;
begin
  {$IFDEF IOS}
  Result := True;
  {$ELSE}
  Result := FBassEncFlacDLL <> 0;
  {$ENDIF}
end;

{$IFNDEF IOS}
procedure LoadBassEncFlacDLL;
begin
  FBassEncFlacDLL := LoadLibrary(PChar(BassEncFlacDLL));
  if FBassEncFlacDLL = 0 then
    Exit;

  BASS_Encode_FLAC_GetVersion:= GetProcAddress(FBassEncFlacDLL, PChar('BASS_Encode_FLAC_GetVersion'));
  BASS_Encode_FLAC_NewStream:= GetProcAddress(FBassEncFlacDLL, PChar('BASS_Encode_FLAC_NewStream'));
  BASS_Encode_FLAC_Start:= GetProcAddress(FBassEncFlacDLL, PChar('BASS_Encode_FLAC_Start'));
  BASS_Encode_FLAC_StartFile:= GetProcAddress(FBassEncFlacDLL, PChar('BASS_Encode_FLAC_StartFile'));
end;

procedure UnloadBassEncFlacDLL;
begin
  FreeLibrary(FBassEncFlacDLL);
end;
{$ENDIF}

initialization
  {$IFNDEF IOS}
  LoadBassEncFlacDLL;
  {$ENDIF}
finalization
  {$IFNDEF IOS}
  UnloadBassEncFlacDLL;
  {$ENDIF}
end.
