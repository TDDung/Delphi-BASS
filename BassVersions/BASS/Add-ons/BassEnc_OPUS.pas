{
  BassEnc_OPUS 2.4 Delphi unit
  Copyright (c) 2016-2020 Un4seen Developments Ltd.

  See the BASSENC_OPUS.CHM file for more detailed documentation

  NOTE: Delphi users should use the BASS_UNICODE flag where possible
}

{
TDDung - Vietnam, May 2021
}

unit BassEnc_OPUS;

interface

uses
 {$IFDEF MSWINDOWS}
  Windows,
 {$ENDIF}
  SysUtils, Bass, BassEnc;

const
{$IFDEF MSWINDOWS}
  BassEncOpusDLL = 'bassenc_opus.dll';
{$ENDIF}

{$IFDEF LINUX}
  BassEncOpusDLL = 'libbassenc_opus.so';
{$ENDIF}

{$IFDEF MACOS}
  {$IFDEF IOS} // Needed frameworks: AudioToolbox, SystemConfiguration, CFNetwork, Accelerate, CoreMIDI (if using BASSMIDI)
    BassEncOpusDLL = 'Bass\Add-ons\Libs\iOS\libbassenc_opus.a'; // Full path for static linking
  {$ELSE}
    BassEncOpusDLL = 'libbassenc_opus.dylib';
  {$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
  BassEncOpusDLL = 'libbassenc_opus.so';
{$ENDIF}

{$IFDEF IOS}
  function BASS_Encode_OPUS_GetVersion: Cardinal; cdecl; external BassEncOpusDLL;
  function BASS_Encode_OPUS_NewStream(handle: HENCODE; options: PChar; flags: Cardinal): BOOL; cdecl; external BassEncOpusDLL;
  function BASS_Encode_OPUS_Start(handle:Cardinal; options:PChar; flags:Cardinal; proc:ENCODEPROC; user:Pointer): HENCODE; cdecl; external BassEncOpusDLL;
  function BASS_Encode_OPUS_StartFile(handle:Cardinal; options:PChar; flags:Cardinal; filename:PChar): HENCODE; cdecl; external BassEncOpusDLL;
{$ELSE}
var
  BASS_Encode_OPUS_GetVersion:function:Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_OPUS_NewStream:function(handle: HENCODE; options: PChar; flags: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_OPUS_Start:function(handle:Cardinal; options:PChar; flags:Cardinal; proc:ENCODEPROC; user:Pointer): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_OPUS_StartFile:function(handle:Cardinal; options:PChar; flags:Cardinal; filename:PChar): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

var
  FBassEncOpusDLL : THandle = 0;
{$ENDIF}

function BassEncOPUS_IsAvailable: Boolean;

implementation

function BassEncOPUS_IsAvailable: Boolean;
begin
  {$IFDEF IOS}
  Result := True;
  {$ELSE}
  Result := FBassEncOpusDLL <> 0;
  {$ENDIF}
end;

{$IFNDEF IOS}
procedure LoadBassEncOpusDLL;
begin
  FBassEncOpusDLL := LoadLibrary(PChar(BassEncOpusDLL));
  if FBassEncOpusDLL = 0 then
    Exit;

  BASS_Encode_OPUS_GetVersion:= GetProcAddress(FBassEncOpusDLL, PChar('BASS_Encode_OPUS_GetVersion'));
  BASS_Encode_OPUS_NewStream:= GetProcAddress(FBassEncOpusDLL, PChar('BASS_Encode_OPUS_NewStream'));
  BASS_Encode_OPUS_Start:= GetProcAddress(FBassEncOpusDLL, PChar('BASS_Encode_OPUS_Start'));
  BASS_Encode_OPUS_StartFile:= GetProcAddress(FBassEncOpusDLL, PChar('BASS_Encode_OPUS_StartFile'));
end;

procedure UnloadBassEncOpusDLL;
begin
  FreeLibrary(FBassEncOpusDLL);
end;
{$ENDIF}

initialization
  {$IFNDEF IOS}
  LoadBassEncOpusDLL;
  {$ENDIF}
finalization
  {$IFNDEF IOS}
  UnloadBassEncOpusDLL;
  {$ENDIF}
end.
