{
  BassEnc_OGG 2.4 Delphi unit
  Copyright (c) 2016-2020 Un4seen Developments Ltd.

  See the BASSENC_OGG.CHM file for more detailed documentation

  NOTE: Delphi users should use the BASS_UNICODE flag where possible
}

{
TDDung - Vietnam, May 2021
}

unit BassEnc_OGG;

interface

uses
 {$IFDEF MSWINDOWS}
  Windows,
 {$ENDIF}
  SysUtils, Bass, BassEnc;

const
{$IFDEF MSWINDOWS}
  BassEncOggDLL = 'bassenc_ogg.dll';
{$ENDIF}

{$IFDEF LINUX}
  BassEncOggDLL = 'libbassenc_ogg.so';
{$ENDIF}

{$IFDEF MACOS}
  {$IFDEF IOS} // Needed frameworks: AudioToolbox, SystemConfiguration, CFNetwork, Accelerate, CoreMIDI (if using BASSMIDI)
    BassEncOggDLL = 'Bass\Add-ons\Libs\iOS\libbassenc_ogg.a'; // Full path for static linking
  {$ELSE}
    BassEncOggDLL = 'libbassenc_ogg.dylib';
  {$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
  BassEncOggDLL = 'libbassenc_ogg.so';
{$ENDIF}

{$IFDEF IOS}
  function BASS_Encode_OGG_GetVersion: Cardinal; cdecl; external BassEncOggDLL;
  function BASS_Encode_OGG_NewStream(handle: HENCODE; options: PChar; flags: Cardinal): BOOL; cdecl; external BassEncOggDLL;
  function BASS_Encode_OGG_Start(handle:Cardinal; options:PChar; flags:Cardinal; proc:ENCODEPROC; user:Pointer): HENCODE; cdecl; external BassEncOggDLL;
  function BASS_Encode_OGG_StartFile(handle:Cardinal; options:PChar; flags:Cardinal; filename:PChar): HENCODE; cdecl; external BassEncOggDLL;
{$ELSE}
var
  BASS_Encode_OGG_GetVersion:function:Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_OGG_NewStream:function(handle: HENCODE; options: PChar; flags: Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_OGG_Start:function(handle:Cardinal; options:PChar; flags:Cardinal; proc:ENCODEPROC; user:Pointer): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_OGG_StartFile:function(handle:Cardinal; options:PChar; flags:Cardinal; filename:PChar): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

var
  FBassEncOggDLL : THandle = 0;
{$ENDIF}

function BassEncOGG_IsAvailable: Boolean;

implementation

function BassEncOGG_IsAvailable: Boolean;
begin
  {$IFDEF IOS}
  Result := True;
  {$ELSE}
  Result := FBassEncOggDLL <> 0;
  {$ENDIF}
end;

{$IFNDEF IOS}
procedure LoadBassEncOggDLL;
begin
  FBassEncOggDLL := LoadLibrary(PChar(BassEncOggDLL));
  if FBassEncOggDLL = 0 then
    Exit;

  BASS_Encode_OGG_GetVersion:= GetProcAddress(FBassEncOggDLL, PChar('BASS_Encode_OGG_GetVersion'));
  BASS_Encode_OGG_NewStream:= GetProcAddress(FBassEncOggDLL, PChar('BASS_Encode_OGG_NewStream'));
  BASS_Encode_OGG_Start:= GetProcAddress(FBassEncOggDLL, PChar('BASS_Encode_OGG_Start'));
  BASS_Encode_OGG_StartFile:= GetProcAddress(FBassEncOggDLL, PChar('BASS_Encode_OGG_StartFile'));
end;

procedure UnloadBassEncOggDLL;
begin
  FreeLibrary(FBassEncOggDLL);
end;
{$ENDIF}

initialization
  {$IFNDEF IOS}
  LoadBassEncOggDLL;
  {$ENDIF}
finalization
  {$IFNDEF IOS}
  UnloadBassEncOggDLL;
  {$ENDIF}
end.
