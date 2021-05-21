{
  Bass_FLAC 2.4 Delphi unit
  Copyright (c) 2004-2020 Un4seen Developments Ltd.

  See the BASS_FLAC.CHM file for more detailed documentation

  NOTE: Delphi users should use the BASS_UNICODE flag where possible
}

{
TDDung - Vietnam, May 2021
}

unit BassFLAC;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Bass;

const
{$IFDEF MSWINDOWS}
  BassFlacDLL = 'bassflac.dll';
{$ENDIF}

{$IFDEF LINUX}
  BassFlacDLL = 'libbassflac.so';
{$ENDIF}

{$IFDEF MACOS}
  {$IFDEF IOS} // Needed frameworks: AudioToolbox, SystemConfiguration, CFNetwork, Accelerate, CoreMIDI (if using BASSMIDI)
    BassFlacDLL = 'Bass\Add-ons\Libs\iOS\libbassflac.a'; // Full path for static linking
  {$ELSE}
    BassFlacDLL = 'libbassflac.dylib';
  {$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
  BassFlacDLL = 'libbassflac.so';
{$ENDIF}

const
  // BASS_CHANNELINFO type
  BASS_CTYPE_STREAM_FLAC        = $10900;
  BASS_CTYPE_STREAM_FLAC_OGG    = $10901;

  // Additional tag type
  BASS_TAG_FLAC_CUE             = 12; // cuesheet : TAG_FLAC_CUE structure
  BASS_TAG_FLAC_PICTURE         = $12000; // + index #, picture : TAG_FLAC_PICTURE structure
  BASS_TAG_FLAC_METADATA        = $12400; // + index #, application metadata : TAG_FLAC_METADATA structure

type
  TAG_FLAC_PICTURE = record
    apic: Cardinal;		// ID3v2 "APIC" picture type
    mime: PAnsiChar;	// mime type
    desc: PAnsiChar;	// description
    width: Cardinal;
    height: Cardinal;
    depth: Cardinal;
    colors: Cardinal;
    length: Cardinal;	// data length
    data: Pointer;
  end;

  TAG_FLAC_METADATA = record
    id: array[0..3] of AnsiChar;
	  length: Cardinal;	// data length
    data: Pointer;
  end;

{$IFDEF IOS}
  procedure BASSFLACplugin; cdecl; external BassFlacDLL;
  function BASS_FLAC_StreamCreateFile(mem:BOOL; f:Pointer; offset,length:QWORD; flags:Cardinal): HSTREAM; cdecl; external BassFlacDLL;
  function BASS_FLAC_StreamCreateFileUser(system,flags:Cardinal; var procs:BASS_FILEPROCS; user:Pointer): HSTREAM; cdecl; external BassFlacDLL;
  function BASS_FLAC_StreamCreateURL(URL:PAnsiChar; offset, flags:Cardinal; proc:DOWNLOADPROC; user:Pointer): HSTREAM; cdecl; external BassFlacDLL;
{$ELSE}
var
  BASS_FLAC_StreamCreateFile:function(mem:BOOL; f:Pointer; offset,length:QWORD; flags:Cardinal): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_FLAC_StreamCreateFileUser:function(system,flags:Cardinal; var procs:BASS_FILEPROCS; user:Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_FLAC_StreamCreateURL:function(URL:PAnsiChar; offset, flags:Cardinal; proc:DOWNLOADPROC; user:Pointer): HSTREAM; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

var
  FBassFlacDLL: THandle = 0;
{$ENDIF}

function BassFLAC_IsAvailable: Boolean;

implementation

function BassFLAC_IsAvailable: Boolean;
begin
  {$IFDEF IOS}
  Result := True;
  {$ELSE}
  Result := FBassFlacDLL <> 0;
  {$ENDIF}
end;

{$IFNDEF IOS}
procedure LoadBassFlacDLL;
begin
  FBassFlacDLL := LoadLibrary(PChar(BassFlacDLL));
  if FBassFlacDLL = 0 then
    Exit;

  BASS_FLAC_StreamCreateFile:= GetProcAddress(FBassFlacDLL, PChar('BASS_FLAC_StreamCreateFile'));
  BASS_FLAC_StreamCreateFileUser:= GetProcAddress(FBassFlacDLL, PChar('BASS_FLAC_StreamCreateFileUser'));
  BASS_FLAC_StreamCreateURL:= GetProcAddress(FBassFlacDLL, PChar('BASS_FLAC_StreamCreateURL'));
end;

procedure UnloadBassFlacDLL;
begin
  FreeLibrary(FBassFlacDLL);
end;
{$ENDIF}

initialization
  {$IFNDEF IOS}
  LoadBassFlacDLL;
  {$ENDIF}
finalization
  {$IFNDEF IOS}
  UnloadBassFlacDLL;
  {$ENDIF}
end.
