{
  BassEnc 2.4 Delphi unit
  Copyright (c) 2003-2020 Un4seen Developments Ltd.

  See the BASSENC.CHM file for more detailed documentation

  NOTE: Delphi users should use the BASS_UNICODE flag where possible
}

{
TDDung - Vietnam, May 2021
}

unit BassEnc;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Bass;

const
  // Additional error codes returned by BASS_ErrorGetCode
  BASS_ERROR_ACM_CANCEL         = 2000; // ACM codec selection cancelled
  BASS_ERROR_CAST_DENIED        = 2100; // access denied (invalid password)

  // Additional BASS_SetConfig options
  BASS_CONFIG_ENCODE_PRIORITY   = $10300; // encoder DSP priority
  BASS_CONFIG_ENCODE_QUEUE      = $10301;
  BASS_CONFIG_ENCODE_ACM_LOAD   = $10302;

  // Additional BASS_SetConfigPtr options
  BASS_CONFIG_ENCODE_CAST_TIMEOUT = $10310; // cast timeout
  BASS_CONFIG_ENCODE_CAST_PROXY = $10311;

  // BASS_Encode_Start flags
  BASS_ENCODE_NOHEAD            = 1;	// don't send a WAV header to the encoder
  BASS_ENCODE_FP_8BIT           = 2;	// convert floating-point sample data to 8-bit integer
  BASS_ENCODE_FP_16BIT          = 4;	// convert floating-point sample data to 16-bit integer
  BASS_ENCODE_FP_24BIT          = 6;	// convert floating-point sample data to 24-bit integer
  BASS_ENCODE_FP_32BIT          = 8;	// convert floating-point sample data to 32-bit integer
  BASS_ENCODE_FP_AUTO           = 14;	// convert floating-point sample data back to channel's format
  BASS_ENCODE_BIGEND            = 16;	// big-endian sample data
  BASS_ENCODE_PAUSE             = 32;	// start encording paused
  BASS_ENCODE_PCM               = 64;	// write PCM sample data (no encoder)
  BASS_ENCODE_RF64              = 128;	// send an RF64 header
  BASS_ENCODE_MONO              = $100; // convert to mono (if not already)
  BASS_ENCODE_QUEUE             = $200; // queue data to feed encoder asynchronously
  BASS_ENCODE_WFEXT             = $400; // WAVEFORMATEXTENSIBLE "fmt" chunk
  BASS_ENCODE_CAST_NOLIMIT      = $1000; // don't limit casting data rate
  BASS_ENCODE_LIMIT             = $2000; // limit data rate to real-time
  BASS_ENCODE_AIFF              = $4000; // send an AIFF header rather than WAV
  BASS_ENCODE_DITHER            = $8000; // apply dither when converting floating-point sample data to integer
  BASS_ENCODE_AUTOFREE          = $40000; // free the encoder when the channel is freed

  // BASS_Encode_GetACMFormat flags
  BASS_ACM_DEFAULT              = 1; // use the format as default selection
  BASS_ACM_RATE                 = 2; // only list formats with same sample rate as the source channel
  BASS_ACM_CHANS                = 4; // only list formats with same number of channels (eg. mono/stereo)
  BASS_ACM_SUGGEST              = 8; // suggest a format (HIWORD=format tag)

  // BASS_Encode_GetCount counts
  BASS_ENCODE_COUNT_IN          = 0; // sent to encoder
  BASS_ENCODE_COUNT_OUT         = 1; // received from encoder
  BASS_ENCODE_COUNT_CAST        = 2; // sent to cast server
  BASS_ENCODE_COUNT_QUEUE       = 3; // queued
  BASS_ENCODE_COUNT_QUEUE_LIMIT = 4; // queue limit
  BASS_ENCODE_COUNT_QUEUE_FAIL  = 5; // failed to queue

  // BASS_Encode_CastInit content MIME types
  BASS_ENCODE_TYPE_MP3          = 'audio/mpeg';
  BASS_ENCODE_TYPE_OGG          = 'audio/ogg';
  BASS_ENCODE_TYPE_AAC          = 'audio/aacp';

  // BASS_Encode_CastGetStats types
  BASS_ENCODE_STATS_SHOUT       = 0; // Shoutcast stats
  BASS_ENCODE_STATS_ICE         = 1; // Icecast mount-point stats
  BASS_ENCODE_STATS_ICESERV     = 2; // Icecast server stats

  // Encoder notifications
  BASS_ENCODE_NOTIFY_ENCODER    = 1; // encoder died
  BASS_ENCODE_NOTIFY_CAST       = 2; // cast server connection died
  BASS_ENCODE_NOTIFY_CAST_TIMEOUT = $10000; // cast timeout
  BASS_ENCODE_NOTIFY_QUEUE_FULL = $10001; // queue is out of space
  BASS_ENCODE_NOTIFY_FREE       = $10002; // encoder has been freed

  // BASS_Encode_ServerInit flags
  BASS_ENCODE_SERVER_NOHTTP     = 1; // no HTTP headers
  BASS_ENCODE_SERVER_META       = 2; // Shoutcast metadata

type
  //Cardinal = LongWord;
  HENCODE = Cardinal; // encoder handle

  ENCODEPROC = procedure(handle:HENCODE; channel:HCHANNEL; buffer:Pointer; length:Cardinal; user:Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Encoding callback function.
    handle : The encoder
    channel: The channel handle
    buffer : Buffer containing the encoded data
    length : Number of bytes
    user   : The 'user' parameter value given when starting the encoder (when calling BASS_EncodeStart)
  }

  ENCODEPROCEX = procedure(handle:HENCODE; channel:HCHANNEL; buffer:Pointer; length:Cardinal; offset:QWORD; user:Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Encoding callback function with offset info.
    handle : The encoder
    channel: The channel handle
    buffer : Buffer containing the encoded data
    length : Number of bytes
    offset : File offset of the data
    user   : The 'user' parameter value given when starting the encoder (when calling BASS_Encode_StartCA)
  }

  ENCODERPROC = function(handle:HENCODE; channel:HCHANNEL; buffer:Pointer; length:Cardinal; maxout:Cardinal; user:Pointer): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Encoder callback function.
    handle : The encoder
    channel: The channel handle
    buffer : Buffer containing the PCM data (input) and receiving the encoded data (output)
    length : Number of bytes in (-1=closing)
    maxout : Maximum number of bytes out
    user   : The 'user' parameter value given when calling BASS_Encode_StartUser
    RETURN : The amount of encoded data (-1=stop)
  }

  ENCODECLIENTPROC = function(handle:HENCODE; connect:BOOL; client:PAnsiChar; headers:PAnsiChar; user:Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Client connection notification callback function.
    handle : The encoder
    connect: TRUE/FALSE=client is connecting/disconnecting
    client : The client's address (xxx.xxx.xxx.xxx:port)
    headers: Request headers (optionally response headers on return)
    user   : The 'user' parameter value given when calling BASS_Encode_ServerInit
    RETURN : TRUE/FALSE=accept/reject connection (ignored if connect=FALSE)
  }

  ENCODENOTIFYPROC = procedure(handle:HENCODE; status:Cardinal; user:Pointer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Encoder death notification callback function.
    handle : The encoder
    status : Notification (BASS_ENCODE_NOTIFY_xxx)
    user   : The 'user' parameter value given when calling BASS_Encode_SetNotify
  }

const
{$IFDEF MSWINDOWS}
  BassEncDLL = 'bassenc.dll';
{$ENDIF}

{$IFDEF LINUX}
  BassEncDLL = 'libbassenc.so';
{$ENDIF}

{$IFDEF MACOS}
  {$IFDEF IOS} // Needed frameworks: AudioToolbox, SystemConfiguration, CFNetwork, Accelerate, CoreMIDI (if using BASSMIDI)
    BassEncDLL = 'Bass\Add-ons\Libs\iOS\libbassenc.a'; // Full path for static linking
  {$ELSE}
    BassEncDLL = 'libbassenc.dylib';
  {$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
  BassEncDLL = 'libbassenc.so';
{$ENDIF}

{$IFDEF IOS}
  function BASS_Encode_GetVersion: Cardinal; cdecl; external bassencdll;
  function BASS_Encode_Start(handle:Cardinal; cmdline:PChar; flags:Cardinal; proc:ENCODEPROC; user:Pointer): HENCODE; cdecl; external bassencdll;
  function BASS_Encode_StartLimit(handle:Cardinal; cmdline:PChar; flags:Cardinal; proc:ENCODEPROC; user:Pointer; limit:Cardinal): HENCODE; cdecl; external bassencdll;
  function BASS_Encode_StartUser(handle:Cardinal; filename:PChar; flags:Cardinal; proc:ENCODERPROC; user:Pointer): HENCODE; cdecl; external bassencdll;
  function BASS_Encode_AddChunk(handle:HENCODE; id:PAnsiChar; buffer:Pointer; length:Cardinal): BOOL; cdecl; external bassencdll;
  function BASS_Encode_IsActive(handle:Cardinal): Cardinal; cdecl; external bassencdll;
  function BASS_Encode_Stop(handle:Cardinal): BOOL; cdecl; external bassencdll;
  function BASS_Encode_StopEx(handle:Cardinal; queue:BOOL): BOOL; cdecl; external bassencdll;
  function BASS_Encode_SetPaused(handle:Cardinal; paused:BOOL): BOOL; cdecl; external bassencdll;
  function BASS_Encode_Write(handle:Cardinal; buffer:Pointer; length:Cardinal): BOOL; cdecl; external bassencdll;
  function BASS_Encode_SetNotify(handle:Cardinal; proc:ENCODENOTIFYPROC; user:Pointer): BOOL; cdecl; external bassencdll;
  function BASS_Encode_GetCount(handle:HENCODE; count:Cardinal): QWORD; cdecl; external bassencdll;
  function BASS_Encode_SetChannel(handle:Cardinal; channel:Cardinal): BOOL; cdecl; external bassencdll;
  function BASS_Encode_GetChannel(handle:HENCODE): Cardinal; cdecl; external bassencdll;
  function BASS_Encode_UserOutput(handle:HENCODE; offset:QWORD; buffer:Pointer; length:Cardinal): BOOL; cdecl; external bassencdll;

  function BASS_Encode_StartCA(handle,ftype,atype,flags,bitrate:Cardinal; proc:ENCODEPROCEX; user:Pointer): HENCODE; cdecl; external bassencdll;
  function BASS_Encode_StartCAFile(handle,ftype,atype,flags,bitrate:Cardinal; filename:PChar): HENCODE; cdecl; external bassencdll;
  function BASS_Encode_GetCARef(handle:HENCODE): Pointer; cdecl; external bassencdll;

  function BASS_Encode_CastInit(handle:HENCODE; server,pass,content,name,url,genre,desc,headers:PAnsiChar; bitrate:Cardinal; pub:BOOL): BOOL; cdecl; external bassencdll;
  function BASS_Encode_CastSetTitle(handle:HENCODE; title,url:PAnsiChar): BOOL; cdecl; external bassencdll;
  function BASS_Encode_CastSendMeta(handle:HENCODE; mtype:Cardinal; data:Pointer; length:Cardinal): BOOL; cdecl; external bassencdll;
  function BASS_Encode_CastGetStats(handle:HENCODE; stype:Cardinal; pass:PAnsiChar): PAnsiChar; cdecl; external bassencdll;

  function BASS_Encode_ServerInit(handle:HENCODE; port:PAnsiChar; buffer,burst,flags:Cardinal; proc:ENCODECLIENTPROC; user:Pointer): Cardinal; cdecl; external bassencdll;
  function BASS_Encode_ServerKick(handle:HENCODE; client:PAnsiChar): BOOL; cdecl; external bassencdll;
{$ELSE}
var
  BASS_Encode_GetVersion:function : Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_Start:function(handle:Cardinal; cmdline:PChar; flags:Cardinal; proc:ENCODEPROC; user:Pointer): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_StartLimit:function(handle:Cardinal; cmdline:PChar; flags:Cardinal; proc:ENCODEPROC; user:Pointer; limit:Cardinal): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_StartUser:function(handle:Cardinal; filename:PChar; flags:Cardinal; proc:ENCODERPROC; user:Pointer): HENCODE; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_AddChunk:function(handle:HENCODE; id:PAnsiChar; buffer:Pointer; length:Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_IsActive:function(handle:Cardinal): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_Stop:function(handle:Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_StopEx:function(handle:Cardinal; queue:BOOL): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_SetPaused:function(handle:Cardinal; paused:BOOL): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_Write:function(handle:Cardinal; buffer:Pointer; length:Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_SetNotify:function(handle:Cardinal; proc:ENCODENOTIFYPROC; user:Pointer): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_GetCount:function(handle:HENCODE; count:Cardinal): QWORD; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_SetChannel:function(handle:Cardinal; channel:Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_GetChannel:function(handle:HENCODE): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_UserOutput:function(handle:HENCODE; offset:QWORD; buffer:Pointer; length:Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  {$IFDEF MSWINDOWS}
  BASS_Encode_GetACMFormat:function(handle:Cardinal; form:Pointer; formlen:Cardinal; title:PAnsiChar; flags:Cardinal): Cardinal; stdcall;
  BASS_Encode_StartACM:function(handle:Cardinal; form:Pointer; flags:Cardinal; proc:ENCODEPROC; user:Pointer): HENCODE; stdcall;
  BASS_Encode_StartACMFile:function(handle:Cardinal; form:Pointer; flags:Cardinal; filename:PChar): HENCODE; stdcall;
  {$ENDIF}

  {$IFDEF MACOS}
  BASS_Encode_StartCA:function(handle,ftype,atype,flags,bitrate:Cardinal; proc:ENCODEPROCEX; user:Pointer): HENCODE; cdecl;
  BASS_Encode_StartCAFile:function(handle,ftype,atype,flags,bitrate:Cardinal; filename:PChar): HENCODE; cdecl;
  BASS_Encode_GetCARef:function(handle:HENCODE): Pointer; cdecl;
  {$ENDIF}

  BASS_Encode_CastInit:function(handle:HENCODE; server,pass,content,name,url,genre,desc,headers:PAnsiChar; bitrate:Cardinal; pub:BOOL): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_CastSetTitle:function(handle:HENCODE; title,url:PAnsiChar): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_CastSendMeta:function(handle:HENCODE; mtype:Cardinal; data:Pointer; length:Cardinal): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_CastGetStats:function(handle:HENCODE; stype:Cardinal; pass:PAnsiChar): PAnsiChar; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  BASS_Encode_ServerInit:function(handle:HENCODE; port:PAnsiChar; buffer,burst,flags:Cardinal; proc:ENCODECLIENTPROC; user:Pointer): Cardinal; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  BASS_Encode_ServerKick:function(handle:HENCODE; client:PAnsiChar): BOOL; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

var
  FBassEncDLL: THandle = 0;
{$ENDIF}

function BassEnc_IsAvailable: Boolean;

implementation

(*{$IFNDEF MSWINDOWS}
uses
  System.IOUtils;
{$ENDIF}*)

function BassEnc_IsAvailable: Boolean;
begin
  {$IFDEF IOS}
  Result := True;
  {$ELSE}
  Result := FBassEncDLL <> 0;
  {$ENDIF}
end;

{$IFNDEF IOS}
procedure LoadBassEncDLL;
begin
  FBassEncDLL := LoadLibrary(PChar(BassEncDLL));
  if FBassEncDLL = 0 then
    Exit;

  BASS_Encode_GetVersion:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_GetVersion'));
  BASS_Encode_Start:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_Start'));
  BASS_Encode_StartLimit:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_StartLimit'));
  BASS_Encode_StartUser:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_StartUser'));
  BASS_Encode_AddChunk:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_AddChunk'));
  BASS_Encode_IsActive:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_IsActive'));
  BASS_Encode_Stop:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_Stop'));
  BASS_Encode_StopEx:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_StopEx'));
  BASS_Encode_SetPaused:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_SetPaused'));
  BASS_Encode_Write:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_Write'));
  BASS_Encode_SetNotify:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_SetNotify'));
  BASS_Encode_GetCount:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_GetCount'));
  BASS_Encode_SetChannel:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_SetChannel'));
  BASS_Encode_GetChannel:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_GetChannel'));
  BASS_Encode_UserOutput:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_UserOutput'));

  {$IFDEF MSWINDOWS}
  BASS_Encode_GetACMFormat:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_GetACMFormat'));
  BASS_Encode_StartACM:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_StartACM'));
  BASS_Encode_StartACMFile:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_StartACMFile'));
  {$ENDIF}

  {$IFDEF MACOS}
  BASS_Encode_StartCA:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_StartCA'));
  BASS_Encode_StartCAFile:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_StartCAFile'));
  BASS_Encode_GetCARef:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_StartCARef'));
  {$ENDIF}

  BASS_Encode_CastInit:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_CastInit'));
  BASS_Encode_CastSetTitle:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_CastSetTitle'));
  BASS_Encode_CastSendMeta:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_CastSendMeta'));
  BASS_Encode_CastGetStats:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_CastGetStats'));

  BASS_Encode_ServerInit:= GetProcAddress(FBassEncDLL, PChar('BASS_Encode_ServerInit'));
  BASS_Encode_ServerKick:= GetProcAddress(FBassEncDLL, PChar( 'BASS_Encode_ServerKick'));
end;

procedure UnloadBassEncDLL;
begin
  FreeLibrary(FBassEncDLL);
end;
{$ENDIF}

initialization
  {$IFNDEF IOS}
  LoadBassEncDLL;
  {$ENDIF}
finalization
  {$IFNDEF IOS}
  UnloadBassEncDLL;
  {$ENDIF}
end.
