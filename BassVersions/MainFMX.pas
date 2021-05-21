unit MainFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Button2: TButton;
    Label2: TLabel;
    Button3: TButton;
    Label3: TLabel;
    Button4: TButton;
    Label4: TLabel;
    Button5: TButton;
    Label5: TLabel;
    Button6: TButton;
    Label6: TLabel;
    Button7: TButton;
    Label7: TLabel;
    Button8: TButton;
    Label8: TLabel;
    Rectangle1: TRectangle;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses StrUtils, Bass, BassEnc, BassEnc_FLAC, BassEnc_MP3, BassEnc_OGG, BassEnc_OPUS, BassFLAC, BassOPUS; {-framework AudioToolbox -framework CFNetwork}

function VersionToString(LibName: string; LibVersion: DWORD): string;
begin
  Result := LibName
    {$IF Defined(WIN32) or Defined(MACOS32) or Defined(LINUX32) or Defined(IOS32) or Defined(ANDROID32)}
    + ' 32-bit '
    {$ELSEIF Defined(WIN64) or Defined(MACOS64) or Defined(LINUX64) or Defined(IOS64) or Defined(ANDROID64)}
    + ' 64-bit '
    {$ENDIF}
    + (LibVersion shr 24).ToString
    + '.' + ((LibVersion shr 16) and $FF).ToString
    + '.' + ((LibVersion shr 8) and $FF).ToString
    + '.' + (LibVersion and $FF).ToString;
end;

function AvailabilityToString(LibName: string; LibAvailability: Boolean): string;
begin
  Result := LibName
    {$IF Defined(WIN32) or Defined(MACOS32) or Defined(LINUX32) or Defined(IOS32) or Defined(ANDROID32)}
    + ' 32-bit '
    {$ELSEIF Defined(WIN64) or Defined(MACOS64) or Defined(LINUX64) or Defined(IOS64) or Defined(ANDROID64)}
    + ' 64-bit '
    {$ENDIF}
    + IfThen(LibAvailability, 'OK', 'not available');
end;

function PluginVersionToString(LibName: string; LibDLL: PChar): string;
var
  Plugin: HPLUGIN;
  Info: PBASS_PLUGININFO;
  Version: DWORD;
begin
  Version:= 0;
  {$IFDEF IOS}
    Plugin := BASS_PluginLoad(LibDLL, 0);
  {$ELSE}
    Plugin := BASS_PluginLoad(LibDLL, BASS_UNICODE);
  {$ENDIF}
  if Plugin <> 0 then
  begin
    Info := BASS_PluginGetInfo(Plugin);
    if Assigned(Info) then
      Version := Info.version;
    BASS_PluginFree(Plugin);
  end;
  Result := LibName + IfThen(Version <> 0, VersionToString('', Version), ' not available');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if BASS_IsAvailable then
    Label1.Text:= VersionToString('BASS', BASS_GetVersion)
  else
    Label1.Text:= 'BASS is not available';
  (Sender as TButton).Enabled := False;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if BassEnc_IsAvailable then
    Label2.Text:= VersionToString('BassEnc', BASS_Encode_GetVersion)
  else
    Label2.Text:= 'BassEnc is not available';
  (Sender as TButton).Enabled := False;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if BassEncFLAC_IsAvailable then
    Label3.Text:= VersionToString('BassEnc_FLAC', BASS_Encode_FLAC_GetVersion)
  else
    Label3.Text:= 'BassEnc_FLAC is not available';
  (Sender as TButton).Enabled := False;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if BassEncMP3_IsAvailable then
    Label4.Text:= VersionToString('BassEnc_MP3', BASS_Encode_MP3_GetVersion)
  else
    Label4.Text:= 'BassEnc_MP3 is not available';
  (Sender as TButton).Enabled := False;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  if BassEncOGG_IsAvailable then
    Label5.Text:= VersionToString('BassEnc_OGG', BASS_Encode_OGG_GetVersion)
  else
    Label5.Text:= 'BassEnc_OGG is not available';
  (Sender as TButton).Enabled := False;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  if BassEncOPUS_IsAvailable then
    Label6.Text:= VersionToString('BassEnc_OPUS', BASS_Encode_OPUS_GetVersion)
  else
    Label6.Text:= 'BassEnc_OPUS is not available';
  (Sender as TButton).Enabled := False;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  {$IFDEF IOS}
  Label7.Text := AvailabilityToString('BassFLAC (add-on)', BassFLAC_IsAvailable)
    + sLineBreak + PluginVersionToString('BassFLAC (plugin)', @BASSFLACplugin);
  {$ELSE}
  Label7.Text := AvailabilityToString('BassFLAC (add-on)', BassFLAC_IsAvailable)
    + sLineBreak + PluginVersionToString('BassFLAC (plugin)', BassFlacDLL);
  {$ENDIF}
  (Sender as TButton).Enabled := False;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  {$IFDEF IOS}
  Label8.Text := AvailabilityToString('BassOPUS (add-on)', BassOPUS_IsAvailable)
    + sLineBreak + PluginVersionToString('BassOPUS (plugin)', @BASSOPUSplugin);
  {$ELSE}
  Label8.Text := AvailabilityToString('BassOPUS (add-on)', BassOPUS_IsAvailable)
    + sLineBreak + PluginVersionToString('BassOPUS (plugin)', BassOpusDLL);
  {$ENDIF}
  (Sender as TButton).Enabled := False;
end;

end.
