unit MainVCL;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Label2: TLabel;
    Button2: TButton;
    Label3: TLabel;
    Button3: TButton;
    Label4: TLabel;
    Button4: TButton;
    Label5: TLabel;
    Button5: TButton;
    Label6: TLabel;
    Button6: TButton;
    Label7: TLabel;
    Button7: TButton;
    Label8: TLabel;
    Button8: TButton;
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
  Form2: TForm2;

implementation

{$R *.dfm}

uses StrUtils, Bass, BassEnc, BassEnc_FLAC, BassEnc_MP3, BassEnc_OGG, BassEnc_OPUS, BassFLAC, BassOPUS;

function VersionToString(LibName: string; LibVersion: DWORD): string;
begin
  Result := LibName
    {$IFDEF WIN32}
    + ' 32-bit '
    {$ELSE}
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
    {$IFDEF WIN32}
    + ' 32-bit '
    {$ELSE}
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
  Plugin := BASS_PluginLoad(LibDLL, BASS_UNICODE);
  if Plugin <> 0 then
  begin
    Info := BASS_PluginGetInfo(Plugin);
    if Assigned(Info) then
      Version := Info.version;
    BASS_PluginFree(Plugin);
  end;
  Result := LibName + IfThen(Version <> 0, VersionToString('', Version), ' not available');
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  if BASS_IsAvailable then
    Label1.Caption:= VersionToString('BASS', BASS_GetVersion)
  else
    Label1.Caption:= 'BASS is not available';
  (Sender as TButton).Enabled := False;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  if BassEnc_IsAvailable then
    Label2.Caption:= VersionToString('BassEnc', BASS_Encode_GetVersion)
  else
    Label2.Caption:= 'BassEnc is not available';
  (Sender as TButton).Enabled := False;
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  if BassEncFLAC_IsAvailable then
    Label3.Caption:= VersionToString('BassEnc_FLAC', BASS_Encode_FLAC_GetVersion)
  else
    Label3.Caption:= 'BassEnc_FLAC is not available';
  (Sender as TButton).Enabled := False;
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  if BassEncMP3_IsAvailable then
    Label4.Caption:= VersionToString('BassEnc_MP3', BASS_Encode_MP3_GetVersion)
  else
    Label4.Caption:= 'BassEnc_MP3 is not available';
  (Sender as TButton).Enabled := False;
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  if BassEncOGG_IsAvailable then
    Label5.Caption:= VersionToString('BassEnc_OGG', BASS_Encode_OGG_GetVersion)
  else
    Label5.Caption:= 'BassEnc_OGG is not available';
  (Sender as TButton).Enabled := False;
end;

procedure TForm2.Button6Click(Sender: TObject);
begin
  if BassEncOPUS_IsAvailable then
    Label6.Caption:= VersionToString('BassEnc_OPUS', BASS_Encode_OPUS_GetVersion)
  else
    Label6.Caption:= 'BassEnc_OPUS is not available';
  (Sender as TButton).Enabled := False;
end;

procedure TForm2.Button7Click(Sender: TObject);
begin
  Label7.Caption := AvailabilityToString('BassFLAC (add-on)', BassFLAC_IsAvailable)
    + sLineBreak + PluginVersionToString('BassFLAC (plugin)', BassFlacDLL);
  (Sender as TButton).Enabled := False;
end;

procedure TForm2.Button8Click(Sender: TObject);
begin
  Label8.Caption := AvailabilityToString('BassOPUS (add-on)', BassOPUS_IsAvailable)
    + sLineBreak + PluginVersionToString('BassOPUS (plugin)', BassOpusDLL);
  (Sender as TButton).Enabled := False;
end;

end.
