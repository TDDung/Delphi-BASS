program TestFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainFMX in 'MainFMX.pas' {Form1},
  Bass in 'BASS\Core\Bass.pas',
  BassEnc in 'BASS\Add-ons\BassEnc.pas',
  BassEnc_FLAC in 'BASS\Add-ons\BassEnc_FLAC.pas',
  BassEnc_MP3 in 'BASS\Add-ons\BassEnc_MP3.pas',
  BassEnc_OGG in 'BASS\Add-ons\BassEnc_OGG.pas',
  BassEnc_OPUS in 'BASS\Add-ons\BassEnc_OPUS.pas',
  BassFLAC in 'BASS\Add-ons\BassFLAC.pas',
  BassOPUS in 'BASS\Add-ons\BassOPUS.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
