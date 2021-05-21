program TestVCL;

uses
  Vcl.Forms,
  MainVCL in 'MainVCL.pas' {Form2},
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
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
