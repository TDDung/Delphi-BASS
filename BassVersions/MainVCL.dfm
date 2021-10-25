object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'TestVCL'
  ClientHeight = 568
  ClientWidth = 355
  Color = clSkyBlue
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 78
    Top = 24
    Width = 200
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'BASS version'
  end
  object Label2: TLabel
    Left = 78
    Top = 88
    Width = 200
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'BassEnc version'
  end
  object Label3: TLabel
    Left = 78
    Top = 152
    Width = 200
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'BassEnc_FLAC version'
  end
  object Label4: TLabel
    Left = 78
    Top = 216
    Width = 200
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'BassEnc_MP3 version'
  end
  object Label5: TLabel
    Left = 78
    Top = 280
    Width = 200
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'BassEnc_OGG version'
  end
  object Label6: TLabel
    Left = 78
    Top = 344
    Width = 200
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'BassEnc_OPUS version'
  end
  object Label7: TLabel
    Left = 78
    Top = 408
    Width = 200
    Height = 26
    Alignment = taCenter
    AutoSize = False
    Caption = 'BassFLAC (add-on) availability + BassFLAC (plugin) version'
    WordWrap = True
  end
  object Label8: TLabel
    Left = 78
    Top = 486
    Width = 200
    Height = 26
    Alignment = taCenter
    AutoSize = False
    Caption = 'BassOPUS (add-on) availability + BassOPUS (plugin) version'
    WordWrap = True
  end
  object Button1: TButton
    Left = 78
    Top = 43
    Width = 200
    Height = 25
    Caption = 'Get BASS version'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 78
    Top = 107
    Width = 200
    Height = 25
    Caption = 'Get BassEnc version'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 78
    Top = 171
    Width = 200
    Height = 25
    Caption = 'Get BassEnc_FLAC version'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 78
    Top = 235
    Width = 200
    Height = 25
    Caption = 'Get BassEnc_MP3 version'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 78
    Top = 299
    Width = 200
    Height = 25
    Caption = 'Get BassEnc_OGG version'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 78
    Top = 363
    Width = 200
    Height = 25
    Caption = 'Get BassEnc_OPUS version'
    TabOrder = 5
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 78
    Top = 441
    Width = 200
    Height = 25
    Caption = 'Get BassFLAC version'
    TabOrder = 6
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 78
    Top = 519
    Width = 200
    Height = 25
    Caption = 'Get BassOPUS version'
    TabOrder = 7
    OnClick = Button8Click
  end
end
