object Form1: TForm1
  Left = 188
  Top = 107
  Width = 431
  Height = 290
  Caption = 'TC64 Delphi class demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 320
    Height = 200
  end
  object Label1: TLabel
    Left = 8
    Top = 216
    Width = 72
    Height = 13
    Caption = '320x200 image'
  end
  object BitBtn1: TBitBtn
    Left = 336
    Top = 8
    Width = 75
    Height = 25
    Caption = 'KOALA'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 336
    Top = 40
    Width = 75
    Height = 25
    Caption = 'HIRES'
    TabOrder = 1
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 336
    Top = 72
    Width = 75
    Height = 25
    Caption = 'AMICA'
    TabOrder = 2
    OnClick = BitBtn3Click
  end
  object BitBtn4: TBitBtn
    Left = 336
    Top = 104
    Width = 75
    Height = 25
    Caption = 'LOGO'
    TabOrder = 3
    OnClick = BitBtn4Click
  end
  object BitBtn5: TBitBtn
    Left = 336
    Top = 136
    Width = 75
    Height = 25
    Caption = 'FONT'
    TabOrder = 4
    OnClick = BitBtn5Click
  end
  object BitBtn6: TBitBtn
    Left = 336
    Top = 168
    Width = 75
    Height = 25
    Caption = 'FONT 2x2'
    TabOrder = 5
    OnClick = BitBtn6Click
  end
  object BitBtnSave: TBitBtn
    Left = 248
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Save img...'
    TabOrder = 6
    OnClick = BitBtnSaveClick
  end
  object BitBtnAbout: TBitBtn
    Left = 88
    Top = 232
    Width = 75
    Height = 25
    Caption = 'About'
    TabOrder = 7
    OnClick = BitBtnAboutClick
  end
  object BitBtn7: TBitBtn
    Left = 336
    Top = 200
    Width = 75
    Height = 25
    Caption = 'MOB'
    TabOrder = 8
    OnClick = BitBtn7Click
  end
  object BitBtnLoad: TBitBtn
    Left = 168
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Load...'
    TabOrder = 9
    OnClick = BitBtnLoadClick
  end
  object BitBtn8: TBitBtn
    Left = 336
    Top = 232
    Width = 75
    Height = 25
    Caption = 'MFB'
    TabOrder = 10
    OnClick = BitBtn8Click
  end
  object SaveDialog1: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 24
    Top = 168
  end
  object OpenDialog1: TOpenDialog
    Left = 24
    Top = 136
  end
end
