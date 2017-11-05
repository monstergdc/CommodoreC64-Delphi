object Form1: TForm1
  Left = 188
  Top = 107
  Width = 618
  Height = 262
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
    Top = 24
    Width = 320
    Height = 200
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 72
    Height = 13
    Caption = '320x200 image'
  end
  object BitBtnSave: TBitBtn
    Left = 432
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Save img...'
    TabOrder = 0
    OnClick = BitBtnSaveClick
  end
  object BitBtnAbout: TBitBtn
    Left = 512
    Top = 192
    Width = 75
    Height = 25
    Caption = 'About'
    TabOrder = 1
    OnClick = BitBtnAboutClick
  end
  object BitBtnLoad: TBitBtn
    Left = 352
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Load...'
    TabOrder = 2
    OnClick = BitBtnLoadClick
  end
  object GroupBox1: TGroupBox
    Left = 344
    Top = 8
    Width = 257
    Height = 177
    Caption = 'Predefined quick exampled'
    TabOrder = 3
    object BitBtn1: TBitBtn
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'KOALA'
      TabOrder = 0
      OnClick = BitBtn1Click
    end
    object BitBtn2: TBitBtn
      Left = 8
      Top = 48
      Width = 75
      Height = 25
      Caption = 'HIRES'
      TabOrder = 1
      OnClick = BitBtn2Click
    end
    object BitBtn3: TBitBtn
      Left = 8
      Top = 80
      Width = 75
      Height = 25
      Caption = 'AMICA'
      TabOrder = 2
      OnClick = BitBtn3Click
    end
    object BitBtn4: TBitBtn
      Left = 88
      Top = 16
      Width = 75
      Height = 25
      Caption = 'LOGO'
      TabOrder = 3
      OnClick = BitBtn4Click
    end
    object BitBtn5: TBitBtn
      Left = 88
      Top = 48
      Width = 75
      Height = 25
      Caption = 'FONT'
      TabOrder = 4
      OnClick = BitBtn5Click
    end
    object BitBtn6: TBitBtn
      Left = 88
      Top = 80
      Width = 75
      Height = 25
      Caption = 'FONT 2x2'
      TabOrder = 5
      OnClick = BitBtn6Click
    end
    object BitBtn7: TBitBtn
      Left = 88
      Top = 112
      Width = 75
      Height = 25
      Caption = 'MOB'
      TabOrder = 6
      OnClick = BitBtn7Click
    end
    object BitBtn8: TBitBtn
      Left = 88
      Top = 144
      Width = 75
      Height = 25
      Caption = 'MFB'
      TabOrder = 7
      OnClick = BitBtn8Click
    end
    object BitBtn9: TBitBtn
      Left = 168
      Top = 16
      Width = 75
      Height = 25
      Caption = 'IFLI?'
      Enabled = False
      TabOrder = 8
      OnClick = BitBtn9Click
    end
    object BitBtn10: TBitBtn
      Left = 168
      Top = 48
      Width = 75
      Height = 25
      Caption = 'BFLI'
      TabOrder = 9
      OnClick = BitBtn10Click
    end
    object BitBtn11: TBitBtn
      Left = 168
      Top = 80
      Width = 75
      Height = 25
      Caption = 'FFLI?'
      Enabled = False
      TabOrder = 10
      OnClick = BitBtn11Click
    end
    object BitBtn12: TBitBtn
      Left = 168
      Top = 112
      Width = 75
      Height = 25
      Caption = 'FLI'
      TabOrder = 11
      OnClick = BitBtn12Click
    end
    object BitBtn13: TBitBtn
      Left = 168
      Top = 144
      Width = 75
      Height = 25
      Caption = 'AFLI?'
      Enabled = False
      TabOrder = 12
      OnClick = BitBtn13Click
    end
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
