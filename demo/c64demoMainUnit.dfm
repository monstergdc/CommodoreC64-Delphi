object Form1: TForm1
  Left = 188
  Top = 107
  BorderStyle = bsDialog
  Caption = 'TC64 Delphi class demo'
  ClientHeight = 429
  ClientWidth = 610
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
    Height = 400
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 128
    Height = 13
    Caption = '320x200 or 320x400 image'
  end
  object BitBtnSave: TBitBtn
    Left = 432
    Top = 224
    Width = 75
    Height = 25
    Caption = 'Save img...'
    TabOrder = 0
    OnClick = BitBtnSaveClick
  end
  object BitBtnAbout: TBitBtn
    Left = 512
    Top = 224
    Width = 75
    Height = 25
    Caption = 'About'
    TabOrder = 1
    OnClick = BitBtnAboutClick
  end
  object BitBtnLoad: TBitBtn
    Left = 352
    Top = 224
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
    Height = 209
    Caption = 'Predefined quick examples'
    TabOrder = 3
    object BitBtnKOALA: TBitBtn
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'KOALA'
      TabOrder = 0
      OnClick = BitBtnKOALAClick
    end
    object BitBtnHIRES: TBitBtn
      Left = 8
      Top = 48
      Width = 75
      Height = 25
      Caption = 'HIRES'
      TabOrder = 1
      OnClick = BitBtnHIRESClick
    end
    object BitBtnAMICA: TBitBtn
      Left = 8
      Top = 80
      Width = 75
      Height = 25
      Caption = 'AMICA'
      TabOrder = 2
      OnClick = BitBtnAMICAClick
    end
    object BitBtnLOGO: TBitBtn
      Left = 88
      Top = 16
      Width = 75
      Height = 25
      Caption = 'LOGO'
      TabOrder = 5
      OnClick = BitBtnLOGOClick
    end
    object BitBtnFONT: TBitBtn
      Left = 88
      Top = 48
      Width = 75
      Height = 25
      Caption = 'FONT 1x1'
      TabOrder = 6
      OnClick = BitBtnFONTClick
    end
    object BitBtnFNTB: TBitBtn
      Left = 88
      Top = 80
      Width = 75
      Height = 25
      Caption = 'FONT 2x2'
      TabOrder = 7
      OnClick = BitBtnFNTBClick
    end
    object BitBtnMOBM: TBitBtn
      Left = 88
      Top = 112
      Width = 75
      Height = 25
      Caption = 'MOB M'
      TabOrder = 8
      OnClick = BitBtnMOBMClick
    end
    object BitBtn8: TBitBtn
      Left = 88
      Top = 176
      Width = 75
      Height = 25
      Caption = 'MFB'
      TabOrder = 10
      OnClick = BitBtn8Click
    end
    object BitBtn9: TBitBtn
      Left = 168
      Top = 16
      Width = 75
      Height = 25
      Caption = 'IFLI?'
      Enabled = False
      TabOrder = 11
      OnClick = BitBtn9Click
    end
    object BitBtnBFLI: TBitBtn
      Left = 168
      Top = 48
      Width = 75
      Height = 25
      Caption = 'BFLI'
      TabOrder = 12
      OnClick = BitBtnBFLIClick
    end
    object BitBtn11: TBitBtn
      Left = 168
      Top = 80
      Width = 75
      Height = 25
      Caption = 'FFLI'
      TabOrder = 13
      OnClick = BitBtn11Click
    end
    object BitBtnFLI: TBitBtn
      Left = 168
      Top = 112
      Width = 75
      Height = 25
      Caption = 'FLI'
      TabOrder = 14
      OnClick = BitBtnFLIClick
    end
    object BitBtnAFLI: TBitBtn
      Left = 168
      Top = 144
      Width = 75
      Height = 25
      Caption = 'AFLI'
      TabOrder = 15
      OnClick = BitBtnAFLIClick
    end
    object BitBtnHIEDDI: TBitBtn
      Left = 8
      Top = 112
      Width = 75
      Height = 25
      Caption = 'HI-EDDI'
      TabOrder = 3
      OnClick = BitBtnHIEDDIClick
    end
    object BitBtnDDL: TBitBtn
      Left = 8
      Top = 144
      Width = 75
      Height = 25
      Caption = 'DDL'
      TabOrder = 4
      OnClick = BitBtnDDLClick
    end
    object BitBtnMOBH: TBitBtn
      Left = 88
      Top = 144
      Width = 75
      Height = 25
      Caption = 'MOB H'
      TabOrder = 9
      OnClick = BitBtnMOBHClick
    end
  end
  object RadioGroup1: TRadioGroup
    Left = 352
    Top = 256
    Width = 97
    Height = 169
    Caption = 'Palette'
    ItemIndex = 0
    Items.Strings = (
      'C64S'
      'CCS64S'
      'FRODO'
      'GODOT'
      'PC64'
      'VICE'
      'C64HQ'
      'OLDVICE'
      'VICEDFLT')
    TabOrder = 4
    OnClick = RadioGroup1Click
  end
  object GroupBox2: TGroupBox
    Left = 456
    Top = 256
    Width = 145
    Height = 169
    Caption = 'Four colors + mode'
    TabOrder = 5
    object Label2: TLabel
      Left = 16
      Top = 28
      Width = 20
      Height = 13
      Caption = 'C#0'
    end
    object Label3: TLabel
      Left = 16
      Top = 52
      Width = 20
      Height = 13
      Caption = 'C#1'
    end
    object Label4: TLabel
      Left = 16
      Top = 76
      Width = 20
      Height = 13
      Caption = 'C#2'
    end
    object Label5: TLabel
      Left = 16
      Top = 100
      Width = 20
      Height = 13
      Caption = 'C#3'
    end
    object ComboBoxC0: TComboBox
      Left = 48
      Top = 24
      Width = 89
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object ComboBoxC1: TComboBox
      Left = 48
      Top = 48
      Width = 89
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
    end
    object ComboBoxC2: TComboBox
      Left = 48
      Top = 72
      Width = 89
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
    end
    object ComboBoxC3: TComboBox
      Left = 48
      Top = 96
      Width = 89
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
    end
    object cbHires: TCheckBox
      Left = 8
      Top = 128
      Width = 97
      Height = 17
      Caption = 'As Hires?'
      TabOrder = 4
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
