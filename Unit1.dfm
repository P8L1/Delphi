object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Multi-Currency Converter'
  ClientHeight = 250
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblPrompt: TLabel
    Left = 16
    Top = 16
    Width = 70
    Height = 13
    Caption = 'Enter Amount:'
  end
  object lblSource: TLabel
    Left = 16
    Top = 48
    Width = 84
    Height = 13
    Caption = 'Source Currency:'
  end
  object lblTarget: TLabel
    Left = 240
    Top = 48
    Width = 83
    Height = 13
    Caption = 'Target Currency:'
  end
  object lblResult: TLabel
    Left = 120
    Top = 120
    Width = 34
    Height = 13
    Caption = 'Result:'
  end
  object edtAmount: TEdit
    Left = 120
    Top = 12
    Width = 100
    Height = 21
    TabOrder = 0
  end
  object cbSource: TComboBox
    Left = 120
    Top = 44
    Width = 100
    Height = 21
    Style = csDropDownList
    TabOrder = 1
  end
  object cbTarget: TComboBox
    Left = 340
    Top = 44
    Width = 100
    Height = 21
    Style = csDropDownList
    TabOrder = 2
  end
  object btnConvert: TButton
    Left = 120
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Convert'
    TabOrder = 3
    OnClick = btnConvertClick
  end
end
