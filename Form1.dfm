object Form1: TForm1
  Caption = 'Rand to Dollar Converter'
  ClientHeight = 200
  ClientWidth = 400
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  object lblPrompt: TLabel
    Left = 16
    Top = 24
    Width = 140
    Height = 13
    Caption = 'Enter amount in Rands:'
    Name = 'lblPrompt'
  end
  object edtRand: TEdit
    Left = 160
    Top = 20
    Width = 200
    Height = 21
    Name = 'edtRand'
    TabOrder = 0
  end
  object btnConvert: TButton
    Left = 160
    Top = 60
    Width = 75
    Height = 25
    Caption = 'Convert'
    OnClick = btnConvertClick
    Name = 'btnConvert'
    TabOrder = 1
  end
  object lblResult: TLabel
    Left = 160
    Top = 100
    Width = 200
    Height = 13
    Caption = 'USD: '
    Name = 'lblResult'
  end
end
