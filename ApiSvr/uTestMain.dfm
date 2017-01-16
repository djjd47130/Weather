object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Weather API Test'
  ClientHeight = 382
  ClientWidth = 759
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pTop: TPanel
    Left = 0
    Top = 0
    Width = 759
    Height = 73
    Align = alTop
    TabOrder = 0
    ExplicitLeft = 24
    ExplicitTop = 16
    ExplicitWidth = 497
    object btnStop: TBitBtn
      Left = 8
      Top = 40
      Width = 89
      Height = 25
      Caption = 'Stop'
      Enabled = False
      TabOrder = 1
      OnClick = btnStopClick
    end
    object btnStart: TBitBtn
      Left = 8
      Top = 9
      Width = 89
      Height = 25
      Caption = 'Start'
      TabOrder = 0
      OnClick = btnStartClick
    end
  end
  object Log: TMemo
    Left = 0
    Top = 73
    Width = 759
    Height = 209
    Align = alTop
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object DB: TADOConnection
    Left = 168
    Top = 312
  end
end
