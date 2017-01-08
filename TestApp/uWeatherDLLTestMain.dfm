object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'JD Weather DLL Test'
  ClientHeight = 531
  ClientWidth = 845
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel4: TPanel
    Left = 0
    Top = 0
    Width = 845
    Height = 121
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 762
    object lstServices: TListView
      Left = 0
      Top = 0
      Width = 211
      Height = 121
      Align = alLeft
      BevelOuter = bvNone
      BevelKind = bkTile
      BorderStyle = bsNone
      Columns = <
        item
          AutoSize = True
          Caption = 'Service Name'
        end>
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ReadOnly = True
      RowSelect = True
      ParentFont = False
      ShowColumnHeaders = False
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = lstServicesSelectItem
    end
    object lstURLs: TListView
      Left = 211
      Top = 0
      Width = 239
      Height = 121
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alLeft
      BevelOuter = bvNone
      BevelKind = bkTile
      BorderStyle = bsNone
      Columns = <
        item
          Caption = 'Purpose'
          Width = 70
        end
        item
          AutoSize = True
          Caption = 'URL'
        end>
      HotTrackStyles = [htHandPoint, htUnderlineHot]
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      TabOrder = 1
      ViewStyle = vsReport
      OnClick = lstURLsClick
      ExplicitLeft = 174
    end
    object Panel10: TPanel
      Left = 652
      Top = 0
      Width = 193
      Height = 121
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitLeft = 624
      object imgLogo: TImage
        Left = 113
        Top = 0
        Width = 80
        Height = 121
        Align = alRight
        Center = True
        Proportional = True
        Stretch = True
        Transparent = True
        ExplicitLeft = 128
        ExplicitTop = 1
        ExplicitHeight = 119
      end
    end
  end
  object GP: TGridPanel
    Left = 0
    Top = 263
    Width = 845
    Height = 268
    Align = alBottom
    ColumnCollection = <
      item
        Value = 20.000000067792550000
      end
      item
        Value = 19.999999599008060000
      end
      item
        Value = 19.999999982899630000
      end
      item
        Value = 20.000000210848560000
      end
      item
        Value = 20.000000139451190000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = Panel1
        Row = 0
      end
      item
        Column = 1
        Control = Panel2
        Row = 0
      end
      item
        Column = 2
        Control = Panel5
        Row = 0
      end
      item
        Column = 3
        Control = Panel3
        Row = 0
      end
      item
        Column = 0
        Control = Panel6
        Row = 1
      end
      item
        Column = 1
        Control = Panel7
        Row = 1
      end
      item
        Column = 2
        Control = Panel8
        Row = 1
      end
      item
        Column = 3
        Control = Panel9
        Row = 1
      end
      item
        Column = 4
        Control = Panel11
        Row = 1
      end
      item
        Column = 4
        Control = Panel12
        Row = 0
      end>
    RowCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    TabOrder = 1
    ExplicitTop = 288
    ExplicitWidth = 686
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 168
      Height = 133
      Align = alClient
      TabOrder = 0
      ExplicitWidth = 224
      ExplicitHeight = 169
      object Label1: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 160
        Height = 13
        Align = alTop
        AutoSize = False
        Caption = 'Supported Information'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitLeft = 16
        ExplicitTop = 8
        ExplicitWidth = 130
      end
      object lstSupportedInfo: TListBox
        AlignWithMargins = True
        Left = 4
        Top = 23
        Width = 160
        Height = 35
        Align = alTop
        BorderStyle = bsNone
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 216
      end
    end
    object Panel2: TPanel
      Left = 169
      Top = 1
      Width = 168
      Height = 133
      Align = alClient
      TabOrder = 1
      ExplicitLeft = 225
      ExplicitWidth = 224
      ExplicitHeight = 169
      object Label2: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 160
        Height = 13
        Align = alTop
        AutoSize = False
        Caption = 'Locations'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitLeft = 16
        ExplicitTop = 8
        ExplicitWidth = 54
      end
      object lstSupportedLocationTypes: TListBox
        AlignWithMargins = True
        Left = 4
        Top = 23
        Width = 160
        Height = 39
        Align = alTop
        BorderStyle = bsNone
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 216
      end
    end
    object Panel5: TPanel
      Left = 337
      Top = 1
      Width = 168
      Height = 133
      Align = alClient
      TabOrder = 2
      ExplicitLeft = 449
      ExplicitWidth = 224
      ExplicitHeight = 169
      object Label3: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 160
        Height = 13
        Align = alTop
        AutoSize = False
        Caption = 'Condition Properties'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitLeft = 16
        ExplicitTop = 8
        ExplicitWidth = 59
      end
      object lstSupportedConditionProps: TListBox
        AlignWithMargins = True
        Left = 4
        Top = 23
        Width = 160
        Height = 39
        Align = alTop
        BorderStyle = bsNone
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 216
      end
    end
    object Panel3: TPanel
      Left = 505
      Top = 1
      Width = 168
      Height = 133
      Align = alClient
      TabOrder = 3
      ExplicitLeft = 673
      ExplicitWidth = 225
      ExplicitHeight = 169
      object Label7: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 160
        Height = 13
        Align = alTop
        AutoSize = False
        Caption = 'Alert Types'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitLeft = 16
        ExplicitTop = 8
        ExplicitWidth = 34
      end
      object lstSupportedAlertTypes: TListBox
        AlignWithMargins = True
        Left = 4
        Top = 23
        Width = 160
        Height = 39
        Align = alTop
        BorderStyle = bsNone
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 217
      end
    end
    object Panel6: TPanel
      Left = 1
      Top = 134
      Width = 168
      Height = 133
      Align = alClient
      TabOrder = 4
      ExplicitTop = 170
      ExplicitWidth = 224
      ExplicitHeight = 169
      object Label4: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 160
        Height = 13
        Align = alTop
        AutoSize = False
        Caption = 'Forecast Summary Props'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitLeft = 16
        ExplicitTop = 8
        ExplicitWidth = 107
      end
      object lstSupportedForecastSummaryProps: TListBox
        AlignWithMargins = True
        Left = 4
        Top = 23
        Width = 160
        Height = 39
        Align = alTop
        BorderStyle = bsNone
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 216
      end
    end
    object Panel7: TPanel
      Left = 169
      Top = 134
      Width = 168
      Height = 133
      Align = alClient
      TabOrder = 5
      ExplicitLeft = 225
      ExplicitTop = 170
      ExplicitWidth = 224
      ExplicitHeight = 169
      object Label5: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 160
        Height = 13
        Align = alTop
        AutoSize = False
        Caption = 'Forecast Hourly Props'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitLeft = 16
        ExplicitTop = 8
        ExplicitWidth = 89
      end
      object lstSupportedForecastHourlyProps: TListBox
        AlignWithMargins = True
        Left = 4
        Top = 23
        Width = 160
        Height = 39
        Align = alTop
        BorderStyle = bsNone
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 216
      end
    end
    object Panel8: TPanel
      Left = 337
      Top = 134
      Width = 168
      Height = 133
      Align = alClient
      TabOrder = 6
      ExplicitLeft = 449
      ExplicitTop = 170
      ExplicitWidth = 224
      ExplicitHeight = 169
      object Label6: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 160
        Height = 13
        Align = alTop
        AutoSize = False
        Caption = 'Forecast Daily Props'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitLeft = 16
        ExplicitTop = 8
        ExplicitWidth = 80
      end
      object lstSupportedForecastDailyProps: TListBox
        AlignWithMargins = True
        Left = 4
        Top = 23
        Width = 160
        Height = 39
        Align = alTop
        BorderStyle = bsNone
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 216
      end
    end
    object Panel9: TPanel
      Left = 505
      Top = 134
      Width = 168
      Height = 133
      Align = alClient
      TabOrder = 7
      ExplicitLeft = 507
      ExplicitTop = 140
      object Label8: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 160
        Height = 13
        Align = alTop
        AutoSize = False
        Caption = 'Map Types'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitLeft = 16
        ExplicitTop = 8
        ExplicitWidth = 30
      end
      object lstSupportedMaps: TListBox
        AlignWithMargins = True
        Left = 4
        Top = 23
        Width = 160
        Height = 39
        Align = alTop
        BorderStyle = bsNone
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 217
      end
    end
    object Panel11: TPanel
      Left = 673
      Top = 134
      Width = 171
      Height = 133
      Align = alClient
      TabOrder = 8
      ExplicitLeft = 449
      ExplicitTop = 170
      ExplicitWidth = 224
      ExplicitHeight = 169
      object Label9: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 163
        Height = 13
        Align = alTop
        AutoSize = False
        Caption = 'Supported Units'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitLeft = 16
        ExplicitTop = 8
        ExplicitWidth = 80
      end
      object lstSupportedUnits: TListBox
        AlignWithMargins = True
        Left = 4
        Top = 23
        Width = 163
        Height = 39
        Align = alTop
        BorderStyle = bsNone
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 160
      end
    end
    object Panel12: TPanel
      Left = 673
      Top = 1
      Width = 171
      Height = 133
      Align = alClient
      TabOrder = 9
      ExplicitTop = 47
      ExplicitWidth = 225
      ExplicitHeight = 169
      object Label10: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 163
        Height = 13
        Align = alTop
        AutoSize = False
        Caption = 'Alert Properties'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitLeft = 16
        ExplicitTop = 8
        ExplicitWidth = 34
      end
      object lstSupportedAlertProps: TListBox
        AlignWithMargins = True
        Left = 4
        Top = 23
        Width = 163
        Height = 39
        Align = alTop
        BorderStyle = bsNone
        ItemHeight = 13
        TabOrder = 0
      end
    end
  end
end
