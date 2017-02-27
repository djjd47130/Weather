object frmWebTest: TfrmWebTest
  Left = 0
  Top = 0
  Caption = 'JD Weather Web REST API Test'
  ClientHeight = 482
  ClientWidth = 934
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MM
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Pages: TPageControl
    Left = 0
    Top = 0
    Width = 934
    Height = 409
    ActivePage = tabConditions
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabHeight = 32
    TabOrder = 0
    TabWidth = 120
    ExplicitWidth = 962
    object tabConditions: TTabSheet
      Caption = 'Conditions'
      ImageIndex = 3
      ExplicitLeft = 8
      ExplicitTop = 42
      object pTop: TPanel
        Left = 0
        Top = 0
        Width = 209
        Height = 367
        Align = alLeft
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Font.Quality = fqAntialiased
        ParentFont = False
        TabOrder = 0
        object lblLocation: TLabel
          Left = 16
          Top = 8
          Width = 50
          Height = 13
          Caption = 'lblLocation'
        end
        object lblConditions: TLabel
          Left = 16
          Top = 88
          Width = 60
          Height = 13
          Caption = 'lblConditions'
        end
        object lblTemp: TLabel
          Left = 16
          Top = 27
          Width = 105
          Height = 58
          Caption = 'Tmp'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -48
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          Font.Quality = fqAntialiased
          ParentFont = False
        end
      end
      object Panel1: TPanel
        Left = 209
        Top = 0
        Width = 345
        Height = 367
        Align = alLeft
        TabOrder = 1
        ExplicitLeft = 384
        ExplicitTop = 32
        ExplicitHeight = 233
        object lstConditions: TListView
          Left = 1
          Top = 1
          Width = 343
          Height = 280
          Align = alTop
          Columns = <>
          TabOrder = 0
        end
      end
    end
    object tabAlerts: TTabSheet
      Caption = 'Severe Alerts'
      ImageIndex = 2
      ExplicitWidth = 954
    end
    object tabForecast: TTabSheet
      Caption = 'Forecast'
      ImageIndex = 2
      ExplicitWidth = 954
    end
    object tabSetup: TTabSheet
      Caption = 'Setup'
      ExplicitWidth = 954
      ExplicitHeight = 399
      object CheckListBox1: TCheckListBox
        Left = 0
        Top = 0
        Width = 161
        Height = 367
        Align = alLeft
        Items.Strings = (
          'Weather Underground'
          'Open Weather Map'
          'AccuWeather'
          'Foreca'
          'OnPOINT'
          'National Weather Service'
          'DarkSKy')
        TabOrder = 0
        ExplicitHeight = 399
      end
      object pSetup: TPanel
        Left = 200
        Top = 0
        Width = 726
        Height = 367
        Align = alRight
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 1
        ExplicitLeft = 216
        DesignSize = (
          726
          367)
        object Label1: TLabel
          Left = 8
          Top = 13
          Width = 131
          Height = 16
          Caption = 'JD Weather API Key'
        end
        object Label2: TLabel
          Left = 8
          Top = 69
          Width = 90
          Height = 16
          Caption = 'Location Type'
        end
        object Label3: TLabel
          Left = 8
          Top = 117
          Width = 108
          Height = 16
          Caption = 'Location Detail 1'
        end
        object Label4: TLabel
          Left = 8
          Top = 165
          Width = 108
          Height = 16
          Caption = 'Location Detail 2'
        end
        object Label5: TLabel
          Left = 8
          Top = 213
          Width = 135
          Height = 16
          Caption = 'Unit of Measurement'
        end
        object Label6: TLabel
          Left = 200
          Top = 13
          Width = 170
          Height = 16
          Caption = 'Location Free-Text Search'
        end
        object Label7: TLabel
          Left = 8
          Top = 262
          Width = 131
          Height = 16
          Caption = 'Refresh Rate (Mins)'
        end
        object txtApiKey: TEdit
          Left = 8
          Top = 32
          Width = 161
          Height = 24
          TabOrder = 0
        end
        object ComboBox1: TComboBox
          Left = 8
          Top = 88
          Width = 161
          Height = 24
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 1
          Text = 'Auto IP'
          Items.Strings = (
            'Auto IP'
            'Zip Code'
            'Coordinates'
            'City / State'
            'City / Country'
            'City Code'
            'Airport Code'
            'Weather Station')
        end
        object Edit1: TEdit
          Left = 8
          Top = 136
          Width = 161
          Height = 24
          TabOrder = 2
        end
        object Edit2: TEdit
          Left = 8
          Top = 184
          Width = 161
          Height = 24
          TabOrder = 3
        end
        object ComboBox2: TComboBox
          Left = 8
          Top = 232
          Width = 161
          Height = 24
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 4
          Text = 'Imperial'
          Items.Strings = (
            'Imperial'
            'Metric'
            'Kelvin')
        end
        object txtLocationSearch: TEdit
          Left = 200
          Top = 32
          Width = 417
          Height = 24
          TabOrder = 5
        end
        object BitBtn1: TBitBtn
          Left = 8
          Top = 331
          Width = 108
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Save'
          TabOrder = 6
          OnClick = BitBtn1Click
        end
        object lstLocations: TListView
          Left = 200
          Top = 59
          Width = 417
          Height = 166
          Columns = <
            item
              Caption = 'City'
              Width = 150
            end
            item
              Caption = 'Region'
              Width = 150
            end
            item
              Caption = 'Country'
              Width = 80
            end>
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          TabOrder = 7
          ViewStyle = vsReport
        end
        object cmdUseLocation: TBitBtn
          Left = 200
          Top = 231
          Width = 105
          Height = 25
          Caption = 'Use Location'
          TabOrder = 8
        end
        object SpinEdit1: TSpinEdit
          Left = 8
          Top = 284
          Width = 65
          Height = 26
          MaxValue = 300
          MinValue = 1
          TabOrder = 9
          Value = 5
        end
        object ComboBox3: TComboBox
          Left = 79
          Top = 284
          Width = 90
          Height = 24
          Style = csDropDownList
          ItemIndex = 1
          TabOrder = 10
          Text = 'Minutes'
          Items.Strings = (
            'Seconds'
            'Minutes'
            'Hours')
        end
      end
    end
  end
  object tmrRefresh: TTimer
    Interval = 500000
    OnTimer = tmrRefreshTimer
    Left = 124
    Top = 417
  end
  object MM: TMainMenu
    Left = 48
    Top = 416
    object File1: TMenuItem
      Caption = 'File'
      object Refresh1: TMenuItem
        Caption = 'Refresh'
        OnClick = Refresh1Click
      end
      object ServiceInfo1: TMenuItem
        Caption = 'Service Info'
        OnClick = ServiceInfo1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object View1: TMenuItem
      Caption = 'View'
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object ShowHelp1: TMenuItem
        Caption = 'Show Help'
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = 'About'
      end
    end
  end
end
