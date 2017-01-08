object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'JD Weather DLL Test'
  ClientHeight = 556
  ClientWidth = 897
  Color = clBtnFace
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
  object Splitter1: TSplitter
    Left = 0
    Top = 145
    Width = 897
    Height = 5
    Cursor = crVSplit
    Align = alTop
    Beveled = True
    ExplicitTop = 137
    ExplicitWidth = 941
  end
  object SB: TScrollBox
    Left = 0
    Top = 215
    Width = 897
    Height = 341
    Align = alBottom
    BorderStyle = bsNone
    TabOrder = 0
    ExplicitWidth = 825
    object pOne: TPanel
      Left = 0
      Top = 0
      Width = 897
      Height = 113
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      OnResize = pOneResize
      ExplicitWidth = 825
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 209
        Height = 113
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitHeight = 241
        DesignSize = (
          209
          113)
        object Label1: TLabel
          Left = 16
          Top = 8
          Width = 109
          Height = 13
          Caption = 'Supported Information'
        end
        object lstSupportedInfo: TListBox
          Left = 16
          Top = 27
          Width = 174
          Height = 72
          Anchors = [akLeft, akTop, akRight, akBottom]
          BorderStyle = bsNone
          ItemHeight = 13
          TabOrder = 0
          ExplicitWidth = 253
          ExplicitHeight = 200
        end
      end
      object Panel2: TPanel
        Left = 209
        Top = 0
        Width = 224
        Height = 113
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitHeight = 241
        DesignSize = (
          224
          113)
        object Label2: TLabel
          Left = 16
          Top = 8
          Width = 45
          Height = 13
          Caption = 'Locations'
        end
        object lstSupportedLocationTypes: TListBox
          Left = 16
          Top = 27
          Width = 189
          Height = 72
          Anchors = [akLeft, akTop, akRight, akBottom]
          BorderStyle = bsNone
          ItemHeight = 13
          TabOrder = 0
          ExplicitWidth = 245
          ExplicitHeight = 200
        end
      end
      object Panel5: TPanel
        Left = 433
        Top = 0
        Width = 224
        Height = 113
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 2
        ExplicitLeft = 545
        ExplicitHeight = 241
        DesignSize = (
          224
          113)
        object Label3: TLabel
          Left = 16
          Top = 8
          Width = 50
          Height = 13
          Caption = 'Conditions'
        end
        object lstSupportedConditionProps: TListBox
          Left = 16
          Top = 27
          Width = 189
          Height = 72
          Anchors = [akLeft, akTop, akRight, akBottom]
          BorderStyle = bsNone
          ItemHeight = 13
          TabOrder = 0
          ExplicitHeight = 200
        end
      end
      object Panel3: TPanel
        Left = 657
        Top = 0
        Width = 224
        Height = 113
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 3
        ExplicitLeft = 673
        ExplicitTop = -6
        DesignSize = (
          224
          113)
        object Label7: TLabel
          Left = 16
          Top = 8
          Width = 28
          Height = 13
          Caption = 'Alerts'
        end
        object lstSupportedAlertProps: TListBox
          Left = 16
          Top = 27
          Width = 189
          Height = 72
          Anchors = [akLeft, akTop, akRight, akBottom]
          BorderStyle = bsNone
          ItemHeight = 13
          TabOrder = 0
        end
      end
    end
    object pTwo: TPanel
      Left = 0
      Top = 113
      Width = 897
      Height = 113
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      OnResize = pTwoResize
      ExplicitLeft = -24
      ExplicitTop = 192
      ExplicitWidth = 825
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 209
        Height = 113
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          209
          113)
        object Label4: TLabel
          Left = 16
          Top = 8
          Width = 89
          Height = 13
          Caption = 'Forecast Summary'
        end
        object lstSupportedForecastSummaryProps: TListBox
          Left = 16
          Top = 27
          Width = 174
          Height = 72
          Anchors = [akLeft, akTop, akRight, akBottom]
          BorderStyle = bsNone
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object Panel7: TPanel
        Left = 209
        Top = 0
        Width = 224
        Height = 113
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          224
          113)
        object Label5: TLabel
          Left = 16
          Top = 8
          Width = 76
          Height = 13
          Caption = 'Forecast Hourly'
        end
        object lstSupportedForecastHourlyProps: TListBox
          Left = 16
          Top = 27
          Width = 189
          Height = 72
          Anchors = [akLeft, akTop, akRight, akBottom]
          BorderStyle = bsNone
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object Panel8: TPanel
        Left = 433
        Top = 0
        Width = 224
        Height = 113
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 2
        DesignSize = (
          224
          113)
        object Label6: TLabel
          Left = 16
          Top = 8
          Width = 68
          Height = 13
          Caption = 'Forecast Daily'
        end
        object lstSupportedForecastDailyProps: TListBox
          Left = 16
          Top = 27
          Width = 189
          Height = 72
          Anchors = [akLeft, akTop, akRight, akBottom]
          BorderStyle = bsNone
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object Panel9: TPanel
        Left = 657
        Top = 0
        Width = 224
        Height = 113
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 3
        ExplicitLeft = 673
        ExplicitTop = -16
        DesignSize = (
          224
          113)
        object Label8: TLabel
          Left = 16
          Top = 8
          Width = 25
          Height = 13
          Caption = 'Maps'
        end
        object lstSupportedMaps: TListBox
          Left = 16
          Top = 27
          Width = 189
          Height = 72
          Anchors = [akLeft, akTop, akRight, akBottom]
          BorderStyle = bsNone
          ItemHeight = 13
          TabOrder = 0
        end
      end
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 0
    Width = 897
    Height = 145
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object imgLogo: TImage
      Left = 688
      Top = 0
      Width = 209
      Height = 145
      Align = alRight
      Center = True
      Proportional = True
      Stretch = True
      Transparent = True
      ExplicitHeight = 161
    end
    object Splitter2: TSplitter
      Left = 683
      Top = 0
      Width = 5
      Height = 145
      Align = alRight
      Beveled = True
      ExplicitLeft = 729
      ExplicitTop = 1
      ExplicitHeight = 159
    end
    object Splitter3: TSplitter
      Left = 184
      Top = 0
      Width = 5
      Height = 145
      Beveled = True
      ExplicitLeft = 160
      ExplicitTop = -16
      ExplicitHeight = 161
    end
    object lstServices: TListView
      Left = 0
      Top = 0
      Width = 184
      Height = 145
      Align = alLeft
      BorderStyle = bsNone
      Columns = <
        item
          AutoSize = True
          Caption = 'Service Name'
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = lstServicesSelectItem
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitHeight = 159
    end
    object lstURLs: TListView
      Left = 189
      Top = 0
      Width = 396
      Height = 145
      Align = alLeft
      BorderStyle = bsNone
      Columns = <
        item
          Caption = 'Purpose'
          Width = 120
        end
        item
          AutoSize = True
          Caption = 'URL'
        end>
      HotTrackStyles = [htHandPoint, htUnderlineHot]
      ReadOnly = True
      RowSelect = True
      TabOrder = 1
      ViewStyle = vsReport
      OnClick = lstURLsClick
      ExplicitLeft = 190
      ExplicitHeight = 166
    end
  end
end
