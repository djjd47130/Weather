object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'JD Weather DLL Test'
  ClientHeight = 567
  ClientWidth = 895
  Color = clWhite
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MM
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel4: TPanel
    Left = 0
    Top = 0
    Width = 895
    Height = 145
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 845
    object lstServices: TListView
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 211
      Height = 139
      Align = alLeft
      BevelOuter = bvNone
      BorderStyle = bsNone
      Color = clWhite
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
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitHeight = 145
    end
    object lstURLs: TListView
      AlignWithMargins = True
      Left = 220
      Top = 3
      Width = 239
      Height = 139
      Align = alLeft
      BevelOuter = bvNone
      BorderStyle = bsNone
      Color = clWhite
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
      ExplicitLeft = 211
      ExplicitTop = 0
      ExplicitHeight = 145
    end
    object Panel10: TPanel
      AlignWithMargins = True
      Left = 671
      Top = 3
      Width = 221
      Height = 139
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitLeft = 624
      ExplicitTop = 0
      ExplicitHeight = 145
      object imgLogo: TImage
        AlignWithMargins = True
        Left = 131
        Top = 10
        Width = 80
        Height = 119
        Margins.Left = 10
        Margins.Top = 10
        Margins.Right = 10
        Margins.Bottom = 10
        Align = alRight
        Center = True
        Proportional = True
        Stretch = True
        Transparent = True
        ExplicitLeft = 128
        ExplicitTop = 1
      end
    end
  end
  object GP: TGridPanel
    Left = 0
    Top = 232
    Width = 895
    Height = 335
    Align = alBottom
    ColumnCollection = <
      item
        Value = 20.000000067792550000
      end
      item
        Value = 19.999999599008060000
      end
      item
        Value = 19.999999982899640000
      end
      item
        Value = 20.000000210848570000
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
    ExplicitWidth = 845
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 178
      Height = 166
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 168
      ExplicitHeight = 133
      object lstSupportedInfo: TListView
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 172
        Height = 86
        Align = alTop
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
            Caption = 'Supported Info'
          end>
        ReadOnly = True
        RowSelect = True
        SmallImages = imgList
        TabOrder = 0
        ViewStyle = vsReport
        ExplicitWidth = 162
      end
    end
    object Panel2: TPanel
      Left = 179
      Top = 1
      Width = 178
      Height = 166
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 169
      ExplicitWidth = 168
      ExplicitHeight = 133
      object lstSupportedLocationTypes: TListView
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 172
        Height = 86
        Align = alTop
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
            Caption = 'Supported Location Types'
          end>
        ReadOnly = True
        RowSelect = True
        SmallImages = imgList
        TabOrder = 0
        ViewStyle = vsReport
        ExplicitLeft = 6
        ExplicitTop = 11
        ExplicitWidth = 162
      end
    end
    object Panel5: TPanel
      Left = 357
      Top = 1
      Width = 178
      Height = 166
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitLeft = 337
      ExplicitWidth = 168
      ExplicitHeight = 133
      object lstSupportedConditionProps: TListView
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 172
        Height = 86
        Align = alTop
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
            Caption = 'Supported Condition Props'
          end>
        ReadOnly = True
        RowSelect = True
        SmallImages = imgList
        TabOrder = 0
        ViewStyle = vsReport
        ExplicitLeft = 6
        ExplicitTop = 11
      end
    end
    object Panel3: TPanel
      Left = 535
      Top = 1
      Width = 178
      Height = 166
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 3
      ExplicitLeft = 505
      ExplicitWidth = 168
      ExplicitHeight = 133
      object lstSupportedAlertTypes: TListView
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 172
        Height = 86
        Align = alTop
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
            Caption = 'Supported Alert Types'
          end>
        ReadOnly = True
        RowSelect = True
        SmallImages = imgList
        TabOrder = 0
        ViewStyle = vsReport
        ExplicitLeft = 6
        ExplicitTop = 11
      end
    end
    object Panel6: TPanel
      Left = 1
      Top = 167
      Width = 178
      Height = 167
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 4
      ExplicitTop = 134
      ExplicitWidth = 168
      ExplicitHeight = 133
      object lstSupportedForecastSummaryProps: TListView
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 172
        Height = 86
        Align = alTop
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
            Caption = 'Supported Forecast Summary Props'
          end>
        ReadOnly = True
        RowSelect = True
        SmallImages = imgList
        TabOrder = 0
        ViewStyle = vsReport
        ExplicitLeft = 6
        ExplicitTop = 11
      end
    end
    object Panel7: TPanel
      Left = 179
      Top = 167
      Width = 178
      Height = 167
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 5
      ExplicitLeft = 169
      ExplicitTop = 134
      ExplicitWidth = 168
      ExplicitHeight = 133
      object lstSupportedForecastHourlyProps: TListView
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 172
        Height = 86
        Align = alTop
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
            Caption = 'Supported Forecast Hourly Props'
          end>
        ReadOnly = True
        RowSelect = True
        SmallImages = imgList
        TabOrder = 0
        ViewStyle = vsReport
        ExplicitLeft = 6
        ExplicitTop = 11
      end
    end
    object Panel8: TPanel
      Left = 357
      Top = 167
      Width = 178
      Height = 167
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 6
      ExplicitLeft = 337
      ExplicitTop = 134
      ExplicitWidth = 168
      ExplicitHeight = 133
      object lstSupportedForecastDailyProps: TListView
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 172
        Height = 86
        Align = alTop
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
            Caption = 'Supported Forecast Daily Props'
          end>
        ReadOnly = True
        RowSelect = True
        SmallImages = imgList
        TabOrder = 0
        ViewStyle = vsReport
        ExplicitLeft = 6
        ExplicitTop = 11
      end
    end
    object Panel9: TPanel
      Left = 535
      Top = 167
      Width = 178
      Height = 167
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 7
      ExplicitLeft = 505
      ExplicitTop = 134
      ExplicitWidth = 168
      ExplicitHeight = 133
      object lstSupportedMaps: TListView
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 172
        Height = 86
        Align = alTop
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
            Caption = 'Supported Map Types'
          end>
        ReadOnly = True
        RowSelect = True
        SmallImages = imgList
        TabOrder = 0
        ViewStyle = vsReport
        ExplicitLeft = 6
        ExplicitTop = 11
      end
    end
    object Panel11: TPanel
      Left = 713
      Top = 167
      Width = 181
      Height = 167
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 8
      ExplicitLeft = 673
      ExplicitTop = 134
      ExplicitWidth = 171
      ExplicitHeight = 133
      object lstSupportedUnits: TListView
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 175
        Height = 86
        Align = alTop
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
            Caption = 'Supported Units'
          end>
        ReadOnly = True
        RowSelect = True
        SmallImages = imgList
        TabOrder = 0
        ViewStyle = vsReport
        ExplicitTop = 6
      end
    end
    object Panel12: TPanel
      Left = 713
      Top = 1
      Width = 181
      Height = 166
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 9
      ExplicitLeft = 673
      ExplicitWidth = 171
      ExplicitHeight = 133
      object lstSupportedAlertProps: TListView
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 175
        Height = 86
        Align = alTop
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
            Caption = 'Supported Alert Types'
          end>
        ReadOnly = True
        RowSelect = True
        SmallImages = imgList
        TabOrder = 0
        ViewStyle = vsReport
        ExplicitLeft = 6
        ExplicitTop = 11
      end
    end
  end
  object imgList: TImageList
    Left = 240
    Top = 160
    Bitmap = {
      494C010102000800200010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      000000000000000000000000000000000000CCCCD6336266A3A0D1D1D72EFFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B4B5
      C24B6869A2986D6EA292676AA29BCECFD7310000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DADADD251925C3FD1B25AFF3F7F7
      F708FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00D1D2D72E1928
      C8FB1127F2FF000BE4FF1119B0F8DADADE250000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00A5A5B45A1522E1FF3539
      94CEFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF005972D4EA132E
      F9FF0010E3FF0208C5FEB7B7C048FFFFFF0000000000B8B5B5F0B4B1B1FFB4B1
      B1FFB4B1B1FFB4B1B1FFB4B1B1FFB4B1B1FFB4B1B1FFB4B1B1FFB4B1B1FFB4B1
      B1FFB8B5B5F00000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF005C5E93A4101F
      E8FF6C6D9793FFFFFF00FFFFFF00FFFFFF00FFFFFF009CAFD5C2344DF8FF000B
      DEFF0B18E3FF6D709A92FFFFFF00FFFFFF0000000000B0ADADFFFEFEFEFFFDFD
      FDFFF8FAF8FFF1F5F1FFEDF3EEFFF4F8F4FFFBFCFBFFFFFFFFFFFFFFFFFFFAFA
      FAFFB0ADADFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00252B
      99DE0B18D6FFB0B0BA4FFFFFFF00FFFFFF00ADBCD17F5E70F2FF0000D8FF141D
      E6FF4E60B8CFFFFFFF00FFFFFF00FFFFFF0000000000ADA9A9FFFDFDFDFFF6F9
      F6FFEAF1EAFF9CC89DFF339334FFC7DFC7FFF3F7F4FFFDFDFDFFFDFDFDFFF7F7
      F7FFADA9A9FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00EFEF
      EF100E16B6F80E16B6F8F9F9F906CBCCD3374456E6FD0101CFFF0709D3FF384F
      D2EFFBFBFB04FFFFFF00FFFFFF00FFFFFF0000000000A9A6A6FFF4F7F4FFE7EF
      E7FF78B678FF148816FF209D31FF369536FFE0EBE1FFF8FAF8FFF8F8F8FFF4F4
      F4FFA9A6A6FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00B0B0BB4F0C14D0FF2B2E94D63E4DADDC1B30F6FF0717E2FF2238DFFDC6C8
      D13CFFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000A6A1A1FFE1EBE2FF55A8
      55FF118C14FF18A42BFF22B03EFF189525FF5AA75BFFEFF1EFFFF2F2F2FFF0F0
      F0FFA6A1A1FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF007172978E2232DEFF232790E31D2FDBFE1A31EDFF8A8FAE77FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000919B8CFF42A542FF1A93
      1AFF139717FF179F22FF19A329FF13A124FF148D16FF85BC85FFEBEBEBFFEDED
      EDFFA29D9DFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00868BAA7A243BEFFF1B2CD2FD212592E52132E1FF82839F7DFFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000389938FF49A74AFF41A1
      41FF41A741FF49AC4AFF3AA53CFF44AA45FF32A232FF2D992DFFA9CBA9FFE6E6
      E6FFA5A0A0FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00C5C8D13C2E4BEEFD1934FCFF2844FFFF3F4DABD9282E9DDF1927D5FEC0C0
      C73FFFFFFF00FFFFFF00FFFFFF00FFFFFF00000000007FA37DFF50B153FF59AA
      59FF5BB25DFF4EAF4EFFBDDEBEFF58B158FF51AD51FF5CAF5CFF41A441FFC2D5
      C2FFADA8A8FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FBFB
      FB04415CD5EF314FFFFF2D4AFFFF5674F5FDCED0D733EDEDED121A27C5FB1E28
      B3F3F9F9F906FFFFFF00FFFFFF00FFFFFF0000000000B4B0B0FF8BCE8FFF61BB
      67FF66BD6BFFEFF8F0FFF9F9F9FFF3F4F3FF93C995FF4DAE4EFF74BE74FF4CAA
      4CFFABAEA7FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF005265
      B8CE4564FFFF3454FFFF859FFFFFACB2CE80FFFFFF00FFFFFF00A0A0B05F1F2F
      E3FF363B94D2FFFFFF00FFFFFF00FFFFFF0000000000BDB8B8FFFBFDFCFFAFDE
      B4FFFCFEFCFFFFFFFFFFF9F9F9FFF6F6F6FFF4F4F4FFD0E4D0FF59B45CFF73C1
      76FF53A453FFFEFFFE0100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF006C6D97933B57
      FAFF4968FFFF6F8BFFFF9DB2D5C2FFFFFF00FFFFFF00FFFFFF00FFFFFF005E60
      92A42535EAFF6A6B9596FFFFFF00FFFFFF0000000000C5C0C0FFFEFEFEFFFFFF
      FFFFFFFFFFFFFBFBFBFFF6F6F6FFF1F1F1FFF5F5F5FFF5F5F5FFF2F3F2FF87C5
      8BFF52B156FF8FCE927800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00B5B5BE4A2838D7FE5676
      FFFF5F7FFFFF647BD4EAFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00313798D92532DCFFB5B5BF4AFFFFFF0000000000CFCBCBF0CCC8C8FFCCC8
      C8FFCCC8C8FFCCC8C8FFCCC8C8FFCCC8C8FFCCC8C8FFCCC8C8FFCCC8C8FFCCC8
      C8FFABC2AAF347B24FC3B7E0BA4A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E4E4E51B1E2AB6F94D67FFFF617D
      FFFF3443C9FBD1D1D82EFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00EDEDED122834B9F71E28B8FBD7D7DA280000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F8FCF8078FD19577DBF0DD250000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CECED7315D61A9A76464A79C5F60
      A7A2B0B0C24FFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C5C6CF3A595BA9AAC5C5D13A0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000E6F6E81A0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF000000FFFF000000000000FFFF00000000
      0000800700000000000080070000000000008007000000000000800700000000
      0000800700000000000080070000000000008007000000000000800700000000
      0000800700000000000080030000000000008003000000000000800100000000
      0000FFF8000000000000FFFE0000000000000000000000000000000000000000
      000000000000}
  end
  object MM: TMainMenu
    Left = 296
    Top = 160
    object File1: TMenuItem
      Caption = 'File'
      object Refresh1: TMenuItem
        Caption = 'Refresh'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
      end
    end
    object Service1: TMenuItem
      Caption = 'Service'
      object EditSettings1: TMenuItem
        Caption = 'Configure...'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object estData1: TMenuItem
        Caption = 'Test Data'
      end
    end
  end
end
