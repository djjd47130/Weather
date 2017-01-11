unit uWeatherDLLTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.ShellAPI,
  System.SysUtils, System.Variants, System.Classes, System.TypInfo,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls,
  JD.Weather.Intf, JD.Weather.SuperObject;

type
  TfrmMain = class(TForm)
    Panel4: TPanel;
    lstServices: TListView;
    lstURLs: TListView;
    GP: TGridPanel;
    Panel1: TPanel;
    Label1: TLabel;
    lstSupportedInfo: TListBox;
    Panel2: TPanel;
    Label2: TLabel;
    lstSupportedLocationTypes: TListBox;
    Panel5: TPanel;
    Label3: TLabel;
    lstSupportedConditionProps: TListBox;
    Panel3: TPanel;
    Label7: TLabel;
    lstSupportedAlertTypes: TListBox;
    Panel6: TPanel;
    Label4: TLabel;
    lstSupportedForecastSummaryProps: TListBox;
    Panel7: TPanel;
    Label5: TLabel;
    lstSupportedForecastHourlyProps: TListBox;
    Panel8: TPanel;
    Label6: TLabel;
    lstSupportedForecastDailyProps: TListBox;
    Panel9: TPanel;
    Label8: TLabel;
    lstSupportedMaps: TListBox;
    Panel10: TPanel;
    imgLogo: TImage;
    Panel11: TPanel;
    Label9: TLabel;
    lstSupportedUnits: TListBox;
    Panel12: TPanel;
    Label10: TLabel;
    lstSupportedAlertProps: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure lstServicesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lstURLsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCreateLib: TCreateJDWeather;
    FLib: HMODULE;
    FWeather: IJDWeather;
    procedure ClearInfo;
    procedure WeatherImageToPicture(const G: IWeatherGraphic;
      const P: TPicture);
  public
    procedure LoadServices;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  E: Integer;
begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown:= True;
  {$ENDIF}
  GP.Align:= alClient;
  imgLogo.Align:= alClient;
  lstURLs.Align:= alClient;
  lstServices.Width:= lstServices.Width + 1;

  lstSupportedInfo.Align:= alClient;
  lstSupportedLocationTypes.Align:= alClient;
  lstSupportedConditionProps.Align:= alClient;
  lstSupportedAlertTypes.Align:= alClient;
  lstSupportedAlertProps.Align:= alClient;
  lstSupportedForecastSummaryProps.Align:= alClient;
  lstSupportedForecastHourlyProps.Align:= alClient;
  lstSupportedForecastDailyProps.Align:= alClient;
  lstSupportedMaps.Align:= alClient;
  lstSupportedUnits.Align:= alClient;

  //Load weather library
  FLib:= LoadLibrary('JDWeather.dll');
  if FLib <> 0 then begin
    FCreateLib:= GetProcAddress(FLib, 'CreateJDWeather');
    if Assigned(FCreateLib) then begin
      try
        FWeather:= FCreateLib(ExtractFilePath(ParamStr(0)));
        LoadServices;
      except
        on E: Exception do begin
          raise Exception.Create('Failed to create new instance of "IJDWeather": '+E.Message);
        end;
      end;
    end else begin
      raise Exception.Create('Function "CreateJDWeather" not found!');
    end;
  end else begin
    E:= GetLastError;
    raise Exception.Create('Failed to load library "JDWeather.dll" with error code '+IntToStr(E));
  end;

end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TfrmMain.LoadServices;
var
  X: Integer;
  S: IWeatherService;
  I: TListItem;
begin
  //Displays a list of supported services
  lstServices.Items.Clear;
  for X := 0 to FWeather.Services.Count-1 do begin
    S:= FWeather.Services.Items[X];
    I:= lstServices.Items.Add;
    I.Caption:= S.Caption;
    I.Data:= Pointer(S);
  end;
end;

procedure TfrmMain.WeatherImageToPicture(const G: IWeatherGraphic; const P: TPicture);
var
  S: TStringStream;
  I: TWicImage;
begin
  //Converts an `IWeatherGraphic` interface to a `TPicture`
  S:= TStringStream.Create;
  try
    S.WriteString(G.Base64);
    S.Position:= 0;
    I:= TWicImage.Create;
    try
      I.LoadFromStream(S);
      P.Assign(I);
    finally
      FreeAndNil(I);
    end;
  finally
    FreeAndNil(S);
  end;
end;

procedure TfrmMain.ClearInfo;
begin
  //Clear all service information
  lstURLs.Items.Clear;
  lstSupportedInfo.Items.Clear;
  lstSupportedLocationTypes.Items.Clear;
  lstSupportedConditionProps.Items.Clear;
  lstSupportedAlertTypes.Items.Clear;
  lstSupportedAlertProps.Items.Clear;
  lstSupportedForecastSummaryProps.Items.Clear;
  lstSupportedForecastHourlyProps.Items.Clear;
  lstSupportedForecastDailyProps.Items.Clear;
  lstSupportedMaps.Items.Clear;
  lstSupportedUnits.Items.Clear;
  imgLogo.Picture.Assign(nil);
end;

procedure TfrmMain.lstURLsClick(Sender: TObject);
var
  U: String;
begin
  //Launch url in browser
  if lstURLs.ItemIndex >= 0 then begin
    U:= lstURLs.Selected.SubItems[0];
    ShellExecute(0, 'open', PChar(U), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TfrmMain.lstServicesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
//Display all supported pieces of selected service
var
  S: IWeatherService;
  TInfo, TCond, TLoc, TAlert, TAlertProp, TForSum, TForHour, TForDay, TMaps, TUnits: Integer;
  PInfo, PCond, PLoc, PAlert, PAlertProp, PForSum, PForHour, PForDay, PMaps, PUnits: Single;
  TP: Single;
  WInfo: TWeatherInfoType;
  WLoc: TJDWeatherLocationType;
  WCond: TWeatherConditionsProp;
  WFor: TWeatherForecastProp;
  WAlt: TWeatherAlertType;
  WAlp: TWeatherAlertProp;
  WMap: TWeatherMapType;
  WMaf: TWeatherMapFormat;
  WUni: TWeatherUnits;
  procedure ChkUrl(const N, V: String);
  var
    I: TListItem;
  begin
    if V <> '' then begin
      I:= lstURLs.Items.Add;
      I.Caption:= N;
      I.SubItems.Add(V);
    end;
  end;
begin
  ClearInfo;
  if Selected then begin
    S:= IWeatherService(Item.Data);

    //Display URLs for Service
    ChkUrl('Website', S.URLs.MainURL);
    ChkUrl('API Docs', S.URLs.ApiURL);
    ChkUrl('Register', S.URLs.RegisterURL);
    ChkUrl('Login', S.URLs.LoginURL);
    ChkUrl('Legal', S.URLs.LegalURL);

    //Show supported information types
    for WInfo := Low(TWeatherInfoType) to High(TWeatherInfoType) do begin
      if WInfo in S.Support.SupportedInfo then begin
        lstSupportedInfo.Items.Add(WeatherInfoTypeToStr(WInfo));
      end;
    end;
    TInfo:= lstSupportedInfo.Items.Count;

    //Show supported location lookup types
    for WLoc := Low(TJDWeatherLocationType) to High(TJDWeatherLocationType) do begin
      if WLoc in S.Support.SupportedLocations then begin
        lstSupportedLocationTypes.Items.Add(WeatherLocationTypeToStr(WLoc));
      end;
    end;
    TLoc:= lstSupportedLocationTypes.Items.Count;

    //Show supported condition properties
    for WCond := Low(TWeatherConditionsProp) to High(TWeatherConditionsProp) do begin
      if WCond in S.Support.SupportedConditionProps then begin
        lstSupportedConditionProps.Items.Add(WeatherConditionPropToStr(WCond));
      end;
    end;
    TCond:= lstSupportedConditionProps.Items.Count;

    //Display supported forecast summary properties
    for WFor := Low(TWeatherForecastProp) to High(TWeatherForecastProp) do begin
      if WFor in S.Support.SupportedForecastSummaryProps then begin
        lstSupportedForecastSummaryProps.Items.Add(WeatherForecastPropToStr(WFor));
      end;
    end;
    TForSum:= lstSupportedForecastSummaryProps.Items.Count;

    //Display supported forecast hourly properties
    for WFor := Low(TWeatherForecastProp) to High(TWeatherForecastProp) do begin
      if WFor in S.Support.SupportedForecastHourlyProps then begin
        lstSupportedForecastHourlyProps.Items.Add(WeatherForecastPropToStr(WFor));
      end;
    end;
    TForHour:= lstSupportedForecastHourlyProps.Items.Count;

    //Display supported forecast daily properties
    for WFor := Low(TWeatherForecastProp) to High(TWeatherForecastProp) do begin
      if WFor in S.Support.SupportedForecastDailyProps then begin
        lstSupportedForecastDailyProps.Items.Add(WeatherForecastPropToStr(WFor));
      end;
    end;
    TForDay:= lstSupportedForecastDailyProps.Items.Count;

    //Display supported alert types
    for WAlt := Low(TWeatherAlertType) to High(TWeatherAlertType) do begin
      if WAlt in S.Support.SupportedAlerts then begin
        lstSupportedAlertTypes.Items.Add(WeatherAlertTypeToStr(WAlt));
      end;
    end;
    TAlert:= lstSupportedAlertTypes.Items.Count;

    //Display supported alert properties
    for WAlp := Low(TWeatherAlertProp) to High(TWeatherAlertProp) do begin
      if WAlp in S.Support.SupportedAlertProps then begin
        lstSupportedAlertProps.Items.Add(WeatherAlertPropToStr(WAlp));
      end;
    end;
    TAlertProp:= lstSupportedAlertProps.Items.Count;

    //Display supported map types
    for WMap := Low(TWeatherMapType) to High(TWeatherMapType) do begin
      if WMap in S.Support.SupportedMaps then begin
        lstSupportedMaps.Items.Add(WeatherMapTypeToStr(WMap));
      end;
    end;

    //Display supported map formats
    for WMaf := Low(TWeatherMapFormat) to High(TWeatherMapFormat) do begin
      if WMaf in S.Support.SupportedMapFormats then begin
        lstSupportedMaps.Items.Add(WeatherMapFormatToStr(WMaf));
      end;
    end;
    TMaps:= lstSupportedMaps.Items.Count;

    //Display supported units of measurement
    for WUni := Low(TWeatherUnits) to High(TWeatherUnits) do begin
      if WUni in S.Support.SupportedUnits then begin
        lstSupportedUnits.Items.Add(WeatherUnitsToStr(WUni));
      end;
    end;
    TUnits:= lstSupportedUnits.Items.Count;

    //Calculate percentage of support for each type of info
    PInfo:= TInfo / (Integer(High(TWeatherInfoType))+1);
    PLoc:= TLoc / (Integer(High(TJDWeatherLocationType))+1);
    PCond:= TCond / (Integer(High(TWeatherConditionsProp))+1);
    PAlert:= TAlert / (Integer(High(TWeatherAlertType))+1);
    PAlertProp:= TAlertProp / (Integer(High(TWeatherAlertProp))+1);
    PForSum:= TForSum / (Integer(High(TWeatherForecastProp))+1);
    PForHour:= TForHour / (Integer(High(TWeatherForecastProp))+1);
    PForDay:= TForDay / (Integer(High(TWeatherForecastProp))+1);
    PMaps:= TMaps / ((Integer(High(TWeatherMapType))+1) + (Integer(High(TWeatherMapFormat))+1));
    PUnits:= TUnits / (Integer(High(TWeatherUnits))+1);

    //Display percentages of support for each type of info
    lstSupportedInfo.Items.Add('-- Percent: '+FormatFloat('0.00%', PInfo*100));
    lstSupportedLocationTypes.Items.Add('-- Percent: '+FormatFloat('0.00%', PLoc*100));
    lstSupportedConditionProps.Items.Add('-- Percent: '+FormatFloat('0.00%', PCond*100));
    lstSupportedAlertTypes.Items.Add('-- Percent: '+FormatFloat('0.00%', PAlert*100));
    lstSupportedAlertProps.Items.Add('-- Percent: '+FormatFloat('0.00%', PAlertProp*100));
    lstSupportedForecastSummaryProps.Items.Add('-- Percent: '+FormatFloat('0.00%', PForSum*100));
    lstSupportedForecastHourlyProps.Items.Add('-- Percent: '+FormatFloat('0.00%', PForHour*100));
    lstSupportedForecastDailyProps.Items.Add('-- Percent: '+FormatFloat('0.00%', PForDay*100));
    lstSupportedMaps.Items.Add('-- Percent: '+FormatFloat('0.00%', PMaps*100));
    lstSupportedUnits.Items.Add('-- Percent: '+FormatFloat('0.00%', PUnits*100));

    //Calculate overall average of support percentage for selected service
    TP:=(PInfo + PLoc + PCond + PAlert + PAlertProp + PForSum +
      PForHour + PForDay + PMaps + PUnits) / 10;
    Caption:= 'JD Weather DLL Test - '+S.Caption+' - '+FormatFloat('0.00%', TP*100);

    //Display service company logo
    WeatherImageToPicture(S.GetLogo(ltColor), imgLogo.Picture);

  end else begin
    Caption:= 'JD Weather DLL Test';
  end;
end;

end.
