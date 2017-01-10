unit uWeatherDLLTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.ShellAPI,
  System.SysUtils, System.Variants, System.Classes, System.TypInfo,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls,
  JD.Weather.Intf, JD.Weather.SuperObject,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdCustomHTTPServer,
  IdHTTPServer, IdContext, IdTCPConnection, IdYarn, IdIOHandler,
  IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL;

type

  TWeatherContext = class(TIdServerContext)
  private
    FCreateLib: TCreateJDWeather;
    FWeather: IJDWeather;
    FKey: WideString;
    FServiceUID: WideString;
    FLocType: WideString;
    FLoc1: WideString;
    FLoc2: WideString;
    FUnits: WideString;
    FDet: WideString;
    FDoc: TStringList;
  public
    constructor Create(AConnection: TIdTCPConnection; AYarn: TIdYarn;
      AList: TIdContextThreadList = nil); override;
    destructor Destroy; override;
  end;

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
    Svr: TIdHTTPServer;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    procedure FormCreate(Sender: TObject);
    procedure lstServicesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lstURLsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SvrCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure SvrConnect(AContext: TIdContext);
    procedure SvrDisconnect(AContext: TIdContext);
    procedure SvrContextCreated(AContext: TIdContext);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FCreateLib: TCreateJDWeather;
    FLib: HMODULE;
    FWeather: IJDWeather;
    procedure ClearInfo;
    procedure WeatherImageToPicture(const G: IWeatherGraphic;
      const P: TPicture);
    procedure HandleServiceGet(AContext: TWeatherContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
      const Svc: IWeatherService);
    procedure HandleServiceList(AContext: TWeatherContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleServiceSupport(AContext: TWeatherContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
      const Svc: IWeatherService);
  public
    procedure LoadServices;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TWeatherContext }

constructor TWeatherContext.Create(AConnection: TIdTCPConnection;
  AYarn: TIdYarn; AList: TIdContextThreadList);
begin
  inherited;
  FDoc:= TStringList.Create;
end;

destructor TWeatherContext.Destroy;
begin
  if Assigned(FWeather) then begin
    FWeather._Release;
    FWeather:= nil;
  end;
  FreeAndNil(FDoc);
  inherited;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  E: Integer;
begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown:= True;
  {$ENDIF}
  Svr.ContextClass:= TWeatherContext;
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

  Svr.Active:= True;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Svr.Active:= False;
end;

procedure TfrmMain.LoadServices;
var
  X: Integer;
  S: IWeatherService;
  I: TListItem;
begin
  lstServices.Items.Clear;
  for X := 0 to FWeather.Services.Count-1 do begin
    S:= FWeather.Services.Items[X];
    I:= lstServices.Items.Add;
    I.Caption:= S.Caption;
    I.Data:= Pointer(S);
  end;
end;

procedure TfrmMain.WeatherImageToPicture(const G: IWeatherGraphic;
  const P: TPicture);
var
  S: TStringStream;
  I: TWicImage;
begin
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

procedure TfrmMain.lstServicesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  S: IWeatherService;
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
  procedure ChkInfo(const I: TWeatherInfoType; const Str: String);
  begin
    if I in S.Support.SupportedInfo then begin
      lstSupportedInfo.Items.Add(Str);
    end;
  end;
  procedure ChkLoc(const L: TJDWeatherLocationType; const Str: String);
  begin
    if L in S.Support.SupportedLocations then begin
      lstSupportedLocationTypes.Items.Add(Str);
    end;
  end;
  procedure ChkCon(const P: TWeatherConditionsProp; const Str: String);
  begin
    if P in S.Support.SupportedConditionProps then begin
      lstSupportedConditionProps.Items.Add(Str);
    end;
  end;
  procedure ChkForSum(const P: TWeatherForecastProp; const Str: String);
  begin
    if P in S.Support.SupportedForecastSummaryProps then begin
      lstSupportedForecastSummaryProps.Items.Add(Str);
    end;
  end;
  procedure ChkForHou(const P: TWeatherForecastProp; const Str: String);
  begin
    if P in S.Support.SupportedForecastHourlyProps then begin
      lstSupportedForecastHourlyProps.Items.Add(Str);
    end;
  end;
  procedure ChkForDay(const P: TWeatherForecastProp; const Str: String);
  begin
    if P in S.Support.SupportedForecastDailyProps then begin
      lstSupportedForecastDailyProps.Items.Add(Str);
    end;
  end;
  procedure ChkAltTyp(const P: TWeatherAlertType; const Str: String);
  begin
    if P in S.Support.SupportedAlerts then begin
      lstSupportedAlertTypes.Items.Add(Str);
    end;
  end;
  procedure ChkAltPro(const P: TWeatherAlertProp; const Str: String);
  begin
    if P in S.Support.SupportedAlertProps then begin
      lstSupportedAlertProps.Items.Add(Str);
    end;
  end;
  procedure ChkMap(const M: TWeatherMapType; const Str: String);
  begin
    if M in S.Support.SupportedMaps then begin
      lstSupportedMaps.Items.Add(Str);
    end;
  end;
  procedure ChkMapFor(const F: TWeatherMapFormat; const Str: String);
  begin
    if F in S.Support.SupportedMapFormats then begin
      lstSupportedMaps.Items.Add(Str);
    end;
  end;
  procedure ChkUni(const U: TWeatherUnits; const Str: String);
  begin
    if U in S.Support.SupportedUnits then begin
      lstSupportedUnits.Items.Add(Str);
    end;
  end;
begin
  ClearInfo;
  if Selected then begin
    S:= IWeatherService(Item.Data);

    ChkUrl('Website', S.URLs.MainURL);
    ChkUrl('API Docs', S.URLs.ApiURL);
    ChkUrl('Register', S.URLs.RegisterURL);
    ChkUrl('Login', S.URLs.LoginURL);
    ChkUrl('Legal', S.URLs.LegalURL);

    ChkInfo(wiConditions, 'Conditions');
    ChkInfo(wiAlerts, 'Alerts');
    ChkInfo(wiForecastSummary, 'Forecast Summary');
    ChkInfo(wiForecastHourly, 'Forecast Hourly');
    ChkInfo(wiForecastDaily, 'Forecast Daily');
    ChkInfo(wiMaps, 'Maps');
    ChkInfo(wiAlmanac, 'Almanac');
    ChkInfo(wiAstronomy, 'Astronomy');
    ChkInfo(wiHurricane, 'Hurricane');
    ChkInfo(wiHistory, 'History');
    ChkInfo(wiPlanner, 'Planner');
    ChkInfo(wiStation, 'Station');

    ChkLoc(TJDWeatherLocationType.wlZip, 'Zip Code');
    ChkLoc(TJDWeatherLocationType.wlCityState, 'City and state');
    ChkLoc(TJDWeatherLocationType.wlCoords, 'Geographical Coordinates');
    ChkLoc(TJDWeatherLocationType.wlAutoIP, 'IP Address');
    ChkLoc(TJDWeatherLocationType.wlCityCode, 'City Code');
    ChkLoc(TJDWeatherLocationType.wlCountryCity, 'Country and City');
    ChkLoc(TJDWeatherLocationType.wlAirportCode, 'Airport Code');
    ChkLoc(TJDWeatherLocationType.wlPWS, 'PWS');

    ChkCon(TWeatherConditionsProp.cpPressureMB, 'Pressure MB');
    ChkCon(TWeatherConditionsProp.cpPressureIn, 'Pressure inHg');
    ChkCon(TWeatherConditionsProp.cpWindDir, 'Wind Direction');
    ChkCon(TWeatherConditionsProp.cpWindSpeed, 'Wind Speed');
    ChkCon(TWeatherConditionsProp.cpHumidity, 'Humidity');
    ChkCon(TWeatherConditionsProp.cpVisibility, 'Visibility');
    ChkCon(TWeatherConditionsProp.cpDewPoint, 'Dew Point');
    ChkCon(TWeatherConditionsProp.cpHeatIndex, 'Heat Index');
    ChkCon(TWeatherConditionsProp.cpWindGust, 'Wind Gusts');
    ChkCon(TWeatherConditionsProp.cpWindChill, 'Wind Chill');
    ChkCon(TWeatherConditionsProp.cpFeelsLike, 'Feels Like Temp');
    ChkCon(TWeatherConditionsProp.cpSolarRad, 'Solar Radiation');
    ChkCon(TWeatherConditionsProp.cpUV, 'UV Index');
    ChkCon(TWeatherConditionsProp.cpTemp, 'Temperature');
    ChkCon(TWeatherConditionsProp.cpTempMin, 'Min Temp');
    ChkCon(TWeatherConditionsProp.cpTempMax, 'Max Temp');
    ChkCon(TWeatherConditionsProp.cpPrecip, 'Precipitation Amount');
    ChkCon(TWeatherConditionsProp.cpIcon, 'Weather Condition Icon');
    ChkCon(TWeatherConditionsProp.cpCaption, 'Caption');
    ChkCon(TWeatherConditionsProp.cpDescription, 'Description');
    ChkCon(TWeatherConditionsProp.cpStation, 'Station');
    ChkCon(TWeatherConditionsProp.cpClouds, 'Cloud Cover');
    ChkCon(TWeatherConditionsProp.cpRain, 'Rain Amount');
    ChkCon(TWeatherConditionsProp.cpSnow, 'Snow Amount');
    ChkCon(TWeatherConditionsProp.cpSunrise, 'Sunrise');
    ChkCon(TWeatherConditionsProp.cpSunset, 'Sunset');

    ChkForSum(TWeatherForecastProp.fpPressureMB, 'Pressure MB');
    ChkForSum(TWeatherForecastProp.fpPressureIn, 'Pressure inHg');
    ChkForSum(TWeatherForecastProp.fpWindDir, 'Wind Direction');
    ChkForSum(TWeatherForecastProp.fpWindSpeed, 'Wind Speed');
    ChkForSum(TWeatherForecastProp.fpHumidity, 'Humidity');
    ChkForSum(TWeatherForecastProp.fpVisibility, 'Visibility');
    ChkForSum(TWeatherForecastProp.fpDewPoint, 'Dew Point');
    ChkForSum(TWeatherForecastProp.fpHeatIndex, 'Heat Index');
    ChkForSum(TWeatherForecastProp.fpWindGust, 'Wind Gusts');
    ChkForSum(TWeatherForecastProp.fpWindChill, 'Wind Chill');
    ChkForSum(TWeatherForecastProp.fpFeelsLike, 'Feels Like');
    ChkForSum(TWeatherForecastProp.fpSolarRad, 'Solar Radiation');
    ChkForSum(TWeatherForecastProp.fpUV, 'UV Index');
    ChkForSum(TWeatherForecastProp.fpTemp, 'Temperature');
    ChkForSum(TWeatherForecastProp.fpTempMin, 'Temp Min');
    ChkForSum(TWeatherForecastProp.fpTempMax, 'Temp Max');
    ChkForSum(TWeatherForecastProp.fpCaption, 'Caption');
    ChkForSum(TWeatherForecastProp.fpDescription, 'Description');
    ChkForSum(TWeatherForecastProp.fpIcon, 'Condition Icon');
    ChkForSum(TWeatherForecastProp.fpGroundPressure, 'Ground Pressure');
    ChkForSum(TWeatherForecastProp.fpSeaPressure, 'Sea Pressure');
    ChkForSum(TWeatherForecastProp.fpPrecip, 'Precipitation Amount');
    ChkForSum(TWeatherForecastProp.fpURL, 'Website URL');
    ChkForSum(TWeatherForecastProp.fpDaylight, 'Daylight Amount');
    ChkForSum(TWeatherForecastProp.fpSnow, 'Snow Amount');
    ChkForSum(TWeatherForecastProp.fpSleet, 'Sleet Amount');
    ChkForSum(TWeatherForecastProp.fpPrecipChance, 'Chance of Precipitation');
    ChkForSum(TWeatherForecastProp.fpClouds, 'Cloud Cover');
    ChkForSum(TWeatherForecastProp.fpRain, 'Rain Amount');

    ChkForHou(TWeatherForecastProp.fpPressureMB, 'Pressure MB');
    ChkForHou(TWeatherForecastProp.fpPressureIn, 'Pressure inHg');
    ChkForHou(TWeatherForecastProp.fpWindDir, 'Wind Direction');
    ChkForHou(TWeatherForecastProp.fpWindSpeed, 'Wind Speed');
    ChkForHou(TWeatherForecastProp.fpHumidity, 'Humidity');
    ChkForHou(TWeatherForecastProp.fpVisibility, 'Visibility');
    ChkForHou(TWeatherForecastProp.fpDewPoint, 'Dew Point');
    ChkForHou(TWeatherForecastProp.fpHeatIndex, 'Heat Index');
    ChkForHou(TWeatherForecastProp.fpWindGust, 'Wind Gusts');
    ChkForHou(TWeatherForecastProp.fpWindChill, 'Wind Chill');
    ChkForHou(TWeatherForecastProp.fpFeelsLike, 'Feels Like');
    ChkForHou(TWeatherForecastProp.fpSolarRad, 'Solar Radiation');
    ChkForHou(TWeatherForecastProp.fpUV, 'UV Index');
    ChkForHou(TWeatherForecastProp.fpTemp, 'Temperature');
    ChkForHou(TWeatherForecastProp.fpTempMin, 'Temp Min');
    ChkForHou(TWeatherForecastProp.fpTempMax, 'Temp Max');
    ChkForHou(TWeatherForecastProp.fpCaption, 'Caption');
    ChkForHou(TWeatherForecastProp.fpDescription, 'Description');
    ChkForHou(TWeatherForecastProp.fpIcon, 'Condition Icon');
    ChkForHou(TWeatherForecastProp.fpGroundPressure, 'Ground Pressure');
    ChkForHou(TWeatherForecastProp.fpSeaPressure, 'Sea Pressure');
    ChkForHou(TWeatherForecastProp.fpPrecip, 'Precipitation Amount');
    ChkForHou(TWeatherForecastProp.fpURL, 'Website URL');
    ChkForHou(TWeatherForecastProp.fpDaylight, 'Daylight Amount');
    ChkForHou(TWeatherForecastProp.fpSnow, 'Snow Amount');
    ChkForHou(TWeatherForecastProp.fpSleet, 'Sleet Amount');
    ChkForHou(TWeatherForecastProp.fpPrecipChance, 'Chance of Precipitation');
    ChkForHou(TWeatherForecastProp.fpClouds, 'Cloud Cover');
    ChkForHou(TWeatherForecastProp.fpRain, 'Rain Amount');

    ChkForDay(TWeatherForecastProp.fpPressureMB, 'Pressure MB');
    ChkForDay(TWeatherForecastProp.fpPressureIn, 'Pressure inHg');
    ChkForDay(TWeatherForecastProp.fpWindDir, 'Wind Direction');
    ChkForDay(TWeatherForecastProp.fpWindSpeed, 'Wind Speed');
    ChkForDay(TWeatherForecastProp.fpHumidity, 'Humidity');
    ChkForDay(TWeatherForecastProp.fpVisibility, 'Visibility');
    ChkForDay(TWeatherForecastProp.fpDewPoint, 'Dew Point');
    ChkForDay(TWeatherForecastProp.fpHeatIndex, 'Heat Index');
    ChkForDay(TWeatherForecastProp.fpWindGust, 'Wind Gusts');
    ChkForDay(TWeatherForecastProp.fpWindChill, 'Wind Chill');
    ChkForDay(TWeatherForecastProp.fpFeelsLike, 'Feels Like');
    ChkForDay(TWeatherForecastProp.fpSolarRad, 'Solar Radiation');
    ChkForDay(TWeatherForecastProp.fpUV, 'UV Index');
    ChkForDay(TWeatherForecastProp.fpTemp, 'Temperature');
    ChkForDay(TWeatherForecastProp.fpTempMin, 'Temp Min');
    ChkForDay(TWeatherForecastProp.fpTempMax, 'Temp Max');
    ChkForDay(TWeatherForecastProp.fpCaption, 'Caption');
    ChkForDay(TWeatherForecastProp.fpDescription, 'Description');
    ChkForDay(TWeatherForecastProp.fpIcon, 'Condition Icon');
    ChkForDay(TWeatherForecastProp.fpGroundPressure, 'Ground Pressure');
    ChkForDay(TWeatherForecastProp.fpSeaPressure, 'Sea Pressure');
    ChkForDay(TWeatherForecastProp.fpPrecip, 'Precipitation Amount');
    ChkForDay(TWeatherForecastProp.fpURL, 'Website URL');
    ChkForDay(TWeatherForecastProp.fpDaylight, 'Daylight Amount');
    ChkForDay(TWeatherForecastProp.fpSnow, 'Snow Amount');
    ChkForDay(TWeatherForecastProp.fpSleet, 'Sleet Amount');
    ChkForDay(TWeatherForecastProp.fpPrecipChance, 'Chance of Precipitation');
    ChkForDay(TWeatherForecastProp.fpClouds, 'Cloud Cover');
    ChkForDay(TWeatherForecastProp.fpRain, 'Rain Amount');

    ChkAltTyp(TWeatherAlertType.waHurricaneStat, 'Hurricane Status');
    ChkAltTyp(TWeatherAlertType.waTornadoWarn, 'Tornado Warning');
    ChkAltTyp(TWeatherAlertType.waTornadoWatch, 'Tornado Watch');
    ChkAltTyp(TWeatherAlertType.waSevThundWarn, 'Severe Thunderstorm Warning');
    ChkAltTyp(TWeatherAlertType.waSevThundWatch, 'Severe Thunderstorm Watch');
    ChkAltTyp(TWeatherAlertType.waWinterAdv, 'Winter Weather Advisory');
    ChkAltTyp(TWeatherAlertType.waFloodWarn, 'Flood Warning');
    ChkAltTyp(TWeatherAlertType.waFloodWatch, 'Flood Watch');
    ChkAltTyp(TWeatherAlertType.waHighWind, 'High Wind Advisory');
    ChkAltTyp(TWeatherAlertType.waSevStat, 'Severe Weather Status');
    ChkAltTyp(TWeatherAlertType.waHeatAdv, 'Heat Advisory');
    ChkAltTyp(TWeatherAlertType.waFogAdv, 'Fog Advisory');
    ChkAltTyp(TWeatherAlertType.waSpecialStat, 'Special Weather Statement');
    ChkAltTyp(TWeatherAlertType.waFireAdv, 'Fire Advisory');
    ChkAltTyp(TWeatherAlertType.waVolcanicStat, 'Volcanic Status');
    ChkAltTyp(TWeatherAlertType.waHurricaneWarn, 'Hurricane Warning');
    ChkAltTyp(TWeatherAlertType.waRecordSet, 'Record Set');
    ChkAltTyp(TWeatherAlertType.waPublicRec, 'Public Record');
    ChkAltTyp(TWeatherAlertType.waPublicStat, 'Public Status');

    ChkAltPro(TWeatherAlertProp.apZones, 'Alerted Zones');
    ChkAltPro(TWeatherAlertProp.apVerticies, 'Storm Verticies');
    ChkAltPro(TWeatherAlertProp.apStorm, 'Storm Information');
    ChkAltPro(TWeatherAlertProp.apType, 'Alert Type');
    ChkAltPro(TWeatherAlertProp.apDescription, 'Description');
    ChkAltPro(TWeatherAlertProp.apExpires, 'Expiration Time');
    ChkAltPro(TWeatherAlertProp.apMessage, 'Alert Message');
    ChkAltPro(TWeatherAlertProp.apPhenomena, 'Phenomena');
    ChkAltPro(TWeatherAlertProp.apSignificance, 'Significance');

    ChkMap(TWeatherMapType.mpSatellite, 'Satellite');
    ChkMap(TWeatherMapType.mpRadar, 'Radar');
    ChkMap(TWeatherMapType.mpSatelliteRadar, 'Satellite and Radar');
    ChkMap(TWeatherMapType.mpAniSatellite, 'Animated Satellite');
    ChkMap(TWeatherMapType.mpAniRadar, 'Animated Radar');
    ChkMap(TWeatherMapType.mpAniSatelliteRadar, 'Animated Satellite and Radar');

    ChkMapFor(TWeatherMapFormat.wfJpg, 'Jpg Format');
    ChkMapFor(TWeatherMapFormat.wfPng, 'Png Format');
    ChkMapFor(TWeatherMapFormat.wfGif, 'Gif Format');
    ChkMapFor(TWeatherMapFormat.wfTiff, 'Tiff Format');
    ChkMapFor(TWeatherMapFormat.wfBmp, 'Bmp Format');
    ChkMapFor(TWeatherMapFormat.wfFlash, 'Flash Format');
    ChkMapFor(TWeatherMapFormat.wfHtml, 'Html Format');

    ChkUni(TWeatherUnits.wuKelvin, 'Kelvin');
    ChkUni(TWeatherUnits.wuImperial, 'Imperial');
    ChkUni(TWeatherUnits.wuMetric, 'Metric');

    WeatherImageToPicture(S.GetLogo(ltColor), imgLogo.Picture);

  end;
end;

procedure TfrmMain.lstURLsClick(Sender: TObject);
var
  U: String;
begin
  if lstURLs.ItemIndex >= 0 then begin
    U:= lstURLs.Selected.SubItems[0];
    ShellExecute(0, 'open', PChar(U), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TfrmMain.SvrConnect(AContext: TIdContext);
var
  C: TWeatherContext;
begin
  C:= TWeatherContext(AContext);

end;

procedure TfrmMain.SvrContextCreated(AContext: TIdContext);
var
  C: TWeatherContext;
  Dir: String;
begin
  C:= TWeatherContext(AContext);
  C.FCreateLib:= Self.FCreateLib;
  Dir:= ExtractFilePath(ParamStr(0));
  C.FWeather:= C.FCreateLib(Dir);
end;

procedure TfrmMain.SvrDisconnect(AContext: TIdContext);
var
  C: TWeatherContext;
begin
  C:= TWeatherContext(AContext);

end;

procedure TfrmMain.HandleServiceSupport(AContext: TWeatherContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
  const Svc: IWeatherService);
var
  O, O2: ISuperObject;
  S: IWeatherService;
  Loc: TJDWeatherLocationType;
  Inf: TWeatherInfoType;
  Uni: TWeatherUnits;
  Alt: TWeatherAlertType;
  Alp: TWeatherAlertProp;
  Fop: TWeatherForecastProp;
  Map: TWeatherMapType;
begin
  O:= SO;
  try
    O.S['caption']:= Svc.Caption;
    O.S['serviceuid']:= Svc.UID;

    O2:= SA([]);
    try
      for Loc := Low(TJDWeatherLocationType) to High(TJDWeatherLocationType) do begin
        if Loc in Svc.Support.SupportedLocations then
          O2.AsArray.Add(SO(GetEnumName(TypeInfo(TJDWeatherLocationType), Ord(Loc))));
      end;
    finally
      O.O['supported_locations']:= O2;
    end;

    O2:= SA([]);
    try
      for Inf := Low(TWeatherInfoType) to High(TWeatherInfoType) do begin
        if Inf in Svc.Support.SupportedInfo then
          O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherInfoType), Ord(Inf))));
      end;
    finally
      O.O['supported_info']:= O2;
    end;

    O2:= SA([]);
    try
      for Uni := Low(TWeatherUnits) to High(TWeatherUnits) do begin
        if Uni in Svc.Support.SupportedUnits then
          O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherUnits), Ord(Uni))));
      end;
    finally
      O.O['supported_units']:= O2;
    end;

    O2:= SA([]);
    try
      for Alt := Low(TWeatherAlertType) to High(TWeatherAlertType) do begin
        if Alt in Svc.Support.SupportedAlerts then
          O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherAlertType), Ord(Alt))));
      end;
    finally
      O.O['supported_alerts']:= O2;
    end;

    O2:= SA([]);
    try
      for Alp := Low(TWeatherAlertProp) to High(TWeatherAlertProp) do begin
        if Alp in Svc.Support.SupportedAlertProps then
          O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherAlertProp), Ord(Alp))));
      end;
    finally
      O.O['supported_alert_props']:= O2;
    end;

    O2:= SA([]);
    try
      for Fop := Low(TWeatherForecastProp) to High(TWeatherForecastProp) do begin
        if Fop in Svc.Support.SupportedForecastSummaryProps then
          O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherForecastProp), Ord(Fop))));
      end;
    finally
      O.O['supported_forecast_summary_props']:= O2;
    end;

    O2:= SA([]);
    try
      for Fop := Low(TWeatherForecastProp) to High(TWeatherForecastProp) do begin
        if Fop in Svc.Support.SupportedForecastHourlyProps then
          O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherForecastProp), Ord(Fop))));
      end;
    finally
      O.O['supported_forecast_hourly_props']:= O2;
    end;

    O2:= SA([]);
    try
      for Fop := Low(TWeatherForecastProp) to High(TWeatherForecastProp) do begin
        if Fop in Svc.Support.SupportedForecastDailyProps then
          O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherForecastProp), Ord(Fop))));
      end;
    finally
      O.O['supported_forecast_daily_props']:= O2;
    end;

    O2:= SA([]);
    try
      for Map := Low(TWeatherMapType) to High(TWeatherMapType) do begin
        if Map in Svc.Support.SupportedMaps then
          O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherMapType), Ord(Map))));
      end;
    finally
      O.O['supported_maps']:= O2;
    end;





  finally
    AResponseInfo.ContentText:= O.AsJSon(True);
    AResponseInfo.ContentType:= 'text/json';
  end;
end;

procedure TfrmMain.HandleServiceList(AContext: TWeatherContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  O, O2: ISuperObject;
  X: Integer;
  S: IWeatherService;
begin
  O:= SO;
  try
    O.O['services']:= SA([]);
    for X := 0 to FWeather.Services.Count-1 do begin
      S:= FWeather.Services[X];
      O2:= SO;
      try
        O2.S['caption']:= S.Caption;
        O2.S['uid']:= S.UID;
        O2.S['url_main']:= S.URLs.MainURL;
        O2.S['url_api']:= S.URLs.ApiURL;
        O2.S['url_login']:= S.URLs.LoginURL;
        O2.S['url_register']:= S.URLs.RegisterURL;
        O2.S['url_legal']:= S.URLs.LegalURL;
      finally
        O.O['services'].AsArray.Add(O2);
      end;
    end;
  finally
    AResponseInfo.ContentText:= O.AsJSon(True);
    AResponseInfo.ContentType:= 'text/json';
  end;
end;

procedure TfrmMain.HandleServiceGet(AContext: TWeatherContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
  const Svc: IWeatherService);
var
  O: ISuperObject;
begin
  O:= SO;
  try

  finally
    AResponseInfo.ContentText:= O.AsJSon(True);
    AResponseInfo.ContentType:= 'text/json';
  end;
end;

procedure TfrmMain.SvrCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  C: TWeatherContext;
  X: Integer;
  D, T: String;
  P: Integer;
begin
  C:= TWeatherContext(AContext);
  C.FKey:= ARequestInfo.Params.Values['k'];
  C.FServiceUID:= ARequestInfo.Params.Values['s'];
  C.FLocType:= ARequestInfo.Params.Values['l'];
  C.FLoc1:= ARequestInfo.Params.Values['l1'];
  C.FLoc2:= ARequestInfo.Params.Values['l2'];
  C.FUnits:= ARequestInfo.Params.Values['u'];
  C.FDet:= ARequestInfo.Params.Values['d'];

  C.FDoc.Clear;
  D:= ARequestInfo.Document;
  Delete(D, 1, 1);
  D:= D + '/';
  while Length(D) > 0 do begin
    P:= Pos('/', D);
    T:= Copy(D, 1, P-1);
    Delete(D, 1, P);
    C.FDoc.Append(T);
  end;

  if C.FDoc.Count > 0 then begin
    if C.FDoc[0] = 'services' then begin
      //Return list of available services
      HandleServiceList(C, ARequestInfo, AResponseInfo);
    end else
    if C.FDoc[0] = 'support' then begin
      for X := 0 to FWeather.Services.Count-1 do begin
        if FWeather.Services[X].UID = C.FServiceUID then begin
          HandleServiceSupport(C, ARequestInfo, AResponseInfo, FWeather.Services[X]);
          Break;
        end;
      end;
    end else begin
      for X := 0 to FWeather.Services.Count-1 do begin
        if FWeather.Services[X].UID = C.FServiceUID then begin
          HandleServiceGet(C, ARequestInfo, AResponseInfo, FWeather.Services[X]);
          Break;
        end;
      end;
    end;
  end else begin
    //No document was requested...

  end;
end;

end.
