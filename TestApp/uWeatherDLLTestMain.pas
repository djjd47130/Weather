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
  TInfo, TCond, TLoc, TAlert, TAlertProp, TForSum, TForHour, TForDay, TMaps, TUnits: Integer;
  PInfo, PCond, PLoc, PAlert, PAlertProp, PForSum, PForHour, PForDay, PMaps, PUnits: Single;
  TP: Single;
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
    TInfo:= lstSupportedInfo.Items.Count;

    ChkLoc(TJDWeatherLocationType.wlZip, 'Zip Code');
    ChkLoc(TJDWeatherLocationType.wlCityState, 'City and state');
    ChkLoc(TJDWeatherLocationType.wlCoords, 'Geographical Coordinates');
    ChkLoc(TJDWeatherLocationType.wlAutoIP, 'IP Address');
    ChkLoc(TJDWeatherLocationType.wlCityCode, 'City Code');
    ChkLoc(TJDWeatherLocationType.wlCountryCity, 'Country and City');
    ChkLoc(TJDWeatherLocationType.wlAirportCode, 'Airport Code');
    ChkLoc(TJDWeatherLocationType.wlPWS, 'PWS');
    TLoc:= lstSupportedLocationTypes.Items.Count;

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
    TCond:= lstSupportedConditionProps.Items.Count;

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
    TForSum:= lstSupportedForecastSummaryProps.Items.Count;

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
    TForHour:= lstSupportedForecastHourlyProps.Items.Count;

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
    TForDay:= lstSupportedForecastDailyProps.Items.Count;

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
    TAlert:= lstSupportedAlertTypes.Items.Count;

    ChkAltPro(TWeatherAlertProp.apZones, 'Alerted Zones');
    ChkAltPro(TWeatherAlertProp.apVerticies, 'Storm Verticies');
    ChkAltPro(TWeatherAlertProp.apStorm, 'Storm Information');
    ChkAltPro(TWeatherAlertProp.apType, 'Alert Type');
    ChkAltPro(TWeatherAlertProp.apDescription, 'Description');
    ChkAltPro(TWeatherAlertProp.apExpires, 'Expiration Time');
    ChkAltPro(TWeatherAlertProp.apMessage, 'Alert Message');
    ChkAltPro(TWeatherAlertProp.apPhenomena, 'Phenomena');
    ChkAltPro(TWeatherAlertProp.apSignificance, 'Significance');
    TAlertProp:= lstSupportedAlertProps.Items.Count;

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
    TMaps:= lstSupportedMaps.Items.Count;

    ChkUni(TWeatherUnits.wuKelvin, 'Kelvin');
    ChkUni(TWeatherUnits.wuImperial, 'Imperial');
    ChkUni(TWeatherUnits.wuMetric, 'Metric');
    TUnits:= lstSupportedUnits.Items.Count;


    PInfo:= TInfo / (Integer(High(TWeatherInfoType))+1);
    PLoc:= TLoc / (Integer(High(TJDWeatherLocationType))+1);
    PCond:= TCond / (Integer(High(TWeatherConditionsProp))+1);
    PAlert:= TAlert / (Integer(High(TWeatherAlertType))+1);
    PAlertProp:= TAlertProp / (Integer(High(TWeatherAlertProp))+1);
    PForSum:= TForSum / (Integer(High(TWeatherForecastProp))+1);
    PForHour:= TForHour / (Integer(High(TWeatherForecastProp))+1);
    PForDay:= TForDay / (Integer(High(TWeatherForecastProp))+1);
    PMaps:= TMaps / (Integer(High(TWeatherMapType))+1);
    PUnits:= TUnits / (Integer(High(TWeatherUnits))+1);

    lstSupportedInfo.Items.Add('-Percent: '+FormatFloat('0.00%', PInfo*100));
    lstSupportedLocationTypes.Items.Add('-Percent: '+FormatFloat('0.00%', PLoc*100));
    lstSupportedConditionProps.Items.Add('-Percent: '+FormatFloat('0.00%', PCond*100));
    lstSupportedAlertTypes.Items.Add('-Percent: '+FormatFloat('0.00%', PAlert*100));
    lstSupportedAlertProps.Items.Add('-Percent: '+FormatFloat('0.00%', PAlertProp*100));
    lstSupportedForecastSummaryProps.Items.Add('-Percent: '+FormatFloat('0.00%', PForSum*100));
    lstSupportedForecastHourlyProps.Items.Add('-Percent: '+FormatFloat('0.00%', PForHour*100));
    lstSupportedForecastDailyProps.Items.Add('-Percent: '+FormatFloat('0.00%', PForDay*100));
    lstSupportedMaps.Items.Add('-Percent: '+FormatFloat('0.00%', PMaps*100));
    lstSupportedUnits.Items.Add('-Percent: '+FormatFloat('0.00%', PUnits*100));

    TP:=(PInfo + PLoc + PCond + PAlert + PAlertProp + PForSum +
      PForHour + PForDay + PMaps + PUnits) / 10;
    Caption:= 'JD Weather DLL Test - '+S.Caption+' - '+FormatFloat('0.00%', TP*100);

    WeatherImageToPicture(S.GetLogo(ltColor), imgLogo.Picture);
  end else begin
    Caption:= 'JD Weather DLL Test';
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

end.
