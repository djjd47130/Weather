unit uWeatherDLLTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  JD.Weather.Intf,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  ShellAPI;

type
  TfrmMain = class(TForm)
    SB: TScrollBox;
    pOne: TPanel;
    Panel1: TPanel;
    Label1: TLabel;
    lstSupportedInfo: TListBox;
    Panel2: TPanel;
    Label2: TLabel;
    lstSupportedLocationTypes: TListBox;
    Panel4: TPanel;
    lstServices: TListView;
    lstURLs: TListView;
    Splitter1: TSplitter;
    imgLogo: TImage;
    Splitter2: TSplitter;
    Panel5: TPanel;
    Label3: TLabel;
    lstSupportedConditionProps: TListBox;
    pTwo: TPanel;
    Panel6: TPanel;
    Label4: TLabel;
    lstSupportedForecastSummaryProps: TListBox;
    Panel7: TPanel;
    Label5: TLabel;
    lstSupportedForecastHourlyProps: TListBox;
    Panel8: TPanel;
    Label6: TLabel;
    lstSupportedForecastDailyProps: TListBox;
    Panel3: TPanel;
    Label7: TLabel;
    lstSupportedAlertProps: TListBox;
    Panel9: TPanel;
    Label8: TLabel;
    lstSupportedMaps: TListBox;
    Splitter3: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure lstServicesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lstURLsClick(Sender: TObject);
    procedure pOneResize(Sender: TObject);
    procedure pTwoResize(Sender: TObject);
  private
    FLib: HMODULE;
    FCreateLib: TCreateJDWeather;
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

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown:= True;
  SB.Align:= alClient;
  lstURLs.Align:= alClient;
  pOne.Height:= 300;
  pTwo.Height:= 300;
  lstServices.Width:= lstServices.Width + 1;

  FLib:= LoadLibrary('JDWeather.dll');
  if FLib <> 0 then begin
    FCreateLib:= GetProcAddress(FLib, 'CreateJDWeather');
    if Assigned(FCreateLib) then begin
      FWeather:= FCreateLib(ExtractFilePath(ParamStr(0)));
      LoadServices;
    end else begin
      //TODO
    end;
  end else begin
    //TODO
  end;
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
      I.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TfrmMain.ClearInfo;
begin
  lstURLs.Items.Clear;
  lstSupportedInfo.Items.Clear;
  lstSupportedLocationTypes.Items.Clear;
  lstSupportedConditionProps.Items.Clear;
  lstSupportedAlertProps.Items.Clear;
  lstSupportedForecastSummaryProps.Items.Clear;
  lstSupportedForecastHourlyProps.Items.Clear;
  lstSupportedForecastDailyProps.Items.Clear;
  lstSupportedMaps.Items.Clear;
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
begin
  ClearInfo;
  if Selected then begin
    S:= IWeatherService(Item.Data);

    ChkUrl('Main Website', S.URLs.MainURL);
    ChkUrl('API Page', S.URLs.ApiURL);
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

procedure TfrmMain.pOneResize(Sender: TObject);
var
  W: Integer;
  X: Integer;
begin
  W:= Trunc(pOne.ClientWidth div 4);
  for X := 0 to pOne.ControlCount-1 do begin
    pOne.Controls[X].Width:= W;
  end;
end;

procedure TfrmMain.pTwoResize(Sender: TObject);
var
  W: Integer;
  X: Integer;
begin
  W:= Trunc(pTwo.ClientWidth div 4);
  for X := 0 to pTwo.ControlCount-1 do begin
    pTwo.Controls[X].Width:= W;
  end;
end;

end.
