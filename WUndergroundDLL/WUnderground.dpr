library WUnderground;

{$R 'WUndergroundRes.res' 'WUndergroundRes.rc'}

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  StrUtils,
  JD.Weather.Intf in '..\JD.Weather.Intf.pas',
  JD.Weather.SuperObject in '..\JD.Weather.SuperObject.pas';

{$R *.res}

type
  TWUEndpoint = (weAll, weAlerts, weAlmanac, weAstronomy, weConditions,
    weCurrentHurricane, weForecast, weForecast10Day, weGeoLookup, weHistory,
    weHourly, weHourly10Day, wePlanner, weRawTide, weTide, weWebCams, weYesterday,
    weRadar, weSatellite, weRadarSatellite,
    weAniRadar, weAniSatellite, weAniRadarSatellite);
  TWUEndpoints = set of TWUEndpoint;

  TWUWeatherSupport = class(TInterfacedObject, IWeatherSupport)
  public
    function GetSupportedLogos: TWeatherLogoTypes;
    function GetSupportedUnits: TWeatherUnitsSet;
    function GetSupportedInfo: TWeatherInfoTypes;
    function GetSupportedLocations: TJDWeatherLocationTypes;
    function GetSupportedAlerts: TWeatherAlertTypes;
    function GetSupportedAlertProps: TWeatherAlertProps;
    function GetSupportedConditionProps: TWeatherConditionsProps;
    function GetSupportedForecasts: TWeatherForecastTypes;
    function GetSupportedForecastSummaryProps: TWeatherForecastProps;
    function GetSupportedForecastHourlyProps: TWeatherForecastProps;
    function GetSupportedForecastDailyProps: TWeatherForecastProps;
    function GetSupportedMaps: TWeatherMapTypes;
    function GetSupportedMapFormats: TWeatherMapFormats;

    property SupportedLogos: TWeatherLogoTypes read GetSupportedLogos;
    property SupportedUnits: TWeatherUnitsSet read GetSupportedUnits;
    property SupportedInfo: TWeatherInfoTypes read GetSupportedInfo;
    property SupportedLocations: TJDWeatherLocationTypes read GetSupportedLocations;
    property SupportedAlerts: TWeatherAlertTypes read GetSupportedAlerts;
    property SupportedAlertProps: TWeatherAlertProps read GetSupportedAlertProps;
    property SupportedConditionProps: TWeatherConditionsProps read GetSupportedConditionProps;
    property SupportedForecasts: TWeatherForecastTypes read GetSupportedForecasts;
    property SupportedForecastSummaryProps: TWeatherForecastProps read GetSupportedForecastSummaryProps;
    property SupportedForecastHourlyProps: TWeatherForecastProps read GetSupportedForecastHourlyProps;
    property SupportedForecastDailyProps: TWeatherForecastProps read GetSupportedForecastDailyProps;
    property SupportedMaps: TWeatherMapTypes read GetSupportedMaps;
    property SupportedMapFormats: TWeatherMapFormats read GetSupportedMapFormats;
  end;

  TWUWeatherURLs = class(TInterfacedObject, IWeatherURLs)
  public
    function GetMainURL: WideString;
    function GetApiURL: WideString;
    function GetLoginURL: WideString;
    function GetRegisterURL: WideString;
    function GetLegalURL: WideString;

    property MainURL: WideString read GetMainURL;
    property ApiURL: WideString read GetApiURL;
    property LoginURL: WideString read GetLoginURL;
    property RegisterURL: WideString read GetRegisterURL;
    property LegalURL: WideString read GetLegalURL;
  end;

  TWUService = class(TWeatherServiceBase, IWeatherService)
  private
    FSupport: TWUWeatherSupport;
    FURLs: TWUWeatherURLs;
    function GetEndpointUrl(const Endpoint: TWUEndpoint): String;
    function GetMultiEndpointUrl(const Endpoints: TWUEndpoints; const Ext: String): String;
    function GetEndpoint(const Endpoint: TWUEndpoint): ISuperObject;
    function GetMultiEndpoints(const Endpoints: TWUEndpoints; const Ext: String): ISuperObject;
    function StrToAlertType(const S: String): TWeatherAlertType;
    procedure FillConditions(const O: ISuperObject;
      Conditions: TWeatherConditions);
    procedure FillAlerts(const O: ISuperObject; Alerts: TWeatherAlerts);
    procedure FillForecastDaily(const O: ISuperObject;
      Forecast: TWeatherForecast);
    procedure FillForecastHourly(const O: ISuperObject;
      Forecast: TWeatherForecast);
    procedure FillForecastSummary(const O: ISuperObject;
      Forecast: TWeatherForecast);
    procedure LoadLogos;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    function GetUID: WideString;
    function GetCaption: WideString;
    function GetURLs: IWeatherURLs;
    function Support: IWeatherSupport;

    function GetMultiple(const Info: TWeatherInfoTypes): IWeatherMultiInfo;
    function GetConditions: IWeatherConditions;
    function GetAlerts: IWeatherAlerts;
    function GetForecastSummary: IWeatherForecast;
    function GetForecastHourly: IWeatherForecast;
    function GetForecastDaily: IWeatherForecast;
    function GetMaps: IWeatherMaps;

    property UID: WideString read GetUID;
    property Caption: WideString read GetCaption;
    property URLs: IWeatherURLs read GetURLs;
  end;

{ TWUWeatherSupport }

function TWUWeatherSupport.GetSupportedUnits: TWeatherUnitsSet;
begin
  Result:= [wuImperial, wuMetric];
end;

function TWUWeatherSupport.GetSupportedLocations: TJDWeatherLocationTypes;
begin
  Result:= [wlZip, wlCityState, wlCoords, wlAutoIP,
    wlCountryCity, wlAirportCode, wlPWS];
end;

function TWUWeatherSupport.GetSupportedLogos: TWeatherLogoTypes;
begin
  Result:= [ltColor, ltColorInvert, ltColorWide, ltColorInvertWide,
    ltColorLeft, ltColorRight];
end;

function TWUWeatherSupport.GetSupportedAlertProps: TWeatherAlertProps;
begin
  Result:= [apZones, apVerticies, apStorm, apType,
    apDescription, apExpires, apMessage, apPhenomena, apSignificance];
end;

function TWUWeatherSupport.GetSupportedAlerts: TWeatherAlertTypes;
begin
  Result:= [waNone, waHurricaneStat, waTornadoWarn,
    waTornadoWatch, waSevThundWarn, waSevThundWatch, waWinterAdv,
    waFloodWarn, waFloodWatch, waHighWind, waSevStat, waHeatAdv, waFogAdv,
    waSpecialStat, waFireAdv, waVolcanicStat, waHurricaneWarn,
    waRecordSet, waPublicRec, waPublicStat];
end;

function TWUWeatherSupport.GetSupportedConditionProps: TWeatherConditionsProps;
begin
  Result:= [cpPressureMB, cpPressureIn, cpWindDir,
    cpWindSpeed, cpHumidity, cpVisibility, cpDewPoint, cpHeatIndex,
    cpWindGust, cpWindChill, cpFeelsLike, cpSolarRad, cpUV, cpTemp, cpTempMin,
    cpTempMax, cpPrecip, cpIcon, cpCaption, cpDescription, cpStation,
    cpClouds, cpRain, cpSnow, cpSunrise, cpSunset];
end;

function TWUWeatherSupport.GetSupportedForecasts: TWeatherForecastTypes;
begin
  Result:= [ftSummary, ftHourly, ftDaily];
end;

function TWUWeatherSupport.GetSupportedForecastDailyProps: TWeatherForecastProps;
begin
  Result:= [fpWindDir,
    fpWindSpeed,
    fpHumidity,
    fpTemp,
    fpTempMin,
    fpTempMax,
    fpCaption,
    fpIcon,
    fpPrecip,
    fpSnow,
    fpPrecipChance];
end;

function TWUWeatherSupport.GetSupportedForecastHourlyProps: TWeatherForecastProps;
begin
  Result:= [fpPressureMB,
    fpPressureIn,
    fpWindDir,
    fpWindSpeed,
    fpHumidity,
    fpDewPoint,
    fpHeatIndex,
    fpWindChill,
    fpFeelsLike,
    fpUV,
    fpTemp,
    fpCaption,
    fpIcon,
    fpPrecip,
    fpSnow,
    fpPrecipChance];
end;

function TWUWeatherSupport.GetSupportedForecastSummaryProps: TWeatherForecastProps;
begin
  Result:= [fpCaption,
    fpDescription,
    fpIcon,
    fpPrecipChance];
end;

function TWUWeatherSupport.GetSupportedInfo: TWeatherInfoTypes;
begin
  Result:= [wiConditions, wiAlerts, wiForecastSummary,
    wiForecastHourly, wiForecastDaily, wiMaps];
end;

function TWUWeatherSupport.GetSupportedMapFormats: TWeatherMapFormats;
begin
  Result:= [wfPng, wfGif];
end;

function TWUWeatherSupport.GetSupportedMaps: TWeatherMapTypes;
begin
  Result:= [mpSatellite, mpRadar, mpSatelliteRadar,
    mpAniSatellite, mpAniRadar, mpAniSatelliteRadar];
end;

{ TWUWeatherURLs }

function TWUWeatherURLs.GetApiURL: WideString;
begin
  Result:= 'https://www.wunderground.com/weather/api/';
end;

function TWUWeatherURLs.GetLegalURL: WideString;
begin
  Result:= 'https://www.wunderground.com/weather/api/d/terms.html';
end;

function TWUWeatherURLs.GetLoginURL: WideString;
begin
  Result:= 'https://www.wunderground.com/login.asp';
end;

function TWUWeatherURLs.GetMainURL: WideString;
begin
  Result:= 'http://www.wunderground.com';
end;

function TWUWeatherURLs.GetRegisterURL: WideString;
begin
  Result:= 'https://www.wunderground.com/member/registration';
end;

{ TWUService }

constructor TWUService.Create;
begin
  inherited;
  FSupport:= TWUWeatherSupport.Create;
  FSupport._AddRef;
  FURLs:= TWUWeatherURLs.Create;
  FURLs._AddRef;
  LoadLogos;
end;

destructor TWUService.Destroy;
begin
  FURLs._Release;
  FURLs:= nil;
  FSupport._Release;
  FSupport:= nil;
  inherited;
end;

procedure TWUService.LoadLogos;
  function Get(const N: String): IWeatherGraphic;
  var
    S: TResourceStream;
    R: TStringStream;
  begin
    Result:= TWeatherGraphic.Create;
    S:= TResourceStream.Create(HInstance, N, 'JPG');
    try
      R:= TStringStream.Create;
      try
        S.Position:= 0;
        R.LoadFromStream(S);
        R.Position:= 0;
        Result.Base64:= R.DataString;
      finally
        R.Free;
      end;
    finally
      S.Free;
    end;
  end;
begin
  //TODO: Load Logos from Resources
  SetLogo(TWeatherLogoType.ltColor, Get('LOGO_COLOR'));


end;

function TWUService.GetUID: WideString;
begin
  Result:= '{87940B1A-0435-4E2D-8DE8-6DE3BDCD43BB}';
end;

function TWUService.GetCaption: WideString;
begin
  Result:= 'Weather Underground';
end;

function TWUService.GetURLs: IWeatherURLs;
begin
  Result:= FURLs;
end;

function TWUService.Support: IWeatherSupport;
begin
  Result:= IWeatherSupport(FSupport);
end;

function TWUService.GetEndpointUrl(const Endpoint: TWUEndpoint): String;
var
  S: String;
begin
  case Endpoint of
    weAll:              S:= 'conditions/alerts/hourly/forecast10day';
    weAlerts:           S:= 'alerts';
    weAlmanac:          S:= 'almanac';
    weAstronomy:        S:= 'astronomy';
    weConditions:       S:= 'conditions';
    weCurrentHurricane: S:= 'currenthurricane';
    weForecast:         S:= 'forecast';
    weForecast10Day:    S:= 'forecast10day';
    weGeoLookup:        S:= 'geolookup';
    weHistory:          S:= 'history';
    weHourly:           S:= 'hourly';
    weHourly10Day:      S:= 'hourly10day';
    wePlanner:          S:= 'planner';
    weRawTide:          S:= 'rawtide';
    weTide:             S:= 'tide';
    weWebCams:          S:= 'webcams';
    weYesterday:        S:= 'yesterday';

    weRadar:            S:= 'radar';
    weSatellite:        S:= 'satellite';
    weRadarSatellite:   S:= 'radar/satellite';
    weAniRadar:         S:= 'animatedradar';
    weAniSatellite:     S:= 'animatedsatellite';
    weAniRadarSatellite:  S:= 'animatedradar/animatedsatellite';
  end;
  Result:= 'http://api.wunderground.com/api/'+Key+'/' + S + '/q/';
  case LocationType of
    wlZip:          Result:= Result + LocationDetail1;
    wlCityState:    Result:= Result + LocationDetail2+'/'+LocationDetail1;
    wlCoords:       Result:= Result + LocationDetail1+','+LocationDetail2;
    wlAutoIP:       Result:= Result + 'autoip';
    wlCityCode:     Result:= Result + ''; //TODO
    wlCountryCity:  Result:= Result + LocationDetail1+'/'+LocationDetail2;
    wlAirportCode:  Result:= Result + LocationDetail1;
    wlPWS:          Result:= Result + 'pws:'+LocationDetail1;
  end;
  case Endpoint of
    weRadar..weAniRadarSatellite: begin
      Result:= Result + '.gif';
    end;
    else begin
      Result:= Result + '.json';
    end;
  end;
end;

function TWUService.GetMultiEndpointUrl(const Endpoints: TWUEndpoints; const Ext: String): String;
var
  S: String;
  procedure Chk(const E: TWUEndpoint; const V: String);
  begin
    if E in Endpoints then begin
      if S <> '' then S:= S + '/';
      S:= S + V;
    end;
  end;
begin
  S:= '';
  Chk(weAlerts, 'alerts');
  Chk(weAlmanac, 'almanac');
  Chk(weAstronomy, 'astronomy');
  Chk(weConditions, 'conditions');
  Chk(weCurrentHurricane, 'currenthurricane');
  Chk(weForecast, 'forecast');
  Chk(weForecast10Day, 'forecast10day');
  Chk(weGeoLookup, 'geolookup');
  Chk(weHistory, 'history');
  Chk(weHourly, 'hourly');
  Chk(weHourly10Day, 'hourly10day');
  Chk(wePlanner, 'planner');
  Chk(weRawTide, 'rawtide');
  Chk(weTide, 'tide');
  Chk(weWebCams, 'webcams');
  Chk(weYesterday, 'yesterday');
  Chk(weRadar, 'radar');
  Chk(weSatellite, 'satellite');
  Chk(weAniRadar, 'animatedradar');
  Chk(weAniSatellite, 'animatedsatellite');

  Result:= 'http://api.wunderground.com/api/'+Key+'/' + S + '/q/';
  case LocationType of
    wlZip:          Result:= Result + LocationDetail1;
    wlCityState:    Result:= Result + LocationDetail2+'/'+LocationDetail1;
    wlCoords:       Result:= Result + LocationDetail1+','+LocationDetail2;
    wlAutoIP:       Result:= Result + 'autoip';
    wlCityCode:     Result:= Result + ''; //TODO
    wlCountryCity:  Result:= Result + LocationDetail1+'/'+LocationDetail2;
    wlAirportCode:  Result:= Result + LocationDetail1;
    wlPWS:          Result:= Result + 'pws:'+LocationDetail1;
  end;
  Result:= Result + Ext;
end;

function TWUService.GetEndpoint(const Endpoint: TWUEndpoint): ISuperObject;
var
  U: String;
  S: String;
begin
  U:= GetEndpointUrl(Endpoint);
  S:= Web.Get(U);
  Result:= SO(S);
end;

function TWUService.GetMultiEndpoints(const Endpoints: TWUEndpoints; const Ext: String): ISuperObject;
var
  U: String;
  S: String;
begin
  U:= GetMultiEndpointUrl(Endpoints, Ext);
  S:= Web.Get(U);
  Result:= SO(S);
end;

function TWUService.GetMultiple(const Info: TWeatherInfoTypes): IWeatherMultiInfo;
var
  O: ISuperObject;
  R: TWeatherMultiInfo;
  Con: TWeatherConditions;
  Alr: TWeatherAlerts;
  Fos: TWeatherForecast;
  Foh: TWeatherForecast;
  Fod: TWeatherForecast;
  Map: IWeatherMaps;
  E: TWUEndpoints;
begin
  R:= TWeatherMultiInfo.Create;
  try
    Con:= TWeatherConditions.Create;
    Alr:= TWeatherAlerts.Create;
    Fos:= TWeatherForecast.Create;
    Foh:= TWeatherForecast.Create;
    Fod:= TWeatherForecast.Create;

    E:= [];
    if wiConditions in Info then begin
      E:= E + [weConditions];
    end;
    if wiAlerts in Info then begin
      E:= E + [weAlerts];
    end;
    if wiForecastSummary in Info then begin
      E:= E + [weForecast];
    end;
    if wiForecastHourly in Info then begin
      E:= E + [weForecast10Day];
    end;
    if wiForecastDaily in Info then begin
      E:= E + [weHourly];
    end;

    O:= GetMultiEndpoints(E, '.json');

    if wiConditions in Info then begin
      FillConditions(O, Con);
    end;
    if wiAlerts in Info then begin
      FillAlerts(O, Alr);
    end;
    if wiForecastSummary in Info then begin
      FillForecastSummary(O, Fos);
    end;
    if wiForecastHourly in Info then begin
      FillForecastHourly(O, Foh);
    end;
    if wiForecastDaily in Info then begin
      FillForecastDaily(O, Fod);
    end;

    if wiMaps in Info then begin
      Map:= GetMaps;
    end else begin
      Map:= TWeatherMaps.Create;
    end;

    R.SetAll(Con, Alr, Fos, Foh, Fod, Map);

  finally
    Result:= R;
  end;
end;

function TWUService.GetConditions: IWeatherConditions;
var
  O: ISuperObject;
  R: TWeatherConditions;
begin
  R:= TWeatherConditions.Create;
  try
    O:= GetEndpoint(TWUEndpoint.weConditions);
    FillConditions(O, R);
  finally
    Result:= R;
  end;
end;

function TWUService.GetAlerts: IWeatherAlerts;
var
  O: ISuperObject;
  R: TWeatherAlerts;
begin
  R:= TWeatherAlerts.Create;
  try
    O:= GetEndpoint(TWUEndpoint.weAlerts);
    FillAlerts(O, R);
  finally
    Result:= R;
  end;
end;

function TWUService.GetForecastDaily: IWeatherForecast;
var
  O: ISuperObject;
  R: TWeatherForecast;
begin
  R:= TWeatherForecast.Create;
  try
    O:= GetEndpoint(TWUEndpoint.weForecast10Day);
    FillForecastDaily(O, R);
  finally
    Result:= R;
  end;
end;

function TWUService.GetForecastHourly: IWeatherForecast;
var
  O: ISuperObject;
  R: TWeatherForecast;
begin
  R:= TWeatherForecast.Create;
  try
    O:= GetEndpoint(TWUEndpoint.weHourly);
    FillForecastHourly(O, R);
  finally
    Result:= R;
  end;
end;

function TWUService.GetForecastSummary: IWeatherForecast;
var
  O: ISuperObject;
  R: TWeatherForecast;
begin
  R:= TWeatherForecast.Create;
  try
    O:= GetEndpoint(TWUEndpoint.weForecast);
    FillForecastSummary(O, R);
  finally
    Result:= R;
  end;
end;

function MapOptions(
  const Width: Integer = 400;
  const Height: Integer = 400;
  const NumFrames: Integer = 10;
  const Delay: Integer = 50;
  const FrameIndex: Integer = 0;
  const NoClutter: Boolean = True;
  const SmoothColors: Boolean = True;
  const RainSnow: Boolean = True;
  const TimeLabel: Boolean = True;
  const TimeLabelX: Integer = 10;
  const TimeLabelY: Integer = 20): String;
begin
  Result:= 'newmaps=1';
  Result:= Result + '&width='+IntToStr(Width);
  Result:= Result + '&height='+IntToStr(Height);
  Result:= Result + '&delay='+IntToStr(Delay);
  Result:= Result + '&num='+IntToStr(NumFrames);
  Result:= Result + '&frame='+IntToStr(FrameIndex);
  Result:= Result + '&noclutter='+IfThen(NoClutter, '1','0');
  Result:= Result + '&smooth='+IfThen(SmoothColors, '1','0');
  Result:= Result + '&rainsnow='+IfThen(RainSnow, '1','0');
  if TimeLabel then begin
    Result:= Result + '&timelabel=1';
    Result:= Result + '&timelabel.x='+IntToStr(TimeLabelX);
    Result:= Result + '&timelabel.y='+IntToStr(TimeLabelY);
  end;
end;

function TWUService.GetMaps: IWeatherMaps;
var
  {$IFDEF USE_VCL}
  I: TGifImage;
  {$ENDIF}
  R: TWeatherMaps;
  S: TStringStream;
  U: String;
  procedure GetMap(const EP: TWUEndpoint; const MT: TWeatherMapType);
  begin
    S.Clear;
    S.Position:= 0;
    U:= GetEndpointUrl(EP);
    U:= U + '?'+MapOptions(700, 700, 15, 60);
    Web.Get(U, S);
    S.Position:= 0;
    {$IFDEF USE_VCL}
    I:= TGifImage.Create;
    try
      I.LoadFromStream(S);
      Maps.Maps[MT].Assign(I);
    finally
      I.Free;
    end;
    {$ELSE}
    Result.Maps[MT].Base64:= S.DataString;
    {$ENDIF}
  end;
begin
  R:= TWeatherMaps.Create;
  try
    S:= TStringStream.Create;
    try
      //TODO...

    finally
      FreeAndNil(S);
    end;
  finally
    Result:= R;
  end;
end;

procedure TWUService.FillConditions(const O: ISuperObject; Conditions: TWeatherConditions);
var
  OB, L: ISuperObject;
  function LD(const O: ISuperObject; const N: String): Double;
  var
    T: String;
  begin
    T:= O.S[N];
    Result:= StrToFloatDef(T, 0);
  end;
begin
  OB:= O.O['current_observation'];
  if not Assigned(OB) then Exit;

  L:= OB.O['display_location'];
  if not Assigned(L) then Exit;

  Conditions.FLocation.FDisplayName:= L.S['full'];
  Conditions.FLocation.FCity:= L.S['city'];
  Conditions.FLocation.FState:= L.S['state_name'];
  Conditions.FLocation.FStateAbbr:= L.S['state'];
  Conditions.FLocation.FCountry:= L.S['country'];
  Conditions.FLocation.FCountryAbbr:= L.S['country_iso3166'];
  Conditions.FLocation.FLongitude:= LD(L, 'longitude');
  Conditions.FLocation.FLatitude:= LD(L, 'latitude');
  Conditions.FLocation.FElevation:= LD(L, 'elevation');
  Conditions.FLocation.FZipCode:= L.S['zip'];

  case Units of
    wuKelvin: begin
      //Not Supported
    end;
    wuImperial: begin
      Conditions.FTemp:= OB.D['temp_f'];
      Conditions.FVisibility:= LD(OB, 'visibility_mi');
      Conditions.FDewPoint:= OB.D['dewpoint_f'];
    end;
    wuMetric: begin
      Conditions.FTemp:= OB.D['temp_c'];
      Conditions.FVisibility:= LD(OB, 'visibility_km');
      Conditions.FDewPoint:= OB.D['dewpoint_c'];
    end;
  end;

  Conditions.FDateTime:= Now; //TODO
  Conditions.FHumidity:= LD(OB, 'relative_humidity');
  Conditions.FPressure:= LD(OB, 'pressure_mb');
  Conditions.FCondition:= OB.S['weather'];
  Conditions.FDescription:= OB.S['weather'];
  Conditions.FWindSpeed:= OB.D['wind_mph'];
  Conditions.FWindDir:= OB.D['wind_degrees'];

  {$IFDEF USE_VCL}
  //LoadPicture(OB.S['icon_url'], Conditions.FPicture);
  {$ELSE}
  {$ENDIF}

end;

function TWUService.StrToAlertType(const S: String): TWeatherAlertType;
  procedure Chk(const Val: String; const T: TWeatherAlertType);
  begin
    if SameText(S, Val) then begin
      Result:= T;
    end;
  end;
begin
  Chk('', TWeatherAlertType.waNone);
  Chk('HUR', TWeatherAlertType.waHurricaneStat);
  Chk('TOR', TWeatherAlertType.waTornadoWarn);
  Chk('TOW', TWeatherAlertType.waTornadoWatch);
  Chk('WRN', TWeatherAlertType.waSevThundWarn);
  Chk('SEW', TWeatherAlertType.waSevThundWatch);
  Chk('WIN', TWeatherAlertType.waWinterAdv);
  Chk('FLO', TWeatherAlertType.waFloodWarn);
  Chk('WAT', TWeatherAlertType.waFloodWatch);
  Chk('WND', TWeatherAlertType.waHighWind);
  Chk('SVR', TWeatherAlertType.waSevStat);
  Chk('HEA', TWeatherAlertType.waHeatAdv);
  Chk('FOG', TWeatherAlertType.waFogAdv);
  Chk('SPE', TWeatherAlertType.waSpecialStat);
  Chk('FIR', TWeatherAlertType.waFireAdv);
  Chk('VOL', TWeatherAlertType.waVolcanicStat);
  Chk('HWW', TWeatherAlertType.waHurricaneWarn);
  Chk('REC', TWeatherAlertType.waRecordSet);
  Chk('REP', TWeatherAlertType.waPublicRec);
  Chk('PUB', TWeatherAlertType.waPublicStat);
end;

procedure TWUService.FillAlerts(const O: ISuperObject;
  Alerts: TWeatherAlerts);
var
  A: TSuperArray;
  Tmp: Int64;
  I: TWeatherAlert;
  Obj: ISuperObject;
  X: Integer;
  function LD(const O: ISuperObject; const N: String): Double;
  var
    T: String;
  begin
    T:= O.S[N];
    Result:= StrToFloatDef(T, 0);
  end;
begin
  A:= O.A['alerts'];
  if not Assigned(A) then Exit;

  for X := 0 to A.Length-1 do begin
    Obj:= A.O[X];
    I:= TWeatherAlert.Create;
    try
      I.FAlertType:= StrToAlertType(Obj.S['type']);
      I.FDescription:= Obj.S['description'];
      Tmp:= StrToIntDef(Obj.S['date_epoch'], 0);
      I.FDateTime:= EpochLocal(Tmp);
      Tmp:= StrToIntDef(Obj.S['expires_epoch'], 0);
      I.FExpires:= EpochLocal(Tmp);
      I.FMsg:= Obj.S['message'];
      I.FPhenomena:= Obj.S['phenomena'];
      I.FSignificance:= Obj.S['significance'];
      //Zones
      //Storm
    finally
      Alerts.FItems.Add(I);
    end;
  end;
end;

procedure TWUService.FillForecastSummary(const O: ISuperObject; Forecast: TWeatherForecast);
var
  OB: ISuperObject;
  A: TSuperArray;
  I: TWeatherForecastItem;
  X: Integer;
  function LD(const O: ISuperObject; const N: String): Double;
  var
    T: String;
  begin
    T:= O.S[N];
    Result:= StrToFloatDef(T, 0);
  end;
begin
  A:= O.A['hourly_forecast'];
  if not Assigned(A) then begin
    Exit;
  end;

  for X := 0 to A.Length-1 do begin
    OB:= A.O[X];
    I:= TWeatherForecastItem.Create(Forecast);
    try
      case Units of
        wuKelvin: begin
          //TODO: NOT SUPPORTED - Calculate?
          I.FTempMin:= StrToFloatDef(OB.O['temp'].S['metric'], 0);
          I.FTempMax:= StrToFloatDef(OB.O['temp'].S['metric'], 0);
          I.FDewPoint:= StrToFloatDef(OB.O['dewpoint'].S['metric'], 0);

          I.FWindSpeed:= OB.O['wspd'].D['metric'];
        end;
        wuImperial: begin
          I.FTempMin:= StrToFloatDef(OB.O['temp'].S['english'], 0);
          I.FTempMax:= StrToFloatDef(OB.O['temp'].S['english'], 0);
          I.FWindSpeed:= StrToFloatDef(OB.O['wspd'].S['english'], 0);
          I.FDewPoint:= StrToFloatDef(OB.O['dewpoint'].S['english'], 0);
        end;
        wuMetric: begin
          I.FTempMin:= StrToFloatDef(OB.O['temp'].S['metric'], 0);
          I.FTempMax:= StrToFloatDef(OB.O['temp'].S['metric'], 0);
          I.FWindSpeed:= StrToFloatDef(OB.O['wspd'].S['metric'], 0);
          I.FDewPoint:= StrToFloatDef(OB.O['dewpoint'].S['metric'], 0);
        end;
      end;
      I.FDateTime:=  EpochLocal(StrToIntDef(OB.O['FCTTIME'].S['epoch'], 0));
      I.FTemp:= I.FTempMax; //TODO
      I.FHumidity:= StrToFloatDef(OB.S['humidity'], 0);
      I.FPressure:= 0;
      I.FCondition:= OB.S['condition'];
      I.FDescription:= OB.S['condition'];
      I.FWindDir:= StrToFloatDef(OB.O['wdir'].S['degrees'], 0);
      I.FVisibility:= 0;
      {$IFDEF USE_VCL}
      LoadPicture(OB.S['icon_url'], I.FPicture);
      {$ENDIF}
    finally
      Forecast.FItems.Add(I);
    end;
  end;
end;

procedure TWUService.FillForecastDaily(const O: ISuperObject;
  Forecast: TWeatherForecast);
var
  OB: ISuperObject;
  A: TSuperArray;
  I: TWeatherForecastItem;
  X: Integer;
  function LD(const O: ISuperObject; const N: String): Double;
  var
    T: String;
  begin
    T:= O.S[N];
    Result:= StrToFloatDef(T, 0);
  end;
begin
  OB:= O.O['forecast'];
  if not Assigned(OB) then begin
    Exit;
  end;

  OB:= OB.O['simpleforecast'];
  if not Assigned(OB) then begin
    Exit;
  end;

  A:= OB.A['forecastday'];
  if not Assigned(A) then begin
    Exit;
  end;

  for X := 0 to A.Length-1 do begin
    OB:= A.O[X];
    I:= TWeatherForecastItem.Create(Forecast);
    try
      case Units of
        wuKelvin: begin
          //TODO: NOT SUPPORTED - Calculate?
          I.FTempMin:= StrToFloatDef(OB.O['low'].S['celsius'], 0);
          I.FTempMax:= StrToFloatDef(OB.O['high'].S['celsius'], 0);
          I.FWindSpeed:= OB.O['avewind'].D['kph'];
          I.FDewPoint:= 0; //Not Supported
        end;
        wuImperial: begin
          I.FTempMin:= StrToFloatDef(OB.O['low'].S['ferenheit'], 0);
          I.FTempMax:= StrToFloatDef(OB.O['high'].S['ferenheit'], 0);
          I.FWindSpeed:= StrToFloatDef(OB.O['avewind'].S['mph'], 0);
          I.FDewPoint:= 0; //Not Supported
        end;
        wuMetric: begin
          I.FTempMin:= StrToFloatDef(OB.O['low'].S['celsius'], 0);
          I.FTempMax:= StrToFloatDef(OB.O['high'].S['celsius'], 0);
          I.FWindSpeed:= StrToFloatDef(OB.O['avewind'].S['kph'], 0);
          I.FDewPoint:= 0; //Not Supported
        end;
      end;
      I.FDateTime:=  EpochLocal(StrToIntDef(OB.O['date'].S['epoch'], 0));
      I.FTemp:= I.FTempMax; //TODO
      I.FHumidity:= StrToFloatDef(OB.S['avehumidity'], 0);
      I.FPressure:= 0;
      I.FCondition:= OB.S['conditions'];
      I.FDescription:= OB.S['conditions'];
      I.FWindDir:= StrToFloatDef(OB.O['avewind'].S['degrees'], 0);
      I.FVisibility:= 0; //Not Supported
      {$IFDEF USE_VCL}
      LoadPicture(OB.S['icon_url'], I.FPicture);
      {$ENDIF}
    finally
      Forecast.FItems.Add(I);
    end;
  end;
end;

procedure TWUService.FillForecastHourly(const O: ISuperObject;
  Forecast: TWeatherForecast);
var
  OB: ISuperObject;
  A: TSuperArray;
  I: TWeatherForecastItem;
  X: Integer;
  function LD(const O: ISuperObject; const N: String): Double;
  var
    T: String;
  begin
    T:= O.S[N];
    Result:= StrToFloatDef(T, 0);
  end;
begin
  A:= O.A['hourly_forecast'];
  if not Assigned(A) then begin
    Exit;
  end;

  for X := 0 to A.Length-1 do begin
    OB:= A.O[X];
    I:= TWeatherForecastItem.Create(Forecast);
    try
      case Units of
        wuKelvin: begin
          //TODO: NOT SUPPORTED - Calculate?
          I.FTempMin:= StrToFloatDef(OB.O['temp'].S['metric'], 0);
          I.FTempMax:= StrToFloatDef(OB.O['temp'].S['metric'], 0);
          I.FDewPoint:= StrToFloatDef(OB.O['dewpoint'].S['metric'], 0);

          I.FWindSpeed:= OB.O['wspd'].D['metric'];
        end;
        wuImperial: begin
          I.FTempMin:= StrToFloatDef(OB.O['temp'].S['english'], 0);
          I.FTempMax:= StrToFloatDef(OB.O['temp'].S['english'], 0);
          I.FWindSpeed:= StrToFloatDef(OB.O['wspd'].S['english'], 0);
          I.FDewPoint:= StrToFloatDef(OB.O['dewpoint'].S['english'], 0);
        end;
        wuMetric: begin
          I.FTempMin:= StrToFloatDef(OB.O['temp'].S['metric'], 0);
          I.FTempMax:= StrToFloatDef(OB.O['temp'].S['metric'], 0);
          I.FWindSpeed:= StrToFloatDef(OB.O['wspd'].S['metric'], 0);
          I.FDewPoint:= StrToFloatDef(OB.O['dewpoint'].S['metric'], 0);
        end;
      end;
      I.FDateTime:=  EpochLocal(StrToIntDef(OB.O['FCTTIME'].S['epoch'], 0));
      I.FTemp:= I.FTempMax; //TODO
      I.FHumidity:= StrToFloatDef(OB.S['humidity'], 0);
      I.FPressure:= 0;
      I.FCondition:= OB.S['condition'];
      I.FDescription:= OB.S['condition'];
      I.FWindDir:= StrToFloatDef(OB.O['wdir'].S['degrees'], 0);
      I.FVisibility:= 0;
      {$IFDEF USE_VCL}
      LoadPicture(OB.S['icon_url'], I.FPicture);
      {$ENDIF}
    finally
      Forecast.FItems.Add(I);
    end;
  end;
end;

















function CreateWeatherService: IWeatherService; stdcall;
var
  R: TWUService;
begin
  R:= TWUService.Create;
  try



  finally
    Result:= R;
  end;
end;

exports
  CreateWeatherService;

begin
end.
