unit JD.Weather.Services.WUnderground;

interface

{$R 'WUndergroundRes.res' 'WUndergroundRes.rc'}

uses
  System.SysUtils,
  System.Classes,
  StrUtils,
  JD.Weather.Intf,
  JD.Weather.SuperObject,
  System.Generics.Collections;

const
  SVC_CAPTION = 'Weather Underground';
  SVC_NAME = 'WUnderground';
  SVC_UID = '{87940B1A-0435-4E2D-8DE8-6DE3BDCD43BB}';

  URL_MAIN = 'http://www.wunderground.com';
  URL_API = 'https://www.wunderground.com/weather/api/';
  URL_REGISTER = 'https://www.wunderground.com/member/registration';
  URL_LOGIN = 'https://www.wunderground.com/login.asp';
  URL_LEGAL = 'https://www.wunderground.com/weather/api/d/terms.html';

  SUP_INFO = [wiLocation, wiConditions, wiAlerts, wiForecastSummary,
    wiForecastHourly, wiForecastDaily, wiMaps];
  SUP_UNITS = [wuImperial, wuMetric];
  SUP_LOC = [wlZip, wlCityState, wlCoords, wlAutoIP,
    wlCountryCity, wlAirportCode, wlPWS];
  SUP_LOGO = [ltColor, ltColorInvert, ltColorWide, ltColorInvertWide,
    ltColorLeft, ltColorRight];
  SUP_COND_PROP = [wpIcon, wpCaption, wpURL, wpStation, wpTemp,
    wpFeelsLike, wpWindDir, wpWindSpeed, wpWindGust, wpHeatIndex, wpPressure,
    wpHumidity, wpDewPoint, wpVisibility, wpSolarRad, wpUVIndex, wpPrecipAmt];
  SUP_ALERT_TYPE = [waNone, waHurricaneStat, waTornadoWarn,
    waTornadoWatch, waSevThundWarn, waSevThundWatch, waWinterAdv,
    waFloodWarn, waFloodWatch, waHighWind, waSevStat, waHeatAdv, waFogAdv,
    waSpecialStat, waFireAdv, waVolcanicStat, waHurricaneWarn,
    waRecordSet, waPublicRec, waPublicStat];
  SUP_ALERT_PROP = [apZones, apVerticies, apStorm, apType,
    apDescription, apExpires, apMessage, apPhenomena, apSignificance];
  SUP_FOR = [ftSummary, ftHourly, ftDaily];
  SUP_FOR_SUM = [wpCaption, wpDescription, wpIcon, wpPrecipPred];
  SUP_FOR_HOUR = [wpIcon,
    wpCaption,
    wpDescription,
    wpTemp,
    wpFeelsLike,
    wpWindDir,
    wpWindSpeed,
    wpWindChill,
    wpHeatIndex,
    wpPressure,
    wpHumidity,
    wpDewPoint,
    wpUVIndex,
    wpPrecipAmt,
    wpSnowAmt,
    wpPrecipPred];
  SUP_FOR_DAY = [wpIcon,
    wpCaption,
    wpDescription,
    wpTemp,
    wpTempMin,
    wpTempMax,
    wpWindDir,
    wpWindSpeed,
    wpHumidity,
    wpPrecipAmt,
    wpSnowAmt,
    wpPrecipPred];
  SUP_MAP = [mpSatellite, mpRadar, mpSatelliteRadar,
    mpAniSatellite, mpAniRadar, mpAniSatelliteRadar];
  SUP_MAP_FOR = [wfPng, wfGif, wfFlash];

type
  TWUEndpoint = (weAll, weAlerts, weAlmanac, weAstronomy, weConditions,
    weCurrentHurricane, weForecast, weForecast10Day, weGeoLookup, weHistory,
    weHourly, weHourly10Day, wePlanner, weRawTide, weTide, weWebCams, weYesterday,
    weRadar, weSatellite, weRadarSatellite,
    weAniRadar, weAniSatellite, weAniRadarSatellite);
  TWUEndpoints = set of TWUEndpoint;

////////////////////////////////////////////////////////////////////////////////

  TWeatherSupport = class(TInterfacedObject, IWeatherSupport)
  public
    function GetSupportedLogos: TWeatherLogoTypes;
    function GetSupportedUnits: TWeatherUnitsSet;
    function GetSupportedInfo: TWeatherInfoTypes;
    function GetSupportedLocations: TWeatherLocationTypes;
    function GetSupportedAlerts: TWeatherAlertTypes;
    function GetSupportedAlertProps: TWeatherAlertProps;
    function GetSupportedConditionProps: TWeatherPropTypes;
    function GetSupportedForecasts: TWeatherForecastTypes;
    function GetSupportedForecastSummaryProps: TWeatherPropTypes;
    function GetSupportedForecastHourlyProps: TWeatherPropTypes;
    function GetSupportedForecastDailyProps: TWeatherPropTypes;
    function GetSupportedMaps: TWeatherMapTypes;
    function GetSupportedMapFormats: TWeatherMapFormats;

    property SupportedLogos: TWeatherLogoTypes read GetSupportedLogos;
    property SupportedUnits: TWeatherUnitsSet read GetSupportedUnits;
    property SupportedInfo: TWeatherInfoTypes read GetSupportedInfo;
    property SupportedLocations: TWeatherLocationTypes read GetSupportedLocations;
    property SupportedAlerts: TWeatherAlertTypes read GetSupportedAlerts;
    property SupportedAlertProps: TWeatherAlertProps read GetSupportedAlertProps;
    property SupportedConditionProps: TWeatherPropTypes read GetSupportedConditionProps;
    property SupportedForecasts: TWeatherForecastTypes read GetSupportedForecasts;
    property SupportedForecastSummaryProps: TWeatherPropTypes read GetSupportedForecastSummaryProps;
    property SupportedForecastHourlyProps: TWeatherPropTypes read GetSupportedForecastHourlyProps;
    property SupportedForecastDailyProps: TWeatherPropTypes read GetSupportedForecastDailyProps;
    property SupportedMaps: TWeatherMapTypes read GetSupportedMaps;
    property SupportedMapFormats: TWeatherMapFormats read GetSupportedMapFormats;
  end;

  TWeatherURLs = class(TInterfacedObject, IWeatherURLs)
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

  TWeatherServiceInfo = class(TInterfacedObject, IWeatherServiceInfo)
  private
    FSupport: TWeatherSupport;
    FURLs: IWeatherURLs;
    FLogos: TLogoArray;
    procedure SetLogo(const LT: TWeatherLogoType; const Value: IWeatherGraphic);
    procedure LoadLogos;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetCaption: WideString;
    function GetName: WideString;
    function GetUID: WideString;
    function GetURLs: IWeatherURLs;
    function GetSupport: IWeatherSupport;
    function GetLogo(const LT: TWeatherLogoType): IWeatherGraphic;

    property Logos[const LT: TWeatherLogoType]: IWeatherGraphic read GetLogo write SetLogo;

    property Caption: WideString read GetCaption;
    property Name: WideString read GetName;
    property UID: WideString read GetUID;
    property Support: IWeatherSupport read GetSupport;
    property URLs: IWeatherURLs read GetURLs;
  end;

////////////////////////////////////////////////////////////////////////////////

  TWeatherService = class(TWeatherServiceBase, IWeatherService)
  private
    FInfo: TWeatherServiceInfo;
    FLocation: TWeatherLocation;
    function GetEndpointUrl(const Endpoint: TWUEndpoint): String;
    function GetMultiEndpointUrl(const Endpoints: TWUEndpoints; const Ext: String): String;
    function GetEndpoint(const Endpoint: TWUEndpoint): ISuperObject;
    function GetMultiEndpoints(const Endpoints: TWUEndpoints; const Ext: String): ISuperObject;
    function StrToAlertType(const S: String): TWeatherAlertType;
    procedure FillConditions(const O: ISuperObject;
      Conditions: TWeatherProps);
    procedure FillAlerts(const O: ISuperObject; Alerts: TWeatherAlerts);
    procedure FillForecastDaily(const O: ISuperObject;
      Forecast: TWeatherForecast);
    procedure FillForecastHourly(const O: ISuperObject;
      Forecast: TWeatherForecast);
    procedure FillForecastSummary(const O: ISuperObject;
      Forecast: TWeatherForecast);
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    function GetInfo: IWeatherServiceInfo;

    function GetMultiple(const Info: TWeatherInfoTypes): IWeatherMultiInfo;
    function GetLocation: IWeatherLocation;
    function GetConditions: IWeatherProps;
    function GetAlerts: IWeatherAlerts;
    function GetForecastSummary: IWeatherForecast;
    function GetForecastHourly: IWeatherForecast;
    function GetForecastDaily: IWeatherForecast;
    function GetMaps: IWeatherMaps;

    property Info: IWeatherServiceInfo read GetInfo;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////

{ TWeatherSupport }

function TWeatherSupport.GetSupportedUnits: TWeatherUnitsSet;
begin
  Result:= SUP_UNITS;
end;

function TWeatherSupport.GetSupportedLocations: TWeatherLocationTypes;
begin
  Result:= SUP_LOC;
end;

function TWeatherSupport.GetSupportedLogos: TWeatherLogoTypes;
begin
  Result:= SUP_LOGO;
end;

function TWeatherSupport.GetSupportedAlertProps: TWeatherAlertProps;
begin
  Result:= SUP_ALERT_PROP;
end;

function TWeatherSupport.GetSupportedAlerts: TWeatherAlertTypes;
begin
  Result:= SUP_ALERT_TYPE;
end;

function TWeatherSupport.GetSupportedConditionProps: TWeatherPropTypes;
begin
  Result:= SUP_COND_PROP;
end;

function TWeatherSupport.GetSupportedForecasts: TWeatherForecastTypes;
begin
  Result:= SUP_FOR;
end;

function TWeatherSupport.GetSupportedForecastDailyProps: TWeatherPropTypes;
begin
  Result:= SUP_FOR_DAY;
end;

function TWeatherSupport.GetSupportedForecastHourlyProps: TWeatherPropTypes;
begin
  Result:= SUP_FOR_HOUR;
end;

function TWeatherSupport.GetSupportedForecastSummaryProps: TWeatherPropTypes;
begin
  Result:= SUP_FOR_SUM;
end;

function TWeatherSupport.GetSupportedInfo: TWeatherInfoTypes;
begin
  Result:= SUP_INFO;
end;

function TWeatherSupport.GetSupportedMapFormats: TWeatherMapFormats;
begin
  Result:= SUP_MAP_FOR;
end;

function TWeatherSupport.GetSupportedMaps: TWeatherMapTypes;
begin
  Result:= SUP_MAP;
end;

{ TWeatherURLs }

function TWeatherURLs.GetApiURL: WideString;
begin
  Result:= URL_API;
end;

function TWeatherURLs.GetLegalURL: WideString;
begin
  Result:= URL_LEGAL;
end;

function TWeatherURLs.GetLoginURL: WideString;
begin
  Result:= URL_LOGIN;
end;

function TWeatherURLs.GetMainURL: WideString;
begin
  Result:= URL_MAIN;
end;

function TWeatherURLs.GetRegisterURL: WideString;
begin
  Result:= URL_REGISTER;
end;

{ TWeatherServiceInfo }

constructor TWeatherServiceInfo.Create;
var
  LT: TWeatherLogoType;
begin
  FSupport:= TWeatherSupport.Create;
  FSupport._AddRef;
  FURLs:= TWeatherURLs.Create;
  FURLs._AddRef;
  for LT:= Low(TWeatherLogoType) to High(TWeatherLogoType) do begin
    FLogos[LT]:= TWeatherGraphic.Create;
    FLogos[LT]._AddRef;
  end;
  LoadLogos;
end;

destructor TWeatherServiceInfo.Destroy;
var
  LT: TWeatherLogoType;
begin
  for LT:= Low(TWeatherLogoType) to High(TWeatherLogoType) do begin
    FLogos[LT]._Release;
  end;
  FURLs._Release;
  FURLs:= nil;
  FSupport._Release;
  FSupport:= nil;
  inherited;
end;

function TWeatherServiceInfo.GetCaption: WideString;
begin
  Result:= SVC_CAPTION;
end;

function TWeatherServiceInfo.GetName: WideString;
begin
  Result:= SVC_NAME;
end;

function TWeatherServiceInfo.GetSupport: IWeatherSupport;
begin
  Result:= FSupport;
end;

function TWeatherServiceInfo.GetUID: WideString;
begin
  Result:= SVC_UID;
end;

function TWeatherServiceInfo.GetURLs: IWeatherURLs;
begin
  Result:= FURLs;
end;

function TWeatherServiceInfo.GetLogo(const LT: TWeatherLogoType): IWeatherGraphic;
begin
  Result:= FLogos[LT];
end;

procedure TWeatherServiceInfo.SetLogo(const LT: TWeatherLogoType;
  const Value: IWeatherGraphic);
begin
  FLogos[LT].Base64:= Value.Base64;
end;

procedure TWeatherServiceInfo.LoadLogos;
  function Get(const N, T: String): IWeatherGraphic;
  var
    S: TResourceStream;
    R: TStringStream;
  begin
    Result:= TWeatherGraphic.Create;
    if ResourceExists(N, T) then begin
      raise Exception.Create('TEST!');
      S:= TResourceStream.Create(HInstance, N, PChar(T));
      try
        R:= TStringStream.Create;
        try
          S.Position:= 0;
          R.LoadFromStream(S);
          R.Position:= 0;
          Result.Base64:= R.DataString;
        finally
          FreeAndNil(R);
        end;
      finally
        FreeAndNil(S);
      end;
    end;
  end;
begin
  SetLogo(ltColor, Get('LOGO_COLOR', 'JPG'));
  SetLogo(ltColorInvert, Get('LOGO_COLOR_INVERT', 'JPG'));
  SetLogo(ltColorWide, Get('LOGO_COLOR_WIDE', 'JPG'));
  SetLogo(ltColorInvertWide, Get('LOGO_COLOR_INVERT_WIDE', 'JPG'));
  SetLogo(ltColorLeft, Get('LOGO_COLOR_LEFT', 'JPG'));
  SetLogo(ltColorRight, Get('LOGO_COLOR_RIGHT', 'JPG'));
end;

////////////////////////////////////////////////////////////////////////////////

{ TWeatherService }

constructor TWeatherService.Create;
begin
  inherited;
  FInfo:= TWeatherServiceInfo.Create;
  FInfo._AddRef;
end;

destructor TWeatherService.Destroy;
begin
  FInfo._Release;
  FInfo:= nil;
  inherited;
end;

function TWeatherService.GetEndpointUrl(const Endpoint: TWUEndpoint): String;
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

function TWeatherService.GetMultiEndpointUrl(const Endpoints: TWUEndpoints; const Ext: String): String;
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

function TWeatherService.GetEndpoint(const Endpoint: TWUEndpoint): ISuperObject;
var
  U: String;
  S: String;
begin
  U:= GetEndpointUrl(Endpoint);
  S:= Web.Get(U);
  Result:= SO(S);
end;

function TWeatherService.GetMultiEndpoints(const Endpoints: TWUEndpoints; const Ext: String): ISuperObject;
var
  U: String;
  S: String;
begin
  U:= GetMultiEndpointUrl(Endpoints, Ext);
  S:= Web.Get(U);
  Result:= SO(S);
end;

function TWeatherService.GetMultiple(const Info: TWeatherInfoTypes): IWeatherMultiInfo;
var
  O: ISuperObject;
  R: TWeatherMultiInfo;
  Con: TWeatherProps;
  Alr: TWeatherAlerts;
  Fos: TWeatherForecast;
  Foh: TWeatherForecast;
  Fod: TWeatherForecast;
  Map: IWeatherMaps;
  E: TWUEndpoints;
begin
  R:= TWeatherMultiInfo.Create;
  try
    Con:= TWeatherProps.Create;
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

function TWeatherService.GetConditions: IWeatherProps;
var
  O: ISuperObject;
  R: TWeatherProps;
begin
  R:= TWeatherProps.Create;
  try
    O:= GetEndpoint(TWUEndpoint.weConditions);
    FillConditions(O, R);
  finally
    Result:= R;
  end;
end;

function TWeatherService.GetAlerts: IWeatherAlerts;
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

function TWeatherService.GetForecastDaily: IWeatherForecast;
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

function TWeatherService.GetForecastHourly: IWeatherForecast;
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

function TWeatherService.GetForecastSummary: IWeatherForecast;
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

function TWeatherService.GetInfo: IWeatherServiceInfo;
begin
  Result:= FInfo;
end;

function TWeatherService.GetLocation: IWeatherLocation;
begin
  Result:= FLocation;
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

function TWeatherService.GetMaps: IWeatherMaps;
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

procedure TWeatherService.FillConditions(const O: ISuperObject; Conditions: TWeatherProps);
var
  OB, L: ISuperObject;
  function LD(const O: ISuperObject; const N: String): Double;
  var
    T: String;
    I: Integer;
  begin
    T:= O.S[N];
    for I := Length(T) downto 1 do begin
      if not CharInSet(T[I], ['0'..'9', '.', ',']) then
        Delete(T, I, 1);
    end;
    Result:= StrToFloatDef(T, 0);
  end;
begin
  OB:= O.O['current_observation'];
  if not Assigned(OB) then Exit;

  L:= OB.O['display_location'];
  if not Assigned(L) then Exit;

  if Assigned(FLocation) then
    IWeatherLocation(FLocation)._Release;
  FLocation:= TWeatherLocation.Create;

  FLocation.FDisplayName:= L.S['full'];
  FLocation.FCity:= L.S['city'];
  FLocation.FState:= L.S['state_name'];
  FLocation.FStateAbbr:= L.S['state'];
  FLocation.FCountry:= L.S['country'];
  FLocation.FCountryAbbr:= L.S['country_iso3166'];
  FLocation.FLongitude:= LD(L, 'longitude');
  FLocation.FLatitude:= LD(L, 'latitude');
  FLocation.FElevation:= LD(L, 'elevation');
  FLocation.FZipCode:= L.S['zip'];
  //TODO: Trigger event for location.....

  Conditions.FSupport:= Self.Info.Support.SupportedConditionProps;

  Conditions.FDateTime:= EpochLocal(StrToIntDef(OB.S['observation_epoch'], 0)); // Now; //TODO
  Conditions.FCaption:= OB.S['weather'];
  Conditions.FURL:= OB.S['ob_url'];
  Conditions.FStation:= OB.S['station_id'];
  Conditions.FHumidity:= LD(OB, 'relative_humidity');
  Conditions.FWindDir:= OB.D['wind_degrees'];
  Conditions.FSolarRad:= LD(OB, 'solarradiation');
  Conditions.FUVIndex:= LD(OB, 'UV');

  case Units of
    wuKelvin: begin
      //Not Supported
      //TODO: Convert so that it can be supported
    end;
    wuImperial: begin
      Conditions.FTemp:= OB.D['temp_f'];
      Conditions.FVisibility:= LD(OB, 'visibility_mi');
      Conditions.FDewPoint:= OB.D['dewpoint_f'];
      Conditions.FPressure:= LD(OB, 'pressure_in');
      Conditions.FWindSpeed:= LD(OB, 'wind_mph');
      Conditions.FWindGusts:= LD(OB, 'wind_gust_mph');
      Conditions.FWindChill:= LD(OB, 'windchill_f');
      Conditions.FFeelsLike:= LD(OB, 'feelslike_f');
      Conditions.FHeatIndex:= LD(OB, 'heat_index_f');
      Conditions.FPrecipAmt:= LD(OB, 'precip_today_in');
    end;
    wuMetric: begin
      Conditions.FTemp:= OB.D['temp_c'];
      Conditions.FVisibility:= LD(OB, 'visibility_km');
      Conditions.FDewPoint:= OB.D['dewpoint_c'];
      Conditions.FPressure:= LD(OB, 'pressure_mb');
      Conditions.FWindSpeed:= LD(OB, 'wind_kph');
      Conditions.FWindGusts:= LD(OB, 'wind_gust_kph');
      Conditions.FWindChill:= LD(OB, 'windchill_c');
      Conditions.FFeelsLike:= LD(OB, 'feelslike_c');
      Conditions.FHeatIndex:= LD(OB, 'heat_index_c');
      Conditions.FPrecipAmt:= LD(OB, 'precip_today_metric');
    end;
  end;

  //LoadPicture(OB.S['icon_url'], Conditions.FPicture);

end;

function TWeatherService.StrToAlertType(const S: String): TWeatherAlertType;
  procedure Chk(const Val: String; const T: TWeatherAlertType);
  begin
    if SameText(S, Val) then begin
      Result:= T;
    end;
  end;
begin
  Chk('',    TWeatherAlertType.waNone);
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

procedure TWeatherService.FillAlerts(const O: ISuperObject; Alerts: TWeatherAlerts);
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

procedure TWeatherService.FillForecastSummary(const O: ISuperObject; Forecast: TWeatherForecast);
var
  OB: ISuperObject;
  A: TSuperArray;
  I: TWeatherProps;
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
  if not Assigned(OB) then Exit;

  OB:= OB.O['txt_forecast'];
  if not Assigned(OB) then Exit;

  A:= OB.A['forecastday'];
  if not Assigned(A) then begin
    Exit;
  end;

  for X := 0 to A.Length-1 do begin
    OB:= A.O[X];
    I:= TWeatherProps.Create;
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
      I.FCaption:= OB.S['condition'];
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

procedure TWeatherService.FillForecastDaily(const O: ISuperObject; Forecast: TWeatherForecast);
var
  OB: ISuperObject;
  A: TSuperArray;
  I: TWeatherProps;
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
    I:= TWeatherProps.Create;
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
      I.FCaption:= OB.S['conditions'];
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

procedure TWeatherService.FillForecastHourly(const O: ISuperObject; Forecast: TWeatherForecast);
var
  OB: ISuperObject;
  A: TSuperArray;
  I: TWeatherProps;
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
    I:= TWeatherProps.Create;
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
      I.FCaption:= OB.S['condition'];
      I.FDescription:= OB.S['condition'];
      I.FWindDir:= StrToFloatDef(OB.O['wdir'].S['degrees'], 0);
      I.FVisibility:= 0;
      {$IFDEF USE_VCL}
      LoadPicture(OB.S['icon_url'], I.FPicture);
      {$ELSE}
      I.FIcon.SetBase64(''); //TODO
      {$ENDIF}
    finally
      Forecast.FItems.Add(I);
    end;
  end;
end;

end.
