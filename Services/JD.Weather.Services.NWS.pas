unit JD.Weather.Services.NWS;

interface

{.$R 'NWSRes.res' 'NWSRes.rc'} //TODO

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  IdHTTP,
  JD.Weather.Intf,
  JD.Weather.NDFD;

const
  SVC_CAPTION = 'National Weather Service';
  SVC_NAME = 'NWS';
  SVC_AUTHOR = 'JD Software Inc.';
  SVC_UID = '{F126FE89-EAE3-4F46-9FDC-FE5B4120924D}';

  SVC_MIN_PRICE = 0.0;
  SVC_MAX_PRICE = 0.0;
  SVC_HAS_TRIAL = True;
  SVC_HAS_PAID = False;
  SVC_IS_UNLIMITED = True;

  URL_MAIN = 'http://www.weather.gov';
  URL_API = 'http://graphical.weather.gov/xml/rest.php';
  URL_REGISTER = '';
  URL_LOGIN = '';
  URL_LEGAL = '';
  URL_POWER = '';
  URL_USAGE = '';

  //Supported Pieces of Information
  SUP_INFO = [];
  SUP_LOC = [];
  SUP_LOGO = [];
  SUP_COND_PROP = [];
  SUP_ALERT_TYPE = [];
  SUP_ALERT_PROP = [];
  SUP_FOR = [];
  SUP_FOR_SUM = [];
  SUP_FOR_HOUR = [];
  SUP_FOR_DAY = [];
  SUP_UNITS = [];
  SUP_MAP = [];
  SUP_MAP_FOR = [];

type
  TNWSEndpoint = (oeForecastDaily, oeForecastHourly, oeHistory);

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
    function GetPowerURL: WideString;
    function GetUsageURL: WideString;

    property MainURL: WideString read GetMainURL;
    property ApiURL: WideString read GetApiURL;
    property LoginURL: WideString read GetLoginURL;
    property RegisterURL: WideString read GetRegisterURL;
    property LegalURL: WideString read GetLegalURL;
    property PowerURL: WideString read GetPowerURL;
    property UsageURL: WideString read GetUsageURL;
  end;

  TWeatherServiceInfo = class(TInterfacedObject, IWeatherServiceInfo)
  private
    FSupport: TWeatherSupport;
    FURLs: TWeatherURLs;
    FLogos: TLogoArray;
    procedure SetLogo(const LT: TWeatherLogoType; const Value: IWeatherGraphic);
    procedure LoadLogos;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetCaption: WideString;
    function GetName: WideString;
    function GetAuthor: WideString;
    function GetUID: WideString;
    function GetURLs: IWeatherURLs;
    function GetSupport: IWeatherSupport;
    function GetMinPrice: Double;
    function GetMaxPrice: Double;
    function GetHasTrial: Bool;
    function GetHasPaid: Bool;
    function GetIsUnlimited: Bool;

    function GetLogo(const LT: TWeatherLogoType): IWeatherGraphic;

    property Caption: WideString read GetCaption;
    property Name: WideString read GetName;
    property UID: WideString read GetUID;
    property Support: IWeatherSupport read GetSupport;
    property URLs: IWeatherURLs read GetURLs;
    property MinPrice: Double read GetMinPrice;
    property MaxPrice: Double read GetMaxPrice;
    property HasTrial: Bool read GetHasTrial;
    property HasPaid: Bool read GetHasPaid;
    property IsUnlimited: bool read GetIsUnlimited;
  end;

  TWeatherService = class(TWeatherServiceBase, IWeatherService)
  private
    FInfo: TWeatherServiceInfo;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetEndpointUrl(const Endpoint: TNWSEndpoint): String;
    function ParseDateTime(const S: String): TDateTime;
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

{ TWeatherSupport }

function TWeatherSupport.GetSupportedAlertProps: TWeatherAlertProps;
begin
  Result:= [{apZones, apVerticies, apStorm, apType, apDescription,
    apExpires, apMessage, apPhenomena, apSignificance}];
end;

function TWeatherSupport.GetSupportedAlerts: TWeatherAlertTypes;
begin
  Result:= [{waNone, waHurricaneStat, waTornadoWarn, waTornadoWatch, waSevThundWarn,
    waSevThundWatch, waWinterAdv, waFloodWarn, waFloodWatch, waHighWind, waSevStat,
    waHeatAdv, waFogAdv, waSpecialStat, waFireAdv, waVolcanicStat, waHurricaneWarn,
    waRecordSet, waPublicRec, waPublicStat}];
end;

function TWeatherSupport.GetSupportedConditionProps: TWeatherPropTypes;
begin
  Result:= [{cpPressureMB, cpPressureIn, cpWindDir, cpWindSpeed,
    cpHumidity, cpVisibility, cpDewPoint, cpHeatIndex, cpWindGust, cpWindChill,
    cpFeelsLike, cpSolarRad, cpUV, cpTemp, cpTempMin, cpTempMax, cpPrecip,
    cpIcon, cpCaption, cpDescription, cpStation, cpClouds,
    cpRain, cpSnow, cpSunrise, cpSunset}];
end;

function TWeatherSupport.GetSupportedForecastDailyProps: TWeatherPropTypes;
begin
  Result:= [{fpPressureMB, fpPressureIn, fpWindDir, fpWindSpeed,
    fpHumidity, fpVisibility, fpDewPoint, fpHeatIndex, fpWindGust, fpWindChill,
    fpFeelsLike, fpSolarRad, fpUV, fpTemp, fpTempMin, fpTempMax, fpCaption,
    fpDescription, fpIcon, fpGroundPressure, fpSeaPressure, fpPrecip, fpURL,
    fpDaylight, fpSnow, fpSleet, fpPrecipChance, fpClouds, fpRain, fpWetBulb,
    fpIce, fpCeiling}];
end;

function TWeatherSupport.GetSupportedForecastHourlyProps: TWeatherPropTypes;
begin
  Result:= [{fpPressureMB, fpPressureIn, fpWindDir, fpWindSpeed,
    fpHumidity, fpVisibility, fpDewPoint, fpHeatIndex, fpWindGust, fpWindChill,
    fpFeelsLike, fpSolarRad, fpUV, fpTemp, fpTempMin, fpTempMax, fpCaption,
    fpDescription, fpIcon, fpGroundPressure, fpSeaPressure, fpPrecip, fpURL,
    fpDaylight, fpSnow, fpSleet, fpPrecipChance, fpClouds, fpRain, fpWetBulb,
    fpIce, fpCeiling}];
end;

function TWeatherSupport.GetSupportedForecasts: TWeatherForecastTypes;
begin
  Result:= [{ftSummary, ftHourly, ftDaily}];
end;

function TWeatherSupport.GetSupportedForecastSummaryProps: TWeatherPropTypes;
begin
  Result:= [{fpPressureMB, fpPressureIn, fpWindDir, fpWindSpeed,
    fpHumidity, fpVisibility, fpDewPoint, fpHeatIndex, fpWindGust, fpWindChill,
    fpFeelsLike, fpSolarRad, fpUV, fpTemp, fpTempMin, fpTempMax, fpCaption,
    fpDescription, fpIcon, fpGroundPressure, fpSeaPressure, fpPrecip, fpURL,
    fpDaylight, fpSnow, fpSleet, fpPrecipChance, fpClouds, fpRain, fpWetBulb,
    fpIce, fpCeiling}];
end;

function TWeatherSupport.GetSupportedInfo: TWeatherInfoTypes;
begin
  Result:= [wiConditions, wiAlerts, wiForecastSummary,
    wiForecastHourly, wiForecastDaily{, wiMaps, wiAlmanac, wiAstronomy,
    wiHurricane, wiHistory, wiPlanner, wiStation}];
end;

function TWeatherSupport.GetSupportedLocations: TWeatherLocationTypes;
begin
  Result:= [{wlZip, wlCityState, wlCoords, wlAutoIP, wlCityCode,
    wlCountryCity, wlAirportCode, wlPWS}];
end;

function TWeatherSupport.GetSupportedLogos: TWeatherLogoTypes;
begin
  Result:= [{ltColor, ltColorInvert, ltColorWide, ltColorInvertWide,
    ltColorLeft, ltColorRight}];
end;

function TWeatherSupport.GetSupportedMapFormats: TWeatherMapFormats;
begin
  Result:= [{wfJpg, wfPng, wfGif, wfTiff, wfBmp, wfFlash, wfHtml}];
end;

function TWeatherSupport.GetSupportedMaps: TWeatherMapTypes;
begin
  Result:= [{mpSatellite, mpRadar, mpSatelliteRadar, mpRadarClouds,
    mpClouds, mpTemp, mpTempChange, mpSnowCover, mpPrecip, mpAlerts, mpHeatIndex,
    mpDewPoint, mpWindChill, mpPressureSea, mpWind,
    mpAniSatellite, mpAniRadar, mpAniSatelliteRadar}];
end;

function TWeatherSupport.GetSupportedUnits: TWeatherUnitsSet;
begin
  Result:= [wuImperial, wuMetric];
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

function TWeatherURLs.GetPowerURL: WideString;
begin
  Result:= URL_POWER;
end;

function TWeatherURLs.GetRegisterURL: WideString;
begin
  Result:= URL_REGISTER;
end;

function TWeatherURLs.GetUsageURL: WideString;
begin
  Result:= URL_USAGE;
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

function TWeatherServiceInfo.GetAuthor: WideString;
begin
  Result:= SVC_AUTHOR;
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

function TWeatherServiceInfo.GetMaxPrice: Double;
begin
  Result:= SVC_MAX_PRICE;
end;

function TWeatherServiceInfo.GetMinPrice: Double;
begin
  Result:= SVC_MIN_PRICE;
end;

function TWeatherServiceInfo.GetHasPaid: Bool;
begin
  Result:= SVC_HAS_PAID;
end;

function TWeatherServiceInfo.GetHasTrial: Bool;
begin
  Result:= SVC_HAS_TRIAL;
end;

function TWeatherServiceInfo.GetIsUnlimited: Bool;
begin
  Result:= SVC_IS_UNLIMITED;
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
    try
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
    except
      on E: Exception do begin

      end;
    end;
  end;
begin
  SetLogo(ltColor, Get('LOGO_COLOR', 'PNG'));
  SetLogo(ltColorInvert, Get('LOGO_COLOR_INVERT', 'PNG'));
  SetLogo(ltColorWide, Get('LOGO_COLOR_WIDE', 'PNG'));
  SetLogo(ltColorInvertWide, Get('LOGO_COLOR_INVERT_WIDE', 'PNG'));
  //SetLogo(ltColorLeft, Get('LOGO_COLOR_LEFT', 'PNG'));
  //SetLogo(ltColorRight, Get('LOGO_COLOR_RIGHT', 'PNG'));
end;

{ TWeatherService }

constructor TWeatherService.Create;
begin
  inherited;
  Web.HandleRedirects:= True;
  FInfo:= TWeatherServiceInfo.Create;
  FInfo._AddRef;
end;

destructor TWeatherService.Destroy;
begin
  FInfo._Release;
  FInfo:= nil;
  inherited;
end;

function TWeatherService.GetEndpointUrl(const Endpoint: TNWSEndpoint): String;
begin
  case Endpoint of
    oeForecastDaily: ;
    oeForecastHourly: ;
    oeHistory: ;
  end;
end;

function TWeatherService.GetInfo: IWeatherServiceInfo;
begin
  Result:= FInfo;
end;

function TWeatherService.GetMultiple(
  const Info: TWeatherInfoTypes): IWeatherMultiInfo;
begin
  //TODO
end;

function TWeatherService.GetConditions: IWeatherProps;
begin
  //TODO
end;

function TWeatherService.GetAlerts: IWeatherAlerts;
begin
  //TODO
end;

function TWeatherService.GetForecastDaily: IWeatherForecast;
begin
  //TODO
end;

function TWeatherService.GetForecastHourly: IWeatherForecast;
begin
  //TODO
end;

function TWeatherService.GetForecastSummary: IWeatherForecast;
begin
  //TODO
end;

function TWeatherService.GetLocation: IWeatherLocation;
begin
  //TODO
end;

function TWeatherService.GetMaps: IWeatherMaps;
begin
  //TODO
end;

function TWeatherService.ParseDateTime(const S: String): TDateTime;
begin
  Result:= 0; //TODO
end;

end.
