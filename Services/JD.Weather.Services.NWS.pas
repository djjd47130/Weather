unit JD.Weather.Services.NWS;

interface

{.$R 'NWSRes.res' 'NWSRes.rc'} //TODO

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  IdHTTP,
  JD.Weather.Intf,
  JD.Weather.NDFD;

const
  SVC_CAPTION = 'National Weather Service';
  SVC_UID = '{F126FE89-EAE3-4F46-9FDC-FE5B4120924D}';

type
  TNWSEndpoint = (oeForecastDaily, oeForecastHourly, oeHistory);

  TNWSWeatherSupport = class(TInterfacedObject, IWeatherSupport)
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

  TNWSWeatherURLs = class(TInterfacedObject, IWeatherURLs)
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

  TNWSService = class(TWeatherServiceBase, IWeatherService)
  private
    FSupport: TNWSWeatherSupport;
    FURLs: TNWSWeatherURLs;
    procedure LoadLogos;
    function GetEndpointUrl(const Endpoint: TNWSEndpoint): String;
    function ParseDateTime(const S: String): TDateTime;
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


implementation

{ TNWSWeatherSupport }

function TNWSWeatherSupport.GetSupportedAlertProps: TWeatherAlertProps;
begin
  Result:= [{apZones, apVerticies, apStorm, apType, apDescription,
    apExpires, apMessage, apPhenomena, apSignificance}];
end;

function TNWSWeatherSupport.GetSupportedAlerts: TWeatherAlertTypes;
begin
  Result:= [{waNone, waHurricaneStat, waTornadoWarn, waTornadoWatch, waSevThundWarn,
    waSevThundWatch, waWinterAdv, waFloodWarn, waFloodWatch, waHighWind, waSevStat,
    waHeatAdv, waFogAdv, waSpecialStat, waFireAdv, waVolcanicStat, waHurricaneWarn,
    waRecordSet, waPublicRec, waPublicStat}];
end;

function TNWSWeatherSupport.GetSupportedConditionProps: TWeatherConditionsProps;
begin
  Result:= [{cpPressureMB, cpPressureIn, cpWindDir, cpWindSpeed,
    cpHumidity, cpVisibility, cpDewPoint, cpHeatIndex, cpWindGust, cpWindChill,
    cpFeelsLike, cpSolarRad, cpUV, cpTemp, cpTempMin, cpTempMax, cpPrecip,
    cpIcon, cpCaption, cpDescription, cpStation, cpClouds,
    cpRain, cpSnow, cpSunrise, cpSunset}];
end;

function TNWSWeatherSupport.GetSupportedForecastDailyProps: TWeatherForecastProps;
begin
  Result:= [{fpPressureMB, fpPressureIn, fpWindDir, fpWindSpeed,
    fpHumidity, fpVisibility, fpDewPoint, fpHeatIndex, fpWindGust, fpWindChill,
    fpFeelsLike, fpSolarRad, fpUV, fpTemp, fpTempMin, fpTempMax, fpCaption,
    fpDescription, fpIcon, fpGroundPressure, fpSeaPressure, fpPrecip, fpURL,
    fpDaylight, fpSnow, fpSleet, fpPrecipChance, fpClouds, fpRain, fpWetBulb,
    fpIce, fpCeiling}];
end;

function TNWSWeatherSupport.GetSupportedForecastHourlyProps: TWeatherForecastProps;
begin
  Result:= [{fpPressureMB, fpPressureIn, fpWindDir, fpWindSpeed,
    fpHumidity, fpVisibility, fpDewPoint, fpHeatIndex, fpWindGust, fpWindChill,
    fpFeelsLike, fpSolarRad, fpUV, fpTemp, fpTempMin, fpTempMax, fpCaption,
    fpDescription, fpIcon, fpGroundPressure, fpSeaPressure, fpPrecip, fpURL,
    fpDaylight, fpSnow, fpSleet, fpPrecipChance, fpClouds, fpRain, fpWetBulb,
    fpIce, fpCeiling}];
end;

function TNWSWeatherSupport.GetSupportedForecasts: TWeatherForecastTypes;
begin
  Result:= [{ftSummary, ftHourly, ftDaily}];
end;

function TNWSWeatherSupport.GetSupportedForecastSummaryProps: TWeatherForecastProps;
begin
  Result:= [{fpPressureMB, fpPressureIn, fpWindDir, fpWindSpeed,
    fpHumidity, fpVisibility, fpDewPoint, fpHeatIndex, fpWindGust, fpWindChill,
    fpFeelsLike, fpSolarRad, fpUV, fpTemp, fpTempMin, fpTempMax, fpCaption,
    fpDescription, fpIcon, fpGroundPressure, fpSeaPressure, fpPrecip, fpURL,
    fpDaylight, fpSnow, fpSleet, fpPrecipChance, fpClouds, fpRain, fpWetBulb,
    fpIce, fpCeiling}];
end;

function TNWSWeatherSupport.GetSupportedInfo: TWeatherInfoTypes;
begin
  Result:= [wiConditions, wiAlerts, wiForecastSummary,
    wiForecastHourly, wiForecastDaily{, wiMaps, wiAlmanac, wiAstronomy,
    wiHurricane, wiHistory, wiPlanner, wiStation}];
end;

function TNWSWeatherSupport.GetSupportedLocations: TJDWeatherLocationTypes;
begin
  Result:= [{wlZip, wlCityState, wlCoords, wlAutoIP, wlCityCode,
    wlCountryCity, wlAirportCode, wlPWS}];
end;

function TNWSWeatherSupport.GetSupportedLogos: TWeatherLogoTypes;
begin
  Result:= [{ltColor, ltColorInvert, ltColorWide, ltColorInvertWide,
    ltColorLeft, ltColorRight}];
end;

function TNWSWeatherSupport.GetSupportedMapFormats: TWeatherMapFormats;
begin
  Result:= [{wfJpg, wfPng, wfGif, wfTiff, wfBmp, wfFlash, wfHtml}];
end;

function TNWSWeatherSupport.GetSupportedMaps: TWeatherMapTypes;
begin
  Result:= [{mpSatellite, mpRadar, mpSatelliteRadar, mpRadarClouds,
    mpClouds, mpTemp, mpTempChange, mpSnowCover, mpPrecip, mpAlerts, mpHeatIndex,
    mpDewPoint, mpWindChill, mpPressureSea, mpWind,
    mpAniSatellite, mpAniRadar, mpAniSatelliteRadar}];
end;

function TNWSWeatherSupport.GetSupportedUnits: TWeatherUnitsSet;
begin
  Result:= [wuImperial, wuMetric];
end;

{ TNWSWeatherURLs }

function TNWSWeatherURLs.GetApiURL: WideString;
begin
  Result:= 'http://graphical.weather.gov/xml/rest.php';
end;

function TNWSWeatherURLs.GetLegalURL: WideString;
begin
  Result:= '';
end;

function TNWSWeatherURLs.GetLoginURL: WideString;
begin
  Result:= '';
end;

function TNWSWeatherURLs.GetMainURL: WideString;
begin
  Result:= 'http://www.weather.gov';
end;

function TNWSWeatherURLs.GetRegisterURL: WideString;
begin
  Result:= '';
end;

{ TNWSService }

constructor TNWSService.Create;
begin
  inherited;
  Web.HandleRedirects:= True;
  FSupport:= TNWSWeatherSupport.Create;
  FSupport._AddRef;
  FURLs:= TNWSWeatherURLs.Create;
  FURLs._AddRef;
  LoadLogos;
end;

destructor TNWSService.Destroy;
begin
  FURLs._Release;
  FURLs:= nil;
  FSupport._Release;
  FSupport:= nil;
  inherited;
end;

function TNWSService.GetCaption: WideString;
begin
  Result:= SVC_CAPTION;
end;

function TNWSService.GetUID: WideString;
begin
  Result:= SVC_UID;
end;

function TNWSService.GetEndpointUrl(const Endpoint: TNWSEndpoint): String;
begin

end;

function TNWSService.GetMultiple(
  const Info: TWeatherInfoTypes): IWeatherMultiInfo;
begin

end;

function TNWSService.GetConditions: IWeatherConditions;
begin

end;

function TNWSService.GetAlerts: IWeatherAlerts;
begin

end;

function TNWSService.GetForecastDaily: IWeatherForecast;
begin

end;

function TNWSService.GetForecastHourly: IWeatherForecast;
begin

end;

function TNWSService.GetForecastSummary: IWeatherForecast;
begin

end;

function TNWSService.GetMaps: IWeatherMaps;
begin

end;

function TNWSService.GetURLs: IWeatherURLs;
begin
  Result:= FURLs;
end;

procedure TNWSService.LoadLogos;
  function Get(const N: String): IWeatherGraphic;
  var
    S: TResourceStream;
    R: TStringStream;
  begin
    Result:= TWeatherGraphic.Create;
    S:= TResourceStream.Create(HInstance, N, 'PNG');
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
begin
  try
    //TODO: Load Logos from Resources
    SetLogo(TWeatherLogoType.ltColor, Get('LOGO_COLOR'));
    SetLogo(TWeatherLogoType.ltColorWide, Get('LOGO_COLOR'));


  except
    on E: Exception do begin
      //TODO
    end;
  end;
end;

function TNWSService.ParseDateTime(const S: String): TDateTime;
begin
  Result:= 0; //TODO
end;

function TNWSService.Support: IWeatherSupport;
begin
  Result:= FSupport;
end;

end.
