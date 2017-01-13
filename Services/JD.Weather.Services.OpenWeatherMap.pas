unit JD.Weather.Services.OpenWeatherMap;

interface

{$R 'OpenWeatherMapsRes.res' 'OpenWeatherMapsRes.rc'}

uses
  System.SysUtils,
  System.Classes,
  JD.Weather.Intf,
  JD.Weather.SuperObject,
  System.Generics.Collections;

const
  SVC_CAPTION = 'Open Weather Map';
  SVC_UID = '{12ECA995-0C16-49EB-AF76-83690B426A9D}';

type
  TOWMWeatherSupport = class(TInterfacedObject, IWeatherSupport)
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

  TOWMWeatherURLs = class(TInterfacedObject, IWeatherURLs)
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

  TOWMService = class(TWeatherServiceBase, IWeatherService)
  private
    FSupport: TOWMWeatherSupport;
    FURLs: TOWMWeatherURLs;
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

implementation

{ TOWMService }

constructor TOWMService.Create;
begin
  inherited;
  FSupport:= TOWMWeatherSupport.Create;
  FSupport._AddRef;
  FURLs:= TOWMWeatherURLs.Create;
  FURLs._AddRef;
  LoadLogos;
end;

destructor TOWMService.Destroy;
begin
  FURLs._Release;
  FURLs:= nil;
  FSupport._Release;
  FSupport:= nil;
  inherited;
end;

function TOWMService.GetCaption: WideString;
begin
  Result:= SVC_CAPTION;
end;

function TOWMService.GetUID: WideString;
begin
  Result:= SVC_UID;
end;

function TOWMService.GetURLs: IWeatherURLs;
begin
  Result:= FURLs;
end;

procedure TOWMService.LoadLogos;
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

function TOWMService.Support: IWeatherSupport;
begin
  Result:= FSupport;
end;

function TOWMService.GetMultiple(
  const Info: TWeatherInfoTypes): IWeatherMultiInfo;
begin

end;

function TOWMService.GetConditions: IWeatherConditions;
begin

end;

function TOWMService.GetAlerts: IWeatherAlerts;
begin
  //Unsupported
end;

function TOWMService.GetForecastDaily: IWeatherForecast;
begin
  //Unsupported
end;

function TOWMService.GetForecastHourly: IWeatherForecast;
begin
  //Unsupported
end;

function TOWMService.GetForecastSummary: IWeatherForecast;
begin

end;

function TOWMService.GetMaps: IWeatherMaps;
begin
  //Unsupported
end;

{ TOWMWeatherSupport }

function TOWMWeatherSupport.GetSupportedUnits: TWeatherUnitsSet;
begin
  Result:= [wuKelvin, wuImperial, wuMetric];
end;

function TOWMWeatherSupport.GetSupportedLocations: TJDWeatherLocationTypes;
begin
  Result:= [wlZip, wlCityState, wlCoords, wlCityCode];
end;

function TOWMWeatherSupport.GetSupportedLogos: TWeatherLogoTypes;
begin
  Result:= [ltColor, ltColorInvert, ltColorWide, ltColorInvertWide];
end;

function TOWMWeatherSupport.GetSupportedAlertProps: TWeatherAlertProps;
begin
  Result:= [];
end;

function TOWMWeatherSupport.GetSupportedAlerts: TWeatherAlertTypes;
begin
  Result:= [];
end;

function TOWMWeatherSupport.GetSupportedConditionProps: TWeatherConditionsProps;
begin
  Result:= [cpPressureMB, cpWindDir,
    cpWindSpeed, cpHumidity, cpTemp, cpTempMin, cpTempMax,
    cpIcon, cpCaption, cpDescription,
    cpClouds, cpRain, cpSnow, cpSunrise, cpSunset];
end;

function TOWMWeatherSupport.GetSupportedForecasts: TWeatherForecastTypes;
begin
  Result:= [ftSummary, ftDaily];
end;

function TOWMWeatherSupport.GetSupportedForecastDailyProps: TWeatherForecastProps;
begin
  Result:= [fpPressureMB, fpWindDir, fpWindSpeed, fpHumidity, fpClouds,
    fpTemp, fpTempMin, fpTempMax, fpCaption, fpDescription, fpIcon,
    fpGroundPressure, fpSeaPressure,
    fpRain, fpSnow];
end;

function TOWMWeatherSupport.GetSupportedForecastHourlyProps: TWeatherForecastProps;
begin
  Result:= [];
end;

function TOWMWeatherSupport.GetSupportedForecastSummaryProps: TWeatherForecastProps;
begin
  Result:= [fpPressureMB, fpWindDir, fpWindSpeed,
    fpHumidity, fpTemp, fpTempMin, fpTempMax, fpCaption,
    fpDescription, fpIcon, fpGroundPressure, fpSeaPressure,
    fpSnow, fpPrecipChance, fpRain, fpClouds];
end;

function TOWMWeatherSupport.GetSupportedInfo: TWeatherInfoTypes;
begin
  Result:= [wiConditions, wiForecastSummary];
end;

function TOWMWeatherSupport.GetSupportedMaps: TWeatherMapTypes;
begin
  Result:= [mpClouds, mpPrecip, mpPressureSea,
    mpWind, mpTemp, mpSnowCover];
end;

function TOWMWeatherSupport.GetSupportedMapFormats: TWeatherMapFormats;
begin
  Result:= [wfPng, wfGif];
end;

{ TOWMWeatherURLs }

function TOWMWeatherURLs.GetApiURL: WideString;
begin
  Result:= 'https://openweathermap.org/api';
end;

function TOWMWeatherURLs.GetLegalURL: WideString;
begin
  Result:= 'http://openweathermap.org/terms';
end;

function TOWMWeatherURLs.GetLoginURL: WideString;
begin
  Result:= 'https://home.openweathermap.org/users/sign_in';
end;

function TOWMWeatherURLs.GetMainURL: WideString;
begin
  Result:= 'https://openweathermap.org/';
end;

function TOWMWeatherURLs.GetRegisterURL: WideString;
begin
  Result:= 'https://home.openweathermap.org/users/sign_up';
end;

end.
