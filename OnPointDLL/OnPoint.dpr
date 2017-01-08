library OnPoint;

{$R 'OnPointRes.res' 'OnPointRes.rc'}

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Imaging.PngImage,
  JD.Weather.Intf in '..\JD.Weather.Intf.pas',
  JD.Weather.SuperObject in '..\JD.Weather.SuperObject.pas';

{$R *.res}

type
  TOPWeatherSupport = class(TInterfacedObject, IWeatherSupport)
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

  TOPWeatherURLs = class(TInterfacedObject, IWeatherURLs)
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

  TOPService = class(TWeatherServiceBase, IWeatherService)
  private
    FSupport: TOPWeatherSupport;
    FURLs: TOPWeatherURLs;
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

{ TOPService }

constructor TOPService.Create;
begin
  inherited;
  FSupport:= TOPWeatherSupport.Create;
  FSupport._AddRef;
  FURLs:= TOPWeatherURLs.Create;
  FURLs._AddRef;
  LoadLogos;
end;

destructor TOPService.Destroy;
begin
  FURLs._Release;
  FURLs:= nil;
  FSupport._Release;
  FSupport:= nil;
  inherited;
end;

function TOPService.GetCaption: WideString;
begin
  Result:= 'Weather Source OnPOINT®';
end;

function TOPService.GetUID: WideString;
begin
  Result:= '{23A3E12A-FB68-4F94-B7AE-53E72915A08F}';
end;

function TOPService.GetURLs: IWeatherURLs;
begin
  Result:= FURLs;
end;

procedure TOPService.LoadLogos;
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

function TOPService.Support: IWeatherSupport;
begin
  Result:= FSupport;
end;

function TOPService.GetMultiple(
  const Info: TWeatherInfoTypes): IWeatherMultiInfo;
begin

end;

function TOPService.GetConditions: IWeatherConditions;
begin

end;

function TOPService.GetAlerts: IWeatherAlerts;
begin

end;

function TOPService.GetForecastDaily: IWeatherForecast;
begin

end;

function TOPService.GetForecastHourly: IWeatherForecast;
begin

end;

function TOPService.GetForecastSummary: IWeatherForecast;
begin

end;

function TOPService.GetMaps: IWeatherMaps;
begin

end;

{ TOPWeatherSupport }

function TOPWeatherSupport.GetSupportedUnits: TWeatherUnitsSet;
begin
  Result:= [wuImperial, wuMetric];
end;

function TOPWeatherSupport.GetSupportedLocations: TJDWeatherLocationTypes;
begin
  Result:= [wlCoords, wlCityState, wlCountryCity, wlCityCode,
    wlZip, wlAutoIP];
end;

function TOPWeatherSupport.GetSupportedLogos: TWeatherLogoTypes;
begin
  Result:= [ltColor, ltColorInvert, ltColorWide, ltColorInvertWide];
end;

function TOPWeatherSupport.GetSupportedAlertProps: TWeatherAlertProps;
begin
  Result:= [apZones, apVerticies, apStorm, apType, apDescription,
    apExpires, apMessage, apSignificance];
end;

function TOPWeatherSupport.GetSupportedAlerts: TWeatherAlertTypes;
begin
  Result:= [waNone, waHurricaneStat, waTornadoWarn, waTornadoWatch, waSevThundWarn,
    waSevThundWatch, waWinterAdv, waFloodWarn, waFloodWatch, waHighWind, waSevStat,
    waHeatAdv, waFogAdv, waSpecialStat, waFireAdv, waVolcanicStat, waHurricaneWarn,
    waRecordSet, waPublicRec, waPublicStat];
end;

function TOPWeatherSupport.GetSupportedConditionProps: TWeatherConditionsProps;
begin
  Result:= [cpPressureMB, cpPressureIn, cpWindDir, cpWindSpeed,
    cpHumidity, cpVisibility, cpDewPoint, cpWindGust, cpWindChill,
    cpFeelsLike, cpUV, cpTemp, cpPrecip,
    cpIcon, cpCaption, cpStation, cpClouds];
end;

function TOPWeatherSupport.GetSupportedForecasts: TWeatherForecastTypes;
begin
  Result:= [ftHourly, ftDaily];
end;

function TOPWeatherSupport.GetSupportedForecastSummaryProps: TWeatherForecastProps;
begin
  Result:= [];
end;

function TOPWeatherSupport.GetSupportedForecastHourlyProps: TWeatherForecastProps;
begin
  Result:= [];
end;

function TOPWeatherSupport.GetSupportedForecastDailyProps: TWeatherForecastProps;
begin
  Result:= [];
end;

function TOPWeatherSupport.GetSupportedInfo: TWeatherInfoTypes;
begin
  Result:= [wiConditions, wiForecastHourly, wiForecastDaily, wiAlerts];
end;

function TOPWeatherSupport.GetSupportedMaps: TWeatherMapTypes;
begin
  Result:= [];
end;

function TOPWeatherSupport.GetSupportedMapFormats: TWeatherMapFormats;
begin
  Result:= [];
end;

{ TOPWeatherURLs }

function TOPWeatherURLs.GetApiURL: WideString;
begin
  Result:= 'https://developer.weathersource.com/documentation/rest/';
end;

function TOPWeatherURLs.GetLegalURL: WideString;
begin
  Result:= 'https://developer.weathersource.com/documentation/terms-of-use/';
end;

function TOPWeatherURLs.GetLoginURL: WideString;
begin
  Result:= 'https://developer.weathersource.com/account/account-login/';
end;

function TOPWeatherURLs.GetMainURL: WideString;
begin
  Result:= 'http://weathersource.com/';
end;

function TOPWeatherURLs.GetRegisterURL: WideString;
begin
  Result:= 'https://developer.weathersource.com/';
end;









function CreateWeatherService: IWeatherService; stdcall;
var
  R: TOPService;
begin
  R:= TOPService.Create;
  try


  finally
    Result:= R;
  end;
end;

exports
  CreateWeatherService;

begin
end.
