library AccuWeather;

{$R 'AccuWeatherRes.res' 'AccuWeatherRes.rc'}

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Imaging.PngImage,
  JD.Weather.Intf in '..\JD.Weather.Intf.pas',
  JD.Weather.SuperObject in '..\JD.Weather.SuperObject.pas';

{$R *.res}

type
  TAWWeatherSupport = class(TInterfacedObject, IWeatherSupport)
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

  TAWWeatherURLs = class(TInterfacedObject, IWeatherURLs)
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

  TAWService = class(TWeatherServiceBase, IWeatherService)
  private
    FSupport: TAWWeatherSupport;
    FURLs: TAWWeatherURLs;
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

{ TAWService }

constructor TAWService.Create;
begin
  inherited;
  FSupport:= TAWWeatherSupport.Create;
  FSupport._AddRef;
  FURLs:= TAWWeatherURLs.Create;
  FURLs._AddRef;
  LoadLogos;
end;

destructor TAWService.Destroy;
begin
  FURLs._Release;
  FURLs:= nil;
  FSupport._Release;
  FSupport:= nil;
  inherited;
end;

function TAWService.GetCaption: WideString;
begin
  Result:= 'AccuWeather';
end;

function TAWService.GetUID: WideString;
begin
  Result:= '{58042045-AA27-444A-AA26-C5C5CD4740B5}';
end;

function TAWService.GetURLs: IWeatherURLs;
begin
  Result:= FURLs;
end;

procedure TAWService.LoadLogos;
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

function TAWService.Support: IWeatherSupport;
begin
  Result:= FSupport;
end;

function TAWService.GetMultiple(
  const Info: TWeatherInfoTypes): IWeatherMultiInfo;
begin

end;

function TAWService.GetConditions: IWeatherConditions;
begin

end;

function TAWService.GetAlerts: IWeatherAlerts;
begin

end;

function TAWService.GetForecastDaily: IWeatherForecast;
begin

end;

function TAWService.GetForecastHourly: IWeatherForecast;
begin

end;

function TAWService.GetForecastSummary: IWeatherForecast;
begin

end;

function TAWService.GetMaps: IWeatherMaps;
begin

end;

{ TAWWeatherSupport }

function TAWWeatherSupport.GetSupportedUnits: TWeatherUnitsSet;
begin
  Result:= [wuImperial, wuMetric];
end;

function TAWWeatherSupport.GetSupportedLocations: TJDWeatherLocationTypes;
begin
  Result:= [wlCoords, wlCityState, wlCountryCity, wlCityCode,
    wlZip, wlAutoIP];
end;

function TAWWeatherSupport.GetSupportedLogos: TWeatherLogoTypes;
begin
  Result:= [ltColor, ltColorInvert, ltColorWide, ltColorInvertWide];
end;

function TAWWeatherSupport.GetSupportedAlertProps: TWeatherAlertProps;
begin
  Result:= [apZones, apVerticies, apStorm, apType, apDescription,
    apExpires, apMessage, apSignificance];
end;

function TAWWeatherSupport.GetSupportedAlerts: TWeatherAlertTypes;
begin
  Result:= [waNone, waHurricaneStat, waTornadoWarn, waTornadoWatch, waSevThundWarn,
    waSevThundWatch, waWinterAdv, waFloodWarn, waFloodWatch, waHighWind, waSevStat,
    waHeatAdv, waFogAdv, waSpecialStat, waFireAdv, waVolcanicStat, waHurricaneWarn,
    waRecordSet, waPublicRec, waPublicStat];
end;

function TAWWeatherSupport.GetSupportedConditionProps: TWeatherConditionsProps;
begin
  Result:= [cpPressureMB, cpPressureIn, cpWindDir, cpWindSpeed,
    cpHumidity, cpVisibility, cpDewPoint, cpWindGust, cpWindChill,
    cpFeelsLike, cpUV, cpTemp, cpPrecip,
    cpIcon, cpCaption, cpStation, cpClouds];
end;

function TAWWeatherSupport.GetSupportedForecasts: TWeatherForecastTypes;
begin
  Result:= [ftHourly, ftDaily];
end;

function TAWWeatherSupport.GetSupportedForecastSummaryProps: TWeatherForecastProps;
begin
  Result:= [];
end;

function TAWWeatherSupport.GetSupportedForecastHourlyProps: TWeatherForecastProps;
begin
  Result:= [];
end;

function TAWWeatherSupport.GetSupportedForecastDailyProps: TWeatherForecastProps;
begin
  Result:= [];
end;

function TAWWeatherSupport.GetSupportedInfo: TWeatherInfoTypes;
begin
  Result:= [wiConditions, wiForecastHourly, wiForecastDaily, wiAlerts];
end;

function TAWWeatherSupport.GetSupportedMaps: TWeatherMapTypes;
begin
  Result:= [];
end;

function TAWWeatherSupport.GetSupportedMapFormats: TWeatherMapFormats;
begin
  Result:= [];
end;

{ TAWWeatherURLs }

function TAWWeatherURLs.GetApiURL: WideString;
begin
  Result:= 'http://apidev.accuweather.com/developers/';
end;

function TAWWeatherURLs.GetLegalURL: WideString;
begin
  Result:= '';
end;

function TAWWeatherURLs.GetLoginURL: WideString;
begin
  Result:= '';
end;

function TAWWeatherURLs.GetMainURL: WideString;
begin
  Result:= 'http://developer.accuweather.com/';
end;

function TAWWeatherURLs.GetRegisterURL: WideString;
begin
  Result:= 'http://developer.accuweather.com/user/register';
end;









function CreateWeatherService: IWeatherService; stdcall;
var
  R: TAWService;
begin
  R:= TAWService.Create;
  try


  finally
    Result:= R;
  end;
end;

exports
  CreateWeatherService;

begin
end.
