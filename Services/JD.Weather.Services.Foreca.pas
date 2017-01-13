unit JD.Weather.Services.Foreca;

{$R 'ForecaRes.res' 'ForecaRes.rc'}

interface

uses
  System.SysUtils,
  System.Classes,
  JD.Weather.Intf,
  JD.Weather.SuperObject,
  System.Generics.Collections;

const
  SVC_CAPTION = 'Foreca';
  SVC_UID = '{54B2BD21-1BDB-413B-B7ED-D1BBBB18FBA8}';

type
  TForecaWeatherSupport = class(TInterfacedObject, IWeatherSupport)
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

  TForecaWeatherURLs = class(TInterfacedObject, IWeatherURLs)
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

  TForecaService = class(TWeatherServiceBase, IWeatherService)
  private
    FSupport: TForecaWeatherSupport;
    FURLs: TForecaWeatherURLs;
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

{ TForecaService }

constructor TForecaService.Create;
begin
  inherited;
  FSupport:= TForecaWeatherSupport.Create;
  FSupport._AddRef;
  FURLs:= TForecaWeatherURLs.Create;
  FURLs._AddRef;
  LoadLogos;
end;

destructor TForecaService.Destroy;
begin
  FURLs._Release;
  FURLs:= nil;
  FSupport._Release;
  FSupport:= nil;
  inherited;
end;

function TForecaService.GetCaption: WideString;
begin
  Result:= SVC_CAPTION;
end;

function TForecaService.GetUID: WideString;
begin
  Result:= SVC_UID;
end;

function TForecaService.GetURLs: IWeatherURLs;
begin
  Result:= FURLs;
end;

procedure TForecaService.LoadLogos;
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

function TForecaService.Support: IWeatherSupport;
begin
  Result:= FSupport;
end;

function TForecaService.GetMultiple(
  const Info: TWeatherInfoTypes): IWeatherMultiInfo;
begin

end;

function TForecaService.GetConditions: IWeatherConditions;
begin

end;

function TForecaService.GetAlerts: IWeatherAlerts;
begin

end;

function TForecaService.GetForecastDaily: IWeatherForecast;
begin

end;

function TForecaService.GetForecastHourly: IWeatherForecast;
begin

end;

function TForecaService.GetForecastSummary: IWeatherForecast;
begin

end;

function TForecaService.GetMaps: IWeatherMaps;
begin

end;

{ TForecaWeatherSupport }

function TForecaWeatherSupport.GetSupportedUnits: TWeatherUnitsSet;
begin
  Result:= [wuKelvin, wuImperial, wuMetric];
end;

function TForecaWeatherSupport.GetSupportedLocations: TJDWeatherLocationTypes;
begin
  Result:= [wlCoords];
end;

function TForecaWeatherSupport.GetSupportedLogos: TWeatherLogoTypes;
begin
  Result:= [ltColor, ltColorInvert, ltColorWide, ltColorInvertWide];
end;

function TForecaWeatherSupport.GetSupportedAlertProps: TWeatherAlertProps;
begin
  Result:= [];
end;

function TForecaWeatherSupport.GetSupportedAlerts: TWeatherAlertTypes;
begin
  Result:= [];
end;

function TForecaWeatherSupport.GetSupportedConditionProps: TWeatherConditionsProps;
begin
  Result:= [cpPressureMB, cpWindDir, cpWindSpeed, cpHumidity, cpVisibility,
    cpDewPoint, cpFeelsLike, cpUV, cpTemp, cpPrecip, cpIcon, cpStation];
end;

function TForecaWeatherSupport.GetSupportedForecasts: TWeatherForecastTypes;
begin
  Result:= [ftHourly, ftDaily];
end;

function TForecaWeatherSupport.GetSupportedForecastSummaryProps: TWeatherForecastProps;
begin
  Result:= [];
end;

function TForecaWeatherSupport.GetSupportedForecastHourlyProps: TWeatherForecastProps;
begin
  Result:= [];
end;

function TForecaWeatherSupport.GetSupportedForecastDailyProps: TWeatherForecastProps;
begin
  Result:= [];
end;

function TForecaWeatherSupport.GetSupportedInfo: TWeatherInfoTypes;
begin
  Result:= [wiConditions, wiForecastHourly, wiForecastDaily];
end;

function TForecaWeatherSupport.GetSupportedMaps: TWeatherMapTypes;
begin
  Result:= [];
end;

function TForecaWeatherSupport.GetSupportedMapFormats: TWeatherMapFormats;
begin
  Result:= [];
end;

{ TForecaWeatherURLs }

function TForecaWeatherURLs.GetApiURL: WideString;
begin
  Result:= 'http://corporate.foreca.com/en/weather-api';
end;

function TForecaWeatherURLs.GetLegalURL: WideString;
begin
  Result:= '';
end;

function TForecaWeatherURLs.GetLoginURL: WideString;
begin
  Result:= '';
end;

function TForecaWeatherURLs.GetMainURL: WideString;
begin
  Result:= 'http://corporate.foreca.com/en/';
end;

function TForecaWeatherURLs.GetRegisterURL: WideString;
begin
  Result:= 'http://corporate.foreca.com/en/en/products-services/weather-api/try-it';
end;

end.
