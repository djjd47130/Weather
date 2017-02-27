unit JD.Weather.Services.DarkSky;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  IdHTTP,
  IdIOHandler,
  IdIOHandlerSocket,
  IdIOHandlerStack,
  IdSSL,
  IdSSLOpenSSL,
  JD.Weather.Intf,
  JD.Weather.SuperObject;

const
  SVC_CAPTION = 'DarkSky';
  SVC_NAME = 'DarkSky';
  SVC_AUTHOR = 'JD Software Inc.';
  SVC_UID = '{C16FD284-7A1B-401F-92D5-28FEA3F83D98}';

  SVC_MIN_PRICE = 0.0;
  SVC_MAX_PRICE = 0.0;
  SVC_HAS_TRIAL = False;
  SVC_HAS_PAID = True;
  SVC_IS_UNLIMITED = False;

  URL_MAIN = 'https://darksky.net';
  URL_API = 'https://darksky.net/dev/docs';
  URL_REGISTER = 'https://darksky.net/dev/register';
  URL_LOGIN = 'https://darksky.net/dev/login';
  URL_LEGAL = 'https://darksky.net/dev/docs/terms';
  URL_POWER = 'https://darksky.net/poweredby/';
  URL_USAGE = 'https://darksky.net/dev/account';

  {                                          //621FDB1332E425797C7603F6F6CEE3B
	   	"nearestStormDistance": 41,            //0B9D684BEB1842E4B5E365467D5DD2E5
	   	"nearestStormBearing": 278,
  		"ozone": 267.36
  }
  SUP_INFO = [wiConditions, wiForecastHourly, wiForecastDaily, wiAlerts, wiHistory];
  SUP_LOC = [wlCoords];
  SUP_UNITS = [wuImperial, wuMetric];
  SUP_LOGO = [ltColor, ltColorInvert, ltColorWide, ltColorInvertWide];
  SUP_COND_PROP = [wpIcon, wpCaption, wpTemp, wpFeelsLike,
    wpPrecipAmt, wpPrecipPred, wpWindSpeed, wpWindDir, wpHumidity, wpDewPoint,
    wpVisibility, wpCloudCover, wpPressure, wpStation];
  SUP_ALERT_TYPE = [];
  SUP_ALERT_PROP = [apDescription, apExpires, apMessage];
  SUP_FOR = [ftSummary, ftHourly, ftDaily];
  SUP_FOR_SUM = [wpPrecipAmt, wpPrecipPred];
  {
	   	"nearestStormDistance": 41,
	   	"nearestStormBearing": 278,
  		"ozone": 267.36
  }
  SUP_FOR_HOUR = [wpIcon, wpCaption, wpTemp, wpFeelsLike,
    wpPrecipAmt, wpPrecipPred, wpWindSpeed, wpWindDir, wpHumidity, wpDewPoint,
    wpVisibility, wpCloudCover, wpPressure, wpStation];
  {
			"moonPhase": 0.89,
			"precipIntensityMax": 0,
			"apparentTemperatureMin": 34.14,
			"apparentTemperatureMax": 48.23,
			"ozone": 266.73
  }
  SUP_FOR_DAY = [wpIcon, wpCaption, wpTemp, wpFeelsLike,
    wpPrecipAmt, wpPrecipPred, wpWindSpeed, wpWindDir, wpHumidity, wpDewPoint,
    wpVisibility, wpCloudCover, wpPressure, wpStation,
    wpTempMin, wpTempMax, wpSunrise, wpSunset];
  SUP_MAP = [];
  SUP_MAP_FOR = [];

type
  TDSEndpoint = (deNormal, deHistory);

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
    FSSL: TIdSSLIOHandlerSocketOpenSSL;
    function GetEndpointUrl(const Endpoint: TDSEndpoint): String;
    function GetEndpoint(const Endpoint: TDSEndpoint): ISuperObject;
  public
    constructor Create; override;
    destructor Destroy; override;
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

function TWeatherSupport.GetSupportedForecastSummaryProps: TWeatherPropTypes;
begin
  Result:= SUP_FOR_SUM;
end;

function TWeatherSupport.GetSupportedForecastHourlyProps: TWeatherPropTypes;
begin
  Result:= SUP_FOR_HOUR;
end;

function TWeatherSupport.GetSupportedForecastDailyProps: TWeatherPropTypes;
begin
  Result:= SUP_FOR_DAY;
end;

function TWeatherSupport.GetSupportedInfo: TWeatherInfoTypes;
begin
  Result:= SUP_INFO;
end;

function TWeatherSupport.GetSupportedMaps: TWeatherMapTypes;
begin
  Result:= SUP_MAP;
end;

function TWeatherSupport.GetSupportedMapFormats: TWeatherMapFormats;
begin
  Result:= SUP_MAP_FOR;
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
  FSSL:= TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  Web.IOHandler:= FSSL;
  Web.HandleRedirects:= True;
  FInfo:= TWeatherServiceInfo.Create;
  FInfo._AddRef;
end;

destructor TWeatherService.Destroy;
begin
  FInfo._Release;
  FInfo:= nil;
  FSSL.Free;
  inherited;
end;

function TWeatherService.GetEndpointUrl(const Endpoint: TDSEndpoint): String;
begin
  //TODO
  Result:= 'https://api.weathersource.com/v1/' + Key + '/' + Result;
end;

function TWeatherService.GetEndpoint(const Endpoint: TDSEndpoint): ISuperObject;
var
  U: String;
  S: String;
begin
  U:= GetEndpointUrl(Endpoint);
  S:= Web.Get(U);
  Result:= SO(S);
end;

function TWeatherService.GetMultiple(const Info: TWeatherInfoTypes): IWeatherMultiInfo;
begin
  //TODO: Fetch multiple pieces of weather data at once

end;

function TWeatherService.GetConditions: IWeatherProps;
var
  //O: ISuperObject;
  R: TWeatherProps;
begin
  R:= TWeatherProps.Create;
  try


  finally
    Result:= R;
  end;
end;

function TWeatherService.GetAlerts: IWeatherAlerts;
begin
  //NOT SUPPORTED
end;

function TWeatherService.ParseDateTime(const S: String): TDateTime;
begin
  Result:= 0;
  //TODO

end;

function TWeatherService.GetForecastDaily: IWeatherForecast;
var
  O: ISuperObject;
  A: TSuperArray;
  F: TWeatherForecast;
  I: TWeatherProps;
  X: Integer;
begin
  F:= TWeatherForecast.Create;
  try
    O:= GetEndpoint(TDSEndpoint.deNormal);
    if Assigned(O) then begin
      A:= O.AsArray;
      for X := 0 to A.Length-1 do begin
        O:= A.O[X];
        I:= TWeatherProps.Create;
        try

        finally
          F.FItems.Add(I);
        end;
      end;
    end;
  finally
    Result:= F;
  end;
end;

function TWeatherService.GetForecastHourly: IWeatherForecast;
var
  O: ISuperObject;
  F: TWeatherForecast;
begin
  F:= TWeatherForecast.Create;
  try
    O:= GetEndpoint(TDSEndpoint.deNormal);

  finally
    Result:= F;
  end;
end;

function TWeatherService.GetForecastSummary: IWeatherForecast;
begin
  //NOT SUPPORTED
end;

function TWeatherService.GetInfo: IWeatherServiceInfo;
begin
  Result:= FInfo;
end;

function TWeatherService.GetLocation: IWeatherLocation;
begin
  //TODO
end;

function TWeatherService.GetMaps: IWeatherMaps;
begin
  //NOT SUPPORTED
end;

end.
