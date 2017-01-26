unit JD.Weather.Services.OnPOINT;

interface

{$R 'OnPointRes.res' 'OnPointRes.rc'}

uses
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
  SVC_CAPTION = 'Weather Source OnPOINT®';
  SVC_NAME = 'OnPOINT';
  SVC_UID = '{23A3E12A-FB68-4F94-B7AE-53E72915A08F}';

  URL_MAIN = 'http://weathersource.com/';
  URL_API = 'https://developer.weathersource.com/documentation/rest/';
  URL_REGISTER = 'https://developer.weathersource.com/';
  URL_LOGIN = 'https://developer.weathersource.com/account/account-login/';
  URL_LEGAL = 'https://developer.weathersource.com/documentation/terms-of-use/';
  URL_POWER = 'https://developer.weathersource.dev/';
  URL_USAGE = 'https://developer.weathersource.com/account/';

  SUP_INFO = [wiConditions, wiForecastHourly, wiForecastDaily];
  SUP_LOC = [wlCoords, wlZip];
  SUP_UNITS = [wuImperial, wuMetric];
  SUP_LOGO = [ltColor, ltColorInvert, ltColorWide, ltColorInvertWide];
  SUP_COND_PROP = [wpIcon, wpCaption, wpStation, wpTemp,
    wpFeelsLike, wpFeelsLikeSun, wpFeelsLikeShade, wpWindDir, wpWindSpeed,
    wpWindGust, wpWindChill, wpPressure, wpHumidity, wpDewPoint, wpVisibility,
    wpUVIndex, wpCloudCover,
    wpPrecipAmt];
  SUP_ALERT_TYPE = [];
  SUP_ALERT_PROP = [];
  SUP_FOR = [ftHourly, ftDaily];
  SUP_FOR_SUM = [];
  SUP_FOR_HOUR = [];
    { [fpPressureMB, fpWindSpeed,
    fpHumidity, fpDewPoint,
    fpFeelsLike, fpTemp, fpPrecip,
    fpSnow, fpPrecipChance, fpClouds, fpWetBulb];}
  SUP_FOR_DAY = [];
    { [fpPressureMB, fpWindSpeed,
    fpHumidity, fpDewPoint, fpTempMin, fpTempMax,
    fpFeelsLike, fpTemp, fpPrecip,
    fpSnow, fpPrecipChance, fpClouds, fpWetBulb];
  SUP_UNITS = [wuImperial, wuMetric];}
  SUP_MAP = [];
  SUP_MAP_FOR = [];

type
  TOPEndpoint = (oeForecastDaily, oeForecastHourly, oeHistory);

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

  TWeatherService = class(TWeatherServiceBase, IWeatherService)
  private
    FInfo: TWeatherServiceInfo;
    FSSL: TIdSSLIOHandlerSocketOpenSSL;
    function GetEndpointUrl(const Endpoint: TOPEndpoint): String;
    function GetEndpoint(const Endpoint: TOPEndpoint): ISuperObject;
    function ParseDateTime(const S: String): TDateTime;
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

function TWeatherService.GetEndpointUrl(const Endpoint: TOPEndpoint): String;
begin
  case Endpoint of
    oeForecastDaily: begin
      case LocationType of
        wlZip:    Result:= 'postal_codes/'+LocationDetail1+',US/forecast.json?period=day';
        wlCoords: Result:= 'forecast.json?period=hour&latitude_eq='+
          LocationDetail1+'&longitude_eq='+LocationDetail2;
      end;
      Result:= Result + '&fields=tempMax,tempAvg,tempMin,precip,precipProb,snowfall,'+
        'snowfallProb,windSpdMax,windSpdAvg,windSpdMin,cldCvrMax,cldCvrAvg,cldCvrMin,'+
        'dewPtMax,dewPtAvg,dewPtMin,feelsLikeMax,feelsLikeAvg,feelsLikeMin,relHumMax,'+
        'relHumAvg,relHumMin,sfcPresMax,sfcPresAvg,sfcPresMin,spcHumMax,spcHumAvg,'+
        'spcHumMin,wetBultMax,wetBulbAvg,wetBulbMin,timestamp';
    end;
    oeForecastHourly: begin
      case LocationType of
        wlZip:    Result:= 'postal_codes/'+LocationDetail1+',US/forecast.json?period=hour';
        wlCoords: Result:= 'forecast.json?period=hour&latitude_eq='+
          LocationDetail1+'&longitude_eq='+LocationDetail2;
      end;
      Result:= Result + '&fields=temp,precip,precipProb,snowfall,snowfallProb,'+
        'windSpd,windDir,cldCvr,dewPt,feelsLike,relHum,sfcPres,spcHum,wetBulb';
    end;
    oeHistory: begin
      case LocationType of
        wlZip:    Result:= 'history_by_postal_code.json?period=hour&postal_code_eq='+
          LocationDetail1+'&country_eq=US';
        wlCoords: Result:= 'history.json?period=hour&latitude_eq='+
          LocationDetail1+'&longitude_eq='+LocationDetail2;
      end;
      Result:= Result + '&fields=tempMax,tempAvg,tempMin,precip,precipProb,snowfall,'+
        'snowfallProb,windSpdMax,windSpdAvg,windSpdMin,cldCvrMax,cldCvrAvg,cldCvrMin,'+
        'dewPtMax,dewPtAvg,dewPtMin,feelsLikeMax,feelsLikeAvg,feelsLikeMin,relHumMax,'+
        'relHumAvg,relHumMin,sfcPresMax,sfcPresAvg,sfcPresMin,spcHumMax,spcHumAvg,'+
        'spcHumMin,wetBultMax,wetBulbAvg,wetBulbMin,timestamp';
    end;
  end;
  Result:= 'https://api.weathersource.com/v1/' + Key + '/' + Result;
end;

function TWeatherService.GetEndpoint(const Endpoint: TOPEndpoint): ISuperObject;
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

    //TODO: A WEATHER INFO SERVICE WHICH DOESN'T SUPPORT CURRENT CONDITIONS??!!
    //I DO NOT COMPREHEND!!!

    //O:= GetEndpoint(TOPEndpoint.weConditions);
    //FillConditions(O, R);
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
    O:= GetEndpoint(TOPEndpoint.oeForecastDaily);
    if Assigned(O) then begin
      A:= O.AsArray;
      for X := 0 to A.Length-1 do begin
        O:= A.O[X];
        I:= TWeatherProps.Create;
        try
          I.FDateTime:= ParseDateTime(O.S['timestamp']);
          //TODO: Cloud Cover
          I.FDewPoint:= O.D['dewPtAvg'];
          //TODO: Feels Like
          //TODO: Precipitation
          I.FHumidity:= O.D['relHumAvg'];
          I.FPressure:= O.D['sfcPresAvg'];
          //TODO: Snowfall
          I.FTempMin:= O.D['tempMin'];
          I.FTempMax:= O.D['tempMax'];
          I.FWindSpeed:= O.D['windSpdAvg'];
          //TODO: Wind Direction (Not Supported???)
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
    O:= GetEndpoint(TOPEndpoint.oeForecastHourly);

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
