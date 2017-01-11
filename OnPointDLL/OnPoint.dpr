library OnPoint;

{$R 'OnPointRes.res' 'OnPointRes.rc'}

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Imaging.PngImage,
  IdHTTP, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL,
  JD.Weather.Intf in '..\JD.Weather.Intf.pas',
  JD.Weather.SuperObject in '..\JD.Weather.SuperObject.pas';

{$R *.res}

type
  TOPEndpoint = (oeForecastDaily, oeForecastHourly, oeHistory);

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
    FSSL: TIdSSLIOHandlerSocketOpenSSL;
    procedure LoadLogos;
    function GetEndpointUrl(const Endpoint: TOPEndpoint): String;
    function GetEndpoint(const Endpoint: TOPEndpoint): ISuperObject;
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

{ TOPWeatherSupport }

function TOPWeatherSupport.GetSupportedUnits: TWeatherUnitsSet;
begin
  Result:= [wuImperial, wuMetric];
end;

function TOPWeatherSupport.GetSupportedLocations: TJDWeatherLocationTypes;
begin
  Result:= [wlCoords, wlZip];
end;

function TOPWeatherSupport.GetSupportedLogos: TWeatherLogoTypes;
begin
  Result:= [ltColor, ltColorInvert, ltColorWide, ltColorInvertWide];
end;

function TOPWeatherSupport.GetSupportedAlertProps: TWeatherAlertProps;
begin
  Result:= [];
end;

function TOPWeatherSupport.GetSupportedAlerts: TWeatherAlertTypes;
begin
  Result:= [];
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
  Result:= [fpPressureMB, fpWindSpeed,
    fpHumidity, fpDewPoint,
    fpFeelsLike, fpTemp, fpPrecip,
    fpSnow, fpPrecipChance, fpClouds, fpWetBulb];
end;

function TOPWeatherSupport.GetSupportedForecastDailyProps: TWeatherForecastProps;
begin
  Result:= [fpPressureMB, fpWindSpeed,
    fpHumidity, fpDewPoint, fpTempMin, fpTempMax,
    fpFeelsLike, fpTemp, fpPrecip,
    fpSnow, fpPrecipChance, fpClouds, fpWetBulb];
end;

function TOPWeatherSupport.GetSupportedInfo: TWeatherInfoTypes;
begin
  Result:= [wiConditions, wiForecastHourly, wiForecastDaily];
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

{ TOPService }

constructor TOPService.Create;
begin
  inherited;
  FSSL:= TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  Web.IOHandler:= FSSL;
  Web.HandleRedirects:= True;
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
  FSSL.Free;
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
  SetLogo(TWeatherLogoType.ltColorWide, Get('LOGO_COLOR'));


end;

function TOPService.Support: IWeatherSupport;
begin
  Result:= FSupport;
end;

function TOPService.GetEndpointUrl(const Endpoint: TOPEndpoint): String;
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

function TOPService.GetEndpoint(const Endpoint: TOPEndpoint): ISuperObject;
var
  U: String;
  S: String;
begin
  U:= GetEndpointUrl(Endpoint);
  S:= Web.Get(U);
  Result:= SO(S);
end;

function TOPService.GetMultiple(const Info: TWeatherInfoTypes): IWeatherMultiInfo;
begin
  //TODO: Fetch multiple pieces of weather data at once

end;

function TOPService.GetConditions: IWeatherConditions;
var
  //O: ISuperObject;
  R: TWeatherConditions;
begin
  R:= TWeatherConditions.Create;
  try

    //TODO: A WEATHER INFO SERVICE WHICH DOESN'T SUPPORT CURRENT CONDITIONS??!!
    //I DO NOT COMPREHEND!!!

    //O:= GetEndpoint(TOPEndpoint.weConditions);
    //FillConditions(O, R);
  finally
    Result:= R;
  end;
end;

function TOPService.GetAlerts: IWeatherAlerts;
begin
  //NOT SUPPORTED
end;

function TOPService.ParseDateTime(const S: String): TDateTime;
begin
  Result:= 0;
  //TODO

end;

function TOPService.GetForecastDaily: IWeatherForecast;
var
  O: ISuperObject;
  A: TSuperArray;
  F: TWeatherForecast;
  I: TWeatherForecastItem;
  X: Integer;
begin
  F:= TWeatherForecast.Create;
  try
    O:= GetEndpoint(TOPEndpoint.oeForecastDaily);
    if Assigned(O) then begin
      A:= O.AsArray;
      for X := 0 to A.Length-1 do begin
        O:= A.O[X];
        I:= TWeatherForecastItem.Create(F);
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

function TOPService.GetForecastHourly: IWeatherForecast;
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

function TOPService.GetForecastSummary: IWeatherForecast;
begin
  //NOT SUPPORTED
end;

function TOPService.GetMaps: IWeatherMaps;
begin
  //NOT SUPPORTED
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
