unit JD.Weather.Intf;

interface

{$MINENUMSIZE 4}

uses
  Winapi.Windows,
  DateUtils,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  IdHTTP;

const
  clOrange = $00003AB3;
  clLightOrange = $002F73FF;
  clIceBlue = $00FFFFD2;
  clLightRed = $00B0B0FF;

type
  IWeatherService = interface;
  IWeatherServices = interface;
  IJDWeather = interface;
  IWeatherConditions = interface;
  IWeatherForecast = interface;
  IWeatherAlerts = interface;
  IWeatherMaps = interface;
  IWeatherGraphic = interface;
  TWeatherGraphic = class;


  TWeatherInfoType = (wiConditions, wiAlerts, wiForecastSummary,
    wiForecastHourly, wiForecastDaily, wiMaps, wiAlmanac, wiAstronomy,
    wiHurricane, wiHistory, wiPlanner, wiStation);
  TWeatherInfoTypes = set of TWeatherInfoType;

  TTemperature = record
    Value: Double;
    function GetAsFarenheit: Double;
    function GetAsCelsius: Double;
    function GetAsStr: String;
    function GetAsFarenheitStr: String;
    function GetAsCelsiusStr: String;
    procedure SetAsFarenheit(const Value: Double);
    procedure SetAsCelsius(const Value: Double);
    property AsFarenheit: Double read GetAsFarenheit write SetAsFarenheit;
    property AsCelsius: Double read GetAsCelsius write SetAsCelsius;
    class operator Implicit(const Value: TTemperature): Double;
    class operator Implicit(const Value: Double): TTemperature;
  end;

  TWeatherMapFormat = (wfJpg, wfPng, wfGif, wfTiff, wfBmp, wfFlash, wfHtml);
  TWeatherMapFormats = set of TWeatherMapFormat;

  TJDWeatherLocationType = (wlZip, wlCityState, wlCoords, wlAutoIP, wlCityCode,
    wlCountryCity, wlAirportCode, wlPWS);
  TJDWeatherLocationTypes = set of TJDWeatherLocationType;

  TWeatherConditionsEvent = procedure(Sender: TObject; const Conditions: IWeatherConditions) of object;

  TWeatherForecastEvent = procedure(Sender: TObject; const Forecast: IWeatherForecast) of object;

  TWeatherAlertEvent = procedure(Sender: TObject; const Alert: IWeatherAlerts) of object;

  TWeatherMapEvent = procedure(Sender: TObject; const Image: IWeatherMaps) of object;

  ///<summary>
  ///  Specifies a particular weather info service provider.
  ///</summary>
  TWeatherService = (wsOpenWeatherMap, wsWUnderground, wsNWS,
    wsNOAA, wsAccuWeather, wsForeca);

  ///<summary>
  ///  Specifies different types of measurement units.
  ///</summary>
  TWeatherUnits = (wuKelvin, wuImperial, wuMetric);
  TWeatherUnitsSet = set of TWeatherUnits;

  TWeatherConditionsProp = (cpPressureMB, cpPressureIn, cpWindDir, cpWindSpeed,
    cpHumidity, cpVisibility, cpDewPoint, cpHeatIndex, cpWindGust, cpWindChill,
    cpFeelsLike, cpSolarRad, cpUV, cpTemp, cpTempMin, cpTempMax, cpPrecip,
    cpIcon, cpCaption, cpDescription, cpStation, cpClouds,
    cpRain, cpSnow, cpSunrise, cpSunset);
  TWeatherConditionsProps = set of TWeatherConditionsProp;

  TWeatherForecastType = (ftSummary, ftHourly, ftDaily);
  TWeatherForecastTypes = set of TWeatherForecastType;

  TWeatherForecastProp = (fpPressureMB, fpPressureIn, fpWindDir, fpWindSpeed,
    fpHumidity, fpVisibility, fpDewPoint, fpHeatIndex, fpWindGust, fpWindChill,
    fpFeelsLike, fpSolarRad, fpUV, fpTemp, fpTempMin, fpTempMax, fpCaption,
    fpDescription, fpIcon, fpGroundPressure, fpSeaPressure, fpPrecip, fpURL,
    fpDaylight, fpSnow, fpSleet, fpPrecipChance, fpClouds, fpRain, fpWetBulb,
    fpIce, fpCeiling);
  TWeatherForecastProps = set of TWeatherForecastProp;

  ///<summary>
  ///  Specifies different types of weather alerts.
  ///</summary>
  TWeatherAlertType = (waNone, waHurricaneStat, waTornadoWarn, waTornadoWatch, waSevThundWarn,
    waSevThundWatch, waWinterAdv, waFloodWarn, waFloodWatch, waHighWind, waSevStat,
    waHeatAdv, waFogAdv, waSpecialStat, waFireAdv, waVolcanicStat, waHurricaneWarn,
    waRecordSet, waPublicRec, waPublicStat);
  TWeatherAlertTypes = set of TWeatherAlertType;

  TWeatherAlertProp = (apZones, apVerticies, apStorm, apType, apDescription,
    apExpires, apMessage, apPhenomena, apSignificance);
  TWeatherAlertProps = set of TWeatherAlertProp;

  ///<summary>
  ///  Specifies different types of weather phenomena.
  ///</summary>
  TWeatherAlertPhenomena = (wpWind, wpHeat, wpSmallCraft);

  TWeatherMapType = (mpSatellite, mpRadar, mpSatelliteRadar, mpRadarClouds,
    mpClouds, mpTemp, mpTempChange, mpSnowCover, mpPrecip, mpAlerts, mpHeatIndex,
    mpDewPoint, mpWindChill, mpPressureSea, mpWind,
    mpAniSatellite, mpAniRadar, mpAniSatelliteRadar);
  TWeatherMapTypes = set of TWeatherMapType;

  {$IFDEF USE_VCL}
  TMapArray = array[TWeatherMapType] of TPicture;
  {$ELSE}
  TMapArray = array[TWeatherMapType] of TWeatherGraphic;
  {$ENDIF}

  ///<summary>
  ///  Specifies whether it's day or night.
  ///</summary>
  TDayNight = (dnDay, dnNight);

  ///<summary>
  ///  Specifies the type of cloud cover.
  ///</summary>
  TCloudCode = (ccClear = 0, ccAlmostClear = 1, ccHalfCloudy = 2, ccBroken = 3,
    ccOvercast = 4, ccThinClouds = 5, ccFog = 6);

  ///<summary>
  ///  Specifies the level of precipitation.
  ///</summary>
  TPrecipCode = (pcNone = 0, pcSlight = 1, pcShowers = 2, pcPrecip = 3, pcThunder = 4);

  ///<summary>
  ///  Specifies the type of precipitation.
  ///</summary>
  TPrecipTypeCode = (ptRain = 0, ptSleet = 1, ptSnow = 2);

  ///<summary>
  ///  Represents standard weather condition information, and is
  ///  interchangable with a standard string format.
  ///</summary>
  TWeatherCode = record
  public
    DayNight: TDayNight;
    Clouds: TCloudCode;
    Precip: TPrecipCode;
    PrecipType: TPrecipTypeCode;
    class operator Implicit(const Value: TWeatherCode): String;
    class operator Implicit(const Value: String): TWeatherCode;
    class operator Implicit(const Value: TWeatherCode): Integer;
    class operator Implicit(const Value: Integer): TWeatherCode;
    function Description: String;
    function Name: String;
    function DayNightStr: String;
  end;




  TWeatherLogoType = (ltColor, ltColorInvert, ltColorWide, ltColorInvertWide,
    ltColorLeft, ltColorRight);
  TWeatherLogoTypes = set of TWeatherLogoType;

  TCreateJDWeather = function(const LibDir: WideString): IJDWeather; stdcall;

  TCreateWeatherService = function: IWeatherService; stdcall;

  IWeatherSupport = interface
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

  IWeatherURLs = interface
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

  IWeatherMultiInfo = interface
    function GetConditions: IWeatherConditions;
    function GetAlerts: IWeatherAlerts;
    function GetForecastSummary: IWeatherForecast;
    function GetForecastHourly: IWeatherForecast;
    function GetForecastDaily: IWeatherForecast;
    function GetMaps: IWeatherMaps;

    property Conditions: IWeatherConditions read GetConditions;
    property Alerts: IWeatherAlerts read GetAlerts;
    property ForecastSummary: IWeatherForecast read GetForecastSummary;
    property ForecastHourly: IWeatherForecast read GetForecastHourly;
    property ForecastDaily: IWeatherForecast read GetForecastDaily;
    property Maps: IWeatherMaps read GetMaps;
  end;

  IWeatherService = interface
    function GetModule: HMODULE;
    procedure SetModule(const Value: HMODULE);
    function GetUID: WideString;
    function GetCaption: WideString;
    function GetURLs: IWeatherURLs;
    function GetKey: WideString;
    procedure SetKey(const Value: WideString);
    function GetLocationType: TJDWeatherLocationType;
    procedure SetLocationType(const Value: TJDWeatherLocationType);
    function GetLocationDetail1: WideString;
    procedure SetLocationDetail1(const Value: WideString);
    function GetLocationDetail2: WideString;
    procedure SetLocationDetail2(const Value: WideString);
    function GetUnits: TWeatherUnits;
    procedure SetUnits(const Value: TWeatherUnits);

    function Support: IWeatherSupport;
    function GetLogo(const LT: TWeatherLogoType): IWeatherGraphic;

    function GetMultiple(const Info: TWeatherInfoTypes): IWeatherMultiInfo;
    function GetConditions: IWeatherConditions;
    function GetAlerts: IWeatherAlerts;
    function GetForecastSummary: IWeatherForecast;
    function GetForecastHourly: IWeatherForecast;
    function GetForecastDaily: IWeatherForecast;
    function GetMaps: IWeatherMaps;

    property Module: HMODULE read GetModule write SetModule;
    property UID: WideString read GetUID;
    property Caption: WideString read GetCaption;
    property URLs: IWeatherURLs read GetURLs;
    property Key: WideString read GetKey write SetKey;
    property LocationType: TJDWeatherLocationType read GetLocationType write SetLocationType;
    property LocationDetail1: WideString read GetLocationDetail1 write SetLocationDetail1;
    property LocationDetail2: WideString read GetLocationDetail2 write SetLocationDetail2;
    property Units: TWeatherUnits read GetUnits write SetUnits;
  end;

  IWeatherServices = interface
    function GetItem(const Index: Integer): IWeatherService;
    function Count: Integer;
    procedure LoadServices(const Dir: WideString);
    property Items[const Index: Integer]: IWeatherService read GetItem; default;
  end;

  IJDWeather = interface
    function GetServices: IWeatherServices;
    procedure SetLocationType(const Value: TJDWeatherLocationType);
    function GetLocationDetail1: WideString;
    function GetLocationType: TJDWeatherLocationType;
    procedure SetLocationDetail1(const Value: WideString);
    function GetLocationDetail2: WideString;
    procedure SetLocationDetail2(const Value: WideString);
    function GetUnits: TWeatherUnits;
    procedure SetUnits(const Value: TWeatherUnits);

    property Services: IWeatherServices read GetServices;
    property LocationType: TJDWeatherLocationType read GetLocationType write SetLocationType;
    property LocationDetail1: WideString read GetLocationDetail1 write SetLocationDetail1;
    property LocationDetail2: WideString read GetLocationDetail2 write SetLocationDetail2;
    property Units: TWeatherUnits read GetUnits write SetUnits;
  end;

  IWeatherGraphic = interface
    function GetExt: WideString;
    function GetBase64: WideString;
    procedure SetBase64(const Value: WideString);

    property Ext: WideString read GetExt;
    property Base64: WideString read GetBase64 write SetBase64;
  end;

  IWeatherLocation = interface
    function GetDisplayName: WideString;
    function GetCity: WideString;
    function GetState: WideString;
    function GetStateAbbr: WideString;
    function GetCountry: WideString;
    function GetCountryAbbr: WideString;
    function GetLongitude: Double;
    function GetLatitude: Double;
    function GetElevation: Double;
    function GetZipCode: WideString;
    property DisplayName: WideString read GetDisplayName;
    property City: WideString read GetCity;
    property State: WideString read GetState;
    property StateAbbr: WideString read GetStateAbbr;
    property Country: WideString read GetCountry;
    property CountryAbbr: WideString read GetCountryAbbr;
    property Longitude: Double read GetLongitude;
    property Latitude: Double read GetLatitude;
    property Elevation: Double read GetElevation;
    property ZipCode: WideString read GetZipCode;
  end;

  IWeatherConditions = interface
    {$IFDEF USE_VCL}
    function GetPicture: TPicture;
    {$ELSE}
    function GetPicture: IWeatherGraphic;
    {$ENDIF}
    function GetLocation: IWeatherLocation;
    function GetDateTime: TDateTime;
    function GetTemp: Single;
    function GetHumidity: Single;
    function GetPressure: Single;
    function GetCondition: WideString;
    function GetDescription: WideString;
    function GetWindDir: Single;
    function GetWindSpeed: Single;
    function GetVisibility: Single;
    function GetDewPoint: Single;
    //function SupportedProps: TWeatherConditionsProps;
    {$IFDEF USE_VCL}
    property Picture: TPicture read GetPicture;
    {$ELSE}
    property Picture: IWeatherGraphic read GetPicture;
    {$ENDIF}
    property Location: IWeatherLocation read GetLocation;
    property DateTime: TDateTime read GetDateTime;
    property Temp: Single read GetTemp;
    property Humidity: Single read GetHumidity;
    property Pressure: Single read GetPressure;
    property Condition: WideString read GetCondition;
    property Description: WideString read GetDescription;
    property WindSpeed: Single read GetWindSpeed;
    property WindDir: Single read GetWindDir;
    property Visibility: Single read GetVisibility;
    property DewPoint: Single read GetDewPoint;
  end;

  IWeatherForecastItem = interface
    {$IFDEF USE_VCL}
    function GetPicture: TPicture;
    {$ENDIF}
    function GetDateTime: TDateTime;
    function GetTemp: Single;
    function GetTempMax: Single;
    function GetTempMin: Single;
    function GetHumidity: Single;
    function GetPressure: Single;
    function GetCondition: WideString;
    function GetDescription: WideString;
    function GetWindDir: Single;
    function GetWindSpeed: Single;
    function GetVisibility: Single;
    function GetDewPoint: Single;
    {$IFDEF USE_VCL}
    property Picture: TPicture read GetPicture;
    {$ENDIF}
    property DateTime: TDateTime read GetDateTime;
    property Temp: Single read GetTemp;
    property TempMin: Single read GetTempMin;
    property TempMax: Single read GetTempMax;
    property Humidity: Single read GetHumidity;
    property Pressure: Single read GetPressure;
    property Condition: WideString read GetCondition;
    property Description: WideString read GetDescription;
    property WindSpeed: Single read GetWindSpeed;
    property WindDir: Single read GetWindDir;
    property Visibility: Single read GetVisibility;
    property DewPoint: Single read GetDewPoint;
  end;

  IWeatherForecast = interface
    function GetLocation: IWeatherLocation;
    function GetItem(const Index: Integer): IWeatherForecastItem;
    function Count: Integer;
    function MinTemp: Single;
    function MaxTemp: Single;
    property Location: IWeatherLocation read GetLocation;
    property Items[const Index: Integer]: IWeatherForecastItem read GetItem; default;
  end;

  IWeatherAlertZone = interface
    function GetState: WideString;
    function GetZone: WideString;
    property State: WideString read GetState;
    property Zone: WideString read GetZone;
  end;

  IWeatherAlertZones = interface
    function GetItem(const Index: Integer): IWeatherAlertZone;
    function Count: Integer;
    property Items[const Index: Integer]: IWeatherAlertZone read GetItem; default;
  end;

  IWeatherStormVertex = interface
    function GetLongitude: Double;
    function GetLatitude: Double;
    property Longitude: Double read GetLongitude;
    property Latitude: Double read GetLatitude;
  end;

  IWeatherStormVerticies = interface
    function GetItem(const Index: Integer): IWeatherStormVertex;
    function Count: Integer;
    property Items[const Index: Integer]: IWeatherStormVertex read GetItem; default;
  end;

  IWeatherAlertStorm = interface
    function GetDateTime: TDateTime;
    function GetDirection: Single;
    function GetSpeed: Single;
    function GetLongitude: Double;
    function GetLatitude: Double;
    function GetVerticies: IWeatherStormVerticies;
    property DateTime: TDateTime read GetDateTime;
    property Direction: Single read GetDirection;
    property Speed: Single read GetDirection;
    property Longitude: Double read GetLongitude;
    property Latitude: Double read GetLatitude;
    property Verticies: IWeatherStormVerticies read GetVerticies;
  end;

  IWeatherAlert = interface
    function GetAlertType: TWeatherAlertType;
    function GetDescription: WideString;
    function GetDateTime: TDateTime;
    function GetExpires: TDateTime;
    function GetMsg: WideString;
    function GetPhenomena: WideString;
    function GetSignificance: WideString;
    function GetZones: IWeatherAlertZones;
    function GetStorm: IWeatherAlertStorm;
    property AlertType: TWeatherAlertType read GetAlertType;
    property Description: WideString read GetDescription;
    property DateTime: TDateTime read GetDateTime;
    property Expires: TDateTime read GetExpires;
    property Msg: WideString read GetMsg;
    property Phenomena: WideString read GetPhenomena;
    property Significance: WideString read GetSignificance;
    property Zones: IWeatherAlertZones read GetZones;
    property Storm: IWeatherAlertStorm read GetStorm;
  end;

  IWeatherAlerts = interface
    function GetItem(const Index: Integer): IWeatherAlert;
    function Count: Integer;
    property Items[const Index: Integer]: IWeatherAlert read GetItem; default;
  end;

  IWeatherMaps = interface
    {$IFDEF USE_VCL}
    function GetMap(const MapType: TWeatherMapType): TPicture;
    {$ELSE}
    function GetMap(const MapType: TWeatherMapType): IWeatherGraphic;
    {$ENDIF}

    {$IFDEF USE_VCL}
    property Maps[const MapType: TWeatherMapType]: TPicture read GetMap;
    {$ELSE}
    property Maps[const MapType: TWeatherMapType]: IWeatherGraphic read GetMap;
    {$ENDIF}
  end;






  { Interface Implementation Objects }

  TWeatherGraphic = class(TInterfacedObject, IWeatherGraphic)
  private
    FStream: TStringStream;
    FExt: WideString;
  public
    constructor Create; overload;
    constructor Create(AStream: TStream); overload;
    constructor Create(AFilename: WideString); overload;
    destructor Destroy; override;
  public
    function GetExt: WideString;
    function GetBase64: WideString;
    procedure SetBase64(const Value: WideString);

    property Ext: WideString read GetExt;
    property Base64: WideString read GetBase64 write SetBase64;
  end;

  TWeatherLocation = class(TInterfacedObject, IWeatherLocation)
  public
    FDisplayName: WideString;
    FCity: WideString;
    FState: WideString;
    FStateAbbr: WideString;
    FCountry: WideString;
    FCountryAbbr: WideString;
    FLongitude: Double;
    FLatitude: Double;
    FElevation: Double;
    FZipCode: WideString;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetDisplayName: WideString;
    function GetCity: WideString;
    function GetState: WideString;
    function GetStateAbbr: WideString;
    function GetCountry: WideString;
    function GetCountryAbbr: WideString;
    function GetLongitude: Double;
    function GetLatitude: Double;
    function GetElevation: Double;
    function GetZipCode: WideString;
    property DisplayName: WideString read GetDisplayName;
    property City: WideString read GetCity;
    property State: WideString read GetState;
    property StateAbbr: WideString read GetStateAbbr;
    property Country: WideString read GetCountry;
    property CountryAbbr: WideString read GetCountryAbbr;
    property Longitude: Double read GetLongitude;
    property Latitude: Double read GetLatitude;
    property Elevation: Double read GetElevation;
    property ZipCode: WideString read GetZipCode;
  end;

  TWeatherConditions = class(TInterfacedObject, IWeatherConditions)
  public
    {$IFDEF USE_VCL}
    FPicture: TPicture;
    {$ELSE}
    FPicture: TWeatherGraphic;
    {$ENDIF}
    //FOwner: TJDWeatherThread;
    FLocation: TWeatherLocation;
    FDateTime: TDateTime;
    FTemp: Single;
    FHumidity: Single;
    FPressure: Single;
    FCondition: WideString;
    FDescription: WideString;
    FWindSpeed: Single;
    FWindDir: Single;
    FVisibility: Single;
    FDewPoint: Single;
  public
    constructor Create;
    destructor Destroy; override;
  public
    {$IFDEF USE_VCL}
    function GetPicture: TPicture;
    {$ELSE}
    function GetPicture: IWeatherGraphic;
    {$ENDIF}
    function GetLocation: IWeatherLocation;
    function GetDateTime: TDateTime;
    function GetTemp: Single;
    function GetHumidity: Single;
    function GetPressure: Single;
    function GetCondition: WideString;
    function GetDescription: WideString;
    function GetWindDir: Single;
    function GetWindSpeed: Single;
    function GetVisibility: Single;
    function GetDewPoint: Single;
    {$IFDEF USE_VCL}
    property Picture: TPicture read GetPicture;
    {$ELSE}
    property Picture: IWeatherGraphic read GetPicture;
    {$ENDIF}
    property Location: IWeatherLocation read GetLocation;
    property DateTime: TDateTime read GetDateTime;
    property Temp: Single read GetTemp;
    property Humidity: Single read GetHumidity;
    property Pressure: Single read GetPressure;
    property Condition: WideString read GetCondition;
    property Description: WideString read GetDescription;
    property WindSpeed: Single read GetWindSpeed;
    property WindDir: Single read GetWindDir;
    property Visibility: Single read GetVisibility;
    property DewPoint: Single read GetDewPoint;
  end;

  TWeatherForecast = class;

  TWeatherForecastItem = class(TInterfacedObject, IWeatherForecastItem)
  public
    FOwner: TWeatherForecast;
    {$IFDEF USE_VCL}
    FPicture: TPicture;
    {$ELSE}
    FPicture: TWeatherGraphic;
    {$ENDIF}
    FDateTime: TDateTime;
    FTemp: Single;
    FTempMin: Single;
    FTempMax: Single;
    FHumidity: Single;
    FPressure: Single;
    FCondition: WideString;
    FDescription: WideString;
    FWindSpeed: Single;
    FWindDir: Single;
    FVisibility: Single;
    FDewPoint: Single;
  public
    constructor Create(AOwner: TWeatherForecast);
    destructor Destroy; override;
  public
    {$IFDEF USE_VCL}
    function GetPicture: TPicture;
    {$ELSE}
    function GetPicture: IWeatherGraphic;
    {$ENDIF}
    function GetDateTime: TDateTime;
    function GetTemp: Single;
    function GetTempMax: Single;
    function GetTempMin: Single;
    function GetHumidity: Single;
    function GetPressure: Single;
    function GetCondition: WideString;
    function GetDescription: WideString;
    function GetWindDir: Single;
    function GetWindSpeed: Single;
    function GetVisibility: Single;
    function GetDewPoint: Single;
    {$IFDEF USE_VCL}
    property Picture: TPicture read GetPicture;
    {$ELSE}
    property Picture: IWeatherGraphic read GetPicture;
    {$ENDIF}
    property DateTime: TDateTime read GetDateTime;
    property Temp: Single read GetTemp;
    property TempMin: Single read GetTempMin;
    property TempMax: Single read GetTempMax;
    property Humidity: Single read GetHumidity;
    property Pressure: Single read GetPressure;
    property Condition: WideString read GetCondition;
    property Description: WideString read GetDescription;
    property WindSpeed: Single read GetWindSpeed;
    property WindDir: Single read GetWindDir;
    property Visibility: Single read GetVisibility;
    property DewPoint: Single read GetDewPoint;
  end;

  TWeatherForecast = class(TInterfacedObject, IWeatherForecast)
  public
    FItems: TList<IWeatherForecastItem>;
    FLocation: TWeatherLocation;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetLocation: IWeatherLocation;
    function GetItem(const Index: Integer): IWeatherForecastItem;
    function Count: Integer;
    function MinTemp: Single;
    function MaxTemp: Single;
    property Location: IWeatherLocation read GetLocation;
    property Items[const Index: Integer]: IWeatherForecastItem read GetItem; default;
  end;

  TWeatherStormVertex = class(TInterfacedObject, IWeatherStormVertex)
  public
    FLongitude: Double;
    FLatitude: Double;
  public
    function GetLongitude: Double;
    function GetLatitude: Double;
    property Longitude: Double read GetLongitude;
    property Latitude: Double read GetLatitude;
  end;

  TWeatherStormVerticies = class(TInterfacedObject, IWeatherStormVerticies)
  public
    FItems: TList<IWeatherStormVertex>;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetItem(const Index: Integer): IWeatherStormVertex;
    function Count: Integer;
    property Items[const Index: Integer]: IWeatherStormVertex read GetItem; default;
  end;

  TWeatherAlertStorm = class(TInterfacedObject, IWeatherAlertStorm)
  public
    FDateTime: TDateTime;
    FDirection: Single;
    FSpeed: Single;
    FLongitude: Double;
    FLatitude: Double;
    FVerticies: TWeatherStormVerticies;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetDateTime: TDateTime;
    function GetDirection: Single;
    function GetSpeed: Single;
    function GetLongitude: Double;
    function GetLatitude: Double;
    function GetVerticies: IWeatherStormVerticies;
    property DateTime: TDateTime read GetDateTime;
    property Direction: Single read GetDirection;
    property Speed: Single read GetDirection;
    property Longitude: Double read GetLongitude;
    property Latitude: Double read GetLatitude;
    property Verticies: IWeatherStormVerticies read GetVerticies;
  end;

  TWeatherAlertZone = class(TInterfacedObject, IWeatherAlertZone)
  public
    FState: WideString;
    FZone: WideString;
  public
    function GetState: WideString;
    function GetZone: WideString;
    property State: WideString read GetState;
    property Zone: WideString read GetZone;
  end;

  TWeatherAlertZones = class(TInterfacedObject, IWeatherAlertZones)
  public
    FItems: TList<TWeatherAlertZone>;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetItem(const Index: Integer): IWeatherAlertZone;
    function Count: Integer;
    property Items[const Index: Integer]: IWeatherAlertZone read GetItem; default;
  end;

  TWeatherAlert = class(TInterfacedObject, IWeatherAlert)
  public
    FAlertType: TWeatherAlertType;
    FDescription: WideString;
    FDateTime: TDateTime;
    FExpires: TDateTime;
    FMsg: WideString;
    FPhenomena: WideString;
    FSignificance: WideString;
    FZones: TWeatherAlertZones;
    FStorm: TWeatherAlertStorm;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetAlertType: TWeatherAlertType;
    function GetDescription: WideString;
    function GetDateTime: TDateTime;
    function GetExpires: TDateTime;
    function GetMsg: WideString;
    function GetPhenomena: WideString;
    function GetSignificance: WideString;
    function GetZones: IWeatherAlertZones;
    function GetStorm: IWeatherAlertStorm;
    property AlertType: TWeatherAlertType read GetAlertType;
    property Description: WideString read GetDescription;
    property DateTime: TDateTime read GetDateTime;
    property Expires: TDateTime read GetExpires;
    property Msg: WideString read GetMsg;
    property Phenomena: WideString read GetPhenomena;
    property Significance: WideString read GetSignificance;
    property Zones: IWeatherAlertZones read GetZones;
    property Storm: IWeatherAlertStorm read GetStorm;
  end;

  TWeatherAlerts = class(TInterfacedObject, IWeatherAlerts)
  public
    FItems: TList<IWeatherAlert>;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetItem(const Index: Integer): IWeatherAlert;
    function Count: Integer;
    property Items[const Index: Integer]: IWeatherAlert read GetItem; default;
  end;

  TWeatherMaps = class(TInterfacedObject, IWeatherMaps)
  public
    FMaps: TMapArray;
  public
    constructor Create;
    destructor Destroy; override;
  public
    {$IFDEF USE_VCL}
    function GetMap(const MapType: TWeatherMapType): TPicture;
    {$ELSE}
    function GetMap(const MapType: TWeatherMapType): IWeatherGraphic;
    {$ENDIF}

    {$IFDEF USE_VCL}
    property Maps[const MapType: TWeatherMapType]: TPicture read GetMap;
    {$ELSE}
    property Maps[const MapType: TWeatherMapType]: IWeatherGraphic read GetMap;
    {$ENDIF}
  end;






  TWeatherMultiInfo = class(TInterfacedObject, IWeatherMultiInfo)
  private
    FConditions: TWeatherConditions;
    FAlerts: TWeatherAlerts;
    FForecastSummary: TWeatherForecast;
    FForecastHourly: TWeatherForecast;
    FForecastDaily: TWeatherForecast;
    FMaps: IWeatherMaps;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetAll(Con: TWeatherConditions; Alr: TWeatherAlerts;
      Fos, Foh, Fod: TWeatherForecast; Map: IWeatherMaps);
  public
    function GetConditions: IWeatherConditions;
    function GetAlerts: IWeatherAlerts;
    function GetForecastSummary: IWeatherForecast;
    function GetForecastHourly: IWeatherForecast;
    function GetForecastDaily: IWeatherForecast;
    function GetMaps: IWeatherMaps;

    property Conditions: IWeatherConditions read GetConditions;
    property Alerts: IWeatherAlerts read GetAlerts;
    property ForecastSummary: IWeatherForecast read GetForecastSummary;
    property ForecastHourly: IWeatherForecast read GetForecastHourly;
    property ForecastDaily: IWeatherForecast read GetForecastDaily;
    property Maps: IWeatherMaps read GetMaps;
  end;

  TLogoArray = array[TWeatherLogoType] of IWeatherGraphic;

  TWeatherServiceBase = class(TInterfacedObject)
  private
    FLogos: TLogoArray;
    FModule: HMODULE;
    FKey: WideString;
    FWeb: TIdHTTP;
    FLocationType: TJDWeatherLocationType;
    FLocationDetail1: WideString;
    FLocationDetail2: WideString;
    FUnits: TWeatherUnits;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Web: TIdHTTP read FWeb;
    procedure SetLogo(const LT: TWeatherLogoType; const Value: IWeatherGraphic);
  public
    function GetModule: HMODULE;
    procedure SetModule(const Value: HMODULE);
    function GetKey: WideString;
    procedure SetKey(const Value: WideString);
    function GetLocationType: TJDWeatherLocationType;
    procedure SetLocationType(const Value: TJDWeatherLocationType);
    function GetLocationDetail1: WideString;
    procedure SetLocationDetail1(const Value: WideString);
    function GetLocationDetail2: WideString;
    procedure SetLocationDetail2(const Value: WideString);
    function GetUnits: TWeatherUnits;
    procedure SetUnits(const Value: TWeatherUnits);

    function GetLogo(const LT: TWeatherLogoType): IWeatherGraphic;

    property Module: HMODULE read GetModule write SetModule;
    property Key: WideString read GetKey write SetKey;
    property LocationType: TJDWeatherLocationType read GetLocationType write SetLocationType;
    property LocationDetail1: WideString read GetLocationDetail1 write SetLocationDetail1;
    property LocationDetail2: WideString read GetLocationDetail2 write SetLocationDetail2;
    property Units: TWeatherUnits read GetUnits write SetUnits;

    {
    The following must be implemented in the inherited class...
    Refer to WUnderground.dpr for a sample implementation...

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
    }
  end;






{$IFDEF USE_VCL}
function TempColor(const Temp: Single): TColor;
{$ENDIF}

///<summary>
///  Converts a degree angle to a cardinal direction string
///</summary>
function DegreeToDir(const D: Single): String;

///<summary>
///  Converts an epoch integer to local TDateTime
///</summary>
function EpochLocal(const Value: Integer): TDateTime; overload;

///<summary>
///  Converts an epoch string to local TDateTime
///</summary>
function EpochLocal(const Value: String): TDateTime; overload;

function LocalDateTimeFromUTCDateTime(const UTCDateTime: TDateTime): TDateTime;

function WeatherUnitsToStr(const Value: TWeatherUnits): String;
function WeatherMapFormatToStr(const Value: TWeatherMapFormat): String;
function WeatherMapTypeToStr(const Value: TWeatherMapType): String;
function WeatherForecastPropToStr(const Value: TWeatherForecastProp): String;
function WeatherForecastTypeToStr(const Value: TWeatherForecastType): String;
function WeatherAlertPropToStr(const Value: TWeatherAlertProp): String;
function WeatherAlertTypeToStr(const Value: TWeatherAlertType): String;
function WeatherConditionPropToStr(const Value: TWeatherConditionsProp): String;
function WeatherInfoTypeToStr(const Value: TWeatherInfoType): String;
function WeatherLocationTypeToStr(const Value: TJDWeatherLocationType): String;

implementation

function WeatherLocationTypeToStr(const Value: TJDWeatherLocationType): String;
begin
  case Value of
    wlZip:          Result:= 'Zip Code';
    wlCityState:    Result:= 'City and State';
    wlCoords:       Result:= 'Coordinates';
    wlAutoIP:       Result:= 'Automatic IP';
    wlCityCode:     Result:= 'City Code';
    wlCountryCity:  Result:= 'City and Country';
    wlAirportCode:  Result:= 'Airport Code';
    wlPWS:          Result:= 'PWS';
  end;
end;

function WeatherInfoTypeToStr(const Value: TWeatherInfoType): String;
begin
  case Value of
    wiConditions:       Result:= 'Current Conditions';
    wiAlerts:           Result:= 'Weather Alerts';
    wiForecastSummary:  Result:= 'Forecast Summary';
    wiForecastHourly:   Result:= 'Forecast Hourly';
    wiForecastDaily:    Result:= 'Forecast Daily';
    wiMaps:             Result:= 'Maps';
    wiAlmanac:          Result:= 'Almanac';
    wiAstronomy:        Result:= 'Astronomy';
    wiHurricane:        Result:= 'Hurricane';
    wiHistory:          Result:= 'History';
    wiPlanner:          Result:= 'Planner';
    wiStation:          Result:= 'Station';
  end;
end;

function WeatherConditionPropToStr(const Value: TWeatherConditionsProp): String;
begin
  case Value of
    cpPressureMB:   Result:= 'Pressure (Mb)';
    cpPressureIn:   Result:= 'Pressure (In)';
    cpWindDir:      Result:= 'Wind Direction';
    cpWindSpeed:    Result:= 'Wind Speed';
    cpHumidity:     Result:= 'Humidity';
    cpVisibility:   Result:= 'Visibility';
    cpDewPoint:     Result:= 'Dew Point';
    cpHeatIndex:    Result:= 'Heat Index';
    cpWindGust:     Result:= 'Wind Gusts';
    cpWindChill:    Result:= 'Wind Chill';
    cpFeelsLike:    Result:= 'Feels Like';
    cpSolarRad:     Result:= 'Solar Radiation';
    cpUV:           Result:= 'UV Index';
    cpTemp:         Result:= 'Temperature';
    cpTempMin:      Result:= 'Temp Min';
    cpTempMax:      Result:= 'Temp Max';
    cpPrecip:       Result:= 'Precipitation';
    cpIcon:         Result:= 'Weather Icon';
    cpCaption:      Result:= 'Caption';
    cpDescription:  Result:= 'Description';
    cpStation:      Result:= 'Station';
    cpClouds:       Result:= 'Clouds';
    cpRain:         Result:= 'Rain';
    cpSnow:         Result:= 'Snow';
    cpSunrise:      Result:= 'Sunrise';
    cpSunset:       Result:= 'Sunset';
  end;
end;

function WeatherAlertTypeToStr(const Value: TWeatherAlertType): String;
begin
  case Value of
    waNone:           Result:= 'None';
    waHurricaneStat:  Result:= 'Hurricane Status';
    waTornadoWarn:    Result:= 'Tornado Warning';
    waTornadoWatch:   Result:= 'Tornado Watch';
    waSevThundWarn:   Result:= 'Severe Thunderstorm Warning';
    waSevThundWatch:  Result:= 'Severe Thunderstorm Watch';
    waWinterAdv:      Result:= 'Winter Weather Advisory';
    waFloodWarn:      Result:= 'Flood Warning';
    waFloodWatch:     Result:= 'Flood Watch';
    waHighWind:       Result:= 'High Wind Advisory';
    waSevStat:        Result:= 'Severe Weather Status';
    waHeatAdv:        Result:= 'Heat Advisory';
    waFogAdv:         Result:= 'Fog Advisory';
    waSpecialStat:    Result:= 'Special Weather Statement';
    waFireAdv:        Result:= 'Fire Advisory';
    waVolcanicStat:   Result:= 'Volcanic Status';
    waHurricaneWarn:  Result:= 'Hurricane Warning';
    waRecordSet:      Result:= 'Record Set';
    waPublicRec:      Result:= 'Public Record';
    waPublicStat:     Result:= 'Public Status';
  end;
end;

function WeatherAlertPropToStr(const Value: TWeatherAlertProp): String;
begin
  case Value of
    apZones:        Result:= 'Affected Zones';
    apVerticies:    Result:= 'Storm Verticies';
    apStorm:        Result:= 'Storm Information';
    apType:         Result:= 'Alert Type';
    apDescription:  Result:= 'Description';
    apExpires:      Result:= 'Expiration Time';
    apMessage:      Result:= 'Alert Message';
    apPhenomena:    Result:= 'Phenomena';
    apSignificance: Result:= 'Significance';
  end;
end;

function WeatherForecastTypeToStr(const Value: TWeatherForecastType): String;
begin
  case Value of
    ftSummary:  Result:= 'Summary';
    ftHourly:   Result:= 'Hourly';
    ftDaily:    Result:= 'Daily';
  end;
end;

function WeatherForecastPropToStr(const Value: TWeatherForecastProp): String;
begin
  case Value of
    fpPressureMB:     Result:= 'Pressure (Mb)';
    fpPressureIn:     Result:= 'Pressure (In)';
    fpWindDir:        Result:= 'Wind Direction';
    fpWindSpeed:      Result:= 'Wind Speed';
    fpHumidity:       Result:= 'Humidity';
    fpVisibility:     Result:= 'Visibility';
    fpDewPoint:       Result:= 'Dew Point';
    fpHeatIndex:      Result:= 'Heat Index';
    fpWindGust:       Result:= 'Wind Gusts';
    fpWindChill:      Result:= 'Wind Chill';
    fpFeelsLike:      Result:= 'Feels Like';
    fpSolarRad:       Result:= 'Solar Radiation';
    fpUV:             Result:= 'UV Index';
    fpTemp:           Result:= 'Temperature';
    fpTempMin:        Result:= 'Temp Min';
    fpTempMax:        Result:= 'Temp Max';
    fpCaption:        Result:= 'Caption';
    fpDescription:    Result:= 'Description';
    fpIcon:           Result:= 'Weather Icon';
    fpGroundPressure: Result:= 'Ground Pressure';
    fpSeaPressure:    Result:= 'Sea Pressure';
    fpPrecip:         Result:= 'Precipitation Amount';
    fpURL:            Result:= 'URL';
    fpDaylight:       Result:= 'Daylight';
    fpSnow:           Result:= 'Snow Amount';
    fpSleet:          Result:= 'Sleet Amount';
    fpPrecipChance:   Result:= 'Chance of Precipitation';
    fpClouds:         Result:= 'Cloud Cover';
    fpRain:           Result:= 'Rain Amount';
    fpWetBulb:        Result:= 'Wet Bulb';
  end;
end;

function WeatherMapTypeToStr(const Value: TWeatherMapType): String;
begin
  case Value of
    mpSatellite:      Result:= 'Satellite';
    mpRadar:          Result:= 'Radar';
    mpSatelliteRadar: Result:= 'Satellite and Radar';
    mpRadarClouds:    Result:= 'Radar and Clouds';
    mpClouds:         Result:= 'Clouds';
    mpTemp:           Result:= 'Temperature';
    mpTempChange:     Result:= 'Temp Change';
    mpSnowCover:      Result:= 'Snow Cover';
    mpPrecip:         Result:= 'Precipitation';
    mpAlerts:         Result:= 'Alerts';
    mpHeatIndex:      Result:= 'Heat Index';
    mpDewPoint:       Result:= 'Dew Point';
    mpWindChill:      Result:= 'Wind Chill';
    mpPressureSea:    Result:= 'Sea Level Pressure';
    mpWind:           Result:= 'Wind';
    mpAniSatellite:   Result:= 'Animated Satellite';
    mpAniRadar:       Result:= 'Animated Radar';
    mpAniSatelliteRadar:  Result:= 'Animated Satellite and Radar';
  end;
end;

function WeatherMapFormatToStr(const Value: TWeatherMapFormat): String;
begin
  case Value of
    wfJpg:    Result:= 'Jpg';
    wfPng:    Result:= 'Png';
    wfGif:    Result:= 'Gif';
    wfTiff:   Result:= 'Tiff';
    wfBmp:    Result:= 'Bitmap';
    wfFlash:  Result:= 'Flash Object';
    wfHtml:   Result:= 'HTML Page';
  end;
end;

function WeatherUnitsToStr(const Value: TWeatherUnits): String;
begin
  case Value of
    wuKelvin:   Result:= 'Kelvin';
    wuImperial: Result:= 'Imperial';
    wuMetric:   Result:= 'Metric';
  end;
end;

{$IFDEF USE_VCL}
function TempColor(const Temp: Single): TColor;
const
  MAX_TEMP = 99999;
  MIN_TEMP = -MIN_TEMP;
var
  T: Integer;
begin
  T:= Trunc(Temp);
  case T of
    -MIN_TEMP..32:  Result:= clIceBlue;
    33..55:         Result:= clSkyBlue;
    56..73:         Result:= clMoneyGreen;
    74..90:         Result:= clLightRed;
    91..MAX_TEMP:   Result:= clLightOrange;
    else            Result:= clWhite;
  end;
end;
{$ENDIF}

function DegreeToDir(const D: Single): String;
var
  I: Integer;
begin
  I:= Trunc(D);
  case I of
    0..11,348..365: Result:= 'N';
    12..33: Result:= 'NNE';
    34..56: Result:= 'NE';
    57..78: Result:= 'ENE';
    79..101: Result:= 'E';
    102..123: Result:= 'ESE';
    124..146: Result:= 'SE';
    147..168: Result:= 'SSE';
    169..191: Result:= 'S';
    192..213: Result:= 'SSW';
    214..236: Result:= 'SW';
    237..258: Result:= 'WSW';
    259..281: Result:= 'W';
    282..303: Result:= 'WNW';
    304..326: Result:= 'NW';
    327..347: Result:= 'NNW';
  end;
end;

function EpochLocal(const Value: Integer): TDateTime; overload;
begin
  Result:= UnixToDateTime(Value);
  Result:= LocalDateTimeFromUTCDateTime(Result);
end;

function EpochLocal(const Value: String): TDateTime; overload;
begin
  Result:= EpochLocal(StrToIntDef(Value, 0));
end;

function LocalDateTimeFromUTCDateTime(const UTCDateTime: TDateTime): TDateTime;
var
  LocalSystemTime: TSystemTime;
  UTCSystemTime: TSystemTime;
  LocalFileTime: TFileTime;
  UTCFileTime: TFileTime;
begin
  DateTimeToSystemTime(UTCDateTime, UTCSystemTime);
  SystemTimeToFileTime(UTCSystemTime, UTCFileTime);
  if FileTimeToLocalFileTime(UTCFileTime, LocalFileTime)
  and FileTimeToSystemTime(LocalFileTime, LocalSystemTime) then begin
    Result := SystemTimeToDateTime(LocalSystemTime);
  end else begin
    Result := UTCDateTime;  // Default to UTC if any conversion function fails.
  end;
end;

{ TWeatherCode }

class operator TWeatherCode.Implicit(const Value: TWeatherCode): String;
begin
  case Value.DayNight of
    dnDay:    Result:= 'd';
    dnNight:  Result:= 'n';
  end;
  Result:= Result + IntToStr(Integer(Value.Clouds));
  Result:= Result + IntToStr(Integer(Value.Precip));
  Result:= Result + IntToStr(Integer(Value.PrecipType));
end;

class operator TWeatherCode.Implicit(const Value: String): TWeatherCode;
begin
  if Length(Value) <> 4 then raise Exception.Create('Value must be 4 characters.');

  case Value[1] of
    'd','D': Result.DayNight:= TDayNight.dnDay;
    'n','N': Result.DayNight:= TDayNight.dnNight;
    else raise Exception.Create('First value must be either d, D, n, or N.');
  end;

  if CharInSet(Value[2], ['0'..'6']) then
    Result.Clouds:= TCloudCode(StrToIntDef(Value[2], 0))
  else
    raise Exception.Create('Second value must be between 0 and 6.');

  if CharInSet(Value[3], ['0'..'4']) then
    Result.Precip:= TPrecipCode(StrToIntDef(Value[3], 0))
  else
    raise Exception.Create('Third value must be between 0 and 4.');

  if CharInSet(Value[4], ['0'..'2']) then
    Result.PrecipType:= TPrecipTypeCode(StrToIntDef(Value[4], 0))
  else
    raise Exception.Create('Fourth value must be between 0 and 2.');
end;

function TWeatherCode.DayNightStr: String;
begin
  case DayNight of
    dnDay:    Result:= 'Day';
    dnNight:  Result:= 'Night';
  end;
end;

function TWeatherCode.Description: String;
begin
  case Clouds of
    ccClear:        Result:= 'Clear';
    ccAlmostClear:  Result:= 'Mostly Clear';
    ccHalfCloudy:   Result:= 'Partly Cloudy';
    ccBroken:       Result:= 'Cloudy';
    ccOvercast:     Result:= 'Overcast';
    ccThinClouds:   Result:= 'Thin High Clouds';
    ccFog:          Result:= 'Fog';
  end;
  case PrecipType of
    ptRain: begin
      case Precip of
        pcNone:         Result:= Result + '';
        pcSlight:       Result:= Result + ' with Light Rain';
        pcShowers:      Result:= Result + ' with Rain Showers';
        pcPrecip:       Result:= Result + ' with Rain';
        pcThunder:      Result:= Result + ' with Rain and Thunderstorms';
      end;
    end;
    ptSleet: begin
      case Precip of
        pcNone:         Result:= Result + '';
        pcSlight:       Result:= Result + ' with Light Sleet';
        pcShowers:      Result:= Result + ' with Sleet Showers';
        pcPrecip:       Result:= Result + ' with Sleet';
        pcThunder:      Result:= Result + ' with Sleet and Thunderstorms';
      end;
    end;
    ptSnow: begin
      case Precip of
        pcNone:         Result:= Result + '';
        pcSlight:       Result:= Result + ' with Light Snow';
        pcShowers:      Result:= Result + ' with Snow Showers';
        pcPrecip:       Result:= Result + ' with Snow';
        pcThunder:      Result:= Result + ' with Snow and Thunderstorms';
      end;
    end;
  end;
end;

class operator TWeatherCode.Implicit(const Value: TWeatherCode): Integer;
begin
  Result:= Integer(Value.DayNight);
  Result:= Result + (Integer(Value.Clouds) * 10);
  Result:= Result + (Integer(Value.Precip) * 100);
  Result:= Result + (Integer(Value.PrecipType) * 1000);
end;

class operator TWeatherCode.Implicit(const Value: Integer): TWeatherCode;
begin
  //Result.DayNight:= TDayNight();
end;

function TWeatherCode.Name: String;
begin
  //TODO: Return unique standardized name
  case Clouds of
    ccClear:        Result:= 'clear_';
    ccAlmostClear:  Result:= 'almost_clear_';
    ccHalfCloudy:   Result:= 'half_cloudy_';
    ccBroken:       Result:= 'broken_clouds_';
    ccOvercast:     Result:= 'overcast_';
    ccThinClouds:   Result:= 'thin_clouds_';
    ccFog:          Result:= 'fog_';
  end;
  case PrecipType of
    ptRain: begin
      case Precip of
        pcNone:     Result:= Result + '';
        pcSlight:   Result:= Result + 'rain_slight';
        pcShowers:  Result:= Result + 'rain_showers';
        pcPrecip:   Result:= Result + 'rain';
        pcThunder:  Result:= Result + 'rain_and_thunder';
      end;
    end;
    ptSleet: begin
      case Precip of
        pcNone:     Result:= Result + '';
        pcSlight:   Result:= Result + 'sleet_slight';
        pcShowers:  Result:= Result + 'sleet_showers';
        pcPrecip:   Result:= Result + 'sleet';
        pcThunder:  Result:= Result + 'sleet_and_thunder';
      end;
    end;
    ptSnow: begin
      case Precip of
        pcNone:     Result:= Result + '';
        pcSlight:   Result:= Result + 'snow_slight';
        pcShowers:  Result:= Result + 'snow_showers';
        pcPrecip:   Result:= Result + 'snow';
        pcThunder:  Result:= Result + 'snow_and_thunder';
      end;
    end;
  end;
end;

{ TTemperature }

function TTemperature.GetAsCelsius: Double;
begin
  Result:= Value;
end;

function TTemperature.GetAsCelsiusStr: String;
begin
  Result:= FormatFloat('0° C', GetAsCelsius);
end;

function TTemperature.GetAsFarenheit: Double;
begin
  Result:= Value - 32;
  Result:= Result * 0.5556;
end;

function TTemperature.GetAsFarenheitStr: String;
begin
  Result:= FormatFloat('0° F', GetAsFarenheit);
end;

function TTemperature.GetAsStr: String;
begin
  Result:= FormatFloat('0°', GetAsFarenheit);
end;

procedure TTemperature.SetAsCelsius(const Value: Double);
begin
  Self.Value:= Value;
end;

procedure TTemperature.SetAsFarenheit(const Value: Double);
begin
  Self.Value:= Value * 1.8;
  Self.Value:= Self.Value + 32;
end;

class operator TTemperature.Implicit(const Value: TTemperature): Double;
begin
  Result:= Value.Value;
end;

class operator TTemperature.Implicit(const Value: Double): TTemperature;
begin
  Result.Value:= Value;
end;

{ TWeatherGraphic }

constructor TWeatherGraphic.Create;
begin
  FStream:= TStringStream.Create;
end;

constructor TWeatherGraphic.Create(AStream: TStream);
begin
  Create;
  FStream.LoadFromStream(AStream);
  FStream.Position:= 0;
end;

constructor TWeatherGraphic.Create(AFilename: WideString);
begin
  Create;
  FStream.LoadFromFile(AFilename);
  FStream.Position:= 0;
end;

destructor TWeatherGraphic.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

function TWeatherGraphic.GetBase64: WideString;
begin
  Result:= FStream.DataString;
end;

function TWeatherGraphic.GetExt: WideString;
begin
  Result:= FExt;
end;

procedure TWeatherGraphic.SetBase64(const Value: WideString);
begin
  FStream.Clear;
  FStream.WriteString(Value);
end;

{ TWeatherLocation }

constructor TWeatherLocation.Create;
begin

end;

destructor TWeatherLocation.Destroy;
begin

  inherited;
end;

function TWeatherLocation.GetCity: WideString;
begin
  Result:= FCity;
end;

function TWeatherLocation.GetCountry: WideString;
begin
  Result:= FCountry;
end;

function TWeatherLocation.GetCountryAbbr: WideString;
begin
  Result:= FCountryAbbr;
end;

function TWeatherLocation.GetDisplayName: WideString;
begin
  Result:= FDisplayName;
end;

function TWeatherLocation.GetElevation: Double;
begin
  Result:= FElevation;
end;

function TWeatherLocation.GetLatitude: Double;
begin
  Result:= FLatitude;
end;

function TWeatherLocation.GetLongitude: Double;
begin
  Result:= FLongitude;
end;

function TWeatherLocation.GetState: WideString;
begin
  Result:= FState;
end;

function TWeatherLocation.GetStateAbbr: WideString;
begin
  Result:= FStateAbbr;
end;

function TWeatherLocation.GetZipCode: WideString;
begin
  Result:= FZipCode;
end;

{ TWeatherConditions }

//constructor TWeatherConditions.Create(AOwner: TJDWeatherThread);
constructor TWeatherConditions.Create;
begin
  //FOwner:= AOwner;
  FLocation:= TWeatherLocation.Create;
  FLocation._AddRef;
  {$IFDEF USE_VCL}
  FPicture:= TPicture.Create;
  {$ELSE}
  FPicture:= TWeatherGraphic.Create;
  FPicture._AddRef;
  {$ENDIF}
end;

destructor TWeatherConditions.Destroy;
begin
  {$IFDEF USE_VCL}
  FreeAndNil(FPicture);
  {$ELSE}
  FPicture._Release;
  FPicture:= nil;
  {$ENDIF}
  FLocation._Release;
  FLocation:= nil;
  inherited;
end;

function TWeatherConditions.GetCondition: WideString;
begin
  Result:= FCondition;
end;

function TWeatherConditions.GetDateTime: TDateTime;
begin
  Result:= FDateTime;
end;

function TWeatherConditions.GetDescription: WideString;
begin
  Result:= FDescription;
end;

function TWeatherConditions.GetDewPoint: Single;
begin
  Result:= FDewPoint;
end;

function TWeatherConditions.GetHumidity: Single;
begin
  Result:= FHumidity;
end;

function TWeatherConditions.GetLocation: IWeatherLocation;
begin
  Result:= FLocation;
end;

{$IFDEF USE_VCL}
function TWeatherConditions.GetPicture: TPicture;
begin
  Result:= FPicture;
end;
{$ELSE}
function TWeatherConditions.GetPicture: IWeatherGraphic;
begin
  Result:= FPicture;
end;
{$ENDIF}

function TWeatherConditions.GetPressure: Single;
begin
  Result:= FPressure;
end;

function TWeatherConditions.GetTemp: Single;
begin
  Result:= FTemp;
end;

function TWeatherConditions.GetVisibility: Single;
begin
  Result:= FVisibility;
end;

function TWeatherConditions.GetWindDir: Single;
begin
  Result:= FWindDir;
end;

function TWeatherConditions.GetWindSpeed: Single;
begin
  Result:= FWindSpeed;
end;

{
function TWeatherConditions.SupportedProps: TWeatherConditionsProps;
begin
  case FOwner.FOwner.FService of
    wsOpenWeatherMap: Result:= [cpPressureMb, cpWindDir, cpWindSpeed,
      cpVisibility, cpHumidity, cpCaption, cpDescription, cpClouds,
      cpRain, cpSnow, cpSunrise, cpSunset, cpTemp, cpTempMin, cpTempMax];
    wsWUnderground: Result:= [cpPressureMB, cpPressureIn, cpWindDir,
      cpWindSpeed, cpHumidity, cpVisibility, cpDewPoint, cpHeatIndex,
      cpWindGust, cpWindChill, cpFeelsLike, cpSolarRad, cpUV, cpTemp,
      cpTempMin, cpTempMax, cpPrecip, cpIcon, cpCaption, cpDescription,
      cpStation];
    wsAccuWeather:      Result:= [
      ];
    wsNWS:              Result:= [
      ];
    wsNOAA:             Result:= [
      ];
  end;
end;
}

{ TWeatherForecastItem }

constructor TWeatherForecastItem.Create(AOwner: TWeatherForecast);
begin
  FOwner:= AOwner;
  {$IFDEF USE_VCL}
  FPicture:= TPicture.Create;
  {$ELSE}
  FPicture:= TWeatherGraphic.Create;
  FPicture._AddRef;
  {$ENDIF}
end;

destructor TWeatherForecastItem.Destroy;
begin
  {$IFDEF USE_VCL}
  FreeAndNil(FPicture);
  {$ELSE}
  FPicture._Release;
  FPicture:= nil;
  {$ENDIF}
  inherited;
end;

function TWeatherForecastItem.GetCondition: WideString;
begin
  Result:= FCondition;
end;

function TWeatherForecastItem.GetDateTime: TDateTime;
begin
  Result:= FDateTime;
end;

function TWeatherForecastItem.GetDescription: WideString;
begin
  Result:= FDescription;
end;

function TWeatherForecastItem.GetDewPoint: Single;
begin
  Result:= FDewPoint;
end;

function TWeatherForecastItem.GetHumidity: Single;
begin
  Result:= FHumidity;
end;

{$IFDEF USE_VCL}
function TWeatherForecastItem.GetPicture: TPicture;
{$ELSE}
function TWeatherForecastItem.GetPicture: IWeatherGraphic;
{$ENDIF}
begin
  Result:= FPicture;
end;

function TWeatherForecastItem.GetPressure: Single;
begin
  Result:= FPressure;
end;

function TWeatherForecastItem.GetTemp: Single;
begin
  Result:= FTemp;
end;

function TWeatherForecastItem.GetTempMax: Single;
begin
  Result:= FTempMax;
end;

function TWeatherForecastItem.GetTempMin: Single;
begin
  Result:= FTempMin;
end;

function TWeatherForecastItem.GetVisibility: Single;
begin
  Result:= FVisibility;
end;

function TWeatherForecastItem.GetWindDir: Single;
begin
  Result:= FWindDir;
end;

function TWeatherForecastItem.GetWindSpeed: Single;
begin
  Result:= FWindSpeed;
end;

{ TWeatherForecast }

constructor TWeatherForecast.Create;
begin
  FItems:= TList<IWeatherForecastItem>.Create;
  FLocation:= TWeatherLocation.Create;
  FLocation._AddRef;
end;

procedure TWeatherForecast.Clear;
var
  X: Integer;
begin
  for X := 0 to FItems.Count-1 do begin
    FItems[X]._Release;
  end;
  FLocation._Release;
  FLocation:= nil;
  FItems.Clear;
end;

destructor TWeatherForecast.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TWeatherForecast.Count: Integer;
begin
  Result:= FItems.Count;
end;

function TWeatherForecast.GetItem(const Index: Integer): IWeatherForecastItem;
begin
  Result:= FItems[Index];
end;

function TWeatherForecast.GetLocation: IWeatherLocation;
begin
  Result:= FLocation;
end;

function TWeatherForecast.MaxTemp: Single;
var
  X: Integer;
begin
  Result:= -9999999;
  for X := 0 to FItems.Count-1 do begin
    if FItems[X].Temp > Result then
      Result:= FItems[X].Temp;
  end;
end;

function TWeatherForecast.MinTemp: Single;
var
  X: Integer;
begin
  Result:= 9999999;
  for X := 0 to FItems.Count-1 do begin
    if FItems[X].Temp < Result then
      Result:= FItems[X].Temp;
  end;
end;

{ TWeatherStormVertex }

function TWeatherStormVertex.GetLatitude: Double;
begin
  Result:= FLatitude;
end;

function TWeatherStormVertex.GetLongitude: Double;
begin
  Result:= FLongitude;
end;

{ TWeatherStormVerticies }

constructor TWeatherStormVerticies.Create;
begin
  FItems:= TList<IWeatherStormVertex>.Create;
end;

destructor TWeatherStormVerticies.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TWeatherStormVerticies.Count: Integer;
begin
  Result:= FItems.Count;
end;

procedure TWeatherStormVerticies.Clear;
var
  X: Integer;
begin
  for X := 0 to FItems.Count-1 do begin
    FItems[X]._Release;
  end;
  FItems.Clear;
end;

function TWeatherStormVerticies.GetItem(
  const Index: Integer): IWeatherStormVertex;
begin
  Result:= FItems[Index];
end;

{ TWeatherAlertStorm }

constructor TWeatherAlertStorm.Create;
begin
  FVerticies:= TWeatherStormVerticies.Create;
  FVerticies._AddRef;
end;

destructor TWeatherAlertStorm.Destroy;
begin
  FVerticies._Release;
  FVerticies:= nil;
  inherited;
end;

function TWeatherAlertStorm.GetDateTime: TDateTime;
begin
  Result:= FDateTime;
end;

function TWeatherAlertStorm.GetDirection: Single;
begin
  Result:= FDirection;
end;

function TWeatherAlertStorm.GetLatitude: Double;
begin
  Result:= FLatitude;
end;

function TWeatherAlertStorm.GetLongitude: Double;
begin
  Result:= FLongitude;
end;

function TWeatherAlertStorm.GetSpeed: Single;
begin
  Result:= FSpeed;
end;

function TWeatherAlertStorm.GetVerticies: IWeatherStormVerticies;
begin
  Result:= FVerticies;
end;

{ TWeatherAlertZone }

function TWeatherAlertZone.GetState: WideString;
begin
  Result:= FState;
end;

function TWeatherAlertZone.GetZone: WideString;
begin
  Result:= FZone;
end;

{ TWeatherAlertZones }

constructor TWeatherAlertZones.Create;
begin
  FItems:= TList<TWeatherAlertZone>.Create;

end;

destructor TWeatherAlertZones.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

procedure TWeatherAlertZones.Clear;
var
  X: Integer;
begin
  for X := 0 to FItems.Count-1 do begin
    FItems[X]._Release;
  end;
  FItems.Clear;
end;

function TWeatherAlertZones.Count: Integer;
begin
  Result:= FItems.Count;
end;

function TWeatherAlertZones.GetItem(const Index: Integer): IWeatherAlertZone;
begin
  Result:= FItems[Index];
end;

{ TWeatherAlert }

constructor TWeatherAlert.Create;
begin
  FZones:= TWeatherAlertZones.Create;
  FZones._AddRef;
  FStorm:= TWeatherAlertStorm.Create;
  FStorm._AddRef;
end;

destructor TWeatherAlert.Destroy;
begin
  FStorm._Release;
  FStorm:= nil;
  FZones._Release;
  FZones:= nil;
  inherited;
end;

function TWeatherAlert.GetAlertType: TWeatherAlertType;
begin
  Result:= FAlertType;
end;

function TWeatherAlert.GetDateTime: TDateTime;
begin
  Result:= FDateTime;
end;

function TWeatherAlert.GetDescription: WideString;
begin
  Result:= FDescription;
end;

function TWeatherAlert.GetExpires: TDateTime;
begin
  Result:= FExpires;
end;

function TWeatherAlert.GetMsg: WideString;
begin
  Result:= FMsg;
end;

function TWeatherAlert.GetPhenomena: WideString;
begin
  Result:= FPhenomena;
end;

function TWeatherAlert.GetSignificance: WideString;
begin
  Result:= FSignificance;
end;

function TWeatherAlert.GetStorm: IWeatherAlertStorm;
begin
  Result:= FStorm;
end;

function TWeatherAlert.GetZones: IWeatherAlertZones;
begin
  Result:= FZones;
end;

{ TWeatherAlerts }

constructor TWeatherAlerts.Create;
begin
  FItems:= TList<IWeatherAlert>.Create;
end;

destructor TWeatherAlerts.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TWeatherAlerts.Count: Integer;
begin
  Result:= FItems.Count;
end;

procedure TWeatherAlerts.Clear;
var
  X: Integer;
begin
  for X := 0 to FItems.Count-1 do begin
    FItems[X]._Release;
  end;
  FItems.Clear;
end;

function TWeatherAlerts.GetItem(const Index: Integer): IWeatherAlert;
begin
  Result:= FItems[Index];
end;

{ TWeatherMaps }

constructor TWeatherMaps.Create;
var
  X: TWeatherMapType;
begin
  {$IFDEF USE_VCL}
  for X := Low(TMapArray) to High(TMapArray) do begin
    FMaps[X]:= TPicture.Create;
  end;
  {$ELSE}
  for X := Low(TMapArray) to High(TMapArray) do begin
    FMaps[X]:= TWeatherGraphic.Create;
  end;
  {$ENDIF}
end;

destructor TWeatherMaps.Destroy;
var
  X: TWeatherMapType;
{$IFDEF USE_VCL}
  P: TPicture;
{$ELSE}
  P: IWeatherGraphic;
{$ENDIF}
begin
  for X := Low(TMapArray) to High(TMapArray) do begin
    P:= FMaps[X];
  {$IFDEF USE_VCL}
    FreeAndNil(P);
  {ELSE}
    P._Release;
    P:= nil;
  {$ENDIF}
  end;
  inherited;
end;

{$IFDEF USE_VCL}
function TWeatherMaps.GetMap(const MapType: TWeatherMapType): TPicture;
begin
  Result:= FMaps[MapType];
end;
{$ELSE}
function TWeatherMaps.GetMap(const MapType: TWeatherMapType): IWeatherGraphic;
begin
  Result:= FMaps[MapType];
end;
{$ENDIF}

{ TWeatherMultiInfo }

constructor TWeatherMultiInfo.Create;
begin

end;

destructor TWeatherMultiInfo.Destroy;
begin

  inherited;
end;

function TWeatherMultiInfo.GetAlerts: IWeatherAlerts;
begin
  Result:= FAlerts;
end;

function TWeatherMultiInfo.GetConditions: IWeatherConditions;
begin
  Result:= FConditions;
end;

function TWeatherMultiInfo.GetForecastDaily: IWeatherForecast;
begin
  Result:= FForecastDaily;
end;

function TWeatherMultiInfo.GetForecastHourly: IWeatherForecast;
begin
  Result:= FForecastHourly;
end;

function TWeatherMultiInfo.GetForecastSummary: IWeatherForecast;
begin
  Result:= FForecastSummary;
end;

function TWeatherMultiInfo.GetMaps: IWeatherMaps;
begin
  Result:= FMaps;
end;

procedure TWeatherMultiInfo.SetAll(Con: TWeatherConditions; Alr: TWeatherAlerts;
  Fos, Foh, Fod: TWeatherForecast; Map: IWeatherMaps);
begin
  FConditions:= Con;
  FAlerts:= Alr;
  FForecastSummary:= Fos;
  FForecastHourly:= Foh;
  FForecastDaily:= Fod;
  FMaps:= Map;
end;

{ TWeatherServiceBase }

constructor TWeatherServiceBase.Create;
var
  LT: TWeatherLogoType;
begin
  FWeb:= TIdHTTP.Create(nil);
  FLocationType:= TJDWeatherLocationType.wlAutoIP;
  for LT:= Low(TWeatherLogoType) to High(TWeatherLogoType) do begin
    FLogos[LT]:= TWeatherGraphic.Create;
    FLogos[LT]._AddRef;
  end;
end;

destructor TWeatherServiceBase.Destroy;
var
  LT: TWeatherLogoType;
begin
  for LT:= Low(TWeatherLogoType) to High(TWeatherLogoType) do begin
    FLogos[LT]._Release;
  end;
  FreeAndNil(FWeb);
  inherited;
end;

function TWeatherServiceBase.GetLogo(const LT: TWeatherLogoType): IWeatherGraphic;
begin
  Result:= FLogos[LT];
end;

function TWeatherServiceBase.GetModule: HMODULE;
begin
  Result:= FModule;
end;

function TWeatherServiceBase.GetKey: WideString;
begin
  Result:= FKey;
end;

function TWeatherServiceBase.GetLocationDetail1: WideString;
begin
  Result:= FLocationDetail1;
end;

function TWeatherServiceBase.GetLocationDetail2: WideString;
begin
  Result:= FLocationDetail2;
end;

function TWeatherServiceBase.GetLocationType: TJDWeatherLocationType;
begin
  Result:= FLocationType;
end;

function TWeatherServiceBase.GetUnits: TWeatherUnits;
begin
  Result:= FUnits;
end;

procedure TWeatherServiceBase.SetKey(const Value: WideString);
begin
  FKey:= Value;
end;

procedure TWeatherServiceBase.SetLocationDetail1(const Value: WideString);
begin
  FLocationDetail1:= Value;
end;

procedure TWeatherServiceBase.SetLocationDetail2(const Value: WideString);
begin
  FLocationDetail2:= Value;
end;

procedure TWeatherServiceBase.SetLocationType(const Value: TJDWeatherLocationType);
begin
  FLocationType:= Value;
end;

procedure TWeatherServiceBase.SetLogo(const LT: TWeatherLogoType;
  const Value: IWeatherGraphic);
begin
  FLogos[LT].Base64:= Value.Base64;
end;

procedure TWeatherServiceBase.SetModule(const Value: HMODULE);
begin
  FModule:= Value;
end;

procedure TWeatherServiceBase.SetUnits(const Value: TWeatherUnits);
begin
  FUnits:= Value;
end;

end.
