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
  IWeatherProps = interface;
  IWeatherGraphic = interface;
  IWeatherLocation = interface;
  IWeatherForecast = interface;
  IWeatherAlertZone = interface;
  IWeatherAlertZones = interface;
  IWeatherStormVertex = interface;
  IWeatherStormVerticies = interface;
  IWeatherAlertStorm = interface;
  IWeatherAlert = interface;
  IWeatherAlerts = interface;
  IWeatherMaps = interface;
  IWeatherSupport = interface;
  IWeatherURLs = interface;
  IWeatherMultiInfo = interface;
  IWeatherServiceInfo = interface;
  IWeatherService = interface;
  IWeatherServices = interface;
  IWeatherMultiService = interface;
  IJDWeather = interface;

  TWeatherGraphic = class;



  TWeatherInfoType = (wiConditions, wiAlerts, wiForecastSummary,
    wiForecastHourly, wiForecastDaily, wiMaps, wiAlmanac, wiAstronomy,
    wiHurricane, wiHistory, wiPlanner, wiStation, wiLocation, wiGeoLookup,
    wiTide, wiRawTide, wiWebcams);
  TWeatherInfoTypes = set of TWeatherInfoType;

  TWeatherLogoType = (ltColor, ltColorInvert, ltColorWide, ltColorInvertWide,
    ltColorLeft, ltColorRight);
  TWeatherLogoTypes = set of TWeatherLogoType;

  TLogoArray = array[TWeatherLogoType] of IWeatherGraphic;

  TWeatherLocationType = (wlZip, wlCityState, wlCoords, wlAutoIP, wlCityCode,
    wlCountryCity, wlAirportCode, wlPWS);
  TWeatherLocationTypes = set of TWeatherLocationType;

  TWeatherUnits = (wuKelvin, wuImperial, wuMetric);
  TWeatherUnitsSet = set of TWeatherUnits;

  TWeatherForecastType = (ftSummary, ftHourly, ftDaily);
  TWeatherForecastTypes = set of TWeatherForecastType;

  TWeatherPropType = (wpIcon, wpCaption, wpDescription, wpDetails, wpURL,
    wpStation, wpPropsMin, wpPropsMax, wpTemp, wpTempMin, wpTempMax,
    wpFeelsLike, wpFeelsLikeSun, wpFeelsLikeShade, wpWindDir, wpWindSpeed,
    wpWindGust, wpWindChill, wpHeatIndex, wpPressure, wpPressureGround,
    wpPressureSea, wpHumidity, wpDewPoint, wpVisibility, wpSolarRad, wpUVIndex,
    wpCloudCover, wpPrecipAmt, wpRainAmt, wpSnowAmt, wpIceAmt, wpSleetAmt,
    wpFogAmt, wpStormAmt, wpPrecipPred, wpRainPred, wpSnowPred, wpIcePred,
    wpSleetPred, wpFogPred, wpStormPred, wpWetBulb, wpCeiling, wpSunrise,
    wpSunset, wpDaylight);
  TWeatherPropTypes = set of TWeatherPropType;

  TWeatherAlertType = (waNone, waHurricaneStat, waTornadoWarn, waTornadoWatch, waSevThundWarn,
    waSevThundWatch, waWinterAdv, waFloodWarn, waFloodWatch, waHighWind, waSevStat,
    waHeatAdv, waFogAdv, waSpecialStat, waFireAdv, waVolcanicStat, waHurricaneWarn,
    waRecordSet, waPublicRec, waPublicStat);
  TWeatherAlertTypes = set of TWeatherAlertType;

  TWeatherAlertProp = (apZones, apVerticies, apStorm, apType, apDescription,
    apExpires, apMessage, apPhenomena, apSignificance);
  TWeatherAlertProps = set of TWeatherAlertProp;

  TWeatherAlertPhenomena = (wpWind, wpHeat, wpSmallCraft);

  TWeatherMapType = (mpSatellite, mpRadar, mpSatelliteRadar, mpRadarClouds,
    mpClouds, mpTemp, mpTempChange, mpSnowCover, mpPrecip, mpAlerts, mpHeatIndex,
    mpDewPoint, mpWindChill, mpPressureSea, mpWind,
    mpAniSatellite, mpAniRadar, mpAniSatelliteRadar);
  TWeatherMapTypes = set of TWeatherMapType;

  TWeatherMapFormat = (wfJpg, wfPng, wfGif, wfTiff, wfBmp, wfFlash, wfHtml);
  TWeatherMapFormats = set of TWeatherMapFormat;

  TMapArray = array[TWeatherMapType] of TWeatherGraphic;

  TDayNight = (dnDay, dnNight);
  TCloudCode = (ccClear = 0, ccAlmostClear = 1, ccHalfCloudy = 2, ccBroken = 3,
    ccOvercast = 4, ccThinClouds = 5, ccFog = 6);
  TPrecipCode = (pcNone = 0, pcSlight = 1, pcShowers = 2, pcPrecip = 3, pcThunder = 4);
  TPrecipTypeCode = (ptRain = 0, ptSleet = 1, ptSnow = 2);

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







  TWeatherConditionsEvent = procedure(Sender: TObject; const Conditions: IWeatherProps) of object;

  TWeatherForecastEvent = procedure(Sender: TObject; const Forecast: IWeatherForecast) of object;

  TWeatherAlertEvent = procedure(Sender: TObject; const Alert: IWeatherAlerts) of object;

  TWeatherMapEvent = procedure(Sender: TObject; const Image: IWeatherMaps) of object;




  TCreateJDWeather = function(const LibDir: WideString): IJDWeather; stdcall;

  TCreateWeatherService = function: IWeatherService; stdcall;

  TGetServiceInfo = function: IWeatherServiceInfo; stdcall;










  //Common Interface Definitions

  IWeatherProps = interface
    function GetDateTime: TDateTime;
    function GetCaption: WideString;
    function GetCeiling: Double;
    function GetCloudCover: Double;
    function GetDaylight: Double;
    function GetDescription: WideString;
    function GetDetails: WideString;
    function GetDewPoint: Double;
    function GetFeelsLike: Double;
    function GetFeelsLikeShade: Double;
    function GetFeelsLikeSun: Double;
    function GetFogAmt: Double;
    function GetFogPred: Double;
    function GetHeatIndex: Double;
    function GetHumidity: Double;
    function GetIceAmt: Double;
    function GetIcePred: Double;
    function GetIcon: IWeatherGraphic;
    function GetPrecipAmt: Double;
    function GetPrecipPred: Double;
    function GetPressure: Double;
    function GetPressureGround: Double;
    function GetPressureSea: Double;
    function GetRainAmt: Double;
    function GetRainPred: Double;
    function GetSleetAmt: Double;
    function GetSleetProd: Double;
    function GetSnowAmt: Double;
    function GetSnowPred: Double;
    function GetSolarRad: Double;
    function GetStation: WideString;
    function GetStormAmt: Double;
    function GetStormPred: Double;
    function GetSunrise: TDateTime;
    function GetSunset: TDateTime;
    function GetTemp: Double;
    function GetTempMax: Double;
    function GetTempMin: Double;
    function GetURL: WideString;
    function GetUVIndex: Double;
    function GetVisibility: Double;
    function GetWetBulb: Double;
    function GetWindChill: Double;
    function GetWindDir: Double;
    function GetWindGusts: Double;
    function GetWindSpeed: Double;

    procedure AddTo(const AProps: IWeatherProps; const ASupport: TWeatherPropTypes);
    procedure CalcAverages;
    function Support: TWeatherPropTypes;
    function PropCount(const Prop: TWeatherPropType): Integer;

    property DateTime: TDateTime read GetDateTime;
    property Icon: IWeatherGraphic read GetIcon;
    property Caption: WideString read GetCaption;
    property Description: WideString read GetDescription;
    property Details: WideString read GetDetails;
    property URL: WideString read GetURL;
    property Station: WideString read GetStation;
    //property PropsMin: Boolean read GetPropsMin; //TODO
    //property PropsMax: Boolean read GetPropsMax; //TODO
    property Temp: Double read GetTemp;
    property TempMax: Double read GetTempMax;
    property TempMin: Double read GetTempMin;
    property FeelsLike: Double read GetFeelsLike;
    property FeelsLikeSun: Double read GetFeelsLikeSun;
    property FeelsLikeShade: Double read GetFeelsLikeShade;
    property WindDir: Double read GetWindDir;
    property WindSpeed: Double read GetWindSpeed;
    property WindGusts: Double read GetWindGusts;
    property WindChill: Double read GetWindChill;
    property HeatIndex: Double read GetHeatIndex;
    property Pressure: Double read GetPressure;
    property PressureGround: Double read GetPressureGround;
    property PressureSea: Double read GetPressureSea;
    property Humidity: Double read GetHumidity;
    property DewPoint: Double read GetDewPoint;
    property Visibility: Double read GetVisibility;
    property SolarRad: Double read GetSolarRad;
    property UVIndex: Double read GetUVIndex;
    property CloudCover: Double read GetCloudCover;
    property PrecipAmt: Double read GetPrecipAmt;
    property RainAmt: Double read GetRainAmt;
    property SnowAmt: Double read GetSnowAmt;
    property IceAmt: Double read GetIceAmt;
    property SleetAmt: Double read GetSleetAmt;
    property FogAmt: Double read GetFogAmt;
    property StormAmt: Double read GetStormAmt;
    property PrecipPred: Double read GetPrecipPred;
    property RainPred: Double read GetRainPred;
    property SnowPred: Double read GetSnowPred;
    property IcePred: Double read GetIcePred;
    property SleetPred: Double read GetSleetProd;
    property FogPred: Double read GetFogPred;
    property StormPred: Double read GetStormPred;
    property WetBulb: Double read GetWetBulb;
    property Ceiling: Double read GetCeiling;
    property Sunrise: TDateTime read GetSunrise;
    property Sunset: TDateTime read GetSunset;
    property Daylight: Double read GetDaylight;
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

  IWeatherForecast = interface
    function GetLocation: IWeatherLocation;
    function GetItem(const Index: Integer): IWeatherProps;
    function Count: Integer;
    function MinTemp: Single;
    function MaxTemp: Single;
    property Location: IWeatherLocation read GetLocation;
    property Items[const Index: Integer]: IWeatherProps read GetItem; default;
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
    function GetMap(const MapType: TWeatherMapType): IWeatherGraphic;

    property Maps[const MapType: TWeatherMapType]: IWeatherGraphic read GetMap;
  end;








  //Service Interface Definitions

  IWeatherSupport = interface
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

  IWeatherURLs = interface
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

  IWeatherMultiInfo = interface
    function GetConditions: IWeatherProps;
    function GetAlerts: IWeatherAlerts;
    function GetForecastSummary: IWeatherForecast;
    function GetForecastHourly: IWeatherForecast;
    function GetForecastDaily: IWeatherForecast;
    function GetMaps: IWeatherMaps;

    property Conditions: IWeatherProps read GetConditions;
    property Alerts: IWeatherAlerts read GetAlerts;
    property ForecastSummary: IWeatherForecast read GetForecastSummary;
    property ForecastHourly: IWeatherForecast read GetForecastHourly;
    property ForecastDaily: IWeatherForecast read GetForecastDaily;
    property Maps: IWeatherMaps read GetMaps;
  end;

  IWeatherServiceInfo = interface
    function GetCaption: WideString;
    function GetName: WideString;
    function GetUID: WideString;
    function GetURLs: IWeatherURLs;
    function GetSupport: IWeatherSupport;

    function GetLogo(const LT: TWeatherLogoType): IWeatherGraphic;

    property Caption: WideString read GetCaption;
    property Name: WideString read GetName;
    property UID: WideString read GetUID;
    property Support: IWeatherSupport read GetSupport;
    property URLs: IWeatherURLs read GetURLs;
  end;

  IWeatherService = interface
    function GetKey: WideString;
    procedure SetKey(const Value: WideString);
    function GetLocationType: TWeatherLocationType;
    procedure SetLocationType(const Value: TWeatherLocationType);
    function GetLocationDetail1: WideString;
    procedure SetLocationDetail1(const Value: WideString);
    function GetLocationDetail2: WideString;
    procedure SetLocationDetail2(const Value: WideString);
    function GetUnits: TWeatherUnits;
    procedure SetUnits(const Value: TWeatherUnits);

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
    property Key: WideString read GetKey write SetKey;
    property LocationType: TWeatherLocationType read GetLocationType write SetLocationType;
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

  IWeatherMultiService = interface
    function GetItem(const Index: Integer): IWeatherService;

    function GetCombinedConditions: IWeatherProps;
    function GetCombinedAlerts: IWeatherAlerts;
    function GetCombinedForecastSummary: IWeatherForecast;
    function GetCombinedForecastHourly: IWeatherForecast;
    function GetCombinedForecastDaily: IWeatherForecast;

    procedure Add(const Svc: IWeatherService);
    procedure Delete(const Index: Integer);
    procedure Clear;
    function Count: Integer;
    property Items[const Index: Integer]: IWeatherService read GetItem; default;
  end;

  TWeatherMultiService = class(TInterfacedObject, IWeatherMultiService)
  private
    FItems: TList<IWeatherService>;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetItem(const Index: Integer): IWeatherService;

    function GetCombinedConditions: IWeatherProps;
    function GetCombinedAlerts: IWeatherAlerts;
    function GetCombinedForecastSummary: IWeatherForecast;
    function GetCombinedForecastHourly: IWeatherForecast;
    function GetCombinedForecastDaily: IWeatherForecast;

    procedure Add(const Svc: IWeatherService);
    procedure Delete(const Index: Integer);
    procedure Clear;
    function Count: Integer;
    property Items[const Index: Integer]: IWeatherService read GetItem; default;
  end;

  IJDWeather = interface
    function GetServices: IWeatherServices;
    procedure SetLocationType(const Value: TWeatherLocationType);
    function GetLocationDetail1: WideString;
    function GetLocationType: TWeatherLocationType;
    procedure SetLocationDetail1(const Value: WideString);
    function GetLocationDetail2: WideString;
    procedure SetLocationDetail2(const Value: WideString);
    function GetUnits: TWeatherUnits;
    procedure SetUnits(const Value: TWeatherUnits);

    property Services: IWeatherServices read GetServices;
    property LocationType: TWeatherLocationType read GetLocationType write SetLocationType;
    property LocationDetail1: WideString read GetLocationDetail1 write SetLocationDetail1;
    property LocationDetail2: WideString read GetLocationDetail2 write SetLocationDetail2;
    property Units: TWeatherUnits read GetUnits write SetUnits;
  end;

























  { Interface Implementation Objects }

  TWeatherProps = class(TInterfacedObject, IWeatherProps)
  public
    FSupport: TWeatherPropTypes;
    FCounts: Array[TWeatherPropType] of Integer;
    FDateTime: TDateTime;
    FIcon: TWeatherGraphic;
    FCaption: WideString;
    FDescription: WideString;
    FDetails: WideString;
    FURL: WideString;
    FStation: WideString;
    FTemp: Double;
    FTempMax: Double;
    FTempMin: Double;
    FFeelsLike: Double;
    FFeelsLikeSun: Double;
    FFeelsLikeShade: Double;
    FWindDir: Double;
    FWindSpeed: Double;
    FWindGusts: Double;
    FWindChill: Double;
    FHeatIndex: Double;
    FPressure: Double;
    FPressureGround: Double;
    FPressureSea: Double;
    FHumidity: Double;
    FDewPoint: Double;
    FVisibility: Double;
    FSolarRad: Double;
    FUVIndex: Double;
    FCloudCover: Double;
    FPrecipAmt: Double;
    FRainAmt: Double;
    FSnowAmt: Double;
    FIceAmt: Double;
    FSleetAmt: Double;
    FFogAmt: Double;
    FStormAmt: Double;
    FPrecipPred: Double;
    FRainPred: Double;
    FSnowPred: Double;
    FIcePred: Double;
    FSleetPred: Double;
    FFogPred: Double;
    FStormPred: Double;
    FWetBulb: Double;
    FCeiling: Double;
    FSunrise: TDateTime;
    FSunset: TDateTime;
    FDaylight: Double;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetDateTime: TDateTime;
    function GetCaption: WideString;
    function GetCeiling: Double;
    function GetCloudCover: Double;
    function GetDaylight: Double;
    function GetDescription: WideString;
    function GetDetails: WideString;
    function GetDewPoint: Double;
    function GetFeelsLike: Double;
    function GetFeelsLikeShade: Double;
    function GetFeelsLikeSun: Double;
    function GetFogAmt: Double;
    function GetFogPred: Double;
    function GetHeatIndex: Double;
    function GetHumidity: Double;
    function GetIceAmt: Double;
    function GetIcePred: Double;
    function GetIcon: IWeatherGraphic;
    function GetPrecipAmt: Double;
    function GetPrecipPred: Double;
    function GetPressure: Double;
    function GetPressureGround: Double;
    function GetPressureSea: Double;
    function GetRainAmt: Double;
    function GetRainPred: Double;
    function GetSleetAmt: Double;
    function GetSleetProd: Double;
    function GetSnowAmt: Double;
    function GetSnowPred: Double;
    function GetSolarRad: Double;
    function GetStation: WideString;
    function GetStormAmt: Double;
    function GetStormPred: Double;
    function GetSunrise: TDateTime;
    function GetSunset: TDateTime;
    function GetTemp: Double;
    function GetTempMax: Double;
    function GetTempMin: Double;
    function GetURL: WideString;
    function GetUVIndex: Double;
    function GetVisibility: Double;
    function GetWetBulb: Double;
    function GetWindChill: Double;
    function GetWindDir: Double;
    function GetWindGusts: Double;
    function GetWindSpeed: Double;

    procedure AddTo(const AProps: IWeatherProps; const ASupport: TWeatherPropTypes);
    procedure CalcAverages;
    function Support: TWeatherPropTypes;
    function PropCount(const Prop: TWeatherPropType): Integer;

    property DateTime: TDateTime read GetDateTime;
    property Icon: IWeatherGraphic read GetIcon;
    property Caption: WideString read GetCaption;
    property Description: WideString read GetDescription;
    property Details: WideString read GetDetails;
    property URL: WideString read GetURL;
    property Station: WideString read GetStation;
    //property PropsMin: Boolean read GetPropsMin; //TODO
    //property PropsMax: Boolean read GetPropsMax; //TODO
    property Temp: Double read GetTemp;
    property TempMax: Double read GetTempMax;
    property TempMin: Double read GetTempMin;
    property FeelsLike: Double read GetFeelsLike;
    property FeelsLikeSun: Double read GetFeelsLikeSun;
    property FeelsLikeShade: Double read GetFeelsLikeShade;
    property WindDir: Double read GetWindDir;
    property WindSpeed: Double read GetWindSpeed;
    property WindGusts: Double read GetWindGusts;
    property WindChill: Double read GetWindChill;
    property HeatIndex: Double read GetHeatIndex;
    property Pressure: Double read GetPressure;
    property PressureGround: Double read GetPressureGround;
    property PressureSea: Double read GetPressureSea;
    property Humidity: Double read GetHumidity;
    property DewPoint: Double read GetDewPoint;
    property Visibility: Double read GetVisibility;
    property SolarRad: Double read GetSolarRad;
    property UVIndex: Double read GetUVIndex;
    property CloudCover: Double read GetCloudCover;
    property PrecipAmt: Double read GetPrecipAmt;
    property RainAmt: Double read GetRainAmt;
    property SnowAmt: Double read GetSnowAmt;
    property IceAmt: Double read GetIceAmt;
    property SleetAmt: Double read GetSleetAmt;
    property FogAmt: Double read GetFogAmt;
    property StormAmt: Double read GetStormAmt;
    property PrecipPred: Double read GetPrecipPred;
    property RainPred: Double read GetRainPred;
    property SnowPred: Double read GetSnowPred;
    property IcePred: Double read GetIcePred;
    property SleetPred: Double read GetSleetProd;
    property FogPred: Double read GetFogPred;
    property StormPred: Double read GetStormPred;
    property WetBulb: Double read GetWetBulb;
    property Ceiling: Double read GetCeiling;
    property Sunrise: TDateTime read GetSunrise;
    property Sunset: TDateTime read GetSunset;
    property Daylight: Double read GetDaylight;
  end;

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

  TWeatherForecast = class(TInterfacedObject, IWeatherForecast)
  public
    FItems: TList<IWeatherProps>;
    FLocation: TWeatherLocation;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetLocation: IWeatherLocation;
    function GetItem(const Index: Integer): IWeatherProps;
    function Count: Integer;
    function MinTemp: Single;
    function MaxTemp: Single;
    property Location: IWeatherLocation read GetLocation;
    property Items[const Index: Integer]: IWeatherProps read GetItem; default;
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
    function GetMap(const MapType: TWeatherMapType): IWeatherGraphic;

    property Maps[const MapType: TWeatherMapType]: IWeatherGraphic read GetMap;
  end;

  TWeatherMultiInfo = class(TInterfacedObject, IWeatherMultiInfo)
  private
    FConditions: IWeatherProps;
    FAlerts: TWeatherAlerts;
    FForecastSummary: TWeatherForecast;
    FForecastHourly: TWeatherForecast;
    FForecastDaily: TWeatherForecast;
    FMaps: IWeatherMaps;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetAll(Con: IWeatherProps; Alr: TWeatherAlerts;
      Fos, Foh, Fod: TWeatherForecast; Map: IWeatherMaps);
  public
    function GetConditions: IWeatherProps;
    function GetAlerts: IWeatherAlerts;
    function GetForecastSummary: IWeatherForecast;
    function GetForecastHourly: IWeatherForecast;
    function GetForecastDaily: IWeatherForecast;
    function GetMaps: IWeatherMaps;

    property Conditions: IWeatherProps read GetConditions;
    property Alerts: IWeatherAlerts read GetAlerts;
    property ForecastSummary: IWeatherForecast read GetForecastSummary;
    property ForecastHourly: IWeatherForecast read GetForecastHourly;
    property ForecastDaily: IWeatherForecast read GetForecastDaily;
    property Maps: IWeatherMaps read GetMaps;
  end;

  TWeatherServiceBase = class(TInterfacedObject)
  private
    //FModule: HMODULE;
    FKey: WideString;
    FWeb: TIdHTTP;
    FLocationType: TWeatherLocationType;
    FLocationDetail1: WideString;
    FLocationDetail2: WideString;
    FUnits: TWeatherUnits;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Web: TIdHTTP read FWeb;
  public
    //function GetModule: HMODULE;
    //procedure SetModule(const Value: HMODULE);
    function GetKey: WideString;
    procedure SetKey(const Value: WideString);
    function GetLocationType: TWeatherLocationType;
    procedure SetLocationType(const Value: TWeatherLocationType);
    function GetLocationDetail1: WideString;
    procedure SetLocationDetail1(const Value: WideString);
    function GetLocationDetail2: WideString;
    procedure SetLocationDetail2(const Value: WideString);
    function GetUnits: TWeatherUnits;
    procedure SetUnits(const Value: TWeatherUnits);


    //property Module: HMODULE read GetModule write SetModule;
    property Key: WideString read GetKey write SetKey;
    property LocationType: TWeatherLocationType read GetLocationType write SetLocationType;
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





function ResourceExists(const Name, ResType: String): Boolean;

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
//function WeatherForecastPropToStr(const Value: TWeatherForecastProp): String;
function WeatherForecastTypeToStr(const Value: TWeatherForecastType): String;
function WeatherPropToStr(const Value: TWeatherPropType): String;
function WeatherAlertPropToStr(const Value: TWeatherAlertProp): String;
function WeatherAlertTypeToStr(const Value: TWeatherAlertType): String;
//function WeatherConditionPropToStr(const Value: TWeatherConditionsProp): String;
function WeatherInfoTypeToStr(const Value: TWeatherInfoType): String;
function WeatherLocationTypeToStr(const Value: TWeatherLocationType): String;

implementation

function ResourceExists(const Name, ResType: String): Boolean;
begin
  Result:= (FindResource(hInstance, PChar(Name), PChar(Name)) <> 0);
end;

function WeatherLocationTypeToStr(const Value: TWeatherLocationType): String;
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
    wiLocation:         Result:= 'Location Info';
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
    wiGeoLookup:        Result:= 'Geo Lookup';
    wiTide:             Result:= 'Tide';
    wiRawTide:          Result:= 'Raw Tide';
    wiWebcams:          Result:= 'Webcams';
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

function WeatherPropToStr(const Value: TWeatherPropType): String;
begin
  case Value of
    wpIcon:           Result:= 'Weather Icon';
    wpCaption:        Result:= 'Caption';
    wpDescription:    Result:= 'Description';
    wpDetails:        Result:= 'Details';
    wpURL:            Result:= 'URL';
    wpStation:        Result:= 'Station';
    wpPropsMin:       Result:= 'Min of All Props';
    wpPropsMax:       Result:= 'Max of All Props';
    wpTemp:           Result:= 'Temperature';
    wpTempMin:        Result:= 'Temp Min';
    wpTempMax:        Result:= 'Temp Max';
    wpFeelsLike:      Result:= 'Feels Like';
    wpFeelsLikeSun:   Result:= 'Feels Like Sun';
    wpFeelsLikeShade: Result:= 'Feels Like Shade';
    wpWindDir:        Result:= 'Wind Direction';
    wpWindSpeed:      Result:= 'Wind Speed';
    wpWindGust:       Result:= 'Wind Gusts';
    wpWindChill:      Result:= 'Wind Chill';
    wpHeatIndex:      Result:= 'Heat Index';
    wpPressure:       Result:= 'Air Pressure';
    wpPressureGround: Result:= 'Pressure at Ground';
    wpPressureSea:    Result:= 'Pressure at Sea';
    wpHumidity:       Result:= 'Humidity';
    wpDewPoint:       Result:= 'Dew Point';
    wpVisibility:     Result:= 'Visibility';
    wpSolarRad:       Result:= 'Solar Radiation';
    wpUVIndex:        Result:= 'UV Index';
    wpCloudCover:     Result:= 'Cloud Cover';
    wpPrecipAmt:      Result:= 'Precipitation Amt';
    wpRainAmt:        Result:= 'Rain Amount';
    wpSnowAmt:        Result:= 'Snow Amount';
    wpIceAmt:         Result:= 'Ice Amount';
    wpSleetAmt:       Result:= 'Sleet Amount';
    wpFogAmt:         Result:= 'Fog Amount';
    wpStormAmt:       Result:= 'Storm Amount';
    wpPrecipPred:     Result:= 'Chance of Precip';
    wpRainPred:       Result:= 'Chance of Rain';
    wpSnowPred:       Result:= 'Chance of Snow';
    wpIcePred:        Result:= 'Chance of Ice';
    wpSleetPred:      Result:= 'Chance of Sleet';
    wpFogPred:        Result:= 'Chance of Fog';
    wpStormPred:      Result:= 'Chance of Storm';
    wpWetBulb:        Result:= 'Wet Bulb';
    wpCeiling:        Result:= 'Ceiling';
    wpSunrise:        Result:= 'Sunrise';
    wpSunset:         Result:= 'Sunset';
    wpDaylight:       Result:= 'Daylight Amount';
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
  if Assigned(FStream) then begin
    Result:= FStream.DataString;
  end else begin
    Result:= '';
  end;
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

{ TWeatherForecast }

constructor TWeatherForecast.Create;
begin
  FItems:= TList<IWeatherProps>.Create;
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

function TWeatherForecast.GetItem(const Index: Integer): IWeatherProps;
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
{$ELSE}
function TWeatherMaps.GetMap(const MapType: TWeatherMapType): IWeatherGraphic;
{$ENDIF}
begin
  Result:= FMaps[MapType];
end;

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

function TWeatherMultiInfo.GetConditions: IWeatherProps;
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

procedure TWeatherMultiInfo.SetAll(Con: IWeatherProps; Alr: TWeatherAlerts;
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
begin
  FWeb:= TIdHTTP.Create(nil);
  FLocationType:= TWeatherLocationType.wlAutoIP;
end;

destructor TWeatherServiceBase.Destroy;
begin
  FreeAndNil(FWeb);
  inherited;
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

function TWeatherServiceBase.GetLocationType: TWeatherLocationType;
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

procedure TWeatherServiceBase.SetLocationType(const Value: TWeatherLocationType);
begin
  FLocationType:= Value;
end;

procedure TWeatherServiceBase.SetUnits(const Value: TWeatherUnits);
begin
  FUnits:= Value;
end;

{ TWeatherMultiService }

constructor TWeatherMultiService.Create;
begin
  FItems:= TList<IWeatherService>.Create;

end;

destructor TWeatherMultiService.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

procedure TWeatherMultiService.Add(const Svc: IWeatherService);
begin
  FItems.Add(Svc);
  Svc._AddRef;
end;

procedure TWeatherMultiService.Clear;
begin
  while Count > 0 do
    Delete(0);
end;

function TWeatherMultiService.Count: Integer;
begin
  Result:= FItems.Count;
end;

procedure TWeatherMultiService.Delete(const Index: Integer);
begin
  FItems[Index]._Release;
  FItems.Delete(Index);
end;

function TWeatherMultiService.GetItem(const Index: Integer): IWeatherService;
begin
  Result:= FItems[Index];
end;

function TWeatherMultiService.GetCombinedConditions: IWeatherProps;
var
  R: TWeatherProps;
  T: IWeatherProps;
  X: Integer;
  P: TWeatherPropType;
  S: IWeatherService;
  U: Array[TWeatherPropType] of Integer;
begin
  //Merge multiple services into one data set, averaging each property
  R:= TWeatherProps.Create;
  try
    for P := Low(U) to High(U) do begin
      U[P]:= 0;
    end;
    if FItems.Count > 0 then begin
      for X := 0 to FItems.Count-1 do begin
        S:= FItems[X];
        T:= S.GetConditions;

        R.AddTo(T, S.Info.Support.SupportedConditionProps);

        //TODO: Is there a better way to do these unmergable ones?
        R.FDateTime:= T.DateTime;
        R.FIcon.Base64:= T.Icon.Base64;
        R.FCaption:= T.Caption;
        R.FDescription:= T.Description;
        R.FDetails:= T.Details;
        R.FURL:= T.URL;
        R.FStation:= T.Station;

      end;

      R.CalcAverages;
    end;
  finally
    Result:= R;
  end;
end;

function TWeatherMultiService.GetCombinedForecastDaily: IWeatherForecast;
var
  R: TWeatherForecast;
begin
  R:= TWeatherForecast.Create;
  try
    //TODO: How to merge forecasts???

  finally
    Result:= R;
  end;
end;

function TWeatherMultiService.GetCombinedForecastHourly: IWeatherForecast;
var
  R: TWeatherForecast;
begin
  R:= TWeatherForecast.Create;
  try
    //TODO: How to merge forecasts???

  finally
    Result:= R;
  end;
end;

function TWeatherMultiService.GetCombinedForecastSummary: IWeatherForecast;
var
  R: TWeatherForecast;
begin
  R:= TWeatherForecast.Create;
  try
    //TODO: How to merge forecasts???

  finally
    Result:= R;
  end;
end;

function TWeatherMultiService.GetCombinedAlerts: IWeatherAlerts;
var
  R: TWeatherAlerts;
begin
  R:= TWeatherAlerts.Create;
  try
    //TODO: How to merge alerts???

  finally
    Result:= R;
  end;
end;

{ TWeatherProps }

constructor TWeatherProps.Create;
var
  X: TWeatherPropType;
begin
  FIcon:= TWeatherGraphic.Create;
  FIcon._AddRef;
  FCaption:= '';
  FDescription:= '';
  FDetails:= '';
  FTemp:= 0;
  FHumidity:= 0;
  FPressure:= 0;
  FWindSpeed:= 0;
  FWindDir:= 0;
  FVisibility:= 0;
  FDewPoint:= 0;

  for X := Low(TWeatherPropType) to High(TWeatherPropType) do begin
    FCounts[X]:= 0;
  end;
end;

destructor TWeatherProps.Destroy;
begin
  FIcon._Release;
  FIcon:= nil;
  inherited;
end;

function TWeatherProps.GetCaption: WideString;
begin
  Result:= FCaption;
end;

function TWeatherProps.GetCeiling: Double;
begin
  Result:= FCeiling;
end;

function TWeatherProps.GetCloudCover: Double;
begin
  Result:= FCloudCover;
end;

function TWeatherProps.GetDateTime: TDateTime;
begin
  Result:= FDateTime;
end;

function TWeatherProps.GetDaylight: Double;
begin
  Result:= FDaylight;
end;

function TWeatherProps.GetDescription: WideString;
begin
  Result:= FDescription;
end;

function TWeatherProps.GetDetails: WideString;
begin
  Result:= FDetails;
end;

function TWeatherProps.GetDewPoint: Double;
begin
  Result:= FDewPoint;
end;

function TWeatherProps.GetFeelsLike: Double;
begin
  Result:= FFeelsLike;
end;

function TWeatherProps.GetFeelsLikeShade: Double;
begin
  Result:= FFeelsLikeShade;
end;

function TWeatherProps.GetFeelsLikeSun: Double;
begin
  Result:= FFeelsLikeSun;
end;

function TWeatherProps.GetFogAmt: Double;
begin
  Result:= FFogAmt;
end;

function TWeatherProps.GetFogPred: Double;
begin
  Result:= FFogPred;
end;

function TWeatherProps.GetHeatIndex: Double;
begin
  Result:= FHeatIndex;
end;

function TWeatherProps.GetHumidity: Double;
begin
  Result:= FHumidity;
end;

function TWeatherProps.GetIceAmt: Double;
begin
  Result:= FIceAmt;
end;

function TWeatherProps.GetIcePred: Double;
begin
  Result:= FIcePred;
end;

function TWeatherProps.GetIcon: IWeatherGraphic;
begin
  Result:= FIcon;
end;

function TWeatherProps.GetPrecipAmt: Double;
begin
  Result:= FPrecipAmt;
end;

function TWeatherProps.GetPrecipPred: Double;
begin
  Result:= FPrecipPred;
end;

function TWeatherProps.GetPressure: Double;
begin
  Result:= FPressure;
end;

function TWeatherProps.GetPressureGround: Double;
begin
  Result:= FPressureGround;
end;

function TWeatherProps.GetPressureSea: Double;
begin
  Result:= FPressureSea;
end;

function TWeatherProps.GetRainAmt: Double;
begin
  Result:= FRainAmt;
end;

function TWeatherProps.GetRainPred: Double;
begin
  Result:= FRainPred;
end;

function TWeatherProps.GetSleetAmt: Double;
begin
  Result:= FSleetAmt;
end;

function TWeatherProps.GetSleetProd: Double;
begin
  Result:= FSleetPred;
end;

function TWeatherProps.GetSnowAmt: Double;
begin
  Result:= FSnowAmt;
end;

function TWeatherProps.GetSnowPred: Double;
begin
  Result:= FSnowPred;
end;

function TWeatherProps.GetSolarRad: Double;
begin
  Result:= FSolarRad;
end;

function TWeatherProps.GetStation: WideString;
begin
  Result:= FStation;
end;

function TWeatherProps.GetStormAmt: Double;
begin
  Result:= FStormAmt;
end;

function TWeatherProps.GetStormPred: Double;
begin
  Result:= FStormPred;
end;

function TWeatherProps.GetSunrise: TDateTime;
begin
  Result:= FSunrise;
end;

function TWeatherProps.GetSunset: TDateTime;
begin
  Result:= FSunset;
end;

function TWeatherProps.GetTemp: Double;
begin
  Result:= FTemp;
end;

function TWeatherProps.GetTempMax: Double;
begin
  Result:= FTempMax;
end;

function TWeatherProps.GetTempMin: Double;
begin
  Result:= FTempMin;
end;

function TWeatherProps.GetURL: WideString;
begin
  Result:= FURL;
end;

function TWeatherProps.GetUVIndex: Double;
begin
  Result:= FUVIndex;
end;

function TWeatherProps.GetVisibility: Double;
begin
  Result:= FVisibility;
end;

function TWeatherProps.GetWetBulb: Double;
begin
  Result:= FWetBulb;
end;

function TWeatherProps.GetWindChill: Double;
begin
  Result:= FWindChill;
end;

function TWeatherProps.GetWindDir: Double;
begin
  Result:= FWindDir;
end;

function TWeatherProps.GetWindGusts: Double;
begin
  Result:= FWindGusts;
end;

function TWeatherProps.GetWindSpeed: Double;
begin
  Result:= FWindSpeed;
end;

function TWeatherProps.PropCount(const Prop: TWeatherPropType): Integer;
begin
  Result:= FCounts[Prop];
end;

function TWeatherProps.Support: TWeatherPropTypes;
begin
  Result:= FSupport;
end;

procedure TWeatherProps.AddTo(const AProps: IWeatherProps; const ASupport: TWeatherPropTypes);
var
  T: TWeatherPropType;
begin
  //Add values of specified props to this prop
  for T := Low(TWeatherPropType) to High(TWeatherPropType) do begin
    if T in ASupport then begin
      FCounts[T]:= FCounts[T] + 1;
    end;
  end;
  FSupport:= FSupport + ASupport;
  FTemp:= FTemp + AProps.Temp;
  FTempMin:= FTempMin + AProps.TempMin;
  FTempMax:= FTempMax + AProps.TempMax;
  FFeelsLike:= FFeelsLike + AProps.FeelsLike;
  FFeelsLikeSun:= FFeelsLikeSun + AProps.FeelsLikeSun;
  FFeelsLikeShade:= FFeelsLikeShade + AProps.FeelsLikeShade;
  FWindDir:= FWindDir + AProps.WindDir;
  FWindSpeed:= FWindSpeed + AProps.WindSpeed;
  FWindGusts:= FWindGusts + AProps.WindGusts;
  FWindChill:= FWindChill + AProps.WindChill;
  FHeatIndex:= FHeatIndex + AProps.HeatIndex;
  FPressure:= FPressure + AProps.Pressure;
  FPressureGround:= FPressureGround + AProps.PressureGround;
  FPressureSea:= FPressureSea + AProps.PressureSea;
  FHumidity:= FHumidity + AProps.Humidity;
  FDewPoint:= FDewPoint + AProps.DewPoint;
  FVisibility:= FVisibility + AProps.Visibility;
  FSolarRad:= FSolarRad + AProps.SolarRad;
  FUVIndex:= FUVIndex + AProps.UVIndex;
  FCloudCover:= FCloudCover + AProps.CloudCover;
  FPrecipAmt:= FPrecipAmt + AProps.PrecipAmt;
  FRainAmt:= FRainAmt + AProps.RainAmt;
  FSnowAmt:= FSnowAmt + AProps.SnowAmt;
  FIceAmt:= FIceAmt + AProps.IceAmt;
  FSleetAmt:= FSleetAmt + AProps.SleetAmt;
  FFogAmt:= FFogAmt + AProps.FogAmt;
  FStormAmt:= FStormAmt + AProps.StormAmt;
  FPrecipPred:= FPrecipPred + AProps.PrecipPred;
  FRainPred:= FRainPred + AProps.RainPred;
  FSnowPred:= FSnowPred + AProps.SnowPred;
  FIcePred:= FIcePred + AProps.IceAmt;
  FSleetPred:= FSleetPred + AProps.SleetPred;
  FFogPred:= FFogPred + AProps.FogPred;
  FStormPred:= FStormPred + AProps.StormPred;
  FWetBulb:= FWetBulb + AProps.WetBulb;
  FCeiling:= FCeiling + AProps.Ceiling;
  FSunrise:= FSunrise + AProps.Sunrise;
  FSunset:= FSunset + AProps.Sunset;
  FDaylight:= FDaylight + AProps.Daylight;
end;

procedure TWeatherProps.CalcAverages;
  procedure Chk(const T: TWeatherPropType; var V: Double);
  begin
    if FCounts[T] > 1 then begin
      V:= V / FCounts[T];
      FCounts[T]:= 1;
    end;
  end;
begin
  //Calculate all averages based on totals
  Chk(wpTemp, FTemp);
  Chk(wpTempMin, FTempMin);
  Chk(wpTempMax, FTempMax);
  Chk(wpFeelsLike, FFeelsLike);
  Chk(wpFeelsLikeSun, FFeelsLikeSun);
  Chk(wpFeelsLikeShade, FFeelsLikeShade);
  Chk(wpWindDir, FWindDir);
  Chk(wpWindSpeed, FWindSpeed);
  Chk(wpWindGust, FWindGusts);
  Chk(wpWindChill, FWindChill);
  Chk(wpHeatIndex, FHeatIndex);
  Chk(wpPressure, FPressure);
  Chk(wpPressureGround, FPressureGround);
  Chk(wpPressureSea, FPressureSea);
  Chk(wpHumidity, FHumidity);
  Chk(wpDewPoint, FDewPoint);
  Chk(wpVisibility, FVisibility);
  Chk(wpSolarRad, FSolarRad);
  Chk(wpUVIndex, FUVIndex);
  Chk(wpCloudCover, FCloudCover);
  Chk(wpPrecipAmt, FPrecipAmt);
  Chk(wpRainAmt, FRainAmt);
  Chk(wpSnowAmt, FSnowAmt);
  Chk(wpIceAmt, FIceAmt);
  Chk(wpSleetAmt, FSleetAmt);
  Chk(wpFogAmt, FFogAmt);
  Chk(wpStormAmt, FStormAmt);
  Chk(wpPrecipPred, FPrecipPred);
  Chk(wpRainPred, FRainPred);
  Chk(wpSnowPred, FSnowPred);
  Chk(wpIcePred, FIcePred);
  Chk(wpSleetPred, FSleetPred);
  Chk(wpFogPred, FFogPred);
  Chk(wpStormPred, FStormPred);
  Chk(wpWetBulb, FWetBulb);
  Chk(wpCeiling, FCeiling);
  if FCounts[wpSunrise] > 1 then begin
    FSunrise:= FSunrise / FCounts[wpSunrise];
    FCounts[wpSunrise]:= 1;
  end;
  if FCounts[wpSunset] > 1 then begin
    FSunset:= FSunset / FCounts[wpSunset];
    FCounts[wpSunset]:= 1;
  end;
  Chk(wpDaylight, FDaylight);
end;

end.
