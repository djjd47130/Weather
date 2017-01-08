unit JD.Weather;

(*
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓

  ▓▓▓▓▓▓▓▓    ▓▓▓▓▓▓▓▓    ▓▓▓▓▓▓    ▓▓▓▓▓▓          ▓▓▓      ▓▓▓  ▓▓▓▓▓▓▓▓  ▓▓▓
  ▓▓▓    ▓▓▓  ▓▓▓       ▓▓▓▓  ▓▓▓▓  ▓▓▓  ▓▓▓        ▓▓▓▓▓  ▓▓▓▓▓  ▓▓▓       ▓▓▓
  ▓▓▓    ▓▓▓  ▓▓▓       ▓▓▓    ▓▓▓  ▓▓▓   ▓▓▓       ▓▓▓  ▓▓  ▓▓▓  ▓▓▓       ▓▓▓
  ▓▓▓▓▓▓▓▓    ▓▓▓▓▓▓▓   ▓▓▓▓▓▓▓▓▓▓  ▓▓▓   ▓▓▓       ▓▓▓  ▓▓  ▓▓▓  ▓▓▓▓▓▓▓   ▓▓▓
  ▓▓▓   ▓▓▓   ▓▓▓       ▓▓▓    ▓▓▓  ▓▓▓   ▓▓▓       ▓▓▓      ▓▓▓  ▓▓▓       ▓▓▓
  ▓▓▓    ▓▓▓  ▓▓▓       ▓▓▓    ▓▓▓  ▓▓▓  ▓▓▓        ▓▓▓      ▓▓▓  ▓▓▓
  ▓▓▓    ▓▓▓  ▓▓▓▓▓▓▓▓  ▓▓▓    ▓▓▓  ▓▓▓▓▓▓          ▓▓▓      ▓▓▓  ▓▓▓▓▓▓▓▓  ▓▓▓

▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓

  JD Weather
  by Jerry Dodge

  NOTE: This unit is currently in extensive restructure

  Component: TJDWeather
  - Encapsulates entire API wrapper system to pull weather data from various
    different weather APIs.
  - Adapts a standard structure to multiple different weather services
  - Dedicated thread to perform periodic checks on a given interval
  - Events triggered upon changes on conditions, forecast, alerts, and maps

▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓

  How to use:
  - Subscribe to one of the supported services
  - Acquire an API Key which authenticates your account
  - Select the service by assigning TJDWeather.Service
  - Enter your API Key by assigning TJDWeather.Key
  - Specify the frequency of each different weather information type
      NOTE: The numbers are seconds between calls
      IMPORTANT: Depending on which service you choose, your account will be likely
      limited to a certain number of requests in a given day. Therefore, it is
      very important to adjust these frequency properties to correspond with
      your particular account's capabilities. Sometimes, this may mean
      ten to twenty minutes between checks for weather, if your account
      has a low limit, or if you use the app in multiple places.
  - Select your desired location by assigning TJDWeather.LocationType
    - wlAutoIP: Automatically detects your location based on your IP Address
    - wlCityState: Assign City to "LocationDetail1" and State to "LocationDetail2"
    - wlZip: Assign Zip Code to "LocationDetail1"
    - wlCoords: Assign Longitude to "LocationDetail1" and Latitude to "LocationDetail2"
        NOTE: Format of each property must be with numeric digits such as:
        45.9764
        -15.9724
  - Assign event handlers to the desired weather information
      NOTE: Weather information is actually provided when these events are fired.
      You are responsible to acquire a copy of the corresponding weather interface
      from the event handler's parameters and store your own reference.
      These interfaces are by default reference-counted.

▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓

  SUPPORTED WEATHER SERVICES:

  - Weather Underground
    - https://www.wunderground.com/weather/api/d/docs?d=index
    - Implemented in JD.Weather.WUnderground.pas
    - Location by Zip, City/State, IP, Coords
    - Current Conditions
    - Forecast - 8 Half Days
    - Forecast - 4, 10 Days
    - Forecast - 10, 20 Hours
    - Alerts
    - Alerts with Storm Info
    - Maps - Satellite
    - Maps - Radar

  - [Coming Soon] Open Weather Maps
    - https://openweathermap.org/api
    - Implemented in JD.Weather.OpenWeatherMaps.pas
    - Location by Zip, City/State, Coords
    - Current Conditions
    - Forecast - 40 x 3hrs (5 Days)
    - Maps - Satellite
    - Maps - Radar
    - UNSUPPORTED:
      - Forecast - Hourly
      - Alerts

  - [Coming Soon] Accu Weather
    - http://apidev.accuweather.com/developers/
    - Implemented in JD.Weather.AccuWeather.pas
    - Location by Zip, City/State, Coords
    - Current Conditions
    - Forecast - 1, 12, 24, 72, 120, 240 Hours
    - Forecast - 1, 5, 10, 15, 25 Days
    - Alerts
    - Maps - Satellite
    - Maps - Radar

  - [Coming Soon] Foreca
    - http://corporate.foreca.com/en/products-services/data/weather-api
    - Implemented in JD.Weather.Foreca.pas
    - Location by Coords
    - Current Conditions
    - Forecast - 56 Hours
    - Forecast - 10 Days

▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓

  TODO:
  - Implement caching of weather data as to not fetch too often
  - Detect coordinates based on IP
    - Supposedly location services does this, but doesn't work in some cases...
  - Finish implementing all services
  - Remove hard-coding of available services
    - Remove dependency of service-specific units from main unit
    - Requires complete re-structure of how threads are spawned
    - Requires each service unit to register itself into the main one
      - Implemented in the `initialization` and `finalization` of each unit
  - Wrap inside a DLL
    - Create DLL for each possible service
    - Remove main DLL dependency on service DLLs
    - Add more services in future without recompiling anything
    - Created new DLL structures, need to finish above tasks first

▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
*)

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  Winapi.Windows,
  SuperObject,
  JD.Weather.Intf,
  IdHTTP;

type



//Weather Thread Base

{$REGION "Weather Thread Base"}

  TJDWeather = class;

  TJDWeatherThread = class(TThread)
  private
    FOwner: TJDWeather;
    FWeb: TIdHTTP;
    FLastAll: TDateTime;
    FConditions: TWeatherConditions;
    FLastConditions: TDateTime;
    FForecast: TWeatherForecast;
    FLastForecast: TDateTime;
    FForecastHourly: TWeatherForecast;
    FLastForecastHourly: TDateTime;
    FForecastDaily: TWeatherForecast;
    FLastForecastDaily: TDateTime;
    FMaps: TWeatherMaps;
    FLastMaps: TDateTime;
    FAlerts: TWeatherAlerts;
    FLastAlerts: TDateTime;
    FOnConditions: TWeatherConditionsEvent;
    FOnForecast: TWeatherForecastEvent;
    FOnForecastDaily: TWeatherForecastEvent;
    FOnForecastHourly: TWeatherForecastEvent;
    FOnAlerts: TWeatherAlertEvent;
    FOnMaps: TWeatherMapEvent;
    procedure CheckAll;
    procedure CheckConditions;
    procedure CheckForecast;
    procedure CheckForecastHourly;
    procedure CheckForecastDaily;
    procedure CheckAlerts;
    procedure CheckMaps;
  protected
    procedure Execute; override;
    procedure Process;
    procedure SYNC_DoOnConditions;
    procedure SYNC_DoOnForecast;
    procedure SYNC_DoOnForecastHourly;
    procedure SYNC_DoOnForecastDaily;
    procedure SYNC_DoOnAlerts;
    procedure SYNC_DoOnMaps;
  public
    constructor Create(AOwner: TJDWeather); reintroduce;
    destructor Destroy; override;
    function Owner: TJDWeather;
    function Web: TIdHTTP;
    {$IFDEF USE_VCL}
    function LoadPicture(const U: String; const P: TPicture): Boolean;
    {$ELSE}
    function LoadPicture(const U: String; const P: IWeatherGraphic): Boolean;
    {$ENDIF}
  public
    property OnConditions: TWeatherConditionsEvent read FOnConditions write FOnConditions;
    property OnForecast: TWeatherForecastEvent read FOnForecast write FOnForecast;
    property OnForecastHourly: TWeatherForecastEvent read FOnForecastHourly write FOnForecastHourly;
    property OnForecastDaily: TWeatherForecastEvent read FOnForecastDaily write FOnForecastDaily;
    property OnAlerts: TWeatherAlertEvent read FOnAlerts write FOnAlerts;
    property OnMaps: TWeatherMapEvent read FOnMaps write FOnMaps;
  public
    function GetUrl: String; virtual; abstract;
    function DoAll(Conditions: TWeatherConditions; Forecast: TWeatherForecast;
      ForecastDaily: TWeatherForecast; ForecastHourly: TWeatherForecast;
      Alerts: TWeatherAlerts; Maps: TWeatherMaps): Boolean; virtual; abstract;
    function DoConditions(Conditions: TWeatherConditions): Boolean; virtual; abstract;
    function DoForecast(Forecast: TWeatherForecast): Boolean; virtual; abstract;
    function DoForecastHourly(Forecast: TWeatherForecast): Boolean; virtual; abstract;
    function DoForecastDaily(Forecast: TWeatherForecast): Boolean; virtual; abstract;
    function DoAlerts(Alerts: TWeatherAlerts): Boolean; virtual; abstract;
    function DoMaps(Maps: TWeatherMaps): Boolean; virtual; abstract;
  end;

{$ENDREGION}












//Main TJDWeather Component

{$REGION "Main TJDWeather Component"}

  ///<summary>
  ///  Encapsulates multiple weather service info providers into a single
  ///  standardized multi-threaded component.
  ///</summary>
  TJDWeather = class(TComponent)
  private
    FThread: TJDWeatherThread;
    FService: TWeatherService;
    FActive: Boolean;
    FAllFreq: Integer;
    FConditionFreq: Integer;
    FForecastFreq: Integer;
    FMapsFreq: Integer;
    FAlertsFreq: Integer;
    FOnConditions: TWeatherConditionsEvent;
    FOnForecast: TWeatherForecastEvent;
    FOnAlerts: TWeatherAlertEvent;
    FOnMaps: TWeatherMapEvent;
    FKey: String;
    FLocationType: TJDWeatherLocationType;
    FLocationDetail2: String;
    FLocationDetail1: String;
    FUnits: TWeatherUnits;
    FAllAtOnce: Boolean;
    FOnForecastDaily: TWeatherForecastEvent;
    FOnForecastHourly: TWeatherForecastEvent;
    FWantedMaps: TWeatherMapTypes;
    procedure EnsureThread;
    procedure DestroyThread;
    procedure ThreadConditions(Sender: TObject; const Conditions: IWeatherConditions);
    procedure ThreadForecast(Sender: TObject; const Forecast: IWeatherForecast);
    procedure ThreadForecastHourly(Sender: TObject; const Forecast: IWeatherForecast);
    procedure ThreadForecastDaily(Sender: TObject; const Forecast: IWeatherForecast);
    procedure ThreadAlerts(Sender: TObject; const Alert: IWeatherAlerts);
    procedure ThreadMaps(Sender: TObject; const Maps: IWeatherMaps);
    procedure SetService(const Value: TWeatherService);
    procedure SetConditionFreq(const Value: Integer);
    procedure SetForecastFreq(const Value: Integer);
    procedure SetActive(const Value: Boolean);
    procedure SetMapsFreq(const Value: Integer);
    procedure SetKey(const Value: String);
    procedure SetLocationType(const Value: TJDWeatherLocationType);
    procedure SetLocationDetail1(const Value: String);
    procedure SetLocationDetail2(const Value: String);
    procedure SetAlertsFreq(const Value: Integer);
    procedure SetUnits(const Value: TWeatherUnits);
    procedure SetAllFreq(const Value: Integer);
    procedure SetAllAtOnce(const Value: Boolean);
    procedure SetWantedMaps(const Value: TWeatherMapTypes);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reload;
  published
    ///<summary>
    ///  Identifies the desired weather service provider to use.
    ///  Be sure to use an appropriate API Key for the chosen service
    ///  and provide it in the "Key" property.
    ///</summary>
    property Service: TWeatherService read FService write SetService;

    ///<summary>
    ///  Combines all different weather information into single increments.
    ///  This drastically saves on the number of API calls for those services
    ///  which have strict limitations.
    ///</summary>
    property AllAtOnce: Boolean read FAllAtOnce write SetAllAtOnce;

    ///<summary>
    ///  Specifies whether or not the component is active. Enabling will begin
    ///  fetching weather information from the service, and disabling will
    ///  stop it. NOTE: Active has no effect in design-time.
    ///</summary>
    property Active: Boolean read FActive write SetActive;

    ///<summary>
    ///  Number of seconds between performing full checks of all pieces
    ///  of weather information. All other specific frequencies are ignored
    ///  when "AllAtOnce" is enabled, and this is used instead.
    ///</summary>
    property AllFreq: Integer read FAllFreq write SetAllFreq;

    ///<summary>
    ///  Number of seconds between performing checks for Current Conditions.
    ///</summary>
    property ConditionFreq: Integer read FConditionFreq write SetConditionFreq;

    ///<summary>
    ///  Number of seconds between performing checks for Forecasts.
    ///</summary>
    property ForecastFreq: Integer read FForecastFreq write SetForecastFreq;

    ///<summary>
    ///  Number of seconds between performing checks for Maps.
    ///</summary>
    property MapsFreq: Integer read FMapsFreq write SetMapsFreq;

    ///<summary>
    ///  Number of seconds between performing checks for Alerts.
    ///</summary>
    property AlertsFreq: Integer read FAlertsFreq write SetAlertsFreq;

    ///<summary>
    ///  The API Key issued from the chosen weather service provider. When
    ///  changing "Service", this key must also change to correspond.
    ///</summary>
    property Key: String read FKey write SetKey;

    ///<summary>
    ///  The method of specifying the current location for weather information.
    ///  Changing this value requires updating the values in:
    ///  LocationDetail1
    ///  LocationDetail2
    ///</summary>
    property LocationType: TJDWeatherLocationType read FLocationType write SetLocationType;

    ///<summary>
    ///  The first piece of information to identify the location
    ///  - Zip Code
    ///  - Longitude Coordinate
    ///  - City
    ///  - IP Address [NOT YET SUPPORTED]
    ///</summary>
    property LocationDetail1: String read FLocationDetail1 write SetLocationDetail1;

    ///<summary>
    ///  The second piece of information to identify the location
    ///  - Latitude Coordinate
    ///  - State
    ///</summary>
    property LocationDetail2: String read FLocationDetail2 write SetLocationDetail2;

    ///<summary>
    ///  Unit of measurement
    ///  - Kelvin [NOT SUPPORTED]
    ///  - Imperial - Ferenheit, Feet, Inches, Miles...
    ///  - Metric - Celcius, Meters, Centimeters, Kilometers...
    ///</summary>
    property Units: TWeatherUnits read FUnits write SetUnits;

    ///<summary>
    ///  Specifies the types of maps to be fetched.
    ///</summary>
    property WantedMaps: TWeatherMapTypes read FWantedMaps write SetWantedMaps;




    ///<summary>
    ///  Triggered when new weather conditions are available.
    ///</summary>
    property OnConditions: TWeatherConditionsEvent read FOnConditions write FOnConditions;

    ///<summary>
    ///  Triggered when new weather forecasts are available.
    ///</summary>
    property OnForecast: TWeatherForecastEvent read FOnForecast write FOnForecast;

    ///<summary>
    ///  Triggered when new weather forecasts are available.
    ///</summary>
    property OnForecastHourly: TWeatherForecastEvent read FOnForecastHourly write FOnForecastHourly;

    ///<summary>
    ///  Triggered when new weather forecasts are available.
    ///</summary>
    property OnForecastDaily: TWeatherForecastEvent read FOnForecastDaily write FOnForecastDaily;

    ///<summary>
    ///  Triggered when new weather alerts are available.
    ///</summary>
    property OnAlerts: TWeatherAlertEvent read FOnAlerts write FOnAlerts;

    ///<summary>
    ///  Triggered when new weather maps are available.
    ///</summary>
    property OnMaps: TWeatherMapEvent read FOnMaps write FOnMaps;

  end;

{$ENDREGION}

implementation

uses
  DateUtils, StrUtils, Math,
  JD.Weather.OpenWeatherMaps,
  JD.Weather.WUnderground,
  JD.Weather.AccuWeather,
  JD.Weather.Foreca,
  JD.Weather.NWS,
  JD.Weather.NOAA;



//Base Weather Thread

{$REGION "Base Weather Thread"}

{ TJDWeatherThread }

constructor TJDWeatherThread.Create(AOwner: TJDWeather);
begin
  inherited Create(True);
  FOwner:= AOwner;
  FWeb:= TIdHTTP.Create(nil);
  FLastAll:= 0;
  FLastConditions:= 0;
  FLastForecast:= 0;
  FLastForecastDaily:= 0;
  FLastForecastHourly:= 0;
  FLastAlerts:= 0;
  FLastMaps:= 0;
end;

destructor TJDWeatherThread.Destroy;
begin
  {
  if Assigned(FConditions) then
    FConditions._Release;
  FConditions:= nil;
  if Assigned(FForecast) then
    FForecast._Release;
  FForecast:= nil;
  if Assigned(FForecastDaily) then
    FForecastDaily._Release;
  FForecastDaily:= nil;
  if Assigned(FForecastHourly) then
    FForecastHourly._Release;
  FForecastHourly:= nil;
  if Assigned(FAlerts) then
    FAlerts._Release;
  FAlerts:= nil;
  if Assigned(FMaps) then
    FMaps._Release;
  FMaps:= nil;
  }
  FreeAndNil(FWeb);
  inherited;
end;

procedure TJDWeatherThread.CheckAll;
var
  R: Boolean;
begin
  FLastAll:= Now;
  try
    {
    if Assigned(FConditions) then
      FConditions._Release;
    if Assigned(FForecast) then
      FForecast._Release;
    if Assigned(FForecastDaily) then
      FForecastDaily._Release;
    if Assigned(FForecastHourly) then
      FForecastHourly._Release;
    if Assigned(FAlerts) then
      FAlerts._Release;
    if Assigned(FMaps) then
      FMaps._Release;
    FConditions:= TWeatherConditions.Create;
    FConditions._AddRef;
    FForecast:= TWeatherForecast.Create;
    FForecast._AddRef;
    FForecastDaily:= TWeatherForecast.Create;
    FForecastDaily._AddRef;
    FForecastHourly:= TWeatherForecast.Create;
    FForecastHourly._AddRef;
    FAlerts:= TWeatherAlerts.Create;
    FAlerts._AddRef;
    FMaps:= TWeatherMaps.Create;
    FMaps._AddRef;
    R:= DoAll(FConditions, FForecast, FForecastDaily, FForecastHourly, FAlerts, FMaps);
    if R then begin
      Synchronize(SYNC_DoOnConditions);
      Synchronize(SYNC_DoOnForecast);
      Synchronize(SYNC_DoOnForecastDaily);
      Synchronize(SYNC_DoOnForecastHourly);
      Synchronize(SYNC_DoOnAlerts);
      Synchronize(SYNC_DoOnMaps);
    end else begin
      FConditions._Release;
      FConditions:= nil;
      FForecast._Release;
      FForecast:= nil;
      FForecastDaily._Release;
      FForecastDaily:= nil;
      FForecastHourly._Release;
      FForecastHourly:= nil;
      FAlerts._Release;
      FAlerts:= nil;
      FMaps._Release;
      FMaps:= nil;
    end;
    }
  except

  end;
end;

procedure TJDWeatherThread.CheckConditions;
var
  R: Boolean;
begin
  FLastConditions:= Now;
  try
    {
    if Assigned(FConditions) then
      FConditions._Release;
    //FConditions:= TWeatherConditions.Create(Self);
    FConditions:= TWeatherConditions.Create;
    FConditions._AddRef;
    R:= DoConditions(FConditions);
    if R then begin
      Synchronize(SYNC_DoOnConditions);
    end else begin
      FConditions._Release;
      FConditions:= nil;
    end;
    }
  except

  end;
end;

procedure TJDWeatherThread.CheckForecast;
var
  R: Boolean;
begin
  FLastForecast:= Now;
  try
    {
    if Assigned(FForecast) then
      FForecast._Release;
    FForecast:= TWeatherForecast.Create;
    FForecast._AddRef;
    R:= DoForecast(FForecast);
    if R then begin
      Synchronize(SYNC_DoOnForecast);
    end else begin
      FForecast._Release;
      FForecast:= nil;
    end;
    }
  except

  end;
end;

procedure TJDWeatherThread.CheckForecastDaily;
var
  R: Boolean;
begin
  FLastForecastDaily:= Now;
  try
    {
    if Assigned(FForecastDaily) then
      FForecastDaily._Release;
    FForecastDaily:= TWeatherForecast.Create;
    FForecastDaily._AddRef;
    R:= DoForecast(FForecastDaily);
    if R then begin
      Synchronize(SYNC_DoOnForecastDaily);
    end else begin
      FForecastDaily._Release;
      FForecastDaily:= nil;
    end;
    }
  except

  end;
end;

procedure TJDWeatherThread.CheckForecastHourly;
var
  R: Boolean;
begin
  FLastForecastHourly:= Now;
  try
    {
    if Assigned(FForecastHourly) then
      FForecastHourly._Release;
    FForecastHourly:= TWeatherForecast.Create;
    FForecastHourly._AddRef;
    R:= DoForecast(FForecastHourly);
    if R then begin
      Synchronize(SYNC_DoOnForecastHourly);
    end else begin
      FForecastHourly._Release;
      FForecastHourly:= nil;
    end;
    }
  except

  end;
end;

procedure TJDWeatherThread.CheckAlerts;
var
  R: Boolean;
begin
  FLastAlerts:= Now;
  try
    {
    if Assigned(FAlerts) then
      FAlerts._Release;
    FAlerts:= TWeatherAlerts.Create;
    FAlerts._AddRef;
    R:= DoAlerts(FAlerts);
    if R then begin
      Synchronize(SYNC_DoOnAlerts);
    end else begin
      FAlerts._Release;
      FAlerts:= nil;
    end;
    }
  except

  end;
end;

procedure TJDWeatherThread.CheckMaps;
var
  R: Boolean;
begin
  FLastMaps:= Now;
  try
    {
    if Assigned(FMaps) then
      FMaps._Release;
    FMaps:= TWeatherMaps.Create;
    FMaps._AddRef;
    R:= DoMaps(FMaps);
    if R then begin
      Synchronize(SYNC_DoOnMaps);
    end else begin
      FMaps._Release;
      FMaps:= nil;
    end;
    }
  except

  end;
end;

procedure TJDWeatherThread.Process;
  function TimePast(const DT: TDateTime; const Freq: Integer): Boolean;
  var
    N: TDateTime;
  begin
    N:= DateUtils.IncSecond(DT, Freq);
    Result:= Now >= N;
  end;
begin
  if FOwner.FAllAtOnce then begin

    if Terminated then Exit;
    if TimePast(FLastAll, FOwner.FAllFreq) then
      CheckAll;

  end else begin

    if Terminated then Exit;
    if TimePast(FLastConditions, FOwner.FConditionFreq) then
      CheckConditions;

    if Terminated then Exit;
    if TimePast(FLastForecast, FOwner.FForecastFreq) then
      CheckForecast;

    if Terminated then Exit;
    if TimePast(FLastForecastDaily, FOwner.FForecastFreq) then
      CheckForecastDaily;

    if Terminated then Exit;
    if TimePast(FLastForecastHourly, FOwner.FForecastFreq) then
      CheckForecastHourly;

    if Terminated then Exit;
    if TimePast(FLastAlerts, FOwner.FAlertsFreq) then
      CheckAlerts;

    if Terminated then Exit;
    if TimePast(FLastMaps, FOwner.FMapsFreq) then
      CheckMaps;

  end;
  if Terminated then Exit;
  Sleep(1);
end;

procedure TJDWeatherThread.Execute;
begin
  while not Terminated do begin
    try
      Process;
    except
      on E: Exception do begin
        //TODO
      end;
    end;
  end;
end;

{$IFDEF USE_VCL}
function TJDWeatherThread.LoadPicture(const U: String; const P: TPicture): Boolean;
var
  S: TMemoryStream;
  I: TGifImage;
begin
  Result:= False;
  try
    S:= TMemoryStream.Create;
    try
      FWeb.Get(U, S);
      S.Position:= 0;
      I:= TGifImage.Create;
      try
        I.LoadFromStream(S);
        P.Assign(I);
        Result:= True;
      finally
        FreeAndNil(I);
      end;
    finally
      FreeAndNil(S);
    end;
  except

  end;
end;
{$ELSE}
function TJDWeatherThread.LoadPicture(const U: String;
  const P: IWeatherGraphic): Boolean;
var
  S: TStringStream;
begin
  Result:= False;
  try
    S:= TStringStream.Create;
    try
      FWeb.Get(U, S);
      S.Position:= 0;
      P.Base64:= S.DataString;
    finally
      FreeAndNil(S);
    end;
  except

  end;
end;
{$ENDIF}

function TJDWeatherThread.Owner: TJDWeather;
begin
  Result:= FOwner;
end;

procedure TJDWeatherThread.SYNC_DoOnConditions;
begin
  if Assigned(FOnConditions) then
    FOnConditions(Self, FConditions);
end;

procedure TJDWeatherThread.SYNC_DoOnForecast;
begin
  if Assigned(FOnForecast) then
    FOnForecast(Self, FForecast);
end;

procedure TJDWeatherThread.SYNC_DoOnForecastDaily;
begin
  if Assigned(FOnForecastDaily) then
    FOnForecastDaily(Self, FForecastDaily);
end;

procedure TJDWeatherThread.SYNC_DoOnForecastHourly;
begin
  if Assigned(FOnForecastHourly) then
    FOnForecastHourly(Self, FForecastHourly);
end;

procedure TJDWeatherThread.SYNC_DoOnAlerts;
begin
  if Assigned(FOnAlerts) then
    FOnAlerts(Self, FAlerts);
end;

procedure TJDWeatherThread.SYNC_DoOnMaps;
begin
  if Assigned(FOnMaps) then
    FOnMaps(Self, FMaps);
end;

function TJDWeatherThread.Web: TIdHTTP;
begin
  Result:= FWeb;
end;

{$ENDREGION}













//Main TJDWeather Component

{$REGION "Main TJDWeather Component"}

{ TJDWeather }

constructor TJDWeather.Create(AOwner: TComponent);
begin
  inherited;
  FLocationType:= wlAutoIP;
  FService:= wsWUnderground;
  EnsureThread;
  FAllFreq:= 300;
  FConditionFreq:= 300;
  FForecastFreq:= 300;
  FAlertsFreq:= 300;
  FMapsFreq:= 300;
  FUnits:= wuImperial;
  FWantedMaps:= [TWeatherMapType.mpAniRadar];
end;

destructor TJDWeather.Destroy;
begin
  DestroyThread;
  inherited;
end;

procedure TJDWeather.EnsureThread;
begin
  if csDesigning in ComponentState then Exit;
  if not FActive then Exit;
  if FThread <> nil then Exit;

  //If thread is not already created, create it now.
  //Depending on the chosen service, it will create the
  //service-specific thread.

  case FService of
    wsOpenWeatherMap:   FThread:= TOWMWeatherThread.Create(Self);
    wsWUnderground:     FThread:= TWUWeatherThread.Create(Self);
    wsNWS:              FThread:= TNWSWeatherThread.Create(Self);
    wsAccuWeather:      FThread:= TAWWeatherThread.Create(Self);
    wsNOAA:             FThread:= TNOAAWeatherThread.Create(Self);
    wsForeca:           FThread:= TForecaWeatherThread.Create(Self);
  end;
  FThread.FreeOnTerminate:= True;
  FThread.OnConditions:= ThreadConditions;
  FThread.OnForecast:= ThreadForecast;
  FThread.OnForecastHourly:= ThreadForecastHourly;
  FThread.OnForecastDaily:= ThreadForecastDaily;
  FThread.OnMaps:= ThreadMaps;
  FThread.OnAlerts:= ThreadAlerts;
  FThread.Start;
end;

procedure TJDWeather.DestroyThread;
begin
  if Assigned(FThread) then begin
    FThread.Terminate;
    //NOTE: DO NOT use WaitFor - thread will free on terminate!
    FThread:= nil;
  end;
end;

procedure TJDWeather.Reload;
begin
  DestroyThread;
  EnsureThread;
end;

procedure TJDWeather.ThreadConditions(Sender: TObject; const Conditions: IWeatherConditions);
begin
  if Assigned(FOnConditions) then
    FOnConditions(Self, Conditions);
end;

procedure TJDWeather.ThreadForecast(Sender: TObject; const Forecast: IWeatherForecast);
begin
  if Assigned(FOnForecast) then
    FOnForecast(Self, Forecast);
end;

procedure TJDWeather.ThreadForecastDaily(Sender: TObject;
  const Forecast: IWeatherForecast);
begin
  if Assigned(FOnForecastDaily) then
    FOnForecastDaily(Self, Forecast);
end;

procedure TJDWeather.ThreadForecastHourly(Sender: TObject;
  const Forecast: IWeatherForecast);
begin
  if Assigned(FOnForecastHourly) then
    FOnForecastHourly(Self, Forecast);
end;

procedure TJDWeather.ThreadMaps(Sender: TObject; const Maps: IWeatherMaps);
begin
  if Assigned(FOnMaps) then
    FOnMaps(Self, Maps);
end;

procedure TJDWeather.ThreadAlerts(Sender: TObject; const Alert: IWeatherAlerts);
begin
  if Assigned(FOnAlerts) then
    FOnAlerts(Self, Alert);
end;

procedure TJDWeather.SetActive(const Value: Boolean);
begin
  if Value then begin
    if not FActive then begin
      FActive:= True;
      EnsureThread;
    end;
  end else begin
    if FActive then begin
      FActive:= False;
      DestroyThread;
    end;
  end;
end;

procedure TJDWeather.SetAlertsFreq(const Value: Integer);
begin
  FAlertsFreq := Value;
end;

procedure TJDWeather.SetAllAtOnce(const Value: Boolean);
begin
  FAllAtOnce := Value;
  //TODO: Invalidate Thread
end;

procedure TJDWeather.SetAllFreq(const Value: Integer);
begin
  FAllFreq := Value;
end;

procedure TJDWeather.SetConditionFreq(const Value: Integer);
begin
  FConditionFreq := Value;
end;

procedure TJDWeather.SetForecastFreq(const Value: Integer);
begin
  FForecastFreq:= Value;
end;

procedure TJDWeather.SetKey(const Value: String);
begin
  FKey := Value;
end;

procedure TJDWeather.SetLocationDetail1(const Value: String);
begin
  FLocationDetail1 := Value;
end;

procedure TJDWeather.SetLocationDetail2(const Value: String);
begin
  FLocationDetail2 := Value;
end;

procedure TJDWeather.SetLocationType(const Value: TJDWeatherLocationType);
begin
  FLocationType := Value;
end;

procedure TJDWeather.SetMapsFreq(const Value: Integer);
begin
  FMapsFreq := Value;
end;

procedure TJDWeather.SetUnits(const Value: TWeatherUnits);
begin
  FUnits := Value;
  //TODO: Invalidate all info
end;

procedure TJDWeather.SetWantedMaps(const Value: TWeatherMapTypes);
begin
  FWantedMaps := Value;
  //TODO: Invalidate maps
end;

procedure TJDWeather.SetService(const Value: TWeatherService);
begin
  //Changing service requires swapping out thread class
  //This switching is the core of multiple services in a single common structure
  //Each inherited thread implementation must be specific to the chosen service
  if Value <> FService then begin
    if FActive then begin
      raise Exception.Create('Cannot change service while active.');
    end else begin
      DestroyThread;
      FService := Value;
      EnsureThread;
    end;
  end;
end;

{$ENDREGION}

end.
