unit JD.Weather;

(*
  JD Weather Component - TJDWeather
  - Encapsulates entire JD weather system in a single component
  - Interacts directly with the JD Weather REST API
  - Multi-threaded to fetch weather data in the background
    - TJDWeatherThread - auto-created within TJDWeather



*)

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils,
  System.SyncObjs,
  JD.Weather.Intf,
  JD.Weather.SuperObject;

type
  TJDWeatherInfoSettings = class;
  TJDWeatherInfoSettingsGroup = class;
  TJDWeather = class;
  TJDWeatherThread = class;

  TJsonEvent = procedure(Sender: TObject; AObj: ISuperObject) of object;

  TJDWeatherInfoSettings = class(TPersistent)
  private
    FOwner: TJDWeatherInfoSettingsGroup;
    FEnabled: Boolean;
    FFrequency: Integer;
    FCached: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetFrequency(const Value: Integer);
    procedure SetCached(const Value: Boolean);
  public
    constructor Create(AOwner: TJDWeatherInfoSettingsGroup);
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Cached: Boolean read FCached write SetCached;
    property Frequency: Integer read FFrequency write SetFrequency;
  end;

  TJDWeatherInfoSettingsGroup = class(TPersistent)
  private
    FOwner: TJDWeather;
    FCombined: TJDWeatherInfoSettings;
    FConditions: TJDWeatherInfoSettings;
    FAlerts: TJDWeatherInfoSettings;
    FForecastSummary: TJDWeatherInfoSettings;
    FForecastHourly: TJDWeatherInfoSettings;
    FForecastDaily: TJDWeatherInfoSettings;
    FMaps: TJDWeatherInfoSettings;
    FAlmanac: TJDWeatherInfoSettings;
    FAstronomy: TJDWeatherInfoSettings;
    FHurricane: TJDWeatherInfoSettings;
    FHistory: TJDWeatherInfoSettings;
    FPlanner: TJDWeatherInfoSettings;
    FStation: TJDWeatherInfoSettings;
    procedure SetCombined(const Value: TJDWeatherInfoSettings);
    procedure SetAlerts(const Value: TJDWeatherInfoSettings);
    procedure SetAlmanac(const Value: TJDWeatherInfoSettings);
    procedure SetAstronomy(const Value: TJDWeatherInfoSettings);
    procedure SetConditions(const Value: TJDWeatherInfoSettings);
    procedure SetForecastDaily(const Value: TJDWeatherInfoSettings);
    procedure SetForecastHourly(const Value: TJDWeatherInfoSettings);
    procedure SetForecastSummary(const Value: TJDWeatherInfoSettings);
    procedure SetHistory(const Value: TJDWeatherInfoSettings);
    procedure SetHurricane(const Value: TJDWeatherInfoSettings);
    procedure SetMaps(const Value: TJDWeatherInfoSettings);
    procedure SetPlanner(const Value: TJDWeatherInfoSettings);
    procedure SetStation(const Value: TJDWeatherInfoSettings);
  public
    constructor Create(AOwner: TJDWeather);
    destructor Destroy; override;
  published
    property Combined: TJDWeatherInfoSettings read FCombined write SetCombined;
    property Conditions: TJDWeatherInfoSettings read FConditions write SetConditions;
    property Alerts: TJDWeatherInfoSettings read FAlerts write SetAlerts;
    property ForecastSummary: TJDWeatherInfoSettings read FForecastSummary write SetForecastSummary;
    property ForecastHourly: TJDWeatherInfoSettings read FForecastHourly write SetForecastHourly;
    property ForecastDaily: TJDWeatherInfoSettings read FForecastDaily write SetForecastDaily;
    property Maps: TJDWeatherInfoSettings read FMaps write SetMaps;
    property Almanac: TJDWeatherInfoSettings read FAlmanac write SetAlmanac;
    property Astronomy: TJDWeatherInfoSettings read FAstronomy write SetAstronomy;
    property Hurricane: TJDWeatherInfoSettings read FHurricane write SetHurricane;
    property History: TJDWeatherInfoSettings read FHistory write SetHistory;
    property Planner: TJDWeatherInfoSettings read FPlanner write SetPlanner;
    property Station: TJDWeatherInfoSettings read FStation write SetStation;
  end;

  TJDWeather = class(TComponent)
  private
    FThread: TJDWeatherThread;
    FOnConditions: TWeatherConditionsEvent;
    FOnMaps: TWeatherMapEvent;
    FOnAlerts: TWeatherAlertEvent;
    FOnForecastDaily: TWeatherForecastEvent;
    FOnForecastHourly: TWeatherForecastEvent;
    FOnForecastSummary: TWeatherForecastEvent;
    function GetWantedInfo: TWeatherInfoTypes;
    function GetWantedMaps: TWeatherMapTypes;
    procedure SetWantedInfo(const Value: TWeatherInfoTypes);
    procedure SetWantedMaps(const Value: TWeatherMapTypes);
    procedure ThreadOnAlerts(Sender: TObject; AObj: ISuperObject);
    procedure ThreadOnConditions(Sender: TObject; AObj: ISuperObject);
    procedure ThreadOnForecastDaily(Sender: TObject; AObj: ISuperObject);
    procedure ThreadOnForecastHourly(Sender: TObject; AObj: ISuperObject);
    procedure ThreadOnForecastSummary(Sender: TObject; AObj: ISuperObject);
    procedure ThreadOnMaps(Sender: TObject; const Image: IWeatherMaps);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Active: Boolean;
  published
    property WantedInfo: TWeatherInfoTypes read GetWantedInfo write SetWantedInfo;
    property WantedMaps: TWeatherMapTypes read GetWantedMaps write SetWantedMaps;

    property OnConditions: TWeatherConditionsEvent read FOnConditions write FOnConditions;
    property OnAlerts: TWeatherAlertEvent read FOnAlerts write FOnAlerts;
    property OnForecastSummary: TWeatherForecastEvent read FOnForecastSummary write FOnForecastSummary;
    property OnForecastHourly: TWeatherForecastEvent read FOnForecastHourly write FOnForecastHourly;
    property OnForecastDaily: TWeatherForecastEvent read FOnForecastDaily write FOnForecastDaily;
    property OnMaps: TWeatherMapEvent read FOnMaps write FOnMaps;
    {
    property OnAlmanac: TWeatherAlmanacEvent read FOnAlmanac write FOnAlmanac;
    property OnAstronomy: TWeatherAstronomyEvent read FOnAstronomy write FOnAstronomy;
    property OnHurricane: TWeatherHurricaneEvent read FOnHurricane write FOnHurricane;
    property OnHistory: TWeatherHistoryEvent read FOnHistory write FOnHistory;
    property OnPlanner: TWeatherPlannerEvent read FOnPlanner write FOnPlanner;
    property OnStation: TWeatherStationEvent read FOnStation write FOnStation;
    property OnLocation: TWeatherLocationEvent read FOnLocation write FOnLocation;
    property OnGeoLookup: TWeatherGeoLookupEvent read FOnGeoLookup write FOnGeoLookup;
    property OnTide: TWeatherTideEvent read FOnTide write FOnTide;
    property OnRawTide: TWeatherRawTideEvent read FOnRawTide write FOnRawTide;
    property OnWebcams: TWeatherWebcamsEvent read FOnWebcams write FOnWebcams;
    }
  end;

  TJDWeatherThread = class(TThread)
  private
    FOwner: TJDWeather;
    FSettings: TJDWeatherInfoSettingsGroup;
    FSettingsLock: TCriticalSection;
    FLib: HMODULE;
    FCreateLib: TCreateJDWeather;
    FWeather: IJDWeather;
    FService: IWeatherService;
    FWantedInfo: TWeatherInfoTypes;
    FWantedMaps: TWeatherMapTypes;

    FOnConditions: TJsonEvent;
    FOnMaps: TJsonEvent;
    FOnAlerts: TJsonEvent;
    FOnForecastDaily: TJsonEvent;
    FOnForecastHourly: TJsonEvent;
    FOnForecastSummary: TJsonEvent;

    FResponse: ISuperObject;
    FOnAstronomy: TJsonEvent;
    FOnTide: TJsonEvent;
    FOnWebcams: TJsonEvent;
    FOnLocation: TJsonEvent;
    FOnGeoLookup: TJsonEvent;
    FOnStation: TJsonEvent;
    FOnHistory: TJsonEvent;
    FOnHurricane: TJsonEvent;
    FOnPlanner: TJsonEvent;
    FOnAlmanac: TJsonEvent;
    FOnRawTide: TJsonEvent;

    procedure SetWantedMaps(const Value: TWeatherMapTypes);
    procedure SetWantedInfo(const Value: TWeatherInfoTypes);
    procedure Init;
    procedure Uninit;
    function Active: Boolean;
    procedure Process;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TJDWeather); reintroduce;
    destructor Destroy; override;
    procedure LoadFromCache;
    procedure SaveToCache;
    function LockSettings: TJDWeatherInfoSettingsGroup;
    procedure UnlockSettings;
  public
    property WantedInfo: TWeatherInfoTypes read FWantedInfo write SetWantedInfo;
    property WantedMaps: TWeatherMapTypes read FWantedMaps write SetWantedMaps;

    property OnConditions: TJsonEvent read FOnConditions write FOnConditions;
    property OnAlerts: TJsonEvent read FOnAlerts write FOnAlerts;
    property OnForecastSummary: TJsonEvent read FOnForecastSummary write FOnForecastSummary;
    property OnForecastHourly: TJsonEvent read FOnForecastHourly write FOnForecastHourly;
    property OnForecastDaily: TJsonEvent read FOnForecastDaily write FOnForecastDaily;
    //property OnMaps: TJsonEvent read FOnMaps write FOnMaps;
    property OnAlmanac: TJsonEvent read FOnAlmanac write FOnAlmanac;
    property OnAstronomy: TJsonEvent read FOnAstronomy write FOnAstronomy;
    property OnHurricane: TJsonEvent read FOnHurricane write FOnHurricane;
    property OnHistory: TJsonEvent read FOnHistory write FOnHistory;
    property OnPlanner: TJsonEvent read FOnPlanner write FOnPlanner;
    property OnStation: TJsonEvent read FOnStation write FOnStation;
    property OnLocation: TJsonEvent read FOnLocation write FOnLocation;
    property OnGeoLookup: TJsonEvent read FOnGeoLookup write FOnGeoLookup;
    property OnTide: TJsonEvent read FOnTide write FOnTide;
    property OnRawTide: TJsonEvent read FOnRawTide write FOnRawTide;
    property OnWebcams: TJsonEvent read FOnWebcams write FOnWebcams;
  end;

implementation

{ TJDWeatherInfoSettings }

constructor TJDWeatherInfoSettings.Create(AOwner: TJDWeatherInfoSettingsGroup);
begin
  FOwner:= AOwner;
  Self.FFrequency:= 300; //5 Minutes
  Self.FEnabled:= False;
  Self.FCached:= True;
end;

destructor TJDWeatherInfoSettings.Destroy;
begin

  inherited;
end;

procedure TJDWeatherInfoSettings.SetCached(const Value: Boolean);
begin
  FCached := Value;
end;

procedure TJDWeatherInfoSettings.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TJDWeatherInfoSettings.SetFrequency(const Value: Integer);
begin
  FFrequency := Value;
end;

{ TJDWeatherInfoSettingsGroup }

constructor TJDWeatherInfoSettingsGroup.Create(AOwner: TJDWeather);
begin
  FOwner:= AOwner;
  FCombined:= TJDWeatherInfoSettings.Create(Self);
  FConditions:= TJDWeatherInfoSettings.Create(Self);
  FAlerts:= TJDWeatherInfoSettings.Create(Self);
  FForecastSummary:= TJDWeatherInfoSettings.Create(Self);
  FForecastHourly:= TJDWeatherInfoSettings.Create(Self);
  FForecastDaily:= TJDWeatherInfoSettings.Create(Self);
  FMaps:= TJDWeatherInfoSettings.Create(Self);
  FAlmanac:= TJDWeatherInfoSettings.Create(Self);
  FAstronomy:= TJDWeatherInfoSettings.Create(Self);
  FHurricane:= TJDWeatherInfoSettings.Create(Self);
  FHistory:= TJDWeatherInfoSettings.Create(Self);
  FPlanner:= TJDWeatherInfoSettings.Create(Self);
  FStation:= TJDWeatherInfoSettings.Create(Self);
end;

destructor TJDWeatherInfoSettingsGroup.Destroy;
begin
  FreeAndNil(FStation);
  FreeAndNil(FPlanner);
  FreeAndNil(FHistory);
  FreeAndNil(FHurricane);
  FreeAndNil(FAstronomy);
  FreeAndNil(FAlmanac);
  FreeAndNil(FMaps);
  FreeAndNil(FForecastDaily);
  FreeAndNil(FForecastHourly);
  FreeAndNil(FForecastSummary);
  FreeAndNil(FAlerts);
  FreeAndNil(FConditions);
  FreeAndNil(FCombined);
  inherited;
end;

procedure TJDWeatherInfoSettingsGroup.SetAlerts(
  const Value: TJDWeatherInfoSettings);
begin
  FAlerts.Assign(Value);
end;

procedure TJDWeatherInfoSettingsGroup.SetAlmanac(
  const Value: TJDWeatherInfoSettings);
begin
  FAlmanac.Assign(Value);
end;

procedure TJDWeatherInfoSettingsGroup.SetAstronomy(
  const Value: TJDWeatherInfoSettings);
begin
  FAstronomy.Assign(Value);
end;

procedure TJDWeatherInfoSettingsGroup.SetCombined(
  const Value: TJDWeatherInfoSettings);
begin
  FCombined.Assign(Value);
end;

procedure TJDWeatherInfoSettingsGroup.SetConditions(
  const Value: TJDWeatherInfoSettings);
begin
  FConditions.Assign(Value);
end;

procedure TJDWeatherInfoSettingsGroup.SetForecastDaily(
  const Value: TJDWeatherInfoSettings);
begin
  FForecastDaily.Assign(Value);
end;

procedure TJDWeatherInfoSettingsGroup.SetForecastHourly(
  const Value: TJDWeatherInfoSettings);
begin
  FForecastHourly.Assign(Value);
end;

procedure TJDWeatherInfoSettingsGroup.SetForecastSummary(
  const Value: TJDWeatherInfoSettings);
begin
  FForecastSummary.Assign(Value);
end;

procedure TJDWeatherInfoSettingsGroup.SetHistory(
  const Value: TJDWeatherInfoSettings);
begin
  FHistory.Assign(Value);
end;

procedure TJDWeatherInfoSettingsGroup.SetHurricane(
  const Value: TJDWeatherInfoSettings);
begin
  FHurricane.Assign(Value);
end;

procedure TJDWeatherInfoSettingsGroup.SetMaps(
  const Value: TJDWeatherInfoSettings);
begin
  FMaps.Assign(Value);
end;

procedure TJDWeatherInfoSettingsGroup.SetPlanner(
  const Value: TJDWeatherInfoSettings);
begin
  FPlanner.Assign(Value);
end;

procedure TJDWeatherInfoSettingsGroup.SetStation(
  const Value: TJDWeatherInfoSettings);
begin
  FStation.Assign(Value);
end;

{ TJDWeather }

constructor TJDWeather.Create(AOwner: TComponent);
begin
  inherited;
  FThread:= TJDWeatherThread.Create(Self);
  FThread.OnConditions:= ThreadOnConditions;
  FThread.OnAlerts:= ThreadOnAlerts;
  FThread.OnForecastSummary:= ThreadOnForecastSummary;
  FThread.OnForecastHourly:= ThreadOnForecastHourly;
  FThread.OnForecastDaily:= ThreadOnForecastDaily;
  //FThread.OnMaps:= ThreadOnMaps;
  FThread.Start;
end;

destructor TJDWeather.Destroy;
begin
  FThread.Terminate;
  FThread.WaitFor;
  FreeAndNil(FThread);
  inherited;
end;

procedure TJDWeather.ThreadOnConditions(Sender: TObject; AObj: ISuperObject);
begin

end;

procedure TJDWeather.ThreadOnAlerts(Sender: TObject; AObj: ISuperObject);
begin

end;

procedure TJDWeather.ThreadOnForecastSummary(Sender: TObject; AObj: ISuperObject);
begin

end;

procedure TJDWeather.ThreadOnForecastHourly(Sender: TObject; AObj: ISuperObject);
begin

end;

procedure TJDWeather.ThreadOnForecastDaily(Sender: TObject; AObj: ISuperObject);
begin

end;

procedure TJDWeather.ThreadOnMaps(Sender: TObject; const Image: IWeatherMaps);
begin

end;

function TJDWeather.GetWantedInfo: TWeatherInfoTypes;
begin
  Result:= FThread.WantedInfo;
end;

function TJDWeather.GetWantedMaps: TWeatherMapTypes;
begin
  Result:= FThread.WantedMaps;
end;

procedure TJDWeather.SetWantedInfo(const Value: TWeatherInfoTypes);
begin
  FThread.WantedInfo:= Value;
end;

procedure TJDWeather.SetWantedMaps(const Value: TWeatherMapTypes);
begin
  FThread.WantedMaps:= Value;
end;

function TJDWeather.Active: Boolean;
begin
  Result:= FThread.Active;
end;

{ TJDWeatherThread }

constructor TJDWeatherThread.Create(AOwner: TJDWeather);
begin
  inherited Create(True);
  FOwner:= AOwner;
  FSettings:= TJDWeatherInfoSettingsGroup.Create(FOwner);
  FSettingsLock:= TCriticalSection.Create;
end;

destructor TJDWeatherThread.Destroy;
begin
  FSettingsLock.Enter;
  try
    //This is just to be sure it can free only after it's unlocked
  finally
    FSettingsLock.Leave;
  end;
  FreeAndNil(FSettingsLock);
  FreeAndNil(FSettings);
  inherited;
end;

procedure TJDWeatherThread.Init;
var
  EC: Integer;
begin
  FService:= nil;
  try
    FLib:= LoadLibrary(JD_WEATHER_LIB);
    if FLib <> 0 then begin
      FCreateLib:= GetProcAddress(FLib, 'CreateJDWeather');
      if Assigned(FCreateLib) then begin
        try
          FWeather:= FCreateLib(ExtractFilePath(ParamStr(0)));
          FWeather._AddRef;
          //TODO: Select default service
          if FWeather.Services.Count > 0 then
            Self.FService:= FWeather.Services.Items[0];
        except
          on E: Exception do begin
            raise Exception.Create('Failed to create new instance of "IJDWeather": '+E.Message);
          end;
        end;
      end else begin
        raise Exception.Create('Function "CreateJDWeather" not found!');
      end;
    end else begin
      EC:= GetLastError;
      raise Exception.Create('LoadLibrary failed with error code '+IntToStr(EC));
    end;
  except
    on E: Exception do begin
      raise Exception.Create('Failed to load JDWeather library: '+E.Message);
    end;
  end;
  if not Assigned(FService) then
    raise Exception.Create('Unexpected error: FService is not assigned!');
end;

procedure TJDWeatherThread.Uninit;
begin
  FWeather._Release;
  FWeather:= nil;
end;

procedure TJDWeatherThread.Process;
var
  Url: String;
  Req: ISuperObject;
begin
  //TODO: Check if anything needs to be updated

  //TODO: Prepare multi-info object, populate multi-service, multi-info options
  Req:= SO;
  Url:= 'http://api.weather.jdsoftwareinc.com:8664/';
  //Lock config object

  //Read config object, concatenate to Url






end;

procedure TJDWeatherThread.Execute;
begin
  Init;
  try
    while not Terminated do begin
      try
        try
          Process;
        finally
          Sleep(1000); //1 Second(s)
        end;
      except
        on E: Exception do begin
          //TODO: Handle exception, log
        end;
      end;
    end;
  finally
    Uninit;
  end;
end;

procedure TJDWeatherThread.SetWantedInfo(const Value: TWeatherInfoTypes);
begin
  FWantedInfo := Value;
  if Assigned(FService) then begin
    //FService.WantedInfo:= Value; //TODO
  end;
end;

procedure TJDWeatherThread.SetWantedMaps(const Value: TWeatherMapTypes);
begin
  FWantedMaps:= Value;
  if Assigned(FService) then begin
    //FService.WantedMaps:= Value; //TODO
  end;
end;

function TJDWeatherThread.Active: Boolean;
begin
  Result:= Assigned(FService);
end;

function TJDWeatherThread.LockSettings: TJDWeatherInfoSettingsGroup;
begin
  FSettingsLock.Enter;
  Result:= FSettings;
end;

procedure TJDWeatherThread.UnlockSettings;
begin
  FSettingsLock.Leave;
end;

procedure TJDWeatherThread.LoadFromCache;
begin
  //TODO: Load the most recently cached weather data

end;

procedure TJDWeatherThread.SaveToCache;
begin
  //TODO: Save current weather data to cache

end;

end.
