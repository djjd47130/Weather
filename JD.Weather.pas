unit JD.Weather;

(*
  JD Weather Component
  Encapsulates entire JD weather system in a single component

*)

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils,
  JD.Weather.Intf;

type
  TJDWeatherInfoSettings = class;
  TJDWeatherInfoSettingsGroup = class;
  TJDWeather = class;
  TJDWeatherThread = class;


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
    FLib: HMODULE;
    FCreateLib: TCreateJDWeather;
    FWeather: IJDWeather;
    FService: IWeatherService;
    FWantedMaps: TWeatherMapTypes;
    FThread: TJDWeatherThread;
    FOnConditions: TWeatherConditionsEvent;
    FOnMaps: TWeatherMapEvent;
    FOnAlerts: TWeatherAlertEvent;
    FOnForecastDaily: TWeatherForecastEvent;
    FOnForecastHourly: TWeatherForecastEvent;
    FOnForecastSummary: TWeatherForecastEvent;
    FWantedInfo: TWeatherInfoTypes;
    FWantedForecasts: TWeatherForecastTypes;
    procedure SetWantedMaps(const Value: TWeatherMapTypes);
    procedure SetWantedInfo(const Value: TWeatherInfoTypes);
    procedure SetWantedForecasts(const Value: TWeatherForecastTypes);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Active: Boolean;
  published
    property WantedInfo: TWeatherInfoTypes read FWantedInfo write SetWantedInfo;
    property WantedForecasts: TWeatherForecastTypes read FWantedForecasts write SetWantedForecasts;
    property WantedMaps: TWeatherMapTypes read FWantedMaps write SetWantedMaps;

    property OnConditions: TWeatherConditionsEvent read FOnConditions write FOnConditions;
    property OnAlerts: TWeatherAlertEvent read FOnAlerts write FOnAlerts;
    property OnForecastSummary: TWeatherForecastEvent read FOnForecastSummary write FOnForecastSummary;
    property OnForecastHourly: TWeatherForecastEvent read FOnForecastHourly write FOnForecastHourly;
    property OnForecastDaily: TWeatherForecastEvent read FOnForecastDaily write FOnForecastDaily;
    property OnMaps: TWeatherMapEvent read FOnMaps write FOnMaps;
  end;

  TJDWeatherThread = class(TThread)
  private
    FOwner: TJDWeather;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TJDWeather); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TJDWeatherInfoSettings }

constructor TJDWeatherInfoSettings.Create(AOwner: TJDWeatherInfoSettingsGroup);
begin
  FOwner:= AOwner;
  Frequency:= 300; //5 Minutes
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
  FStation.Free;
  FPlanner.Free;
  FHistory.Free;
  FHurricane.Free;
  FAstronomy.Free;
  FAlmanac.Free;
  FMaps.Free;
  FForecastDaily.Free;
  FForecastHourly.Free;
  FForecastSummary.Free;
  FAlerts.Free;
  FConditions.Free;
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
var
  EC: Integer;
begin
  inherited;
  FService:= nil;
  try
    FLib:= LoadLibrary('JDWeather.dll');
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
end;

destructor TJDWeather.Destroy;
begin
  FWeather._Release;
  FWeather:= nil;
  inherited;
end;

procedure TJDWeather.SetWantedForecasts(const Value: TWeatherForecastTypes);
begin
  FWantedForecasts := Value;
  if Assigned(FService) then begin
    //FService.WantedForecasts:= Value; //TODO
  end;
end;

procedure TJDWeather.SetWantedInfo(const Value: TWeatherInfoTypes);
begin
  FWantedInfo := Value;
  if Assigned(FService) then begin
    //FService.WantedInfo:= Value; //TODO
  end;
end;

procedure TJDWeather.SetWantedMaps(const Value: TWeatherMapTypes);
begin
  FWantedMaps:= Value;
  if Assigned(FService) then begin
    //FService.WantedMaps:= Value; //TODO
  end;
end;

function TJDWeather.Active: Boolean;
begin
  Result:= Assigned(FService);
end;

{ TJDWeatherThread }

constructor TJDWeatherThread.Create(AOwner: TJDWeather);
begin
  inherited Create(True);
  FOwner:= AOwner;
end;

destructor TJDWeatherThread.Destroy;
begin

  inherited;
end;

procedure TJDWeatherThread.Execute;
begin
  while not Terminated do begin


  end;
end;

end.
