unit JD.Weather;

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils,
  JD.Weather.Intf;

type
  TJDWeather = class(TComponent)
  private
    FLib: HMODULE;
    FCreateLib: TCreateJDWeather;
    FWeather: IJDWeather;
    FService: IWeatherService;
    FWantedMaps: TWeatherMapTypes;
    FOnConditions: TWeatherConditionsEvent;
    FOnMaps: TWeatherMapEvent;
    FOnAlerts: TWeatherAlertEvent;
    FOnForecastDaily: TWeatherForecastEvent;
    FOnForecastHourly: TWeatherForecastEvent;
    FOnForecastSummary: TWeatherForecastEvent;
    procedure SetWantedMaps(const Value: TWeatherMapTypes);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Active: Boolean;
  published
    property WantedMaps: TWeatherMapTypes read FWantedMaps write SetWantedMaps;

    property OnConditions: TWeatherConditionsEvent read FOnConditions write FOnConditions;
    property OnAlerts: TWeatherAlertEvent read FOnAlerts write FOnAlerts;
    property OnForecastSummary: TWeatherForecastEvent read FOnForecastSummary write FOnForecastSummary;
    property OnForecastHourly: TWeatherForecastEvent read FOnForecastHourly write FOnForecastHourly;
    property OnForecastDaily: TWeatherForecastEvent read FOnForecastDaily write FOnForecastDaily;
    property OnMaps: TWeatherMapEvent read FOnMaps write FOnMaps;
  end;

implementation

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

end.
