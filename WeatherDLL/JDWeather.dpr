library JDWeather;

(*
  JD Weather Library
  Core DLL for all weather information


*)

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Generics.Collections,
  System.IOUtils,
  Winapi.Windows,
  JD.Weather.Intf in '..\JD.Weather.Intf.pas';

{$R *.res}

type
  TWeatherServices = class(TInterfacedObject, IWeatherServices)
  private
    FItems: TList<IWeatherService>;
    procedure ClearServices;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetItem(const Index: Integer): IWeatherService;
    function Count: Integer;
    procedure LoadServices(const Dir: WideString);
    property Items[const Index: Integer]: IWeatherService read GetItem; default;
  end;

  TJDWeather = class(TInterfacedObject, IJDWeather)
  private
    FServices: TWeatherServices;
    FLocationType: TWeatherLocationType;
    FLocationDetail1: WideString;
    FLocationDetail2: WideString;
    FUnits: TWeatherUnits;
  public
    constructor Create;
    destructor Destroy; override;
  public
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

{ TWeatherServices }

constructor TWeatherServices.Create;
begin
  FItems:= TList<IWeatherService>.Create;
end;

destructor TWeatherServices.Destroy;
begin
  ClearServices;
  FreeAndNil(FItems);
  inherited;
end;

function TWeatherServices.Count: Integer;
begin
  Result:= FItems.Count;
end;

function TWeatherServices.GetItem(const Index: Integer): IWeatherService;
begin
  Result:= FItems[Index];
end;

procedure TWeatherServices.ClearServices;
var
  X: Integer;
  S: IWeatherService;
  //L: HMODULE;
begin
  for X := 0 to FItems.Count-1 do begin
    S:= FItems[X];
    //L:= S.Module;
    S._Release;
    S:= nil;
    //FreeLibrary(L); //TODO: Fix abnormal access violation...
    //https://forums.embarcadero.com/thread.jspa?messageID=488930
    //https://www.experts-exchange.com/questions/27803279/DLL-Crashing-on-Exit.html

  end;
  FItems.Clear;
end;

procedure TWeatherServices.LoadServices(const Dir: WideString);
var
  F: TStringDynArray;
  X: Integer;
  S: IWeatherService;
  FN: String;
  L: HMODULE;
  Svc: TCreateWeatherService;
begin
  ClearServices;
  F:= TDirectory.GetFiles(Dir, '*.dll');
  for X := Low(F) to High(F) do begin
    FN:= F[X];
    if SameText(ExtractFileName(FN), 'JDWeather.dll') then Continue;
    L:= LoadLibrary(PChar(FN));
    if L <> 0 then begin
      Svc:= GetProcAddress(L, 'CreateWeatherService');
      if Assigned(Svc) then begin
        S:= Svc;
        if Assigned(S) then begin
          S._AddRef;
          //S.Module:= L;

          //TODO...

          Self.FItems.Add(S);
        end;
      end;
    end;
  end;
end;

{ TJDWeather }

constructor TJDWeather.Create;
begin
  FServices:= TWeatherServices.Create;
  FServices._AddRef;
end;

destructor TJDWeather.Destroy;
begin
  FServices._Release;
  FServices:= nil;
  inherited;
end;

function TJDWeather.GetServices: IWeatherServices;
begin
  Result:= FServices;
end;

function TJDWeather.GetUnits: TWeatherUnits;
begin
  Result:= FUnits;
end;

function TJDWeather.GetLocationDetail1: WideString;
begin
  Result:= FLocationDetail1;
end;

function TJDWeather.GetLocationDetail2: WideString;
begin
  Result:= FLocationDetail2;
end;

function TJDWeather.GetLocationType: TWeatherLocationType;
begin
  Result:= FLocationType;
end;

procedure TJDWeather.SetLocationDetail1(const Value: WideString);
begin
  FLocationDetail1:= Value;
  //Invalidate;
end;

procedure TJDWeather.SetLocationDetail2(const Value: WideString);
begin
  FLocationDetail2:= Value;
  //Invalidate;
end;

procedure TJDWeather.SetLocationType(const Value: TWeatherLocationType);
begin
  FLocationType := Value;
  //Invalidate...
end;

procedure TJDWeather.SetUnits(const Value: TWeatherUnits);
begin
  FUnits:= Value;
end;










function CreateJDWeather(const LibDir: WideString): IJDWeather; stdcall;
var
  R: TJDWeather;
begin
  R:= TJDWeather.Create;
  try
    R.Services.LoadServices(LibDir);
  finally
    Result:= R;
  end;
end;

exports
  CreateJDWeather;

begin
end.
