unit JD.Weather.Config;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  JD.Weather.SuperObject;

type

  IJDWeatherConfigService = interface
    function GetName: WideString;
    procedure SetName(const Value: WideString);
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);

    property Name: WideString read GetName write SetName;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  IJDWeatherConfigServices = interface
    function GetItem(const Index: Integer): IJDWeatherConfigService;
    function GetCount: Integer;
    function Add: IJDWeatherConfigService;
    procedure Delete(const Index: Integer);
    procedure Clear;

    property Count: Integer read GetCount;
    property Items[const Index: Integer]: IJDWeatherConfigService read GetItem; default;
  end;

  IJDWeatherConfig = interface
    function GetLocationType: Integer;
    procedure SetLocationType(const Value: Integer);
    function GetLocationDetail1: WideString;
    procedure SetLocationDetail1(const Value: WideString);
    function GetLocationDetail2: WideString;
    procedure SetLocationDetail2(const Value: WideString);
    function GetServices: IJDWeatherConfigServices;
    function GetUnitMeasure: Integer;
    procedure SetUnitMeasure(const Value: Integer);

    procedure LoadFromFile(const Filename: WideString);
    procedure SaveToFile(const Filename: WideString);

    property LocationType: Integer read GetLocationType write SetLocationType;
    property LocationDetail1: WideString read GetLocationDetail1 write SetLocationDetail1;
    property LocationDetail2: WideString read GetLocationDetail2 write SetLocationDetail2;
    property Services: IJDWeatherConfigServices read GetServices;
    property UnitMeasure: Integer read GetUnitMeasure write SetUnitMeasure;
  end;









  // ---------------------- IMPLEMENTATION OBJECTS ----------------------


  TJDWeatherConfigService = class(TInterfacedObject, IJDWeatherConfigService)
  private
    FName: WideString;
    FEnabled: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetName: WideString;
    procedure SetName(const Value: WideString);
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);

    property Name: WideString read GetName write SetName;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  TJDWeatherConfigServices = class(TInterfacedObject, IJDWeatherConfigServices)
  private
    FItems: TList<IJDWeatherConfigService>;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function GetItem(const Index: Integer): IJDWeatherConfigService;
    function GetCount: Integer;
    function Add: IJDWeatherConfigService;
    procedure Delete(const Index: Integer);
    procedure Clear;

    property Count: Integer read GetCount;
    property Items[const Index: Integer]: IJDWeatherConfigService read GetItem; default;
  end;

  TJDWeatherConfig = class(TInterfacedObject, IJDWeatherConfig)
  private
    FObj: ISuperObject;
    FServices: TJDWeatherConfigServices;
    FInvalidateLocation: Boolean;
    FInvalidateServices: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure InvalidateLocation;
    procedure InvalidateServices;
  public
    function GetLocationType: Integer;
    procedure SetLocationType(const Value: Integer);
    function GetLocationDetail1: WideString;
    procedure SetLocationDetail1(const Value: WideString);
    function GetLocationDetail2: WideString;
    procedure SetLocationDetail2(const Value: WideString);
    function GetServices: IJDWeatherConfigServices;
    function GetUnitMeasure: Integer;
    procedure SetUnitMeasure(const Value: Integer);

    procedure LoadFromFile(const Filename: WideString);
    procedure SaveToFile(const Filename: WideString);

    property LocationType: Integer read GetLocationType write SetLocationType;
    property LocationDetail1: WideString read GetLocationDetail1 write SetLocationDetail1;
    property LocationDetail2: WideString read GetLocationDetail2 write SetLocationDetail2;
    property Services: IJDWeatherConfigServices read GetServices;
    property UnitMeasure: Integer read GetUnitMeasure write SetUnitMeasure;
  end;



implementation

{ TJDWeatherConfigService }

constructor TJDWeatherConfigService.Create;
begin

end;

destructor TJDWeatherConfigService.Destroy;
begin

  inherited;
end;

function TJDWeatherConfigService.GetEnabled: Boolean;
begin

end;

function TJDWeatherConfigService.GetName: WideString;
begin

end;

procedure TJDWeatherConfigService.SetEnabled(const Value: Boolean);
begin

end;

procedure TJDWeatherConfigService.SetName(const Value: WideString);
begin

end;

{ TJDWeatherConfigServices }

constructor TJDWeatherConfigServices.Create;
begin
  FItems:= TList<IJDWeatherConfigService>.Create;
end;

destructor TJDWeatherConfigServices.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TJDWeatherConfigServices.Add: IJDWeatherConfigService;
var
  R: TJDWeatherConfigService;
begin
  R:= TJDWeatherConfigService.Create;
  Self.FItems.Add(R);
  Result:= R;
end;

procedure TJDWeatherConfigServices.Clear;
begin
  while Count > 0 do
    Delete(0);
end;

procedure TJDWeatherConfigServices.Delete(const Index: Integer);
begin
  FItems.Delete(Index);
end;

function TJDWeatherConfigServices.GetCount: Integer;
begin
  Result:= FItems.Count;
end;

function TJDWeatherConfigServices.GetItem(
  const Index: Integer): IJDWeatherConfigService;
begin
  Result:= FItems[Index];
end;

{ TJDWeatherConfig }

constructor TJDWeatherConfig.Create;
begin
  FServices:= TJDWeatherConfigServices.Create;
  FServices._AddRef;
  FObj:= SO;
  FObj._AddRef;
end;

destructor TJDWeatherConfig.Destroy;
begin
  if Assigned(FServices) then
    FServices._Release;
  FServices:= nil;
  if Assigned(FObj) then
    FObj._Release;
  FObj:= nil;
  inherited;
end;

function TJDWeatherConfig.GetLocationDetail1: WideString;
begin
  Result:= FObj.S['location_detail_1'];
end;

function TJDWeatherConfig.GetLocationDetail2: WideString;
begin
  Result:= FObj.S['location_detail_2'];
end;

function TJDWeatherConfig.GetLocationType: Integer;
begin
  Result:= FObj.I['location_type'];
end;

function TJDWeatherConfig.GetServices: IJDWeatherConfigServices;
begin
  Result:= FServices;
end;

function TJDWeatherConfig.GetUnitMeasure: Integer;
begin
  Result:= FObj.I['unit_measure'];
end;

procedure TJDWeatherConfig.InvalidateLocation;
begin
  FInvalidateLocation:= True;
end;

procedure TJDWeatherConfig.InvalidateServices;
begin
  FInvalidateServices:= True;
end;

procedure TJDWeatherConfig.SetLocationDetail1(const Value: WideString);
begin
  FObj.S['location_detail_1']:= Value;
end;

procedure TJDWeatherConfig.SetLocationDetail2(const Value: WideString);
begin
  FObj.S['location_detail_2']:= Value;
end;

procedure TJDWeatherConfig.SetLocationType(const Value: Integer);
begin
  FObj.I['location_type']:= Value;
end;

procedure TJDWeatherConfig.SetUnitMeasure(const Value: Integer);
begin
  FObj.I['unit_measure']:= Value;
end;

procedure TJDWeatherConfig.LoadFromFile(const Filename: WideString);
var
  L: TStringList;
begin
  if Assigned(FObj) then
    FObj._Release;
  FObj:= nil;
  L:= TStringList.Create;
  try
    L.LoadFromFile(Filename);
    FObj:= SO(L.Text);
    if not Assigned(FObj) then
      FObj:= SO;
    FObj._AddRef;
  finally
    FreeAndNil(L);
  end;
end;

procedure TJDWeatherConfig.SaveToFile(const Filename: WideString);
var
  L: TStringList;
begin
  L:= TStringList.Create;
  try
    L.Text:= FObj.AsJSon(True);
    L.SaveToFile(Filename);
  finally
    FreeAndNil(L);
  end;
end;

end.
