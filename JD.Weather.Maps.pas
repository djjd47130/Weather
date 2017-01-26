unit JD.Weather.Maps;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  JD.Weather.Intf;

type
  TWeatherMap = class;
  TWeatherMapItem = class;

  TWeatherMapEvent = procedure(Sender: TObject; const Map: TWeatherMapItem) of object;

  TWeatherMapItem = class(TObject)
  private
    FOwner: TWeatherMap;
    FLayers: TList<IWeatherGraphic>;
    FTimestamp: TDateTime;
    procedure PopulateLayers;
  public
    constructor Create(AOwner: TWeatherMap);
    destructor Destroy; override;
    property Timestamp: TDateTime read FTimestamp;
  end;

  TWeatherMap = class(TComponent)
  private
    FItems: TObjectList<TWeatherMapItem>;
    FZoom: Double;
    procedure SetZoom(const Value: Double);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ZoomTo(const Lon, Lat: Double; const Amt: Double = 1.0);
  published
    property Zoom: Double read FZoom write SetZoom;
  end;

implementation

{ TWeatherMapItem }

constructor TWeatherMapItem.Create(AOwner: TWeatherMap);
begin
  FOwner:= AOwner;
end;

destructor TWeatherMapItem.Destroy;
var
  X: Integer;
begin
  for X := 0 to FLayers.Count-1 do begin
    FLayers[X]._Release;
    FLayers[X]:= nil;
  end;
  FLayers.Clear;
  FreeAndNil(FLayers);
  inherited;
end;

procedure TWeatherMapItem.PopulateLayers;
var
  X: Integer;
  G: IWeatherGraphic;
  T: TWeatherMapType;
begin
  for X := 0 to FLayers.Count-1 do begin
    FLayers[X]._Release;
    FLayers[X]:= nil;
  end;
  FLayers.Clear;
  for T := Low(TWeatherMapType) to High(TWeatherMapType) do begin
    G:= TWeatherGraphic.Create;
    G._AddRef;
    FLayers.Add(G);
  end;
end;

{ TWeatherMap }

constructor TWeatherMap.Create(AOwner: TComponent);
begin
  inherited;
  FItems:= TObjectList<TWeatherMapItem>.Create(True);
end;

destructor TWeatherMap.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TWeatherMap.SetZoom(const Value: Double);
begin
  FZoom := Value;
end;

procedure TWeatherMap.ZoomTo(const Lon, Lat: Double; const Amt: Double);
begin
  //Lon/Lat are the center point to be zoomed.
  //Amt is the amount to be zoomed by.
end;

end.
