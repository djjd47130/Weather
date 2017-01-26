unit JD.Weather.MapControl;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  JD.Weather.Intf,
  JD.Weather.Maps,
  Vcl.Controls, Vcl.Forms;

type
  TWeatherMapView = class(TScrollingWinControl)
  private
    FMap: TWeatherMap;
    procedure SetMap(const Value: TWeatherMap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Map: TWeatherMap read FMap write SetMap;
  end;


implementation

{ TWeatherMapView }

constructor TWeatherMapView.Create(AOwner: TComponent);
begin
  inherited;
  FMap:= nil;
end;

destructor TWeatherMapView.Destroy;
begin

  inherited;
end;

procedure TWeatherMapView.SetMap(const Value: TWeatherMap);
begin
  FMap := Value;
end;

end.
