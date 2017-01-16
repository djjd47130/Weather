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
    FImage: IWeatherGraphic;
    FTimestamp: TDateTime;
  public
    constructor Create(AOwner: TWeatherMap); overload;
    constructor Create(AOwner: TWeatherMap; const Image: IWeatherGraphic); overload;
    destructor Destroy; override;
    property Timestamp: TDateTime read FTimestamp;
  end;

  TWeatherMap = class(TComponent)
  private
    FItems: TObjectList<TWeatherMapItem>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published

  end;

implementation

{ TWeatherMapItem }

constructor TWeatherMapItem.Create(AOwner: TWeatherMap);
begin
  FOwner:= AOwner;
end;

constructor TWeatherMapItem.Create(AOwner: TWeatherMap;
  const Image: IWeatherGraphic);
begin
  FOwner:= AOwner;
  FImage:= Image;
end;

destructor TWeatherMapItem.Destroy;
begin
  if Assigned(FImage) then begin
    FImage._Release;
    FImage:= nil;
  end;
  inherited;
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

end.
