unit JD.HTTPServer;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  TJDHttpServerRequest = class;
  TJDHttpServerRequests = class;
  TJDHttpServer = class;



  TJDHttpServerRequest = class(TCollectionItem)
  private
    FName: String;
    FRequests: TJDHttpServerRequests;
  protected
    function GetDisplayName: String; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published

  end;

  TJDHttpServerRequests = class(TOwnedCollection)
  private
    function GetItem(const Index: Integer): TJDHttpServerRequest;
    procedure SetItem(const Index: Integer; const Value: TJDHttpServerRequest);
  public
    constructor Create(AOwner: TPersistent); reintroduce;
    property Items[const Index: Integer]: TJDHttpServerRequest read GetItem write SetItem; default;
  end;

  TJDHttpServer = class(TComponent)
  private
    FRequests: TJDHttpServerRequests;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published

  end;

implementation

{ TJDHttpServerRequest }

constructor TJDHttpServerRequest.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FRequests:= TJDHttpServerRequests.Create(Self);
end;

destructor TJDHttpServerRequest.Destroy;
begin
  FreeAndNil(FRequests);
  inherited;
end;

function TJDHttpServerRequest.GetDisplayName: String;
begin
  if FName <> '' then
    Result:= '(No Name)'
  else
    Result:= FName;
end;

{ TJDHttpServerRequests }

constructor TJDHttpServerRequests.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJDHttpServerRequest);
end;

function TJDHttpServerRequests.GetItem(
  const Index: Integer): TJDHttpServerRequest;
begin
  Result:= TJDHttpServerRequest(inherited Items[Index]);
end;

procedure TJDHttpServerRequests.SetItem(const Index: Integer;
  const Value: TJDHttpServerRequest);
begin
  inherited Items[Index]:= Value;
end;

{ TJDHttpServer }

constructor TJDHttpServer.Create(AOwner: TComponent);
begin
  inherited;
  FRequests:= TJDHttpServerRequests.Create(Self);
end;

destructor TJDHttpServer.Destroy;
begin
  FreeAndNil(FRequests);
  inherited;
end;

end.
