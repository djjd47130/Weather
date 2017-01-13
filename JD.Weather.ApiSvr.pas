unit JD.Weather.ApiSvr;

interface

uses
  Winapi.Windows,
  System.Classes, System.SysUtils, System.TypInfo,
  JD.Weather.Intf, JD.Weather.SuperObject,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdCustomHTTPServer,
  IdHTTPServer, IdContext, IdTCPConnection, IdYarn;

type
  TWeatherContext = class(TIdServerContext)
  private
    FKey: WideString;
    FServiceUID: WideString;
    FLocType: WideString;
    FLoc1: WideString;
    FLoc2: WideString;
    FUnits: WideString;
    FDet: WideString;
    FDoc: TStringList;
    FWeather: IJDWeather;
    FService: IWeatherService;
  public
    constructor Create(AConnection: TIdTCPConnection; AYarn: TIdYarn;
      AList: TIdContextThreadList = nil); override;
    destructor Destroy; override;
  end;

  TJDWeatherApiSvrThread = class(TThread)
  private
    FLib: HMODULE;
    FCreateLib: TCreateJDWeather;
    FServer: TIdHTTPServer;
    procedure Init;
    procedure UnInit;
    procedure Process;
    procedure SvrCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure SvrContextCreated(AContext: TIdContext);
    procedure HandleServiceGet(AContext: TWeatherContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleServiceList(AContext: TWeatherContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleServiceSupport(AContext: TWeatherContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleNoRequest(AContext: TWeatherContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  protected
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TWeatherContext }

constructor TWeatherContext.Create(AConnection: TIdTCPConnection;
  AYarn: TIdYarn; AList: TIdContextThreadList);
begin
  inherited;
  FDoc:= TStringList.Create;
  FWeather:= nil;
end;

destructor TWeatherContext.Destroy;
begin
  if Assigned(FWeather) then begin
    FWeather._Release;
    FWeather:= nil;
  end;
  FreeAndNil(FDoc);
  inherited;
end;

{ TJDWeatherApiSvrThread }

constructor TJDWeatherApiSvrThread.Create;
begin
  inherited Create(True);

end;

destructor TJDWeatherApiSvrThread.Destroy;
begin

  inherited;
end;

procedure TJDWeatherApiSvrThread.Init;
var
  E: Integer;
begin
  FServer:= TIdHTTPServer.Create(nil);
  FServer.ContextClass:= TWeatherContext;
  FServer.DefaultPort:= 8664;
  FServer.OnCommandGet:= Self.SvrCommandGet;
  FServer.OnContextCreated:= Self.SvrContextCreated;

  FServer.Active:= True;

  FLib:= LoadLibrary('JDWeather.dll');
  if FLib <> 0 then begin
    FCreateLib:= GetProcAddress(FLib, 'CreateJDWeather');
    if Assigned(FCreateLib) then begin
      try

      except
        on E: Exception do begin
          raise Exception.Create('Failed to create new instance of "IJDWeather": '+E.Message);
        end;
      end;
    end else begin
      raise Exception.Create('Function "CreateJDWeather" not found!');
    end;
  end else begin
    E:= GetLastError;
    raise Exception.Create('Failed to load library "JDWeather.dll" with error code '+IntToStr(E));
  end;

end;

procedure TJDWeatherApiSvrThread.UnInit;
begin
  //FreeLibrary(FLib);
  FServer.Active:= False;
  FreeAndNil(FServer);
end;

procedure TJDWeatherApiSvrThread.Process;
begin
  Sleep(1);
end;

procedure TJDWeatherApiSvrThread.Execute;
begin
  while not Terminated do begin
    try
      Init;
      try
        Process;
      finally
        Uninit;
      end;
    except
      on E: Exception do begin
        //TODO
      end;
    end;
  end;
end;

procedure TJDWeatherApiSvrThread.SvrContextCreated(AContext: TIdContext);
var
  C: TWeatherContext;
  Dir: String;
begin
  C:= TWeatherContext(AContext);
  Dir:= ExtractFilePath(ParamStr(0));
  C.FWeather:= FCreateLib(Dir);

end;

procedure TJDWeatherApiSvrThread.HandleServiceSupport(AContext: TWeatherContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  O, O2: ISuperObject;
  Loc: TJDWeatherLocationType;
  Inf: TWeatherInfoType;
  Uni: TWeatherUnits;
  Alt: TWeatherAlertType;
  Alp: TWeatherAlertProp;
  Fop: TWeatherForecastProp;
  Map: TWeatherMapType;
begin
  O:= SO;
  try
    O.S['caption']:= AContext.FService.Info.Caption;
    O.S['serviceuid']:= AContext.FService.Info.UID;

    O2:= SA([]);
    try
      for Loc := Low(TJDWeatherLocationType) to High(TJDWeatherLocationType) do begin
        if Loc in AContext.FService.Info.Support.SupportedLocations then
          O2.AsArray.Add(SO(GetEnumName(TypeInfo(TJDWeatherLocationType), Ord(Loc))));
      end;
    finally
      O.O['supported_locations']:= O2;
    end;

    O2:= SA([]);
    try
      for Inf := Low(TWeatherInfoType) to High(TWeatherInfoType) do begin
        if Inf in AContext.FService.Info.Support.SupportedInfo then
          O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherInfoType), Ord(Inf))));
      end;
    finally
      O.O['supported_info']:= O2;
    end;

    O2:= SA([]);
    try
      for Uni := Low(TWeatherUnits) to High(TWeatherUnits) do begin
        if Uni in AContext.FService.Info.Support.SupportedUnits then
          O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherUnits), Ord(Uni))));
      end;
    finally
      O.O['supported_units']:= O2;
    end;

    O2:= SA([]);
    try
      for Alt := Low(TWeatherAlertType) to High(TWeatherAlertType) do begin
        if Alt in AContext.FService.Info.Support.SupportedAlerts then
          O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherAlertType), Ord(Alt))));
      end;
    finally
      O.O['supported_alerts']:= O2;
    end;

    O2:= SA([]);
    try
      for Alp := Low(TWeatherAlertProp) to High(TWeatherAlertProp) do begin
        if Alp in AContext.FService.Info.Support.SupportedAlertProps then
          O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherAlertProp), Ord(Alp))));
      end;
    finally
      O.O['supported_alert_props']:= O2;
    end;

    O2:= SA([]);
    try
      for Fop := Low(TWeatherForecastProp) to High(TWeatherForecastProp) do begin
        if Fop in AContext.FService.Info.Support.SupportedForecastSummaryProps then
          O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherForecastProp), Ord(Fop))));
      end;
    finally
      O.O['supported_forecast_summary_props']:= O2;
    end;

    O2:= SA([]);
    try
      for Fop := Low(TWeatherForecastProp) to High(TWeatherForecastProp) do begin
        if Fop in AContext.FService.Info.Support.SupportedForecastHourlyProps then
          O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherForecastProp), Ord(Fop))));
      end;
    finally
      O.O['supported_forecast_hourly_props']:= O2;
    end;

    O2:= SA([]);
    try
      for Fop := Low(TWeatherForecastProp) to High(TWeatherForecastProp) do begin
        if Fop in AContext.FService.Info.Support.SupportedForecastDailyProps then
          O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherForecastProp), Ord(Fop))));
      end;
    finally
      O.O['supported_forecast_daily_props']:= O2;
    end;

    O2:= SA([]);
    try
      for Map := Low(TWeatherMapType) to High(TWeatherMapType) do begin
        if Map in AContext.FService.Info.Support.SupportedMaps then
          O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherMapType), Ord(Map))));
      end;
    finally
      O.O['supported_maps']:= O2;
    end;


  finally
    AResponseInfo.ContentText:= O.AsJSon(True);
    AResponseInfo.ContentType:= 'text/json';
  end;
end;

procedure TJDWeatherApiSvrThread.HandleServiceList(AContext: TWeatherContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  O, O2: ISuperObject;
  X: Integer;
  S: IWeatherService;
begin
  O:= SO;
  try
    O.O['services']:= SA([]);
    for X := 0 to AContext.FWeather.Services.Count-1 do begin
      S:= AContext.FWeather.Services[X];
      O2:= SO;
      try
        O2.S['caption']:= S.Info.Caption;
        O2.S['uid']:= S.Info.UID;
        O2.S['url_main']:= S.Info.URLs.MainURL;
        O2.S['url_api']:= S.Info.URLs.ApiURL;
        O2.S['url_login']:= S.Info.URLs.LoginURL;
        O2.S['url_register']:= S.Info.URLs.RegisterURL;
        O2.S['url_legal']:= S.Info.URLs.LegalURL;
      finally
        O.O['services'].AsArray.Add(O2);
      end;
    end;
  finally
    AResponseInfo.ContentText:= O.AsJSon(True);
    AResponseInfo.ContentType:= 'text/json';
  end;
end;

procedure TJDWeatherApiSvrThread.HandleServiceGet(AContext: TWeatherContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  O: ISuperObject;
begin
  O:= SO;
  try

  finally
    AResponseInfo.ContentText:= O.AsJSon(True);
    AResponseInfo.ContentType:= 'text/json';
  end;
end;

procedure TJDWeatherApiSvrThread.HandleNoRequest(AContext: TWeatherContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  O: ISuperObject;
begin
  O:= SO;
  try
    O.S['error']:= 'No document was requested. Please include at least one document in the request.';
  finally
    AResponseInfo.ContentText:= O.AsJSon(True);
    AResponseInfo.ContentType:= 'text/json';
  end;
end;

procedure TJDWeatherApiSvrThread.SvrCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  C: TWeatherContext;
  X: Integer;
  D, T: String;
  P: Integer;
begin
  C:= TWeatherContext(AContext);

  //Read Params
  C.FKey:= ARequestInfo.Params.Values['k'];
  C.FServiceUID:= ARequestInfo.Params.Values['s'];
  C.FLocType:= ARequestInfo.Params.Values['l'];
  C.FLoc1:= ARequestInfo.Params.Values['l1'];
  C.FLoc2:= ARequestInfo.Params.Values['l2'];
  C.FUnits:= ARequestInfo.Params.Values['u'];
  C.FDet:= ARequestInfo.Params.Values['d'];

  //Read Requested Document(s)
  C.FDoc.Clear;
  D:= ARequestInfo.Document;
  Delete(D, 1, 1);
  D:= D + '/';
  while Length(D) > 0 do begin
    P:= Pos('/', D);
    T:= Copy(D, 1, P-1);
    Delete(D, 1, P);
    C.FDoc.Append(T);
  end;

  //Identify Service
  C.FService:= nil;
  for X := 0 to C.FWeather.Services.Count-1 do begin
    if C.FWeather.Services[X].Info.UID = C.FServiceUID then begin
      C.FService:= C.FWeather.Services[X];
      Break;
    end;
  end;

  //Handle Request
  if C.FDoc.Count > 0 then begin
    if C.FDoc[0] = 'services' then begin
      HandleServiceList(C, ARequestInfo, AResponseInfo);
    end else
    if C.FDoc[0] = 'support' then begin
      HandleServiceSupport(C, ARequestInfo, AResponseInfo);
    end else begin
      HandleServiceGet(C, ARequestInfo, AResponseInfo);
    end;
  end else begin
    HandleNoRequest(C, ARequestInfo, AResponseInfo);
  end;
end;

end.
