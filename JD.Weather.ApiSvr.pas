unit JD.Weather.ApiSvr;

interface

uses
  Winapi.Windows, Winapi.ActiveX,
  System.Classes, System.SysUtils, System.TypInfo,
  JD.Weather.Intf, JD.Weather.SuperObject,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdCustomHTTPServer,
  IdHTTPServer, IdContext, IdTCPConnection, IdYarn, IdSocketHandle,
  IdServerIOHandler, IdSSL, IdSSLOpenSSL,
  Data.DB, Data.Win.ADODB,
  JD.Weather.Logger;

type
  TJDWeatherApiSvrThread = class;

  TLogEvent = procedure(Sender: TObject; const Timestamp: TDateTime;
    const Msg: String) of object;

  TWeatherContext = class(TIdServerContext)
  private
    FThread: TJDWeatherApiSvrThread;
    FDB: TADOConnection;
    FKey: WideString;
    FKeyID: Integer;
    FUserID: Integer;
    FUserName: WideString;
    FLocType: WideString;
    FLoc1: WideString;
    FLoc2: WideString;
    FUnits: WideString;
    FDet: WideString;
    FDoc: TStringList;
    FWeather: IJDWeather;
    FServices: IWeatherMultiService;
    procedure HandleGet(
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleRequest(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);

    procedure HandleServiceList(
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleServiceSupport(
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleServiceLogo(
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleUsage(
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleAccount(
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    procedure HandleNoRequest(
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleConditions(
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleAlerts(
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleNoKey(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleInvalidKey(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    function GetConditions: ISuperObject;
    function GetAlerts: ISuperObject;
    procedure SaveReq(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    function GetSupport(const S: IWeatherService): ISuperObject;
  public
    constructor Create(AConnection: TIdTCPConnection; AYarn: TIdYarn;
      AList: TIdContextThreadList = nil); override;
    destructor Destroy; override;
    procedure Log(const Msg: String);
    function NewQuery: TADOQuery;
  end;

  TJDWeatherApiSvrThread = class(TThread)
  private
    FLib: HMODULE;
    FCreateLib: TCreateJDWeather;
    FSSL: TIdServerIOHandlerSSLOpenSSL;
    FServer: TIdHTTPServer;
    FLogTime: TDateTime;
    FLogMsg: String;
    FConnStr: String;
    FPort: Integer;
    FOnLog: TLogEvent;
    procedure Init;
    procedure UnInit;
    procedure Process;
    procedure SvrCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure SvrContextCreated(AContext: TIdContext);
    procedure SetConnStr(const Value: String);
    procedure SetPort(const Value: Integer);
    procedure SvrGetPassword(var Password: String);
  protected
    procedure Execute; override;
    procedure SYNC_OnLog;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Log(const Msg: String);
  public
    property ConnStr: String read FConnStr write SetConnStr;
    property Port: Integer read FPort write SetPort;
    property OnLog: TLogEvent read FOnLog write FOnLog;
  end;

implementation

function StrToLocationType(const S: String): TWeatherLocationType;
begin
  if SameText(S, '') then Result:= TWeatherLocationType.wlAutoIP
  else if SameText(S, 'city') then Result:= TWeatherLocationType.wlCityState
  else if SameText(S, 'coords') then Result:= TWeatherLocationType.wlCoords
  else if SameText(S, 'zip') then Result:= TWeatherLocationType.wlZip
  else if SameText(S, 'ip') then Result:= TWeatherLocationType.wlAutoIP
  else if SameText(S, 'countrycity') then Result:= TWeatherLocationType.wlCountryCity
  else if SameText(S, 'airport') then Result:= TWeatherLocationType.wlAirportCode
  else if SameText(S, 'pws') then Result:= TWeatherLocationType.wlPWS
  else Result:= TWeatherLocationType.wlAutoIP;
end;

function StrToUnits(const S: String): TWeatherUnits;
begin
  if SameText(S, '') then Result:= wuImperial
  else if SameText(S, 'imperial') then Result:= wuImperial
  else if SameText(S, 'metric') then Result:= wuMetric
  else if SameText(S, 'kelvin') then Result:= wuKelvin
  else Result:= wuImperial;
end;

{ TJDWeatherApiSvrThread }

constructor TJDWeatherApiSvrThread.Create;
begin
  inherited Create(True);
  FPort:= 8664;
end;

destructor TJDWeatherApiSvrThread.Destroy;
begin

  inherited;
end;

procedure TJDWeatherApiSvrThread.Init;
var
  E: Integer;
  //B: TIdSocketHandle;
begin
  CoInitialize(nil);

  FSSL:= TIdServerIOHandlerSSLOpenSSL.Create(nil);
  FSSL.SSLOptions.CertFile := 'C:\demo\my-pubcert.pem';
  FSSL.SSLOptions.KeyFile := 'C:\demo\my-pubcert.pem';
  FSSL.SSLOptions.RootCertFile := 'C:\demo\my-pubcert.pem';
  FSSL.SSLOptions.Method := sslvSSLv23;
  FSSL.SSLOptions.Mode := sslmServer;
  FSSL.OnGetPassword:= SvrGetPassword;

  FServer:= TIdHTTPServer.Create(nil);
  FServer.ContextClass:= TWeatherContext;
  FServer.DefaultPort:= FPort;
  FServer.KeepAlive:= True;
  //FServer.IOHandler:= FSSL; //TODO: Properly support HTTPS
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

procedure TJDWeatherApiSvrThread.Log(const Msg: String);
begin
  //TODO: Implement new logger

  //PostLog(0, Msg);

  {
  FLogTime:= Now;
  FLogMsg:= Msg;
  Synchronize(SYNC_OnLog);
  }
end;

procedure TJDWeatherApiSvrThread.UnInit;
begin
  //FreeLibrary(FLib);
  FServer.Active:= False;
  FreeAndNil(FServer);
  CoUninitialize;
end;

procedure TJDWeatherApiSvrThread.Process;
begin
  Sleep(10);
end;

procedure TJDWeatherApiSvrThread.Execute;
begin
  while not Terminated do begin
    try
      Init;
      try
        while not Terminated do begin
          Process;
        end;
      finally
        Uninit;
      end;
      Sleep(10);
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
  C.FThread:= Self;
  C.FDB.ConnectionString:= FConnStr;
  C.FDB.Connected:= True;
end;

procedure TJDWeatherApiSvrThread.SvrGetPassword(var Password: String);
begin
  Password:= '';
end;

procedure TJDWeatherApiSvrThread.SYNC_OnLog;
begin
  if Assigned(FOnLog) then
    FOnLog(Self, FLogTime, FLogMsg);
end;

procedure TJDWeatherApiSvrThread.SetConnStr(const Value: String);
begin
  FConnStr := Value;
end;

procedure TJDWeatherApiSvrThread.SetPort(const Value: Integer);
begin
  FPort := Value;
end;

procedure TJDWeatherApiSvrThread.SvrCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  C: TWeatherContext;
begin
  C:= TWeatherContext(AContext);
  C.HandleGet(ARequestInfo, AResponseInfo);
end;

{ TWeatherContext }

constructor TWeatherContext.Create(AConnection: TIdTCPConnection;
  AYarn: TIdYarn; AList: TIdContextThreadList);
begin
  inherited;
  CoInitialize(nil);
  FDB:= TADOConnection.Create(nil);
  FDB.LoginPrompt:= False;
  FServices:= TWeatherMultiService.Create;
  FServices._AddRef;
  FDoc:= TStringList.Create;
  FWeather:= nil;
end;

destructor TWeatherContext.Destroy;
begin
  if Assigned(FWeather) then begin
    FWeather._Release;
    FWeather:= nil;
  end;
  FServices._Release;
  FServices:= nil;
  FreeAndNil(FDoc);
  FDB.Connected:= False;
  FreeAndNil(FDB);
  CoUninitialize;
  inherited;
end;

procedure TWeatherContext.SaveReq(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Q: TADOQuery;
  S: TStringStream;
begin
  Q:= NewQuery;
  try
    Q.SQL.Text:= 'select * from Requests where 1<>1';
    Q.Open;
    try
      Q.Append;
      try
        Q['Timestamp']:= Now;
        Q['RemoteIP']:= Connection.Socket.Binding.PeerIP;
        Q['RemotePort']:= Connection.Socket.Binding.PeerPort;
        Q['Command']:= ARequestInfo.Command;
        Q['Doc']:= ARequestInfo.Document;
        Q['Qry']:= ARequestInfo.Params.Text;
        if Assigned(ARequestInfo.PostStream) then begin
          S:= TStringStream.Create;
          try
            ARequestInfo.PostStream.Position:= 0;
            S.LoadFromStream(ARequestInfo.PostStream);
            S.Position:= 0;
            Q['ReqContent']:= S.DataString;
          finally
            FreeAndNil(S);
          end;
        end;
        Q['ReqContentType']:= ARequestInfo.ContentType;
        Q['KeyID']:= FKeyID;
        Q['UserID']:= FUserID;
        Q['ResponseCode']:= AResponseInfo.ResponseNo;
        Q['Response']:= AResponseInfo.ContentText;
        Q['ResponseType']:= AResponseInfo.ContentType;

      finally
        Q.Post;
      end;
    finally
      Q.Close;
    end;
  finally
    FreeAndNil(Q);
  end;
end;

procedure TWeatherContext.HandleRequest(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  //D, T: String;
  //P: Integer;
  Q: TADOQuery;
  X: Integer;
  O: ISuperObject;
begin
  Q:= NewQuery;
  try

    if (FDoc.Count > 2) then begin

      O:= SO;
      try
        //Handle multi-info request

        //TODO - Each service will need its "GetMulti" function implemented.




        Self.FServices[0].GetMultiple([TWeatherInfoType.wiConditions]);
        //Self.FServices.GetCombinedMulti(); //TODO

        for X := 1 to FDoc.Count - 1 do begin
          if SameText(FDoc[X], 'conditions') then begin
            O.O['conditions']:= Self.GetConditions;
          end else
          if SameText(FDoc[X], 'alerts') then begin
            O.O['alerts']:= Self.GetAlerts;
          end else
          if SameText(FDoc[X], 'forecastsummary') then begin
            //O.O['forecastsummary']:= Self.GetForecastSummary;
          end else begin
            //Unrecognized
          end;
        end;

      finally
        AResponseInfo.ContentText:= O.AsJSon(True);
        AResponseInfo.ContentType:= 'text/json';
      end;

    end else begin
      if SameText(FDoc[1], 'conditions') then begin
        HandleConditions(ARequestInfo, AResponseInfo);
      end else
      if SameText(FDoc[1], 'alerts') then begin
        HandleAlerts(ARequestInfo, AResponseInfo);
      end else
      if SameText(FDoc[1], 'forecastsummary') then begin
        //HandleForecastSummary();
      end else
      if SameText(FDoc[1], 'forecasthourly') then begin

      end else
      if SameText(FDoc[1], 'forecastdaily') then begin

      end else
      if SameText(FDoc[1], 'maps') then begin

      end else
      if SameText(FDoc[1], 'geolookup') then begin

      end else
      if SameText(FDoc[1], 'history') then begin

      end else
      if SameText(FDoc[1], 'usage') then begin
        HandleUsage(ARequestInfo, AResponseInfo);
      end else
      if SameText(FDoc[1], 'account') then begin
        HandleAccount(ARequestInfo, AResponseInfo);
      end else begin
        HandleNoRequest(ARequestInfo, AResponseInfo);
      end;
    end;
  finally
    FreeAndNil(Q);
  end;
end;

procedure TWeatherContext.HandleGet(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  //X: Integer;
  D, T: String;
  P: Integer;
  Q: TADOQuery;
  X: Integer;
begin
  ARequestInfo.Params.Delimiter:= '&';
  Log('GET '+ARequestInfo.RemoteIP+' - '+ARequestInfo.URI+'?'+ARequestInfo.Params.DelimitedText);

  //Read Params
  FLocType:= ARequestInfo.Params.Values['l'];
  FLoc1:= ARequestInfo.Params.Values['l1'];
  FLoc2:= ARequestInfo.Params.Values['l2'];
  FUnits:= ARequestInfo.Params.Values['u'];
  FDet:= ARequestInfo.Params.Values['d'];

  //Read Requested Document(s)
  FDoc.Clear;
  D:= ARequestInfo.Document;
  Delete(D, 1, 1);
  D:= D + '/';
  while Length(D) > 0 do begin
    P:= Pos('/', D);
    T:= Copy(D, 1, P-1);
    Delete(D, 1, P);
    FDoc.Append(T);
  end;

  AResponseInfo.CustomHeaders.Values['Access-Control-Allow-Origin']:= '*';

  FServices.Clear;

  //Parse service list
  FWeather.Services.LoadServices(ExtractFilePath(ParamStr(0)));
  D:= ARequestInfo.Params.Values['s']+',';
  Q:= NewQuery;
  try
    while Length(D) > 0 do begin
      P:= Pos(',', D);
      T:= Copy(D, 1, P-1);
      Delete(D, 1, P);
      for X := 0 to FWeather.Services.Count-1 do begin
        if SameText(FWeather.Services[X].Info.Name, T) then begin
          //Q.Parameters.Clear;
          Q.SQL.Text:= 'select * from ServiceKeys where UserID = :u '+
            'and ServiceID = (select ID from Services where Name = :s)';
          Q.Parameters.ParamValues['u']:= FUserID;
          Q.Parameters.ParamValues['s']:= T;
          Q.Open;
          try
            FWeather.Services[X].Key:= Q.FieldByName('ApiKey').AsString;
            FWeather.Services[X].Units:= StrToUnits(FUnits);

            //These 3 lines were missing...
            FWeather.Services[X].LocationType:= StrToLocationType(FLocType);
            FWeather.Services[X].LocationDetail1:= FLoc1;
            FWeather.Services[X].LocationDetail2:= FLoc2;

            //But these 3 lines should have worked...
            FWeather.LocationType:= StrToLocationType(FLocType);
            FWeather.LocationDetail1:= FLoc1;
            FWeather.LocationDetail2:= FLoc2;

            FServices.Add(FWeather.Services[X]);
          finally
            Q.Close;
          end;
          Break;
        end;
      end;
    end;
  finally
    FreeAndNil(Q);
  end;


  Q:= NewQuery;
  try
    if FDoc.Count > 0 then begin

      if SameText(FDoc[0], 'services') then begin
        HandleServiceList(ARequestInfo, AResponseInfo);
      end else
      if Sametext(FDoc[0], 'support') then begin
        HandleServiceSupport(ARequestInfo, AResponseInfo);
      end else
      if SameText(FDoc[0], 'logo') then begin
        HandleServiceLogo(ARequestInfo, AResponseInfo);
      end else begin
        FKey:= FDoc[0];
        if FDoc.Count > 1 then begin
          Q.SQL.Text:= 'select K.*, U.Username from ApiKeys K join Users U on U.ID = K.UserID'+
            ' where K.ApiKey = :key and K.Status = 1';
          Q.Parameters.ParamValues['key']:= FKey;
          Q.Open;
          try
            if not Q.IsEmpty then begin
              FUserID:= Q.FieldByName('UserID').AsInteger;
              FUserName:= Q.FieldByName('Username').AsString;
              FKeyID:= Q.FieldByName('ID').AsInteger;


              //Actual handling of JSON requests which require key
              HandleRequest(ARequestInfo, AResponseInfo);

            end else begin
              HandleInvalidKey(ARequestInfo, AResponseInfo);
            end;
          finally
            Q.Close;
          end;
        end else begin
          HandleNoRequest(ARequestInfo, AResponseInfo);
        end;
      end;

    end else begin
      HandleNoKey(ARequestInfo, AResponseInfo);
    end;
  finally
    FreeAndNil(Q);
  end;

  SaveReq(ARequestInfo, AResponseInfo);

end;

function TWeatherContext.GetSupport(const S: IWeatherService): ISuperObject;
var
  O2: ISuperObject;
  Loc: TWeatherLocationType;
  Inf: TWeatherInfoType;
  Uni: TWeatherUnits;
  Alt: TWeatherAlertType;
  Alp: TWeatherAlertProp;
  Foc: TWeatherForecastType;
  Fop: TWeatherPropType;
  Map: TWeatherMapType;
  Maf: TWeatherMapFormat;
begin
  Result:= SO;

  O2:= SA([]);
  try
    for Loc := Low(TWeatherLocationType) to High(TWeatherLocationType) do begin
      if Loc in S.Info.Support.SupportedLocations then
        O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherLocationType), Ord(Loc))));
    end;
  finally
    Result.O['supported_locations']:= O2;
  end;

  O2:= SA([]);
  try
    for Fop := Low(TWeatherPropType) to High(TWeatherPropType) do begin
      if Fop in S.Info.Support.SupportedConditionProps then
        O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherPropType), Ord(Fop))));
    end;
  finally
    Result.O['supported_condition_props']:= O2;
  end;

  O2:= SA([]);
  try
    for Inf := Low(TWeatherInfoType) to High(TWeatherInfoType) do begin
      if Inf in S.Info.Support.SupportedInfo then
        O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherInfoType), Ord(Inf))));
    end;
  finally
    Result.O['supported_info']:= O2;
  end;

  O2:= SA([]);
  try
    for Uni := Low(TWeatherUnits) to High(TWeatherUnits) do begin
      if Uni in S.Info.Support.SupportedUnits then
        O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherUnits), Ord(Uni))));
    end;
  finally
    Result.O['supported_units']:= O2;
  end;

  O2:= SA([]);
  try
    for Alt := Low(TWeatherAlertType) to High(TWeatherAlertType) do begin
      if Alt in S.Info.Support.SupportedAlerts then
        O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherAlertType), Ord(Alt))));
    end;
  finally
    Result.O['supported_alerts']:= O2;
  end;

  O2:= SA([]);
  try
    for Alp := Low(TWeatherAlertProp) to High(TWeatherAlertProp) do begin
      if Alp in S.Info.Support.SupportedAlertProps then
        O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherAlertProp), Ord(Alp))));
    end;
  finally
    Result.O['supported_alert_props']:= O2;
  end;

  O2:= SA([]);
  try
    for Foc := Low(TWeatherForecastType) to High(TWeatherForecastType) do begin
      if Foc in S.Info.Support.SupportedForecasts then
        O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherForecastType), Ord(Foc))));
    end;
  finally
    Result.O['supported_forecasts']:= O2;
  end;

  O2:= SA([]);
  try
    for Fop := Low(TWeatherPropType) to High(TWeatherPropType) do begin
      if Fop in S.Info.Support.SupportedForecastSummaryProps then
        O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherPropType), Ord(Fop))));
    end;
  finally
    Result.O['supported_forecast_summary_props']:= O2;
  end;

  O2:= SA([]);
  try
    for Fop := Low(TWeatherPropType) to High(TWeatherPropType) do begin
      if Fop in S.Info.Support.SupportedForecastHourlyProps then
        O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherPropType), Ord(Fop))));
    end;
  finally
    Result.O['supported_forecast_hourly_props']:= O2;
  end;

  O2:= SA([]);
  try
    for Fop := Low(TWeatherPropType) to High(TWeatherPropType) do begin
      if Fop in S.Info.Support.SupportedForecastDailyProps then
        O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherPropType), Ord(Fop))));
    end;
  finally
    Result.O['supported_forecast_daily_props']:= O2;
  end;

  O2:= SA([]);
  try
    for Map := Low(TWeatherMapType) to High(TWeatherMapType) do begin
      if Map in S.Info.Support.SupportedMaps then
        O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherMapType), Ord(Map))));
    end;
  finally
    Result.O['supported_maps']:= O2;
  end;

  O2:= SA([]);
  try
    for Maf := Low(TWeatherMapFormat) to High(TWeatherMapFormat) do begin
      if Maf in S.Info.Support.SupportedMapFormats then
        O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherMapFormat), Ord(Maf))));
    end;
  finally
    Result.O['supported_map_formats']:= O2;
  end;

end;

procedure TWeatherContext.HandleServiceSupport(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  O: ISuperObject;
  Svc: IWeatherService;
begin
  O:= SO;
  try
    if FServices.Count > 0 then begin
      Svc:= FServices[0];

      O.S['caption']:= Svc.Info.Caption;
      O.S['name']:= Svc.Info.Name;
      O.S['serviceuid']:= Svc.Info.UID;

      O.O['support']:= GetSupport(Svc);

    end else begin
      O.S['error']:= 'No services were specified.';
    end;

  finally
    AResponseInfo.ContentText:= O.AsJSon(True);
    AResponseInfo.ContentType:= 'text/json';
  end;
end;

procedure TWeatherContext.HandleUsage(ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin

end;

procedure TWeatherContext.HandleServiceList(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  O, O2: ISuperObject;
  X: Integer;
  S: IWeatherService;
begin
  O:= SO;
  try
    O.O['services']:= SA([]);
    for X := 0 to FWeather.Services.Count-1 do begin
      S:= FWeather.Services[X];
      O2:= SO;
      try
        O2.S['caption']:= S.Info.Caption;
        O2.S['name']:= S.Info.Name;
        O2.S['uid']:= S.Info.UID;
        O2.S['url_main']:= S.Info.URLs.MainURL;
        O2.S['url_api']:= S.Info.URLs.ApiURL;
        O2.S['url_login']:= S.Info.URLs.LoginURL;
        O2.S['url_register']:= S.Info.URLs.RegisterURL;
        O2.S['url_legal']:= S.Info.URLs.LegalURL;
        O2.S['url_power']:= S.Info.URLs.PowerURL;
        O2.S['url_usage']:= S.Info.URLs.UsageURL;
        if ARequestInfo.Params.Values['support'] = '1' then begin
          O2.O['support']:= GetSupport(S);
        end;
      finally
        O.O['services'].AsArray.Add(O2);
      end;
    end;
  finally
    AResponseInfo.ContentText:= O.AsJSon(True);
    AResponseInfo.ContentType:= 'text/json';
  end;
end;

procedure TWeatherContext.HandleServiceLogo(ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  S: TStringStream;
  //X: Integer;
  G: IWeatherGraphic;
  Svc: IWeatherService;
begin
  S:= TStringStream.Create;
  try
    try
      if FServices.Count > 0 then begin
        Svc:= FServices[0];
        G:= Svc.Info.GetLogo(TWeatherLogoType.ltColor);
        if Assigned(G) then begin
          S.WriteString(G.Base64);
        end;
      end else begin
        //No service requested

      end;
    except
      on E: Exception do begin

      end;
    end;
  finally
    AResponseInfo.ContentType:= 'image/png';
    S.Position:= 0;
    AResponseInfo.ContentStream:= S;
  end;
end;

procedure TWeatherContext.Log(const Msg: String);
begin
  if Assigned(FThread) then
    FThread.Log(Msg);
end;

function TWeatherContext.NewQuery: TADOQuery;
begin
  Result:= TADOQuery.Create(nil);
  Result.Connection:= FDB;
end;

function TWeatherContext.GetAlerts: ISuperObject;
var
  A: IWeatherAlerts;
  Al: IWeatherAlert;
  O: ISuperObject;
  X: Integer;
begin
  Result:= SO;

  A:= FServices.GetCombinedAlerts;
  if Assigned(A) then begin
    for X := 0 to A.Count-1 do begin
      Al:= A[X];
      O:= SO;
      try
        O.S['type']:= WeatherAlertTypeToStr(Al.AlertType);
        O.S['caption']:= Al.Description;
        O.S['message']:= Al.Msg;
        O.D['expires']:= Al.Expires;
      finally
        Result.AsArray.Add(O);
      end;
    end;
  end else begin
    //A is not assigned!
    //TODO
    Result.S['error']:= 'FServices.GetCombinedAlerts did not return any data!';
  end;

end;

function TWeatherContext.GetConditions: ISuperObject;
var
  C: IWeatherProps;
  procedure ChkD(const N: String; Prop: TWeatherPropType; const V: Double);
  begin
    if Prop in C.Support then
      Result.D[N]:= V;
  end;
begin
  Result:= SO;

  C:= FServices.GetCombinedConditions;
  if Assigned(C) then begin
    {
      wpIcon: ;
      wpCaption: ;
      wpDescription: ;
      wpDetails: ;
      wpURL: ;
      wpStation: ;
      wpPropsMin: ;
      wpPropsMax: ;
    }
    Result.S['timestamp']:= FormatDateTime('mm-dd-yyyy hh:nn AMPM', C.DateTime);
    Result.S['caption']:= C.Caption;
    Result.S['description']:= C.Description;
    Result.S['details']:= C.Details;
    Result.S['url']:= C.URL;
    Result.S['station']:= C.Station;
    Result.S['icon_url']:= ''; //TODO
    ChkD('temp', wpTemp, C.Temp);
    ChkD('temp_min', wpTempMin, C.TempMin);
    ChkD('temp_max', wpTempMax, C.TempMax);
    ChkD('feels_like', wpFeelsLike, C.FeelsLike);
    ChkD('feels_like_sun', wpFeelsLikeSun, C.FeelsLikeSun);
    ChkD('feels_like_shade', wpFeelsLikeShade, C.FeelsLikeShade);
    ChkD('rel_humidity', wpHumidity, C.Humidity);
    ChkD('visibility', wpVisibility, C.Visibility);
    ChkD('dew_point', wpDewPoint, C.DewPoint);
    ChkD('wind_dir', wpWindDir, C.WindDir);
    ChkD('wind_speed', wpWindSpeed, C.WindSpeed);
    ChkD('wind_gusts', wpWindGust, C.WindGusts);
    ChkD('wind_chill', wpWindChill, C.WindChill);
    ChkD('heat_index', wpHeatIndex, C.HeatIndex);
    ChkD('pressure', wpPressure, C.Pressure);
    ChkD('pressure_ground', wpPressureGround, C.PressureGround);
    ChkD('pressure_sea', wpPressureSea, C.PressureSea);
    ChkD('solar_radiation', wpSolarRad, C.SolarRad);
    ChkD('uv_index', wpUVIndex, C.UVIndex);
    ChkD('cloud_cover', wpCloudCover, C.CloudCover);
    ChkD('precip_amt', wpPrecipAmt, C.PrecipAmt);
    ChkD('rain_amt', wpRainAmt, C.RainAmt);
    ChkD('snow_amt', wpSnowAmt, C.SnowAmt);
    ChkD('ice_amt', wpIceAmt, C.IceAmt);
    ChkD('sleet_amt', wpSleetAmt, C.SleetAmt);
    ChkD('fog_amt', wpFogAmt, C.FogAmt);
    ChkD('storm_amt', wpStormAmt, C.StormAmt);
    ChkD('precip_pred', wpPrecipPred, C.PrecipPred);
    ChkD('rain_pred', wpRainPred, C.RainPred);
    ChkD('snow_pred', wpSnowPred, C.SnowPred);
    ChkD('ice_pred', wpIcePred, C.IcePred);
    ChkD('sleet_pred', wpSleetPred, C.SleetPred);
    ChkD('fog_pred', wpFogPred, C.FogPred);
    ChkD('storm_pred', wpStormPred, C.StormPred);
    ChkD('wet_bulb', wpWetBulb, C.WetBulb);
    ChkD('ceiling', wpCeiling, C.Ceiling);
    //ChkD('sunrise', wpSunrise, C);
    //ChkD('sunset', wpSunset, C);
    ChkD('daylight', wpDaylight, C.Daylight);

  end else begin
    Result.S['error']:= 'Conditions object was not assigned!';
  end;

end;

procedure TWeatherContext.HandleAccount(ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  O, A, K, S: ISuperObject;
  X: Integer;
begin
  O:= SO;
  try
    A:= SO;
    try
      A.S['key']:= Self.FKey;
      A.S['user_name']:= Self.FUserName;
      K:= SA([]);
      try
        for X := 0 to FServices.Count-1 do begin
          S:= SO;
          try
            S.S['service_name']:= FServices[X].Info.Name;
            S.S['service_caption']:= FServices[X].Info.Caption;
            S.S['service_uid']:= FServices[X].Info.UID;
            S.S['key']:= FServices[X].Key;
          finally
            K.AsArray.Add(S);
          end;
        end;
      finally
        A.O['keys']:= K;
      end;
    finally
      O.O['account']:= A;
    end;
  finally
    AResponseInfo.ContentText:= O.AsJSon(True);
    AResponseInfo.ContentType:= 'text/json';
  end;
end;

procedure TWeatherContext.HandleAlerts(ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  O: ISuperObject;
begin
  O:= SO;
  try
    O.O['alerts']:= GetAlerts;
  finally
    AResponseInfo.ContentText:= O.AsJSon(True);
    AResponseInfo.ContentType:= 'text/json';
  end;
end;

procedure TWeatherContext.HandleConditions(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  O: ISuperObject;
begin
  O:= SO;
  try
    O.O['conditions']:= GetConditions;
  finally
    AResponseInfo.ContentText:= O.AsJSon(True);
    AResponseInfo.ContentType:= 'text/json';
  end;
end;

procedure TWeatherContext.HandleInvalidKey(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  O: ISuperObject;
begin
  O:= SO;
  try
    O.S['error']:= 'Invalid API Key.';
  finally
    AResponseInfo.ContentText:= O.AsJSon(True);
    AResponseInfo.ContentType:= 'text/json';
  end;
end;

procedure TWeatherContext.HandleNoRequest(
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

procedure TWeatherContext.HandleNoKey(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  O: ISuperObject;
begin
  O:= SO;
  try
    O.S['error']:= 'No API Key was specified.';
  finally
    AResponseInfo.ContentText:= O.AsJSon(True);
    AResponseInfo.ContentType:= 'text/json';
  end;
end;

end.
