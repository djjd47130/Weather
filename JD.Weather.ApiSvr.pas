unit JD.Weather.ApiSvr;

interface

uses
  Winapi.Windows, Winapi.ActiveX,
  System.Classes, System.SysUtils, System.TypInfo,
  JD.Weather.Intf, JD.Weather.SuperObject,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdCustomHTTPServer,
  IdHTTPServer, IdContext, IdTCPConnection, IdYarn, IdSocketHandle,
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
    FUserID: Integer;
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
    procedure HandleServiceList(
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleServiceSupport(
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleNoRequest(
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleConditions(
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleNoKey(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleInvalidKey(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleRequest(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleUI(ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    function GetConditions: ISuperObject;
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
  B: TIdSocketHandle;
begin
  CoInitialize(nil);
  FServer:= TIdHTTPServer.Create(nil);
  FServer.ContextClass:= TWeatherContext;
  FServer.DefaultPort:= FPort;
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

procedure TWeatherContext.HandleRequest(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  X: Integer;
  D, T: String;
  P: Integer;
  Q: TADOQuery;
begin
  Q:= NewQuery;
  try
    //Parse service list
    FWeather.Services.LoadServices(ExtractFilePath(ParamStr(0)));
    D:= ARequestInfo.Params.Values['s']+',';
    while Length(D) > 0 do begin
      P:= Pos(',', D);
      T:= Copy(D, 1, P-1);
      Delete(D, 1, P);
      for X := 0 to FWeather.Services.Count-1 do begin
        if SameText(FWeather.Services[X].Info.Name, T) then begin
          Q.Parameters.Clear;
          Q.SQL.Text:= 'select * from ServiceKeys where UserID = :u and ServiceID = (select ID from Services where Name = :s)';
          Q.Parameters.ParamValues['u']:= FUserID;
          Q.Parameters.ParamValues['s']:= T;
          Q.Open;
          try
            FWeather.Services[X].Key:= Q.FieldByName('ApiKey').AsString;
            FWeather.Services[X].Units:= StrToUnits(FUnits);
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

    if SameText(FDoc[1], 'services') then begin
      HandleServiceList(ARequestInfo, AResponseInfo);
    end else
    if SameText(FDoc[1], 'support') then begin
      HandleServiceSupport(ARequestInfo, AResponseInfo);
    end else
    if SameText(FDoc[1], 'conditions') then begin
      HandleConditions(ARequestInfo, AResponseInfo);
    end else begin
      HandleNoRequest(ARequestInfo, AResponseInfo);
    end;

  finally
    FreeAndNil(Q);
  end;
end;

procedure TWeatherContext.HandleGet(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  X: Integer;
  D, T: String;
  P: Integer;
  Q: TADOQuery;
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

  Q:= NewQuery;
  try
    if FDoc.Count > 0 then begin
      if SameText(FDoc[0], 'ui') then begin
        HandleUI(ARequestInfo, AResponseInfo);
      end else begin
        if FDoc.Count > 1 then begin
          FKey:= FDoc[0];
          Q.SQL.Text:= 'select * from ApiKeys where ApiKey = :key and Status = 1';
          Q.Parameters.ParamValues['key']:= FKey;
          Q.Open;
          try
            if not Q.IsEmpty then begin
              FUserID:= Q.FieldByName('UserID').AsInteger;
              HandleRequest(ARequestInfo, AResponseInfo);
            end else begin
              HandleInvalidKey(ARequestInfo, AResponseInfo);
            end;
          finally
            Q.Close;
          end;
        end else begin
          if SameText(FDoc[0], 'favicon.ico') then begin
            //TODO: Return favicon
          end else begin
            HandleNoKey(ARequestInfo, AResponseInfo);
          end;
        end;
      end;
    end else begin
      HandleNoRequest(ARequestInfo, AResponseInfo);
    end;

  finally
    FreeAndNil(Q);
  end;
end;

procedure TWeatherContext.HandleServiceSupport(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Svc: IWeatherService;
  O, O2: ISuperObject;
  Loc: TWeatherLocationType;
  Inf: TWeatherInfoType;
  Uni: TWeatherUnits;
  Alt: TWeatherAlertType;
  Alp: TWeatherAlertProp;
  Fop: TWeatherPropType;
  Map: TWeatherMapType;
begin
  O:= SO;
  try
    if FServices.Count > 0 then begin
      Svc:= FServices[0];

      O.S['caption']:= Svc.Info.Caption;
      O.S['name']:= Svc.Info.Name;
      O.S['serviceuid']:= Svc.Info.UID;

      O2:= SA([]);
      try
        for Loc := Low(TWeatherLocationType) to High(TWeatherLocationType) do begin
          if Loc in Svc.Info.Support.SupportedLocations then
            O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherLocationType), Ord(Loc))));
        end;
      finally
        O.O['supported_locations']:= O2;
      end;

      O2:= SA([]);
      try
        for Inf := Low(TWeatherInfoType) to High(TWeatherInfoType) do begin
          if Inf in Svc.Info.Support.SupportedInfo then
            O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherInfoType), Ord(Inf))));
        end;
      finally
        O.O['supported_info']:= O2;
      end;

      O2:= SA([]);
      try
        for Uni := Low(TWeatherUnits) to High(TWeatherUnits) do begin
          if Uni in Svc.Info.Support.SupportedUnits then
            O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherUnits), Ord(Uni))));
        end;
      finally
        O.O['supported_units']:= O2;
      end;

      O2:= SA([]);
      try
        for Alt := Low(TWeatherAlertType) to High(TWeatherAlertType) do begin
          if Alt in Svc.Info.Support.SupportedAlerts then
            O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherAlertType), Ord(Alt))));
        end;
      finally
        O.O['supported_alerts']:= O2;
      end;

      O2:= SA([]);
      try
        for Alp := Low(TWeatherAlertProp) to High(TWeatherAlertProp) do begin
          if Alp in Svc.Info.Support.SupportedAlertProps then
            O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherAlertProp), Ord(Alp))));
        end;
      finally
        O.O['supported_alert_props']:= O2;
      end;

      O2:= SA([]);
      try
        for Fop := Low(TWeatherPropType) to High(TWeatherPropType) do begin
          if Fop in Svc.Info.Support.SupportedForecastSummaryProps then
            O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherPropType), Ord(Fop))));
        end;
      finally
        O.O['supported_forecast_summary_props']:= O2;
      end;

      O2:= SA([]);
      try
        for Fop := Low(TWeatherPropType) to High(TWeatherPropType) do begin
          if Fop in Svc.Info.Support.SupportedForecastHourlyProps then
            O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherPropType), Ord(Fop))));
        end;
      finally
        O.O['supported_forecast_hourly_props']:= O2;
      end;

      O2:= SA([]);
      try
        for Fop := Low(TWeatherPropType) to High(TWeatherPropType) do begin
          if Fop in Svc.Info.Support.SupportedForecastDailyProps then
            O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherPropType), Ord(Fop))));
        end;
      finally
        O.O['supported_forecast_daily_props']:= O2;
      end;

      O2:= SA([]);
      try
        for Map := Low(TWeatherMapType) to High(TWeatherMapType) do begin
          if Map in Svc.Info.Support.SupportedMaps then
            O2.AsArray.Add(SO(GetEnumName(TypeInfo(TWeatherMapType), Ord(Map))));
        end;
      finally
        O.O['supported_maps']:= O2;
      end;

    end else begin
      O.S['error']:= 'No services were specified.';
    end;

  finally
    AResponseInfo.ContentText:= O.AsJSon(True);
    AResponseInfo.ContentType:= 'text/json';
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
      finally
        O.O['services'].AsArray.Add(O2);
      end;
    end;
  finally
    AResponseInfo.ContentText:= O.AsJSon(True);
    AResponseInfo.ContentType:= 'text/json';
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

procedure TWeatherContext.HandleConditions(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  O: ISuperObject;
  C: IWeatherProps;
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

procedure TWeatherContext.HandleUI(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  L: TStringList;
  R: TResourceStream;
  procedure DoScript;
  begin
    R:= TResourceStream.Create(HInstance, 'SCRIPT_JS', 'JS');
    try
      R.Position:= 0;
      L.LoadFromStream(R);
    finally
      FreeAndNil(R);
    end;
  end;
  procedure DoStyles;
  begin
    R:= TResourceStream.Create(HInstance, 'STYLES_CSS', 'CSS');
    try
      R.Position:= 0;
      L.LoadFromStream(R);
    finally
      FreeAndNil(R);
    end;
  end;
  procedure DoFavicon;
  begin
    R:= TResourceStream.Create(HInstance, 'FAVICON_ICO', 'ICO');
    try
      R.Position:= 0;
      AResponseInfo.ContentStream:= R;
      AResponseInfo.ContentType:= 'image/x-icon';
    finally
      //FreeAndNil(R);
    end;
  end;
  procedure DoHome;
  begin
    R:= TResourceStream.Create(HInstance, 'HOME_HTML', 'HTML');
    try
      R.Position:= 0;
      L.LoadFromStream(R);
    finally
      FreeAndNil(R);
    end;
  end;
  procedure DoLogin;
  begin
    //TODO: Return Login Page

  end;
  procedure DoRegister;
  begin
    //TODO: Return Register Page

  end;
  procedure DoService;
  begin
    R:= TResourceStream.Create(HInstance, 'SERVICE_HTML', 'HTML');
    try
      R.Position:= 0;
      L.LoadFromStream(R);
    finally
      FreeAndNil(R);
    end;
  end;
begin
  L:= TStringList.Create;
  AResponseInfo.ContentType:= 'text/plain';
  try
    try
      if (FDoc.Count = 1) then begin
        DoHome;
      end else begin
        //Respond with requested page
        if FDoc[1] = '' then begin
          DoHome;
        end else
        if SameText(FDoc[1], 'JDWeatherScript.js') then begin
          DoScript;
        end else
        if SameText(FDoc[1], 'JDWeatherStyles.css') then begin
          DoStyles;
        end else
        if SameText(FDoc[1], 'favicon.ico') then begin
          DoFavicon;
        end else
        if SameText(FDoc[1], 'login') then begin
          DoLogin;
        end else
        if SameText(FDoc[1], 'register') then begin
          DoRegister;
        end else
        if SameText(FDoc[1], 'service') then begin
          DoService;
        end else begin
          DoHome;
        end;
      end;
    finally
      if AResponseInfo.ContentType = 'text/plain' then begin
        AResponseInfo.ContentText:= L.Text;
        AResponseInfo.ContentType:= 'text/html';
      end;
    end;
  finally
    FreeAndNil(L);
  end;
end;

end.
