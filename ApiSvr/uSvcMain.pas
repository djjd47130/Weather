unit uSvcMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes,
  Vcl.SvcMgr,
  JD.Weather.ApiSvr,
  JD.Weather.SuperObject;

type
  TJDWeatherApiSvrService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FSvr: TJDWeatherApiSvrThread;
    FObj: ISuperObject;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  JDWeatherApiSvrService: TJDWeatherApiSvrService;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  JDWeatherApiSvrService.Controller(CtrlCode);
end;

function TJDWeatherApiSvrService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TJDWeatherApiSvrService.ServiceStart(Sender: TService;
  var Started: Boolean);
var
  FN: String;
  L: TStringList;
begin
  FN:= ExtractFilePath(ParamStr(0));
  FN:= IncludeTrailingPathDelimiter(FN)+'ApiSvr.json';
  L:= TStringList.Create;
  try
    L.LoadFromFile(FN);
    FObj:= SO(L.Text);
  finally
    FreeAndNil(L);
  end;
  FSvr:= TJDWeatherApiSvrThread.Create;
  FSvr.ConnStr:= FObj.S['connstr'];
  FSvr.Port:= FObj.I['port'];
  //FSvr.OnLog:= ThreadLog;
  FSvr.Start;
end;

procedure TJDWeatherApiSvrService.ServiceStop(Sender: TService;
  var Stopped: Boolean);
begin
  if Assigned(FSvr) then begin
    FSvr.Terminate;
    FSvr.WaitFor;
    FreeAndNil(FSvr);
  end;
end;

end.
