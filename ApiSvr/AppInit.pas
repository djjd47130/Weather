unit AppInit;

interface

uses
  System.SysUtils,
  Vcl.Forms,
  Vcl.SvcMgr,
  JD.Weather.ApiSvr,
  uTestMain,
  uSvcMain;

procedure RunApp;

implementation

procedure RunAsUI;
begin
  Vcl.Forms.Application.Initialize;
  Vcl.Forms.Application.MainFormOnTaskbar := True;
  Vcl.Forms.Application.CreateForm(TfrmApiSvrTest, frmApiSvrTest);
  Vcl.Forms.Application.Run;
end;

procedure RunAsSvc;
begin
  if not Vcl.SvcMgr.Application.DelayInitialize or Vcl.SvcMgr.Application.Installing then
    Vcl.SvcMgr.Application.Initialize;
  Vcl.SvcMgr.Application.CreateForm(TJDWeatherApiSvrService, JDWeatherApiSvrService);
  Vcl.SvcMgr.Application.Run;
end;

procedure RunApp;
begin
  {$IFDEF DEBUG}
  RunAsUI;
  {$ELSE}
  RunAsSvc;
  {$ENDIF}
end;

end.
