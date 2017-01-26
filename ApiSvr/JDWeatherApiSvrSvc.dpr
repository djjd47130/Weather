program JDWeatherApiSvrSvc;

uses
  Vcl.Forms,
  AppInit in 'AppInit.pas',
  JD.Weather.ApiSvr in '..\JD.Weather.ApiSvr.pas',
  JD.Weather.Intf in '..\JD.Weather.Intf.pas',
  JD.Weather.SuperObject in '..\JD.Weather.SuperObject.pas',
  JD.Weather.Logger in '..\JD.Weather.Logger.pas',
  uTestMain in 'uTestMain.pas' {frmApiSvrTest},
  uSvcMain in 'uSvcMain.pas' {JDWeatherApiSvrService: TService},
  JD.HTTPServer in '..\JD.HTTPServer.pas';

{$R *.res}

begin
  RunApp;
end.
