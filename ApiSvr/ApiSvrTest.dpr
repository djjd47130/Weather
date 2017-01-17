program ApiSvrTest;

{$R 'ApiSvrRes.res' 'ApiSvrRes.rc'}

uses
  Vcl.Forms,
  uTestMain in 'uTestMain.pas' {Form1},
  JD.Weather.ApiSvr in '..\JD.Weather.ApiSvr.pas',
  JD.Weather.Intf in '..\JD.Weather.Intf.pas',
  JD.Weather.SuperObject in '..\JD.Weather.SuperObject.pas',
  JD.Weather.Logger in '..\JD.Weather.Logger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
