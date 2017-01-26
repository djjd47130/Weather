program WeatherDLLTest;

uses
  Vcl.Forms,
  uWeatherDLLTestMain in 'uWeatherDLLTestMain.pas' {frmMain},
  JD.Weather.Intf in '..\JD.Weather.Intf.pas',
  Vcl.Themes,
  Vcl.Styles,
  JD.Weather.SuperObject in '..\JD.Weather.SuperObject.pas',
  JD.Weather in '..\JD.Weather.pas',
  JD.Weather.Maps in '..\JD.Weather.Maps.pas',
  JD.Weather.MapControl in '..\JD.Weather.MapControl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
