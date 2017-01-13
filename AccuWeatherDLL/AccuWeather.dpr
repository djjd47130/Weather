library AccuWeather;

uses
  JD.Weather.Intf in '..\JD.Weather.Intf.pas',
  JD.Weather.SuperObject in '..\JD.Weather.SuperObject.pas',
  JD.Weather.Services.AccuWeather in '..\Services\JD.Weather.Services.AccuWeather.pas';

{$R *.res}

function CreateWeatherService: IWeatherService; stdcall;
var
  R: TAWService;
begin
  R:= TAWService.Create;
  try

  finally
    Result:= R;
  end;
end;

exports
  CreateWeatherService;

begin
end.
