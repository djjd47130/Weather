library AccuWeather;

uses
  JD.Weather.Intf in '..\JD.Weather.Intf.pas',
  JD.Weather.SuperObject in '..\JD.Weather.SuperObject.pas',
  JD.Weather.Services.AccuWeather in '..\Services\JD.Weather.Services.AccuWeather.pas';

{$R *.res}

function CreateWeatherService: IWeatherService; stdcall;
var
  R: TWeatherService;
begin
  R:= TWeatherService.Create;
  try

  finally
    Result:= R;
  end;
end;

function GetServiceInfo: IWeatherServiceInfo; stdcall;
var
  R: TWeatherServiceInfo;
begin
  R:= TWeatherServiceInfo.Create;
  try

  finally
    Result:= R;
  end;
end;

exports
  CreateWeatherService,
  GetServiceInfo;

begin
end.
