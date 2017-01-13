library Foreca;

uses
  JD.Weather.Intf in '..\JD.Weather.Intf.pas',
  JD.Weather.Services.Foreca in '..\Services\JD.Weather.Services.Foreca.pas',
  JD.Weather.SuperObject in '..\JD.Weather.SuperObject.pas';

{$R *.res}

function CreateWeatherService: IWeatherService; stdcall;
var
  R: TForecaService;
begin
  R:= TForecaService.Create;
  try

  finally
    Result:= R;
  end;
end;

exports
  CreateWeatherService;

begin
end.
