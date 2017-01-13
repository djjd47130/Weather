library OpenWeatherMap;

uses
  JD.Weather.Intf in '..\JD.Weather.Intf.pas',
  JD.Weather.Services.OpenWeatherMap in '..\Services\JD.Weather.Services.OpenWeatherMap.pas',
  JD.Weather.SuperObject in '..\JD.Weather.SuperObject.pas';

{$R *.res}

function CreateWeatherService: IWeatherService; stdcall;
var
  R: TOWMService;
begin
  R:= TOWMService.Create;
  try

  finally
    Result:= R;
  end;
end;

exports
  CreateWeatherService;

begin
end.
