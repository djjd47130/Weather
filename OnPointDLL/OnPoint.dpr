library OnPoint;

uses
  JD.Weather.Intf in '..\JD.Weather.Intf.pas',
  JD.Weather.SuperObject in '..\JD.Weather.SuperObject.pas',
  JD.Weather.Services.OnPOINT in '..\Services\JD.Weather.Services.OnPOINT.pas';

{$R *.res}

function CreateWeatherService: IWeatherService; stdcall;
var
  R: TOPService;
begin
  R:= TOPService.Create;
  try

  finally
    Result:= R;
  end;
end;

exports
  CreateWeatherService;

begin
end.
