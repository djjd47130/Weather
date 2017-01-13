library WUnderground;

uses
  JD.Weather.Intf in '..\JD.Weather.Intf.pas',
  JD.Weather.SuperObject in '..\JD.Weather.SuperObject.pas',
  JD.Weather.Services.WUnderground in '..\Services\JD.Weather.Services.WUnderground.pas';

{$R *.res}

function CreateWeatherService: IWeatherService; stdcall;
var
  R: TWUService;
begin
  R:= TWUService.Create;
  try

  finally
    Result:= R;
  end;
end;

function GetServiceInfo: IWeatherServiceInfo; stdcall;
var
  R: TWUServiceInfo;
begin
  R:= TWUServiceInfo.Create;
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
