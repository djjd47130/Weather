library NWS;

{$R 'NWSRes.res' 'NWSRes.rc'}

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Imaging.PngImage,
  IdHTTP,
  JD.Weather.Intf in '..\JD.Weather.Intf.pas',
  JD.Weather.NDFD in '..\JD.Weather.NDFD.pas',
  JD.Weather.Services.NWS in '..\Services\JD.Weather.Services.NWS.pas',
  JD.Weather.SuperObject in '..\JD.Weather.SuperObject.pas';

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
