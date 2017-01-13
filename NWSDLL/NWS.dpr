library NWS;

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Imaging.PngImage,
  IdHTTP,
  JD.Weather.Intf in '..\JD.Weather.Intf.pas',
  JD.Weather.NDFD in '..\JD.Weather.NDFD.pas',
  JD.Weather.Services.NWS in '..\Services\JD.Weather.Services.NWS.pas';

{$R *.res}

function CreateWeatherService: IWeatherService; stdcall;
var
  R: TNWSService;
begin
  R:= TNWSService.Create;
  try

  finally
    Result:= R;
  end;
end;

exports
  CreateWeatherService;

begin
end.
