unit uTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  JD.Weather.ApiSvr, Data.DB, Data.Win.ADODB,
  JD.Weather.SuperObject, IdBaseComponent, IdComponent, IdCustomTCPServer,
  IdCustomHTTPServer, IdHTTPServer,
  JD.Weather.Logger, IdServerIOHandler, IdSSL, IdSSLOpenSSL;

type
  TfrmApiSvrTest = class(TForm)
    pTop: TPanel;
    btnStop: TBitBtn;
    btnStart: TBitBtn;
    Log: TMemo;
    IdServerIOHandlerSSLOpenSSL1: TIdServerIOHandlerSSLOpenSSL;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FSvr: TJDWeatherApiSvrThread;
    FObj: ISuperObject;
    procedure ThreadLog(Sender: TObject; const Timestamp: TDateTime;
      const Msg: String);
  public

  end;

var
  frmApiSvrTest: TfrmApiSvrTest;

implementation

{$R *.dfm}

procedure TfrmApiSvrTest.btnStartClick(Sender: TObject);
begin
  btnStart.Enabled:= False;
  btnStop.Enabled:= True;
  FSvr:= TJDWeatherApiSvrThread.Create;
  FSvr.ConnStr:= FObj.S['connstr'];
  FSvr.Port:= FObj.I['port'];
  FSvr.OnLog:= ThreadLog;
  FSvr.Start;
end;

procedure TfrmApiSvrTest.btnStopClick(Sender: TObject);
begin
  btnStart.Enabled:= True;
  btnStop.Enabled:= False;
  if Assigned(FSvr) then begin
    FSvr.Terminate;
    FSvr.WaitFor;
    FreeAndNil(FSvr);
  end;
end;

procedure TfrmApiSvrTest.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FSvr) then begin
    FSvr.Terminate;
    FSvr.WaitFor;
    FreeAndNil(FSvr);
  end;
end;

procedure TfrmApiSvrTest.FormCreate(Sender: TObject);
var
  FN: String;
  L: TStringList;
begin
  Log.Align:= alClient;
  FN:= ExtractFilePath(ParamStr(0));
  FN:= IncludeTrailingPathDelimiter(FN)+'ApiSvr.json';
  L:= TStringList.Create;
  try
    L.LoadFromFile(FN);
    FObj:= SO(L.Text);
  finally
    FreeAndNil(L);
  end;
end;

procedure TfrmApiSvrTest.FormShow(Sender: TObject);
begin
  btnStart.Click;
end;

procedure TfrmApiSvrTest.ThreadLog(Sender: TObject; const Timestamp: TDateTime;
  const Msg: String);
begin
  //PostLog(0, Msg);
  //Log.Lines.Append(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Timestamp)+' - '+Msg);
end;

end.
