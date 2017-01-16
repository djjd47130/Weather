unit uTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  JD.Weather.ApiSvr, Data.DB, Data.Win.ADODB,
  JD.Weather.SuperObject, IdBaseComponent, IdComponent, IdCustomTCPServer,
  IdCustomHTTPServer, IdHTTPServer;

type
  TForm1 = class(TForm)
    pTop: TPanel;
    btnStop: TBitBtn;
    btnStart: TBitBtn;
    Log: TMemo;
    DB: TADOConnection;
    IdHTTPServer1: TIdHTTPServer;
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
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnStartClick(Sender: TObject);
begin
  btnStart.Enabled:= False;
  btnStop.Enabled:= True;
  FSvr:= TJDWeatherApiSvrThread.Create;
  FSvr.ConnStr:= FObj.S['connstr'];
  FSvr.OnLog:= ThreadLog;
  FSvr.Start;
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  btnStart.Enabled:= True;
  btnStop.Enabled:= False;
  if Assigned(FSvr) then begin
    FSvr.Terminate;
    FSvr.WaitFor;
    FreeAndNil(FSvr);
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FSvr) then begin
    FSvr.Terminate;
    FSvr.WaitFor;
    FreeAndNil(FSvr);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
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

procedure TForm1.FormShow(Sender: TObject);
begin
  btnStart.Click;
end;

procedure TForm1.ThreadLog(Sender: TObject; const Timestamp: TDateTime;
  const Msg: String);
begin
  Log.Lines.Append(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Timestamp)+' - '+Msg);
end;

end.
