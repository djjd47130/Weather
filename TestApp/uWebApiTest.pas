unit uWebApiTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.CheckLst,
  Vcl.ExtCtrls,
  JD.Weather,
  JD.Weather.Config,
  JD.Weather.Intf, Vcl.Buttons, Vcl.Menus, Vcl.Samples.Spin;

type
  TfrmWebTest = class(TForm)
    Pages: TPageControl;
    tabSetup: TTabSheet;
    tabAlerts: TTabSheet;
    CheckListBox1: TCheckListBox;
    pSetup: TPanel;
    txtApiKey: TEdit;
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    Edit1: TEdit;
    Label3: TLabel;
    Edit2: TEdit;
    Label4: TLabel;
    ComboBox2: TComboBox;
    Label5: TLabel;
    txtLocationSearch: TEdit;
    Label6: TLabel;
    tabForecast: TTabSheet;
    tmrRefresh: TTimer;
    BitBtn1: TBitBtn;
    lstLocations: TListView;
    cmdUseLocation: TBitBtn;
    MM: TMainMenu;
    File1: TMenuItem;
    Refresh1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    View1: TMenuItem;
    Help1: TMenuItem;
    ShowHelp1: TMenuItem;
    N3: TMenuItem;
    About1: TMenuItem;
    ServiceInfo1: TMenuItem;
    tabConditions: TTabSheet;
    pTop: TPanel;
    lblLocation: TLabel;
    lblConditions: TLabel;
    lblTemp: TLabel;
    Label7: TLabel;
    SpinEdit1: TSpinEdit;
    ComboBox3: TComboBox;
    Panel1: TPanel;
    lstConditions: TListView;
    procedure tmrRefreshTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Refresh1Click(Sender: TObject);
    procedure ServiceInfo1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FConfig: IJDWeatherConfig;
    FWeather: TJDWeather;
    function ConfigFilename: String;
  public
    { Public declarations }
  end;

var
  frmWebTest: TfrmWebTest;

implementation

{$R *.dfm}

uses
  uWeatherDLLTestMain;

procedure TfrmWebTest.tmrRefreshTimer(Sender: TObject);
begin
  //TODO: Fetch data

end;

procedure TfrmWebTest.FormCreate(Sender: TObject);
begin
  Pages.Align:= alClient;
  Pages.ActivePageIndex:= 0;
  lstConditions.Align:= alClient;

  Show;
  BringToFront;
  Application.ProcessMessages;

  //TODO: Load configuration and begin timer
  FConfig:= TJDWeatherConfig.Create;
  FConfig._AddRef;
  FConfig.LoadFromFile(ConfigFilename);

  FWeather:= TJDWeather.Create(nil);



end;

procedure TfrmWebTest.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FWeather);
  FConfig._Release;
  FConfig:= nil;
end;

function TfrmWebTest.ConfigFilename: String;
begin
  Result:= ExtractFilePath(ParamStr(0));
  Result:= IncludeTrailingPathDelimiter(Result);
  Result:= Result + 'Client.json';
end;

procedure TfrmWebTest.Refresh1Click(Sender: TObject);
begin
  //TODO: Load weather conditions and other info


end;

procedure TfrmWebTest.ServiceInfo1Click(Sender: TObject);
begin
  //TODO: Open service info screen
  frmMain.ShowModal;
end;

procedure TfrmWebTest.BitBtn1Click(Sender: TObject);
begin
  //TODO: Save configuration

end;

procedure TfrmWebTest.Exit1Click(Sender: TObject);
begin
  Close;
end;

end.
