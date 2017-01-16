unit JD.Weather.NDFD;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  Xml.XmlDoc, Xml.XmlIntf;

type
  TNDFDDocument = class;
  TNDFDHead = class;
  TNDFDData = class;
  TNDFDLocation = class;
  TNDFDLocations = class;
  TNDFDMoreInfo = class;
  TNDFDMoreInfoList = class;
  TNDFDTimeLayoutItem = class;
  TNDFDTimeLayoutItems = class;
  TNDFDTimeLayout = class;
  TNDFDTimeLayouts = class;
  TNDFDParameter = class;
  TNDFDParameters = class;
  TNDFDParameterList = class;
  TNDFDParameterLists = class;

  TNDFDCategoryType = (catForecastL);

  TNDFDConciseNameType = (cntTimeSeries, cntGlance, cntTabularDigital,
    cntDigitalZone, cntByDay);

  TNDFDOperationalModeType = (omtTest, omtDevelopmental, omtExperimental, omtOfficial);

  TNDFDSrsNameType = (sntWGS1984);

  TNDFDDocument = class(TObject)
  private
    FHead: TNDFDHead;
    FData: TNDFDData;
    FDoc: IXMLDocument;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseXML(const ADoc: IXMLDocument);
  end;

  TNDFDHead = class(TObject)
  private
    FProductName: String;
    FProductConciseName: String;
    FProductOperationalMode: String;
    FProductTitle: String;
    FProductField: String;
    FProductCategory: String;
    FProductDate: TDateTime;
    FRefreshRate: Integer;
    FSourceInfo: String;
    FSourceProductionCenter: String;
    FSourceSubCenter: String;
    FSourceDisclaimer: String;
    FSourceCredit: String;
    FSourceCreditLogo: String;
    FSourceFeedback: String;
  public
    constructor Create;
    destructor Destroy; override;
    property ProductName: String read FProductName;
    property ProductConciseName: String read FProductConciseName;
    property ProductOperationalMode: String read FProductOperationalMode;
    property ProductTitle: String read FProductTitle;
    property ProductField: String read FProductField;
    property ProductCategory: String read FProductCategory;
    property ProductDate: TDateTime read FProductDate;
    property RefreshRate: Integer read FRefreshRate;
    property SourceInfo: String read FSourceInfo;
    property SourceProductionCenter: String read FSourceProductionCenter;
    property SourceSubCenter: String read FSourceSubCenter;
    property SourceDisclaimer: String read FSourceDisclaimer;
    property SourceCredit: String read FSourceCredit;
    property SourceCreditLogo: String read FSourceCreditLogo;
    property SourceFeedback: String read FSourceFeedback;
  end;

  TNDFDData = class(TObject)
  private
    FLocations: TNDFDLocations;
    FMoreInfo: TNDFDMoreInfoList;
    FTimeLayouts: TNDFDTimeLayouts;
    FParameterLists: TNDFDParameterLists;
  public
    constructor Create;
    destructor Destroy; override;
    property Locations: TNDFDLocations read FLocations;
    property MoreInfo: TNDFDMoreInfoList read FMoreInfo;
    property TimeLayouts: TNDFDTimeLayouts read FTimeLayouts;
    property ParameterLists: TNDFDParameterLists read FParameterLists;
  end;

  TNDFDLocation = class(TObject)
  private
    FKey: String;
    FLatitude: Double;
    FLongitude: Double;
  public
    constructor Create;
    destructor Destroy; override;
    property Key: String read FKey;
    property Latitude: Double read FLatitude;
    property Longitude: Double read FLongitude;
  end;

  TNDFDLocations = class(TObject)
  private
    FItems: TObjectList<TNDFDLocation>;
    function GetItem(Index: Integer): TNDFDLocation;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    property Items[Index: Integer]: TNDFDLocation read GetItem; default;
  end;

  TNDFDMoreInfo = class(TObject)
  private
    FLocation: TNDFDLocation;
    FURL: String;
  public
    constructor Create;
    destructor Destroy; override;
    property Location: TNDFDLocation read FLocation;
    property URL: String read FURL;
  end;

  TNDFDMoreInfoList = class(TObject)
  private
    FItems: TObjectList<TNDFDMoreInfo>;
    function GetItem(const Index: Integer): TNDFDMoreInfo;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    property Items[const Index: Integer]: TNDFDMoreInfo read GetItem; default;
  end;

  TNDFDTimeLayoutItem = class(TObject)
  private
    FTimeStart: TDateTime;
    FTimeEnd: TDateTime;
    procedure SetTimeEnd(const Value: TDateTime);
    procedure SetTimeStart(const Value: TDateTime);
  public
    constructor Create;
    destructor Destroy; override;
    property TimeStart: TDateTime read FTimeStart write SetTimeStart;
    property TimeEnd: TDateTime read FTimeEnd write SetTimeEnd;
  end;

  TNDFDTimeLayoutItems = class(TObject)
  private
    FItems: TObjectList<TNDFDTimeLayoutItem>;
    function GetItem(const Index: Integer): TNDFDTimeLayoutItem;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    property Items[const Index: Integer]: TNDFDTimeLayoutItem read GetItem; default;
  end;

  TNDFDTimeLayout = class(TObject)
  private
    FItems: TNDFDTimeLayoutItems;
    FTimeCoordinate: String;
    FSummarization: String;
    FKey: String;
  public
    constructor Create;
    destructor Destroy; override;
    property Items: TNDFDTimeLayoutItems read FItems;
    property TimeCoordinate: String read FTimeCoordinate;
    property Summarization: String read FSummarization;
    property Key: String read FKey;
  end;

  TNDFDTimeLayouts = class(TObject)
  private
    FItems: TObjectList<TNDFDTimeLayout>;
    function GetItem(const Index: Integer): TNDFDTimeLayout;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    property Items[const Index: Integer]: TNDFDTimeLayout read GetItem; default;
  end;

  TNDFDParameter = class(TObject)
  private
    FValue: String;
  public
    constructor Create;
    destructor Destroy; override;
    function AsString: String;
    function AsInteger: Integer;
    function AsFloat: Double;
    function AsBoolean: Boolean;
    function AsDateTime: TDateTime;
  end;

  TNDFDParameters = class(TObject)
  private
    FItems: TObjectList<TNDFDParameter>;
    FTimeLayout: TNDFDTimeLayout;
    function GetItem(const Index: Integer): TNDFDParameter;
    procedure SetTimeLayout(const Value: TNDFDTimeLayout);
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    property Items[const Index: Integer]: TNDFDParameter read GetItem; default;
    property TimeLayout: TNDFDTimeLayout read FTimeLayout write SetTimeLayout;
  end;

  TNDFDParameterList = class(TObject)
  private
    FParameters: TNDFDParameters;
  public
    constructor Create;
    destructor Destroy; override;
    property Parameters: TNDFDParameters read FParameters;
  end;

  TNDFDParameterLists = class(TObject)
  private
    FItems: TObjectList<TNDFDParameterList>;
    function GetItem(const Index: Integer): TNDFDParameterList;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    property Items[const Index: Integer]: TNDFDParameterList read GetItem; default;
  end;

implementation

function ParseDateTime(const S: String): TDateTime;
var
  T: String;
  Y, D, M, H, N, C: Integer;
  function CopyTo(const D: String): Integer;
  var
    P: Integer;
  begin
    P:= Pos(D, T);
    Result:= StrToIntDef(Copy(T, 1, P-1), 0);
    Delete(T, 1, P);
  end;
begin
  try
    //2017-01-12T19:00:00-05:00
    T:= S;
    Y:= CopyTo('-');
    M:= CopyTo('-');
    D:= CopyTo('T');
    H:= CopyTo(':');
    N:= CopyTo(':');
    C:= CopyTo('-');
    T:= IntToStr(M)+'/'+IntToStr(D)+'/'+IntToStr(Y)+' '+
        IntToStr(H)+':'+IntToStr(N)+':'+IntToStr(C);
  finally
    Result:= StrToDateTimeDef(T, 0);
  end;
end;

function ParseRate(const S: String): Integer;
begin
  Result:= 0;
  //TODO: Return number of minutes
end;

{ TNDFDDocument }

constructor TNDFDDocument.Create;
begin
  FHead:= TNDFDHead.Create;
  FData:= TNDFDData.Create;
end;

destructor TNDFDDocument.Destroy;
begin
  FData.Free;
  FHead.Free;
  inherited;
end;

procedure TNDFDDocument.ParseXML(const ADoc: IXMLDocument);
var
  Head, HeadProduct, HeadSource: IXMLNode;
  Data, Location: IXMLNode;
begin
  FDoc:= ADoc;

  Head:= FDoc.DocumentElement.ChildNodes['head'];

  HeadProduct:= Head.ChildNodes['product'];
  FHead.FProductName:= HeadProduct.Attributes['srsName'];
  FHead.FProductConciseName:= HeadProduct.Attributes['concise-name'];
  FHead.FProductOperationalMode:= HeadProduct.Attributes['operational-mode'];
  FHead.FProductTitle:= HeadProduct.ChildNodes['title'].Text;
  FHead.FProductField:= HeadProduct.ChildNodes['field'].Text;
  FHead.FProductCategory:= HeadProduct.ChildNodes['category'].Text;
  FHead.FProductDate:= ParseDateTime(HeadProduct.ChildNodes['creation-date'].Text);
  FHead.FRefreshRate:= ParseRate(HeadProduct.ChildNodes['category'].Attributes['refresh-frequency'].Text);

  HeadSource:= Head.ChildNodes['source'];
  FHead.FSourceInfo:= HeadSource.ChildNodes['more-information'].Text;
  FHead.FSourceProductionCenter:= HeadSource.ChildNodes['production-center'].Text;
  FHead.FSourceSubCenter:= HeadSource.ChildNodes['production-center'].ChildNodes['sub-center'].Text;
  FHead.FSourceDisclaimer:= HeadSource.ChildNodes['disclaimer'].Text;
  FHead.FSourceCredit:= HeadSource.ChildNodes['credit'].Text;
  FHead.FSourceCreditLogo:= HeadSource.ChildNodes['credit-logo'].Text;
  FHead.FSourceFeedback:= HeadSource.ChildNodes['feedback'].Text;

  Data:= FDoc.DocumentElement.ChildNodes['data'];

  Location:= Data.ChildNodes['location'];



end;

{ TNDFDHead }

constructor TNDFDHead.Create;
begin

end;

destructor TNDFDHead.Destroy;
begin

  inherited;
end;

{ TNDFDData }

constructor TNDFDData.Create;
begin
  FLocations:= TNDFDLocations.Create;
  FMoreInfo:= TNDFDMoreInfoList.Create;
  FTimeLayouts:= TNDFDTimeLayouts.Create;
  FParameterLists:= TNDFDParameterLists.Create;
end;

destructor TNDFDData.Destroy;
begin
  FParameterLists.Free;
  FTimeLayouts.Free;
  FMoreInfo.Free;
  FLocations.Free;
  inherited;
end;

{ TNDFDLocation }

constructor TNDFDLocation.Create;
begin

end;

destructor TNDFDLocation.Destroy;
begin

  inherited;
end;

{ TNDFDLocations }

constructor TNDFDLocations.Create;
begin
  FItems:= TObjectList<TNDFDLocation>.Create(True);
end;

destructor TNDFDLocations.Destroy;
begin
  FItems.Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TNDFDLocations.Count: Integer;
begin
  Result:= FItems.Count;
end;

function TNDFDLocations.GetItem(Index: Integer): TNDFDLocation;
begin
  Result:= FItems[Index];
end;

{ TNDFDMoreInfo }

constructor TNDFDMoreInfo.Create;
begin

end;

destructor TNDFDMoreInfo.Destroy;
begin

  inherited;
end;

{ TNDFDMoreInfoList }

function TNDFDMoreInfoList.Count: Integer;
begin
  Result:= FItems.Count;
end;

constructor TNDFDMoreInfoList.Create;
begin
  FItems:= TObjectList<TNDFDMoreInfo>.Create(True);
end;

destructor TNDFDMoreInfoList.Destroy;
begin
  FItems.Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TNDFDMoreInfoList.GetItem(const Index: Integer): TNDFDMoreInfo;
begin
  Result:= FItems[Index];
end;

{ TNDFDTimeLayoutItem }

constructor TNDFDTimeLayoutItem.Create;
begin

end;

destructor TNDFDTimeLayoutItem.Destroy;
begin

  inherited;
end;

procedure TNDFDTimeLayoutItem.SetTimeEnd(const Value: TDateTime);
begin
  FTimeEnd := Value;
end;

procedure TNDFDTimeLayoutItem.SetTimeStart(const Value: TDateTime);
begin
  FTimeStart := Value;
end;

{ TNDFDTimeLayoutItems }

function TNDFDTimeLayoutItems.Count: Integer;
begin
  Result:= FItems.Count;
end;

constructor TNDFDTimeLayoutItems.Create;
begin
  FItems:= TObjectList<TNDFDTimeLayoutItem>.Create(True);
end;

destructor TNDFDTimeLayoutItems.Destroy;
begin
  FItems.Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TNDFDTimeLayoutItems.GetItem(
  const Index: Integer): TNDFDTimeLayoutItem;
begin
  Result:= FItems[Index];
end;

{ TNDFDTimeLayout }

constructor TNDFDTimeLayout.Create;
begin
  FItems:= TNDFDTimeLayoutItems.Create;
end;

destructor TNDFDTimeLayout.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

{ TNDFDTimeLayouts }

function TNDFDTimeLayouts.Count: Integer;
begin
  Result:= FItems.Count;
end;

constructor TNDFDTimeLayouts.Create;
begin
  FItems:= TObjectList<TNDFDTimeLayout>.Create(True);
end;

destructor TNDFDTimeLayouts.Destroy;
begin
  FItems.Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TNDFDTimeLayouts.GetItem(const Index: Integer): TNDFDTimeLayout;
begin
  Result:= FItems[Index];
end;

{ TNDFDParameter }

constructor TNDFDParameter.Create;
begin

end;

destructor TNDFDParameter.Destroy;
begin

  inherited;
end;

function TNDFDParameter.AsBoolean: Boolean;
begin
  Result:= SameText(FValue, 'true');
end;

function TNDFDParameter.AsDateTime: TDateTime;
begin
  Result:= ParseDateTime(FValue);
end;

function TNDFDParameter.AsFloat: Double;
begin
  Result:= StrToFloatDef(FValue, 0);
end;

function TNDFDParameter.AsInteger: Integer;
begin
  Result:= StrToIntDef(FValue, 0);
end;

function TNDFDParameter.AsString: String;
begin
  Result:= FValue;
end;

{ TNDFDParameters }

function TNDFDParameters.Count: Integer;
begin
  Result:= FItems.Count;
end;

constructor TNDFDParameters.Create;
begin
  FItems:= TObjectList<TNDFDParameter>.Create(True);

end;

destructor TNDFDParameters.Destroy;
begin

  FItems.Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TNDFDParameters.GetItem(const Index: Integer): TNDFDParameter;
begin
  Result:= FItems[Index];
end;

procedure TNDFDParameters.SetTimeLayout(const Value: TNDFDTimeLayout);
begin
  FTimeLayout := Value;
end;

{ TNDFDParameterList }

constructor TNDFDParameterList.Create;
begin
  FParameters:= TNDFDParameters.Create;
end;

destructor TNDFDParameterList.Destroy;
begin
  FreeAndNil(FParameters);
  inherited;
end;

{ TNDFDParameterLists }

function TNDFDParameterLists.Count: Integer;
begin
  Result:= FItems.Count;
end;

constructor TNDFDParameterLists.Create;
begin
  FItems:= TObjectList<TNDFDParameterList>.Create(True);
end;

destructor TNDFDParameterLists.Destroy;
begin
  FItems.Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TNDFDParameterLists.GetItem(const Index: Integer): TNDFDParameterList;
begin
  Result:= FItems[Index];
end;

end.

