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
    FLocation: TNDFDLocation;
    FMoreInfo: TNDFDMoreInfoList;
    FTimeLayouts: TNDFDTimeLayouts;
    FParameters: TNDFDParameters;
  public
    constructor Create;
    destructor Destroy; override;

  end;

  TNDFDLocation = class(TObject)
  private

  public
    constructor Create;
    destructor Destroy; override;

  end;

  TNDFDMoreInfo = class(TObject)
  private
    FLocation: String;
    FURL: String;
  public
    constructor Create;
    destructor Destroy; override;

  end;

  TNDFDMoreInfoList = class(TObject)
  private
    FItems: TObjectList<TNDFDMoreInfo>;
  public
    constructor Create;
    destructor Destroy; override;

  end;

  TNDFDTimeLayoutItem = class(TObject)
  private

  public
    constructor Create;
    destructor Destroy; override;

  end;

  TNDFDTimeLayoutItems = class(TObject)
  private
    FItems: TObjectList<TNDFDTimeLayoutItem>;
  public
    constructor Create;
    destructor Destroy; override;

  end;

  TNDFDTimeLayout = class(TObject)
  private

  public
    constructor Create;
    destructor Destroy; override;

  end;

  TNDFDTimeLayouts = class(TObject)
  private
    FItems: TObjectList<TNDFDTimeLayout>;
  public
    constructor Create;
    destructor Destroy; override;

  end;

  TNDFDParameter = class(TObject)
  private

  public
    constructor Create;
    destructor Destroy; override;

  end;

  TNDFDParameters = class(TObject)
  private
    FItems: TObjectList<TNDFDParameter>;
  public
    constructor Create;
    destructor Destroy; override;

  end;

  TNDFDParameterList = class(TObject)
  private

  public
    constructor Create;
    destructor Destroy; override;

  end;

  TNDFDParameterLists = class(TObject)
  private
    FItems: TObjectList<TNDFDParameterList>;
  public
    constructor Create;
    destructor Destroy; override;

  end;

implementation

{ TNDFDDocument }

constructor TNDFDDocument.Create;
begin

end;

destructor TNDFDDocument.Destroy;
begin

  inherited;
end;

function ParseDateTime(const S: String): TDateTime;
begin
  Result:= 0; //TODO
end;

function ParseRate(const S: String): Integer;
begin
  Result:= 0;
  //TODO: Return number of minutes
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

end;

destructor TNDFDData.Destroy;
begin

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

{ TNDFDMoreInfo }

constructor TNDFDMoreInfo.Create;
begin

end;

destructor TNDFDMoreInfo.Destroy;
begin

  inherited;
end;

{ TNDFDMoreInfoList }

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

{ TNDFDTimeLayoutItem }

constructor TNDFDTimeLayoutItem.Create;
begin

end;

destructor TNDFDTimeLayoutItem.Destroy;
begin

  inherited;
end;

{ TNDFDTimeLayoutItems }

constructor TNDFDTimeLayoutItems.Create;
begin

end;

destructor TNDFDTimeLayoutItems.Destroy;
begin

  inherited;
end;

{ TNDFDTimeLayout }

constructor TNDFDTimeLayout.Create;
begin

end;

destructor TNDFDTimeLayout.Destroy;
begin

  inherited;
end;

{ TNDFDTimeLayouts }

constructor TNDFDTimeLayouts.Create;
begin

end;

destructor TNDFDTimeLayouts.Destroy;
begin

  inherited;
end;

{ TNDFDParameter }

constructor TNDFDParameter.Create;
begin

end;

destructor TNDFDParameter.Destroy;
begin

  inherited;
end;

{ TNDFDParameters }

constructor TNDFDParameters.Create;
begin

end;

destructor TNDFDParameters.Destroy;
begin

  inherited;
end;

{ TNDFDParameterList }

constructor TNDFDParameterList.Create;
begin

end;

destructor TNDFDParameterList.Destroy;
begin

  inherited;
end;

{ TNDFDParameterLists }

constructor TNDFDParameterLists.Create;
begin

end;

destructor TNDFDParameterLists.Destroy;
begin

  inherited;
end;

end.

