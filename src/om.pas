unit om;

{$mode objfpc}{$H+}
{$M+}

interface

uses
  Classes, SysUtils;

type

  { TOutputObject }

  TOutputObject = class(TPersistent)
  private
    Fkey: string;
    Ftemplate: string;
    procedure Setkey(AValue: string);
    procedure Settemplate(AValue: string);
  published
    property template: string read Ftemplate write Settemplate;
    property key: string read Fkey write Setkey;
  end;

  { TRouterObject }

  TRouterObject = class(TCollectionItem)
  private
    Fcompare: string;
    Fdataset: string;
    FMethod: string;
    Foutput: TOutputObject;
    FoutputKey: string;
    FoutputTemplate: string;
    FPayload: string;
    FRoute: string;
    procedure Setcompare(AValue: string);
    procedure Setdataset(AValue: string);
    procedure SetMethod(AValue: string);
    procedure Setoutput(AValue: TOutputObject);
    procedure SetoutputKey(AValue: string);
    procedure SetoutputTemplate(AValue: string);
    procedure SetPayload(AValue: string);
    procedure SetRoute(AValue: string);
  published
    property method: string read FMethod write SetMethod;
    property route: string read FRoute write SetRoute;
    property dataset: string read FDataset write SetDataset;
    property payload: string read FPayload write SetPayload;
    property compare: string read Fcompare write Setcompare;
    property outputTemplate: string read FoutputTemplate write SetoutputTemplate;
    property outputKey: string read FoutputKey write SetoutputKey;
  end;

  { TApplicationConfigObject }

  TApplicationConfigObject = class(TPersistent)
  private
    FPort: integer;
    procedure SetPort(AValue: integer);
  public
    constructor Create;
  published
    property port: integer read FPort write SetPort;
  end;

  { TConfigObject }

  TConfigObject = class(TPersistent)
  private
    Fconfig: TApplicationConfigObject;
    FRouters: TCollection;
    FData: TCollection;
    procedure Setconfig(AValue: TApplicationConfigObject);
    procedure SetRouters(AValue: TCollection);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property config: TApplicationConfigObject read Fconfig write Setconfig;
    property routers: TCollection read FRouters;
    property Data: TCollection read FData;
  end;

implementation

{ TOutputObject }

procedure TOutputObject.Setkey(AValue: string);
begin
  if Fkey = AValue then
    Exit;
  Fkey := AValue;
end;

procedure TOutputObject.Settemplate(AValue: string);
begin
  if Ftemplate = AValue then
    Exit;
  Ftemplate := AValue;
end;


{ TApplicationConfigObject }

procedure TApplicationConfigObject.SetPort(AValue: integer);
begin
  if FPort = AValue then
    Exit;
  FPort := AValue;
end;

constructor TApplicationConfigObject.Create;
begin
  FPort := 2000;
end;

{ TRouterObject }

procedure TRouterObject.SetMethod(AValue: string);
begin
  if FMethod = AValue then
    Exit;
  FMethod := AValue;
end;

procedure TRouterObject.Setoutput(AValue: TOutputObject);
begin
  if Foutput = AValue then
    Exit;
  Foutput := AValue;
end;

procedure TRouterObject.SetoutputKey(AValue: string);
begin
  if FoutputKey = AValue then
    Exit;
  FoutputKey := AValue;
end;

procedure TRouterObject.SetoutputTemplate(AValue: string);
begin
  if FoutputTemplate = AValue then
    Exit;
  FoutputTemplate := AValue;
end;

procedure TRouterObject.SetPayload(AValue: string);
begin
  if Fpayload = AValue then
    Exit;
  Fpayload := AValue;
end;

procedure TRouterObject.Setdataset(AValue: string);
begin
  if Fdataset = AValue then
    Exit;
  Fdataset := AValue;
end;

procedure TRouterObject.Setcompare(AValue: string);
begin
  if Fcompare = AValue then
    Exit;
  Fcompare := AValue;
end;

procedure TRouterObject.SetRoute(AValue: string);
begin
  if FRoute = AValue then
    Exit;
  FRoute := AValue;
end;

{ TConfigObject }

procedure TConfigObject.Setconfig(AValue: TApplicationConfigObject);
begin
  if Fconfig = AValue then
    Exit;
  Fconfig := AValue;
end;

procedure TConfigObject.SetRouters(AValue: TCollection);
begin
  if FRouters = AValue then
    Exit;
  FRouters := AValue;
end;

constructor TConfigObject.Create;
begin
  FConfig := TApplicationConfigObject.Create;
  FRouters := TCollection.Create(TRouterObject);
end;

destructor TConfigObject.Destroy;
begin
  FreeAndNil(Fconfig);
  FreeAndNil(FRouters);
  inherited Destroy;
end;

end.
