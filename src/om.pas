unit om;

{$mode objfpc}{$H+}
{$M+}

interface

uses
  Classes, SysUtils;

type

  { TRouterObject }

  TRouterObject = class(TCollectionItem)
  private
    Fdataset: string;
    FMethod: string;
    FRoute: string;
    procedure Setdataset(AValue: string);
    procedure SetMethod(AValue: string);
    procedure SetRoute(AValue: string);
  published
    property method: string read FMethod write SetMethod;
    property route: string read FRoute write SetRoute;
    property dataset: string read Fdataset write Setdataset;
  end;

  { TDataDescriptorObject }

  TDataDescriptorObject = class(TCollectionItem)
  private
    Fkey: string;
    Fname: string;
    procedure Setkey(AValue: string);
    procedure Setname(AValue: string);
  published
    property name: string read Fname write Setname;
    property key: string read Fkey write Setkey;
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
    property data: TCollection read FData;
  end;

implementation

{ TDataDescriptorObject }

procedure TDataDescriptorObject.Setkey(AValue: string);
begin
  if Fkey = AValue then
    Exit;
  Fkey := AValue;
end;

procedure TDataDescriptorObject.Setname(AValue: string);
begin
  if Fname = AValue then
    Exit;
  Fname := AValue;
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

procedure TRouterObject.Setdataset(AValue: string);
begin
  if Fdataset = AValue then
    Exit;
  Fdataset := AValue;
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
  Fconfig := TApplicationConfigObject.Create;
  FRouters := TCollection.Create(TRouterObject);
  FData := TCollection.Create(TDataDescriptorObject);
end;

destructor TConfigObject.Destroy;
begin
  FreeAndNil(Fconfig);
  FreeAndNil(FRouters);
  FreeAndNil(FData);
  inherited Destroy;
end;

end.
