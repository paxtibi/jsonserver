unit app;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, custhttpapp, fphttpapp, httproute, HTTPDefs, fpjson, fpjsonrtti, om, fgl;

type

  { TTimerObject }

  TTimerObject = class
  private
    FRequest: TRequest;
    Frequestor: string;
    FStart: TDateTime;
    procedure SetRequest(AValue: TRequest);
    procedure SetRequestor(AValue: string);
  public
    constructor Create;
    property Request: TRequest read FRequest write SetRequest;
    property Start: TDateTime read FStart;
    property Requestor: string read Frequestor write Setrequestor;
  end;

  TTimersHolder = specialize fgl.TFPGObjectList<TTimerObject>;

  { TTimersHolderHelper }

  TTimersHolderHelper = class helper for TTimersHolder
    function findByRequest(aRequest: TRequest): TTimerObject;
    procedure stopTimer(aTimer: TTimerObject);
  end;

  { TFakeJsonServer }

  TFakeJsonServer = class(TCustomHTTPApplication)
  private
    FConfig: TConfigObject;
    FTimers: TTimersHolder;
    procedure SetConfig(AValue: TConfigObject);
  public
    procedure StartRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
    procedure EndRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
    procedure Initialize; override;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Config: TConfigObject read FConfig write SetConfig;
  protected
    procedure Response404(ARequest: TRequest; AResponse: TResponse);
  end;

var
  Application: TFakeJsonServer;
  ShowCleanUpErrors: boolean = False;

implementation

uses
  dateutils, CustApp, jsonparser, routers;

{ TTimersHolderHelper }

function TTimersHolderHelper.findByRequest(aRequest: TRequest): TTimerObject;
var
  cursor: TTimerObject;
begin
  result := nil;
  for cursor in self do
  begin
    if cursor.Request = aRequest then
      result := cursor;
  end;
end;

procedure TTimersHolderHelper.stopTimer(aTimer: TTimerObject);
var
  idx: integer;
begin
  if (aTimer <> nil) then
  begin
    idx := IndexOf(aTimer);
    if idx > -1 then
    begin
      Remove(aTimer);
      try
      except
        aTimer.Free;
      end;
    end;
  end;
end;

{ TTimerObject }

procedure TTimerObject.SetRequest(AValue: TRequest);
begin
  if FRequest = AValue then
    Exit;
  FRequest := AValue;
end;

procedure TTimerObject.Setrequestor(AValue: string);
begin
  if Frequestor = AValue then
    Exit;
  Frequestor := AValue;
end;

constructor TTimerObject.Create;
begin
  FStart := now;
end;

{ TFakeJsonServer }

constructor TFakeJsonServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FConfig := TConfigObject.Create;
  FTimers := TTimersHolder.Create(True);
end;

destructor TFakeJsonServer.Destroy;
begin
  FreeAndNil(FTimers);
  FreeAndNil(FConfig);
  inherited Destroy;
end;

procedure TFakeJsonServer.Response404(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Content := '{ status :400 }';
end;

procedure TFakeJsonServer.SetConfig(AValue: TConfigObject);
begin
  if FConfig = AValue then
    Exit;
  FConfig := AValue;
end;

procedure TFakeJsonServer.StartRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
var
  Timer: TTimerObject;
begin
  Timer := TTimerObject.Create;
  Timer.Request := ARequest;
  Timer.requestor := Format('%s:[%10s]%s', [ARequest.RemoteAddress, ARequest.Method, ARequest.URL]);
  FTimers.Add(Timer);
end;

procedure TFakeJsonServer.EndRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
var
  timer: TTimerObject;
begin
  timer := FTimers.findByRequest(ARequest);
  if timer <> nil then
  begin
    Writeln(Timer.requestor, ' served in :', FormatDateTime('hh:nn:ss:zzzz', Now - Timer.start));
  end;
  FTimers.stopTimer(timer);
end;

procedure TFakeJsonServer.Initialize;
var
  FileStream: TFileStream;
  DeStreamer: TJSONDeStreamer;
  c: TCollectionItem;
  r: TRouterObject;
  jsonData: TJSONStringType;
  handle: TRouter;
begin
  inherited Initialize;
  Application.Threaded := True;
  RedirectOnError := True;
  FileStream := TFileStream.Create('config.json', fmOpenRead);
  SetLength(jsonData, FileStream.Size);
  FileStream.Read(jsonData[1], FileStream.Size);
  Writeln(jsonData);
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    DeStreamer.JSONToObject(jsonData, FConfig);
    Application.Port := Config.config.Port;
    for c in Config.Routers do
    begin
      r := TRouterObject(c);
      if (r.Method <> '') and (r.Route <> '') then
      begin
        Writeln(r.Method, ' ', r.Route);
        handle := TRouter.Create(Application);
        handle.DataSetName := r.dataset;
        HTTPRouter.RegisterRoute(r.Route, getRouteMethod(r.Method), handle);
      end;
    end;
  finally
    FreeAndNil(DeStreamer);
    FreeAndNil(FileStream);
  end;
  HTTPRouter.RegisterRoute('/stop', rmHead, TStopHandle.Create(self));
  HTTPRouter.BeforeRequest := @StartRequest;
  HTTPRouter.AfterRequest := @EndRequest;
end;


procedure InitHTTP;
begin
  Application := TFakeJsonServer.Create(nil);
  if not assigned(CustomApplication) then
    CustomApplication := Application;
end;

procedure DoneHTTP;
begin
  if CustomApplication = Application then
    CustomApplication := nil;
  try
    FreeAndNil(Application);
  except
    if ShowCleanUpErrors then
      raise;
  end;
end;

initialization
  InitHTTP;

finalization
  DoneHTTP;
end.



