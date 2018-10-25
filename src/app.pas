unit app;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, paxhttp.server, custhttpapp, log4d, HTTPDefs, fpjson, fpjsonrtti, om, fgl;

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

  TFakeJsonServer = class(TCustomSlimHttpApplication)
  private
    FConfig: TConfigObject;
    FTimers: TTimersHolder;
    procedure ExceptionHandle(Sender: TObject; E: Exception);
    procedure SetConfig(AValue: TConfigObject);
  public
    procedure StartRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
    procedure EndRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
    procedure Initialize; override;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Config: TConfigObject read FConfig write SetConfig;
    class function normalizePath(Path: string): string;
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
  Result := nil;
  for cursor in self do
  begin
    if cursor.Request = aRequest then
      Result := cursor;
  end;
end;

procedure TTimersHolderHelper.stopTimer(aTimer: TTimerObject);
var
  idx: integer;
begin
  if self <> nil then
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


procedure ShowRequestException(AResponse: TResponse; AnException: Exception; var handled: boolean);
begin
  TLogLog.GetLogger('error').error(
    Format('serving : %s, exception: %s, message: %s', [AResponse.Referer, AnException.ClassName, AnException.Message])
    );
  AResponse.Code := 500;
  AResponse.Content := AnException.Message;
  handled := True;
end;

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

procedure TFakeJsonServer.ExceptionHandle(Sender: TObject; E: Exception);
begin
  TLogLog.GetLogger('server').Error(Sender, e);
end;

procedure TFakeJsonServer.StartRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
var
  Timer: TTimerObject;
begin
  TLogLog.GetLogger('server').info(Format('%s:[%10s]%s', [ARequest.RemoteAddress, ARequest.Method, ARequest.URL]));
  Timer := TTimerObject.Create;
  Timer.Request := ARequest;
  TLogLog.GetLogger('server').info(Format('%s:[%10s]%s', [ARequest.RemoteAddress, ARequest.Method, ARequest.URL]));
  FTimers.Add(Timer);
end;

procedure TFakeJsonServer.EndRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
var
  timer: TTimerObject;
  message: string;
begin
  timer := FTimers.findByRequest(ARequest);
  if timer <> nil then
  begin
    message := Format('%s serverd in : %s ', [Timer.requestor, FormatDateTime('hh:nn:ss:zzzz', Now - Timer.start)]);
    TLogLog.GetLogger('server').info(message);
  end;
  FTimers.stopTimer(timer);
end;


class function TFakeJsonServer.normalizePath(Path: string): string;
var
  idx: integer;
  buffer: TStringList;
begin
  buffer := TStringList.Create;
  Result := ExcludeTrailingPathDelimiter(Path);
  buffer.LineBreak := DirectorySeparator;
  buffer.Text := Result;
  idx := 0;
  while idx < buffer.Count - 1 do
  begin
    if buffer[idx] = '.' then
    begin
      buffer.delete(idx);
      idx := -1; // restart;
    end
    else
    if buffer[idx] = '..' then
    begin
      buffer.Delete(idx - 1);
      buffer.delete(idx - 1);
      idx := -1; // restart;
    end;
    inc(idx);
  end;
  result := ExcludeTrailingPathDelimiter(buffer.Text);
  idx := 0;
  FreeAndNil(buffer);
end;


procedure handleStopRequest(aReq: TRequest; aResp: TResponse; args: TStrings);
begin
  aResp.ContentType := 'application/text';
  aResp.Content := 'OK';
  Application.Terminate;
end;

procedure TFakeJsonServer.Initialize;
var
  configFileName: string;
  FileStream: TFileStream;
  DeStreamer: TJSONDeStreamer;
  c: TCollectionItem;
  r: TRouterObject;
  jsonData: TJSONStringType;
  handle: TRouter;
begin
  inherited Initialize;
  Application.Threaded := True;
  Application.BeforeServe := @StartRequest;
  Application.AfterServe := @EndRequest;
  OnException := @ExceptionHandle;
  //OnShowRequestException := @ShowRequestException;
  RedirectOnError := True;
  configFileName := IncludeTrailingPathDelimiter(GetCurrentDir) + 'config.json';
  if not FileExists(configFileName) then
    raise Exception.Create(configFileName + ' not found, terminate');

  FileStream := TFileStream.Create(configFileName, fmOpenRead);
  SetLength(jsonData, FileStream.Size);
  FileStream.Read(jsonData[1], FileStream.Size);
  TLogLog.getLogger('server').info(jsonData);
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    DeStreamer.JSONToObject(jsonData, FConfig);
    Application.Port := Config.config.Port;
    for c in Config.routers do
    begin
      r := TRouterObject(c);
      if (r.Method <> '') and (r.Route <> '') then
      begin
        TLogLog.getLogger('server').info(Format('%s %s', [r.Method, r.Route]));
        if r.payload <> '' then
        begin
          handle := TPayloadRouter.Create(Application);
          TPayloadRouter(handle).compare := r.compare;
        end
        else
          handle := TRouter.Create(Application);
        if (r.outputTemplate <> '') and (r.outputKey <> '') then
        begin
          handle.output := TOutput.Create;
          handle.output.Template := r.outputTemplate;
          handle.output.Key := r.outputKey;
        end;
        handle.DataSetName := r.dataset;
        handle.Payload := r.payload;
        AddRoute(r.method, r.route, handle);
      end;
    end;
  finally
    FreeAndNil(DeStreamer);
    FreeAndNil(FileStream);
  end;
  AddRoute('GET', '/stop', @handleStopRequest);
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
