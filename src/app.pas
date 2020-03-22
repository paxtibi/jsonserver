unit app;

{$mode objfpc}{$H+}


interface

uses
  eventlog,
  Classes, SysUtils, paxhttp.server, custhttpapp, HTTPDefs,
  fpjson, fpjsonrtti, om, fgl;

type

  { TTimerObject }

  TTimerObject = class
  private
    FRequest:   TRequest;
    Frequestor: string;
    FStart:     TDateTime;
    procedure SetRequest(AValue: TRequest);
    procedure SetRequestor(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    property Request: TRequest read FRequest write SetRequest;
    property Start: TDateTime read FStart;
    property Requestor: string read Frequestor write Setrequestor;
  end;

  TTimersHolder = specialize TFPGObjectList<TTimerObject>;

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
    FLog:    TEventLog;
    procedure ExceptionHandle(Sender: TObject; E: Exception);
    procedure SetConfig(AValue: TConfigObject);
  protected
    procedure DoLog(EventType: TEventType; const Msg: string); override;
  public
    class function normalizePath(Path: string): string;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure InitializeRouters;
    procedure StartRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
    procedure EndRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
    property Config: TConfigObject read FConfig write SetConfig;
  protected
    procedure Response404(ARequest: TRequest; AResponse: TResponse);
  end;

var
  Application: TFakeJsonServer;
  ShowCleanUpErrors: boolean = False;

procedure handleStopRequest(aReq: TRequest; aResp: TResponse; args: TStrings);
procedure handleReloadRequest(aReq: TRequest; aResp: TResponse; args: TStrings);
procedure handleEmitConfigRequest(aReq: TRequest; aResp: TResponse; args: TStrings);

implementation

uses
  dateutils, CustApp, jsonparser, routers;

procedure handleStopRequest(aReq: TRequest; aResp: TResponse; args: TStrings);
begin
  aResp.ContentType := 'text/html';
  aResp.Content     := '<body><p>Bye!</p></body>';
  Application.Terminate;
end;

procedure handleReloadRequest(aReq: TRequest; aResp: TResponse; args: TStrings);
begin
  Application.ClearRouters;
  Application.Initialize;
  aResp.ContentType := 'application/text';
  aResp.Content     := 'OK';
end;

procedure handleEmitConfigRequest(aReq: TRequest; aResp: TResponse; args: TStrings);
const
  scripts: array [0..4] of string = (
    '<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.2.1/css/bootstrap.min.css" integrity="sha384-GJzZqFGwb1QTTN6wy59ffF1BuGJpLSa9DkKMp0DgiMDm4iYMj70gZWKYbI706tWS" crossorigin="anonymous">',
    '<link rel="stylesheet" href="https://editor.swagger.io/dist/swagger-editor.css">',
    '<script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>',
    '<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.6/umd/popper.min.js" integrity="sha384-wHAiFfRlMFy6i5SRaxvfOCifBUQy1xHdJ/yoi7FRNXMRBu5WHdZYu1hA6ZOblgut" crossorigin="anonymous"></script>',
    '<script src="https://stackpath.bootstrapcdn.com/bootstrap/4.2.1/js/bootstrap.min.js" integrity="sha384-B0UglyR+jN6CkvvICOB2joaf5I4l3gm9GU6Hc1og6Ls7i6U/mkkaduKaBhlAXv9k" crossorigin="anonymous"></script>');
var
  stringItem: string;
  rc: TRouteContainer;
begin
  aResp.ContentType := 'text/html';
  aResp.Content     := '<html>' + LineEnding;
  aResp.Content     := aResp.Content + '  <head>' + LineEnding;
  aResp.Content     := aResp.Content + '  <!-- Latest compiled and minified CSS -->' + LineEnding;
  for stringItem in scripts do
  begin
    aResp.Content := aResp.Content + stringItem + LineEnding;
  end;
  aResp.Content := aResp.Content + '  </head>' + LineEnding;
  aResp.Content := aResp.Content + '  <body>' + LineEnding;
  aResp.Content := aResp.Content + '    <div class="panel panel-primary">' + LineEnding;
  aResp.Content := aResp.Content + '      <div class="panel-heading">' + LineEnding;
  aResp.Content := aResp.Content + '        <h3 class="panel-title">Current Routers</h3>' + LineEnding;
  aResp.Content := aResp.Content + '      </div>' + LineEnding;
  aResp.Content := aResp.Content + '      <div class="card" style="width: 18rem;">' + LineEnding;
  aResp.Content := aResp.Content + '        <div class="card-body">' + LineEnding;
  aResp.Content := aResp.Content + '        <ul class="list-group list-group-flush">' + LineEnding;
  for rc in Application.getRoutersList do
  begin
    aResp.Content := aResp.Content + Format('    <li class="list-group-item">%s', [LineEnding]);
    aResp.Content := aResp.Content + Format('    <div class="swagger-ui opblock opblock-options">', [LineEnding]);
    aResp.Content := aResp.Content + Format('    <div class="opblock-summary opblock-summary-options"><span class="opblock-summary-method">%s</span><span class="opblock-summary-path"><a class="nostyle"><span>%s</span></a></span><div class="opblock-summary-description">%s</div></div>', [rc.requestMethod, rc.urlPattern, '', LineEnding]);
    aResp.Content := aResp.Content + Format('    </li>%s', [LineEnding]);
  end;
  aResp.Content := aResp.Content + '  </ul>' + LineEnding;
  aResp.Content := aResp.Content + '</div>' + LineEnding;
  aResp.Content := aResp.Content + '</div>' + LineEnding;
  aResp.Content := aResp.Content + '    <div>' + LineEnding;
  aResp.Content := aResp.Content + '  </body>' + LineEnding;
  aResp.Content := aResp.Content + '</html>' + LineEnding;
end;

{ TTimersHolderHelper }

function TTimersHolderHelper.findByRequest(aRequest: TRequest): TTimerObject;
var
  cursor: TTimerObject;
begin
  Result := nil;
  for cursor in self do
  begin
    if cursor.Request = aRequest then
    begin
      Result := cursor;
    end;
  end;
end;

procedure TTimersHolderHelper.stopTimer(aTimer: TTimerObject);
var
  idx: integer;
begin
  if self <> nil then
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
end;

{ TTimerObject }

procedure TTimerObject.SetRequest(AValue: TRequest);
begin
  if FRequest = AValue then
  begin
    Exit;
  end;
  FRequest := AValue;
end;

procedure TTimerObject.SetRequestor(AValue: string);
begin
  if Frequestor = AValue then
  begin
    Exit;
  end;
  Frequestor := AValue;
end;

constructor TTimerObject.Create;
begin
  FStart := now;
end;

destructor TTimerObject.Destroy;
begin
  FRequest := nil;
  inherited Destroy;
end;

{ TFakeJsonServer }


procedure ShowRequestException(AResponse: TResponse; AnException: Exception; var handled: boolean);
begin
  Writeln(Format('serving : %s, exception: %s, message: %s', [AResponse.Referer, AnException.ClassName, AnException.Message]));
  AResponse.Code := 500;
  AResponse.Content := AnException.Message;
  handled := True;
end;

constructor TFakeJsonServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FConfig := TConfigObject.Create;
  FTimers := TTimersHolder.Create(True);
  FLog    := TEventLog.Create(self);
  FLog.Active := False;
  FLog.LogType := ltFile;
  FLog.FileName := ApplicationName + '.log';
  FLog.Active := True;
end;

destructor TFakeJsonServer.Destroy;
begin
  FreeAndNil(FTimers);
  FreeAndNil(FConfig);
  inherited Destroy;
end;

procedure TFakeJsonServer.Response404(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Content := '{ status : 400 }';
end;

procedure TFakeJsonServer.SetConfig(AValue: TConfigObject);
begin
  if FConfig = AValue then
  begin
    Exit;
  end;
  FConfig := AValue;
end;

procedure TFakeJsonServer.DoLog(EventType: TEventType; const Msg: string);
begin
  FLog.Log(EventType, Msg);
end;

procedure TFakeJsonServer.ExceptionHandle(Sender: TObject; E: Exception);
begin
  Writeln(e.Message);
end;

procedure TFakeJsonServer.StartRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
var
  Timer: TTimerObject;
begin
  Log(etInfo, '%s:[%10s]%s', [ARequest.RemoteAddress, ARequest.Method, ARequest.URL]);
  Timer := TTimerObject.Create;
  Timer.Request := ARequest;
  FTimers.Add(Timer);
end;

procedure TFakeJsonServer.EndRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
var
  timer: TTimerObject;
begin
  timer := FTimers.findByRequest(ARequest);
  if timer <> nil then
  begin
    Log(etInfo, '%s served in : %s ', [Timer.requestor, FormatDateTime('hh:nn:ss:zzzz', Now - Timer.start)]);
  end;
  FTimers.stopTimer(timer);
end;

{$ifdef Darwin}
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
      buffer.Delete(idx);
      idx := -1; // restart;
    end
    else
    if buffer[idx] = '..' then
    begin
      buffer.Delete(idx);
      buffer.Delete(idx);
      idx := -1; // restart;
    end;
    Inc(idx);
  end;
  Result := ExcludeTrailingPathDelimiter(buffer.Text);
  idx    := 0;
  FreeAndNil(buffer);
end;

{$ELSE}
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
      buffer.Delete(idx);
      idx := -1; // restart;
    end
    else
    if buffer[idx] = '..' then
    begin
      buffer.Delete(idx - 1);
      buffer.Delete(idx - 1);
      idx := -1; // restart;
    end;
    Inc(idx);
  end;
  Result := ExcludeTrailingPathDelimiter(buffer.Text);
  idx    := 0;
  FreeAndNil(buffer);
end;

{$ENDIF}

procedure TFakeJsonServer.InitializeRouters;
var
  configFileName: string;
  FileStream: TFileStream;
  DeStreamer: TJSONDeStreamer;
  c: TCollectionItem;
  r: TRouterObject;
  jsonData: TJSONStringType;
  handle: TRouter;
begin
  Log(etInfo, 'Initializing');
  configFileName := IncludeTrailingPathDelimiter(GetCurrentDir) + 'config.json';
  if not FileExists(configFileName) then
  begin
    raise Exception.Create(configFileName + ' not found, terminate');
  end;
  FileStream := TFileStream.Create(configFileName, fmOpenRead);
  SetLength(jsonData, FileStream.Size);
  FileStream.Read(jsonData[1], FileStream.Size);
  Log(etDebug, '%s', [jsonData]);
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    DeStreamer.JSONToObject(jsonData, FConfig);
    Application.Port := Config.config.Port;
    for c in Config.routers do
    begin
      r := TRouterObject(c);
      if (r.Method <> '') and (r.Route <> '') then
      begin
        if r.payload <> '' then
        begin
          handle := TPayloadRouter.Create(Application);
          TPayloadRouter(handle).compare := r.compare;
        end
        else
        begin
          handle := TRouter.Create(Application);
        end;
        if (r.outputTemplate <> '') and (r.outputKey <> '') then
        begin
          handle.output     := TOutput.Create;
          handle.output.Template := r.outputTemplate;
          handle.output.Key := r.outputKey;
        end;
        handle.DataSetName := r.dataset;
        handle.Payload := r.payload;
        handle.url := r.route;
        if (handle.dataSetName <> '') then
        begin
          if FileExists(handle.dataSetName) then
          begin
            Log(etInfo, 'Dataset %s OK', [handle.dataSetName]);
          end
          else
          begin
            Log(etInfo, 'Dataset %s ERROR', [handle.dataSetName]);
          end;
        end
        else
        begin
          Log(etInfo, 'Dataset not provided');
        end;
        AddRoute(r.method, r.route, handle);
      end;
    end;
  finally
    FreeAndNil(DeStreamer);
    FreeAndNil(FileStream);
  end;
  Log(etInfo, 'Routers count %d', [FRoutes.Count]);
  Log(etInfo, 'Done');
end;


procedure TFakeJsonServer.Initialize;
begin
  inherited Initialize;
  Application.Threaded := True;
  Application.BeforeServe := @StartRequest;
  Application.AfterServe := @EndRequest;
  OnException     := @ExceptionHandle;
  RedirectOnError := True;
end;


procedure InitHTTP;
begin
  Application := TFakeJsonServer.Create(nil);
  if not assigned(CustomApplication) then
  begin
    CustomApplication := Application;
  end;
end;

procedure DoneHTTP;
begin
  if CustomApplication = Application then
  begin
    CustomApplication := nil;
  end;
end;

initialization
  InitHTTP;

finalization
  DoneHTTP;

end.
