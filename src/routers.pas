unit routers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httproute, HTTPDefs, om, fpjson;

type
  { TRouter }

  TRouter = class(TComponent, IRouteInterface)
  private
    FDataSetName: string;
    FPayload: string;
    procedure SetDataSetName(AValue: string);
    procedure SetPayload(AValue: string);
  public
    function Data: TCollection;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); virtual;
    property DataSetName: string read FDataSetName write SetDataSetName;
    property Payload: string read FPayload write SetPayload;
  end;

  { TPayloadRouter }

  TPayloadRouter = class(TRouter)
  private
    Fcompare: string;
    function matchInto(aData: TJSONData; anArray: TJSONArray): boolean;
    function match(aData: TJSONObject; filter: TJSONData): boolean;
    procedure Setcompare(AValue: string);
  public
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
    property compare: string read Fcompare write Setcompare;
  end;

  { TStopHandle }

  TStopHandle = class(TRouter)
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

function getRouteMethod(aMethodString: string): TRouteMethod;

implementation

uses
  app, jsonparser, jsonscanner;

function getRouteMethod(aMethodString: string): TRouteMethod;
begin
  aMethodString := upperCase(aMethodString);
  if aMethodString = 'GET' then
    exit(rmGet)
  else if aMethodString = 'POST' then
    exit(rmPost)
  else if aMethodString = 'PUT' then
    exit(rmPut)
  else if aMethodString = 'OPTIONS' then
    exit(rmOptions)
  else if aMethodString = 'HEAD' then
    exit(rmHead)
  else if aMethodString = 'TRACE' then
    exit(rmTrace)
  else
    exit(rmAll);
end;

{ TPayloadRouter }

procedure TPayloadRouter.Setcompare(AValue: string);
begin
  if Fcompare = AValue then
    Exit;
  Fcompare := AValue;
end;

function TPayloadRouter.matchInto(aData: TJSONData; anArray: TJSONArray): boolean;
var
  idx: integer;
begin
  result := False;
  for idx := 0 to anArray.Count - 1 do
  begin
    if anArray[idx].Value = aData.Value then
      exit(True);
  end;
end;

function TPayloadRouter.match(aData: TJSONObject; filter: TJSONData): boolean;
var
  jsonData, jsonFilter: TJSONData;
begin
  result := False;
  if filter = nil then
    exit;
  if aData = nil then
    exit;
  try
    jsonFilter := filter.FindPath(FPayload);
    jsonData := aData.Find(FCompare);
    if jsonFilter is TJSONArray then
    begin
      result := matchInto(jsonData, jsonFilter as TJSONArray);
    end
    else
    begin
      result := jsonFilter.Value = jsonData.Value;
    end;
  except
  end;
end;

procedure TPayloadRouter.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  Parameters: TStringList;
  dataSetIndex: integer;
  dataSetFile: TFileStream;
  jsonParsed: TJSONData;
  jsonData: TJSONArray;
  jsonRecord: TJSONObject;
  jsonResponse: TJSONArray;
  payloadInput: TJSONObject;
  parser: TJSONParser;
  matchParameters: boolean;
  payloadString: string;
begin
  Parameters := TStringList.Create;
  try
    AResponse.ContentType := 'application/json';
    httprouter.GetHTTPRoute(ARequest.URL, getRouteMethod(ARequest.Method), Parameters);
    jsonResponse := TJSONArray.Create();
    try
      dataSetFile := TFileStream.Create(self.FDataSetName, fmOpenRead);
      parser := TJSONParser.Create(dataSetFile, [joUTF8, joStrict, joComments, joIgnoreTrailingComma]);
      jsonParsed := parser.Parse;
      FreeAndNil(parser);

      payloadString := ARequest.Content;
      parser := TJSONParser.Create(payloadString, [joUTF8, joStrict, joComments, joIgnoreTrailingComma]);
      payloadInput := TJSONObject(parser.Parse);
      FreeAndNil(parser);

      if (jsonParsed is TJSONArray) then
      begin
        jsonData := jsonParsed as TJSONArray;
        for dataSetIndex := 0 to jsonData.Count - 1 do
        begin
          matchParameters := True;
          jsonRecord := TJSONObject(jsonData[dataSetIndex]);
          matchParameters := match(jsonRecord, payloadInput);
          if matchParameters then
            jsonResponse.Add(jsonRecord.Clone);
        end;
      end
      else
      begin
        jsonResponse.Add(jsonParsed.Clone);
      end;
      if jsonResponse.Count = 1 then
      begin
        AResponse.Content := jsonResponse.Objects[0].AsJSON;
      end
      else
      begin
        AResponse.Content := jsonResponse.AsJSON;
      end;
    finally
      if assigned(jsonParsed) then
        jsonParsed.Clear;
      jsonResponse.Clear;
      freeAndNil(jsonResponse);
      FreeAndNil(dataSetFile);
      FreeAndNil(jsonData);
    end;
  except
    on e: Exception do
    begin
      Writeln(e.Message);
    end;
  end;
  FreeAndNil(Parameters);
end;

{ TStopHandle }

procedure TStopHandle.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.ContentType := 'application/text';
  AResponse.Content := 'OK';
  Application.Terminate;
end;

{ TRouter }

procedure TRouter.SetDataSetName(AValue: string);
begin
  if FDataSetName = AValue then
    Exit;
  FDataSetName := AValue;
end;

procedure TRouter.SetPayload(AValue: string);
begin
  if FPayload = AValue then
    Exit;
  FPayload := AValue;
end;

function TRouter.Data: TCollection;
begin
  result := (Owner as TFakeJsonServer).Config.data;
end;

procedure TRouter.HandleRequest(ARequest: TRequest; AResponse: TResponse);
var
  Parameters: TStringList;
  index, dataSetIndex: integer;
  aName, aValue, aDataSetValue: string;
  dataSetFile: TFileStream;
  jsonParsed: TJSONData;
  jsonData: TJSONArray;
  jsonRecord: TJSONObject;
  jsonResponse: TJSONArray;
  parser: TJSONParser;
  matchParameters: boolean;
begin
  Parameters := TStringList.Create;
  try
    AResponse.ContentType := 'application/json';
    httprouter.GetHTTPRoute(ARequest.URL, getRouteMethod(ARequest.Method), Parameters);
    try
      dataSetFile := TFileStream.Create(self.FDataSetName, fmOpenRead);
      parser := TJSONParser.Create(dataSetFile, [joUTF8, joStrict, joComments, joIgnoreTrailingComma]);
      jsonResponse := TJSONArray.Create();
      jsonParsed := parser.Parse;
      if (jsonParsed is TJSONArray) then
      begin
        jsonData := jsonParsed as TJSONArray;
        for dataSetIndex := 0 to jsonData.Count - 1 do
        begin
          matchParameters := True;
          for index := 0 to Parameters.Count - 1 do
          begin
            Parameters.GetNameValue(index, aName, aValue);
            if (aName <> '') then
            begin
              jsonRecord := jsonData.Objects[dataSetIndex];
              if jsonRecord <> nil then
              begin
                aDataSetValue := jsonRecord.Get(aName);
                if aDataSetValue <> aValue then
                begin
                  matchParameters := False;
                  break;
                end;
              end;
            end
            else
            begin
              matchParameters := False;
            end;
          end;
          if matchParameters then
            jsonResponse.Add(jsonRecord.Clone);
        end;
      end
      else
      begin
        jsonResponse.Add(jsonParsed.Clone);
      end;
      if jsonResponse.Count = 1 then
      begin
        AResponse.Content := jsonResponse.Objects[0].AsJSON;
      end
      else
      begin
        AResponse.Content := jsonResponse.AsJSON;
      end;
    finally
      if assigned(jsonParsed) then
        jsonParsed.Clear;
      jsonResponse.Clear;
      freeAndNil(jsonResponse);
      FreeAndNil(dataSetFile);
      FreeAndNil(parser);
      FreeAndNil(jsonData);
    end;
  except
    on e: Exception do
    begin
      Writeln(e.Message);
    end;
  end;
  FreeAndNil(Parameters);
end;


end.
