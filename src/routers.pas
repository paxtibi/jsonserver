unit routers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, paxhttp.server, HTTPDefs, om, fpjson;

type
  { TOutput }
  TOutput = class
  private
    FKey: string;
    FTemplate: string;
    procedure SetKey(AValue: string);
    procedure SetTemplate(AValue: string);
  published
    property Template: string read FTemplate write SetTemplate;
    property Key: string read FKey write SetKey;
  end;

  { TRouter }
  TRouter = class(TComponent, IRoute)
  private
    FDataSetName: string;
    Foutput: TOutput;
    FPayload: string;
    Fcompare: string;
    procedure SetDataSetName(AValue: string);
    procedure Setoutput(AValue: TOutput);
    procedure SetPayload(AValue: string);
    procedure Setcompare(AValue: string);
  protected
    procedure produceResponse(ARequest: TRequest; AResponse: TResponse; results: TJSONArray; templateObject: TJSONObject);
    function match(aData: TJSONObject; filter: TJSONData): boolean; virtual;
  public
    function Data: TCollection;
    procedure handleRequest(aReq: TRequest; aResp: TResponse; args: TStrings); virtual;
    property dataSetName: string read FDataSetName write SetDataSetName;
    property payload: string read FPayload write SetPayload;
    property output: TOutput read Foutput write Setoutput;
    property compare: string read Fcompare write Setcompare;
  end;

  { TPayloadRouter }

  TPayloadRouter = class(TRouter)
  private
    function matchInto(aData: TJSONData; anArray: TJSONArray): boolean;
    function match(aData: TJSONObject; filter: TJSONData): boolean;
  public
    procedure handleRequest(aReq: TRequest; aResp: TResponse; args: TStrings); override;
  end;

  { TStopHandle }

  TStopHandle = class(TRouter)
    procedure handleRequest(aReq: TRequest; aResp: TResponse; args: TStrings); override;
  end;


implementation

uses
  app, jsonparser, jsonscanner;

{ TOutput }

procedure TOutput.SetKey(AValue: string);
begin
  if FKey = AValue then
    Exit;
  FKey := AValue;
end;

procedure TOutput.SetTemplate(AValue: string);
begin
  if FTemplate = AValue then
    Exit;
  FTemplate := AValue;
end;

{ TPayloadRouter }

function TPayloadRouter.matchInto(aData: TJSONData; anArray: TJSONArray): boolean;
var
  idx: integer;
begin
  Result := False;
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
  Result := False;
  if filter = nil then
    exit;
  if aData = nil then
    exit;
  try
    jsonFilter := filter.FindPath(FPayload);
    jsonData := aData.Find(FCompare);
    if jsonFilter is TJSONArray then
    begin
      Result := matchInto(jsonData, jsonFilter as TJSONArray);
    end
    else
    begin
      Result := jsonFilter.Value = jsonData.Value;
    end;
  except
  end;
end;

procedure TPayloadRouter.handleRequest(aReq: TRequest; aResp: TResponse; args: TStrings);
var
  Parameters: TStringList;
  dataSetIndex: integer;
  dataSetFile: TFileStream;
  jsonParsed: TJSONData;
  jsonData: TJSONArray;
  jsonRecord: TJSONObject;
  jsonResponse: TJSONArray;
  payloadInput: TJSONObject;
  parser, remapParser: TJSONParser;
  matchParameters: boolean;
  remapObject: TJSONData = nil;

  payloadString: string;
begin
  Parameters := TStringList.Create;
  try
    try
      aResp.ContentType := 'application/json';
      jsonResponse := TJSONArray.Create();
      try
        dataSetFile := TFileStream.Create(self.FDataSetName, fmOpenRead);
        parser := TJSONParser.Create(dataSetFile, [joUTF8, joStrict, joComments, joIgnoreTrailingComma]);
        jsonParsed := parser.Parse;
        FreeAndNil(parser);
        payloadString := aReq.Content;
        parser := TJSONParser.Create(payloadString, [joUTF8, joStrict, joComments, joIgnoreTrailingComma]);
        payloadInput := TJSONObject(parser.Parse);
        FreeAndNil(parser);
        if (Output <> nil) then
        begin
          Writeln('Using : ', output.Template);
          remapParser := TJSONParser.Create(output.Template, [joUTF8, joStrict, joComments, joIgnoreTrailingComma]);
          remapObject := remapParser.Parse;
          if (remapObject is TJSONObject) then
          begin
            jsonResponse := (remapObject as TJSONObject).Find(output.key) as TJSONArray;
          end;
          FreeAndNil(remapParser);
        end;
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
        produceResponse(aReq, aResp, jsonResponse, remapObject as TJSONObject);
      except
        on e: Exception do
          Writeln(e.message);
      end;
    finally
      try
        if assigned(jsonParsed) then
          jsonParsed.Clear;
        if (remapObject <> nil) then
          FreeAndNil(remapObject)
        else
          FreeAndNil(jsonResponse);
        if (dataSetFile <> nil) and (dataSetFile.Handle > 0) then
          FreeAndNil(dataSetFile);
        FreeAndNil(jsonData);
      except
        on E: Exception do
        begin
          Writeln('An error freeing memory, check configuration file please:');
          Writeln(e.Message);
        end;
      end;
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

procedure TStopHandle.handleRequest(aReq: TRequest; aResp: TResponse; args: TStrings);
begin
  aResp.ContentType := 'application/text';
  aResp.Content := 'OK';
  Application.Terminate;
end;

{ TRouter }

procedure TRouter.SetDataSetName(AValue: string);
begin
  if FDataSetName = AValue then
    Exit;
  FDataSetName := AValue;
end;

procedure TRouter.Setoutput(AValue: TOutput);
begin
  if Foutput = AValue then
    Exit;
  Foutput := AValue;
end;

procedure TRouter.SetPayload(AValue: string);
begin
  if FPayload = AValue then
    Exit;
  FPayload := AValue;
end;

procedure TRouter.Setcompare(AValue: string);
begin
  Fcompare := AValue;
end;

procedure TRouter.produceResponse(ARequest: TRequest; AResponse: TResponse; results: TJSONArray; templateObject: TJSONObject);
begin
  if templateObject = nil then
  begin
    if (results <> nil) and (results.Count = 1) then
    begin
      AResponse.Content := (results as TJSONArray).Objects[0].AsJSON;
    end
    else
    begin
      if results <> nil then
        AResponse.Content := results.FormatJSON([])
      else
        AResponse.Content := 'null';
    end;
  end
  else
    AResponse.Content := templateObject.FormatJSON([]);
end;

function TRouter.match(aData: TJSONObject; filter: TJSONData): boolean;
begin
  result := True;
end;

function TRouter.Data: TCollection;
begin
  Result := (Owner as TFakeJsonServer).Config.Data;
end;

procedure TRouter.handleRequest(aReq: TRequest; aResp: TResponse; args: TStrings);
var
  index, dataSetIndex: integer;
  aName, aValue, aDataSetValue: string;
  dataSetFile: TFileStream = nil;
  jsonParsed: TJSONData = nil;
  jsonData: TJSONArray = nil;
  jsonRecord: TJSONObject = nil;
  jsonResponse: TJSONArray = nil;
  parser: TJSONParser = nil;
  remapParser: TJSONParser = nil;
  matchParameters: boolean;
  remapObject: TJSONData = nil;
begin
  try
    try
      aResp.ContentType := 'application/json';
      try
        if (self.FDataSetName <> '') then
        begin
          dataSetFile := TFileStream.Create(self.FDataSetName, fmOpenRead);
          parser := TJSONParser.Create(dataSetFile, [joUTF8, joStrict, joComments, joIgnoreTrailingComma]);
          if (output <> nil) and (output.Template <> '') and (output.Key <> '') then
          begin
            remapParser := TJSONParser.Create(output.template, [joUTF8, joStrict, joComments, joIgnoreTrailingComma]);
            remapObject := remapParser.Parse;
            if (remapObject is TJSONObject) then
            begin
              jsonResponse := (remapObject as TJSONObject).Find(output.key) as TJSONArray;
            end;
            FreeAndNil(remapParser);
          end
          else
          begin
            jsonResponse := TJSONArray.Create();
          end;
          jsonParsed := parser.Parse;
          if (jsonParsed is TJSONArray) then
          begin
            jsonData := jsonParsed as TJSONArray;
            for dataSetIndex := 0 to jsonData.Count - 1 do
            begin
              jsonRecord := jsonData.Objects[dataSetIndex];
              matchParameters := True;
              for index := 0 to args.Count - 1 do
              begin
                args.GetNameValue(index, aName, aValue);
                if (aName <> '') then
                begin
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
        end;
        produceResponse(aReq, aResp, jsonResponse, remapObject as TJSONObject);
      except
        on e: Exception do
          Writeln(e.message);
      end;
    finally
      if assigned(jsonParsed) then
        jsonParsed.Clear;
      if (remapObject <> nil) then
        FreeAndNil(remapObject)
      else
        FreeAndNil(jsonResponse);
      try
        if (dataSetFile <> nil) and (dataSetFile.Handle >= 0) then
          FreeAndNil(dataSetFile);
      except
      end;
      try
        FreeAndNil(parser);
      except
      end;
      try
        FreeAndNil(jsonData);
      except
      end;
    end;
  except
    on e: Exception do
    begin
      Writeln(e.Message);
    end;
  end;
end;


end.
