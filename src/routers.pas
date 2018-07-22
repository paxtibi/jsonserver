unit routers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httproute, HTTPDefs, om, fpjson, fpjsontopas;

type
  { TRouter }

  TRouter = class(TComponent, IRouteInterface)
  private
    FDataDescriptor: TDataDescriptorObject;
    FDataSetName: string;
    procedure SetDataDescriptor(AValue: TDataDescriptorObject);
    procedure SetDataSetName(AValue: string);
  public
    function Data: TCollection;
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); virtual;
    property DataSetName: string read FDataSetName write SetDataSetName;
    property DataDescriptor: TDataDescriptorObject read FDataDescriptor write SetDataDescriptor;
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

procedure TRouter.SetDataDescriptor(AValue: TDataDescriptorObject);
begin
  if FDataDescriptor = AValue then
    Exit;
  FDataDescriptor := AValue;
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
      jsonData := parser.Parse as TJSONArray;
      jsonResponse := TJSONArray.Create();
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
      if jsonResponse.Count = 1 then
      begin
        AResponse.Content := jsonResponse.Objects[0].AsJSON;
      end
      else
      begin
        AResponse.Content := jsonResponse.AsJSON;
      end;
    finally
      jsonData.Clear;
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
