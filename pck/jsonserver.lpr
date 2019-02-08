program jsonserver;

{$mode objfpc}{$H+}
{$IFNDEF    Windows}
 {$define UseCThreads}
{$ENDIF}

uses {$IFDEF UseCThreads}
  cthreads, {$ENDIF}
  Classes,
  SysUtils,
  app,
  paxhttp.server,
  paxlog
  { you can add units after this };

{$R *.res}

  procedure processParameters;
  var
    idx: integer;
    parameter: string;
  begin
    parameter := ExtractFileDir(ParamStr(0));
    parameter += DirectorySeparator + '..';
    parameter += DirectorySeparator + '..';
    parameter += DirectorySeparator + 'data';
    parameter := TFakeJsonServer.normalizePath(parameter);
    if DirectoryExists(parameter) then
    begin
      chdir(parameter);
    end;
    idx := 1;
    if ParamCount <> 0 then
    begin
      while idx < ParamCount do
      begin
        if (ParamStr(idx) = '--workingpath') or (ParamStr(idx) = '-wp') then
        begin
          Inc(idx);
          try
            if ParamStr(idx)[1] = DirectorySeparator then
            begin
              parameter := ParamStr(idx);
            end
            else
            begin
              parameter := '.';
            end;
            parameter := TFakeJsonServer.normalizePath(parameter);
            if DirectoryExists(parameter) then
            begin
              chdir(parameter);
            end
            else
            begin
              Application.Terminate;
              raise Exception.Create('working dir parameter not valid');
            end;
          except
            on E: Exception do
            begin
              Writeln('processing cli parameters', e.message);
              ExitCode := 1;
              exit;
            end;
          end;
        end;
        Inc(idx);
      end;
    end;
  end;


begin
  processParameters;
  Application.Initialize;
  Application.AddRoute('get', '/favicon.ico', @defaultFavIcon);
  Application.AddRoute('GET', '/server/stop', @handleStopRequest);
  Application.AddRoute('GET', '/server/reload', @handleReloadRequest);
  Application.AddRoute('GET', '/server/config', @handleEmitConfigRequest);
  Application.StopOnException := False;
  TFakeJsonServer(Application).InitializeRouters;
  getLogger('Server').info('Accept request on port :%d', [Application.Port]);
  Application.Run;
  Application.Free;
end.
