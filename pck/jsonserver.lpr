program jsonserver;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  app,
  om,
  log4d,
  paxhttp.server,
  routers { you can add units after this };

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
      chdir(parameter);
    idx := 1;
    if ParamCount <> 0 then
    begin
      while idx < ParamCount do
      begin
        if (ParamStr(idx) = '--workingpath') or (ParamStr(idx) = '-wp') then
        begin
          Inc(idx);
          try
            parameter := ExtractFileDir(ParamStr(0)) + DirectorySeparator +
              ParamStr(idx);
            parameter := TFakeJsonServer.normalizePath(parameter);
            if DirectoryExists(parameter) then
              chdir(parameter)
            else
            begin
              Application.Terminate;
              raise Exception.Create('working dir parameter not valid');
            end;
          except
            on E: Exception do
            begin
              TLogLog.GetLogger('server').Error('processing cli parameters', e);
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
  TLogPropertyConfigurator.Configure('log4d.properties');
  processParameters;
  Application.Initialize;
  Application.AddRoute('get', '/favicon.ico', @defaultFavIcon);
  Application.StopOnException := False;
  TLogLog.GetLogger('server').info(Format('Accept request on port :%d',
    [Application.Port]));

  Application.Run;
  Application.Free;
  halt(0);
end.
