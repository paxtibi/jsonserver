program jsonserver;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  app,
  om,
  paxhttp.server,
  routers { you can add units after this };

{$R *.res}

  procedure processParameters;
  var
    idx: integer;
    parameter: string;
  begin
    parameter := ExtractFileDir(Paramstr(0));
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
          inc(idx);
          try
            parameter := ExtractFileDir(Paramstr(0)) + DirectorySeparator + ParamStr(idx);
            parameter := TFakeJsonServer.normalizePath(parameter);
            writeln('Try to set ', parameter, ' as working path');
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
              Writeln(e.Message);
              ExitCode := 1;
              exit;
            end;
          end;
        end;
        inc(idx);
      end;
    end;
  end;


begin
  processParameters;
  Application.Initialize;
  Application.AddRoute('get', '/favicon.ico', @defaultFavIcon);
  Application.StopOnException := False;
  WriteLn('Accept request on port :', Application.Port);
  Application.Run;
  Application.Free;
end.
