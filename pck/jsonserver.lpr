program jsonserver;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  app, om, routers { you can add units after this };

begin
  Application.Initialize;
  Application.StopOnException := False;
  WriteLn('Accept request on port :', Application.Port);
  Application.Run;
  Application.Free;
end.


