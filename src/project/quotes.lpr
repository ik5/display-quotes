program quotes;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{.$IFDEF UseCThreads}
  cthreads,
  {.$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, untDisplayQuotes
  { you can add units after this };

{$R *.res}

procedure set_random;
var
  f    : file of cardinal;
begin
  filemode := 0;
  AssignFile(f, '/dev/urandom');
  {$I-}reset (f,1); {$I+}
  if IOResult <> 0 then Exit;
  read (f,RandSeed);
  CloseFile (f);
  Randomize;
end;

begin
  set_random;
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmDisplayQuotes, frmDisplayQuotes);
  Application.Run;
end.

