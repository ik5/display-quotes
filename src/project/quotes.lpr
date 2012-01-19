program quotes;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{.$IFDEF UseCThreads}
  cthreads,
  {.$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, untDisplayQuotes, untFindQuote,
  untSaveSettings, untSearchDialog
  {$IFDEF UNIX}
    {$IFDEF LCLGTK2}
    , libnotify, untGTKNotify
    {$ENDIF}
  {$ENDIF}
  { you can add units after this };

{$R *.res}

{$IFDEF UNIX}
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
end;
{$ENDIF}

begin
  Application.Title:='Display Quotes';
  {$IFDEF UNIX}set_random;{$ENDIF}
  Randomize;
  // For Lazarus 0.9.31 and beyond ...
  {$IF defined(RequireDerivedFormResource)}
  RequireDerivedFormResource := True;
  {$ENDIF}
  Application.Initialize;

  ProgramSettings := TSettings.Create;

  Application.CreateForm(TfrmDisplayQuotes, frmDisplayQuotes);
  Application.Run;

  ProgramSettings.Free;
end.

