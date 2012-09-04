{ Display Quotes program

  Copyright (c) 2012 Ido Kanner

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

program DisplayQuotes;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{.$IFDEF UseCThreads}
  cthreads,
  {.$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, untDisplayQuotes, untFindQuote,
  untSaveSettings, fb_details, untSearchDialog
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

