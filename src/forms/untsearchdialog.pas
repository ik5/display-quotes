{ Display search frame

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

unit untSearchDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons;

type

  { TfrmSearchDialog }

  TfrmSearchDialog = class(TFrame)
    btnNext: TBitBtn;
    btnPrev: TBitBtn;
    cbxCaseSensitive: TCheckBox;
    cbxRegex: TCheckBox;
    edtSearch: TEdit;
    lblFind: TLabel;
    lblNotFound: TLabel;
    procedure btnPrevClick(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure edtSearchKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation
uses untFindQuote, untDisplayQuotes;

{$R *.lfm}

{ TfrmSearchDialog }

procedure TfrmSearchDialog.btnPrevClick(Sender: TObject);
var
 idx, loc, max : integer;
 search        : String;

begin
 Search := edtSearch.Text;
 loc    := frmDisplayQuotes.QuoteNum;
 max    := frmDisplayQuotes.QuoteCount -1;
 if Search = '' then
   begin
     Beep;
     exit;
   end;

  if TComponent(Sender).Tag = 1 then // prev ?
    begin
      // Circular search
      if loc = 0 then loc := max
      else dec(loc); // from prev position
    end
  else begin // next ?
      // Circular search
      if loc = max then loc := 0
      else inc(loc); // from next position
  end;

  frmDisplayQuotes.ChangeCursor;
  idx := FindQuote(Search,
                  frmDisplayQuotes.Quotes,
                  cbxCaseSensitive.Checked,
                  loc,
                  cbxRegex.Checked,
                  TSearchDirection(TComponent(Sender).Tag -1));
  frmDisplayQuotes.ChangeCursor(false);

  lblNotFound.Visible := idx = -1;
  if idx <> -1 then frmDisplayQuotes.ChangeQuote(idx);
end;

procedure TfrmSearchDialog.edtSearchChange(Sender: TObject);
var txt : String;
begin
  txt             := edtSearch.Text;
  btnPrev.Enabled := txt <> '';
  btnNext.Enabled := txt <> '';
end;

procedure TfrmSearchDialog.edtSearchKeyPress(Sender: TObject; var Key: char);
begin
  if (key = #13) and (edtSearch.Text <> '') then //enter key
    btnNext.Click;
end;

end.

