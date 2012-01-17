unit untSearchDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type

  { TfrmQuoteSearch }

  TfrmQuoteSearch = class(TFrame)
    btnNext: TBitBtn;
    btnPrev: TBitBtn;
    cbxCaseSensitive: TCheckBox;
    cbxRegex: TCheckBox;
    edtSearch: TEdit;
    lblFind: TLabel;
    lblNotFound: TLabel;
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmQuoteSearch: TfrmQuoteSearch;

implementation
uses untDisplayQuotes, untFindQuote;

{$R *.lfm}

{ TfrmQuoteSearch }

{%todo more DRY}

procedure TfrmQuoteSearch.btnPrevClick(Sender: TObject);
var
  index : integer;
  s     : string;
begin
  s := edtSearch.Text;
  if cbxRegex.Checked then
   index := FindPrevRegex(s, frmDisplayQuotes.Quotes,
                          cbxCaseSensitive.Checked,
                          frmDisplayQuotes.QuoteNum)
  else
    index := FindPrevText(s, frmDisplayQuotes.Quotes,
                          cbxCaseSensitive.Checked,
                          frmDisplayQuotes.QuoteNum);

  lblNotFound.Visible := index = -1;
  if index <> -1 then
   frmDisplayQuotes.ChangeQuote(index);
end;

procedure TfrmQuoteSearch.btnNextClick(Sender: TObject);
var
  index : integer;
  s     : string;
begin
  s := edtSearch.Text;
  if cbxRegex.Checked then
   index := FindNextRegex(s, frmDisplayQuotes.Quotes,
                          cbxCaseSensitive.Checked,
                          frmDisplayQuotes.QuoteNum)
  else
    index := FindNextText(s, frmDisplayQuotes.Quotes,
                          cbxCaseSensitive.Checked,
                          frmDisplayQuotes.QuoteNum);

  lblNotFound.Visible := index = -1;
  if index <> -1 then
   frmDisplayQuotes.ChangeQuote(index);
end;

end.

