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
type
  TFindProc = function(S : String; List : TStringList; Sensitive : Boolean;
                       Index : Integer) : integer;
const
  cNextFind : array[Boolean] of TFindProc = (@FindNextText, @FindNextRegex);
  cPrevFind : array[Boolean] of TFindProc = (@FindPrevText, @FindPrevRegex);

var
  FindProc      : TFindProc;
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
      if loc = 0 then // Circular search
        loc := max
      else
        dec(loc); // from prev position

      FindProc := cPrevFind[cbxRegex.Checked];
    end
  else begin // next ?
      if loc = max then // Circular search
        loc := 0
      else
        inc(loc); // from next position

      FindProc := cNextFind[cbxRegex.Checked];
  end;

  idx := FindProc(Search,
                  frmDisplayQuotes.Quotes,
                  cbxCaseSensitive.Checked,
                  loc);

  lblNotFound.Visible := idx = -1;
  if idx <> -1 then
    frmDisplayQuotes.ChangeQuote(idx);
end;

procedure TfrmSearchDialog.edtSearchChange(Sender: TObject);
var
  txt : String;
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

