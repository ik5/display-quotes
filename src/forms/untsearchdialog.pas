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

{var
  index : integer;
  s     : string;}

  (*
    frmQuoteSearch := TfrmQuoteSearch.Create(self);
    try
      if frmQuoteSearch.ShowModal in [mrAbort, mrCancel, mrNo, mrNoToAll] then
        exit;

      s := frmQuoteSearch.edtSearch.Text;
      if Trim(s) =  '' then
        exit;

      index := FindQuoteByPart(s, Quotes);
      if index = -1 then
        begin
          ShowMessage(Format(txtNotFound, [s]));
          exit;
        end;

      ChangeQuote(index);
      Beep;
    finally
      FreeAndNil(frmQuoteSearch);
    end;
  *)

procedure TfrmQuoteSearch.btnPrevClick(Sender: TObject);
begin

end;

procedure TfrmQuoteSearch.btnNextClick(Sender: TObject);
begin

end;

{$R *.lfm}

{ TfrmQuoteSearch }



end.

