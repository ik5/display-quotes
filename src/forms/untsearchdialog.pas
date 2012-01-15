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
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmQuoteSearch: TfrmQuoteSearch;

implementation
uses untDisplayQuotes;

{$R *.lfm}

{ TfrmQuoteSearch }



end.

