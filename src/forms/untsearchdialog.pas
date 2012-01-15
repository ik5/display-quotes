unit untSearchDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, untDisplayQuotes;

type

  { TfrmQuoteSearch }

  TfrmQuoteSearch = class(TFrame)
    btnNext: TBitBtn;
    btnPrev: TBitBtn;
    cbxCaseSensitive: TCheckBox;
    cbxRegex: TCheckBox;
    edtSearch: TEdit;
    lblNotFound: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmQuoteSearch: TfrmQuoteSearch;

implementation

{$R *.lfm}

{ TfrmQuoteSearch }



end.

