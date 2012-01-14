unit untSearchDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type

  { TfrmQuoteSearch }

  TfrmQuoteSearch = class(TForm)
    btnCancel: TBitBtn;
    btnFind: TBitBtn;
    cbCaseSensetive: TCheckBox;
    edtSearch: TEdit;
    grpbxSearchOptoins: TGroupBox;
    lblQuotePart: TLabel;
    rdQuote: TRadioButton;
    rdAuthor: TRadioButton;
    procedure cbCaseSensetiveChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rdQuoteChange(Sender: TObject);
  private
    FCase    : Boolean;
    FQuote   : Boolean;
    FClicked : Boolean;
    { private declarations }
  public
    { public declarations }
  published
    property CaseSensitive : Boolean read FCase write FCase;
    property Quote : Boolean read FQuote write FQuote;
  end; 

var
  frmQuoteSearch: TfrmQuoteSearch;

implementation

{$R *.lfm}

{ TfrmQuoteSearch }

procedure TfrmQuoteSearch.cbCaseSensetiveChange(Sender: TObject);
begin
  FCase := cbCaseSensetive.Checked;
end;

procedure TfrmQuoteSearch.FormCreate(Sender: TObject);
begin
  FQuote   := True;
  FCase    := False;
  FClicked := False;
end;

procedure TfrmQuoteSearch.rdQuoteChange(Sender: TObject);
begin
  FQuote := TRadioButton(Sender).Tag = 2;
end;

end.

