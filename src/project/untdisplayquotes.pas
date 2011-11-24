unit untDisplayQuotes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons;

type

  { TfrmDisplayQuotes }

  TfrmDisplayQuotes = class(TForm)
    btnCopyToClipBoard: TBitBtn;
    btnRandomQuote: TBitBtn;
    btnReloadQuotes: TBitBtn;
    cmbxQuotes: TComboBox;
    lblQuoteNumber: TLabel;
    lblQuotesCount: TLabel;
    mmoQuote: TMemo;
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure btnRandomQuoteClick(Sender: TObject);
    procedure btnReloadQuotesClick(Sender: TObject);
    procedure cmbxQuotesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    QuoteCount: cardinal;
    Quotes: TStringList;
    procedure IterateQuotes;
    procedure LoadQuotes;
  end;

var
  frmDisplayQuotes: TfrmDisplayQuotes;

resourcestring
  txtQuoteCount = '#%d quotes loaded';
  txtQuoteNumber = 'Quote #%d';

implementation

uses Clipbrd;

{$R *.lfm}

{ TfrmDisplayQuotes }

procedure TfrmDisplayQuotes.btnReloadQuotesClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;

  LoadQuotes;

  Screen.Cursor := crDefault;
  Application.ProcessMessages;
end;

procedure TfrmDisplayQuotes.btnRandomQuoteClick(Sender: TObject);
var
  Item: integer;
begin
  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;

  Item := Random(QuoteCount);
  mmoQuote.Lines.BeginUpdate;
  mmoQuote.Lines.Clear;
  mmoQuote.Lines.Add(Quotes.Strings[Item]);
  mmoQuote.Lines.EndUpdate;
  cmbxQuotes.ItemIndex := Item;
  lblQuoteNumber.Caption := Format(txtQuoteNumber, [Item + 1]);

  Screen.Cursor := crDefault;
  Application.ProcessMessages;
end;

procedure TfrmDisplayQuotes.btnCopyToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := mmoQuote.Lines.Text;
end;

procedure TfrmDisplayQuotes.cmbxQuotesChange(Sender: TObject);
begin
  mmoQuote.Lines.Clear;
  mmoQuote.Lines.Text := cmbxQuotes.Text;
  lblQuoteNumber.Caption := Format(txtQuoteNumber, [cmbxQuotes.ItemIndex + 1]);
end;

procedure TfrmDisplayQuotes.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(Quotes);
end;

procedure TfrmDisplayQuotes.FormCreate(Sender: TObject);
begin
  Quotes := TStringList.Create;
  LoadQuotes;
end;

procedure TfrmDisplayQuotes.LoadQuotes;
begin
  mmoQuote.Lines.Clear;
  cmbxQuotes.Items.BeginUpdate;
  cmbxQuotes.Items.Clear;
  IterateQuotes;
  cmbxQuotes.Items.Assign(Quotes);
  cmbxQuotes.ItemIndex := 0;
  cmbxQuotes.Items.EndUpdate;
  mmoQuote.Lines.Text    := cmbxQuotes.Text;
  lblQuotesCount.Caption := Format(txtQuoteCount, [Quotes.Count]);
  lblQuoteNumber.Caption := Format(txtQuoteNumber, [1]);
end;

procedure TfrmDisplayQuotes.IterateQuotes;
var
  qtfl: TextFile;
  line, quote: ansistring;
begin
  AssignFile(qtfl, ExpandFileNameUTF8('~/quotes.txt'));
  reset(qtfl);
  quote := '';
  QuoteCount := 0;
  while not EOF(qtfl) do
  begin
    line := '';
    ReadLn(qtfl, line);
    if TrimRight(line) = '----' then
    begin
      Quotes.Add(quote);
      quote := '';
      Inc(QuoteCount);
    end
    else
    begin
      if quote <> '' then
        quote := quote + LineEnding + line
      else
        quote := line;
    end;
  end;
  CloseFile(qtfl);
end;

end.

