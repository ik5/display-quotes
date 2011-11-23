unit untDisplayQuotes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TfrmDisplayQuotes }

  TfrmDisplayQuotes = class(TForm)
    btnLoadQuotes: TButton;
    btnRandomQuote: TButton;
    btnCopyToClipboard: TButton;
    cmbxQuotes: TComboBox;
    mmoQuote: TMemo;
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure btnRandomQuoteClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cmbxQuotesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    QuoteCount: cardinal;
    procedure IterateQuotes;
  end;

var
  frmDisplayQuotes: TfrmDisplayQuotes;

implementation
Uses Clipbrd;

{$R *.lfm}

{ TfrmDisplayQuotes }

procedure TfrmDisplayQuotes.Button1Click(Sender: TObject);
begin
  Screen.Cursor := crHandPoint;
  Application.ProcessMessages;

  mmoQuote.Lines.Clear;
  cmbxQuotes.Items.BeginUpdate;
  cmbxQuotes.Items.Clear;
  IterateQuotes;
  cmbxQuotes.Items.EndUpdate;

  Screen.Cursor := crDefault;
  Application.ProcessMessages;
end;

procedure TfrmDisplayQuotes.btnRandomQuoteClick(Sender: TObject);
var
  Item : Integer;
begin
  mmoQuote.Lines.Clear;
  Item := Random(QuoteCount);
  mmoQuote.Lines.Add(cmbxQuotes.Items.Strings[Item]);
  cmbxQuotes.ItemIndex := Item;
end;

procedure TfrmDisplayQuotes.btnCopyToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := mmoQuote.Lines.Text;
end;

procedure TfrmDisplayQuotes.cmbxQuotesChange(Sender: TObject);
begin
  mmoQuote.Lines.Clear;
  mmoQuote.Lines.Text := cmbxQuotes.Text;
end;

procedure TfrmDisplayQuotes.FormCreate(Sender: TObject);
begin
  IterateQuotes;
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
      cmbxQuotes.Items.Add(quote);
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

  cmbxQuotes.ItemIndex := 0;
  mmoQuote.Lines.Add(cmbxQuotes.Text);
end;

end.

