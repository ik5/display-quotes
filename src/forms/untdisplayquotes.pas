unit untDisplayQuotes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus, ActnList, StdActns;

type

  { TfrmDisplayQuotes }

  TfrmDisplayQuotes = class(TForm)
    acFindQuote: TAction;
    ActionList: TActionList;
    btnCopyToClipBoard: TBitBtn;
    btnRandomQuote: TBitBtn;
    btnReloadQuotes: TBitBtn;
    cmbxQuotes: TComboBox;
    lblQuoteNumber: TLabel;
    lblQuotesCount: TLabel;
    MainMenu: TMainMenu;
    mnuFindQuote: TMenuItem;
    mmoQuote: TMemo;
    procedure acFindQuoteExecute(Sender: TObject);
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure btnRandomQuoteClick(Sender: TObject);
    procedure btnReloadQuotesClick(Sender: TObject);
    procedure cmbxQuotesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure mnuFindQuoteClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    QuoteCount: cardinal;
    Quotes: TStringList;
    procedure IterateQuotes;
    procedure LoadQuotes;
    procedure ChangeQuote(index : Integer);
  end;

var
  frmDisplayQuotes: TfrmDisplayQuotes;

resourcestring
  txtQuoteCount = '#%d quotes loaded';
  txtQuoteNumber = 'Quote #%d';

implementation

uses Clipbrd, untFindQuote
{$IFDEF UNIX}
  {$IFDEF LCLGTK2}
    , untGTKNotify
  {$ENDIF}
{$ENDIF};

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
  ChangeQuote(Item);

  Screen.Cursor := crDefault;
  Application.ProcessMessages;
end;

procedure TfrmDisplayQuotes.btnCopyToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := mmoQuote.Lines.Text;
end;

procedure TfrmDisplayQuotes.acFindQuoteExecute(Sender: TObject);
begin
  //
end;

procedure TfrmDisplayQuotes.cmbxQuotesChange(Sender: TObject);
begin
  ChangeQuote(cmbxQuotes.ItemIndex);
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

procedure TfrmDisplayQuotes.mnuFindQuoteClick(Sender: TObject);
begin

end;

procedure TfrmDisplayQuotes.LoadQuotes;
begin
  mmoQuote.Lines.Clear;
  cmbxQuotes.Items.BeginUpdate;
  cmbxQuotes.Items.Clear;
  IterateQuotes;
  cmbxQuotes.Items.Assign(Quotes);
  cmbxQuotes.Items.EndUpdate;
  ChangeQuote(0);
  lblQuotesCount.Caption := Format(txtQuoteCount, [Quotes.Count]);
end;

procedure TfrmDisplayQuotes.ChangeQuote(index: Integer);
var
  AQuote : String;
begin
  AQuote := Quotes.Strings[Index];
  mmoQuote.Lines.BeginUpdate;
  mmoQuote.Lines.Clear;
  mmoQuote.Lines.Add(AQuote);
  mmoQuote.Lines.EndUpdate;
  cmbxQuotes.ItemIndex := Index;
  lblQuoteNumber.Caption := Format(txtQuoteNumber, [Index + 1]);
  {$IFDEF UNIX}
    NotifyQuote(AQuote);
  {$ENDIF}
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

