unit untDisplayQuotes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus, ActnList;

type

  { TfrmDisplayQuotes }

  TfrmDisplayQuotes = class(TForm)
    acFindQuote: TAction;
    acNextQuote: TAction;
    acPrevQuote: TAction;
    acFirstQuote: TAction;
    acLastQuote: TAction;
    acChangeQuote: TAction;
    acReloadQuotes: TAction;
    acCopyToClipboard: TAction;
    acRandomQuote: TAction;
    ActionList: TActionList;
    btnNextQuote: TBitBtn;
    btnLastQuote: TBitBtn;
    btnPrevQuote: TBitBtn;
    btnFirstQuote: TBitBtn;
    btnCopyToClipBoard: TBitBtn;
    btnRandomQuote: TBitBtn;
    btnReloadQuotes: TBitBtn;
    ImageList1: TImageList;
    lblQuoteNumber: TLabel;
    lblQuotesCount: TLabel;
    MainMenu: TMainMenu;
    mnuFindQuote: TMenuItem;
    mmoQuote: TMemo;
    procedure acChangeQuoteExecute(Sender: TObject);
    procedure acCopyToClipboardExecute(Sender: TObject);
    procedure acFindQuoteExecute(Sender: TObject);
    procedure acNextQuoteExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure mnuFindQuoteClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    QuoteCount,
    QuoteNum    : cardinal;
    Quotes      : TStringList;
    procedure IterateQuotes;
    procedure LoadQuotes;
    procedure ChangeQuote(index : Integer);
    procedure ChangeCursor(Busy : Boolean = true); inline;
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

procedure TfrmDisplayQuotes.acChangeQuoteExecute(Sender: TObject);
begin

end;

procedure TfrmDisplayQuotes.acCopyToClipboardExecute(Sender: TObject);
begin
  Clipboard.AsText := mmoQuote.Lines.Text;
end;

procedure TfrmDisplayQuotes.acFindQuoteExecute(Sender: TObject);
begin
  //
end;

procedure TfrmDisplayQuotes.acNextQuoteExecute(Sender: TObject);
var
  Item : Integer;
begin
  ChangeCursor;
  case TAction(Sender).Tag of
    1 : Item := QuoteNum +1;
    2 : Item := QuoteNum -1;
    3 : Item := 0;
    4 : Item := QuoteCount -1;
    5 : ;
    6 : begin
          Item := -1;
          LoadQuotes;
        end;

    7 : Item := Random(QuoteCount);
  end;

  if Item > -1 then
    ChangeQuote(Item);

  ChangeCursor(false);
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
  IterateQuotes;
  ChangeQuote(0);
  lblQuotesCount.Caption := Format(txtQuoteCount, [Quotes.Count]);
end;

procedure TfrmDisplayQuotes.ChangeQuote(index: Integer);
var
  AQuote : String;
begin
  QuoteNum := Index;
  AQuote   := Quotes.Strings[Index];
  mmoQuote.Lines.BeginUpdate;
  mmoQuote.Lines.Clear;
  mmoQuote.Lines.Add(AQuote);
  mmoQuote.Lines.EndUpdate;
  lblQuoteNumber.Caption := Format(txtQuoteNumber, [Index + 1]);
  acFirstQuote.Enabled   := index > 0;
  acPrevQuote.Enabled    := index > 0;
  acNextQuote.Enabled    := index < (QuoteCount -1);
  acLastQuote.Enabled    := index < (QuoteCount -1);
  Application.ProcessMessages;

  {$IFDEF UNIX}
  NotifyQuote(AQuote);
  {$ENDIF}
end;

procedure TfrmDisplayQuotes.ChangeCursor(Busy : Boolean = true);
const CursorImage : array[Boolean] of TCursor = (crDefault, crHourGlass);
begin
   Screen.Cursor := CursorImage[Busy];
  Application.ProcessMessages;
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

