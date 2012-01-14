unit untDisplayQuotes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus, ActnList;

const
  DefaultWidth  = 712;
  DefaultHeight = 237;

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
    acNotifyQuote: TAction;
    ActionList: TActionList;
    chkNotify: TCheckBox;
    ImageList: TImageList;
    lblQuotesCount: TLabel;
    btnFirst: TSpeedButton;
    btnPrev: TSpeedButton;
    btnNext: TSpeedButton;
    btnLast: TSpeedButton;
    btnReloadQuotes: TSpeedButton;
    btnCopy: TSpeedButton;
    btnFindQuote: TSpeedButton;
    btnRandom: TSpeedButton;
    mmoQuote: TMemo;
    procedure acCopyToClipboardExecute(Sender: TObject);
    procedure acFindQuoteExecute(Sender: TObject);
    procedure acNextQuoteExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    QuoteCount,
    QuoteNum    : cardinal;
    Quotes      : TStringList;
    Started     : Boolean;
    procedure IterateQuotes;
    procedure LoadQuotes;
    procedure ChangeQuote(index : Integer);
    procedure ChangeCursor(Busy : Boolean = true); inline;
  end;

var
  frmDisplayQuotes: TfrmDisplayQuotes;

resourcestring
  txtQuoteCount = '#%d/%d quotes';
  txtNotFound   = 'Could not find "%s"';

implementation

uses Math, Clipbrd, untFindQuote, untSearchDialog, untSaveSettings
{$IFDEF UNIX}
  {$IFDEF LCLGTK2}
    , untGTKNotify
  {$ENDIF}
{$ENDIF};

{$R *.lfm}

{ TfrmDisplayQuotes }

procedure TfrmDisplayQuotes.acCopyToClipboardExecute(Sender: TObject);
begin
  Clipboard.AsText := mmoQuote.Lines.Text;
end;

procedure TfrmDisplayQuotes.acFindQuoteExecute(Sender: TObject);
var
  index : integer;
  s     : string;
begin
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
  ProgramSettings.LastQuote   := QuoteNum;
  ProgramSettings.NotifyEvent := chkNotify.Checked;
  ProgramSettings.Left        := Left;
  ProgramSettings.Top         := Top;
  ProgramSettings.Width       := Width;
  ProgramSettings.Height      := Height;
  ProgramSettings.WriteFile;
  FreeAndNil(Quotes);
end;

procedure TfrmDisplayQuotes.FormCreate(Sender: TObject);
begin
  Quotes            := TStringList.Create;
  Started           := True;
  chkNotify.Checked := ProgramSettings.NotifyEvent;
  Left              := ProgramSettings.Left;
  Top               := ProgramSettings.Top;
  Width             := ProgramSettings.Width;
  Height            := ProgramSettings.Height;

  LoadQuotes;
  Started := False;
end;

procedure TfrmDisplayQuotes.LoadQuotes;
begin
  mmoQuote.Lines.Clear;
  Quotes.Clear;
  IterateQuotes;
  if Started then
    ChangeQuote(Min(Max(ProgramSettings.LastQuote, 0), QuoteCount))
  else
    ChangeQuote(0);
  lblQuotesCount.Caption := Format(txtQuoteCount, [QuoteNum +1, QuoteCount]);
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

  lblQuotesCount.Caption := Format(txtQuoteCount, [Index + 1, QuoteCount]);
  acFirstQuote.Enabled   := index > 0;
  acPrevQuote.Enabled    := index > 0;
  acNextQuote.Enabled    := index < (QuoteCount -1);
  acLastQuote.Enabled    := index < (QuoteCount -1);
  Application.ProcessMessages;

  {$IFDEF UNIX}
  if chkNotify.Checked then
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

