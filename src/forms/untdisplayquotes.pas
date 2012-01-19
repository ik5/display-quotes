unit untDisplayQuotes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus, ActnList, untSearchDialog;

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
    acQuit: TAction;
    acDisplayTooltip: TAction;
    acUseTray: TAction;
    ActionList: TActionList;
    btnCopy: TSpeedButton;
    btnFirst: TSpeedButton;
    btnLast: TSpeedButton;
    btnNext: TSpeedButton;
    btnPrev: TSpeedButton;
    btnRandom: TSpeedButton;
    btnReloadQuotes: TSpeedButton;
    chkNotify: TCheckBox;
    frmSearchDialog1: TfrmSearchDialog;
    ImageList: TImageList;
    lblQuotesCount: TLabel;
    mnuQuite: TMenuItem;
    mnuSep3: TMenuItem;
    mnuReloadQuote: TMenuItem;
    mnuDisplayToolTip: TMenuItem;
    mnuCopyToClipboard: TMenuItem;
    mnuSep2: TMenuItem;
    MenuItem1mnuRandomQuote: TMenuItem;
    mnuLastQuote: TMenuItem;
    mnuNextQuote: TMenuItem;
    mnuPrevQuote: TMenuItem;
    mnuQuotes: TMenuItem;
    mnuFirstQuote: TMenuItem;
    mnusep: TMenuItem;
    mmoQuote: TMemo;
    pnlTop: TPanel;
    ppmnuTray: TPopupMenu;
    Tray: TTrayIcon;
    procedure acCopyToClipboardExecute(Sender: TObject);
    procedure acDisplayTooltipExecute(Sender: TObject);
    procedure acFindQuoteExecute(Sender: TObject);
    procedure acNextQuoteExecute(Sender: TObject);
    procedure acQuitExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TrayDblClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    QuoteCount,
    QuoteNum    : cardinal;
    Quotes      : TStringList;
    Started     : Boolean;
    QuoteFile   : String;
    procedure IterateQuotes;
    procedure LoadQuotes;
    procedure ChangeQuote(index : Integer);
    procedure ChangeCursor(Busy : Boolean = true); inline;
    procedure PromptFile; inline;
  end;

var
  frmDisplayQuotes: TfrmDisplayQuotes;

resourcestring
  txtQuoteCount = '#%d/%d quotes';
  txtNotFound   = 'Could not find "%s"';


  txtQuoteOpenDialogTitle   = 'Please select the quote file:';
  txtQuoteFileNotFoundTitle = 'The quote file was not found';
  txtQuoteFileNotFoundBody  = 'The file "%s" was not found.' +
                              'Would you like to look for it ?';
  txtNoQuotesInTheListTitle = 'No Quotes';
  txtNoQuotesInTheListBody  = 'There are no quotes in the list.'
                              + LineEnding +
                              'The application will exit now';

implementation

uses Math, Clipbrd, untSaveSettings
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

procedure TfrmDisplayQuotes.acDisplayTooltipExecute(Sender: TObject);
begin
  ProgramSettings.DisplayToolTip := acDisplayTooltip.Checked;
end;

procedure TfrmDisplayQuotes.acFindQuoteExecute(Sender: TObject);
begin
  frmSearchDialog1.edtSearch.SetFocus;
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

procedure TfrmDisplayQuotes.acQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmDisplayQuotes.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ProgramSettings.LastQuote   := QuoteNum;
  ProgramSettings.NotifyEvent := chkNotify.Checked;
  ProgramSettings.Left        := Left;
  ProgramSettings.Top         := Top;
  ProgramSettings.Width       := Width;
  ProgramSettings.Height      := Height;
  ProgramSettings.QuoteFile   := QuoteFile;
  ProgramSettings.FormVisible := Self.Visible;
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
  QuoteFile         := ProgramSettings.QuoteFile;
  Tray.Visible      := ProgramSettings.UseTray;

  if Tray.Visible then
    begin
      // if the settings are for invisible form, but no systray, then ignore it...
      Self.Visible             := ProgramSettings.FormVisible;
      Tray.Icon                := Application.Icon;
      acDisplayTooltip.Checked := ProgramSettings.DisplayToolTip;
    end;

  LoadQuotes;
  Started := False;
end;

procedure TfrmDisplayQuotes.TrayDblClick(Sender: TObject);
begin
  self.Visible := not self.Visible;
end;

procedure TfrmDisplayQuotes.LoadQuotes;
begin
  mmoQuote.Lines.Clear;
  Quotes.Clear;
  IterateQuotes;

  if Quotes.Count = 0 then // Avoid reading empty content ...
    begin
      MessageDlg(txtNoQuotesInTheListTitle, txtNoQuotesInTheListBody,
                 mtError, [mbOK], 0);
      Application.Terminate;
      Application.ProcessMessages;
      exit;
    end;

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

  if Tray.Visible then
    if (ProgramSettings.DisplayToolTip) and not Started  then
      begin
        Tray.BalloonHint  := AQuote;
        Tray.BalloonFlags := bfNone;
        Tray.BalloonTitle := 'Display Quote';
        Tray.ShowBalloonHint;
      end;
end;

procedure TfrmDisplayQuotes.ChangeCursor(Busy : Boolean = true);
const CursorImage : array[Boolean] of TCursor = (crDefault, crHourGlass);
begin
   Screen.Cursor := CursorImage[Busy];
  Application.ProcessMessages;
end;

procedure TfrmDisplayQuotes.PromptFile;
  procedure show_dialog; inline;
  var
    opendialog : TOpenDialog;
  begin
   opendialog          := TOpenDialog.Create(self);
   opendialog.Filter   := 'Text Files|*.txt|All Files|*'
                          {$IFDEF WINDOWS} + '.*' {$ENDIF};
   opendialog.Title    := txtQuoteOpenDialogTitle;
   opendialog.FileName := QuoteFile;
   opendialog.Options  := [ofReadOnly, ofPathMustExist, ofFileMustExist,
                           ofAutoPreview] + DefaultOpenDialogOptions;
   try
     if not opendialog.Execute then
       begin
         QuoteFile := '';
         Exit;
       end;

     QuoteFile := opendialog.FileName;
   finally
     opendialog.Free;
   end;
  end;

var Button : Integer;

begin
  Button := MessageDlg(txtQuoteFileNotFoundTitle,
                      Format(txtQuoteFileNotFoundBody, [QuoteFile]),
                      mtError, mbYesNo, 0);
 if Button = mrNo then
   begin
    QuoteFile := '';
    Exit;
   end;

 show_dialog;
end;

procedure TfrmDisplayQuotes.IterateQuotes;
var
  qtfl        : TextFile;
  line, quote : ansistring;

begin
  QuoteFile := ExpandFileNameUTF8(QuoteFile);
  if not FileExists(QuoteFile) then
    begin
      PromptFile;
    end;

  if QuoteFile = '' then Exit;

  AssignFile(qtfl, QuoteFile);
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

