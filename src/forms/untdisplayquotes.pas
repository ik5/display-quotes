{ Main GUI form to display quotes

  Copyright (c) 2012 Ido Kanner

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
unit untDisplayQuotes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus, ActnList, untSearchDialog, untConsts;

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
    acNewQuote: TAction;
    acEditQuote: TAction;
    acSaveQuote: TAction;
    acDeleteQuote: TAction;
    acToggleModes: TAction;
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
    pnlEdit: TPanel;
    pnlNavigation: TPanel;
    pnlTop: TPanel;
    ppmnuTray: TPopupMenu;
    btnMode: TSpeedButton;
    btnNewQuote: TSpeedButton;
    btnEditQuote: TSpeedButton;
    btnSaveQuote: TSpeedButton;
    btnDeleteQuote: TSpeedButton;
    Tray: TTrayIcon;
    procedure acCopyToClipboardExecute(Sender: TObject);
    procedure acDisplayTooltipExecute(Sender: TObject);
    procedure acFindQuoteExecute(Sender: TObject);
    procedure acNextQuoteExecute(Sender: TObject);
    procedure acQuitExecute(Sender: TObject);
    procedure acToggleModesExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
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
    Mode        : TModes;
    procedure IterateQuotes;
    procedure LoadQuotes;
    procedure ChangeQuote(index : Integer);
    procedure ChangeCursor(Busy : Boolean = true); inline;
    procedure PromptFile; inline;
    procedure LoadConfig;
    procedure SetPanelState(const panel : TPanel; const Vis : Boolean);
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
  txtHideToTrayTitle        = 'Close or Hide ?';
  txtHideToTrayBody         = 'A close request was given.' +
                              ' Do you want to hide to tray ?';

implementation

uses Math, Clipbrd, untSaveSettings
{$IFDEF UNIX}
  {$IFDEF LCLGTK2}
  {$DEFINE NOTIFY}
    , untGTKNotify
  {$ENDIF}
{$ENDIF};

{$R *.lfm}

{ TfrmDisplayQuotes }

procedure TfrmDisplayQuotes.LoadConfig;
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

  {$IFNDEF NOTIFY}
    acNotifyQuote.Visible := false;
  {$ENDIF}

  if Tray.Visible then
    begin
      // if the settings are for invisible form, but no systray, then ignore it...
      Self.Visible             := ProgramSettings.FormVisible;
      Tray.Icon                := Application.Icon;
      acDisplayTooltip.Checked := ProgramSettings.DisplayToolTip;
    end;

  pnlEdit.Top  := pnlNavigation.Top;
  Mode         := ProgramSettings.Mode;
  case Mode of
    mdNav  : ;
    mdEdit : acToggleModes.Execute;
  end;
end;

procedure TfrmDisplayQuotes.SetPanelState(const panel: TPanel;
  const Vis: Boolean);
var i : Integer;
begin
  for i := 0 to panel.ControlCount -1 do
    begin
      if panel.Controls[i] is TSpeedButton then
        begin
          if Assigned(TSpeedButton(panel.Controls[i]).Action) then
            TAction(TSpeedButton(panel.Controls[i]).Action).Enabled := Vis
          else
            TSpeedButton(panel.Controls[i]).Enabled := Vis;
        end;
    end;
end;

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

procedure TfrmDisplayQuotes.acToggleModesExecute(Sender: TObject);
var
  vispan, hidpan : TPanel;
begin
  if acToggleModes.Checked then
    begin
      pnlNavigation.Visible := True;
      pnlEdit.Visible       := False;
      acToggleModes.Checked := False;
      vispan                := pnlNavigation;
      hidpan                := pnlEdit;
      Mode                  := mdNav;
    end
  else begin
    pnlNavigation.Visible := False;
    pnlEdit.Visible       := True;
    acToggleModes.Checked := True;
    vispan                := pnlEdit;
    hidpan                := pnlNavigation;
    Mode                  := mdEdit;
  end;

  SetPanelState(hidpan, False);
  SetPanelState(vispan, True);
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
  ProgramSettings.Mode        := Mode;
  ProgramSettings.WriteFile;
  FreeAndNil(Quotes);
end;

procedure TfrmDisplayQuotes.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  button : integer;
begin
  if not Tray.Visible then
    begin
      CanClose := True;
      Exit;
    end;

  button := MessageDlg(txtHideToTrayTitle, txtHideToTrayBody,
               mtConfirmation, mbYesNoCancel, 0);

  CanClose := button = mrNo;
  if button = mrCancel then exit;

  Hide;
end;

procedure TfrmDisplayQuotes.FormCreate(Sender: TObject);
begin
  LoadConfig;
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

  {$IFDEF NOTIFY}
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

