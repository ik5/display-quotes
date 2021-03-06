{ Saving and loading settings t

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
unit untSaveSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jsonConf, untConsts;

const
  SettingsFileName = 'settings.json';
  DefaultDirName   = 'display_quotes';
  DefaultWidth     = 712;
  DefaultHeight    = 237;

type

  { TSettings }

  TSettings = class
  private
    FDisplayToolTip : Boolean;
    FFormVisible    : Boolean;
    FHeight         : Integer;
    FHight          : Integer;
    FLastQuote      : Integer;
    FLeft           : Integer;
    FMode           : TModes;
    FNotifyEvent    : Boolean;
    FConfig         : TJSONConfig;
    FQuoteFile      : String;
    FTop            : Integer;
    FWidth          : Integer;
    FUseTray        : Boolean;

    function GetAppConfigDir : String;
  public
    DefaultQuoteFile : String;

    constructor Create;     virtual;
    destructor Destroy;     override;
    procedure WriteFile;    virtual;
  published
    property DisplayToolTip : Boolean read  FDisplayToolTip
                                      write FDisplayToolTip;
    property FormVisible    : Boolean  read FFormVisible
                                      write FFormVisible;
    property Height         : Integer  read FHeight
                                      write FHight;
    property LastQuote      : Integer  read FLastQuote
                                      write FLastQuote;
    property Left           : Integer  read FLeft
                                      write FLeft;
    property Mode           : TModes   read FMode
                                      write FMode;
    property NotifyEvent    : Boolean  read FNotifyEvent
                                      write FNotifyEvent;
    property Top            : Integer  read FTop
                                      write FTop;
    property QuoteFile      : String   read FQuoteFile
                                      write FQuoteFile;
    property UseTray        : Boolean  read FUseTray
                                      write FUseTray;
    property Width          : Integer  read FWidth
                                      write FWidth;
  end;

var
  ProgramSettings : TSettings;

implementation
uses FileUtil, Forms, untDisplayQuotes;

resourcestring
  errCreateConfigDirectory = 'Unable to create config directory "%s".';

const
  NotifyEventPath    = '/window/NotifyEvent';
  WindowLeftPath     = '/window/demensions/left';
  WindowTopPath      = '/window/demensions/top';
  WindowWidthPath    = '/window/demensions/width';
  WindowHeightPath   = '/window/demensions/height';
  WindowMode         = '/window/mode';
  LastQuotePath      = '/quotes/LastQuote';
  QuoteFilePath      = '/quotes/file';


  UseTrayPath        = '/window/tray/use';
  DisplayTooltipPath = '/window/tray/displaytooltip';
  FormVisiblePath    = '/window/tray/formvisible';


{ TSettings }

// Small hack for having the proper directory name
function TSettings.GetAppConfigDir : String;
begin
  Result := StringReplace(SysUtils.GetAppConfigDir(False),
              Application.Title, DefaultDirName,
              [rfReplaceAll, rfIgnoreCase]);
end;

constructor TSettings.Create;
var
  UserDir : String;
begin
  {$IFDEF UNIX}
    DefaultQuoteFile := '~/quotes.txt';
  {$ENDIF}
  {$IFDEF WINDOWS}
    DefaultQuoteFile := GetAppConfigDirUTF8(false) + 'quotes.txt';
  {$ENDIF}
   UserDir := self.GetAppConfigDir;
   if not DirectoryExists(UserDir) then
     if not ForceDirectories(UserDir) then
       raise Exception.CreateFmt(errCreateConfigDirectory, [UserDir]);

  FConfig          := TJSONConfig.Create(nil);
  FConfig.Filename := UserDir + SettingsFileName;;

  FLastQuote      := FConfig.GetValue(LastQuotePath, 0);
  FNotifyEvent    := FConfig.GetValue(NotifyEventPath, true);
  FLeft           := FConfig.GetValue(WindowLeftPath,
                              (Screen.Width - DefaultWidth) div 2);
  FTop            := FConfig.GetValue(WindowTopPath,
                              (Screen.Height - DefaultHeight) div 2);
  FWidth          := FConfig.GetValue(WindowWidthPath, DefaultWidth);
  FHeight         := FConfig.GetValue(WindowHeightPath, DefaultHeight);
  FQuoteFile      := FConfig.GetValue(QuoteFilePath, DefaultQuoteFile);
  FDisplayToolTip := FConfig.GetValue(DisplayTooltipPath, false);
  FFormVisible    := FConfig.GetValue(FormVisiblePath, true);
  FUseTray        := FConfig.GetValue(UseTrayPath, true);
  FMode           := TModes(FConfig.GetValue(WindowMode, Ord(mdNav)));
end;

destructor TSettings.Destroy;
begin
  FConfig.Flush;
  FreeAndNil(FConfig);
  inherited Destroy;
end;

procedure TSettings.WriteFile;
begin
  FConfig.SetValue(LastQuotePath,      FLastQuote);
  FConfig.SetValue(NotifyEventPath,    FNotifyEvent);
  FConfig.SetValue(WindowLeftPath,     FLeft);
  FConfig.SetValue(WindowTopPath,      FTop);
  FConfig.SetValue(WindowWidthPath,    FWidth);
  FConfig.SetValue(WindowHeightPath,   FHeight);
  FConfig.SetValue(QuoteFilePath,      FQuoteFile);
  FConfig.SetValue(DisplayTooltipPath, FDisplayToolTip);
  FConfig.SetValue(FormVisiblePath,    FFormVisible);
  FConfig.SetValue(UseTrayPath,        FUseTray);
  FConfig.SetValue(WindowMode,         Ord(FMode));
  FConfig.Flush;
end;

end.

