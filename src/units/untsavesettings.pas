unit untSaveSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jsonConf;

const
  SettingsFileName = 'settings.json';

type

  { TSettings }

  TSettings = class
  private
    FLastQuote   : Integer;
    FNotifyEvent : Boolean;
    FWindow      : TRect;
    FConfig      : TJSONConfig;
  public
    constructor Create;     virtual;
    destructor Destroy;     override;
    procedure WriteFile;    virtual;
  published
    property LastQuote   : Integer read FLastQuote   write FLastQuote;
    property NotifyEvent : Boolean read FNotifyEvent write FNotifyEvent;
    property Window      : TRect   read FWindow      write FWindow;
  end;

var
  ProgramSettings : TSettings;

implementation
uses FileUtil, Forms, untDisplayQuotes;

resourcestring
  errCreateConfigDirectory = 'Unable to create config directory "%s".';

const
  LastQuotePath    = '/quotes/LastQuote';
  NotifyEventPath  = '/window/NotifyEvent';
  WindowLeftPath   = '/window/demensions/left';
  WindowTopPath    = '/window/demensions/top';
  WindowRightPath  = '/window/demensions/right';
  WindowBottomPath = '/window/demensions/bottom';

{ TSettings }

constructor TSettings.Create;
var
  UserDir : String;
begin
   UserDir := GetAppConfigDir(False);
   if not
   if not DirectoryExists(UserDir) then
     if not ForceDirectories(UserDir) then
       raise Exception.CreateFmt(errCreateConfigDirectory, [UserDir]);

  FConfig          := TJSONConfig.Create(nil);
  FConfig.Filename := UserDir + SettingsFileName;;

  FLastQuote     := FConfig.GetValue(LastQuotePath, 0);
  FNotiftEvent   := FConfig.GetValue(NotifyEventPath, true);
  FWindow.Left   := FConfig.GetValue(WindowLeftPath,
                              (Screen.Width div 2) - DefaultWidth);
  FWindow.Top    := FConfig.GetValue(WindowTopPath,
                              (Screen.Height div 2) - DefaultHeight);
  FWindow.Right  := FConfig.GetValue(WindowRightPath, DefaultWidth);
  FWindow.Bottom := FConfig.GetValue(WindowBottomPath, DefaultHeight);
end;

destructor TSettings.Destroy;
begin
  FConfig.Flush;
  FreeAndNil(FConfig);
  inherited Destroy;
end;

procedure TSettings.WriteFile;
begin
  FConfig.SetValue(LastQuotePath, FLastQuote);
  FConfig.SetValue(NotifyEventPath, FNotifyEvent);
  FConfig.SetValue(WindowLeftPath, FWindow.Left);
  FConfig.SetValue(WindowTopPath, FWindow.Top);
  FConfig.SetValue(WindowRightPath, FWindow.Right);
  FConfig.SetValue(WindowBottomPath, FWindow.Bottom);
  FConfig.Flush;
end;

end.

