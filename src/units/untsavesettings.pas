unit untSaveSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jsonConf;

const
  SettingsFileName = 'settings.json';
  DefaultDirName   = 'display_quotes';

type

  { TSettings }

  TSettings = class
  private
    FHeight      : Integer;
    FHight       : Integer;
    FLastQuote   : Integer;
    FLeft        : Integer;
    FNotifyEvent : Boolean;
    FConfig      : TJSONConfig;
    FTop         : Integer;
    FWidth       : Integer;

    function GetAppConfigDir : String;
  public
    constructor Create;     virtual;
    destructor Destroy;     override;
    procedure WriteFile;    virtual;
  published
    property Height      : Integer read FHeight      write FHight;
    property LastQuote   : Integer read FLastQuote   write FLastQuote;
    property Left        : Integer read FLeft        write FLeft;
    property NotifyEvent : Boolean read FNotifyEvent write FNotifyEvent;
    property Top         : Integer read FTop         write FTop;
    property Width       : Integer read FWidth       write FWidth;
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
  WindowWidthPath  = '/window/demensions/width';
  WindowHeightPath = '/window/demensions/height';

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
   UserDir := self.GetAppConfigDir;
   if not DirectoryExists(UserDir) then
     if not ForceDirectories(UserDir) then
       raise Exception.CreateFmt(errCreateConfigDirectory, [UserDir]);

  FConfig          := TJSONConfig.Create(nil);
  FConfig.Filename := UserDir + SettingsFileName;;

  FLastQuote   := FConfig.GetValue(LastQuotePath, 0);
  FNotifyEvent := FConfig.GetValue(NotifyEventPath, true);
  FLeft        := FConfig.GetValue(WindowLeftPath,
                              (Screen.Width - DefaultWidth) div 2);
  FTop         := FConfig.GetValue(WindowTopPath,
                              (Screen.Height - DefaultHeight) div 2);
  FWidth       := FConfig.GetValue(WindowWidthPath, DefaultWidth);
  FHeight      := FConfig.GetValue(WindowHeightPath, DefaultHeight);
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
  FConfig.SetValue(WindowLeftPath, FLeft);
  FConfig.SetValue(WindowTopPath, FTop);
  FConfig.SetValue(WindowWidthPath, FWidth);
  FConfig.SetValue(WindowHeightPath, FHeight);
  FConfig.Flush;
end;

end.

