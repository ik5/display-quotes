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
uses FileUtil;

resourcestring
  errCreateConfigDirectory = 'Unable to create config directory "%s".';

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
end;

destructor TSettings.Destroy;
begin
  FConfig.Flush;
  FreeAndNil(FConfig);
  inherited Destroy;
end;

procedure TSettings.WriteFile;
begin

end;

end.

