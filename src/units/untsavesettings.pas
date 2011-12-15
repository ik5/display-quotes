unit untSaveSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jsonConf;

type

  { TSettings }

  TSettings = class
  private
    FLastQuote   : Integer;
    FNotifyEvent : Boolean;
    FWindow      : TRect;
    FFile        : String;
    FConfig      : TJSONConfig;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Save;     virtual;
    procedure Write;    virtual;
  published
    property LastQuote   : Integer read FLastQuote   write FLastQuote;
    property NotifyEvent : Boolean read FNotifyEvent write FNotifyEvent;
    property Window      : TRect   read FWindow      write FWindow;
  end;

var
  ProgramSettings : TSettings;

implementation

{ TSettings }

constructor TSettings.Create;
var
  UserDir : String;
begin
   UserDir := GetAppConfigDir(False);
   if not DirectoryExists(UserDir) then
     ForceDirectories(UserDir);
end;

destructor TSettings.Destroy;
begin
  inherited Destroy;
end;

procedure TSettings.Save;
begin

end;

procedure TSettings.Write;
begin

end;

end.

