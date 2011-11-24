unit untQuoteDBUS;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function NotifyQuote(const AQuote : AnsiString) : Boolean;

implementation
uses dbus;

type
  EDbusEception = class(Exception);

  { TDBusConnection }

  TDBusConnection = class
  private
    FConn : PDBusConnection;
    FErr  : DBusError;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function SendNotification(const AStr : AnsiString) : Boolean;
  end;

function NotifyQuote(const AQuote: AnsiString): Boolean;
var
  DBusConn : TDBusConnection;

begin
  DBusConn   := nil;
  try
    DBusConn := TDBusConnection.Create;
    try
      Result := DBusConn.SendNotification(AQuote);
    finally
      if Assigned(DBusConn) then
        FreeAndNil(DBusConn);
    end;
  Except
    Result := False;
  end;
end;

{ TDBusConnection }

constructor TDBusConnection.Create;
var msg : String;
begin
  // init errors
  dbus_error_init(@FErr);

  { Connection }
  FConn := dbus_bus_get(DBUS_BUS_SESSION, @FErr);
  if dbus_error_is_set(@FErr) <> 0 then
    begin
     msg := StrPas(FErr.message);
     dbus_error_free(@FErr);
     raise EDbusEception.CreateFmt('DBUS Error: %s', [msg]);
    end;

  if FConn = nil then
    raise EDbusEception.Create('Unable to initialize the dbus connection.');
end;

destructor TDBusConnection.Destroy;
begin
  inherited Destroy;
end;

function TDBusConnection.SendNotification(const AStr: AnsiString): Boolean;
var
  msg      : PDBusMessage;
  args     : DBusMessageIter;
  pending  : PDBusPendingCall;
  Answer   : dbus_bool_t;
  id       : Integer;
  Timeout  : LongInt;
  AppName,
  AppIcon,
  Summery,
  Body,
  Hash     : PChar;
  Arr      : PLongint;
begin
  msg := dbus_message_new_method_call('org.freedesktop.Notifications',  // target for the method call
                                      '/org/freedesktop/Notifications', // object to call on
                                      'org.freedesktop.Notifications',  // interface to call on
                                      'Notify');                        // method name
  if msg = nil then Exit(False);

  dbus_message_iter_init_append(msg, @args);
  dbus_message_set_no_reply(msg, 1); // don't really wait for an answer ...
  id      := 0;
  Timeout := -1;
  Arr := Getmem(0);
  AppName := 'Display Quote';
  AppIcon := '';
  Summery := 'Testing';
  Body    := 'Hello World';
  Hash    := '{}';
  Answer := dbus_message_append_args(msg,
         DBUS_TYPE_STRING,
       [
         @AppName,                    // App name
         DBUS_TYPE_UINT32,
         @id,                        // Request ID
         DBUS_TYPE_STRING,
         @AppIcon,                   // App Icon Path
         DBUS_TYPE_STRING,
         @Summery,                    // Summery
         DBUS_TYPE_STRING,
         @Body,                       // Body
         DBUS_TYPE_ARRAY,
         // actions
         DBUS_TYPE_UINT32, Arr, 0,
         DBUS_TYPE_STRING,
         @Hash,                       // hints
         DBUS_TYPE_UINT32,
         @Timeout,                   // TIMEOUT
         DBUS_TYPE_INVALID
       ]
  );
  Freemem(Arr);
  if (Answer = 0) then Exit(False);

  Answer := dbus_connection_send_with_reply(FConn, msg, @pending, -1);
  if (Answer = 0) then Exit(False);

  dbus_connection_flush(FConn);
  dbus_message_unref(msg);

  Result := True;
end;

end.

