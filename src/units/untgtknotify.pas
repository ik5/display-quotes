unit untGTKNotify;

{$mode objfpc}{$H+}

interface

procedure NotifyQuote(AQuote : String);

implementation
uses
  libnotify;

procedure NotifyQuote(AQuote: String);
var
  Quote : PNotifyNotification;
begin
  Quote := notify_notification_new (
             'Display Quote',
             @AQuote[1],
             nil);
  notify_notification_show (Quote, nil);
end;

initialization
  notify_init('Display Quotes');
finalization
  notify_uninit;
end.

