unit untFindQuote;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

// Search by S, and return the index or -1 if not found
function FindQuoteByPart(const S : String; List : TStringList) : Integer;

implementation
uses Regex;

function FindQuoteByPart(const S : String; List: TStringList) : Integer;
var
  i, count, loc, offset : integer;
  aregex                : TRegexEngine;
begin
 Result            := -1;
 offset            := 0;
 Count             := List.Count -1;
 aregex            := TRegexEngine.Create(S);
 aregex.IgnoreCase := true;
 aregex.MultiLine  := true;
 for i := 0 to Count do
   begin
     if aregex.MatchString(List.Strings[i], loc, offset) then
       begin
         Result := i;
         break;
       end;
   end;
 aregex.Free;
end;

end.

