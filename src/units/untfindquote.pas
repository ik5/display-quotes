unit untFindQuote;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

function FindNextText(S         : String;
                      List      : TStringList;
                      Sensitive : Boolean;
                      Index     : Integer) : integer;
function FindNextRegex(S         : String;
                       List      : TStringList;
                       Sensitive : Boolean;
                       Index     : Integer) : integer;

function FindPrevText(S         : String;
                      List      : TStringList;
                      Sensitive : Boolean;
                      Index     : Integer) : integer;
function FindPrevRegex(S         : String;
                       List      : TStringList;
                       Sensitive : Boolean;
                       Index     : Integer) : integer;


implementation
uses strutils,Regex;

{function FindQuoteByPart(const S : String; List: TStringList) : Integer;
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
}

{%TODO: Rewrite this implementation to be more DRY}

function FindNextText(S: String; List: TStringList; Sensitive: Boolean;
                      Index: Integer): integer;
var
  i, count : integer;
  FindFunc           : function(const AText, ASubText : String) : Boolean;
begin
 if Sensitive then
   FindFunc := @AnsiContainsStr
 else
   FindFunc := @AnsiContainsText;

 Result := -1;
 Count := List.Count -1;
 for i := Index +1 to Count do
  begin
    if FindFunc(List.Strings[i], S) then
      begin
       Result := i;
       break;
      end;
  end;
end;

function FindPrevText(S: String; List: TStringList; Sensitive: Boolean;
                      Index: Integer): integer;
var
  i, count : integer;
  FindFunc           : function(const AText, ASubText : String) : Boolean;
begin
  if Sensitive then
   FindFunc := @AnsiContainsStr
 else
   FindFunc := @AnsiContainsText;

 Result := -1;
 Count := List.Count -1;
 for i := Count downto Index +1 do
  begin
    if FindFunc(List.Strings[i], s) then
      begin
       Result := i;
       break;
      end;
  end;
end;

function FindNextRegex(S: String; List: TStringList; Sensitive: Boolean;
  Index: Integer): integer;
var
  i, count, loc, offset : integer;
  aregex                : TRegexEngine;
begin
 Result            := -1;
 offset            := 0;
 Count             := List.Count -1;
 aregex            := TRegexEngine.Create(S);
 aregex.IgnoreCase := Sensitive;
 aregex.MultiLine  := true;
 for i := Index +1 to Count do
   begin
     if aregex.MatchString(List.Strings[i], loc, offset) then
       begin
         Result := i;
         break;
       end;
   end;
 aregex.Free;
end;

function FindPrevRegex(S: String; List: TStringList; Sensitive: Boolean;
  Index: Integer): integer;
var
  i, count, loc, offset : integer;
  aregex                : TRegexEngine;
begin
 Result            := -1;
 offset            := 0;
 Count             := List.Count -1;
 aregex            := TRegexEngine.Create(S);
 aregex.IgnoreCase := Sensitive;
 aregex.MultiLine  := true;

  for i := Count downto Index +1 do
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

