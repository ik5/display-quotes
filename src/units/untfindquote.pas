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
uses strutils, SynRegExpr; //Regex;

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
 for i := Index to Count do
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
  i        : integer;
  FindFunc : function(const AText, ASubText : String) : Boolean;
begin
  if Sensitive then
   FindFunc := @AnsiContainsStr
 else
   FindFunc := @AnsiContainsText;

 Result := -1;
 for i := index downto 0 do
  begin
    if FindFunc(List.Strings[i], s) then
      begin
       Result := i;
       break;
      end;
  end;
end;

function initregex(regex : String; Sensitive : Boolean) : TRegExpr;
begin
 Result    := TRegExpr.Create;

 Result.ModifierI := not Sensitive; //
 Result.ModifierM := true;          // multiline
 Result.ModifierG := true;          // not greedy
 try
  Result.Expression := Regex;
 except // bad regex syntax
   Result.Free;
   Exit(nil);
 end;
end;

function FindNextRegex(S: String; List: TStringList; Sensitive: Boolean;
  Index: Integer): integer;
var
  i, count : integer;
  regex    : TRegExpr;
begin
 Result := -1;
 Count  := List.Count -1;
 regex  := initregex(s, Sensitive);

 if not Assigned(regex) then
  Exit(-1);

 for i := Index to Count do
   begin
     if regex.Exec(List.Strings[i]) then
       begin
         Result := i;
         break;
       end;
   end;

 regex.Free;
end;

function FindPrevRegex(S: String; List: TStringList; Sensitive: Boolean;
  Index: Integer): integer;
var
  i     : integer;
  regex : TRegExpr;
begin
 Result  := -1;
 regex  := initregex(s, Sensitive);

 if not Assigned(regex) then
   Exit(-1);

 for i := Index downto 0 do
   begin
     if regex.Exec(List.Strings[i]) then
       begin
         Result := i;
         break;
       end;
   end;

  regex.Free;
end;

end.

