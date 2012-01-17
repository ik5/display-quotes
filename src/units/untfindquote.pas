unit untFindQuote;

{$mode objfpc}{$H+}

interface

uses
  Classes; // For TStringList

type
 TSearchDirection = (sdPrev, sdNext);

function FindQuote(s         : String;
                   List      : TStringList;
                   Sensitive : Boolean;
                   Index     : Integer;
                   Regex     : Boolean;
                   Direction : TSearchDirection) : Integer; inline;


implementation
uses strutils, SynRegExpr;

type
  TFindProc = function(S : String; List : TStringList; Sensitive : Boolean;
                       Index : Integer) : integer;

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

function FindQuote(s : String; List: TStringList; Sensitive: Boolean;
  Index: Integer; Regex: Boolean; Direction: TSearchDirection): Integer;
const
  cNextFind : array[Boolean] of TFindProc = (@FindNextText, @FindNextRegex);
  cPrevFind : array[Boolean] of TFindProc = (@FindPrevText, @FindPrevRegex);
var
  FindProc : TFindProc;
begin
  case Direction of
   sdPrev : FindProc := cPrevFind[Regex];
   sdNext : FindProc := cNextFind[Regex];
  end;

  Result := FindProc(s, List, Sensitive, Index);
end;

end.

