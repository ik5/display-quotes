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
  TStrFindFunc = function(const AText, ASubText : String) : Boolean;
  TCounterProc = procedure(var x : integer);
  TFindFunc    = function(sub, s : String) : Boolean;
  TInitFunc    = function (s : String; Sensitive : Boolean) : Boolean;

var
  rx          : TRegExpr;
  StrFindFunc : TStrFindFunc;

const
  cSensitiveFind : array[Boolean] of TStrFindFunc =
    (@AnsiContainsText, @AnsiContainsStr);

procedure myinc(var x : integer); inline; begin inc(x); end;
procedure mydec(var x : integer); inline; begin dec(x); end;

function initregex(regex : String; Sensitive : Boolean) : TRegExpr;
begin
 Result           := TRegExpr.Create;

 Result.ModifierI := not Sensitive; //
 Result.ModifierM := true;          // multiline
 Result.ModifierG := true;          // not greedy unless the regex itself is
 try
  Result.Expression := Regex;
 except // bad regex syntax
   Result.Free;
   Result := nil;
 end;
end;

function RegexFind(sub, s : String) : Boolean; inline;
begin
  Result := rx.Exec(s);
end;

function TextFind(sub, s : String) : Boolean;
begin
 Result := StrFindFunc(s, sub);
end;

function init_regex(s : String; Sensitive : Boolean) : Boolean; inline;
begin
  rx     := initregex(s, Sensitive);
  Result := assigned(rx)
end;

function init_text(s : String; Sensitive : Boolean) : Boolean; inline;
begin
 StrFindFunc := cSensitiveFind[Sensitive];
 Result := True;
end;

const
 cCounterProc : array[TSearchDirection] of TCounterProc =
   (@mydec, @myinc);
 cFindFunc    : array[Boolean]          of TFindFunc    =
   (@TextFind, @RegexFind);
 cInitFunc    : array[Boolean]          of TInitFunc    =
   (@init_text, @init_regex);

function FindQuote(s : String; List: TStringList; Sensitive: Boolean;
  Index: Integer; Regex: Boolean; Direction: TSearchDirection): Integer;
var
  FindProc  : TFindFunc;
  InitFunc  : TInitFunc;
  loop_dir  : TCounterProc;
  i, Finish : Integer;

begin
 loop_dir    := cCounterProc[Direction];
 FindProc    := cFindFunc[Regex];
 InitFunc    := cInitFunc[Regex];
 i           := Index;
 Result      := -1;

 if not InitFunc(s, Sensitive) then Exit(-1);
 if Direction = sdPrev         then Finish := 0
 else                               Finish := List.Count -1;

 while I <> Finish do begin
  if FindProc(s, List.Strings[i]) then begin
     Result := i;
     break;
  end;
  loop_dir(i);
 end;

 if Assigned(rx) then rx.Free;
end;

end.

