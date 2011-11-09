(******************************************************************************)
(*
(*  Context Scripting Suite
(*
(*  General routines registration
(*
(*                TCtxPkgSysUtils = class (TCtxPackage)
(*
(*  Copyright (c) 2010 Michael Baytalsky
(*
(******************************************************************************)
unit CtxPkgSysUtils;

{$I CtxVer.inc}

interface

uses Classes, CtxScript;

type
  {:: The only purpose of this component is to make CtxUnitSysUtils unit is }
  {:: included into uses clause. }
  TCtxPkgSysUtils = class (TCtxPackage);

  procedure InitCtxSysUtils;

implementation

uses Windows, SysUtils, {$IFnDEF VER130}Variants,{$ENDIF} ComObj, ActiveX;

{ System & SysUtils }

procedure _write(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  I: Integer;
begin
  // An example of procedure with variable number of parameters
  CheckParams(ParCount, 1, 256);
  for I := ParCount downto 1 do
    write(Sender.GetParam(I));
end;

procedure _writeln(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  I: Integer;
begin
  // An example of procedure with variable number of parameters
  CheckParams(ParCount, 1, 256);
  for I := ParCount downto 2 do
    write(Sender.GetParam(I));
  writeln(Sender.GetParam(1));
end;

procedure _Sin(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := sin(Sender.GetParam(1));
end;

procedure _Cos(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := cos(Sender.GetParam(1));
end;

procedure _IFF(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
  if GetParam(3) then
    Result := GetParam(2)
  else Result := GetParam(1)
end;

procedure _Min(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  I: Integer;
  V: Variant;
begin
  CheckParams(ParCount, 2, MaxInt);
  Sender.Result := Sender.GetParam(1);
  for I := 2 to ParCount do
  begin
    V := Sender.GetParam(I);
    if V < Sender.Result then Sender.Result := V;
  end;
end;

procedure _Trunc(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := Trunc(Sender.Pop);
end;

procedure _Round(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := Round(Sender.Pop);
end;

procedure _Max(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  I: Integer;
  V: Variant;
begin
  CheckParams(ParCount, 2, MaxInt);
  Sender.Result := Sender.GetParam(1);
  for I := 2 to ParCount do
  begin
    V := Sender.GetParam(I);
    if V > Sender.Result then Sender.Result := V;
  end;
end;

procedure _Coalesce(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  DefaultValue: Variant;
begin
  CheckParams(ParCount, 1, 2);
  if ParCount = 2 then
    DefaultValue := Sender.Pop
  else DefaultValue := 0.0;
  Sender.Result := Sender.Pop;
  if VarIsNull(Sender.Result) or VarIsEmpty(Sender.Result) then
    Sender.Result := DefaultValue;
end;

procedure _VarToStr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := VarToStr(Sender.GetParam(1));
end;

procedure _VarToStrEx(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := VarToStrEx(Sender.GetParam(1));
end;

procedure _VarArrayCreate(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := VarArrayCreate([0, Sender.GetParam(1)], varVariant);
end;

procedure _VarArrayRedim(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  NewDim: Integer;
  Arr: Variant;
begin
  NewDim := Sender.GetParam(1);
  Arr := Sender.GetParam(1);
  VarArrayRedim(Arr, NewDim - 1);
  Sender.Result := Arr;
end;

procedure _VarArrayOf(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  I: Integer;
  Arr: Variant;
begin
  CheckParams(ParCount, 1, MaxInt);
  Arr := VarArrayCreate([0, ParCount - 1], varVariant);
  for I := 1 to ParCount do
    Arr[ParCount - I] := Sender.GetParam(I);
  Sender.Result := Arr;
end;

procedure _CreateOleObject(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := CreateOleObject(VarToStr(Sender.GetParam(1)));
end;

procedure _GetActiveOleObject(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := GetActiveOleObject(VarToStr(Sender.GetParam(1)));
end;

procedure _CreateComObject(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := CreateComObject(StringToGUID(VarToStr(Sender.GetParam(1))));
end;

{ Imported from SysUtils }

procedure _UpperCase(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do 
    Result := UpperCase(VarToStr(GetParam(1)));
end;

procedure _LowerCase(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := LowerCase(VarToStr(GetParam(1)));
end;

procedure _CompareStr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := CompareStr(VarToStr(GetParam(2)), VarToStr(GetParam(1)));
end;

procedure _CompareText(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := CompareText(VarToStr(GetParam(2)), VarToStr(GetParam(1)));
end;

procedure _SameText(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := SameText(VarToStr(GetParam(2)), VarToStr(GetParam(1)));
end;

procedure _AnsiUpperCase(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := AnsiUpperCase(VarToStr(GetParam(1)));
end;

procedure _AnsiLowerCase(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := AnsiLowerCase(VarToStr(GetParam(1)));
end;

procedure _AnsiCompareStr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := AnsiCompareStr(VarToStr(GetParam(2)), VarToStr(GetParam(1)));
end;

procedure _AnsiSameStr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := AnsiSameStr(VarToStr(GetParam(2)), VarToStr(GetParam(1)));
end;

procedure _AnsiCompareText(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := AnsiCompareText(VarToStr(GetParam(2)), VarToStr(GetParam(1)));
end;

procedure _AnsiSameText(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := AnsiSameText(VarToStr(GetParam(2)), VarToStr(GetParam(1)));
end;

procedure _Trim(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := Trim(VarToStr(GetParam(1)));
end;

procedure _TrimLeft(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := TrimLeft(VarToStr(GetParam(1)));
end;

procedure _TrimRight(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := TrimRight(VarToStr(GetParam(1)));
end;

procedure _QuotedStr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := QuotedStr(VarToStr(GetParam(1)));
end;

procedure _AnsiQuotedStr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := AnsiQuotedStr(VarToStr(GetParam(2)), VarToStr(GetParam(1))[1]);
end;

procedure _AdjustLineBreaks(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := AdjustLineBreaks(VarToStr(GetParam(1)));
end;

procedure _IsValidIdent(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := IsValidIdent(VarToStr(GetParam(1)));
end;

procedure _IntToStr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := IntToStr(GetParam(1));
end;

procedure _IntToHex(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := IntToHex(GetParam(2), GetParam(1));
end;

procedure _StrToInt(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := StrToInt(VarToStr(GetParam(1)));
end;

procedure _StrToIntDef(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := StrToIntDef(VarToStr(GetParam(2)), GetParam(1));
end;

procedure _FileAge(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := VarFromDateTime(FileDateToDateTime(FileAge(GetParam(1))));
end;

procedure _FileExists(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := FileExists(VarToStr(GetParam(1)));
end;

procedure _FileGetAttr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := FileGetAttr(VarToStr(GetParam(1)));
end;

procedure _FileSetAttr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := FileSetAttr(GetParam(2), GetParam(1));
end;

procedure _DeleteFile(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := DeleteFile(GetParam(1));
end;

procedure _RenameFile(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := RenameFile(GetParam(2), GetParam(1));
end;

type
  TSearchRecObj = class (TPersistent)
  protected
    function GetTime: TDateTime;
  public
    SearchRec: TSearchRec;
    destructor Destroy; override;
  published
    property Name: TFileName read SearchRec.Name;
    {$IFDEF D2006_ORLATER}
    property Size: Int64 read SearchRec.Size;
    {$ELSE}
    property Size: Integer read SearchRec.Size;
    {$ENDIF}
    property Time: TDateTime read GetTime;
    property Attr: Integer read SearchRec.Attr;
  end;

destructor TSearchRecObj.Destroy;
begin
  FindClose(SearchRec);
  inherited;
end;

function TSearchRecObj.GetTime: TDateTime;
begin
  Result := FileDateToDateTime(SearchRec.Time);
end;

procedure _FindFirst(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  SR: TSearchRecObj;
  Found: Integer;
begin
  with Sender do
  begin
    SR := TSearchRecObj.Create;
    Found := FindFirst(GetParam(3), GetParam(2), SR.SearchRec);
    if Found = 0 then
      SetVarByRef(GetParam(1), VarFromObject(SR))
    else begin
      SetVarByRef(GetParam(1), VarFromObject(nil));
      SR.Free;
    end;
    Result := Found;
  end;
end;

procedure _FindNext(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  SR: TSearchRecObj;
begin
  with Sender do
  begin
    SR := TSearchRecObj(VarToObject(GetVarByRef(GetParam(1))));
    Result := FindNext(SR.SearchRec);
  end;
end;

procedure _FindClose(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  SR: TSearchRecObj;
begin
  with Sender do
  begin
    SR := TSearchRecObj(VarToObject(GetVarByRef(GetParam(1))));
    if SR <> nil then
    begin
      SR.Free;
      SetVarByRef(GetParam(1), VarFromObject(nil));
    end;
  end;
end;

procedure _ChangeFileExt(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := ChangeFileExt(GetParam(2), GetParam(1));
end;

procedure _ExtractFilePath(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := ExtractFilePath(GetParam(1));
end;

procedure _ExtractFileDir(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := ExtractFileDir(GetParam(1));
end;

procedure _ExtractFileDrive(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := ExtractFileDrive(GetParam(1));
end;

procedure _ExtractFileName(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := ExtractFileName(GetParam(1));
end;

procedure _ExtractFileExt(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := ExtractFileExt(GetParam(1));
end;

procedure _ExpandFileName(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := ExpandFileName(GetParam(1));
end;

procedure _ExpandUNCFileName(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := ExpandUNCFileName(GetParam(1));
end;

procedure _ExtractRelativePath(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := ExtractRelativePath(GetParam(2), GetParam(1));
end;

procedure _ExtractShortPathName(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := ExtractShortPathName(GetParam(1));
end;

procedure _FileSearch(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := FileSearch(GetParam(2), GetParam(1));
end;

procedure _GetCurrentDir(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := GetCurrentDir;
end;

procedure _SetCurrentDir(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := SetCurrentDir(GetParam(1));
end;

procedure _CreateDir(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := CreateDir(GetParam(1));
end;

procedure _RemoveDir(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := RemoveDir(GetParam(1));
end;

function FormatVar(const Fmt: String; Value: Variant): String;
begin
  case VarType(Value) of
    varBoolean: Result := FormatFloat(Fmt, Integer(Value));
    {$IFnDEF VER130}
    varShortInt, varWord,
    varLongWord, varInt64,
    {$ENDIF}
    varSmallint, varInteger, varByte,
    varSingle, varDouble,
    varCurrency: Result := FormatFloat(Fmt, Value);
    varDate: Result := FormatDateTime(Fmt, VarToDateTime(Value));
    else Result := Format(Fmt, [VarToStr(Value)]);
  end;
end;

procedure _FormatVar(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  CheckParams(ParCount, 1, 2);
  with Sender do
    if ParCount = 1 then
      Result := VarToStr(GetParam(1))
    else Result := FormatVar(GetParam(1), GetParam(2));
end;

procedure _FloatToStr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := FloatToStr(GetParam(1));
end;

procedure _CurrToStr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := CurrToStr(GetParam(1));
end;

procedure _FormatFloat(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := FormatFloat(GetParam(2), GetParam(1));
end;

procedure _FormatCurr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := FormatCurr(GetParam(2), GetParam(1));
end;

procedure _StrToFloat(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := StrToFloat(GetParam(1));
end;

procedure _StrToCurr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := StrToCurr(GetParam(1));
end;

procedure _EncodeDate(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := VarFromDateTime(EncodeDate(GetParam(3), GetParam(2), GetParam(1)));
end;

procedure _EncodeTime(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := VarFromDateTime(EncodeTime(GetParam(4), GetParam(3), GetParam(2), GetParam(1)));
end;

procedure _DayOfWeek(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := DayOfWeek(VarToDateTime(GetParam(1)));
end;

procedure _Date(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := VarFromDateTime(Date);
end;

procedure _Time(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := VarFromDateTime(Time);
end;

procedure _Now(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := VarFromDateTime(Now);
end;

procedure _IncMonth(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := VarFromDateTime(IncMonth(VarToDateTime(GetParam(2)), GetParam(1)));
end;

procedure _IsLeapYear(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := IsLeapYear(GetParam(1));
end;

procedure _DateToStr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := DateToStr(VarToDateTime(GetParam(1)));
end;

procedure _TimeToStr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := TimeToStr(VarToDateTime(GetParam(1)));
end;

procedure _DateTimeToStr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := DateTimeToStr(VarToDateTime(GetParam(1)));
end;

procedure _StrToDate(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := StrToDate(GetParam(1));
end;

procedure _StrToTime(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := StrToTime(GetParam(1));
end;

procedure _StrToDateTime(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := StrToDateTime(GetParam(1));
end;

procedure _FormatDateTime(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := FormatDateTime(GetParam(2), VarToDateTime(GetParam(1)));
end;

procedure _IsPathDelimiter(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := IsPathDelimiter(GetParam(2), GetParam(1));
end;

procedure _IsDelimiter(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := IsDelimiter(GetParam(3), GetParam(2), GetParam(1));
end;

procedure _IncludeTrailingBackslash(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := IncludeTrailingPathDelimiter(GetParam(1));
end;

procedure _ExcludeTrailingBackslash(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := ExcludeTrailingPathDelimiter(GetParam(1));
end;

procedure _LastDelimiter(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := LastDelimiter(GetParam(2), GetParam(1));
end;

procedure _AnsiCompareFileName(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := AnsiCompareFileName(GetParam(2), GetParam(1));
end;

procedure _AnsiLowerCaseFileName(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := AnsiLowerCaseFileName(GetParam(1));
end;

procedure _AnsiUpperCaseFileName(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := AnsiUpperCaseFileName(GetParam(1));
end;

procedure _AnsiPos(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := AnsiPos(GetParam(2), GetParam(1));
end;

procedure _StringReplace(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := StringReplace(GetParam(3), GetParam(2), GetParam(1),
      [rfReplaceAll, rfIgnoreCase]);
end;

procedure _WrapText(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := WrapText(VarToStr(GetParam(2)), GetParam(1));
end;

procedure _FillStr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  Str: String;
  Ch: String;
  I: Integer;
begin
  with Sender do
  begin
    I := GetParam(2);
    SetLength(Str, I);
    Ch := VarToStr(GetParam(1));
    if Ch = '' then Ch := ' ';
    for I := 1 to Length(Str) do
      Str[I] := Ch[1];
    Result := Str;
  end;
end;

procedure _RepeatStr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  Str: String;
  Ch: String;
  Cnt, I: Integer;
begin
  with Sender do
  begin
    Cnt := GetParam(2);
    Ch := VarToStr(GetParam(1));
    Str := '';
    for I := 1 to Cnt do
      Str := Str + Ch;
    Result := Str;
  end;
end;


procedure _ParamStr(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := ParamStr(GetParam(1));
end;

procedure _ParamCount(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := ParamCount;
end;

procedure _Length(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := Length(VarToStr(Pop));
end;

procedure _SetLength(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  Str: String;
begin
  with Sender do
  begin
    Result := ParamCount;
    Str := VarToStr(GetVarByRef(GetParam(2)));
    SetLength(Str, Integer(GetParam(1)));
    SetVarByRef(GetParam(2), Str);
  end;
end;

procedure _Delete(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  Str: String;
begin
  with Sender do
  begin
    Str := VarToStr(GetVarByRef(GetParam(3)));
    Delete(Str, Integer(GetParam(2)), Integer(GetParam(1)));
    Result := Str;
  end;
end;

procedure _Copy(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  Str: String;
begin
  with Sender do
  begin
    Str := VarToStr(GetVarByRef(GetParam(3)));
    Result := Copy(Str, Integer(GetParam(2)), Integer(GetParam(1)));
  end;
end;

procedure InitCtxSysUtils;
begin
  // System methods registration
  with TCtxCustomIntrospector.Create(TCtxSystemScope) do
  begin
    AddMethod('write', @_write);
    AddMethod('writeln', @_writeln);

    AddMethod('Min', @_Min);
    AddMethod('Max', @_Max);
    AddMethod('Trunc', @_Trunc, 1);
    AddMethod('Round', @_Round, 1);
    AddMethod('Coalesce', @_Coalesce);
    AddMethod('Nvl', @_Coalesce);

    AddMethod('Sin', @_Sin, 1);
    AddMethod('Cos', @_Cos, 1);

    AddMethod('iff', @_IFF, 3);

    AddMethod('VarToStr', @_VarToStr, 1);
    AddMethod('VarToStrEx', @_VarToStrEx, 1);
    AddMethod('FormatFloat', @_FormatFloat, 2);
    AddMethod('FormatDateTime', @_FormatDateTime, 2);

    AddMethod('CreateOleObject', @_CreateOleObject, 1);
    AddMethod('GetActiveOleObject', @_GetActiveOleObject, 1);
    AddMethod('CreateComObject', @_CreateComObject, 1);

    AddMethod('VarArrayCreate', @_VarArrayCreate, 1);
    AddMethod('VarArrayOf', @_VarArrayOf);
    AddMethod('VarArrayRedim', @_VarArrayRedim, 2);

    AddMethod('UpperCase', @_UpperCase, 1);
    AddMethod('LowerCase', @_LowerCase, 1);
    AddMethod('CompareStr', @_CompareStr, 2);
    AddMethod('CompareText', @_CompareText, 2);
    AddMethod('SameText', @_SameText, 2);
    AddMethod('AnsiUpperCase', @_AnsiUpperCase, 1);
    AddMethod('AnsiLowerCase', @_AnsiLowerCase, 1);
    AddMethod('AnsiCompareStr', @_AnsiCompareStr, 2);
    AddMethod('AnsiSameStr', @_AnsiSameStr, 2);
    AddMethod('AnsiCompareText', @_AnsiCompareText, 2);
    AddMethod('AnsiSameText', @_AnsiSameText, 2);
    AddMethod('Trim', @_Trim, 1);
    AddMethod('TrimLeft', @_TrimLeft, 1);
    AddMethod('TrimRight', @_TrimRight, 1);
    AddMethod('QuotedStr', @_QuotedStr, 1);
    AddMethod('AnsiQuotedStr', @_AnsiQuotedStr, 1);
    AddMethod('AdjustLineBreaks', @_AdjustLineBreaks, 1);
    AddMethod('IsValidIdent', @_IsValidIdent, 1);
    AddMethod('IntToStr', @_IntToStr, 1);
    AddMethod('IntToHex', @_IntToHex, 2);
    AddMethod('StrToInt', @_StrToInt, 1);
    AddMethod('StrToIntDef', @_StrToIntDef, 2);
    AddMethod('FileAge', @_FileAge, 1);
    AddMethod('FileExists', @_FileExists, 1);
    AddMethod('FileGetAttr', @_FileGetAttr, 1);
    AddMethod('FileSetAttr', @_FileSetAttr, 1);
    AddMethod('DeleteFile', @_DeleteFile, 1);
    AddMethod('RenameFile', @_RenameFile, 2);

    AddMethod('FindFirst', @_FindFirst, 3);
    AddMethod('FindNext', @_FindNext, 1);
    AddMethod('FindClose', @_FindClose, 1);

    AddMethod('ChangeFileExt', @_ChangeFileExt, 2);
    AddMethod('ExtractFilePath', @_ExtractFilePath, 1);
    AddMethod('ExtractFileDir', @_ExtractFileDir, 1);
    AddMethod('ExtractFileDrive', @_ExtractFileDrive, 1);
    AddMethod('ExtractFileName', @_ExtractFileName, 1);
    AddMethod('ExtractFileExt', @_ExtractFileExt, 1);
    AddMethod('ExpandFileName', @_ExpandFileName, 1);
    AddMethod('ExpandUNCFileName', @_ExpandUNCFileName, 1);
    AddMethod('ExtractRelativePath', @_ExtractRelativePath, 2);
    AddMethod('ExtractShortPathName', @_ExtractShortPathName, 1);
    AddMethod('FileSearch', @_FileSearch, 2);
    AddMethod('GetCurrentDir', @_GetCurrentDir, 0);
    AddMethod('SetCurrentDir', @_SetCurrentDir, 1);
    AddMethod('CreateDir', @_CreateDir, 1);
    AddMethod('RemoveDir', @_RemoveDir, 1);
    AddMethod('FormatVar', @_FormatVar);
    AddMethod('FloatToStr', @_FloatToStr);
    AddMethod('CurrToStr', @_CurrToStr);
    AddMethod('FormatFloat', @_FormatFloat, 2);
    AddMethod('FormatCurr', @_FormatCurr, 2);
    AddMethod('StrToFloat', @_StrToFloat, 1);
    AddMethod('StrToCurr', @_StrToCurr, 1);
    AddMethod('EncodeDate', @_EncodeDate, 3);
    AddMethod('EncodeTime', @_EncodeTime, 4);
    AddMethod('DayOfWeek', @_DayOfWeek, 1);
    AddMethod('Date', @_Date, 0);
    AddMethod('Time', @_Time, 0);
    AddMethod('Now', @_Now, 0);

    AddMethod('IncMonth', @_IncMonth, 2);
    AddMethod('IsLeapYear', @_IsLeapYear, 1);
    AddMethod('DateToStr', @_DateToStr, 1);
    AddMethod('TimeToStr', @_TimeToStr, 1);
    AddMethod('DateTimeToStr', @_DateTimeToStr, 1);
    AddMethod('StrToDate', @_StrToDate, 1);
    AddMethod('StrToTime', @_StrToTime, 1);
    AddMethod('StrToDateTime', @_StrToDateTime, 1);
    AddMethod('FormatDateTime', @_FormatDateTime, 2);
    AddMethod('IsPathDelimiter', @_IsPathDelimiter, 2);
    AddMethod('IsDelimiter', @_IsDelimiter, 3);
    AddMethod('IncludeTrailingBackslash', @_IncludeTrailingBackslash, 1);
    AddMethod('ExcludeTrailingBackslash', @_ExcludeTrailingBackslash, 1);
    AddMethod('LastDelimiter', @_LastDelimiter, 2);
    AddMethod('AnsiCompareFileName', @_AnsiCompareFileName, 2);
    AddMethod('AnsiLowerCaseFileName', @_AnsiLowerCaseFileName, 1);
    AddMethod('AnsiUpperCaseFileName', @_AnsiUpperCaseFileName, 1);
    AddMethod('AnsiPos', @_AnsiPos, 2);
    AddMethod('StringReplace', @_StringReplace, 3);
    AddMethod('WrapText', @_WrapText, 2);
    AddMethod('FillStr', @_FillStr, 2);
    AddMethod('RepeatStr', @_RepeatStr, 2);

    AddMethod('ParamStr', @_ParamStr, 1);
    AddMethod('ParamCount', @_ParamCount, 0);

    AddMethod('SetLength', @_SetLength, 2);
    AddMethod('Length', @_Length, 1);
    AddMethod('Delete', @_Delete, 3);
    AddMethod('Copy', @_Copy, 3);

    AddConst('faAnyFile', faAnyFile);
    AddConst('faDirectory', faDirectory);
    AddConst('faReadOnly', faReadOnly);
    AddConst('faHidden', faHidden);
    AddConst('faSysFile', faSysFile);
    AddConst('faArchive', faArchive);

    AddConst('fmCreate', fmCreate);
    AddConst('fmOpenRead', fmOpenRead);
    AddConst('fmOpenWrite', fmOpenWrite);
    AddConst('fmOpenReadWrite', fmOpenReadWrite);
  end;
end;

initialization
  InitCtxSysUtils;
end.
