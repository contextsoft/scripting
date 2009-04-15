(******************************************************************************)
(*
(*  Context Scripting Suite
(*
(*  Classes implementing generic text reporting
(*
(*  Contains:
(*                TCtxTextReporter = class (TComponent)
(*
(*  Copyright (c) 2005 Michael Baytalsky
(*
(*  ------------------------------------------------------------
(*  FILE        : CtxTextReport.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 1.3
(*  DELPHI      : Delphi 5,6,7,2005
(*  ------------------------------------------------------------
(*  HISTORY     :
(*    6/23/2005    v1.3    	Released
(*
(*    No changes to this file since v1.3.
(*
(******************************************************************************)
unit CtxTextReport;

interface

uses Classes, SysUtils, Contnrs, {$IFnDEF VER130} Variants, {$ENDIF}
  CtxScript, CtxPkgSysUtils;

type
  TCtxTextReportSection = class;

  TOnParseField = function (const FieldText: String; IsField: Boolean): String of object;
  TOnPrintSection = procedure (Sender: TObject; Section: TCtxTextReportSection) of object;

  TAggregate = record
    Sum: Double;
    Count: Integer;
  end;

  TSectionPrint = (spReport, spDetail, spBefore, spAfter, spBeforeGroup, spAfterGroup);

  {:: TCtxTextReporter implements generic text/html reporter. Call method }
  {:: ProduceReport to produce text report based on template specified in }
  {:: Template property. TCtxTextReporter supports aggregates (sum, count, }
  {:: avg, runsum and runcount. The report is generated based on object passed }
  {:: as Scope parameter. }
  TCtxTextReporter = class (TComponent)
  private
  protected
    FReportScope: TObject;
    FScripting: TCtxScript;
    FSections: TObjectList; // of TCtxTextReportSection
    FTemplate: TStrings;
    FReport: TStrings;
    FReportStr: String;
    FNextTokenType: Integer;
    FNextToken: String;
    FLineNo: Integer;
    FLastPrintedSection: TCtxTextReportSection;
    FVariables: String;
    FPrepared: Boolean;
    FAggregates: TObjectList; // of TCtxScriptCode
    FReportCode: TCtxScriptCode;
    FBeforeSection: TOnPrintSection;
    FAfterSection: TOnPrintSection;
    FCurrentSection: TCtxTextReportSection;
    FFlushToFile: String;
    FDestDir: String;
    FOpenBracket: String;
    FCloseBracket: String;
    FParams: TStrings;
    FAggrValues: array of TAggregate;

    procedure SetParams(const Value: TStrings);
    procedure ParseError(const Msg, Token: String);
    function GetSectionCount: Integer;
    function GetSections(I: Integer): TCtxTextReportSection;
    function DoPrintReport: Boolean;
    procedure PrintSection(Section, ParentSection: TCtxTextReportSection);
    procedure PrintSections(Section: TCtxTextReportSection; SectionPrint: TSectionPrint);
    procedure PrintThisSection(Section: TCtxTextReportSection; FirstSection: Boolean); virtual;

    procedure TemplateChanged(Sender: TObject);
    function GetScripting: TCtxScript;
    procedure SetPrepared(const Value: Boolean);
    procedure SetTemplate(const Value: TStrings);

    procedure NextToken(const Str: String; var StartPos: Integer);
    procedure Match(const Line: String; TokenType: Integer; var StartPos: Integer;
      const Token: String = '');

    procedure UpdateAggregates(Section: TCtxTextReportSection);
    property ReportCode: TCtxScriptCode read FReportCode;
  public
    {:: Creates an instance of TCtxTextReporter component. }
    constructor Create(AOwner: TComponent); override;
    {:: Destroys the instance of TCtxTextReporter component. }
    destructor Destroy; override;

    procedure PrepareReport;
    procedure ProduceReport(Report: TStrings; Scope: TObject = nil); overload;
    function ProduceReport(Scope: TObject = nil): String; overload;

    function FindSection(const Name: String): TCtxTextReportSection;
    function GetSection(const Name: String): TCtxTextReportSection;

    property ReportScope: TObject read FReportScope write FReportScope;
    property Sections[I: Integer]: TCtxTextReportSection read GetSections;
    property SectionCount: Integer read GetSectionCount;
    property Scripting: TCtxScript read GetScripting;
    property CurrentSection: TCtxTextReportSection read FCurrentSection;
    property ReportStr: String read FReportStr write FReportStr;
  published
    property PrintReport: Boolean read DoPrintReport;
    property Params: TStrings read FParams write SetParams;
    property DestDir: String read FDestDir write FDestDir;
    property Template: TStrings read FTemplate write SetTemplate;
    property Prepared: Boolean read FPrepared write SetPrepared;
    property BeforeSection: TOnPrintSection read FBeforeSection write FBeforeSection;
    property AfterSection: TOnPrintSection read FAfterSection write FAfterSection;
    property OpenBracket: String read FOpenBracket write FOpenBracket;
    property CloseBracket: String read FCloseBracket write FCloseBracket;
  end;

  TCtxTextReportSection = class (TPersistent)
  protected
    FReporter: TCtxTextReporter;
    FName: String;
    FSectionPrint: TSectionPrint;
    FParentName: String;
    FBody: String;
    FDelimiter: String;
    FNewLine: Boolean;
    FDataSet: String;
    FGroupBy: String;
    FSectionCode: TCtxScriptCode;
    FAggregateIDs: array of Integer;
    FAggrValues: array of TAggregate;

    FSubSections: TList; // of TCtxTextReportSection;
    FLastGroupValue: Variant;
    FDataSetObject: TObject;
    FCurrentObject: TObject;
    FIterator: TCtxObjectIterator;

    // FCompiledBody: String;
    // FFields: TObjectList;
    FStack: TStack;

    // function DoEvaluateField(const FieldText: String): String;
    function DoCompileField(const FieldText: String; IsField: Boolean): String;
    procedure PushState;
    procedure PopState;
    procedure AddSubSection(SubSection: TCtxTextReportSection);
    procedure AddAggregate(Idx: Integer);
  public
    destructor Destroy; override;

    property DataSetObject: TObject read FDataSetObject;
    property CurrentObject: TObject read FCurrentObject;
    property Iterator: TCtxObjectIterator read FIterator;
  published
    property Name: String read FName;
    property ParentName: String read FParentName;
    property DataSet: String read FDataSet;
    property GroupBy: String read FGroupBy;
    property Body: String read FBody;
    property Delimiter: String read FDelimiter write FDelimiter;
    property NewLine: Boolean read FNewLine write FNewLine;
    property SectionPrint: TSectionPrint read FSectionPrint;
  end;

  procedure DumpSections(Reporter: TCtxTextReporter; Output: TStrings);

resourcestring
  SUnexpectedCommand = 'Unexpected token. Line %d near %s';
  SBeginSectionMissing = '#begin <section> instruction missing. Line: %d, near %s';
  SCannotBeEmpty = 'cannot be empty. Line %d near %s';
  SExpression = 'Expression';
  SSectionNotFound = 'Section not found: %s';
  SExpectedToken = 'expected. Line %d near %s';

implementation

uses StrUtils;

const
  { Parser Token Types }
  tokenEOF = -1;
  tokenString = -2;
  tokenComma = -3;
  tokenIdentifier = -4;
  tokenRem = 0;
  tokenSection = 1;
  tokenBegin = 2;
  tokenVar = 3;
  tokenAggregate = 4;
  tokenEnd = 5;

const
  STokens: array [0..5] of String = ('rem', 'section', 'begin', 'var', 'aggregate', 'end');

{ Helper Routines }

function ParseFields(const InpStr, Open, Close: String; OnParseField: TOnParseField): String;
var
  OpenPos, ClosePos: Integer;
  Res: String;

  function PosFrom(const SubStr, Str: String; FromPos: Integer): Integer;
  var P: PChar;
  begin
    P := StrPos(@PChar(Str)[FromPos - 1], PChar(SubStr));
    if P <> nil then
      Result := Longint(P) - Longint(PChar(Str)) + 1
    else Result := 0;
  end;

begin
  Result := '';
  Res := InpStr;
  ClosePos := 1;
  repeat
    // Look for next open bracket from last Close pos
    OpenPos := PosFrom(Open, Res, ClosePos);
    if OpenPos = 0 then break;
    Result := Result + OnParseField(copy(Res, ClosePos, OpenPos - ClosePos), False);
    Inc(OpenPos, Length(Open)); // Skip open bracket
    // Open bracket found. Look for Close
    ClosePos := PosFrom(Close, Res, OpenPos);
    if ClosePos = 0 then break;
    // Close Bracket Found. Extract/Replace field
    Result := Result + OnParseField(copy(Res, OpenPos, ClosePos - OpenPos), True);
    Inc(ClosePos, Length(Close));
  until false;
  Result := Result + OnParseField(copy(Res, ClosePos, Length(Res)), False);
end;

procedure DumpSections(Reporter: TCtxTextReporter; Output: TStrings);
const
  sSectionPrint: array [TSectionPrint] of String = ('Report', 'In', 'Before', 'After', 'Before Group', 'After Group');
var
  I: Integer;
begin
  // if we meet end of Group condition print group footer
  Output.Add('--------------------------------------');
  for I := 0 to Reporter.SectionCount - 1 do
  with Reporter.Sections[I] do
  begin
    Output.Add('Section: ' + Name);
    Output.Add('Parent: ' + ParentName);
    Output.Add('Type: ' + sSectionPrint[SectionPrint]);
    Output.Add('DataSet: ' + DataSet);
    Output.Add('Group By: ' + GroupBy);
    Output.Add('Body: ' + Body);
    Output.Add('--------------------------------------');
  end;
end;

procedure CopyValues(var Src, Dest: array of TAggregate);
var
  I: Integer;
begin
  if Length(Src) > 0 then
  for I := Low(Src) to High(Src) do
    Dest[I] := Src[I];
end;

{ TCtxTextReportSection }

destructor TCtxTextReportSection.Destroy;
begin
  FStack.Free;
  // FFields.Free;
  FSubSections.Free;
  FSectionCode.Free;
  inherited Destroy;
end;

function TCtxTextReportSection.DoCompileField(
  const FieldText: String; IsField: Boolean): String;
begin
  Result := '';
  if FieldText = '' then exit;
  if IsField then
  begin
    if FieldText[1] = '=' then
      Result := '''+FormatVar(' + copy(FieldText, 2, Length(FieldText)) + ')+'''
    else Result := ''';'#13#10 + FieldText + #13#10'Result := Result+''';
  end else
    Result := StringReplace(FieldText, #13#10, '''+#13#10+''', [rfReplaceAll, rfIgnoreCase]);
end;

procedure TCtxTextReportSection.AddSubSection(
  SubSection: TCtxTextReportSection);
begin
  if FSubSections = nil then
    FSubSections := TList.Create; // of TCtxTextReportSection;
  FSubSections.Add(SubSection);
end;

procedure TCtxTextReportSection.PushState;
begin
  // Push current state: Iterator, CurrentObject, DataSetObject
  if (CurrentObject <> nil) or (Iterator <> nil) then
  begin
    if FStack = nil then
      FStack := TStack.Create;
    if FIterator <> nil then
      FIterator.PushState;
    FStack.Push(FCurrentObject);
    FStack.Push(FIterator);
    FStack.Push(FDataSetObject);
  end;
end;

procedure TCtxTextReportSection.PopState;
begin
  // Pop current state: Iterator, CurrentObject, DataSetObject
  if (FStack <> nil) and FStack.AtLeast(3) then
  begin
    FDataSetObject := FStack.Pop;
    FIterator := FStack.Pop;
    FCurrentObject := FStack.Pop;
    if FIterator <> nil then
      FIterator.PopState;
  end;
end;

procedure TCtxTextReportSection.AddAggregate(Idx: Integer);
begin
  SetLength(FAggregateIDs, Length(FAggregateIDs) + 1);
  FAggregateIDs[High(FAggregateIDs)] := Idx;
end;

{ TCtxTextReporter }

constructor TCtxTextReporter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TStringList.Create;
  FSections := TObjectList.Create(True);
  FReportScope := nil;
  FTemplate := TStringList.Create;
  TStringList(FTemplate).OnChange := TemplateChanged;
  FPrepared := False;
  FAggregates := TObjectList.Create(True);
  FOpenBracket := '<%';
  FCloseBracket := '%>';
end;

destructor TCtxTextReporter.Destroy;
begin
  FAggregates.Free;
  FSections.Free;
  FTemplate.Free;
  FScripting.Free;
  FParams.Free;
  inherited Destroy;
end;

function TCtxTextReporter.FindSection(
  const Name: String): TCtxTextReportSection;
var
  I: Integer;
begin
  for I := 0 to FSections.Count - 1 do
  begin
    Result := FSections[I] as TCtxTextReportSection;
    if AnsiSameText(Result.Name, Name) then exit;
  end;
  Result := nil;
end;

function TCtxTextReporter.GetSection(
  const Name: String): TCtxTextReportSection;
begin
  Result := FindSection(Name);
  if Result = nil then
    raise Exception.CreateFmt(SSectionNotFound, [Name]);
end;

function TCtxTextReporter.GetSectionCount: Integer;
begin
  Result := FSections.Count;
end;

function TCtxTextReporter.GetSections(I: Integer): TCtxTextReportSection;
begin
  Result := TCtxTextReportSection(FSections[I]);
end;

procedure TCtxTextReporter.ParseError(const Msg, Token: String);
begin
  raise Exception.CreateFmt(Msg, [FLineNo, Token]);
end;

procedure TCtxTextReporter.Match(const Line: String; TokenType: Integer;
  var StartPos: Integer; const Token: String = '');
begin
  NextToken(Line, StartPos);
  if (FNextTokenType <> tokenIdentifier) and (FNextTokenType <> TokenType) then
    ParseError(SUnexpectedCommand, FNextToken);
  if (Token <> '') and not AnsiSameText(Token, FNextToken) then
    ParseError('''' + Token + ''' ' + SExpectedToken, FNextToken);
end;

procedure TCtxTextReporter.NextToken(const Str: String; var StartPos: Integer);
const
  AphaNum: set of char = ['a'..'z', 'A'..'Z', '_', '-', '0'..'9'];
var
  I: Integer;
begin
  FNextTokenType := tokenEOF;
  FNextToken := '';
  while (StartPos <= Length(Str)) and (Str[StartPos] = ' ') do Inc(StartPos);
  if StartPos <= Length(Str) then
    case Str[StartPos] of
      ',': begin
        FNextTokenType := tokenComma;
        FNextToken := ',';
        Inc(StartPos);
      end;
      'a'..'z', 'A'..'Z', '_': begin
        FNextTokenType := tokenIdentifier;
        while (StartPos <= Length(Str)) and (Str[StartPos] in AphaNum) do
        begin
          FNextToken := FNextToken + Str[StartPos];
          Inc(StartPos);
        end;
        for I := Low(STokens) to High(STokens) do
          if AnsiSameText(STokens[I], FNextToken) then
          begin
            FNextTokenType := I;
            break;
          end;
      end;
      '''': begin
        FNextTokenType := tokenString;
        Inc(StartPos);
        while StartPos <= Length(Str) do
        begin
          if Str[StartPos] = '''' then
          begin
            Inc(StartPos);
            if (StartPos > Length(Str)) or (Str[StartPos] <> '''') then
              break;
          end;
          FNextToken := FNextToken + Str[StartPos];
          Inc(StartPos);
        end;
      end;
    end;
end;

procedure TCtxTextReporter.PrintSections(Section: TCtxTextReportSection; SectionPrint: TSectionPrint);
var
  I: Integer;
begin
  if Section.FSubSections <> nil then
  for I := 0 to Section.FSubSections.Count - 1 do
    if (TCtxTextReportSection(Section.FSubSections[I]).SectionPrint = SectionPrint) then
      PrintSection(TCtxTextReportSection(Section.FSubSections[I]), Section);
end;

procedure TCtxTextReporter.PrintThisSection(Section: TCtxTextReportSection; FirstSection: Boolean);
var
  F: TFileStream;
begin
  FCurrentSection := Section;

  if Assigned(FBeforeSection) then FBeforeSection(Self, Section);
  // Print delimiter if necessary
  if Section.NewLine and (FReportStr <> '') then
  begin
    if FReportStr[Length(FReportStr)] <> #10 then
      FReportStr := FReportStr + #13#10;
  end else if not FirstSection and (FLastPrintedSection = Section) then
    FReportStr := FReportStr + Section.Delimiter;

  // FReportStr := FReportStr + ParseFields(Section.FCompiledBody, '<%', '%>', Section.DoEvaluateField);
  if Section.FSectionCode <> nil then
    FReportStr := FReportStr + Scripting.Execute(Section.FSectionCode, ReportCode)
  else FReportStr := FReportStr + Section.Body;

  FLastPrintedSection := Section;
  if Assigned(FAfterSection) then FAfterSection(Self, Section);

  // Flush section to file if flush option set
  if FFlushToFile <> '' then
  begin
    if FDestDir <> '' then
      FFlushToFile := IncludeTrailingPathDelimiter(FDestDir) + FFlushToFile;

    F := TFileStream.Create(FFlushToFile, fmCreate);
    try
      F.Write(FReportStr[1], Length(FReportStr));
    finally
      F.Free;
    end;
    FReportStr := '';
    FFlushToFile := '';
  end;

  UpdateAggregates(Section);
  // After printing remember last values
  CopyValues(FAggrValues, Section.FAggrValues);
end;

procedure TCtxTextReporter.PrintSection(Section, ParentSection: TCtxTextReportSection);

var
  GroupChanged: Boolean;
  FirstSection: Boolean;
  NewGroupValue: Variant;

  procedure ResetGroups;
  var
    I: Integer;
  begin
    if Section.FSubSections <> nil then
    for I := 0 to Section.FSubSections.Count - 1 do
    with TCtxTextReportSection(Section.FSubSections[I]) do
      begin
        if SectionPrint = spBeforeGroup then
          FLastGroupValue := Unassigned
        else if SectionPrint = spAfterGroup then
          FLastGroupValue := Scripting.Evaluate(GroupBy, Section.CurrentObject);
      end;
  end;

begin
  FirstSection := True;
  if Section.SectionPrint = spReport then
  begin
    // Print report headers
    PrintSections(Section, spBefore);
    // Print this section
    PrintThisSection(Section, FirstSection);
    // Print report sections
    PrintSections(Section, spDetail);
    // Print report footers
    PrintSections(Section, spAfter);
  end else if Section.SectionPrint = spDetail then
  begin
    // Push Section State
    Section.PushState;
    Section.FDataSetObject := VarToObject(Scripting.Evaluate(Section.DataSet, FReportCode));
    Section.FIterator := VarToObject(Scripting.Evaluate('GetIterator', Section.DataSetObject)) as TCtxObjectIterator;
    try
      Section.FCurrentObject := Section.Iterator.First;
      if Section.FCurrentObject <> nil then
      begin
        // Print headers of this section
        PrintSections(Section, spBefore);
        // Reset groups
        ResetGroups;
        while Section.CurrentObject <> nil do
        begin
          // Print group header of this section
          PrintSections(Section, spBeforeGroup);
          // Print this section
          PrintThisSection(Section, FirstSection);
          FirstSection := False;
          // Print child sections
          PrintSections(Section, spDetail);
          // Move next row
          Section.FCurrentObject := Section.Iterator.Next;
          // Print group footer for this section
          PrintSections(Section, spAfterGroup);
        end;
        // Print footers of this section
        PrintSections(Section, spAfter);
      end;
    finally
      FreeAndNil(Section.FIterator);
      Section.FCurrentObject := nil;
      Section.FDataSetObject := nil;
      // Pop Section State
      Section.PopState;
    end;
  end else { Before, After & Groups }
  begin
    Section.FDataSetObject := nil;
    Section.FIterator := nil;
    Section.FCurrentObject := ParentSection.CurrentObject;
    GroupChanged := False;
    if Section.SectionPrint in [spBeforeGroup, spAfterGroup] then
    begin
      if Section.CurrentObject <> nil then
      begin
        NewGroupValue := Scripting.Evaluate(Section.GroupBy, Section.CurrentObject);
        GroupChanged := VarIsEmpty(Section.FLastGroupValue) or (Section.FLastGroupValue <> NewGroupValue);
      end else begin
        NewGroupValue := NULL;
        GroupChanged := True;
      end;
      if GroupChanged then
        Section.FLastGroupValue := NewGroupValue;
    end;
    if not (Section.SectionPrint in [spBeforeGroup, spAfterGroup]) or GroupChanged then
    begin
      // Temporarily step back
      if Section.SectionPrint = spAfterGroup then
      begin
        ParentSection.FCurrentObject := ParentSection.Iterator.Prior;
        Section.FCurrentObject := ParentSection.FCurrentObject;
      end;
      // Print headers of this section
      PrintSections(Section, spBefore);
      // Print this section
      PrintThisSection(Section, FirstSection);
      // Print child sections
      PrintSections(Section, spDetail);
      // Print footers of this section
      PrintSections(Section, spAfter);
      // Move forward, because we stepped back before
      if Section.SectionPrint = spAfterGroup then
        ParentSection.FCurrentObject := ParentSection.Iterator.Next;
    end;
  end;
end;

function TCtxTextReporter.DoPrintReport: Boolean;
begin
  PrintSection(Sections[0], nil);
  Result := True;
end;

function TCtxTextReporter.GetScripting: TCtxScript;
begin
  if FScripting = nil then
    FScripting := TCtxScript.Create(nil);
  Result := FScripting;
end;

procedure TCtxTextReporter.PrepareReport;
var
  LinePos, I: Integer;
  Section: TCtxTextReportSection;
  ReadingBody: Boolean;
  Line, Body: String;
  Code: TCtxScriptCode;

  function NonEmpty(const Part, Str: String): String;
  begin
    Result := Trim(Str);
    if Result = '' then
      ParseError(Part + ' ' + SCannotBeEmpty, FNextToken);
  end;

begin
  (*
    Template Syntaxis
    ~~~~~~~~~~~~~~~~~
    #section <section> [before|after|in] <parent_section> [on <expression>|group by <expression>]
    #var <comma_delimited_list_of_variables>
    #aggregate <name> for detail on <expression>
    #begin <section> [delimiter 'x' | newline]
    Detail text <%= expression %><% procedure %>
    #end
  *)
  FAggregates.Clear;
  FSections.Clear;
  FSections.Add(TCtxTextReportSection.Create);
  Sections[0].FReporter := Self;
  Sections[0].FName := 'report';
  Sections[0].FSectionPrint := spReport;
  ReadingBody := False;
  Body := '';
  FVariables := '';
  Section := nil;
  for I := 0 to FTemplate.Count - 1 do
  begin
    FLineNo := I;
    Line := Trim(FTemplate[I]);

    if Line = '# -->' then continue;

    if AnsiPos('<!-- #', Line) = 1 then
    begin
      Delete(Line, 1, 5);

      if (Length(Line) > 4) and (copy(Line, Length(Line) - 3, 4) = ' -->') then
        SetLength(Line, Length(Line) - 4);
    end;

    if ReadingBody and not AnsiSameText(Line, '#end') then
      Body := Body + Line + #13#10
    else if Line <> '' then
    begin
      LinePos := 1;
      if Line[1] = '#' then
        LinePos := 2;
      NextToken(Line, LinePos);
      case FNextTokenType of
        tokenRem: ;
        tokenSection: begin
          Section := TCtxTextReportSection.Create;
          try
            Section.FReporter := Self;
            Match(Line, tokenIdentifier, LinePos);
            Section.FName := FNextToken;
            Match(Line, tokenIdentifier, LinePos);
            if AnsiSameText(FNextToken, 'before') then
              Section.FSectionPrint := spBefore
            else if AnsiSameText(FNextToken, 'after') then
              Section.FSectionPrint := spAfter
            else if AnsiSameText(FNextToken, 'in') then
              Section.FSectionPrint := spDetail
            else ParseError(SUnexpectedCommand, FNextToken);

            repeat
              Match(Line, tokenIdentifier, LinePos);
              if Section.FParentName <> '' then
                Section.FParentName := Section.FParentName + ',';
              Section.FParentName := Section.FParentName + FNextToken;
              NextToken(Line, LinePos);
            until FNextTokenType <> tokenComma;

            if FNextTokenType = tokenIdentifier then
            begin
              if AnsiSameText(FNextToken, 'on') then
              begin
                Section.FDataSet := NonEmpty(SExpression, copy(Line, LinePos, Length(Line)));
              end else if AnsiSameText(FNextToken, 'group') then
              begin
                Match(Line, tokenIdentifier, LinePos, 'by');
                Section.FGroupBy := NonEmpty(SExpression, copy(Line, LinePos, Length(Line)));
                if Section.SectionPrint = spBefore then
                  Section.FSectionPrint := spBeforeGroup
                else if Section.SectionPrint = spAfter then
                  Section.FSectionPrint := spAfterGroup
                else ParseError(SUnexpectedCommand, FNextToken);
              end else
                ParseError(SUnexpectedCommand, FNextToken);
            end;
          except
            Section.Free;
            raise;
          end;
          FSections.Add(Section);
          Section := nil;
        end;
        tokenBegin: begin
          Match(Line, tokenIdentifier, LinePos);
          Section := GetSection(FNextToken);
          NextToken(Line, LinePos);
          if AnsiSameText(FNextToken, 'delimiter') then
          begin
            Match(Line, tokenString, LinePos);
            Section.Delimiter := FNextToken;
          end else if AnsiSameText(FNextToken, 'newline') then
            Section.NewLine := True;
          ReadingBody := True;
        end;
        tokenVar: begin
          repeat
            Match(Line, tokenIdentifier, LinePos);
            if FVariables <> '' then
              FVariables := FVariables + ', ';
            FVariables := FVariables + FNextToken;
            NextToken(Line, LinePos);
          until FNextTokenType <> tokenComma;
        end;
        tokenAggregate: begin
          // #aggregate <name> for detail on <expression>
          Code := TCtxScriptCode.Create;
          try
            Code.IsExpression := True;
            Match(Line, tokenIdentifier, LinePos);
            Code.ScriptName := FNextToken;
            Match(Line, tokenIdentifier, LinePos, 'for');
            Match(Line, tokenIdentifier, LinePos);
            Code.Description := FNextToken;
            Match(Line, tokenIdentifier, LinePos, 'on');
            Code.Code := NonEmpty(SExpression, copy(Line, LinePos, Length(Line)));
          except
            Code.Free;
            raise;
          end;
          GetSection(Code.Description).AddAggregate(FAggregates.Add(Code));
        end;
        tokenEnd: begin
          if Section = nil then
            ParseError(SBeginSectionMissing, FNextToken);
          if Body <> '' then
            Body := copy(Body, 1, Length(Body) - 2);
          Section.FBody := Body; // unescape
          Body := '';
          ReadingBody := False;
        end;
        else
          ParseError(SUnexpectedCommand, FNextToken);
      end;
    end;
  end;
  // Fixup references & compile expressions
  for I := 1 to SectionCount - 1 do
  with Sections[I] do
  begin
    LinePos := 1;
    repeat
      Match(ParentName, tokenIdentifier, LinePos);
      GetSection(FNextToken).AddSubSection(Sections[I]);
      NextToken(ParentName, LinePos);
    until FNextTokenType <> tokenComma;
    if AnsiPos(FOpenBracket, FBody) > 0 then
    begin
      FSectionCode := TCtxScriptCode.Create;
      FSectionCode.Code :=
        'function Section' + IntToStr(I) + ';'#13#10'begin Result := ''' +
        ParseFields(FBody, FOpenBracket, FCloseBracket, DoCompileField)
        + ''';'+#13#10'end;';
    end;
  end;
  SetLength(FAggrValues, FAggregates.Count);

  FPrepared := True;
end;

procedure TCtxTextReporter.ProduceReport(Report: TStrings; Scope: TObject);
begin
  Report.Text := ProduceReport(Scope);
end;

function TCtxTextReporter.ProduceReport(Scope: TObject): String;
var
  I: Integer;
begin
  Prepared := True;
  Scripting.GlobalScope := Self;
  if Scope <> nil then
    Sections[0].FCurrentObject := Scope
  else Sections[0].FCurrentObject := ReportScope;
  FReportStr := '';
  FLastPrintedSection := nil;

  // Reset Aggregates
  if Length(FAggrValues) > 0 then
  begin
    for I := Low(FAggrValues) to High(FAggrValues) do
    with FAggrValues[I] do
    begin
      Sum := 0;
      Count := 0;
    end;
    for I := 0 to SectionCount - 1 do
    begin
      SetLength(Sections[I].FAggrValues, Length(FAggrValues));
      CopyValues(FAggrValues, Sections[I].FAggrValues);
    end;
  end;

  FReportCode := TCtxScriptCode.Create;
  try
    with FReportCode do
    begin
      Code := 'procedure Report; '#13#10;
      if Trim(FVariables) <> '' then
        Code := Code + 'var ' + FVariables + ';'#13#10;

      if FAggregates.Count > 0 then
      begin
        Code := Code + 'const '#13#10;
        for I := 0 to FAggregates.Count - 1 do
          Code := Code + ' ' +
            TCtxScriptCode(FAggregates[I]).ScriptName + ' = ' + IntToStr(I) + ';'#13#10;
      end;

      Code := Code + 'begin'#13#10'PrintReport;'#13#10'end;';
    end;

    FScripting.Execute(FReportCode, Self);

    Result := FReportStr;
  finally
    FReportCode.Free;
  end;
end;

procedure TCtxTextReporter.SetPrepared(const Value: Boolean);
begin
  if FPrepared <> Value then
  begin
    if Value then PrepareReport;
    FPrepared := Value;
  end;
end;

procedure TCtxTextReporter.SetTemplate(const Value: TStrings);
begin
  FTemplate.Assign(Value);
end;

procedure TCtxTextReporter.TemplateChanged(Sender: TObject);
begin
  FPrepared := False;
end;

procedure TCtxTextReporter.UpdateAggregates(
  Section: TCtxTextReportSection);
var
  Code: TCtxScriptCode;
  Idx, I: Integer;
begin
  if Length(Section.FAggregateIDs) > 0 then
  for I := Low(Section.FAggregateIDs) to High(Section.FAggregateIDs) do
  begin
    Idx := Section.FAggregateIDs[I];
    Code := FAggregates[Idx] as TCtxScriptCode;
    with FAggrValues[Idx] do
    begin
      Sum := Sum + Scripting.Execute(Code, ReportCode);
      Count := Count + 1;
    end;
  end;
end;

procedure TCtxTextReporter.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;

type
  TCtxTextReporterIntrospector = class (TCtxIntrospector)
  public
    { Finds out wheather this introspector can be used to invoke the code }
    function ResolveName(CtxScript: TCtxScript; Scope: TObject;
      const Name: String; var Index: Integer): Boolean; override;
    { Invokes method or property by index }
    function Invoke(CtxScript: TCtxScript; InvokeType: TCtxInvokeType;
      Scope: TObject; const Name: String; var Index: Integer; ParCount: Integer): Boolean; override;
  end;

{ TCtxTextReporterIntrospector }

function TCtxTextReporterIntrospector.ResolveName(CtxScript: TCtxScript;
  Scope: TObject; const Name: String; var Index: Integer): Boolean;
var
  Comp: TCtxTextReportSection;
begin
  Comp := TCtxTextReporter(Scope).FindSection(Name);
  Result := Comp <> nil;
  if Result then Index := Integer(Comp);
end;

function TCtxTextReporterIntrospector.Invoke(CtxScript: TCtxScript;
  InvokeType: TCtxInvokeType; Scope: TObject; const Name: String;
  var Index: Integer; ParCount: Integer): Boolean;
begin
  Result := Index <> 0;
  if not Result then
    Result := ResolveName(CtxScript, Scope, Name, Index);
  if Result then
  case InvokeType of
    citGetMethodOrProp:
      CtxScript.Result := VarFromObject(TCtxTextReportSection(Index).CurrentObject);
    else CtxScriptErrorFmt(SInvalidInvokeType, [Name]);
  end;
end;

type
  TCtxTextReporterParamsIntrospector = class (TCtxIntrospector)
  public
    { Finds out wheather this introspector can be used to invoke the code }
    function ResolveName(CtxScript: TCtxScript; Scope: TObject;
      const Name: String; var Index: Integer): Boolean; override;
    { Invokes method or property by index }
    function Invoke(CtxScript: TCtxScript; InvokeType: TCtxInvokeType;
      Scope: TObject; const Name: String; var Index: Integer; ParCount: Integer): Boolean; override;
  end;

function TCtxTextReporterParamsIntrospector.ResolveName(CtxScript: TCtxScript;
  Scope: TObject; const Name: String; var Index: Integer): Boolean;
var
  Idx: Integer;
begin
  Idx := TCtxTextReporter(Scope).Params.IndexOfName(Name);
  Result := Idx >= 0;
  if Result then Index := Idx;
end;

function TCtxTextReporterParamsIntrospector.Invoke(CtxScript: TCtxScript;
  InvokeType: TCtxInvokeType; Scope: TObject; const Name: String;
  var Index: Integer; ParCount: Integer): Boolean;
begin
  Result := Index <> 0;
  if not Result then
    Result := ResolveName(CtxScript, Scope, Name, Index);
  if Result then
  case InvokeType of
    citGetMethodOrProp:
      CtxScript.Result := TCtxTextReporter(Scope).Params.ValueFromIndex[Index]
    else CtxScriptErrorFmt(SInvalidInvokeType, [Name]);
  end;
end;


procedure _TCtxTextReporterSum(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  Idx: Integer;
begin
  Idx := Sender.Pop;
  with TCtxTextReporter(Instance) do
    Sender.Result := FAggrValues[Idx].Sum - CurrentSection.FAggrValues[Idx].Sum;
end;

procedure _TCtxTextReporterCount(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  Idx: Integer;
begin
  Idx := Sender.Pop;
  with TCtxTextReporter(Instance) do
    Sender.Result := FAggrValues[Idx].Count - CurrentSection.FAggrValues[Idx].Count;
end;

procedure _TCtxTextReporterAvg(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  Idx, Count: Integer;
begin
  Idx := Sender.Pop;
  with TCtxTextReporter(Instance) do
  begin
    Count := FAggrValues[Idx].Count - CurrentSection.FAggrValues[Idx].Count;
    if Count > 0 then
      Sender.Result := (FAggrValues[Idx].Sum - CurrentSection.FAggrValues[Idx].Sum) / Count
    else Sender.Result := 0.0;
  end;
end;

procedure _TCtxTextReporterRunSum(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  Idx: Integer;
begin
  Idx := Sender.Pop;
  with TCtxTextReporter(Instance) do
    Sender.Result := FAggrValues[Idx].Sum;
end;

procedure _TCtxTextReporterRunCount(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  Idx: Integer;
begin
  Idx := Sender.Pop;
  with TCtxTextReporter(Instance) do
    Sender.Result := FAggrValues[Idx].Count;
end;

procedure _TCtxTextReporterFlush(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TCtxTextReporter(Instance).FFlushToFile := VarToStr(Sender.Pop);
end;

initialization
  TCtxTextReporterIntrospector.Create(TCtxTextReporter);
  TCtxTextReporterParamsIntrospector.Create(TCtxTextReporter);

  with TCtxCustomIntrospector.Create(TCtxTextReporter) do
  begin
    // object methods
    AddMethod('Sum', @_TCtxTextReporterSum, 1);
    AddMethod('Count', @_TCtxTextReporterCount, 1);
    AddMethod('Avg', @_TCtxTextReporterAvg, 1);
    AddMethod('RunSum', @_TCtxTextReporterRunSum, 1);
    AddMethod('RunCount', @_TCtxTextReporterRunCount, 1);
    AddMethod('Flush', @_TCtxTextReporterFlush, 1);
  end;
end.
