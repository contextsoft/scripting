(******************************************************************************)
(*
(*  Context Scripting Suite
(*
(*  Classes that implement scripting
(*
(*  Contains:
(*                TCtxPasCompiler = class (TCtxCompilter)
(*
(*  Copyright (c) 2010 Michael Baytalsky
(*
(******************************************************************************)
unit CtxPasCompiler;

interface

uses SysUtils, Classes, CtxScript;

type

  {:: Compiler for Context Script. This object should never be created or }
  {:: called directly. It is accessed automatically by scripting engine }
  {:: when locating a propper compiler for a script. }
  TCtxPasCompiler = class (TCtxCompiler)
  protected
    function BinopCompare: TCtxScriptInstruction;
    function BinopAdd: TCtxScriptInstruction;
    function BinopMul: TCtxScriptInstruction;
    function Unop: TCtxScriptInstruction;

    procedure Parameters;
    procedure ParamList;
    procedure Declarations;
    procedure DeclVarList(SymType: TSymbolType);
    procedure DeclConstList;

    // Parsing non-terminals
    procedure ScriptHeader; override;
    procedure Script; override;
    procedure Expression; override;

    procedure Block;
    procedure Statements;
    procedure Statement;
    procedure _Exit;

    procedure AddExpression;
    procedure Term;
    procedure UTerm;
    procedure Factor;

    function ObjectCall(const Identifier: String; AllowAssignment: Boolean): Boolean;
    function FunctionCall(const Identifier: String; IsObjectMethod: Boolean = False;
      AllowAssignment: Boolean = False): Boolean;

    function GetDefaultErrorMessage(ErrorCode: Integer): String; override;
    procedure GetNextToken; override;
  public
    constructor Create(AOwner: TComponent); override;
    {:: Creates empty procedure stub for Context Pas Script. }
    function GetCodeStub(PCode: TCtxScriptCode): String; override;
  end;

implementation

{$I CtxVer.inc}
{$I CtxD2009.inc}

{  Parser Constants }

const
  // Token types
  tokenIdentifier   = 1001;
  tokenBinop        = 1002;
  tokenUnop         = 1003;
  tokenStrConst     = 1004;
  tokenIntConst     = 1005;
  tokenDoubleConst  = 1006;
  tokenReference    = 1008;

  tokenReservedBase = 2000;
  tokenProcedure    = 2001;
  tokenFunction     = 2002;
  tokenBegin        = 2003;
  tokenEnd          = 2004;
  tokenExit         = 2005;
  tokenBreak        = 2006;
  tokenIf           = 2007;
  tokenThen         = 2008;
  tokenElse         = 2009;
  tokenWhile        = 2010;
  tokenDo           = 2011;
  tokenRepeat       = 2012;
  tokenUntil        = 2013;
  tokenFor          = 2014;
  tokenTo           = 2015;
  tokenDownto       = 2016;
  tokenTry          = 2017;
  tokenFinally      = 2018;
  tokenExcept       = 2019;
  tokenRaise        = 2020;
  tokenException    = 2021;
  tokenWith         = tokenUnknown; // 2022;

  tokenRem2Begin    = 2024;
  tokenRem          = 2025;
  tokenGoto         = 2026;
  tokenContinue     = 2027;

  tokenBinopBase    = 3000;
  tokenAnd          = 3001;
  tokenNot          = 3002;
  tokenOr           = 3003;
  tokenXor          = 3004;
  tokenMod          = 3005;
  tokenDiv          = 3006;
  tokenAssign       = 3007;
  tokenGE           = 3008;
  tokenLE           = 3009;
  tokenNE           = 3010;

  tokenIn           = 4001;
  tokenOut          = 4002;
  tokenVar          = 4003;
  tokenConst        = 4004;

  tokenTrue         = 5001;
  tokenFalse        = 5002;
  tokenNull         = 5003;

  strQuote     = '''';
  strResult    = 'result';
  strBreak     = 'break';
  strContinue  = 'continue';

const
  ReservedWords: array [1..37] of TReservedWord = (
    (Name: 'procedure'; TokenType: tokenProcedure),
    (Name: 'function'; TokenType: tokenFunction),
    (Name: 'begin'; TokenType: tokenBegin),
    (Name: 'end'; TokenType: tokenEnd),
    (Name: 'exit'; TokenType: tokenExit),
    (Name: 'break'; TokenType: tokenBreak),
    (Name: 'continue'; TokenType: tokenContinue),
    (Name: 'if'; TokenType: tokenIf),
    (Name: 'then'; TokenType: tokenThen),
    (Name: 'else'; TokenType: tokenElse),
    (Name: 'while'; TokenType: tokenWhile),
    (Name: 'with'; TokenType: tokenWith),
    (Name: 'do'; TokenType: tokenDo),
    (Name: 'repeat'; TokenType: tokenRepeat),
    (Name: 'until'; TokenType: tokenUntil),
    (Name: 'for'; TokenType: tokenFor),
    (Name: 'to'; TokenType: tokenTo),
    (Name: 'downto'; TokenType: tokenDownto),
    (Name: 'try'; TokenType: tokenTry),
    (Name: 'finally'; TokenType: tokenFinally),
    (Name: 'except'; TokenType: tokenExcept),
    (Name: 'raise'; TokenType: tokenRaise),
    (Name: 'const'; TokenType: tokenConst),
    (Name: 'in'; TokenType: tokenIn),
    (Name: 'out'; TokenType: tokenOut),
    (Name: 'var'; TokenType: tokenVar),
    (Name: 'and'; TokenType: tokenAnd),
    (Name: 'not'; TokenType: tokenNot),
    (Name: 'or'; TokenType: tokenOr),
    (Name: 'xor'; TokenType: tokenXor),
    (Name: 'mod'; TokenType: tokenMod),
    (Name: 'div'; TokenType: tokenDiv),
    (Name: 'goto'; TokenType: tokenGoto),
    (Name: 'true'; TokenType: tokenTrue),
    (Name: 'false'; TokenType: tokenFalse),
    (Name: 'null'; TokenType: tokenNull),
    (Name: 'exception'; TokenType: tokenException)
  );

  ReservedOp: array [1..6] of TReservedWord = (
    (Name: '//'; TokenType: tokenRem),
    (Name: ':='; TokenType: tokenAssign),
    (Name: '>='; TokenType: tokenGE),
    (Name: '<='; TokenType: tokenLE),
    (Name: '<>'; TokenType: tokenNE),
    (Name: '(*'; TokenType: tokenRem2Begin)
  );

{ TCtxPasCompiler }

constructor TCtxPasCompiler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RegisterCtxCompiler('Pascal', Self);
end;

procedure TCtxPasCompiler.GetNextToken;
label start;
var
  DecimalPointCount: Integer;
  HexConst: Boolean;
begin
  start:
  inherited GetNextToken;
  FNextToken := '';
  FNextTokenType := tokenUnknown;
  while ((FNextChar = ' ') or (FNextChar = #08) or (FNextChar = strNL)) do
    GetNextChar;
  if FNextChar = strEOF then exit;

  if FNextChar = strQuote then
  begin
    // Extract string constant
    FNextTokenType := tokenStrConst;
    FNextToken := '';
    while True do begin
      GetNextChar;
      if FNextChar = strQuote then
      begin
        // Process double quotes
        GetNextChar;
        if FNextChar = strQuote then
           FNextToken := FNextToken + strQuote
        else Break;
      end else if FNextChar >= #32 then
        FNextToken := FNextToken + FNextChar
      else ParseError(CSCE_UNTERMINATEDSTRING);
    end;
  end else if (FNextChar = '{') then
  begin
    // Skip { } comments
    FComments := '';
    GetNextChar;
    while (FNextChar <> '}') and (FNextChar <> strEOF) do
    begin
      FComments := FComments + FNextChar;
      GetNextChar;
    end;
    if (FNextChar = strEOF) then
      ParseError(CSCE_UNEXPECTEDEOF);
    GetNextChar;
    goto start;
  end else if CharInSet(FNextChar, set_digits) or CharInSet(FNextChar, ['#', '$']) then
  begin
    // Extract integer or floating point number
    if FNextChar = '#' then
    begin
      // string const
      FNextTokenType := tokenStrConst;
      GetNextChar;
    end else FNextTokenType := tokenIntConst;

    HexConst := FNextChar = '$';
    if HexConst then
      GetNextChar;

    FNextToken := '';
    DecimalPointCount := 0;
    while CharInSet(FNextChar, set_digits)
      or (not (HexConst or (FNextTokenType = tokenStrConst)) and (FNextChar = '.'))
      or (HexConst and CharInSet(FNextChar, ['A'..'F'])) do
    begin
      FNextToken := FNextToken + FNextChar;
      GetNextChar;
      if FNextChar = '.' then
        Inc(DecimalPointCount);
    end;

    if DecimalPointCount = 0 then
    begin
      if FNextTokenType = tokenStrConst then
      begin
        if HexConst then
          FNextToken := Char(HexToInt(FNextToken))
        else FNextToken := Char(StrToInt(FNextToken));
      end else begin
        if HexConst then
          FNextToken := IntToStr(HexToInt(FNextToken));
      end;
    end else if DecimalPointCount = 1 then begin
      FNextTokenType := tokenDoubleConst;
    end else
      ParseError(CSCE_INVALIDNUMBER, FNextToken);

  end else if CharInSet(FNextChar, set_alpha) then
  begin
    // Extract identifier
    FNextToken := '';
    repeat
      FNextToken := FNextToken + FNextChar;
      GetNextChar;
    until not (CharInSet(FNextChar, set_alnum) or (FNextChar >= #192));

    if FNextToken = DefArrayPropName then
      FNextTokenType := tokenUnknown
    else begin
      // See if it is a reserved word
      FNextTokenType := IsReserved(ReservedWords, AnsiLowerCase(FNextToken));
      if (FNextTokenType = tokenUnknown) then
        FNextTokenType := tokenIdentifier;
    end;
  end else if FNextChar = '@' then begin
    FNextToken := FNextChar;
    GetNextChar;
    FNextTokenType := tokenReference;
  end else begin
    if CharInSet(FNextChar, ['/', '<', '>', ':', '(']) then
    begin
      // Other small tokens
      FNextToken := FNextChar;
      GetNextChar;
      FNextToken := FNextToken + FNextChar;
      FNextTokenType := IsReserved(ReservedOp, FNextToken);
      if FNextTokenType <> tokenUnknown then
      begin
        // Skip comments
        if (FNextTokenType = tokenRem2Begin) then
        begin
          FComments := '';
          GetNextChar;
          while FNextChar <> strEOF do
          begin
            if FNextChar = '*' then
            begin
              GetNextChar;
              if FNextChar = ')' then
              begin
                GetNextChar;
                break;
              end else FComments := FComments + '*';
            end;
            FComments := FComments + FNextChar;
            GetNextChar;
          end;
          if (FNextChar = strEOF) then exit
          else goto start;
        end else if (FNextTokenType = tokenRem) then
        begin
          FComments := '';
          GetNextChar;
          while ((FNextChar <> strEOF) and (FNextChar <> strNL)) do
          begin
            FComments := FComments + FNextChar;
            GetNextChar;
          end;
          if (FNextChar = strEOF) then exit
          else goto start;
        end else
          GetNextChar;
      end else begin
        FNextToken := FNextToken[1];
        FNextTokenType := Integer(FNextToken[1]);
      end;
    end else begin
      FNextToken := FNextChar;
      FNextTokenType := Integer(FNextChar);
      GetNextChar;
    end;
  end;
end;

procedure TCtxPasCompiler.ScriptHeader;
begin
  if (FNextTokenType = tokenProcedure) or (FNextTokenType = tokenFunction) then
  begin
    FReturnResult := FNextTokenType = tokenFunction;
    GetNextToken;
    FPCode.Description := Trim(FComments);
    // Check program name (optional)
    if FNextTokenType = tokenIdentifier then
    begin
      FPCode.ScriptName := FNextToken;
      GetNextToken;
    end;
    // Process parameters (optional)
    if FNextTokenType = Ord('(') then begin
      Match(Ord('('));
      Parameters;
      Match(Ord(')'));
    end;
    // End of procedure declaration ';'
    Match(Ord(';'));
  end;
end;

procedure TCtxPasCompiler.Script;
var
  ParamCount, TotalSymbols: Integer;
begin
  ParamCount := SymbolCount;
  // Process local declarations
  Declarations;
  // Declare result symbol as a last local variable, so it goes last in stack
  if FReturnResult then
    DeclareSymbol(strResult, stLocalVar);
  TotalSymbols := SymbolCount;
  // allocate place in stack for parameters & local variables
  Emit(CSOP_BEGIN, ParamCount, TotalSymbols);
  EmitBreakPoint;
  Block;
  Match(Ord(';'));
  _Exit;
end;

procedure TCtxPasCompiler._Exit;
begin
  GetNextToken;
  if FReturnResult then
    // Push Result on stack. index of result - last
    Emit(CSOP_GETVAR, SymbolCount - 1);
  Emit(CSOP_EXIT);
end;

procedure TCtxPasCompiler.Parameters;
begin
  // We don't allow empty lists
  repeat
    ParamList;
    if FNextTokenType <> Ord(';') then break;
    GetNextToken;
  until FNextTokenType = Ord(')');
end;

procedure TCtxPasCompiler.ParamList;
var
  SymType: TSymbolType;
begin
  SymType := stInParam;
  case FNextTokenType of
    tokenIdentifier:;
    tokenIn: GetNextToken;
    tokenOut: begin
      SymType := stOutParam;
      GetNextToken;
    end;
    tokenVar: begin
      SymType := stVarParam;
      GetNextToken;
    end;
    tokenConst: begin
      SymType := stConst;
      GetNextToken;
    end;
    else ParseError(CSCE_INVALIDPARAMETER); // Invalid parameter
  end;
  DeclVarList(SymType);
end;

procedure TCtxPasCompiler.DeclVarList(SymType: TSymbolType);
var
  VarName, TypeName: String;
  FirstSymbolInList: Integer;
  I: Integer;
begin
  FirstSymbolInList := SymbolCount;
  while True do
  begin
    VarName := Match(tokenIdentifier);
    // This identifer is declared as a symbol of type SymType
    DeclareSymbol(VarName, SymType);
    if FNextTokenType <> Ord(',') then break;
    GetNextToken;
  end;
  // Variable type
  if FNextTokenType = Ord(':') then
  begin
    GetNextToken;
    TypeName := Match(tokenIdentifier);
    for I := FirstSymbolInList to SymbolCount - 1 do
      Symbols[I].SymbolVarTypeName := TypeName;
  end;
end;

procedure TCtxPasCompiler.DeclConstList;
var
  ConstName, ConstValue: String;
begin
  Match(tokenConst);
  // We don't allow empty lists
  repeat
    ConstName := Match(tokenIdentifier);
    // This identifer is declared as a symbol of type SymType
    Match(Ord('='));
    case FNextTokenType of
      tokenTrue: begin
          DeclareSymbol(ConstName, stConst, True);
          GetNextToken;
        end;
      tokenFalse: begin
          DeclareSymbol(ConstName, stConst, False);
          GetNextToken;
        end;
      tokenIntConst: begin
          DeclareSymbol(ConstName, stConst, StrToInt(FNextToken));
          GetNextToken;
        end;
      tokenDoubleConst: begin
          DeclareSymbol(ConstName, stConst, StrToDouble(FNextToken));
          GetNextToken;
        end;
      tokenStrConst: begin
          ConstValue := FNextToken;
          GetNextToken;
          while FNextTokenType = tokenStrConst do
          begin
            ConstValue := ConstValue + FNextToken;
            GetNextToken;
          end;
          DeclareSymbol(ConstName, stConst, ConstValue);
        end;
      tokenNull: begin
          DeclareSymbol(ConstName, stConst);
          GetNextToken;
        end
      else ParseError(CSCE_CONSTEXPECTED);
    end;
    Match(Ord(';'));
  until FNextTokenType <> tokenIdentifier;
end;

procedure TCtxPasCompiler.Declarations;
begin
  while True do
  begin
    if FNextTokenType = tokenVar then
    begin
      GetNextToken;
      DeclVarList(stLocalVar);
      Match(Ord(';'));
    end else if FNextTokenType = tokenConst then
    begin
      DeclConstList;
    end else break;
  end;
end;

procedure TCtxPasCompiler.Block;
begin
  Match(tokenBegin);
  Statements;
  Match(tokenEnd);
end;

procedure TCtxPasCompiler.Statements;
begin
  while True do
  begin
    Statement;
    if FNextTokenType <> Ord(';') then exit;
    GetNextToken;
  end;
end;

procedure TCtxPasCompiler.Statement;
var
  Identifier: String;
  Pos1, Pos2: Integer;
  LoopVarID, LoopDirection: Integer;
begin
  EmitBreakPoint;

  case FNextTokenType of
    tokenIdentifier: begin
      Identifier := FNextToken;
      GetNextToken;
      case FNextTokenType of
        Ord('('),
        Ord('['),
        Ord('.'):
          ObjectCall(Identifier, True { left to := });

        tokenAssign: begin
          GetNextToken;
          Expression;
          if FReturnResult and (Identifier = FPCode.ScriptName) then
            Identifier := strResult;

          // <Identifier> must be a local or global variable.
          // Depending on type we'll code either VARASSIGN or VARASSIGNBYREF
          LoopVarID := FindSymbol(Identifier, set_anysymbol);
          if LoopVarID >= 0 then
          begin
            // Local variable
            if not (Symbols[LoopVarID].SymbolType in set_assignable) then
              ParseError(CSCE_VARNOTASSIGNABLE, Identifier);

            if Symbols[LoopVarID].SymbolType in [stVarParam, stOutParam] then
              Emit(CSOP_SETVARBYREF, LoopVarID)
            else Emit(CSOP_SETVAR, LoopVarID);
          end else begin
            // Property or global variable
            Emit(CSOP_SETPROP, Identifier, 0);
          end;
        end;

        tokenExcept,
        tokenFinally,
        tokenUntil,
        tokenElse,
        tokenEnd,
        Ord(';'): FunctionCall(Identifier);

        Ord(':'): begin
          DeclareLabel(UpperCase(Identifier));
          GetNextToken;
          Statement;
        end;
        else ParseError(CSCE_SYNTAXERROR, Identifier);
      end;
    end;
    tokenGoto: begin
      GetNextToken;
      Identifier := Match(tokenIdentifier);
      AddLabel(Identifier, Emit(CSOP_JUMP));
    end;
    tokenIf: begin
      GetNextToken;
      Expression;
      // Jump to else or the end of the statement
      Pos1 := Emit(CSOP_JUMPIFFALSE);
      Match(tokenThen);
      Statement;
      if FNextTokenType = tokenElse then
      begin
        // Jump to the end of the statement
        Pos2 := Emit(CSOP_JUMP);
        // Jump here if expression is false
        FixupJump(Pos1);
        GetNextToken;
        Statement;
        // Fixup jump to the end of the statement
        FixupJump(Pos2);
      end else begin
        // Fixup jump to the end of the statement
        FixupJump(Pos1);
      end;
    end;
    tokenWhile: begin
      Inc(FLoopLevel);
      GetNextToken;
      Pos1 := FPCodePos + 1; // Jump to the next instruction
      Expression;
      Pos2 := Emit(CSOP_JUMPIFFALSE);
      Match(tokenDo);
      Statement;
      Emit(CSOP_JUMP, Pos1);
      FixupJump(Pos2);
      // Fixup jumps after loop
      FixupLabel(strContinue + IntToStr(FLoopLevel), Pos1);
      FixupLabel(strBreak + IntToStr(FLoopLevel), FPCodePos + 1);
      Dec(FLoopLevel);
    end;
    tokenRepeat: begin
      Inc(FLoopLevel);
      GetNextToken;
      Pos1 := FPCodePos + 1;
      Statements;
      Match(tokenUntil);
      EmitBreakPoint;
      Expression;
      Emit(CSOP_JUMPIFFALSE, Pos1);
      // Fixup jumps after loop
      FixupLabel(strContinue + IntToStr(FLoopLevel), Pos1);
      FixupLabel(strBreak + IntToStr(FLoopLevel), FPCodePos + 1);
      Dec(FLoopLevel);
    end;
    tokenFor: begin
      Inc(FLoopLevel);
      GetNextToken;
      Identifier := Match(tokenIdentifier);
      // Identify it as a local variable
      LoopVarID := FindSymbol(Identifier, set_localvars);
      if LoopVarID < 0 then
        ParseError(CSCE_LOOPVARNOTFOUND, Identifier);
      Match(tokenAssign);
      Expression;
      // Initialize loop variable
      Emit(CSOP_SETVAR, LoopVarID);

      LoopDirection := 1;
      if FNextTokenType = tokenTo then
        LoopDirection := 1
      else if FNextTokenType = tokenDownto then
        LoopDirection := -1
      else ParseError(CSCE_TOORDOWNTOEXPECTED);
      GetNextToken;
      // Loop starts here, check for expression
      EmitBreakPoint;
      Pos1 := Emit(CSOP_GETVAR, LoopVarID);
      Emit(CSOP_PUSH);
      Expression;
      Match(tokenDo);
      if LoopDirection > 0 then
        Emit(CSOP_LESSOREQUAL)
      else Emit(CSOP_GREATEROREQUAL);
      Emit(CSOP_JUMPIFFALSE);
      Pos2 := FPCodePos;
      // Here goes body statement
      Statement;
      // Jump back to check
      if LoopDirection > 0 then
        Emit(CSOP_INCREMENT, LoopVarID)
      else Emit(CSOP_DECREMENT, LoopVarID);
      Emit(CSOP_JUMP, Pos1);
      FixupJump(Pos2);
      // Fixup jumps after loop
      FixupLabel(strContinue + IntToStr(FLoopLevel), Pos1);
      FixupLabel(strBreak + IntToStr(FLoopLevel), FPCodePos + 1);
      Dec(FLoopLevel);
    end;
    tokenBreak: begin
      GetNextToken;
      AddLabel(strBreak + IntToStr(FLoopLevel), Emit(CSOP_JUMP));
    end;
    tokenContinue: begin
      GetNextToken;
      AddLabel(strContinue + IntToStr(FLoopLevel), Emit(CSOP_JUMP));
    end;
    tokenTry: begin
      GetNextToken;
      // Emit Try
      Pos1 := Emit(CSOP_TRY);
      Statements;
      Emit(CSOP_ENDTRY);
      if FNextTokenType = tokenFinally then
      begin
        GetNextToken;
        // Emit Finally
        FixupJump(Pos1);
        Statements;
        Emit(CSOP_RERAISE);
      end else if FNextTokenType = tokenExcept then
      begin
        GetNextToken;
        // Emit Except
        Emit(CSOP_JUMP);
        Pos2 := FPCodePos;
        FixupJump(Pos1);
        Statements;
        Emit(CSOP_CLEAREXCEPTION);
        FixupJump(Pos2);
      end else ParseError(CSCE_SYNTAXERROR, FNextToken);
      Match(tokenEnd);
    end;
    tokenRaise: begin
      GetNextToken;
      Expression;
      Emit(CSOP_RAISE);
    end;
    tokenExit: _Exit;
    tokenBegin: Block;
    (*
    tokenWith: begin
      GetNextToken;
      Expression;
      Match(tokenDo);
      Emit(CSOP_WITHBEGIN);
      Statement;
      Emit(CSOP_WITHEND);
    end;
    *)
    tokenExcept,
    tokenFinally,
    tokenUntil,
    tokenEnd,
    Ord(';'):;
    else ParseError(CSCE_SYNTAXERROR, FNextToken);
  end;
end;

function TCtxPasCompiler.BinopCompare: TCtxScriptInstruction;
begin
  case FNextTokenType of
    Ord('='): Result := CSOP_EQUAL;
    Ord('>'): Result := CSOP_GREATERTHEN;
    Ord('<'): Result := CSOP_LESSTHEN;
    tokenGE : Result := CSOP_GREATEROREQUAL;
    tokenLE : Result := CSOP_LESSOREQUAL;
    tokenNE : Result := CSOP_NOTEQUAL;
    else      Result := CSOP_NOP;
  end;
end;

function TCtxPasCompiler.BinopAdd: TCtxScriptInstruction;
begin
  case FNextTokenType of
    Ord('+'): Result := CSOP_PLUS;
    Ord('-'): Result := CSOP_MINUS;
    tokenOr : Result := CSOP_OR;
    tokenXor: Result := CSOP_XOR;
    else      Result := CSOP_NOP;
  end;
end;

function TCtxPasCompiler.BinopMul: TCtxScriptInstruction;
begin
  case FNextTokenType of
    Ord('*'): Result := CSOP_MUL;
    Ord('/'): Result := CSOP_DIV;
    tokenDiv: Result := CSOP_IDIV;
    tokenMod: Result := CSOP_MOD;
    tokenAnd: Result := CSOP_AND;
    else      Result := CSOP_NOP;
  end;
end;

function TCtxPasCompiler.Unop: TCtxScriptInstruction;
begin
  case FNextTokenType of
    Ord('+'): Result := CSOP_UPLUS;
    Ord('-'): Result := CSOP_UMINUS;
    tokenNot: Result := CSOP_NOT;
    else      Result := CSOP_NOP;
  end;
end;

procedure TCtxPasCompiler.Expression;
var
  Operation: TCtxScriptInstruction;
begin
  AddExpression;
  Operation := BinopCompare;
  while Operation <> CSOP_NOP do
  begin
    Emit(CSOP_PUSH);
    GetNextToken;
    AddExpression;
    Emit(Operation);
    Operation := BinopCompare;
  end;
end;

procedure TCtxPasCompiler.AddExpression;
var
  Operation: TCtxScriptInstruction;
  Pos: Integer;
begin
  Term;
  Operation := BinopAdd;
  while Operation <> CSOP_NOP do
  begin
    Pos := -1;
    if Operation = CSOP_OR then
      Pos := Emit(CSOP_JUMPIFTRUE);
    Emit(CSOP_PUSH);
    GetNextToken;
    Term;
    Emit(Operation);
    if Pos > 0 then
      FixupJump(Pos);
    Operation := BinopAdd;
  end;
end;

procedure TCtxPasCompiler.Term;
var
  Operation: TCtxScriptInstruction;
  Pos: Integer;
begin
  UTerm;
  Operation := BinopMul;
  while Operation <> CSOP_NOP do
  begin
    Pos := -1;
    if Operation = CSOP_AND then
      Pos := Emit(CSOP_JUMPIFFALSE);
    Emit(CSOP_PUSH);
    GetNextToken;
    UTerm;
    Emit(Operation);
    if Pos > 0 then
      FixupJump(Pos);
    Operation := BinopMul;
  end;
end;

procedure TCtxPasCompiler.UTerm;
var
  Operation: TCtxScriptInstruction;
begin
  Operation := Unop;
  if Operation <> CSOP_NOP then
  begin
    GetNextToken;
    UTerm;
    Emit(Operation);
  end else
    Factor;
end;

procedure TCtxPasCompiler.Factor;
var
  Identifier: String;
  VarID: Integer;
begin
  Identifier := FNextToken;
  case FNextTokenType of
    tokenTrue: begin
      Emit(CSOP_BCONST, True);
      GetNextToken;
    end;
    tokenFalse: begin
      Emit(CSOP_BCONST, False);
      GetNextToken;
    end;
    tokenIntConst: begin
      Emit(CSOP_ICONST, StrToInt(FNextToken));
      GetNextToken;
    end;
    tokenDoubleConst: begin
      Emit(CSOP_DCONST, StrToDouble(FNextToken));
      GetNextToken;
    end;
    tokenStrConst: begin
      Identifier := FNextToken;
      GetNextToken;
      while FNextTokenType = tokenStrConst do
      begin
        Identifier := Identifier + FNextToken;
        GetNextToken;
      end;
      Emit(CSOP_SCONST, Identifier);
    end;
    tokenNull: begin
      Emit(CSOP_NULL);
      GetNextToken;
    end;
    tokenException: begin
      Emit(CSOP_EXCEPTION);
      GetNextToken;
    end;
    tokenIdentifier: begin
      GetNextToken;
      if not ObjectCall(Identifier, False { left to := } ) then
        ParseError(CSCE_FUNCTIONEXPECTED, Identifier);
    end;
    tokenReference: begin
      GetNextToken;
      Identifier := Match(tokenIdentifier);
      // This must be only local variable
      VarID := FindSymbol(Identifier, set_assignable);
      if VarID < 0 then
        ParseError(CSCE_VARNOTASSIGNABLE, Identifier);

      if Symbols[VarID].SymbolType in [stVarParam, stOutParam] then
         Emit(CSOP_GETVAR, VarID)
      else Emit(CSOP_GETVARREF, VarID);
    end;
    Ord('('): begin
      Match(Ord('('));
      Expression;
      Match(Ord(')'));
    end;
    else ParseError(CSCE_SYNTAXERROR, FNextToken);
  end;
end;

function TCtxPasCompiler.ObjectCall(const Identifier: String; AllowAssignment: Boolean): Boolean;
begin
  // Result: returns true if there's an object in Result register
  Result := FunctionCall(Identifier, False, AllowAssignment);
  while Result and (FNextTokenType = Ord('.')) do
  begin
    if not Result then
      ParseError(CSCE_FUNCTIONEXPECTED, Identifier);
    Emit(CSOP_PUSH);  // Save Result on stack for object call
    GetNextToken;
    Result := FunctionCall(Match(tokenIdentifier), True, AllowAssignment);
  end;
end;

function TCtxPasCompiler.FunctionCall(const Identifier: String;
  IsObjectMethod: Boolean = False; AllowAssignment: Boolean = False): Boolean;
var
  VarID, ParamCount, ClosedBracketType: Integer;
begin
  // Result: returns true is there's an object pushed on stack
  ClosedBracketType := tokenUnknown;
  ParamCount := 0;

  // <Identifier> is an array or function. We must find its template and
  // find out whether it returns a result and which parameters
  // does it have. We can't do it now, because we don't have type
  // info for Object methos calls.
  if FNextTokenType in set_brackets then
  begin
    if FNextTokenType = Ord('[') then
      ClosedBracketType := Ord(']')
    else ClosedBracketType := Ord(')');
    GetNextToken;
    repeat
      // TODO: Match parameters to function template
      // TODO: Named parameters
      Expression;
      Emit(CSOP_PUSH); // Push all parameters on stack
      Inc(ParamCount);
      if FNextTokenType <> Ord(',') then break;
      GetNextToken;
    until FNextTokenType = ClosedBracketType;
    Match(ClosedBracketType);
  end;
  // If we're dealing with assign, don't code call in here.
  if FNextTokenType = tokenAssign then
  begin
    if not AllowAssignment then
      ParseError(CSCE_NOASSIGNINEXPRESSION, Identifier);
    if ClosedBracketType = Ord(')') then
      ParseError(CSCE_ASSIGNABLEEXPECTED, Identifier);
    GetNextToken;
    Expression;  // Value remains in Result
    // Code setter
    // Input: Identifier, IsObjectMethod, BracketType and ParamCount
    if IsObjectMethod then
    begin
      if ParamCount = 0 then
        Emit(CSOP_SETOBJPROP, Identifier)
      else Emit(CSOP_SETOBJARRAYPROP, Identifier, ParamCount);
    end else begin
      VarID := FindSymbol(Identifier, set_anysymbol);
      if VarID >= 0 then
      begin
        if not (Symbols[VarID].SymbolType in set_assignable) then
          ParseError(CSCE_VARNOTASSIGNABLE, Identifier);
        if Symbols[VarID].SymbolType in [stVarParam, stOutParam] then
          Emit(CSOP_SETARRAYBYREF, VarID, ParamCount)
        else Emit(CSOP_SETARRAY, VarID, ParamCount);
      end else begin
        Emit(CSOP_SETARRAYPROP, Identifier, ParamCount);
      end;
    end;
    Result := False; // After assignment there's never anything on stack
  end else begin
    // Code getter
    // Input: Identifier, IsObjectMethod, BracketType and ParamCount
    if IsObjectMethod then
    begin
      if ClosedBracketType = Ord(']') then
        Emit(CSOP_OBJARRAYCALL, Identifier, ParamCount)
      else Emit(CSOP_OBJMETHODCALL, Identifier, ParamCount);
    end else begin
      if ClosedBracketType = Ord(']') then
      begin
        VarID := FindSymbol(Identifier, set_anysymbol);
        if VarID >= 0 then
        begin
          if Symbols[VarID].SymbolType in [stVarParam, stOutParam] then
            Emit(CSOP_GETARRAYBYREF, VarID, ParamCount)
          else Emit(CSOP_GETARRAY, VarID, ParamCount);
        end else begin
          Emit(CSOP_ARRAYCALL, Identifier, ParamCount);
        end;
      end else if ClosedBracketType = Ord(')') then
      begin
        Emit(CSOP_METHODCALL, Identifier, ParamCount);
      end else { no brackets } begin
        // <Identifier> can be a variable (only local supported) or function
        VarID := FindSymbol(Identifier, set_anysymbol);
        if VarID >= 0 then
        begin
          if Symbols[VarID].SymbolType in [stVarParam, stOutParam] then
            Emit(CSOP_GETVARBYREF, VarID)
          else Emit(CSOP_GETVAR, VarID);
        end else begin
          Emit(CSOP_METHODCALL, Identifier, ParamCount);
        end;
      end;
    end;
    Result := True; // IsFunction, i.e. something is in Result
  end;
end;

function TCtxPasCompiler.GetCodeStub(PCode: TCtxScriptCode): String;
var
  Params: String;
  I: Integer;
  LastType: TSymbolType;
begin
  Result := '';
  if PCode.IsExpression then exit;
  Params := '';
  LastType := stInParam;
  with PCode do
  for I := 0 to Symbols.Count - 1 do
  if Symbols[I].SymbolType in [stInParam, stOutParam, stVarParam] then
  begin
    if Params <> '' then
    begin
      if Symbols[I].SymbolType = LastType then
        Params := Params + ', '
      else Params := Params + '; ';
    end;
    if Symbols[I].SymbolType <> LastType then
      case Symbols[I].SymbolType of
        stOutParam: Params := Params + ' out ';
        stVarParam: Params := Params + ' var ';
      end;
    Params := Params + Symbols[I].SymbolName;
  end;
  if Params <> '' then
    Params := '(' + Params + ')';
  if PCode.ReturnsValue then
    Result := Format('function %s%s;'#13#10'begin'#13#10'  Result := NULL;'#13#10'end;',
      [PCode.ScriptName, Params])
  else
    Result := Format('procedure %s%s;'#13#10'begin'#13#10'end;',
      [PCode.ScriptName, Params]);
end;

function TCtxPasCompiler.GetDefaultErrorMessage(
  ErrorCode: Integer): String;
begin
  case ErrorCode of
    CSCE_INTERNALERROR        : Result := 'Parser internal error';
    CSCE_SYNTAXERROR          : Result := 'Syntax Error';
    CSCE_IDENTIFIERREDECLARED : Result := 'Identifier redeclared';
    CSCE_UNEXPECTEDEOF        : Result := 'Unexpected end of script';
    CSCE_INVALIDNUMBER        : Result := 'Invalid number format';
    CSCE_UNTERMINATEDSTRING   : Result := 'Unterminated string';
    CSCE_CONSTEXPECTED        : Result := 'Constant expected';
    CSCE_LABELREDECLARED      : Result := 'Label redeclared';
    CSCE_ASSIGNABLEEXPECTED   : Result := 'Assignable expression expected to the left of '':=''';
    CSCE_NOASSIGNINEXPRESSION : Result := 'Assignment in expressions is not supported';
    CSCE_FUNCTIONEXPECTED     : Result := 'This form of method call is not allowed';
    CSCE_INVALIDPARAMETER     : Result := 'Invalid parameter';
    CSCE_TOORDOWNTOEXPECTED   : Result := 'To or downto keywords expected';
    CSCE_LOOPVARNOTFOUND      : Result := 'Loop variable must be a local variable or an input parameter';
    CSCE_LABELNOTFOUND        : Result := 'Label not found';
    CSCE_VARNOTASSIGNABLE     : Result := 'Variable not found or cannot be assigned to';
    else Result := 'Unknown error';
  end;
end;

end.
