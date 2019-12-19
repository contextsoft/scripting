(******************************************************************************)
(*
(*  Context Scripting Suite
(*
(*  Classes that implement scripting
(*
(*  Contains:
(*                TCtxScript = class (TComponent)
(*                TCtxScriptCode = class (TPersistent)
(*                TCtxCompiler = class (TComponent)
(*
(*  Copyright (c) 2010 Michael Baytalsky
(*
(******************************************************************************)
unit CtxScript;

interface

uses
  SysUtils, Contnrs, TypInfo,
  {$IFnDEF VER130}
  Variants,
  {$ENDIF}
  Classes;

type
  TCtxInvokeType = (citGetMethodOrProp, citGetArrayProp,
    citSetProp, citSetArrayProp);
  TCtxInvokeTypes = set of TCtxInvokeType;

const
  citAll = [citGetMethodOrProp, citGetArrayProp, citSetProp, citSetArrayProp];

type
  TCtxScript = class;
  TCtxIntrospector = class;

  ECtxScriptError = class(Exception);

  ECtxScriptCompileError = class(Exception)
  public
    Code: Integer;
    Line: Integer;
    Pos: Integer;
    Context: String;
    ScriptName: String;
    constructor Create(const Msg: String; ACode, ALine, APos: Integer;
      const AScriptName: String; const AContext: String = '');
  end;

  EVariantException = class(Exception)
  public
    Value: Variant;
    constructor Create(AValue: Variant);
  end;

  TOnCtxDebugHook = procedure (Sender: TCtxScript; Exception: Boolean) of object;

  TCtxVarStack = array of Variant;

  TCtxScriptInstruction = (
    CSOP_NOP,           // not used, reserved
    CSOP_BREAKPOINT,    // not used, reserved

    CSOP_PLUS,
    CSOP_UPLUS,
    CSOP_MINUS,
    CSOP_UMINUS,
    CSOP_MUL,
    CSOP_DIV,
    CSOP_IDIV,
    CSOP_MOD,
    CSOP_GREATERTHEN,
    CSOP_LESSTHEN,
    CSOP_GREATEROREQUAL,
    CSOP_LESSOREQUAL,
    CSOP_EQUAL,
    CSOP_NOTEQUAL,
    CSOP_NOT,
    CSOP_AND,
    CSOP_OR,
    CSOP_XOR,

    { Const }
    CSOP_SCONST,            { Set Result to const Value stored in PCodeRec}
    CSOP_ICONST,
    CSOP_DCONST,
    CSOP_BCONST,
    CSOP_NULL,

    { Getters }
               { locals, completely resolved }
    CSOP_GETVAR,            { Get local variable }
    CSOP_GETVARBYREF,       { Get local variable by ref }
    CSOP_GETARRAY,          { Get local array }
    CSOP_GETARRAYBYREF,     { Get local array by ref }
    CSOP_GETVARREF,         { Creates a reference to a local variable }
               { externals, no object on stack }
    CSOP_METHODCALL,        { Get global method. Params on stack }
    CSOP_ARRAYCALL,         { Same as above with array brackets }
               { externals, object on stack }
    CSOP_OBJMETHODCALL,     { Get object's method. Params on stack }
    CSOP_OBJARRAYCALL,      { Same as above with array brackets }

    { Setters }
               { locals, completely resolved }
    CSOP_SETVAR,            { Assign local variable }
    CSOP_SETVARBYREF,       { Assign local variable by ref }
    CSOP_SETARRAY,          { Assign local array }
    CSOP_SETARRAYBYREF,     { Assign local array by ref }
               { externals, no object on stack }
    CSOP_SETPROP,           { Assign object's property }
    CSOP_SETARRAYPROP,      { Assign object's array property }
               { externals, object on stack }
    CSOP_SETOBJPROP,        { Assign object's property }
    CSOP_SETOBJARRAYPROP,   { Assign object's array property }

    { Loops }
    CSOP_INCREMENT,         { Increment local variable }
    CSOP_DECREMENT,         { Decrement local variable }

    { Stack }
    CSOP_POP,               { Result := pop }
    CSOP_PUSH,              { push(Result) }

    { With }
    CSOP_WITHBEGIN,         { Not used, reserved }
    CSOP_WITHEND,           { Not used, reserved }

    { Jumps }
    CSOP_JUMP,
    CSOP_JUMPIFFALSE,       { Result = False }
    CSOP_JUMPIFTRUE,        { Result = True }

    { Exception handling }
    CSOP_TRY,
    CSOP_ENDTRY,
    CSOP_RERAISE,
    CSOP_RAISE,
    CSOP_CLEAREXCEPTION,
    CSOP_EXCEPTION,

    CSOP_BEGIN,             { allocates local vars in stack }
    CSOP_EXIT               { exits the loop }
  );

  { SizeOf(TCtxPCodeRec) = 5 * 4 byte = 20 bytes }
  TCtxPCodeRec = packed record
    OpCode: TCtxScriptInstruction;
    SValue: String;
    ParCount: Byte;
    LineNo: Word;
    case Integer of
      0: (IValue: Integer; IValue2: Integer);
      1: (LIValue: Int64);
      2: (DValue: Double);
      3: (BValue: Boolean);
      4: (Index: Integer;
          Instance: TObject;
          Introspector: TCtxIntrospector);
  end;

  TSymbolType = (stLocalVar, stInParam, stOutParam, stVarParam, stConst);
  TSymbolTypes = set of TSymbolType;

  TSymbol = class (TObject)
  public
    SymbolType: TSymbolType;
    SymbolName: String;
    SymbolVarTypeName: String;
    Value: Variant;
    function Clone: TSymbol;
  end;

  TSymbols = class (TObjectList)
  private
    function GetItem(Idx: Integer): TSymbol;
    procedure SetItem(Idx: Integer; const Value: TSymbol);
  public
    procedure AddSymbols(Symbols: TSymbols);
    function FindSymbol(const SymName: String; SymTypes: TSymbolTypes): Integer;
    function DeclareSymbol(const SymName: String; SymType: TSymbolType; Value: Variant): Integer;
    property Items[Idx: Integer]: TSymbol read GetItem write SetItem; default;
  end;

  {:: Class used to contain Context Script source code as well as PCode }
  {:: resulted from script compilation. }
  TCtxScriptCode = class (TPersistent)
  protected
    FCompiled: Boolean;
    FCode: String;
    FScriptName: String;
    FDescription: String;
    FLanguage: String;
    FSymbols: TSymbols;
    FCodeScope: TCtxScriptCode;

    FIsExpression: Boolean;
    FReturnsValue: Boolean;

    function GetDescription: String;
    procedure SetCodeScope(const Value: TCtxScriptCode);
    procedure SetIsExpression(const Value: Boolean);
    procedure SetLanguage(const Value: String);
    procedure SetCompiled(Value: Boolean);
    procedure SetCode(Value: String);
  public
    PCode: array of TCtxPCodeRec;
    {:: Creates an instance of TCtxScriptCode representing global scope method }
    {:: or expression. }
    constructor Create; overload;

    {:: Destroys the instance of TCtxScriptCode class. }
    destructor Destroy; override;

    {:: Resets compiled state and clears PCode. }
    procedure ResetPCode; // Clears P-code, so it will have to be re-compiled again.
    {:: Compiles the script if it is not compiled. }
    procedure Compile;
    {:: Parses script header (for procedures only) and extracts name, symbols and comments. }
    procedure ParseHeader;

    {:: Returns empty code stub for this script. }
    function GetCodeStub: String;
    {:: Returns debug dump of compiled PCode. }
    function GetDebugText: String;

    {:: Contains a collection of local symbols and formal parameters. }
    {:: This collection is initialized only after compilation. }
    property Symbols: TSymbols read FSymbols;

    property CodeScope: TCtxScriptCode read FCodeScope write SetCodeScope;
  published
    {:: Determinees if Code represent an expression or a complete procedure }
    {:: declaration. }
    property IsExpression: Boolean read FIsExpression write SetIsExpression default False;
    {:: Name of procedure. Assigned automatically during compilation from }
    {:: procedure declaration: <br> }
    {:: procedure ScriptName;<br> }
    {:: begin<br> }
    {:: end;<br> }
    property ScriptName: String read FScriptName write FScriptName;
    {:: Language, used to compile the script. By default if Language is not }
    {:: specified DefaultCtxCompiler variable will be used to locate default }
    {:: compiler. Otherwise it will be located using GetCtxCompiler function. }
    property Language: String read FLanguage write SetLanguage;
    {:: Descripting of this script. Usually the assigned from a comment }
    {:: immediately preceding the script. This field is only available }
    {:: after compilation or a call to ParseHeader. }
    property Description: String read GetDescription write FDescription;
    {:: Contains script code. Default CtxScript syntaxis is similar to Delphi. }
    property Code: String read FCode write SetCode;
    {:: Specifies wheather the script is compiled. Public PCode field contains }
    {:: compiled code. }
    property Compiled: Boolean read FCompiled write SetCompiled stored False;
    {:: Specified whether the script returns value. }
    property ReturnsValue: Boolean read FReturnsValue write FReturnsValue;
  end;

  TCtxScriptState = record
    PCode: TCtxScriptCode;
    StackBegin: Integer;
    ExecPoint: Integer;
    LineNo: Integer;
    SPtr: Integer;
    ExceptSPtr: Integer;
    ExceptValue: Variant;
    Instance: TObject;
    TraceMode: Boolean;
  end;

  TCtxDebugOption = (cdoStopOnException, cdoStopOnBreakpoint);
  TCtxDebugOptions = set of TCtxDebugOption;

  {:: Component used for Context Script debugging and execution. }
  {:: Context script could be written in any language as long as it is }
  {:: compiled into TCtxScriptCode, using instructions from TCtxScriptInstruction }
  {:: enumeration. One global instance of this component can be created and }
  {:: accessed by GetCtxScripting function. }
  TCtxScript = class (TComponent)
  protected
    // Virtual Machine State
    FStackArr: TCtxVarStack;
    FStackSize: Integer;
    // Process State Stack
    FStateStack: array of TCtxScriptState;
    FState: TCtxScriptState;
    FStateStackSize: Integer;

    // Processor Registers
    FResult: Variant;
    FGlobalScope: TObject;

    // Process state
    FSuspended: Boolean;
    FTerminated: Boolean;

    // Debug settings
    FDebugOptions: TCtxDebugOptions;
    FTraceInto: Boolean;
    FOnDebugHook: TOnCtxDebugHook;

    function GetScriptState(Idx: Integer): TCtxScriptState;
    procedure SetStackSize(NewValue: Integer);
    procedure ExecutePCode;

    procedure LoadPCode(PCode: TCtxScriptCode; Params: Variant; Instance: TObject);
    procedure UnloadPCode;
    procedure ResetScriptState;
  public
    {:: Creates an instance of TCtxScript component. }
    constructor Create(AOwner: TComponent); override;
    {:: Destroys the instance of TCtxScript component. }
    destructor Destroy; override;

    {:: Main execute method that is always called to execute any script. }
    function Execute(PCode: TCtxScriptCode; Params: Variant; Instance: TObject = nil): Variant; overload;
    {:: Executes a script without parameters. }
    function Execute(PCode: TCtxScriptCode; Instance: TObject = nil): Variant; overload;
    {:: Executes a script within scope. }
    function Execute(PCode: TCtxScriptCode; CodeScope: TCtxScriptCode): Variant; overload;

    {:: Helper method that create temporary TCtxScriptCode object and }
    {:: execute a script without parameters. }
    function Execute(const Code: String; Instance: TObject = nil): Variant; overload;
    {:: Helper method that create temporary TCtxScriptCode object and }
    {:: execute a script with parameters. }
    function Execute(const Code: String; Params: Variant; Instance: TObject = nil): Variant; overload;

    {:: Helper method that create temporary TCtxScriptCode object and }
    {:: execute it internally. }
    function Evaluate(const Expression: String): Variant; overload;
    {:: Helper method that create temporary TCtxScriptCode object and }
    {:: execute it internally. }
    function Evaluate(const Expression: String; Instance: TObject): Variant; overload;
    {:: Helper method that create temporary TCtxScriptCode object and }
    {:: execute it internally. }
    function Evaluate(const Expression: String; CodeScope: TCtxScriptCode): Variant; overload;

    {:: Suspend script execution. Called only from within DebugHook event handler. }
    {:: Suspended is a modal state. Use process messages loop to emulate suspended state. )
    {:: <br>while Sender.Suspended do Application.ProcessMessages;<br>}
    {:: Use Resume or Terminate to resume or teminate script execution after debugging. }
    procedure Suspend;
    {:: Resume suspended script execution. }
    procedure Resume(TraceMode: Boolean; TraceInto: Boolean);
    {:: Terminate process execution and unloads all scripts currently being executed. }
    procedure Terminate;

    {:: Pop value from stack. This method be used to read parameters }
    {:: passed to external procedures. }
    function Pop: Variant;
    {:: Pushes value on stack. This method be used to pass parameters }
    {:: to scripts. }
    procedure Push(Value: Variant);

    {:: Retrieves parameters from stack in reverse order. Parameters are reverted }
    {:: on stack, i.e. last parameter pops first.}
    {:: This method does not affect stack pointer. }
    function GetParamsRevert(ParamCount: Integer): OleVariant;
    {:: Retrieves parameters from stack in their natural order. Parameters are reverted }
    {:: on stack, i.e. last parameter pops first.}
    {:: This method does not affect stack pointer. }
    function GetParams(ParamCount: Integer): OleVariant;
    {:: Retrieves one parameter by number (ParamCount) from stack. }
    {:: Parameters are reverted on stack, so ParamCount=1 corresponds to }
    {:: the last parameter. }
    {:: This method does not affect stack pointer. }
    function GetParam(ParamCount: Integer): Variant;

    {:: Refer to PCode being executed by scripting. }
    property PCode: TCtxScriptCode read FState.PCode;
    {:: Current stack pointer. SPtr = -1 when stack is empty. }
    property SPtr: Integer read FState.SPtr;
    {:: Current line number of script being executed. }
    property LineNo: Integer read FState.LineNo;
    {:: Current execution point. Correspoinding instruction can be }
    {:: located by calling PCode.PCode[ExecPoint]. }
    property ExecPoint: Integer read FState.ExecPoint;
    {:: SPtr state on the beginning of current script execution. }
    {:: Local variables are store above this point. I.e. first local }
    {:: variable or (Result variable if this script represents a function }
    {:: returning value) can be accessed by index StackArr[StackBegin + 1]. }
    {:: Parameters are located at StackArr[StackBegin], StackArr[StackBegin - 1], etc.}
    property StackBegin: Integer read FState.StackBegin;
    {:: State of the processor. }
    property Suspended: Boolean read FSuspended;
    {:: State of the processor. }
    property Terminated: Boolean read FTerminated;

    {:: Value of Result register. After a script (function or expressio) has been }
    {:: successfully executed this property contains the result of its}
    {:: execution. This property is also used to return result from }
    {:: external methods and introspectors. }
    property Result: Variant read FResult write FResult;
    {:: A pointer to an object used as a static class, whose methods are }
    {:: directly accessible by any script executed by this component. }
    {:: I.e. if GlobalScope is set to TForm, a script may contain }
    {:: calls into form's properties and methods, registered by form's }
    {:: introspectors. For example, a script "Left + Width" will }
    {:: return form's right position. }
    property GlobalScope: TObject read FGlobalScope write FGlobalScope;
    {:: Determines default size of stack. }
    property StackSize: Integer read FStackSize write SetStackSize;

    property ScriptState[Idx: Integer]: TCtxScriptState read GetScriptState;
    property StateStackSize: Integer read FStateStackSize;
  published
    {:: If TraceInto is True, the next script called from this script }
    {:: will have TraceMode set to True. }
    property TraceInto: Boolean read FTraceInto write FTraceInto;
    {:: If TraceMode set to True a script will stop itself on each line }
    {:: before every call to DebugHook. }
    property TraceMode: Boolean read FState.TraceMode write FState.TraceMode;
    {:: Specifies when to call DebugHook. }
    property DebugOptions: TCtxDebugOptions read FDebugOptions write FDebugOptions;
    {:: This event is triggered for every line of executed script. }
    {:: A user can determine that the current line is a break point and }
    {:: call Stop method to pause script execution and evaluate watch expressions. }
    property OnDebugHook: TOnCtxDebugHook read FOnDebugHook write FOnDebugHook;
  end;

  TCtxFieldType = (cftConst, cftMethod, cftProperty, cftArrayProperty);

  TCtxMethod = procedure (Sender: TCtxScript; InvokeType: TCtxInvokeType;
    Instance: TObject; ParCount: Integer);

  TCtxNameCallback = procedure (Sender: TCtxIntrospector; Instance: TObject;
    InvokeType: TCtxInvokeType; const Name: String; ParCount: Integer; Data: Pointer);

  {:: Objects, that are descendants of TCtxIntrospector class are used }
  {:: to create custom name spaces and resolve names during Context Script }
  {:: execution. }
  TCtxIntrospector = class
  protected
    FNext: TCtxIntrospector;
    FClass: TClass;
  public
    constructor Create(AClass: TClass);
    destructor Destroy; override;

    property Next: TCtxIntrospector read FNext;
    property _Class: TClass read FClass;

    function InvokeDefaultArrayProp(CtxScript: TCtxScript; InvokeType: TCtxInvokeType;
      Scope: TObject; const Name: String; var Index: Integer; ParCount: Integer): Boolean; virtual;
    { Finds out wheather this introspector can be used to invoke the code }
    function ResolveName(CtxScript: TCtxScript; Scope: TObject;
      const Name: String; var Index: Integer): Boolean; virtual; abstract;
    { Invokes method or property by index }
    function Invoke(CtxScript: TCtxScript; InvokeType: TCtxInvokeType;
      Scope: TObject; const Name: String; var Index: Integer; ParCount: Integer): Boolean; virtual; abstract;
    procedure ForEachName(Scope: TObject; OnName: TCtxNameCallback;
      Data: Pointer; InvokeTypes: TCtxInvokeTypes = citAll); virtual;
    procedure GetNames(Scope: TObject; List: TStrings; InvokeTypes: TCtxInvokeTypes = citAll); virtual;
  end;

  {:: This object introspects instances of TObject, using only FieldAddress method }
  {:: to locate object's fields. }
  TCtxObjectIntrospector = class (TCtxIntrospector)
  public
    { Finds out wheather this introspector can be used to invoke the code }
    function ResolveName(CtxScript: TCtxScript; Scope: TObject;
      const Name: String; var Index: Integer): Boolean; override;
    { Invokes method or property by index }
    function Invoke(CtxScript: TCtxScript; InvokeType: TCtxInvokeType;
      Scope: TObject; const Name: String; var Index: Integer; ParCount: Integer): Boolean; override;
  end;

  {:: This object introspects instances of TPersistent, using RTTI information }
  {:: to locate object's published properties. }
  TCtxPersistentIntrospector = class (TCtxIntrospector)
  public
    { Finds out wheather this introspector can be used to invoke the code }
    function ResolveName(CtxScript: TCtxScript; Scope: TObject;
      const Name: String; var Index: Integer): Boolean; override;
    { Invokes method or property by index }
    function Invoke(CtxScript: TCtxScript; InvokeType: TCtxInvokeType;
      Scope: TObject; const Name: String; var Index: Integer; ParCount: Integer): Boolean; override;
    procedure ForEachName(Scope: TObject; OnName: TCtxNameCallback;
      Data: Pointer; InvokeTypes: TCtxInvokeTypes = citAll); override;
  end;

  {:: This object introspects instances of TComponent, using component's name }
  {:: to locate child components. }
  TCtxComponentIntrospector = class (TCtxIntrospector)
  public
    { Finds out wheather this introspector can be used to invoke the code }
    function ResolveName(CtxScript: TCtxScript; Scope: TObject;
      const Name: String; var Index: Integer): Boolean; override;
    { Invokes method or property by index }
    function Invoke(CtxScript: TCtxScript; InvokeType: TCtxInvokeType;
      Scope: TObject; const Name: String; var Index: Integer; ParCount: Integer): Boolean; override;
    procedure ForEachName(Scope: TObject; OnName: TCtxNameCallback;
      Data: Pointer; InvokeTypes: TCtxInvokeTypes = citAll); override;
  end;

  {:: This object allows to register custom methods, that will be interpreted }
  {:: as methods of any component of the specified class. }
  {:: This type of introspector is used to publish public methods of }
  {:: different classes. For instance, one can publish a method MyMethod of TForm1 class }
  {:: with TCtxCustomIntrospector.Create(TForm1) do <br>}
  {::   AddMethod('MyMethod', MyMethod); <br>}
  {:: MyMethod should be declared as follows:<br> }
  {::   procedure MyMethod(Sender: TCtxScript; Instance: TObject; ParCount: Integer);<br> }
  {::   var<br> }
  {::     LastName, FirstName: String;<br> }
  {::   begin<br> }
  {::     // We get parameters from stack (last to first).<br> }
  {::     // for the Result := MyMethod('Jack', 'Smith');<br> }
  {::     LastName := Pop; // Last parameter<br> }
  {::     FirstName := Pop; // First parameter<br> }
  {::     // We return result in Sender's Result register.<br> }
  {::     Sender.Result := 'Welcome, ' + FirstName + ' ' + LastName + '!';<br> }
  {::   end;<br> }

  TCtxFieldInfo = class
  protected
    FFieldName: String;
  public
    procedure Invoke(CtxScript: TCtxScript; InvokeType: TCtxInvokeType;
      Scope: TObject; ParCount: Integer); virtual; abstract;
    function GetFieldType: TCtxFieldType; virtual; abstract;
    property FieldName: String read FFieldName;
  end;

  TCtxMethodFieldInfo = class (TCtxFieldInfo)
  protected
    FMethod: TCtxMethod;
    FParCount: Integer;
  public
    constructor Create(const Name: String; Method: TCtxMethod;
      ParCount: Integer = -1);
    procedure Invoke(CtxScript: TCtxScript; InvokeType: TCtxInvokeType;
      Scope: TObject; ParCount: Integer); override;
    function GetFieldType: TCtxFieldType; override;
  end;

  TCtxPropFieldInfo = class (TCtxFieldInfo)
  protected
    FGetMethod, FSetMethod: TCtxMethod;
  public
    constructor Create(const Name: String; GetMethod: TCtxMethod;
      SetMethod: TCtxMethod = nil);
    procedure Invoke(CtxScript: TCtxScript; InvokeType: TCtxInvokeType;
      Scope: TObject; ParCount: Integer); override;
    function GetFieldType: TCtxFieldType; override;
  end;

  TCtxArrayPropFieldInfo = class (TCtxPropFieldInfo)
  protected
    FParCount: Integer;
  public
    constructor Create(const Name: String; GetMethod: TCtxMethod;
      SetMethod: TCtxMethod = nil; ParCount: Integer = -1);
    procedure Invoke(CtxScript: TCtxScript; InvokeType: TCtxInvokeType;
      Scope: TObject; ParCount: Integer); override;
    function GetFieldType: TCtxFieldType; override;
  end;

  TCtxConstFieldInfo = class (TCtxFieldInfo)
  protected
    FValue: Variant;
  public
    constructor Create(const Name: String; ConstValue: Variant);
    procedure Invoke(CtxScript: TCtxScript; InvokeType: TCtxInvokeType;
      Scope: TObject; ParCount: Integer); override;
    function GetFieldType: TCtxFieldType; override;
  end;

  TCtxCustomIntrospector = class (TCtxIntrospector)
  protected
    FFields: TStringList; // of TCtxFieldDescriptor
    function GetFields(Idx: Integer): TCtxFieldInfo;
  public
    constructor Create(AClass: TClass);
    destructor Destroy; override;

    procedure AddFieldInfo(FieldInfo: TCtxFieldInfo);

    procedure AddMethod(const Name: String; Method: TCtxMethod;
      ParCount: Integer = -1);
    procedure AddProp(const Name: String; GetMethod: TCtxMethod;
      SetMethod: TCtxMethod = nil);
    procedure AddArrayProp(const Name: String; GetMethod: TCtxMethod;
      SetMethod: TCtxMethod = nil; ParCount: Integer = -1);
    procedure AddDefaultArrayProp(GetMethod: TCtxMethod;
      SetMethod: TCtxMethod = nil; ParCount: Integer = -1);
    procedure AddConst(const Name: String; Value: Variant);

    function FindField(const Name: String): Integer;

    property Fields[Idx: Integer]: TCtxFieldInfo read GetFields;

    { Finds out wheather this introspector can be used to invoke the code }
    function ResolveName(CtxScript: TCtxScript; Scope: TObject;
      const Name: String; var Index: Integer): Boolean; override;
    { Invokes method or property by index }
    function Invoke(CtxScript: TCtxScript; InvokeType: TCtxInvokeType;
      Scope: TObject; const Name: String; var Index: Integer; ParCount: Integer): Boolean; override;
    procedure ForEachName(Scope: TObject; OnName: TCtxNameCallback;
      Data: Pointer; InvokeTypes: TCtxInvokeTypes = [citGetMethodOrProp..citSetArrayProp]); override;
  end;

  TCtxObjectIterator = class (TPersistent)
    function Current: TObject; virtual; abstract;
    function First: TObject; virtual; abstract;
    function Next: TObject; virtual; abstract;
    function Prior: TObject; virtual; abstract;
    procedure PushState; virtual;
    procedure PopState; virtual;
    function Count: Integer; virtual;
  end;

  TCtxSystemScope = class (TObject);
  TCtxPackage = class (TComponent);

  TReservedWord = record
    Name: String;
    TokenType: Integer;
  end;

  TOnGetErrorMessage = function (ErrorCode: Integer): String of object;

  {:: Virtual ancestor of any Context Compiler. }
  {:: Provides access to a compiled code and gives ways to Emit pcode instructions. }
  TCtxCompiler = class (TComponent)
  protected
    FNextToken: String;
    FComments: String;
    FNextTokenType: Integer;
    FNextChar: Char;
    FCodePos: Integer;
    FPCodePos: Integer;
    FEndOfStatement: Boolean;
    FStatementLineNo: Integer;
    FLastBreakPointLineNo: Integer;
    FLoopLevel: Integer;
    FReturnResult: Boolean;
    FLineNo: Integer;
    FLinePos: Integer;
    FPrevLine: Integer;
    FPrevPos: Integer;
    FLabels: TStringList;
    FLabelEntries: TStringList;
    FPCode: TCtxScriptCode;
    FOnGetErrorMessage: TOnGetErrorMessage;

    function GetSymbolCount: Integer;
    function GetSymbols(Idx: Integer): TSymbol;
    procedure SetSymbols(Idx: Integer; const Value: TSymbol);

    procedure Initialize; virtual;
    procedure GetNextChar; virtual;
    procedure GetNextToken; virtual;
    function Match(TokenType: Integer): String; virtual;
    function GetDefaultErrorMessage(ErrorCode: Integer): String; virtual;

    // Code generation. All Emit functions return Code Position
    function Emit(PSI: TCtxScriptInstruction): Integer; overload;
    function Emit(PSI: TCtxScriptInstruction; SValue: String; ParCount: Integer = 0): Integer; overload;
    function Emit(PSI: TCtxScriptInstruction; IValue: Integer; ParCount: Integer = 0): Integer; overload;
    function Emit(PSI: TCtxScriptInstruction; DValue: Double): Integer; overload;
    function Emit(PSI: TCtxScriptInstruction; BValue: Boolean): Integer; overload;
    procedure EmitBreakPoint;

    procedure ScriptHeader; virtual;
    procedure Script; virtual;
    procedure Expression; virtual;

    procedure ParseError(ErrorCode: Integer; Context: String = ''); virtual;

    procedure AddLabel(const Id: String; JumpPos: Integer);
    procedure FixupJump(JumpInstrPos: Integer);

    // Local labels
    procedure InitializeLabels;
    procedure FinalizeLabels;
    procedure FixupLabels;
    procedure FixupLabel(const LabelName: String; LabelPos: Integer);
    procedure DeclareLabel(const Name: String);

    function DeclareSymbol(const SymName: String; SymType: TSymbolType; Value: Variant): Integer; overload;
    function DeclareSymbol(const SymName: String; SymType: TSymbolType): Integer; overload;
    function FindSymbol(const SymName: String; SymTypes: TSymbolTypes): Integer;

    {:: Parse time only. Returns Symbols declared in PCode. }
    property Symbols[Idx: Integer]: TSymbol read GetSymbols write SetSymbols;
    {:: Parse time only. Returns number of Symbols declared in PCode. }
    property SymbolCount: Integer read GetSymbolCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {:: Compiles passed PCode object. }
    procedure ParseHeader(PCode: TCtxScriptCode); virtual;
    {:: Compiles passed PCode object. }
    procedure Compile(PCode: TCtxScriptCode); virtual;
    {:: Creates empty code stub for this language. }
    function GetCodeStub(PCode: TCtxScriptCode): String; virtual;
    {:: This event can be handled in order to substitute localized error messages. }
    property OnGetErrorMessage: TOnGetErrorMessage read FOnGetErrorMessage write FOnGetErrorMessage;
  end;

const
  MAX_STACK_SIZE = 1024; { of variants }

  varObject    = $000E;   { Variant type corresponding to Delphi objects }
  varReference = $0010;   { Variant type used for var and out parameters }

  DefArrayPropName = '_'; { Reserved name of default array property }

  tokenUnknown = -1;
  strEOF       = #0;
  strNL        = #$0A;

  set_digits        = ['0'..'9'];
  set_alnum         = ['0'..'9', 'a'..'z', 'A'..'Z', '_', #192..#255];
  set_alpha         = ['a'..'z', 'A'..'Z', '_', #192..#255];
  set_brackets      = [Ord('('), Ord('[')];
  set_anysymbol     = [stLocalVar, stInParam, stOutParam, stVarParam, stConst];
  set_localvars     = [stLocalVar, stInParam];
  set_assignable    = [stLocalVar, stInParam, stOutParam, stVarParam];


  // General Parsing routines
  function IsReserved(ReservedWords: array of TReservedWord; const Str: String): Integer;
  function StrToDouble(const Value: String): Double;

  // Variant support routines
  procedure SetDispatchPropValue(Disp: IDispatch; Name: WideString; const Value: OleVariant);
  function GetDispatchPropValue(Disp: IDispatch; Name: WideString): OleVariant;
  function VarDispatchInvoke(Disp: IDispatch; Name: WideString; ParamCount: Integer; Params: Pointer): OleVariant;

  function VarIsObject(const V: Variant; out Obj: TObject): Boolean;
  function VarIsDispatch(const V: Variant; out Disp: IDispatch): Boolean;

  function VarToObject(V: Variant): TObject;
  function VarFromObject(Obj: TObject): Variant;

  function GetObjectProperty(Instance: TObject; const PropName: String): Variant; overload;
  function GetObjectProperty(Instance: TObject; PropInfo: PPropInfo): Variant; overload;
  procedure SetObjectProperty(Instance: TObject; const PropName: String; Value: Variant); overload;
  procedure SetObjectProperty(Instance: TObject; PropInfo: PPropInfo; Value: Variant); overload;

  function VarRef(Value: PVariant): Variant;
  function GetVarByRef(Ref: Variant): Variant;
  procedure SetVarByRef(Ref, Value: Variant);

  function VarToStrEx(Value: Variant): String;
  function HexToInt(const Value: String): Integer;
  // Error raising
  procedure CheckParams(ParCount, MinCount: Integer; MaxCount: Integer = -1);
  procedure CtxScriptError(const Msg: String);
  procedure CtxScriptErrorFmt(const Msg: String; Params: array of const);

  function GetCtxCompiler(const Language: String): TCtxCompiler;
  procedure RegisterCtxCompiler(const Language: String; Compiler: TCtxCompiler);
  procedure UnRegisterCtxCompiler(Compiler: TCtxCompiler);

  procedure RegisterIntrospector(Introspector: TCtxIntrospector);
  procedure UnRegisterIntrospector(Introspector: TCtxIntrospector);
  function FindIntrospector(const ClassName: String): TCtxIntrospector;
  function GetClassIntrospector(ClassName: String): TCtxIntrospector;

  function ResolveObjectName(CtxScript: TCtxScript; Obj: TObject; const Name: String; var Index: Integer): TCtxIntrospector;
  function InvokeObjectMethod(CtxScript: TCtxScript;
    InvokeType: TCtxInvokeType; Instance: TObject; const Name: String;
    ParCount: Integer): Boolean;
  procedure GetNamesInScope(Obj: TObject; List: TStrings; InvokeTypes: TCtxInvokeTypes);

  function GetCtxScripting: TCtxScript;

resourcestring
  SCompileError = 'Error compiling %s. %s at line %d, pos %d';

  SUnableToResume = 'Unable to resume process';
  SUnableToSusupend = 'Unable to suspend the process because it is not executing';

  SUnsupportedInstruction = 'Unsupported instruction';

  SIDispatchNotSupported = 'IDispatch interface is not supported';

  SNotEnoughActualParameters = 'Not enough actual parameters';
  STooManyActualParameters = 'Too many actual parameters';
  SIncorrectStackRestoration = 'Incorrect stack restoration after execute. SPRT = %d, saved_SPTR = %d.';
  SStackUnderflow = 'Stack underflow';
  SStackOverflow = 'Stack overflow';

  SPropertyNotFound = 'Property not found. Property: %s';
  SMethodNotFound = 'Method not found. Method: %s';

  SPropertyCannotBeAssignedTo = 'Property cannot be assigned to: %s';
  SErrorAssigningProperty = 'Error assigning property: %s';
  SNoMultiDimensionalArray = 'Multidimensional arrays are not supported';
  SVariantIsNotAnArray = 'Variable is not an array';
  SPropertyNotAnArray = '%s.%s cannot be references as an array';

  SInvalidObjectGetProperty = 'Invalid object reference. Get property: %s';
  SInvalidObjectSetProperty = 'Invalid object reference. Set property: %s';
  SInvalidObjectMethod = 'Invalid object reference. Method: %s';
  SInvalidObjectReference = 'Error calling %s on invalid object.';

  SVariantIsNotAnObject = 'Variant is not an object';
  SInvalidPropertyType = 'Invalid property type: %s.%s';
  SObjectPropertyNotFound = 'Property not found: %s.%s';
  SAccessViolationNilObject = 'Error accessing a field or method %s of nil object';
  SObjectMethodNotFound = 'Method not found: %s.%s';

  SUnableToInvokeSet = 'Cannot assign a read-only property %s.%s';
  SUnableToInvokeGet = 'Cannot invoke method or property %s.%s';
  SInvalidInvokeType = 'Unable to invoke method or property %s in this manner';

  SParentProcessNotFound = 'Parent process not found';

  SCompilerNotFound = 'Compiler not found for %s';

  STooManyAriguments = 'Too many arguments';

const
  // Compiler errors
  CSCE_INTERNALERROR        = 1001; // 'Parser internal error'
  CSCE_SYNTAXERROR          = 1002; // 'Syntax Error'
  CSCE_IDENTIFIERREDECLARED = 1003; // 'Identifier redeclared'
  CSCE_UNEXPECTEDEOF        = 1004; // 'Unexpected end of script'
  CSCE_INVALIDNUMBER        = 1005; // 'Invalid number format'
  CSCE_UNTERMINATEDSTRING   = 1006; // 'Unterminated string'
  CSCE_CONSTEXPECTED        = 1007; // 'Constant expected'
  CSCE_LABELREDECLARED      = 1008; // 'Label redeclared'
  CSCE_ASSIGNABLEEXPECTED   = 1009; // 'Assignable expression expected to the left of ':=''
  CSCE_NOASSIGNINEXPRESSION = 1010; // 'Assignment in expressions is not supported'
  CSCE_FUNCTIONEXPECTED     = 1011; // 'This form of method call is not allowed'
  CSCE_INVALIDPARAMETER     = 1012; // 'Invalid parameter'
  CSCE_TOORDOWNTOEXPECTED   = 1013; // 'To or downto keywords expected'
  CSCE_LOOPVARNOTFOUND      = 1014; // 'Loop variable must be a local variable or an input parameter'
  CSCE_LABELNOTFOUND        = 1015; // 'Label not found'
  CSCE_VARNOTASSIGNABLE     = 1016; // 'Variable not found or can not be assigned to'

var
  DefaultCtxCompiler: TCtxCompiler;
  CtxSystemScope: TCtxSystemScope;
  CtxIntrospectors: TStringList;
  CtxCompilers: TStringList;

implementation

{$I CtxVer.inc}

uses CtxPasCompiler, Windows, ComObj, ActiveX;

var
  FinalizingIntrospectors: Boolean;
  CtxScripting: TCtxScript;

function GetCtxScripting: TCtxScript;
begin
  if CtxScripting = nil then
    CtxScripting := TCtxScript.Create(nil);
  Result := CtxScripting;
end;

{ ECtxScriptCompilerError }

constructor ECtxScriptCompileError.Create(const Msg: String; ACode, ALine, APos: Integer;
  const AScriptName: String; const AContext: String);
var
  Str: String;
begin
  Str := Msg;
  if Str = '' then
    Str := 'Error ' + IntToStr(ACode);
  if AContext <> '' then
    Str := Str + ': ' + AContext;
  Code := ACode;
  Line := ALine;
  Pos := APos;
  ScriptName := AScriptName;
  Context := AContext;
  inherited CreateFmt(SCompileError, [ScriptName, Str, ALine, APos]);
end;

procedure CtxScriptError(const Msg: String);
begin
  raise ECtxScriptError.Create(Msg);
end;

procedure CtxScriptErrorFmt(const Msg: String; Params: array of const);
begin
  raise ECtxScriptError.CreateFmt(Msg, Params);
end;

function GetCtxCompiler(const Language: String): TCtxCompiler;
var
  Idx: Integer;
begin
  Result := DefaultCtxCompiler;
  if Language = '' then exit;
  Idx := CtxCompilers.IndexOf(Language);
  if Idx < 0 then
    CtxScriptErrorFmt(SCompilerNotFound, [Language])
  else Result := CtxCompilers.Objects[Idx] as TCtxCompiler;
end;

procedure RegisterCtxCompiler(const Language: String; Compiler: TCtxCompiler);
var
  Idx: Integer;
begin
  Idx := CtxCompilers.IndexOf(Language);
  if Idx < 0 then
    CtxCompilers.AddObject(Language, Compiler)
  else CtxCompilers.Objects[Idx] := Compiler;
end;

procedure UnRegisterCtxCompiler(Compiler: TCtxCompiler);
var
  I: Integer;
begin
  I := 0;
  while I < CtxCompilers.Count do
    if CtxCompilers.Objects[I] = Compiler then
      CtxCompilers.Delete(I)
    else Inc(I);
end;

procedure RegisterIntrospector(Introspector: TCtxIntrospector);
var
  Idx: Integer;
begin
  Idx := CtxIntrospectors.IndexOf(Introspector._Class.ClassName);
  if Idx < 0 then
    CtxIntrospectors.AddObject(Introspector._Class.ClassName, Introspector)
  else begin
    Introspector.FNext := TCtxIntrospector(CtxIntrospectors.Objects[Idx]);
    CtxIntrospectors.Objects[Idx] := Introspector;
  end;
end;

procedure UnRegisterIntrospector(Introspector: TCtxIntrospector);
var
  Idx: Integer;
  Temp: TCtxIntrospector;
begin
  Idx := CtxIntrospectors.IndexOf(Introspector._Class.ClassName);
  if Idx >= 0 then
  begin
    Temp := TCtxIntrospector(CtxIntrospectors.Objects[Idx]);
    if Temp = Introspector then
      CtxIntrospectors.Objects[Idx] := Temp.Next
    else
      while Temp <> nil do
      begin
        if Temp.Next = Introspector then
        begin
          Temp.FNext := Introspector.Next;
          break;
        end;
        Temp := Temp.Next;
      end;
  end;
end;

function FindIntrospector(const ClassName: String): TCtxIntrospector;
var
  Idx: Integer;
begin
  Idx := CtxIntrospectors.IndexOf(ClassName);
  if Idx >= 0 then
    Result := TCtxIntrospector(CtxIntrospectors.Objects[Idx])
  else Result := nil;
end;

procedure FinalizeIntrospectors;
var
  I: Integer;
  Temp, Introspector: TCtxIntrospector;
begin
  FinalizingIntrospectors := True;
  try
    I := CtxIntrospectors.Count - 1;
    while I >= 0 do
    begin
      Introspector := TCtxIntrospector(CtxIntrospectors.Objects[I]);
      while Introspector <> nil do
      begin
        Temp := Introspector.FNext;
        Introspector.Free;
        Introspector := Temp;
      end;
      Dec(I);
    end;
    CtxIntrospectors.Clear;
  finally
    FinalizingIntrospectors := False;
  end;
end;

function GetClassIntrospector(ClassName: String): TCtxIntrospector;
var
  ClassInstance: TClass;
begin
  Result := nil;
  while ClassName <> '' do
  begin
    Result := FindIntrospector(ClassName);
    if Result <> nil then
      exit;
    ClassName := '';
    ClassInstance := GetClass(ClassName);
    if ClassInstance <> nil then
    begin
      ClassInstance := ClassInstance.ClassParent;
      if ClassInstance <> nil then
        ClassName := ClassInstance.ClassName;
    end;
  end;
end;

function ResolveObjectName(CtxScript: TCtxScript; Obj: TObject; const Name: String;
  var Index: Integer): TCtxIntrospector;
var
  ObjClass: TClass;
begin
  Result := nil;
  ObjClass := Obj.ClassType;
  while ObjClass <> nil do
  begin
    Result := FindIntrospector(ObjClass.ClassName);
    while Result <> nil do
    begin
      Index := 0;
      if Result.ResolveName(CtxScript, Obj, Name, Index) then break;
      Result := Result.Next;
    end;
    if Result = nil then
      ObjClass := ObjClass.ClassParent
    else break;
  end;
end;

procedure GetNamesInScope(Obj: TObject; List: TStrings; InvokeTypes: TCtxInvokeTypes);
var
  ObjClass: TClass;
  Introspector: TCtxIntrospector;
begin
  if Obj = nil then exit;
  ObjClass := Obj.ClassType;
  while ObjClass <> nil do
  begin
    Introspector := FindIntrospector(ObjClass.ClassName);
    while Introspector <> nil do
    begin
      Introspector.GetNames(Obj, List, InvokeTypes);
      Introspector := Introspector.Next;
    end;
    ObjClass := ObjClass.ClassParent;
  end;
end;

function InvokeObjectMethod(CtxScript: TCtxScript;
  InvokeType: TCtxInvokeType; Instance: TObject; const Name: String;
  ParCount: Integer): Boolean;
var
  Index: Integer;
  Introspector: TCtxIntrospector;
begin
  Index := 0;
  Introspector := ResolveObjectName(CtxScript, Instance, Name, Index);
  Result := Introspector <> nil;
  if Result then
    Result := Introspector.Invoke(CtxScript, InvokeType, Instance,
      Name, Index, ParCount);
end;

function StrToChar(const Str: String): Char;
begin
  if Str = '' then Result := #0
  else Result := Str[1];
end;

procedure CheckParams(ParCount, MinCount: Integer; MaxCount: Integer = -1);
begin
  if MaxCount < 0 then
    MaxCount := MinCount;
  if ParCount < MinCount then
    CtxScriptError(SNotEnoughActualParameters)
  else if ParCount > MaxCount then
    CtxScriptError(STooManyActualParameters);
end;

function VarIsDispatch(const V: Variant; out Disp: IDispatch): Boolean;
begin
  Disp := nil;
  if TVarData(V).VType = varDispatch then
    Disp := IDispatch(TVarData(V).VDispatch)
  else if TVarData(V).VType = (varDispatch or varByRef) then
    Disp := IDispatch(Pointer(TVarData(V).VPointer^));
  Result := Disp <> nil;
end;

function VarIsObject(const V: Variant; out Obj: TObject): Boolean;
begin
  Result := TVarData(V).VType = varObject;
  if Result then
    Obj := TVarData(V).VPointer;
end;

function VarToObject(V: Variant): TObject;
begin
  if not VarIsObject(V, Result) then
    CtxScriptError(SVariantIsNotAnObject);
end;

function VarFromObject(Obj: TObject): Variant;
begin
  TVarData(Result).VType := varObject;
  TVarData(Result).VPointer := Obj;
end;

function GetDispatchPropValue(Disp: IDispatch; Name: WideString): OleVariant;
var
  ID: Integer;
  ExcepInfo: TExcepInfo;
  DispParams: TDispParams;
  Status: HResult;
begin
  Status := Disp.GetIDsOfNames(GUID_NULL, @Name, 1, 0, @ID);
  OleCheck(Status);
  FillChar(ExcepInfo, SizeOf(ExcepInfo), 0);
  with DispParams do
  begin
    rgvarg := nil;
    rgdispidNamedArgs := nil;
    cArgs := 0;
    cNamedArgs := 0;
  end;
  Status := Disp.Invoke(ID, GUID_NULL, 0, DISPATCH_METHOD or DISPATCH_PROPERTYGET,
    DispParams, @Result, @ExcepInfo, nil);
  if Status <> S_OK then DispatchInvokeError(Status, ExcepInfo);
end;

procedure SetDispatchPropValue(Disp: IDispatch; Name: WideString;
  const Value: OleVariant);
const
  DispIDArgs: Longint = DISPID_PROPERTYPUT;
var
  ID: Integer;
  ExcepInfo: TExcepInfo;
  DispParams: TDispParams;
  Status: HResult;
begin
  with DispParams do
  begin
    rgvarg := @Value;
    rgdispidNamedArgs := @DispIDArgs;
    cArgs := 1;
    cNamedArgs := 1;
  end;
  FillChar(ExcepInfo, SizeOf(ExcepInfo), 0);
  OleCheck(Disp.GetIDsOfNames(GUID_NULL, @Name, 1, 0, @ID));
  Status := Disp.Invoke(ID, GUID_NULL, 0, DISPATCH_PROPERTYPUT, DispParams,
    nil, @ExcepInfo, nil);
  if Status <> S_OK then DispatchInvokeError(Status, ExcepInfo);
end;

procedure DispatchInvoke(const Dispatch: IDispatch; CallDesc: PCallDesc;
  DispIDs: PDispIDList; Params: Pointer; Result: PVariant);
const
  MaxDispArgs = 64; {!!!}
  { Parameter type masks }
  atVarMask  = $3F;
  atTypeMask = $7F;
  atByRef    = $80;
type
  PVarArg = ^TVarArg;
  TVarArg = array[0..3] of DWORD;
  TStringDesc = record
    BStr: PWideChar;
    PStr: PString;
  end;
var
  I, J, K, ArgType, ArgCount, StrCount, DispID, InvKind, Status: Integer;
  VarFlag: Byte;
  ParamPtr: ^Integer;
  ArgPtr, VarPtr: PVarArg;
  DispParams: TDispParams;
  ExcepInfo: TExcepInfo;
  Strings: array[0..MaxDispArgs - 1] of TStringDesc;
  Args: array[0..MaxDispArgs - 1] of TVarArg;
begin
  StrCount := 0;
  try
    ArgCount := CallDesc^.ArgCount;
    if ArgCount > MaxDispArgs then
      CtxScriptError(STooManyAriguments);
    if ArgCount <> 0 then
    begin
      ParamPtr := Params;
      ArgPtr := @Args[ArgCount];
      I := 0;
      repeat
        Dec(Integer(ArgPtr), SizeOf(TVarData));
        ArgType := CallDesc^.ArgTypes[I] and atTypeMask;
        VarFlag := CallDesc^.ArgTypes[I] and atByRef;
        if ArgType = varError then
        begin
          ArgPtr^[0] := varError;
          ArgPtr^[2] := DWORD(DISP_E_PARAMNOTFOUND);
        end
        else
        begin
          if VarFlag <> 0 then
          begin
            if (ArgType = varVariant) and
               (PVarData(ParamPtr^)^.VType = varString) then
              VarCast(PVariant(ParamPtr^)^, PVariant(ParamPtr^)^, varOleStr);

            ArgPtr^[0] := ArgType or varByRef;
            ArgPtr^[2] := ParamPtr^;
          end
          else if ArgType = varVariant then
          begin
            if PVarData(ParamPtr)^.VType = varString then
            begin
              with Strings[StrCount] do
              begin
                BStr := StringToOleStr(string(PVarData(ParamPtr)^.VString));
                PStr := nil;
                ArgPtr^[0] := varOleStr;
                ArgPtr^[2] := Integer(BStr);
              end;
              Inc(StrCount);
            end else if PVarData(ParamPtr)^.VType = varReference then begin

              if PVariant(PVarData(ParamPtr)^.VPointer)^.VType = varString then
                VarCast(PVariant(PVarData(ParamPtr)^.VPointer)^,
                  PVariant(PVarData(ParamPtr)^.VPointer)^, varOleStr);

              ArgPtr^[0] := ArgType or varByRef;
              ArgPtr^[2] := Integer(PVarData(ParamPtr)^.VPointer);
            end else begin
              VarPtr := PVarArg(ParamPtr);
              ArgPtr^[0] := VarPtr^[0];
              ArgPtr^[1] := VarPtr^[1];
              ArgPtr^[2] := VarPtr^[2];
              ArgPtr^[3] := VarPtr^[3];
            end;
          end;
          Inc(Integer(ParamPtr), SizeOf(TVarData));
        end;
        Inc(I);
      until I = ArgCount;
    end;
    DispParams.rgvarg := @Args;
    DispParams.rgdispidNamedArgs := @DispIDs[1];
    DispParams.cArgs := ArgCount;
    DispParams.cNamedArgs := CallDesc^.NamedArgCount;
    DispID := DispIDs[0];
    InvKind := CallDesc^.CallType;
    if InvKind = DISPATCH_PROPERTYPUT then
    begin
      if Args[0][0] and varTypeMask = varDispatch then
        InvKind := DISPATCH_PROPERTYPUTREF;
      DispIDs[0] := DISPID_PROPERTYPUT;
      Dec(Integer(DispParams.rgdispidNamedArgs), SizeOf(Integer));
      Inc(DispParams.cNamedArgs);
    end else
      if (InvKind = DISPATCH_METHOD) and (ArgCount = 0) and (Result <> nil) then
        InvKind := DISPATCH_METHOD or DISPATCH_PROPERTYGET;
    Status := Dispatch.Invoke(DispID, GUID_NULL, 0, InvKind, DispParams,
      Result, @ExcepInfo, nil);
    if Status <> 0 then DispatchInvokeError(Status, ExcepInfo);
    J := StrCount;
    while J <> 0 do
    begin
      Dec(J);
      with Strings[J] do
        if PStr <> nil then OleStrToStrVar(BStr, PStr^);
    end;
  finally
    K := StrCount;
    while K <> 0 do
    begin
      Dec(K);
      SysFreeString(Strings[K].BStr);
    end;
  end;
end;

function VarDispatchInvoke(Disp: IDispatch; Name: WideString; ParamCount: Integer; Params: Pointer): OleVariant;
var
  CallDesc: TCallDesc;
  ID: Integer;
  I, Status: Integer;
begin
  with CallDesc do
  begin
    CallType := DISPATCH_METHOD;
    ArgCount := ParamCount;
    if ArgCount = 0 then
      CallType := CallType or DISPATCH_PROPERTYGET;
    NamedArgCount := 0;
    for I := 0 to ArgCount - 1 do
      ArgTypes[I] := varVariant;
    StrPCopy(PChar(@ArgTypes[ArgCount]), LowerCase(Name));
  end;
  Status := Disp.GetIDsOfNames(GUID_NULL, @Name, 1, 0, @ID);
  OleCheck(Status);
  VarClear(Result);
  DispatchInvoke(Disp, @CallDesc, @ID, Params, @Result);
end;

function FindPropInfo(Instance: TObject; const PropName: string): PPropInfo; overload;
begin
  Result := GetPropInfo(Instance, PropName);
  if Result = nil then
    CtxScriptErrorFmt(SPropertyNotFound, [PropName]);
end;

function GetObjectProperty(Instance: TObject; PropInfo: PPropInfo): Variant;
var
  TypeData: PTypeData;
begin
  case PropInfo^.PropType^^.Kind of
    tkInteger, tkChar, tkWChar:
      Result := GetOrdProp(Instance, PropInfo);
    tkClass:
      Result := VarFromObject(TObject(GetOrdProp(Instance, PropInfo)));
    tkEnumeration: begin
      TypeData := GetTypeData(PropInfo^.PropType^);
      if TypeData^.BaseType^ = TypeInfo(Boolean) then
        Result := Boolean(GetOrdProp(Instance, PropInfo))
      else Result := GetOrdProp(Instance, PropInfo);
    end;
    tkSet:
      Result := GetOrdProp(Instance, PropInfo);
    tkFloat:
      Result := GetFloatProp(Instance, PropInfo);
    tkMethod:
      Result := PropInfo^.PropType^.Name;
    {$IFDEF D2009_ORLATER}
    tkUString,
    {$ENDIF}
    tkString, tkLString, tkWString:
      Result := GetStrProp(Instance, PropInfo);
    tkVariant:
      Result := GetVariantProp(Instance, PropInfo);
    tkInt64:
      Result := GetInt64Prop(Instance, PropInfo) + 0.0;
    else CtxScriptErrorFmt(SInvalidPropertyType, [Instance.ClassName, PropInfo.Name]);
  end;
end;

procedure SetPropValue(Instance: TObject; PropInfo: PPropInfo;
  const Value: Variant);

  function RangedValue(const AMin, AMax: Int64): Int64;
  begin
    Result := Trunc(Value);
    if (Result < AMin) or (Result > AMax) then
      CtxScriptErrorFmt(SErrorAssigningProperty, [PropInfo.Name]);
  end;

var
  TypeData: PTypeData;
  DynArray: Pointer;
begin
  // get the prop info
  TypeData := GetTypeData(PropInfo^.PropType^);

  // set the right type
  case PropInfo.PropType^^.Kind of
    tkInteger, tkChar, tkWChar:
      if TypeData^.MinValue < TypeData^.MaxValue then
        SetOrdProp(Instance, PropInfo, RangedValue(TypeData^.MinValue,
          TypeData^.MaxValue))
      else
        // Unsigned type
        SetOrdProp(Instance, PropInfo,
          RangedValue(LongWord(TypeData^.MinValue),
          LongWord(TypeData^.MaxValue)));
    tkEnumeration:
      if VarType(Value) = varString then
        SetEnumProp(Instance, PropInfo, VarToStr(Value))
      else if VarType(Value) = varBoolean then
        // Need to map variant boolean values -1,0 to 1,0
        SetOrdProp(Instance, PropInfo, Abs(Trunc(Value)))
      else
        SetOrdProp(Instance, PropInfo, RangedValue(TypeData^.MinValue,
          TypeData^.MaxValue));
    tkSet:
      if VarType(Value) = varInteger then
        SetOrdProp(Instance, PropInfo, Value)
      else
        SetSetProp(Instance, PropInfo, VarToStr(Value));
    tkFloat:
      SetFloatProp(Instance, PropInfo, Value);
    {$IFDEF D2009_ORLATER}
    tkUString,
    {$ENDIF}
    tkString, tkLString:
      SetStrProp(Instance, PropInfo, VarToStr(Value));
    {$IFnDEF VER130}
    tkWString:
      SetWideStrProp(Instance, PropInfo, VarToWideStr(Value));
    {$ENDIF}
    tkVariant:
      SetVariantProp(Instance, PropInfo, Value);
    tkInt64:
      SetInt64Prop(Instance, PropInfo, RangedValue(TypeData^.MinInt64Value,
        TypeData^.MaxInt64Value));
    tkDynArray:
      begin
        DynArrayFromVariant(DynArray, Value, PropInfo^.PropType^);
        SetOrdProp(Instance, PropInfo, Integer(DynArray));
      end;
    else
      CtxScriptErrorFmt(SErrorAssigningProperty, [PropInfo.Name]);
  end;
end;

function GetObjectProperty(Instance: TObject; const PropName: String): Variant;
begin
  Result := GetObjectProperty(Instance, FindPropInfo(Instance, PropName));
  if Instance = nil then
    CtxScriptErrorFmt(SAccessViolationNilObject, [PropName]);
end;

procedure SetObjectProperty(Instance: TObject; PropInfo: PPropInfo; Value: Variant);
var
  Obj: TObject;
begin
  if VarIsObject(Value, Obj) then
    SetOrdProp(Instance, PropInfo, Integer(Obj))
  else SetPropValue(Instance, PropInfo, Value);
end;

procedure SetObjectProperty(Instance: TObject; const PropName: String; Value: Variant);
begin
  SetObjectProperty(Instance, FindPropInfo(Instance, PropName), Value);
end;

function VarRef(Value: PVariant): Variant;
begin
  VarClear(Result);
  TVarData(Result).VType := varReference;
  TVarData(Result).VPointer := Value;
end;

function GetVarByRef(Ref: Variant): Variant;
begin
  Result := PVariant(TVarData(Ref).VPointer)^;
end;

procedure SetVarByRef(Ref, Value: Variant);
begin
  PVariant(TVarData(Ref).VPointer)^ := Value;
end;

function VarToStrEx(Value: Variant): String;
var
  Dim, A, I: Integer;
  Obj: TObject;
begin
  if TVarData(Value).VType = varReference then
    Result := '^' + VarToStrEx(GetVarByRef(Value))
  else if TVarData(Value).VType = varObject then
  begin
    try
      Obj := VarToObject(Value);
      if Obj = nil then
        Result := 'nil object'
      else Result := Obj.ClassName + ' at ' + IntToHex(Integer(Obj), 8);
    except
      Result := 'Invalid object at $' + IntToHex(Integer(TVarData(Value).VPointer), 8)
    end;
  end else if TVarData(Value).VType = varBoolean then
    Result := BooleanIdents[Boolean(Value)]
  else if TVarData(Value).VType and varArray <> 0 then
  begin
    Result := '';
    Dim := VarArrayDimCount(Value);
    if Dim = 1 then
    begin
      A := VarArrayLowBound(Value, 1);
      if A <= VarArrayHighBound(Value, 1) then
        Result := '[' + VarToStrEx(Value[A]);
      for I := A + 1 to VarArrayHighBound(Value, 1) do
        Result := Result + ', ' + VarToStrEx(Value[I]);
      Result := Result + ']';
    end else
      Result := IntToStr(Dim) + ' dimensional array';
  end else if VarIsEmpty(Value) then
    Result := 'Unassigned'
  else if VarIsNull(Value) then
    Result := 'Null'
  else Result := VarToStr(Value);
end;

function HexToInt(const Value: String): Integer;
var
  I, P: Integer;
begin
  Result := 0;
  for I := 1 to Length(Value) do
  begin
    P := AnsiPos(Value[I], '0123456789ABCDEF') - 1;
    if P < 0 then
    begin
      // Invalid character in input...
      Result := 0;
      exit;
    end;
    Result := Result shl 4 + P;
  end;
end;

{ EVariantException }

constructor EVariantException.Create(AValue: Variant);
begin
  inherited Create(VarToStr(AValue));
  Value := AValue;
end;

{ TCtxScriptCode }

constructor TCtxScriptCode.Create;
begin
  inherited Create;
  FCode := '';
  FScriptName := '';
  FDescription := '';
  FLanguage := '';
  SetLength(PCode, 0);
  FCompiled := False;
  FSymbols := TSymbols.Create(True);
end;

destructor TCtxScriptCode.Destroy;
begin
  FreeAndNil(FSymbols);
  inherited Destroy;
end;

procedure TCtxScriptCode.SetCode(Value: String);
begin
  if FCode <> Value then
  begin
    FCode := Value;
    ResetPCode;
  end;
end;

procedure TCtxScriptCode.ResetPCode;
begin
  SetLength(PCode, 0);
  FCompiled := False;
  FSymbols.Clear;
end;

procedure TCtxScriptCode.SetCompiled(Value: Boolean);
begin
  if FCompiled <> Value then
  begin
    if Value then Compile
    else ResetPCode;
  end;
end;

procedure TCtxScriptCode.Compile;
begin
  if not FCompiled then
  begin
    ResetPCode;
    if FCodeScope <> nil then
      FSymbols.AddSymbols(FCodeScope.Symbols);
    GetCtxCompiler(Language).Compile(Self);
    FCompiled := True;
  end;
end;

procedure TCtxScriptCode.ParseHeader;
begin
  if not FCompiled then
  begin
    ResetPCode;
    GetCtxCompiler(Language).ParseHeader(Self);
  end;
end;

procedure TCtxScriptCode.SetIsExpression(const Value: Boolean);
begin
  if FIsExpression <> Value then
  begin
    FIsExpression := Value;
    ResetPCode;
  end;
end;

procedure TCtxScriptCode.SetCodeScope(const Value: TCtxScriptCode);
begin
  if FCodeScope <> Value then
  begin
    FCodeScope := Value;
    ResetPCode;
  end;
end;

procedure TCtxScriptCode.SetLanguage(const Value: String);
begin
  if FLanguage <> Value then
  begin
    FLanguage := Value;
    ResetPCode;
  end;
end;

const
  carBoolean: array [Boolean] of String = ('False', 'True');
  // Parameters
  // %0:s -- SValue
  // %1:d -- ParCount
  // %2:d -- IValue
  // %3:d -- IValue2
  // %4:f -- DValue
  // %5:s -- BValue
  carCSOP: array [TCtxScriptInstruction] of String = (
    'NOP',
    '+ BREAK POINT AT %2:d',
    'PLUS', 'UPLUS', 'MINUS', 'UMINUS', 'MUL' , 'DIV' , 'IDIV' , 'MOD' ,
    'GREATER THEN' , 'LESS THEN' , 'GR.OR EQ.' , 'LESS OR EQ.' , 'EQUAL' ,
    'NOT EQUAL', 'NOT' , 'AND' , 'OR' , 'XOR',

    'Result <- STR %0:s',
    'Result <- INT %2:d',
    'Result <- DOUBLE %4:f',
    'Result <- BOOL %5:s',
    'Result <- NULL',

    'Result <- VAR %2:d',
    'Result <- VAR BY REF %2:d',
    'Result <- ARRAY %2:d [%1:d params]',
    'Result <- ARRAY BY REF %2:d [%1:d params]',
    'Result <- @VAR %2:d',

    'Result <- METHOD %0:s (%1:d params)',
    'Result <- ARRAY %0:s (%1:d params)',
    'Result <- OBJ. METHOD %0:s (%1:d params)' ,
    'Result <- OBJ. ARRAY PROP. %0:s (%1:d params)' ,

    'Result -> VAR %2:d',
    'Result -> VAR BY REF %2:d',
    'Result -> ARRAY %2:d [%1:d]',
    'Result -> ARRAY BY REF %2:d [%1:d]',

    'Result -> PROP %0:s (%1:d params)',
    'Result -> PROP %0:s [%1:d params]',
    'Result -> OBJ. PROP %0:s (%1:d params)',
    'Result -> OBJ. PROP %0:s [%1:d params]',

    'INC %2:d', 'DEC %2:d',
    'POP RESULT', 'PUSH RESULT' ,
    'WITH RESULT', 'WITH END',

    'JUMP %2:d',
    'JUMP IF NOT RESULT TO %2:d',
    'JUMP IF RESULT TO %2:d',

    'TRY %2:d',
    'END TRY',
    'RAISE (Exception)' ,
    'RAISE (Result)' ,
    'CLEAR Exception' ,
    'Result := Exception',
    'BEGIN %2:d of %1:d',
    'EXIT'
  );

function TCtxScriptCode.GetDebugText: String;
var
  CurPos: Integer;
begin
  CurPos := 0;
  Result := '';
  while CurPos < Length(PCode) do
  with PCode[CurPos] do
  begin
    Result := Result + Format(IntToStr(CurPos) + ': ' + carCSOP[OpCode] + #13#10,
      [SValue, ParCount, IValue, IValue2, DValue, carBoolean[BValue]]);
    if OpCode = CSOP_EXIT then
      break;
    Inc(CurPos);
  end;
end;

function TCtxScriptCode.GetCodeStub: String;
begin
  Result := GetCtxCompiler(Language).GetCodeStub(Self);
end;

function TCtxScriptCode.GetDescription: String;
begin
  Result := FDescription;
  if Result = '' then
    Result := ScriptName;
end;


{ TCtxScript }

constructor TCtxScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStackSize := 0;
  SetStackSize(32);

  ResetScriptState;
  TraceMode := False; // stops on each line breakpoint
  TraceInto := False; // set TraceMode = True for called methods

  SetLength(FStateStack, 10);
  FStateStackSize := 0;

  FResult := Unassigned;
  FGlobalScope := nil;
end;

destructor TCtxScript.Destroy;
begin
  if Suspended then Terminate;
  inherited Destroy;
end;

procedure TCtxScript.ResetScriptState;
begin
  FState.SPtr := -1;
  FState.StackBegin := -1;
  FState.ExceptSPtr := -1;
  FState.PCode := nil;
  FState.LineNo := 0;
  FState.ExecPoint := 0;
  FState.ExceptValue := Unassigned;
end;

procedure TCtxScript.SetStackSize(NewValue: Integer);
begin
  FStackSize := NewValue;
  SetLength(FStackArr, FStackSize);
end;

function TCtxScript.Execute(const Code: String; Instance: TObject = nil): Variant;
var
  PCode: TCtxScriptCode;
begin
  PCode := TCtxScriptCode.Create;
  try
    PCode.Code := Code;
    Result := Execute(PCode, Instance);
  finally
    PCode.Free;
  end;
end;

function TCtxScript.Execute(const Code: String; Params: Variant; Instance: TObject = nil): Variant;
var
  PCode: TCtxScriptCode;
begin
  PCode := TCtxScriptCode.Create;
  try
    PCode.Code := Code;
    Result := Execute(PCode, Params, Instance);
  finally
    PCode.Free;
  end;
end;

function TCtxScript.Evaluate(const Expression: String): Variant;
begin
  Result := Evaluate(Expression, nil);
end;

function TCtxScript.Evaluate(const Expression: String; Instance: TObject): Variant;
var
  PCode: TCtxScriptCode;
begin
  PCode := TCtxScriptCode.Create;
  try
    PCode.Code := Expression;
    PCode.IsExpression := True;
    Result := Execute(PCode, Instance);
  finally
    PCode.Free;
  end;
end;

function TCtxScript.Evaluate(const Expression: String; CodeScope: TCtxScriptCode): Variant;
var
  PCode: TCtxScriptCode;
begin
  PCode := TCtxScriptCode.Create;
  try
    PCode.Code := Expression;
    PCode.IsExpression := True;
    Result := Execute(PCode, CodeScope);
  finally
    PCode.Free;
  end;
end;

function TCtxScript.Execute(PCode: TCtxScriptCode; Instance: TObject = nil): Variant;
begin
  Result := Execute(PCode, Unassigned, Instance);
end;

function TCtxScript.Execute(PCode: TCtxScriptCode; Params: Variant; Instance: TObject = nil): Variant;
begin
  {$IFDEF CTX_TRAL}
  {$I ..\ctxtrial.inc}
  {$ENDIF}
  // Compile PCode
  PCode.Compiled := True;
  if Length(PCode.PCode) = 0 then exit;
  // Load PCode
  FTerminated := False;
  LoadPCode(PCode, Params, Instance);
  try
    // Execute PCode
    ExecutePCode;
  finally
    // Unload PCode
    UnloadPCode;
  end;
  if FTerminated then
    CtxScriptError('Process terminated by user');
  Result := FResult;
end;

function TCtxScript.Execute(PCode: TCtxScriptCode; CodeScope: TCtxScriptCode): Variant;
begin
  PCode.CodeScope := CodeScope;
  Result := Execute(PCode);
end;

procedure TCtxScript.LoadPCode(PCode: TCtxScriptCode; Params: Variant;
  Instance: TObject);
var
  I: Integer;
begin
  // Push current state futher on stack
  if FState.PCode <> nil then
  begin
    if Length(FStateStack) <= FStateStackSize then
      SetLength(FStateStack, FStateStackSize + 5);
    FStateStack[FStateStackSize] := FState;
    Inc(FStateStackSize);

    FState.TraceMode := TraceInto;
  end;
  try
    // Push params on stack
    if not VarIsEmpty(Params) then
    begin
      if not VarIsArray(Params) then
        Push(Params)
      else
        for I := VarArrayLowBound(Params, 1) to VarArrayHighBound(Params, 1) do
          Push(Params[I]);
    end;
    // Prepare program
    FState.PCode := PCode;
    FState.ExecPoint := 0;
    FState.LineNo := 0;
    FState.Instance := Instance;

    // Store the beginning of stack to locate local variables allocated in stack
    FState.StackBegin := FState.SPtr;
    if PCode.CodeScope <> nil then
    begin
      // Find state corresponding to that scope and setup StackBegin
      I := FStateStackSize - 1;
      if I >= 0 then
      repeat
        if FStateStack[I].PCode = PCode.CodeScope then break;
        Dec(I);
      until I < 0;

      if I >= 0 then
      begin
        FState.StackBegin := FStateStack[I].StackBegin;
        FState.Instance := FStateStack[I].Instance;
      end else
        CtxScriptError(SParentProcessNotFound);
    end;
  except
    UnloadPCode;
    raise;
  end;
end;

procedure TCtxScript.UnloadPCode;
var
  CurStack, I: Integer;
begin
  // 1. Restore previous PCode and StackState
  CurStack := SPtr;
  if FStateStackSize > 0 then
  begin
    Dec(FStateStackSize);
    FState := FStateStack[FStateStackSize];
  end else
    ResetScriptState;
  // 2. Restore and clean stack
  if (CurStack >= SPtr) and (SPtr >= -1) then
  begin
    if CurStack > SPtr then
    for I := CurStack downto SPtr + 1 do
      FStackArr[I] := Unassigned;
  end else
    CtxScriptErrorFmt(SIncorrectStackRestoration, [CurStack, SPtr]);
end;

procedure TCtxScript.Resume(TraceMode: Boolean; TraceInto: Boolean);
begin
  if not Suspended then
    CtxScriptError(SUnableToResume);
  FState.TraceMode := TraceMode; // will stop on next line
  FTraceInto := TraceInto;
  FSuspended := False;
end;

procedure TCtxScript.Suspend;
begin
  if Suspended then
    CtxScriptError(SUnableToSusupend);
  FSuspended := True;
  FTerminated := False;
end;

procedure TCtxScript.Terminate;
begin
  if Suspended then
  begin
    FTerminated := True;
    FSuspended := False;
  end;
end;

procedure TCtxScript.Push(Value: Variant);
begin
  Inc(FState.SPtr);
  if FState.SPtr > FStackSize then
  begin
    if FState.SPtr + 32 > MAX_STACK_SIZE then
      CtxScriptError(SStackOverflow);
    SetStackSize(FState.SPtr + 32);
  end;
  FStackArr[FState.SPtr] := Value;
end;

function TCtxScript.Pop: Variant;
begin
  if FState.SPtr >= 0 then
  begin
    Result := FStackArr[FState.SPtr];
    FStackArr[FState.SPtr] := Unassigned; // ???
    Dec(FState.SPtr);
  end else
    CtxScriptError(SStackUnderflow);
end;

function TCtxScript.GetParams(ParamCount: Integer): OleVariant;
var
  I: Integer;
begin
  if ParamCount = 0 then
    Result := Null
  else begin
    Result := VarArrayCreate([0, ParamCount - 1], varVariant);
    for I := 0 to ParamCount - 1 do
      Result[I] := FStackArr[FState.SPtr - ParamCount + 1 + I];
  end;
end;

function TCtxScript.GetParamsRevert(ParamCount: Integer): OleVariant;
var
  I: Integer;
begin
  Result := VarArrayCreate([0, ParamCount - 1], varVariant);
  for I := 0 to ParamCount - 1 do
    Result[I] := FStackArr[FState.SPtr - I];
end;

function TCtxScript.GetParam(ParamCount: Integer): Variant;
begin
  if ((FState.SPtr - ParamCount + 1) >= 0) and (ParamCount > 0) then
    Result := FStackArr[FState.SPtr - ParamCount + 1]
  else Result := NULL;
end;

function VarEqual(const V1, V2: Variant): Boolean;
var
  V1Null, V2Null: Boolean;
begin
  V1Null := FindVarData(V1)^.VType <= varNull;
  V2Null := FindVarData(V2)^.VType <= varNull;
  if V1Null or V2Null then
    Result := V1Null and V2Null
  else Result := V1 = V2;
end;

procedure TCtxScript.ExecutePCode;
var
  PCodeRec: ^TCtxPCodeRec;
  RestoreSPtr, I: Integer;
  Scope: Variant;
  Disp: IDispatch;

  procedure RestoreStack;
  begin
    while SPtr > RestoreSPtr do
    begin
      FStackArr[FState.SPtr] := Unassigned;
      Dec(FState.SPtr);
    end;
    if SPtr <> RestoreSPtr then
      CtxScriptErrorFmt(SIncorrectStackRestoration, [RestoreSPtr, SPtr]);
  end;

  procedure DoDebugHook(Exception: Boolean);
  begin
    if not Suspended and Assigned(FOnDebugHook)
      and ((cdoStopOnBreakpoint in DebugOptions) or
        (Exception and (cdoStopOnException in DebugOptions)))
    then begin
      FOnDebugHook(Self, Exception);
      FSuspended := False;
    end;
  end;

  procedure GetLocVar(ID: Integer);
  begin
    if StackBegin + ID < -1 then CtxScriptError(SStackUnderflow);
    Result := FStackArr[StackBegin + ID + 1];
  end;

  procedure SetLocVar(ID: Integer; AValue: Variant);
  begin
    if StackBegin + ID < -1 then CtxScriptError(SStackUnderflow);
    FStackArr[StackBegin + ID + 1] := AValue;
  end;

  procedure GetLocVarByRef(ID: Integer);
  begin
    if StackBegin + ID < -1 then CtxScriptError(SStackUnderflow);
    Result := GetVarByRef(FStackArr[StackBegin + ID + 1]);
  end;

  procedure SetLocVarByRef(ID: Integer; AValue: Variant);
  begin
    if StackBegin + ID < -1 then CtxScriptError(SStackUnderflow);
    SetVarByRef(FStackArr[StackBegin + ID + 1], AValue);
  end;

  procedure GetArrayAt(ArrVar: Variant);
  var
    Obj: TObject;
  begin
    if VarIsArray(ArrVar) then
      Result := ArrVar[Pop]
    else if (VarType(ArrVar) = varString) or (VarType(ArrVar) = varOleStr) then
      Result := VarToStr(ArrVar)[Integer(Pop)]
    else if VarIsObject(ArrVar, Obj) then
    begin
      if not InvokeObjectMethod(Self, citGetArrayProp, Obj,
        DefArrayPropName, 1)
      then CtxScriptError(SVariantIsNotAnArray);
      Pop; // remove array index from stack
    end else
      CtxScriptError(SVariantIsNotAnArray);
  end;

  procedure SetArrayAt(var ArrVar: Variant; Value: Variant);
  var
    Obj: TObject;
  begin
    if VarIsArray(ArrVar) then
      ArrVar[Pop] := Value
    else if (VarType(ArrVar) = varString) or (VarType(ArrVar) = varOleStr) then
      String(TVarData(ArrVar).VString)[Integer(Pop)] := StrToChar(VarToStr(Value))
    else if VarIsObject(ArrVar, Obj) then
    begin
      if not InvokeObjectMethod(Self, citSetArrayProp, Obj,
        DefArrayPropName, 1)
      then CtxScriptError(SVariantIsNotAnArray);
      Pop; // remove array index from stack
    end else
      CtxScriptError(SVariantIsNotAnArray);
  end;

  procedure GetLocVarArray(ID, ParCount: Integer);
  begin
    if StackBegin + ID < -1 then CtxScriptError(SStackUnderflow);
    if ParCount <> 1 then CtxScriptError(SNoMultiDimensionalArray);
    GetArrayAt(FStackArr[StackBegin + ID + 1]);
  end;

  procedure GetLocVarArrayByRef(ID, ParCount: Integer);
  begin
    if StackBegin + ID < -1 then CtxScriptError(SStackUnderflow);
    if ParCount <> 1 then CtxScriptError(SNoMultiDimensionalArray);
    GetArrayAt(GetVarByRef(FStackArr[StackBegin + ID + 1]));
  end;

  procedure SetLocVarArray(ID, ParCount: Integer; AValue: Variant);
  begin
    if StackBegin + ID < -1 then CtxScriptError(SStackUnderflow);
    if ParCount <> 1 then CtxScriptError(SNoMultiDimensionalArray);
    SetArrayAt(FStackArr[StackBegin + ID + 1], AValue);
  end;

  procedure SetLocVarArrayByRef(ID, ParCount: Integer; AValue: Variant);
  var
    Temp: Variant;
  begin
    if StackBegin + ID < -1 then CtxScriptError(SStackUnderflow);
    if ParCount <> 1 then CtxScriptError(SNoMultiDimensionalArray);
    Temp := GetVarByRef(FStackArr[StackBegin + ID + 1]);
    SetArrayAt(Temp, AValue);
  end;

  function InvokeObject(Obj: TObject; InvokeType: TCtxInvokeType): Boolean;
  begin
    with PCodeRec^ do
    begin
      if Obj = nil then
        CtxScriptErrorFmt(SAccessViolationNilObject, [SValue]);
(*
      if (Introspector = nil) or (Instance <> Obj) then
      begin
        // [thread safety]
        // This block is not thread safe, because it assigns to PCodeRec which can be!
        // invoked from different instances of TCtxScript running in different threads!
        // In this case assigning Instance and Introspector makes little sence
        // because instance will most likely be different all the time.
        // We can theoretically disregard Instance and once introspector is assigned
        // always assume that it is the same.
        Introspector := ResolveObjectName(Self, Obj, SValue, Index);
        if Introspector = nil then
        begin
          Result := False;
          exit;
        end;
        Instance := Obj;
        // [/thread safety]
      end;
      Result := Introspector.Invoke(Self, InvokeType, Instance,
        SValue, Index, ParCount);
*)
      // Thread-safe version of this code
      if (Introspector = nil)
        or ((Obj = FState.Instance) or (Obj = FGlobalScope) or (Obj = CtxSystemScope)) // Always loop up introspector for scope objects
      then
      begin
        Introspector := ResolveObjectName(Self, Obj, SValue, Index);
        if Introspector = nil then
        begin
          Result := False;
          exit;
        end;
      end;
      Result := Introspector.Invoke(Self, InvokeType, Obj,
        SValue, Index, ParCount);
    end;
  end;

  procedure ProcessScope(InvokeType: TCtxInvokeType);
  var
    Obj: TObject;
  begin
    with PCodeRec^ do
    if VarIsObject(Scope, Obj) then begin
      if not InvokeObject(Obj, InvokeType) then
        CtxScriptErrorFmt(SMethodNotFound, [PCodeRec^.SValue]);
    end else if VarIsDispatch(Scope, Disp) then begin
      if InvokeType = citGetMethodOrProp then
      begin
        if ParCount = 0 then
          FResult := GetDispatchPropValue(Disp, SValue)
        else FResult := VarDispatchInvoke(Disp, SValue, ParCount,
          @FStackArr[SPtr - ParCount + 1]);
      end else if InvokeType = citSetProp then
        SetDispatchPropValue(Disp, SValue, Result)
      else
        CtxScriptErrorFmt(SMethodNotFound, [SValue]);
    end else
      CtxScriptErrorFmt(SMethodNotFound, [SValue]);
  end;

  procedure ProcessObj(InvokeType: TCtxInvokeType);
  begin
    RestoreSPtr := SPtr - PCodeRec^.ParCount - 1 {Self};
    try
      Scope := GetParam(PCodeRec^.ParCount + 1);
      ProcessScope(InvokeType);
    finally
      RestoreStack;
    end;
  end;

  procedure Process(InvokeType: TCtxInvokeType);
  begin
    RestoreSPtr := SPtr - PCodeRec^.ParCount;
    with PCodeRec^ do
    try
      if (Introspector <> nil) and (Instance <> nil) and (
        (Instance = FState.Instance) or
        (Instance = FGlobalScope) or
        (Instance = CtxSystemScope)) then
      begin
        if InvokeObject(Instance, InvokeType) then
          exit;
      end else begin
        if FState.Instance <> nil then
          if InvokeObject(FState.Instance, InvokeType) then exit;
        if FGlobalScope <> nil then
          if InvokeObject(FGlobalScope, InvokeType) then exit;
        if CtxSystemScope <> nil then
          if InvokeObject(CtxSystemScope, InvokeType) then exit;
      end;
      CtxScriptErrorFmt(SMethodNotFound, [PCodeRec^.SValue]);
    finally
      RestoreStack;
    end;
  end;

begin
  while not Terminated do
  try
    PCodeRec := @PCode.PCode[FState.ExecPoint];

    // Breakpoint
    if FState.LineNo <> PCodeRec^.LineNo then
    begin
      FState.LineNo := PCodeRec^.LineNo;
      DoDebugHook(False);
    end;

    case PCodeRec^.OpCode of
      CSOP_NOP: ; // NOP
      CSOP_BREAKPOINT: ; // not used

      CSOP_PLUS: Result := Pop + Result;
      CSOP_MINUS: Result := Pop - Result;
      CSOP_UMINUS: Result := - Result;
      CSOP_MUL: Result := Pop * Result;
      CSOP_DIV: Result := Pop / Result;
      CSOP_IDIV: Result := Pop div Result;
      CSOP_MOD: Result := Pop mod Result;

      CSOP_GREATERTHEN: Result := Pop > Result;
      CSOP_LESSTHEN: Result := Pop < Result;
      CSOP_GREATEROREQUAL: Result := Pop >= Result;
      CSOP_LESSOREQUAL: Result := Pop <= Result;

      CSOP_EQUAL: Result := VarEqual(Pop, Result);
      CSOP_NOTEQUAL: Result := not VarEqual(Pop, Result);
      CSOP_NOT: Result := not Result;
      CSOP_AND: Result := Pop and Result;
      CSOP_OR: Result := Pop or Result;
      CSOP_XOR: Result := Pop xor Result;

      { Consts }
      CSOP_SCONST: Result := PCodeRec^.SValue;
      CSOP_ICONST: Result := PCodeRec^.IValue;
      CSOP_DCONST: Result := PCodeRec^.DValue;
      CSOP_BCONST: Result := PCodeRec^.BValue;
      CSOP_NULL:   Result := NULL;

      { Getters: local, competely resolved }
      CSOP_GETVAR:
              GetLocVar(PCodeRec^.IValue);
      CSOP_GETVARBYREF:
              GetLocVarByRef(PCodeRec^.IValue);
      CSOP_GETARRAY:
              GetLocVarArray(PCodeRec^.IValue, PCodeRec^.ParCount);
      CSOP_GETARRAYBYREF:
              GetLocVarArrayByRef(PCodeRec^.IValue, PCodeRec^.ParCount);
      CSOP_GETVARREF:
              Result := VarRef(@FStackArr[StackBegin + PCodeRec^.IValue + 1]);

      { Getters: external, unresolved }
      CSOP_METHODCALL:      Process(citGetMethodOrProp);
      CSOP_ARRAYCALL:       Process(citGetArrayProp);
      CSOP_OBJMETHODCALL:   ProcessObj(citGetMethodOrProp);
      CSOP_OBJARRAYCALL:    ProcessObj(citGetArrayProp);

      { Setters: local, completely resolved }
      CSOP_SETVAR:
              SetLocVar(PCodeRec^.IValue, Result);
      CSOP_SETVARBYREF:
              SetLocVarByRef(PCodeRec^.IValue, Result);
      CSOP_SETARRAY:
              SetLocVarArray(PCodeRec^.IValue, PCodeRec^.ParCount, Result);
      CSOP_SETARRAYBYREF:
              SetLocVarArrayByRef(PCodeRec^.IValue, PCodeRec^.ParCount, Result);

      { Setters: external, unresolved }
      CSOP_SETPROP:         Process(citSetProp);
      CSOP_SETARRAYPROP:    Process(citSetArrayProp);
      CSOP_SETOBJPROP:      ProcessObj(citSetProp);
      CSOP_SETOBJARRAYPROP: ProcessObj(citSetArrayProp);

      { Loops }
      CSOP_INCREMENT: begin
              GetLocVar(PCodeRec^.IValue);
              SetLocVar(PCodeRec^.IValue, Result + 1);
            end;
      CSOP_DECREMENT: begin
              GetLocVar(PCodeRec^.IValue);
              SetLocVar(PCodeRec^.IValue, Result - 1);
            end;

      { Stack }
      CSOP_POP:       Result := Pop;
      CSOP_PUSH:      Push(Result);

      { Jumps }

      CSOP_JUMP:
              FState.ExecPoint := PCodeRec^.IValue - 1;
      CSOP_JUMPIFFALSE:
              if not Result then FState.ExecPoint := PCodeRec^.IValue - 1;
      CSOP_JUMPIFTRUE:
              if Result then FState.ExecPoint := PCodeRec^.IValue - 1;
      { Exceptions }
      CSOP_TRY: begin
              Push(PCodeRec^.IValue);
              Push(FState.ExceptSPtr);
              FState.ExceptSPtr := FState.SPtr;
            end;
      CSOP_ENDTRY: begin
              FState.ExceptSPtr := Pop;
              Pop;
            end;
      CSOP_RERAISE:
              if not VarIsEmpty(FState.ExceptValue) then
                raise EVariantException.Create(FState.ExceptValue);
      CSOP_RAISE:
              raise EVariantException.Create(Result);
      CSOP_CLEAREXCEPTION:
              FState.ExceptValue := Unassigned;
      CSOP_EXCEPTION:
              Result := FState.ExceptValue;
      CSOP_BEGIN: begin
              // !!!ASSERTION: FStackBegin == FSPtr !!!
              FState.StackBegin := FState.SPtr - PCodeRec^.IValue; // reserve place for input parameters
              FState.SPtr := FState.StackBegin + PCodeRec^.ParCount; // reserve place for variables
              // Initialize consts
              for I := 0 to PCode.Symbols.Count - 1 do
              with PCode.Symbols[I] do
                if SymbolType = stConst then
                  SetLocVar(I, Value)
                else if SymbolType = stOutParam then
                  SetLocVarByRef(I, NULL);
            end;
      CSOP_EXIT: exit;
      else
        CtxScriptError(SUnsupportedInstruction);
    end;
    Inc(FState.ExecPoint);
  except
    on E: Exception do
    begin
      if FState.ExceptSPtr >= 0 then
      begin
        if E is EVariantException then
          FState.ExceptValue := EVariantException(E).Value
        else FState.ExceptValue := E.Message;

        DoDebugHook(True);

        FState.SPtr := FState.ExceptSPtr;
        FState.ExceptSPtr := Pop;
        FState.ExecPoint := Pop;
      end else
        raise;
    end;
  end;
end;

function TCtxScript.GetScriptState(Idx: Integer): TCtxScriptState;
begin
  Result := FStateStack[Idx];
end;

{ TCtxIntrospector }

constructor TCtxIntrospector.Create(AClass: TClass);
begin
  inherited Create;
  FClass := AClass;
  RegisterIntrospector(Self);
end;

destructor TCtxIntrospector.Destroy;
begin
  if not FinalizingIntrospectors then
    UnRegisterIntrospector(Self);
  inherited Destroy;
end;

function TCtxIntrospector.InvokeDefaultArrayProp(CtxScript: TCtxScript;
  InvokeType: TCtxInvokeType; Scope: TObject; const Name: String;
  var Index: Integer; ParCount: Integer): Boolean;
var
  Value: Variant;
  Obj: TObject;
begin
  // After call to Invoke CtxScript.Result contains object,
  // that have default array property: VarArray, String or varObject.
  // We need to invoke it.
  Result := True;
  // Save result
  Value := CtxScript.Result;
  if not Invoke(CtxScript, citGetMethodOrProp, Scope, Name, Index, 0) then
    CtxScriptErrorFmt(SPropertyNotAnArray, [_Class.ClassName, Name]);

  if VarIsObject(CtxScript.Result, Obj) then
  begin
    CtxScript.Result := Value;
    if not InvokeObjectMethod(CtxScript, InvokeType, Obj,
      DefArrayPropName, ParCount)
    then
      CtxScriptErrorFmt(SPropertyNotAnArray, [_Class.ClassName, Name]);
  end else
    if InvokeType = citGetArrayProp then
    with CtxScript do
    begin
      if VarIsArray(Result) then
        Result := Result[GetParam(1)]
      else if (VarType(Result) = varString) or (VarType(Result) = varOleStr) then
        Result := VarToStr(Result)[Integer(GetParam(1))]
      else CtxScriptErrorFmt(SPropertyNotAnArray, [_Class.ClassName, Name]);
    end else
      CtxScriptErrorFmt(SPropertyNotAnArray, [_Class.ClassName, Name]);
end;

procedure TCtxIntrospector.ForEachName(Scope: TObject; OnName: TCtxNameCallback;
  Data: Pointer; InvokeTypes: TCtxInvokeTypes = citAll);
begin
  // Implement in descendants. Invoke OnName callback for each name in context
end;

procedure _GetNames(Sender: TCtxIntrospector; Instance: TObject;
    InvokeType: TCtxInvokeType; const Name: String; ParCount: Integer; Data: Pointer);
begin
  TStrings(Data).Add(Name);
end;

procedure TCtxIntrospector.GetNames(Scope: TObject; List: TStrings; InvokeTypes: TCtxInvokeTypes = citAll);
begin
  ForEachName(Scope, _GetNames, List, InvokeTypes);
end;

{ TCtxMethodFieldInfo }

constructor TCtxMethodFieldInfo.Create(const Name: String;
  Method: TCtxMethod; ParCount: Integer = -1);
begin
  inherited Create;
  FFieldName := Name;
  FMethod := Method;
  FParCount := ParCount;
end;

procedure TCtxMethodFieldInfo.Invoke(CtxScript: TCtxScript; InvokeType: TCtxInvokeType;
  Scope: TObject; ParCount: Integer);
begin
  if (InvokeType = citGetMethodOrProp) and ((FParCount < 0) or (ParCount = FParCount)) then
    FMethod(CtxScript, InvokeType, Scope, ParCount)
  else CtxScriptErrorFmt(SInvalidInvokeType, [FieldName]);
end;

function TCtxMethodFieldInfo.GetFieldType: TCtxFieldType;
begin
  Result := cftMethod;
end;

{ TCtxPropFieldInfo }
constructor TCtxPropFieldInfo.Create(const Name: String; GetMethod,
  SetMethod: TCtxMethod);
begin
  inherited Create;
  FFieldName := Name;
  FGetMethod := GetMethod;
  FSetMethod := SetMethod;
end;

function TCtxPropFieldInfo.GetFieldType: TCtxFieldType;
begin
  Result := cftProperty;
end;

procedure TCtxPropFieldInfo.Invoke(CtxScript: TCtxScript;
  InvokeType: TCtxInvokeType; Scope: TObject; ParCount: Integer);
begin
  CheckParams(ParCount, 0);
  if InvokeType = citGetMethodOrProp then
    FGetMethod(CtxScript, InvokeType, Scope, ParCount)
  else if (InvokeType = citSetProp) and Assigned(FSetMethod) then
    FSetMethod(CtxScript, InvokeType, Scope, ParCount)
  else CtxScriptErrorFmt(SInvalidInvokeType, [FieldName]);
end;

{ TCtxArrayPropFieldInfo }

constructor TCtxArrayPropFieldInfo.Create(const Name: String; GetMethod,
  SetMethod: TCtxMethod; ParCount: Integer);
begin
  FFieldName := Name;
  FGetMethod := GetMethod;
  FSetMethod := SetMethod;
  FParCount := ParCount;
end;

function TCtxArrayPropFieldInfo.GetFieldType: TCtxFieldType;
begin
  Result := cftArrayProperty;
end;

procedure TCtxArrayPropFieldInfo.Invoke(CtxScript: TCtxScript;
  InvokeType: TCtxInvokeType; Scope: TObject; ParCount: Integer);
begin
  CheckParams(ParCount, FParCount);
  if InvokeType = citGetArrayProp then
    FGetMethod(CtxScript, InvokeType, Scope, ParCount)
  else if (InvokeType = citSetArrayProp) and Assigned(FSetMethod) then
    FSetMethod(CtxScript, InvokeType, Scope, ParCount)
  else CtxScriptErrorFmt(SInvalidInvokeType, [FieldName]);
end;

{ TCtxConstFieldInfo }
constructor TCtxConstFieldInfo.Create(const Name: String; ConstValue: Variant);
begin
  inherited Create;
  FFieldName := Name;
  FValue := ConstValue;
end;

procedure TCtxConstFieldInfo.Invoke(CtxScript: TCtxScript; InvokeType: TCtxInvokeType;
  Scope: TObject; ParCount: Integer);
begin
  if (InvokeType = citGetMethodOrProp) and (ParCount = 0) then
    CtxScript.Result := FValue
  else CtxScriptErrorFmt(SInvalidInvokeType, [FieldName]);
end;

function TCtxConstFieldInfo.GetFieldType: TCtxFieldType;
begin
  Result := cftConst;
end;

{ TCtxCustomIntrospector }

constructor TCtxCustomIntrospector.Create(AClass: TClass);
begin
  inherited Create(AClass);
  FFields := TStringList.Create;
  FFields.Sorted := True;
  FFields.Duplicates := dupError;
end;

destructor TCtxCustomIntrospector.Destroy;
var
  I: Integer;
begin
  for I := 0 to FFields.Count - 1 do
    FFields.Objects[I].Free;
  FreeAndNil(FFields);
  inherited Destroy;
end;

procedure TCtxCustomIntrospector.AddFieldInfo(FieldInfo: TCtxFieldInfo);
var
  Idx: Integer;
  TempName: String;
begin
  TempName := UpperCase(FieldInfo.FieldName);
  Idx := FFields.IndexOf(TempName);
  if Idx < 0 then
    FFields.AddObject(TempName, FieldInfo)
  else begin
    Fields[Idx].Free;
    FFields.Objects[Idx] := FieldInfo;
  end;
end;

procedure TCtxCustomIntrospector.AddMethod(const Name: String;
   Method: TCtxMethod; ParCount: Integer = -1);
begin
  AddFieldInfo(TCtxMethodFieldInfo.Create(Name, Method, ParCount));
end;

procedure TCtxCustomIntrospector.AddProp(const Name: String; GetMethod: TCtxMethod;
      SetMethod: TCtxMethod = nil);
begin
  AddFieldInfo(TCtxPropFieldInfo.Create(Name, GetMethod, SetMethod));
end;

procedure TCtxCustomIntrospector.AddArrayProp(const Name: String;
  GetMethod, SetMethod: TCtxMethod; ParCount: Integer);
begin
  AddFieldInfo(TCtxArrayPropFieldInfo.Create(Name, GetMethod, SetMethod, ParCount));
end;

procedure TCtxCustomIntrospector.AddConst(const Name: String;
  Value: Variant);
begin
  AddFieldInfo(TCtxConstFieldInfo.Create(Name, Value));
end;

function TCtxCustomIntrospector.FindField(const Name: String): Integer;
begin
  Result := FFields.IndexOf(UpperCase(Name));
end;

function TCtxCustomIntrospector.GetFields(
  Idx: Integer): TCtxFieldInfo;
begin
  Result := TCtxFieldInfo(FFields.Objects[Idx]);
end;

function TCtxCustomIntrospector.ResolveName(CtxScript: TCtxScript;
  Scope: TObject; const Name: String; var Index: Integer): Boolean;
var
  Idx: Integer;
begin
  Idx := FindField(Name);
  Result := Idx >= 0;
  if Result then Index := Idx + 1;
end;

function TCtxCustomIntrospector.Invoke(CtxScript: TCtxScript;
  InvokeType: TCtxInvokeType; Scope: TObject;
  const Name: String; var Index: Integer; ParCount: Integer): Boolean;
var
  FieldInfo: TCtxFieldInfo;
begin
  Result := (Index > 0) and (Index <= FFields.Count)
    and AnsiSameText(FFields[Index - 1], Name);
  if not Result then
    Result := ResolveName(CtxScript, Scope, Name, Index);
  if Result then
  begin
    FieldInfo := Fields[Index - 1];
    if (InvokeType in [citGetArrayProp, citSetArrayProp])
      and (FieldInfo.GetFieldType <> cftArrayProperty)
    then
      InvokeDefaultArrayProp(CtxScript, InvokeType, Scope, Name, Index, ParCount)
    else
      FieldInfo.Invoke(CtxScript, InvokeType, Scope, ParCount);
  end;
end;

procedure TCtxCustomIntrospector.AddDefaultArrayProp(
  GetMethod, SetMethod: TCtxMethod; ParCount: Integer);
begin
  AddArrayProp(DefArrayPropName, GetMethod, SetMethod, ParCount);
end;

procedure TCtxCustomIntrospector.ForEachName(Scope: TObject;
  OnName: TCtxNameCallback; Data: Pointer; InvokeTypes: TCtxInvokeTypes);
var
  I: Integer;
  FieldInfo: TCtxFieldInfo;
begin
  for I := 0 to FFields.Count - 1 do
  begin
    FieldInfo := FFields.Objects[I] as TCtxFieldInfo;
    if FieldInfo.GetFieldType = cftMethod then
    begin
      if citGetMethodOrProp in InvokeTypes then
        OnName(Self, Scope, citGetMethodOrProp, FieldInfo.FieldName, TCtxMethodFieldInfo(FieldInfo).FParCount, Data);
    end else if FieldInfo.GetFieldType = cftProperty then
    begin
      if (citGetMethodOrProp in InvokeTypes) or
        (Assigned(TCtxPropFieldInfo(FieldInfo).FSetMethod) and (citSetProp in InvokeTypes))
      then
        OnName(Self, Scope, citGetMethodOrProp, FieldInfo.FieldName, 0, Data);
    end else if FieldInfo.GetFieldType = cftArrayProperty then
    begin
      if (citGetArrayProp in InvokeTypes) or
        (Assigned(TCtxPropFieldInfo(FieldInfo).FSetMethod) and (citSetArrayProp in InvokeTypes))
      then
        OnName(Self, Scope, citGetMethodOrProp, FieldInfo.FieldName, 0, Data);
    end else if FieldInfo.GetFieldType = cftConst then
    begin
      if (citGetMethodOrProp in InvokeTypes) then
        OnName(Self, Scope, citGetMethodOrProp, FieldInfo.FieldName, 0, Data);
    end;
  end;
end;

{ TCtxObjectIntrospector }

function TCtxObjectIntrospector.ResolveName(CtxScript: TCtxScript;
  Scope: TObject; const Name: String; var Index: Integer): Boolean;
var
  Field: Pointer;
begin
  Field := Scope.FieldAddress(Name);
  Result := Field <> nil;
  if Result then Index := Integer(Field);
end;

function TCtxObjectIntrospector.Invoke(CtxScript: TCtxScript;
  InvokeType: TCtxInvokeType; Scope: TObject; const Name: String; var Index: Integer;
  ParCount: Integer): Boolean;
var
  Field: ^TObject;
begin
  Result := Index <> 0;
  if not Result then
    Result := ResolveName(CtxScript, Scope, Name, Index);
  if Result then
  case InvokeType of
    citGetMethodOrProp: begin
      Field := Pointer(Index);
      CtxScript.Result := VarFromObject(Field^);
    end;
    citSetProp: begin
      Field := Pointer(Index);
      Field^ := VarToObject(CtxScript.Result);
    end;
    citGetArrayProp, citSetArrayProp:
      InvokeDefaultArrayProp(CtxScript, InvokeType, Scope, Name, Index, ParCount);
    else
      CtxScriptErrorFmt(SInvalidInvokeType, [Name]);
  end;
end;

{ TCtxPersistentIntrospector }

function TCtxPersistentIntrospector.ResolveName(CtxScript: TCtxScript;
  Scope: TObject; const Name: String; var Index: Integer): Boolean;
begin
  Index := Integer(GetPropInfo(Scope, Name));
  Result := Index <> 0;
end;

function TCtxPersistentIntrospector.Invoke(CtxScript: TCtxScript;
  InvokeType: TCtxInvokeType; Scope: TObject; const Name: String; var Index: Integer; ParCount: Integer): Boolean;
begin
  Result := Index <> 0;
  if not Result then
    Result := ResolveName(CtxScript, Scope, Name, Index);
  if Result then
  case InvokeType of
    citGetMethodOrProp:
      CtxScript.Result := GetObjectProperty(Scope, PPropInfo(Index));
    citSetProp:
      SetObjectProperty(Scope, PPropInfo(Index), CtxScript.Result);
    citGetArrayProp, citSetArrayProp:
      InvokeDefaultArrayProp(CtxScript, InvokeType, Scope, Name, Index, ParCount);
    else CtxScriptErrorFmt(SInvalidInvokeType, [Name]);
  end;
end;

procedure TCtxPersistentIntrospector.ForEachName(Scope: TObject;
  OnName: TCtxNameCallback; Data: Pointer; InvokeTypes: TCtxInvokeTypes);
const
  tkSimpleProperties = [tkInteger, tkChar, tkEnumeration, tkFloat,
    tkString, tkSet, tkWChar, tkLString, tkWString, tkVariant, tkInt64];
var
  Cnt, I: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
begin
  if Scope = nil then exit;
  Cnt := GetPropList(Scope, PropList);
  try
    for I := 0 to Cnt - 1 do
    begin
      PropInfo := PropList^[I];
      if (PropInfo^.PropType^.Kind in tkSimpleProperties) then
        if (citGetMethodOrProp in InvokeTypes) or (citSetProp in InvokeTypes) then
          OnName(Self, Scope, citGetMethodOrProp, String(PropInfo^.Name), 0, Data);
    end;
  finally
    FreeMem(PropList);
  end;
end;

{ TCtxComponentIntrospector }

function TCtxComponentIntrospector.ResolveName(CtxScript: TCtxScript;
  Scope: TObject; const Name: String; var Index: Integer): Boolean;
var
  Comp: TComponent;
begin
  Comp := TComponent(Scope).FindComponent(Name);
  Result := Comp <> nil;
  if Result then Index := Integer(Comp);
end;

function TCtxComponentIntrospector.Invoke(CtxScript: TCtxScript;
  InvokeType: TCtxInvokeType; Scope: TObject; const Name: String;
  var Index: Integer; ParCount: Integer): Boolean;
begin
  Result := Index <> 0;
  if not Result then
    Result := ResolveName(CtxScript, Scope, Name, Index);
  if Result then
  case InvokeType of
    citGetMethodOrProp:
      CtxScript.Result := VarFromObject(TComponent(Index));
    citGetArrayProp, citSetArrayProp:
      InvokeDefaultArrayProp(CtxScript, InvokeType, Scope, Name, Index, ParCount);
    else CtxScriptErrorFmt(SInvalidInvokeType, [Name]);
  end;
end;

procedure TCtxComponentIntrospector.ForEachName(Scope: TObject;
  OnName: TCtxNameCallback; Data: Pointer; InvokeTypes: TCtxInvokeTypes);
var
  I: Integer;
begin
  if Scope = nil then exit;
  for I := 0 to TComponent(Scope).ComponentCount - 1 do
    if citGetMethodOrProp in InvokeTypes then
      OnName(Self, Scope, citGetMethodOrProp, TComponent(Scope).Components[I].Name, 0, Data);
end;

{ TCtxObjectIterator }

procedure TCtxObjectIterator.PushState;
begin
  // Save current bookmark
end;

procedure TCtxObjectIterator.PopState;
begin
  // Restore to last saved bookmark
end;

function TCtxObjectIterator.Count: Integer;
begin
  Result := -1; { Negative value means, that count is not supported }
end;

{ General Routines }

function IsReserved(ReservedWords: array of TReservedWord; const Str: String): Integer;
var I: Integer;
begin
  for I := Low(ReservedWords) to High(ReservedWords) do
    if ReservedWords[I].Name = Str then
    begin
      Result := ReservedWords[I].TokenType;
      exit;
    end;
  Result := tokenUnknown;
end;

function StrToDouble(const Value: String): Double;
var OldDS: Char;
begin
  OldDS := {$IFDEF DXE2_ORLATER}FormatSettings.{$ENDIF}DecimalSeparator;
  try
    {$IFDEF DXE2_ORLATER}FormatSettings.{$ENDIF}DecimalSeparator := '.';
    Result := StrToFloat(Value);
  finally
    {$IFDEF DXE2_ORLATER}FormatSettings.{$ENDIF}DecimalSeparator := OldDS;
  end;
end;

{ TSymbol }

function TSymbol.Clone: TSymbol;
begin
  Result := TSymbol.Create;
  Result.SymbolType := SymbolType;
  Result.SymbolName := SymbolName;
  Result.SymbolVarTypeName := SymbolVarTypeName;
  Result.Value := Value;
end;

{  TSymbols = class (TObjectList) }

procedure TSymbols.AddSymbols(Symbols: TSymbols);
var
  I: Integer;
begin
  if Symbols <> nil then
    for I := 0 to Symbols.Count - 1 do
      Add(Symbols[I].Clone);
end;

function TSymbols.FindSymbol(const SymName: String; SymTypes: TSymbolTypes): Integer;
var I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  with TSymbol(Items[I]) do
    if SameText(SymbolName, SymName) and (SymbolType in SymTypes) then
    begin
      Result := I;
      exit;
    end;
end;

function TSymbols.DeclareSymbol(const SymName: String; SymType:
  TSymbolType; Value: Variant): Integer;
var
  Symbol: TSymbol;
begin
  // Add to the list
  Symbol := TSymbol.Create;
  Symbol.SymbolType := SymType;
  Symbol.SymbolName := SymName;
  Symbol.Value := Value;
  Result := Add(Symbol);
end;

function TSymbols.GetItem(Idx: Integer): TSymbol;
begin
  Result := TSymbol(inherited Items[Idx]);
end;

procedure TSymbols.SetItem(Idx: Integer; const Value: TSymbol);
begin
  inherited Items[Idx] := Value;
end;

{ TCtxCompiler }

constructor TCtxCompiler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetLength(FNextToken, 255);
  FNextToken := '';
  FNextTokenType := 0;
  FNextChar := #0;
  FPCode := nil;
  FCodePos := 1;
  FPCodePos := 1;
  FLineNo := 0;
  FLinePos := 1;
  FPrevLine := 1;
  FPrevPos := 1;
  FReturnResult := False;
end;

destructor TCtxCompiler.Destroy;
begin
  UnRegisterCtxCompiler(Self);
  inherited;
end;

procedure TCtxCompiler.Initialize;
begin
  FCodePos := 1;
  FLineNo := 1;
  FLinePos := 1;
  FPrevLine := 1;
  FPrevPos := 1;
  FPCodePos := -1;
  FComments := '';
  FNextToken := '';
  FEndOfStatement := False;
  FReturnResult := False;
  FLoopLevel := 0;
  FStatementLineNo := 0;
  FLastBreakPointLineNo := 0;
  GetNextChar;
  GetNextToken;
end;

procedure TCtxCompiler.ParseHeader(PCode: TCtxScriptCode);
begin
  if PCode.IsExpression then exit;
  FPCode := PCode;
  Initialize;
  ScriptHeader;
end;

procedure TCtxCompiler.Compile(PCode: TCtxScriptCode);
begin
  FPCode := PCode;
  Initialize;
  SetLength(FPCode.PCode, Length(FPCode.Code) div 5 + 1);
  try
    if PCode.IsExpression then
    begin
      Expression;
      Emit(CSOP_EXIT); // Result is on stack
    end else begin
      // Process main block, allow local labels
      InitializeLabels;
      try
        ScriptHeader;
        Script;
        FixupLabels;
        if FLabelEntries.Count > 0 then
          ParseError(CSCE_LABELNOTFOUND, FLabelEntries[0]);
      finally
        FinalizeLabels;
      end;
    end;
    SetLength(FPCode.PCode, FPCodePos + 1);
  except
    SetLength(FPCode.PCode, 0);
    raise;
  end;
end;

procedure TCtxCompiler.ParseError(ErrorCode: Integer; Context: String = '');
var
  Msg: String;
begin
  if Assigned(FOnGetErrorMessage) then
    Msg := FOnGetErrorMessage(ErrorCode)
  else Msg := GetDefaultErrorMessage(ErrorCode);
    raise ECtxScriptCompileError.Create(Msg, ErrorCode, FPrevLine,
    FPrevPos, FPCode.ScriptName, Context);
end;

function TCtxCompiler.GetDefaultErrorMessage(ErrorCode: Integer): String;
begin
  // Implement in descendants
  Result := 'Error ' + IntToStr(ErrorCode);
end;

function TCtxCompiler.Emit(PSI: TCtxScriptInstruction): Integer;
begin
  if FPCodePos >= Length(FPCode.PCode) - 2 then
    SetLength(FPCode.PCode, FPCodePos + 10);

  Inc(FPCodePos);

  (*
  if FEndOfStatement {and (FLastBreakPointLineNo < FStatementLineNo)} then
  begin
    FEndOfStatement := False;
    FPCode.PCode[FPCodePos].OpCode := CSOP_BREAKPOINT;
    FPCode.PCode[FPCodePos].IValue := FStatementLineNo;
    FLastBreakPointLineNo := FStatementLineNo;
    Inc(FPCodePos);
  end;
  *)

  FPCode.PCode[FPCodePos].OpCode := PSI;
  FPCode.PCode[FPCodePos].LineNo := FStatementLineNo;

  Result := FPCodePos;
end;

function TCtxCompiler.Emit(PSI: TCtxScriptInstruction; SValue: String;
  ParCount: Integer = 0): Integer;
begin
  Result := Emit(PSI);
  FPCode.PCode[FPCodePos].SValue := SValue;
  FPCode.PCode[FPCodePos].ParCount := ParCount;
end;

function TCtxCompiler.Emit(PSI: TCtxScriptInstruction; IValue: Integer;
  ParCount: Integer = 0): Integer;
begin
  Result := Emit(PSI);
  FPCode.PCode[FPCodePos].IValue := IValue;
  FPCode.PCode[FPCodePos].ParCount := ParCount;
end;

function TCtxCompiler.Emit(PSI: TCtxScriptInstruction; DValue: Double): Integer;
begin
  Result := Emit(PSI);
  FPCode.PCode[FPCodePos].DValue := DValue;
end;

function TCtxCompiler.Emit(PSI: TCtxScriptInstruction; BValue: Boolean): Integer;
begin
  Result := Emit(PSI);
  FPCode.PCode[FPCodePos].BValue := BValue;
end;

procedure TCtxCompiler.EmitBreakPoint;
begin
  FStatementLineNo := FLineNo;
  FEndOfStatement := True;
end;

procedure TCtxCompiler.AddLabel(const Id: String; JumpPos: Integer);
begin
  FLabelEntries.AddObject(UpperCase(Id), TObject(JumpPos));
end;

procedure TCtxCompiler.FixupJump(JumpInstrPos: Integer);
begin
  FPCode.PCode[JumpInstrPos].IValue := FPCodePos + 1;
end;

procedure TCtxCompiler.InitializeLabels;
begin
  FLabelEntries := nil;
  FLabels := TStringList.Create;
  FLabelEntries := TStringList.Create;
  TStringList(FLabels).Sorted := True;
  TStringList(FLabelEntries).Sorted := True;
end;

procedure TCtxCompiler.FinalizeLabels;
begin
  FreeAndNil(FLabels);
  FreeAndNil(FLabelEntries);
end;

procedure TCtxCompiler.FixupLabels;
var
  I, LabelIdx, IPos: Integer;
begin
  I := FLabelEntries.Count - 1;
  while I >= 0 do
  begin
    LabelIdx := FLabels.IndexOf(FLabelEntries[I]);
    if LabelIdx >= 0 then
    begin
      IPos := Integer(FLabels.Objects[LabelIdx]);
      FPCode.PCode[Integer(FLabelEntries.Objects[I])].IValue := IPos;
      FLabelEntries.Delete(I);
    end;
    Dec(I);
  end;
end;

procedure TCtxCompiler.FixupLabel(const LabelName: String; LabelPos: Integer);
var
  I: Integer;
begin
  // Fixup all entries of LabelName
  I := FLabelEntries.Count - 1;
  while I >= 0 do
  begin
    if FLabelEntries[I] = LabelName then
    begin
      FPCode.PCode[Integer(FLabelEntries.Objects[I])].IValue := LabelPos;
      FLabelEntries.Delete(I);
    end;
    Dec(I);
  end;
end;

procedure TCtxCompiler.GetNextChar;

  function GetChar: Char;
  begin
    if FCodePos <= Length(FPCode.Code) then
    begin
      Result := FPCode.Code[FCodePos]; // Strings 1-based position
      Inc(FCodePos);
    end else Result := #0;
  end;

begin
  FNextChar := GetChar;
  if FNextChar = #$0D then
    FNextChar := GetChar; // skip returns
  if FNextChar = strNL then
  begin
    FLinePos := 1;
    Inc(FLineNo);
  end else Inc(FLinePos);
end;

procedure TCtxCompiler.GetNextToken;
begin
  // Implement in descendants
  FPrevLine := FLineNo;
  FPrevPos := FLinePos;
end;

function TCtxCompiler.Match(TokenType: Integer): String;
begin
  Result := FNextToken;
  if FNextTokenType <> TokenType then
    ParseError(CSCE_SYNTAXERROR);
  GetNextToken;
end;

function TCtxCompiler.DeclareSymbol(const SymName: String;
  SymType: TSymbolType; Value: Variant): Integer;
begin
  Result := FPCode.Symbols.FindSymbol(SymName, set_anysymbol);
  if Result >= 0 then
    ParseError(CSCE_IDENTIFIERREDECLARED, SymName);
  Result := FPCode.Symbols.DeclareSymbol(SymName, SymType, Value);
end;

function TCtxCompiler.DeclareSymbol(const SymName: String;
  SymType: TSymbolType): Integer;
begin
  Result := DeclareSymbol(SymName, SymType, NULL);
end;

function TCtxCompiler.FindSymbol(const SymName: String;
  SymTypes: TSymbolTypes): Integer;
begin
  Result := FPCode.Symbols.FindSymbol(SymName, SymTypes);
end;

function TCtxCompiler.GetSymbolCount: Integer;
begin
  Result := FPCode.Symbols.Count;
end;

function TCtxCompiler.GetSymbols(Idx: Integer): TSymbol;
begin
  Result := FPCode.Symbols[Idx];
end;

procedure TCtxCompiler.SetSymbols(Idx: Integer; const Value: TSymbol);
begin
  FPCode.Symbols[Idx] := Value;
end;

procedure TCtxCompiler.DeclareLabel(const Name: String);
var
  Idx: Integer;
begin
  Idx := FLabels.IndexOf(Name);
  if Idx >= 0 then
    ParseError(CSCE_LABELREDECLARED, Name);
  FLabels.AddObject(Name, TObject(FPCodePos));
end;

procedure TCtxCompiler.ScriptHeader;
begin
  // Implement in descendants to compile expressions
end;

procedure TCtxCompiler.Script;
begin
  // Implement in descendants to compile scripts
end;

procedure TCtxCompiler.Expression;
begin
  // Implement in descendants to compile expressions
end;

function TCtxCompiler.GetCodeStub(PCode: TCtxScriptCode): String;
begin
  Result := '';
end;

procedure _ObjectFree(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  if Instance = CtxSystemScope then
    CtxScriptErrorFmt(SInvalidInvokeType, ['Free']);
  Instance.Free;
end;

procedure _ObjectSelf(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := VarFromObject(Instance);
end;

procedure _ObjectClassName(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := Instance.ClassName;
end;

procedure _ObjectInvoke(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  Name: String;
begin
  CheckParams(ParCount, 1, 255);
  Name := Sender.Pop;
  if not InvokeObjectMethod(Sender, InvokeType, Instance, Name, ParCount - 1) then
    CtxScriptErrorFmt(SMethodNotFound, [Name]);
end;

procedure _Evaluate(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  // Evaluation within the scope of executing code
  Sender.Evaluate(VarToStr(Sender.Pop), Sender.PCode);
end;

procedure _Assigned(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := Assigned(VarToObject(Sender.Pop));
end;


initialization
  CtxScripting := nil;
  FinalizingIntrospectors := False;

  CtxCompilers := TStringList.Create;
  CtxCompilers.Sorted := True;
  CtxCompilers.Duplicates := dupError;
  DefaultCtxCompiler := TCtxPasCompiler.Create(nil);

  CtxIntrospectors := TStringList.Create;
  CtxIntrospectors.Sorted := True;
  CtxIntrospectors.Duplicates := dupError;

  { Create system scope }
  CtxSystemScope := TCtxSystemScope.Create;

  { Create default introspectors }
  TCtxObjectIntrospector.Create(TObject);

  with TCtxCustomIntrospector.Create(TCtxSystemScope) do
  begin
    AddMethod('Evaluate', @_Evaluate, 1);
    AddMethod('Assigned', @_Assigned, 1);
    AddConst('nil', VarFromObject(nil));
  end;

  with TCtxCustomIntrospector.Create(TObject) do
  begin
    // object methods
    AddMethod('Free', @_ObjectFree, 0);
    AddMethod('Invoke', @_ObjectInvoke);
    AddProp('Self', @_ObjectSelf);
    AddProp('ClassName', @_ObjectClassName);
  end;

  TCtxPersistentIntrospector.Create(TPersistent);
  TCtxComponentIntrospector.Create(TComponent);

finalization
  FreeAndNil(CtxSystemScope);
  FinalizeIntrospectors;
  FreeAndNil(CtxIntrospectors);
  FreeAndNil(CtxScripting);
  FreeAndNil(DefaultCtxCompiler);
  FreeAndNil(CtxCompilers);
end.

