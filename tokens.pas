unit tokens;

{$mode delphi}
{$H+}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections;

type
  // all special symbols
  TSymbol = (
    openParan = '(',
    closeParan = ')',
    lineEnd = ';'
    );

  // enum of all possible operations
  TOperation = (
    defi,      // define an integer
    defs,      // define a string
    add,       // add two integers
    sub,       // sub two integers
    write,     // write to stdout
    read,      // read from stdin
    syscall    // perform a syscall (linux: man unistd.h)
    );


  // the form is (function arg1 arg1); ... the args get applied to the function

  TNamedStrs = TDictionary<string, string>;
  TNamedInts = TDictionary<string, integer>;

  TFormDef = class(TObject)
    pOpenParan: TSymbol;
    pFunToEval: TOperation;
    pFuncArgs: TStringList;
    pCloseParan: TSymbol;
    pEndDelimiter: TSymbol;
    pComment: TSymbol;
    pStackDepth: integer;
  public

    property OpenParam: TSymbol read pOpenParan write pOpenParan;
    property FunToEval: TOperation read pFunToEval write pFunToEval;
    property Argumenst: TStringList read pFuncArgs write pFuncArgs;
    property Closeparan: TSymbol read pCloseParan write pCloseParan;
    property EndDelimiter: TSymbol read pEndDelimiter write pEndDelimiter;
    property Comment: TSymbol read pOpenParan write pOpenParan;
    property StackDepth: integer read pStackDepth write pStackDepth;

    constructor Create();
    destructor Destroy(); override;

  end;

  TCallStack = TArray<TFormDef>;
  TProgram = TDictionary<integer, TFormDef>;

implementation

constructor TFormDef.Create();
begin
  Self.pFuncArgs := TStringList.Create;
  inherited;
end;

destructor TFormDef.Destroy();
begin
  if (Assigned(Self.pFuncArgs)) then FreeAndNil(pFuncArgs);
  inherited;
end;

end.
