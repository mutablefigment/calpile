unit parser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Tokens,
  Lexer;

type
  TSourceCode = TStringList;

  TParser = class(TObject)

  private
    // file handle
    pSourceFile: TextFile;
    pSourceCode: TSourceCode;
    pStringStack: TNamedStrs;
    pIntStack: TNamedInts;
    pLexer: TLexer;

    pProgram: TProgram;

  public
    constructor Create(aSourceFile: string);
    destructor DestroyParser();

    // read all the lines of a file and remove all the unneeded chars
    procedure readLines();

    // once we read the source-code we will call the lexer
    // the lexer will then return a AST (abstract syntax tree)
    // which is then "transpiled" to assembler, which is compiled by nasm
    procedure callLexer();

    // Translates all of the internally represented forms, into assembly
    // so that we can call nasm on the assembly and get out a native binary!
    procedure TranslateASTToASM();

    // getter for the source code, lexer is called
    // with internal representation, so this is only needed
    // for debug printing steps!
    property SourceCode: TSourceCode read pSourceCode;
    property StringStack: TNamedStrs read pStringStack;
    property IntegerStack: TNamedInts read pIntStack;
  end;

  { TParser }
implementation

constructor TParser.Create(aSourceFile: string);
begin
  AssignFile(pSourceFile, aSourceFile);
  Self.pIntStack := TNamedInts.Create;
  Self.pStringStack := TNamedStrs.Create;

  Self.pProgram := TProgram.Create;
  Self.pLexer := TLexer.Create;
end;

destructor TParser.DestroyParser();
begin
  CloseFile(Self.pSourceFile);
  if Assigned(pProgram) then pProgram.Destroy;
  if Assigned(pLexer) then pLexer.Destroy;
  if (Assigned(pStringStack)) then Self.pStringStack.Free;
  if (Assigned(pIntStack)) then Self.pIntStack.Free;

  inherited;
end;


procedure TParser.readLines();
var
  lSourceText: string;
begin

  Self.pSourceCode := TStringList.Create;

  try
    // reset seek header, like in c
    Reset(pSourceFile);

    // read all the file
    while not EOF(Self.pSourceFile) do
    begin
      ReadLn(pSourceFile, lSourceText);
      if (lSourceText = string.Empty) then continue;
      pSourceCode.Add(lSourceText);
    end;
  except
    on E: EInOutError do
    begin
       WriteLn('Failed to read from file!!' );
       Exit;
    end;
  end;
end;

procedure TParser.callLexer();
var
  lFormDef: TFormDef;
  {lCallStack: TCallStack;}
  lLine: string;

  iCounter: integer;
  {iCallDepth: integer;}
begin
  iCounter := 0;

  for lLine in Self.pSourceCode do
  begin
    lFormDef := pLexer.eval(lLine, iCounter, pStringStack, pIntStack);
    Inc(iCounter);
    pProgram.Add(iCounter, lFormDef);
  end;
end;

procedure TParser.TranslateASTToASM();
var
  lForm: TFormDef;
  lArgs: TStringList;
  lArgName: string;

  lIntResult: integer;
  lStrResult: string;
begin
  for lForm in pProgram.Values do
  begin

    case lForm.FunToEval of
      TOperation.add:
      begin
        lArgs := lForm.Argumenst;
        lIntResult := 0;

        for lArgName in lArgs do
        begin
          lIntResult := lIntResult + IntegerStack.Items[lArgName];
        end;

        WriteLn('ADD> ' + IntToStr(lIntResult));
      end;

      TOperation.sub:
      begin
        lArgs := lForm.Argumenst;
        lIntResult := 0;

        for lArgName in lArgs do
        begin
          lIntResult := lIntResult - IntegerStack.Items[lArgName];
        end;

        WriteLn('SUB> ' + IntToStr(lIntResult));
      end;

      TOperation.Write:
      begin
        lArgs := lForm.Argumenst;
        lStrResult := string.Empty;

        for lArgName in lArgs do
        begin
          lStrResult := lStrResult + StringStack.Items[lArgName];
        end;

        WriteLn('WRITE> ' + lStrResult);
      end;
    end;
  end;
end;



end.
