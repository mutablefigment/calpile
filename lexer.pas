unit Lexer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  RTTI,
  TypInfo,
  tokens;

const
  MAX_DEPTH = 250;

type
  TLexer = class
  private
    pFunList: TList;
    pRecursionDepth: integer;

    function ParseString(aArgText: string): string;
    function ParseInt(aArgText: string): integer;

    procedure ParseNamedVar(aFormDef: TFormDef; aName, aValue: string;
      aStringStack: TNamedStrs; aIntStack: TNamedInts);
  public

    function eval(aFormToEval: string; aCurrentLine: integer;
      aStringStack: TNamedStrs; aIntStack: TNamedInts): TFormDef;

    constructor Create();
    destructor Destroy(); override;

  end;

implementation

constructor TLexer.Create();
begin
  Self.pFunList := TList.Create;
  Self.pRecursionDepth := 0;
end;

destructor TLexer.Destroy();
begin
  if (Assigned(pFunList)) then Self.pFunList.Free;
end;

function TLexer.ParseString(aArgText: string): string;
begin
  Result := aArgText.Trim(['"']);
end;

function TLexer.ParseInt(aArgText: string): integer;
begin
  Result := StrToInt(aArgText);
end;

procedure TLexer.ParseNamedVar(aFormDef: TFormDef; aName, aValue: string;
  aStringStack: TNamedStrs; aIntStack: TNamedInts);
begin
  case aFormDef.FunToEval of
    TOperation.defs:
    begin
      if aStringStack.ContainsKey(aName) then
      begin
        aIntStack.Remove(aName);
      end;
      aStringStack.Add(aName, ParseString(aValue));
      //aFormDef.Argumenst.Add(aName);
    end;

    TOperation.defi:
    begin
      if aIntStack.ContainsKey(aName) then
      begin
        aIntStack.Remove(aName);
      end;
      aIntStack.Add(aName, ParseInt(aValue));
      //aFormDef.Argumenst.Add(aName);
    end;

    else
      raise Exception.Create('The we cannot parse ' + aName);
  end;
end;


function TLexer.eval(aFormToEval: string; aCurrentLine: integer;
  aStringStack: TNamedStrs; aIntStack: TNamedInts): TFormDef;
var
  lOpenParanIndex: integer;
  lCloseParanIndex: integer;
  lQuoteStartIndex: integer;
  lQuoteEndIndex: integer;

  { #todo : Can I use something instead of an index to find out where in the enum I am? }
  lEnumIndex: integer;
  lFormDef: TFormDef;
  lOPType: ^TTypeInfo;
  lOPString: string;
  lArgOffset: integer;
  lArgText: string;
  lArgsSplit: TStringArray;
  lStringVal: string;

  {
  lNestedLexer: TLexer;
  lNestedForm: TFormDef;
  lNestedParanOpen: integer;
  lNestedParanClose: integer;
  }

  // iterators
  iOperation: TOperation;
  iCurrArgOffset: integer;
  iQuote: integer;
  {iNested: integer;}

  iArgName: string;
begin
  lFormDef := TFormDef.Create;
  lOpenParanIndex := aFormToEval.IndexOf(char(TSymbol.openParan));
  lEnumIndex := 0;
  lArgText := '';

  // First we find the `(`
  if (lOpenParanIndex >= 0) then
  begin
    lFormDef.OpenParam := TSymbol.openParan;

    // Reverse lookup!
    // now we find the `)`
    lCloseParanIndex := aFormToEval.LastIndexOf(char(TSymbol.closeParan));
    if (lCloseParanIndex <= 0) then
      raise Exception.Create(Format(
        'No closing parantheses found on the current line! %d', [aCurrentline]));

    // now after the `(` should come a function
    for iOperation in TOperation do
    begin

      lOPType := TypeInfo(iOperation);
      lOPString := GetEnumName(lOPType, lEnumIndex);
      Inc(lEnumIndex);

      if (aFormToEval.Contains(lOPString)) then
      begin
        lFormDef.FunToEval := iOperation;
        break;
      end;
    end;

    {
     now we get the args!
    }

    // get a buffer and read until the closing parameter!
    // len + len(lOpStr) + whitespace!
    lArgOffset := lOpenParanIndex + Length(lOPString) + 1;
    for iCurrArgOffset := lArgOffset to lCloseParanIndex - 1 do
    begin
      AppendStr(lArgText, aFormToEval.Chars[iCurrArgOffset]);
    end;

    if (lArgText = string.Empty) then
      raise Exception.Create(Format(
        'No arguments passed to function %s on line %d', [lOPString, aCurrentLine]));

    { #todo: recusive call to the lexer for nested functions

    if (lArgText.Contains(char(TSymbol.openParan))) and
      (lArgText.Contains(char(TSymbol.closeParan))) then
    begin

      // check recursion depth
      if (aRecursionDepth >= MAX_DEPTH) then
        raise Exception.Create('Reached max recursion depth!');

      lStringVal := string.Empty;
      lNestedParanOpen := lArgText.IndexOf(char(TSymbol.openParan));
      lNestedParanClose := lArgText.LastIndexOf(char(TSymbol.closeParan));

      for iNested := lNestedParanOpen to lNestedParanClose + 1 do
      begin
        lStringVal := lStringVal + lArgText[iNested];
      end;

      lNestedLexer := TLexer.Create;
      lNestedForm := lNestedLexer.eval(lStringVal, aCurrentLine,
        aStringStack, aIntStack, aCallStack, aRecursionDepth);

      aCallStack[aRecursionDepth] := lNestedForm;

      Inc(aRecursionDepth);
      lNestedLexer.Free;
    end;
    }

    // now we split on whitespace and try to parse the args
    lArgsSplit := lArgText.Split(' ');
    case lFormDef.FunToEval of
      TOperation.defi:
      begin
        Self.ParseNamedVar(
          lFormDef,
          Trim(lArgsSplit[1]),
          Trim(lArgsSplit[2]),
          aStringStack,
          aIntStack);
      end;
      TOperation.defs:
      begin
        lStringVal := string.Empty;
        lQuoteStartIndex := lArgText.IndexOf('"');
        lQuoteEndIndex := lArgText.LastIndexOf('"');

        for iQuote := lQuoteStartIndex to lQuoteEndIndex do
        begin
          lStringVal := lStringVal + lArgText[iQuote];
        end;

        Self.ParseNamedVar(
          lFormDef,
          Trim(lArgsSplit[1]),
          lStringVal,
          aStringStack,
          aIntStack);

      end
      else
        for iArgName in lArgsSplit do
        begin
          if not (iArgName = string.Empty) then lFormDef.Argumenst.Add(iArgName);
        end;

    end;

    { #todo : limit amount of max args you can pass to a func }
    Result := lFormDef;
  end;

end;

end.
