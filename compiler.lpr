{%RunCommand $MakeExe($(EdFile))}
program compiler;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,

  // core logic
  Parser;

type

  { TCalpile_Compiler }

  TCalpile_Compiler = class(TCustomApplication)
  protected

    procedure DoRun; override;
  public

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;

  end;

  { TCalpile_Compiler }

  procedure TCalpile_Compiler.DoRun;
  var
    ErrorMsg: string;
    lPath: string;
    lParser: TParser;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('hi:', 'help input:');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if HasOption('i', 'input') then
    begin
      lPath := GetOptionValue('i', 'input');
      lParser := TParser.Create(lPath);
      lParser.readLines();
      lParser.callLexer();
      lParser.TranslateASTToASM();
      lParser.Free;
      Terminate;
      Exit;
    end;

    // stop program loop
    Terminate;
  end;

  constructor TCalpile_Compiler.Create(aOwner: TComponent);
  begin
    inherited Create(aOwner);
    StopOnException := True;
  end;

  destructor TCalpile_Compiler.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TCalpile_Compiler.WriteHelp;
  begin
    writeln('Usage: ', ExeName, ' -h');
    writeln(ExeName, ' -i [The .pcl source file]');
  end;


var
  Application: TCalpile_Compiler;

begin
  Application := TCalpile_Compiler.Create(nil);
  Application.Title := 'Calpile Compiler';
  Application.Run;
  Application.Free;
end.
