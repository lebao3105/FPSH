{
    This file is a part of the FPOS (Free Pascal Operating System) Shell,
    which known as the fpsh project, a part of the FPOS core component.

    Copyright (C) 2010-2021 Yacine REZGUI.
    Copyright (C) 2024 Le Bao Nguyen.

    This project is licensed in the same license as the FPOS project.
    *********************************************************************
}

unit utilities;
{$mode objFPC} {$H+}

interface

uses
    sysutils, process;

{ Logging functions }

procedure printinfo(const message: string);
procedure printdebug(const message: string);
procedure printerror(const message: string);
procedure printsuccess(const message: string);

{ Some more things }

function RunProgram(const prog: string; argv: array of string): integer;

implementation

procedure printinfo(const message: string);
begin
    writeln('[Shell - Info] ', message);
end;

procedure printdebug(const message: string);
begin
    if GetEnvironmentVariable('DEBUG') = '1' then
        writeln('[Shell - Debug] ', message);
end;

procedure printerror(const message: string);
begin
    writeln('[Shell - Error] ', message);
end;

procedure printsuccess(const message: string);
begin
    writeln('[Shell - Success] ', message);
end;

function RunProgram(const prog: string; argv: array of string): integer;
var
    aprocess: TProcess;

begin
    aprocess := TProcess.Create(nil);

    { Program to be ran }
    aprocess.Executable := prog;

    { Program arguments }
    aprocess.Parameters.AddStrings(argv);

    { Run options
      https://www.freepascal.org/docs-html/fcl/process/tprocess.options.html }
    aprocess.Options := aprocess.Options + [poWaitOnExit];

    { Current directory }
    aprocess.CurrentDirectory := GetCurrentDir;

    { Run the program and free the variable. Do not forget to return the exit code! }
    aprocess.Execute;
    aprocess.Free;
    Result := aprocess.ExitCode;
end;

end.