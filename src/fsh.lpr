{
    This file is a part of the FPOS (Free Pascal Operating System) Shell,
    which known as the fpsh project, a part of the FPOS core component.
    
    Copyright (C) 2010-2021 Yacine REZGUI.
    Copyright (C) 2024 Le Bao Nguyen.

    This project is licensed in the same license as the FPOS project.
    *********************************************************************
}

program fsh;
{$mode ObjFPC} {$H+}

uses
    strutils, internals, sysutils, promptstrings, process, utilities;

function RunProg(const cmd: string; argv: array of string): integer;
var aprocess: TProcess; i: integer;
begin
    aprocess := TProcess.Create(nil);
    aprocess.Executable := cmd;
    for i := Low(argv) to High(argv) do
    begin
        // writeln(argv[i]);
        aprocess.Parameters.Add(argv[i]);
    end;
    aprocess.Options := aprocess.Options + [poWaitOnExit];
    // writeln(CurrDir);
    aprocess.CurrentDirectory := GetCurrentDir;
    aprocess.Execute;
    aprocess.Free;
    Result := aprocess.ExitCode;
end;

procedure ProcessLine(const line: string);
var
    i: byte;
    cmd: string;
    args: array of string;
    args_string: string;
    search: string;

begin
    printdebug('Got ' + line);

    args := strutils.SplitString(line, ' ');
    cmd := args[0];

    printdebug('Gonna try to search for ''' + cmd + '''');

    if cmd in ShellCmdsAndAliases then begin
        i := IndexStr(cmd, ShellCmdsAndAliases);

        printdebug('''' + cmd + ''' index in commands + aliases list: ' + IntToStr(i));
        printdebug('The list is made for internal commands and aliases first.');

        if i >= (Length(ShellCommands) + 1) then
        begin
            i := i - Length(ShellCommands);
        end;

        Status := ShellCommands[i].Proc(High(args) + 1, args);
    end

    else begin
        args_string := StringReplace(line, cmd + ' ', '', []);

        if args_string = cmd then args_string := '';

        if not FileExists(cmd) then begin
            search := ExeSearch(lowerCase(cmd), GetEnvironmentVariable('PATH'));
            if search <> '' then begin
                Status := RunProg(cmd, SplitString(args_string, ' '));
            end
            else
                WriteLn('''' + cmd + ''' is not recognized as an internal command ' +
                        'or external command, operable program or batch file, a function or a variable.');
        end
        else
            Status := RunProg(cmd, SplitString(args_string, ' '));
    end;
end;

var
    input: string;

begin
    while true do begin
        Write(PSOne + ' ');
        ReadLn(input);
        ProcessLine(input);
    end;
end.