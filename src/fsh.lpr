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
    strutils, internals, sysutils, promptstrings, process;

function IsShellCommand(const cmd: string; var idx: byte): boolean;
    function SearchCommand(l, r: Byte): Boolean;
    var
        mid: Byte;

    begin
        if l > r then begin
            SearchCommand := False;
            Exit;
        end;
        mid := (l + r) div 2;
        if (Cmd = ShellCommands[mid].Alias) then begin
            SearchCommand := True;
            idx := mid;
        end;
        if (Cmd < ShellCommands[mid].Name) then
            SearchCommand := SearchCommand(l, mid - 1)
        else if (Cmd > ShellCommands[mid].Name) then
            SearchCommand := SearchCommand(mid + 1, r)
        else begin
            SearchCommand := True;
            idx := mid;
        end;
    end;

begin
    IsShellCommand := SearchCommand(Low(ShellCommands), High(ShellCommands));
end;

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
    args := strutils.SplitString(line, ' ');
    cmd := args[0];

    if IsShellCommand(lowerCase(cmd), i) then
        Status := ShellCommands[i].Proc(High(args) - Low(args) + 1, args)
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