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
    strutils, internals, sysutils;

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
        if Cmd < ShellCommands[mid].Name then
            SearchCommand := SearchCommand(l, mid - 1)
        else if Cmd > ShellCommands[mid].Name then
            SearchCommand := SearchCommand(mid + 1, r)
        else begin
            SearchCommand := True;
            idx := mid;
        end;
    end;

begin
    IsShellCommand := SearchCommand(Low(ShellCommands), High(ShellCommands));
end;


procedure ProcessLine(const line: string);
var
    i: byte;
    cmd: string;
    args: array of string;
    search: string;

begin
    args := strutils.SplitString(line, ' ');
    cmd := args[0];
    if IsShellCommand(lowerCase(cmd), i) then
        ShellCommands[i].Proc(High(args) - Low(args) + 1, args)
    else begin
        search := ExeSearch(lowerCase(cmd), GetEnvironmentVariable('PATH'));
        writeln(search);
        if search <> '' then begin
            Status := ExecuteProcess(search, sysutils.StringReplace(line, cmd + ' ', '', []));
        end
        else
            WriteLn('''' + cmd + ''' is not recognized as an internal command ' +
                    'or external command, operable program or batch file.');
    end;
end;

var
    input: string;
    dollarOrNot: string;

begin
    if AmIRoot then
        dollarOrNot := '#'
    else
        dollarOrNot := '$';

    PS1 := sysutils.GetCurrentDir + ' []';
    while true do begin
        Write(PS1 + '> ');
        ReadLn(input); // you may think about making your own one...
        ProcessLine(input);
    end;
end.