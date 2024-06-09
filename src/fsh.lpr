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
    custapp, strutils, internals,
    sysutils, promptstrings, process, utilities;

type TFPSH = class(TCustomApplication)
    protected
        procedure DoRun; override;
    private
        function RunProg(const cmd: string; argv: array of string): integer;
        procedure ProcessLine(const line: string);
        procedure WriteHelp;
end;

procedure TFPSH.DoRun;

var errorMsg: string;
    input: string;
    f: TextFile;
    tmp: string;

begin
    errorMsg := CheckOptions('hc:f:', ['help', 'command:', 'file:']);

    if errorMsg <> '' then begin
        WriteLn('Got an error: ' + errorMsg);
        Terminate;
        Exit;
    end;

    if HasOption('h', 'help') then begin
        WriteHelp;
        Terminate;
        Exit;
    end;

    if HasOption('c', 'command') then begin
        ProcessLine(GetOptionValue('c', 'command'));
        Terminate;
        Exit;
    end;

    if HasOption('f', 'file') then begin
        AssignFile(f, GetOptionValue('f', 'file'));
        Reset(f);

        while not EOF(f) do begin
            ReadLn(f, tmp);
            ProcessLine(tmp);
        end;

        Close(f);
        Terminate;
        Exit;
    end;

    while true do begin
        Write(PSOne);
        ReadLn(input);
        ProcessLine(input);
    end;

    Terminate;
end;

function TFPSH.RunProg(const cmd: string; argv: array of string): integer;
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

procedure TFPSH.ProcessLine(const line: string);
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

        Status := ShellCommands[i].Proc(args);
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

procedure TFPSH.WriteHelp;
begin
    Help(['no-internal']);
end;

var App: TFPSH;

begin
    App := TFPSH.Create(nil);
    App.Title := 'fpsh';
    App.StopOnException := true; // should I?
    App.Run;
    App.Free;
end.