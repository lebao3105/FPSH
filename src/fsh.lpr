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
    sysutils, promptstrings, process, utilities,
    history, readtime;

type
    /// Main application class.
    TFPSH = class(TCustomApplication)
    protected
        /// Main function that handles command-line arguments
        /// and base setup for the interactive console
        /// (look at the code instead of this description if
        /// you don't understand)
        procedure DoRun; override;
    private

        /// Process entered line (for both script and interactive mode)
        procedure ProcessLine(line: string);

        /// ProcessLine but for C's Char* (just cast it to string)
        procedure ProcessLine(line: pchar); overload;

        /// Write out the help message
        procedure WriteHelp;
    end;

procedure TFPSH.DoRun;

var errorMsg: string;
    input: pchar;
    f: TextFile;
    tmp: string;

begin
    errorMsg := CheckOptions('hc:f:w:', ['help', 'command:', 'file:', 'where:']);

    if errorMsg <> '' then
    begin
        WriteHelp;
        WriteLn('Got an error: ' + errorMsg);
        Terminate;
        Exit;
    end;

    if HasOption('h', 'help') then
    begin
        WriteHelp;
        Terminate;
        Exit;
    end;

    if HasOption('c', 'command') then
    begin
        printdebug('(FSH argument) got a command to run.');
        ProcessLine(GetOptionValue('c', 'command'));
        Terminate;
        Exit;
    end;

    if HasOption('f', 'file') then
    begin
        printdebug('(FSH argument) got a file to run');
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

    if HasOption('w', 'where') then
    begin
        printdebug('(FSH argument) got a directory to switch to');
        SetCurrentDir(GetOptionValue('w', 'where'));
    end
    else
        DirStack.Add(GetCurrentDir); 

    printdebug('Entering the main loop');
    while true do begin
        input := readline(pchar(PSOne()));
        ProcessLine(input);
        add_history(input);
    end;

    Terminate;

    write_history(pchar('~/.fpsh_history'));
end;

procedure TFPSH.ProcessLine(line: pchar);
begin
    ProcessLine(string(line));
end;

procedure TFPSH.ProcessLine(line: string);
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

    if cmd in ShellCmdsAndAliases then
    begin
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

        if not FileExists(cmd) then
        begin
            printdebug(GetCurrentDir() + DirectorySeparator + cmd + ' not found.');
            printdebug('Finding ' + cmd + ' in PATH environment variable');

            search := ExeSearch(cmd, GetEnvironmentVariable('PATH'));
            if search <> '' then
            begin
                printdebug('Found in PATH: ' + search);
                Status := RunProgram(search, SplitString(args_string, ' '));
            end
            else
                WriteLn('''' + cmd + ''' is not recognized as an internal command ' +
                        'or external command, operable program or batch file, a function or a variable.');
        end
        else
            Status := RunProgram(cmd, SplitString(args_string, ' '));
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
    App.Run;
    App.Free;
end.