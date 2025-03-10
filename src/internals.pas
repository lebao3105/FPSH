{
    This file is a part of the FPOS (Free Pascal Operating System) Shell,
    which known as the fpsh project, a part of the FPOS core component.
    
    Copyright (C) 2010-2021 Yacine REZGUI.
    Copyright (C) 2024 Le Bao Nguyen.

    This project is licensed in the same license as the FPOS project.
    *********************************************************************
    This file contains internal things for the shell, from commands to
    variables like PS1 and commands history.
}

unit internals;

{$mode ObjFPC} {$H+}
{$I-} // disable raising errors on IOResult != 0

interface

uses
    strutils, sysutils,
    classes, utilities;

{ Internal variables, types and constants }

type
    RFSHCommands = record
        Name: String;
        Alias: String;
        Description: String;
        Proc: function (const argv: array of String): integer;
    end;

    RFSHFlags = record
        Short, Long: string;
        Argument: string;
        Description: string;
    end;

var
    Status: integer;
    DirStack: TStringList;
    ShellCmdsAndAliases: array of string;

const
    version: string = '0.1';
    flagsHelp: array of RFSHFlags = (
        (Short: 'f'; Long: 'file'; Argument: 'path'; Description: 'Script to execute'),
        (Short: 'c'; Long: 'command'; Argument: 'commands, separated by semicolons'; Description: 'Commands to execute'),
        (Short: 'h'; Long: 'help'; Description: 'Show this help and peacefully exit'),
        (Short: 'w'; Long: 'where'; Argument: 'directory'; Description: 'Sets the working directory')
    );

{ Internal commands }

function Help(const argv: array of string): integer;
function SwitchDir(const argv: array of string): integer;
function PushDir(const argv: array of string): integer;
function PopDir(const argv: array of string): integer;
function Quit(const argv: array of string): integer;
function ShowDirStack(const argv: array of string): integer;

{ Internal shell functions }

{ Internal shell variables, types and constants (cont) }

const
    // An array of command records.
    // Not strictly required for the alphabet Name sort, but highly recommended (easy to read and to show with help command)
    // Remember to update the number of records inside on command add/remove!
    ShellCommands: array [1..6] of RFSHCommands = (
        (Name: 'cd'; Alias: ''; Description: 'Change working directory'; Proc: @SwitchDir),
        (Name: 'exit'; Alias: 'quit'; Description: 'Quits the script/FPSH'; Proc: @Quit),
        (Name: 'help'; Alias: ''; Description: 'Show this and exit'; Proc: @Help),
        (Name: 'popd'; Alias: ''; Description: 'The same as pushd, but the opposite'; Proc: @PopDir),
        (Name: 'pushd'; Alias: ''; Description: 'Push a directory to the top of the directory stack'; Proc: @PushDir),
        (Name: 'dirstack'; Alias: ''; Description: 'Shows the directory stack'; Proc: @ShowDirStack)
    );

implementation

// variables here are mostly used for the initialization tasks
var n: integer;

function Help(const argv: array of string): integer;
var
    i: Byte;

begin
    Result := 0;
    WriteLn(ParamStr(0) + ' [flags] [values/commands]');
    WriteLn('Version ' + version + '.');
    WriteLn;
    WriteLn('A shell written in Pascal.');
    WriteLn('(C) 2010-2021 Yacine REZGUI.');
    WriteLn('(C) 2024 Le Bao Nguyen.');
    WriteLn('Licensed under the GNU GPL Version 3.');
    WriteLn;

    if argv[1] <> 'no-internal' then begin
        WriteLn('Internal commands:');
        for i := Low(ShellCommands) to High(ShellCommands) do
            with ShellCommands[i] do begin
                Write(Name);
                if (Alias <> '') then
                    Write(' (' + Alias + ')');
                Write(StringOfChar(' ', 8 - Length(Name) + 1));
                WriteLn(Description);
            end;
    end
    else begin
        writeln('Command line flags:');
        for i := Low(flagsHelp) to High(flagsHelp) do
            writeln(
                ''.Format(
                    '-%s [%s] / --%s=[%s]       : %s',
                    [flagsHelp[i].Short, flagsHelp[i].Argument,
                     flagsHelp[i].Long, flagsHelp[i].Argument,
                     flagsHelp[i].Description]
                )
            );
    end;
    WriteLn('Set DEBUG environment variable to 1 to enable debugging messages.');
    WriteLn('Run man fsh-<command> for command help. man fsh for FSH informations.');
    WriteLn('Run man fsh-codes for some exit codes.');
end;

function SwitchDir(const argv: array of string): integer;
var argc: integer;
begin
    argc := Length(argv);
    Result := 0;

    if argc = 0 then
    begin
        RunProgram('man', ['1', 'fsh-cd']);
        writeln('An argument is required.');
        Result := 1;
    end

    else if not DirectoryExists(argv[1]) then
    begin
        WriteLn(argv[1] + ': no such directory');
        Result := 3;
    end

    else begin
        SetCurrentDir(ExpandFileName(argv[1]));
        if IOresult <> 0 then begin
            WriteLn('Unable to change to ' + argv[1] + ' directory');
            Result := IOresult;
        end;
        DirStack.Strings[0] := ExpandFileName(argv[1]);
    end;

end;

function PushDir(const argv: array of string): integer;
// please allow new variables to be created outside this var sectionnnnn
var
    Target, i: integer;
    Backup: string;
    argc: integer;
begin
    argc := Length(argv);
    Result := 0;

    if argc = 0 then begin
        RunProgram('man', ['1', 'fsh-dirstack']);
        writeln('An argument is required.');
        Result := 1;
        Exit;
    end;

    if strutils.StartsStr('$', argv[1]) then begin

        if argv[1] = '$' then begin
            WriteLn('Wrong syntax used. Correct: pushd $<number>.');
            Result := 1;
            Exit;
        end;

        if not sysutils.TryStrToInt(strutils.AnsiReplaceStr(argv[1], '$', ''), Target) then
        begin
            WriteLn('Unable to convert argument from string to integer ($ excluded): ', argv[1]);
            Result := 1;
            Exit;
        end

        else if not ((DirStack.Count - 1) >= Target) and (Target >= 0) then
        begin
            WriteLn('Index out of range: ' + IntToStr(Target));
            Result := 1;
            Exit;
        end;

        Backup := DirStack[Target];

        printdebug('Target directory to move to: ' + Backup);

        SwitchDir(['', Backup]);

        DirStack.Delete(Target);
        DirStack.Add(Backup);

        printdebug('Is the selected directory pushed to the top of DirStack: ' +
                   BoolToStr(DirStack.IndexOf(Backup) + 1 = DirStack.Count, true));
        
        Exit;

    end else if not DirectoryExists(argv[1]) then begin
        WriteLn(argv[1] + ': no such directory');
        Result := 3;
        Exit;
    end;

    DirStack.Add(ExpandFileName(IncludeTrailingPathDelimiter(argv[1])));
    SwitchDir(argv);
end;

function PopDir(const argv: array of string): integer;
var
    Target: integer = -1;
begin
    Result := 0;

    if strutils.StartsStr('$', argv[1]) then begin

        if argv[1] = '$' then begin
            RunProgram('man', ['1', 'fsh-dirstack']);
            WriteLn('Wrong syntax used. Correct: popd $<number>.');
            Result := 1;
            Exit;
        end;

        if not sysutils.TryStrToInt(strutils.AnsiReplaceStr(argv[1], '$', ''), Target) then
        begin
            RunProgram('man', ['1', 'fsh-dirstack']);
            WriteLn('Unable to convert argument from string to integer ($ excluded): ', argv[1]);
            Result := 1;
            Exit;
        end

        else if not ((DirStack.Count - 1) >= Target) and (Target >= 0) then
        begin
            RunProgram('man', ['1', 'fsh-dirstack']);
            WriteLn('Index out of range: ' + IntToStr(Target));
            Result := 1;
            Exit;
        end;

    end

    else if not DirectoryExists(argv[1]) then begin
        WriteLn(argv[1] + ': no such directory');
        Result := 3;
        Exit;
    end;
    
    if Target = -1 then Target := DirStack.IndexOf(argv[1]);

    DirStack.Delete(Target);
end;

function Quit(const argv: array of string): integer;
var code: longint = 0;
begin
    TryStrToInt(argv[1], code);
    printdebug('Quitting with code ' + argv[1] + ' (defaults to 0 on convert failure)');
    halt(code);
end;

function ShowDirStack(const argv: array of string): integer;
var
    n: integer;
begin
    Result := 0;
    for n := 0 to DirStack.Count - 1 do
        WriteLn(IntToStr(n) + '  |    ' + DirStack.Strings[n]);
end;

initialization

printdebug(IntToStr(Length(ShellCommands)) + ' internal commands');
printdebug('Initializing internals unit... Add all internal commands with their aliases to a list.');

DirStack := TStringList.Create;
DirStack.Duplicates := dupError; // we can just derive TStringList:v

SetLength(ShellCmdsAndAliases, Length(ShellCommands) * 2);

printdebug('Adding commands to ShellCmdsAndAliases...');
for n := Low(ShellCommands) to High(ShellCommands) do
begin
    ShellCmdsAndAliases[n] := ShellCommands[n].Name;
end;

printdebug('As well as their alias...');
for n := High(ShellCommands) + 1 to High(ShellCmdsAndAliases) do
begin
    ShellCmdsAndAliases[n] := ShellCommands[n - Length(ShellCommands)].Alias;
end;

printdebug('Completed Internals unit tasks!');

end.