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
    dos, // GetDate
    strutils, sysutils,
    utilities;

{ Internal variables, types and constants }

type
    RFSHCommands = record
        Name: String;
        Alias: String;
        Description: String;
        Proc: function (const argc: Integer; argv: array of String): integer;
    end;

    TCommandsHistory = record
        Commands: array [1..255] of String;
        Current: Byte;
    end;

var
    CommandsHistory: TCommandsHistory;
    Status: integer;
    DirStack: array of string = ();
    ShellCmdsAndAliases: array of string;

const
    version: string = '0.1';

{ Internal commands }

function Help(const argc: integer; argv: array of string): integer;
function SwitchDir(const argc: integer; argv: array of string): integer;
function PushDir(const argc: integer; argv: array of string): integer;
function PopDir(const argc: integer; argv: array of string): integer;
function Quit(const argc: integer; argv: array of string): integer;
function ShowDirStack(const argc: integer; argv: array of string): integer;

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

function Help(const argc: integer; argv: array of string): integer;
var
    i: Byte;

begin
    Result := 0;
    WriteLn(ParamStr(0) + ' [flags] [values/commands]');
    WriteLn('Version ' + version + '.');
    WriteLn('A shell written in Pascal.');
    WriteLn('(C) 2010-2021 Yacine REZGUI.');
    WriteLn('(C) 2024 Le Bao Nguyen.');
    WriteLn('Licensed under the GNU GPL Version 3.');
    WriteLn;
    WriteLn('Internal commands:');
    for i := Low(ShellCommands) to High(ShellCommands) do
        with ShellCommands[i] do begin
            Write(Name);
            if (Alias <> '') then
                Write(' (' + Alias + ')');
            Write(StringOfChar(' ', 8 - Length(Name) + 1));
            WriteLn(Description);
        end;
    WriteLn('Set DEBUG environment variable to enable debugging messages.');
end;

function SwitchDir(const argc: integer; argv: array of string): integer;
begin
    Result := 0;

    if argc = 0 then begin
        writeln('An argument is required.');
        Result := 1;
    end

    else if not DirectoryExists(argv[1]) then begin
        WriteLn(argv[1] + ': no such directory');
        Result := 3;
    end

    else begin
        SetCurrentDir(ExpandFileName(IncludeTrailingPathDelimiter(argv[1])));
        if IOresult <> 0 then begin
            WriteLn('Unable to change to ' + argv[1] + ' directory');
            Result := IOresult;
        end;
    end;

end;

function PushDir(const argc: integer; argv: array of string): integer;
// please allow new variables to be created outside this var sectionnnnn
var
    Target, i: integer;
    Backup: string;
begin
    Result := 0;

    if argc = 0 then begin
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

        else if (Target < Low(DirStack)) or (Target > High(DirStack)) then
        begin
            WriteLn('Index out of range: ' + IntToStr(Target));
            Result := 1;
            Exit;
        end;

        Backup := DirStack[Target];

        printdebug('Target directory to move to: ' + Backup);

        SwitchDir(1, ['', Backup]);

        for i := Target + 1 to High(DirStack) do
            DirStack[i-1] := DirStack[i];
        
        DirStack[High(DirStack)] := Backup;
        printdebug('Is the selected directory pushed to the top of DirStack: ' +
                   BoolToStr(IndexStr(Backup, DirStack) = High(DirStack), true));
        
        Exit;

    end else if not DirectoryExists(argv[1]) then begin
        WriteLn(argv[1] + ': no such directory');
        Result := 3;
        Exit;
    end;

    SetLength(DirStack, Length(DirStack) + 1);
    DirStack[High(DirStack)] := ExpandFileName(IncludeTrailingPathDelimiter(argv[1]));
    SwitchDir(1, argv);
end;

function PopDir(const argc: integer; argv: array of string): integer;
var
    Target: integer = -1;
begin
    Result := 0;

    if strutils.StartsStr('$', argv[1]) then begin

        if argv[1] = '$' then begin
            WriteLn('Wrong syntax used. Correct: popd $<number>.');
            Result := 1;
            Exit;
        end;

        if not sysutils.TryStrToInt(strutils.AnsiReplaceStr(argv[1], '$', ''), Target) then
        begin
            WriteLn('Unable to convert argument from string to integer ($ excluded): ', argv[1]);
            Result := 1;
            Exit;
        end

        else if (Target < Low(DirStack)) or (Target > High(DirStack)) then
        begin
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
    
    if Target = -1 then Target := IndexStr(argv[1], DirStack);

    SetLength(DirStack, Length(DirStack) - 1);

    Delete(DirStack, Target - 1, 1); // let's try if this works
end;

function Quit(const argc: integer; argv: array of string): integer;
var code: longint = 0;
begin
    TryStrToInt(argv[1], code);
    printdebug('Quitting with code ' + argv[1] + ' (defaults to 0 on convert failure)');
    halt(code);
end;

function ShowDirStack(const argc: integer; argv: array of string): integer;
var n: integer;
begin
    for n := Low(DirStack) to High(DirStack) do
        WriteLn(IntToStr(n) + '. ' + DirStack[n]);
end;

initialization

printdebug(IntToStr(Length(ShellCommands)) + ' internal commands');
printdebug('Initializing internals unit... Add all internal commands with their aliases to a list.');

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