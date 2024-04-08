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
    strutils, sysutils;

{ Internal commands }

function Help(const argc: integer; argv: array of string): integer; { Shows the help message. }
function SwitchDir(const argc: integer; argv: array of string): integer; { Changes the current working directory. }
function PushDir(const argc: integer; argv: array of string): integer; { Push a directory to the top of directories stack. }
function PopDir(const argc: integer; argv: array of string): integer; { Same as PushDir, but do the opposite }
function Quit(const argc: integer; argv: array of string): integer; { Quits the current script/FPSH }

{ Internal shell functions }

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
    DirStack: array of string;

const
    ShellCommands: array [1..5] of RFSHCommands = (
        (Name: 'cd'; Alias: ''; Description: 'Change working directory'; Proc: @SwitchDir),
        (Name: 'exit'; Alias: ''; Description: 'Quits the script/FPSH'; Proc: @Quit),
        (Name: 'help'; Alias: ''; Description: 'Show this and exit'; Proc: @Help),
        (Name: 'popd'; Alias: ''; Description: 'The same as pushd, but the opposite'; Proc: @PopDir),
        (Name: 'pushd'; Alias: ''; Description: 'Push a directory to the top of the directory stack'; Proc: @PushDir)
    );

    Line_BUFSIZE = 1024;

implementation

function Help(const argc: integer; argv: array of string): integer;
var
    i: Byte;

begin
    Result := 0;
    WriteLn(ParamStr(0) + ' [flags] [values/commands]');
    WriteLn('A shell written in Pascal.');
    WriteLn('(C) 2024 Le Bao Nguyen. Licensed under the GNU GPL Version 3.');
    WriteLn;
    WriteLn('Internal commands:');
    for i := Low(ShellCommands) to High(ShellCommands) do
        with ShellCommands[i] do begin
            Write(Name);
            if (Alias <> '') then
                Write(' Alias: ', Alias);
            Write(StringOfChar(' ', 8 - Length(Name) + 1));
            WriteLn(Description);
        end;
end;

function SwitchDir(const argc: integer; argv: array of string): integer;
begin
    Result := 0;
    if argv[1] = '' then begin
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
begin
    Result := 0;

    if not DirectoryExists(argv[1]) then
    begin
        WriteLn(argv[1] + ': no such file or directory');
        Result := 3;
        Exit;
    end;
end;

function PopDir(const argc: integer; argv: array of string): integer;
begin
    Result := 0;
end;

function Quit(const argc: integer; argv: array of string): integer;
var code: longint = 0;
begin
    TryStrToInt(argv[1], code);
    halt(code);
end;

end.