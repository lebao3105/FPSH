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
    {$ifdef UNIX}
    BaseUnix,
    {$endif UNIX}

    {$ifdef WINDOWS}
    winutils,
    {$endif WINDOWS}
    custapp, // TCustomApplication
    dos, // GetDate
    strutils, sysutils;

{ Internal commands }

{$ifdef FPOS}

function Restart(const argc: integer; argv: array of string): integer; { Restarts the OS. }
function OSHalt(const argc: integer; argv: array of string): integer; { Halts the OS. }

{$endif FPOS}

function DumpRegs(const argc: integer; argv: array of string): integer; { Dumps registry contents }
function ShowDate(const argc: integer; argv: array of string): integer; { Shows the current date. }
function Help(const argc: integer; argv: array of string): integer; { Shows the help message. }
function SwitchDir(const argc: integer; argv: array of string): integer; { Changes the current working directory. }
function PushDir(const argc: integer; argv: array of string): integer; { Push a directory to the top of directories stack. }
function PopDir(const argc: integer; argv: array of string): integer; { Same as PushDir, but do the opposite }
function Quit(const argc: integer; argv: array of string): integer; { Quits the current script/FPSH }

{ Internal shell functions }

function AmIRoot: boolean;

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
    ShellCommands: array [1..{$ifdef FPOS}9{$else}7{$endif}] of RFSHCommands = (
        (Name: 'cd'; Alias: ''; Description: 'Change working directory'; Proc: @SwitchDir),
        (Name: 'date'; Alias: 'dt'; Description: 'Get the current date'; Proc: @ShowDate),
        (Name: 'exit'; Alias: ''; Description: 'Quits the script/FPSH'; Proc: @Quit),
        (Name: 'help'; Alias: ''; Description: 'Show this and exit'; Proc: @Help),
        (Name: 'popd'; Alias: ''; Description: 'The same as pushd, but the opposite'; Proc: @PopDir),
        (Name: 'pushd'; Alias: ''; Description: 'Push a directory to the top of the directory stack'; Proc: @PushDir),
        (Name: 'regs'; Alias: ''; Description: 'Dump register contents'; Proc: @DumpRegs)
        {$ifdef FPOS}
        ,(Name: 'restart'; Description: 'Restart the OS'; Alias: 'reboot'; Proc: @Restart),
        (Name: 'shutdown'; Description: 'Halt the OS'; Alias: 'poweroff'; Proc: @OSHalt)
        {$endif}
    );

    Line_BUFSIZE = 1024;

implementation

function AmIRoot: boolean;
begin
    {$ifdef UNIX}
    Result := (fpgeteuid = 0);
    {$endif}

    {$ifdef WINDOWS}
    Result := isWindowsAdmin;
    {$endif}

    {$ifdef FPOS}
    Result := true;
    {$endif}
end;

{$ifdef FPOS}

function Restart(const argc: integer; argv: array of string): integer;
begin
    asm
        mov al, $FE
        out 64h, al
    end ['eax'];
end;

function OSHalt(const argc: integer; argv: array of string): integer;
begin
    SetTextColor(scBlack, scLightGreen);
    WriteLn('OS halted itself. It''s now safe to turn off your computer/emulator.');
    asm
        cli
        hlt
    end;
end;

{$endif}

function DumpRegs(const argc: integer; argv: array of string): integer; { Dumps registry contents }
{$ifdef CPUI386}
label
    GetEIP;
var
    Reg: array [0..7] of LongWord;
    SReg: array [0..5] of Word;
    EFLAGS, CurrentEIP: LongWord;

begin
    Result := 0;
    asm
        // save registers while they are not modified by another function call. note that
        // depending on your compiler settings, ebp may already be trashed (stack frame)
        mov dword ptr Reg[4*0],eax
        mov dword ptr Reg[4*1],ecx
        mov dword ptr Reg[4*2],edx
        mov dword ptr Reg[4*3],ebx
        mov dword ptr Reg[4*4],esp
        // esp is already incorrect since it was decreased by
        // the amount of stack space the local variables require
        mov eax,8*4+6*2+4+4
        add dword ptr Reg[4*4],eax // correct esp
        mov dword ptr Reg[4*5],ebp
        mov dword ptr Reg[4*6],esi
        mov dword ptr Reg[4*7],edi
        // save segment registers
        mov word ptr SReg[2*0],ds
        mov word ptr SReg[2*1],es
        mov word ptr SReg[2*2],cs
        mov word ptr SReg[2*3],ss
        mov word ptr SReg[2*4],fs
        mov word ptr SReg[2*5],gs
        // save EFLAGS
        pushfd
        pop dword ptr EFLAGS
        // now get eip
        call GetEIP
        GetEIP: pop dword ptr CurrentEIP
    end ['eax','ebx','ecx','edx','esp','ebp','esi','edi','ds','es','cs','ss','fs','gs'];
    WriteLn('EAX    = ' + HexStr(Reg[0], 8));
    WriteLn('ECX    = ' + HexStr(Reg[1], 8));
    WriteLn('EDX    = ' + HexStr(Reg[2], 8));
    WriteLn('EBX    = ' + HexStr(Reg[3], 8));
    WriteLn('ESP    = ' + HexStr(Reg[4], 8));
    WriteLn('EBP    = ' + HexStr(Reg[5], 8));
    WriteLn('ESI    = ' + HexStr(Reg[6], 8));
    WriteLn('EDI    = ' + HexStr(Reg[7], 8));
    WriteLn('DS     = ' + HexStr(SReg[0], 8));
    WriteLn('ES     = ' + HexStr(SReg[1], 8));
    WriteLn('CS     = ' + HexStr(SReg[2], 8));
    WriteLn('SS     = ' + HexStr(SReg[3], 8));
    WriteLn('FS     = ' + HexStr(SReg[4], 8));
    WriteLn('GS     = ' + HexStr(SReg[5], 8));
    WriteLn('EFLAGS = ' + HexStr(EFLAGS, 8));
    WriteLn('EIP    = ' + HexStr(CurrentEIP, 8));
end;
{$else}
begin
// not implemented on non-x86 platforms.
// this was mainly made for FPOS though.
    Result := 0;
end;
{$endif}

function ShowDate(const argc: integer; argv: array of string): integer;
const
    Days: array [1..7] of String = (
        'Sunday', 'Monday', 'Tuesday', 'Wednesday',
        'Thursday', 'Friday', 'Saturday'
    );
    Months: array [1..12] of String = (
        'January', 'February', 'March', 'April',
        'May', 'June', 'July', 'August',
        'September', 'October', 'November', 'December'
    );

var
    s: String;
    Year, Month, Day, WDay: word;
begin
    // normally on FPOS GetTime is made differrent from the FPC's RTL
    // but whatever... the OS will be rewritten soon.
    // TODO: Custom formats
    Result := 0;
    GetDate(Year, Month, Day, WDay);
    Str(Day, s);
    Write(Days[WDay + 1] + ', ' + Months[Month] + #32 + s);
    if Day in [11..13] then
        Write('th')
    else
        case Day mod 10 of
            1: Write('st');
            2: Write('nd');
            3: Write('rd');
            else Write('th');
        end;
    Str(Year, s);
    Write(' ' + s + #13);
end;

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