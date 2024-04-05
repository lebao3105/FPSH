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
// {$I-} // disable raising errors on IOResult != 0

interface

uses
    {$ifdef UNIX}
    BaseUnix,
    {$endif UNIX}

    {$ifdef WINDOWS}
    winutils,
    {$endif WINDOWS}

    {$ifdef FPOS}
    console, rtc,
    {$endif FPOS}

    dos, // GetDate
    strutils, sysutils;

{ Internal commands }

{$ifdef FPOS}

procedure Restart(const argc: integer; argv: array of string); { Restarts the OS. }
procedure OSHalt(const argc: integer; argv: array of string); { Halts the OS. }

{$endif FPOS}

procedure ClearMyConsole(const argc: integer; argv: array of string); { Clears the console. }
procedure DumpRegs(const argc: integer; argv: array of string); { Dumps registry contents }
procedure ShowDate(const argc: integer; argv: array of string); { Shows the current date. }
procedure Help(const argc: integer; argv: array of string); { Shows the help message. }
procedure SwitchDir(const argc: integer; argv: array of string); { Changes the current working directory. }
procedure PushDir(const argc: integer; argv: array of string); { Push a directory to the top of directories stack. }
procedure PopDir(const argc: integer; argv: array of string); { Same as PushDir, but do the opposite }
procedure Quit(const argc: integer; argv: array of string); { Quits the current script/FPSH }

{ Internal shell functions }

{ Internal variables, types and constants }

type
    TFSHCommands = record
        Name: String;
        Alias: String;
        Description: String;
        Proc: procedure (const argc: Integer; argv: array of String);
    end;

    TCommandsHistory = record
        Commands: array [1..255] of String;
        Current: Byte;
    end;

var
    CommandsHistory: TCommandsHistory;
    Status: integer;
    PS1: string;

const
    ShellCommands: array [1..{$ifdef FPOS}10{$else}8{$endif}] of TFSHCommands = (
        (Name: 'cd'; Alias: ''; Description: 'Change working directory'; Proc: @SwitchDir),
        (Name: 'clear'; Alias: 'cls'; Description: 'Clears the screen'; Proc: @ClearMyConsole),
        (Name: 'date'; Alias: ''; Description: 'Get the current date'; Proc: @ShowDate),
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

    AmIRoot: boolean = true;
        // = {$ifdef UNIX} (fpgeteuid() = 0) {$endif}
        //   {$ifdef WINDOWS} isWindowsAdmin() {$endif}
        //   {$ifdef FPOS} true {$endif} // TBC
        // ;
    Line_BUFSIZE = 1024;

implementation

{$ifdef FPOS}

procedure Restart(const argc: integer; argv: array of string);
begin
    asm
        mov al, $FE
        out 64h, al
    end ['eax'];
end;

procedure OSHalt(const argc: integer; argv: array of string);
begin
    ClearMyConsole(argc, argv);
    SetTextColor(scBlack, scLightGreen);
    WriteLn('OS halted itself. It''s now safe to turn off your computer/emulator.');
    asm
        cli
        hlt
    end;
end;

{$endif}

procedure ClearMyConsole(const argc: integer; argv: array of string);
begin
    {$ifdef FPOS}
    ClearScreen;
    {$endif}
    // not implemented on other platforms
end;

procedure DumpRegs(const argc: integer; argv: array of string); { Dumps registry contents }
{$ifdef CPUI386}
label
    GetEIP;
var
    Reg: array [0..7] of LongWord;
    SReg: array [0..5] of Word;
    EFLAGS, CurrentEIP: LongWord;

begin
    asm
        // save registers while they are not modified by another procedure call. note that
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
end;
{$endif}

procedure ShowDate(const argc: integer; argv: array of string);
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

procedure Help(const argc: integer; argv: array of string);
var
    i: Byte;

begin
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

procedure SwitchDir(const argc: integer; argv: array of string);
begin
    if argv[0] = '' then
        WriteLn('An argument is required.')
    else if strutils.StartsStr('.', argv[0]) then
    begin
        Writeln('got there');
        ChDir(GetCurrentDir + '/..');
    end
    else
    begin
        writeln('got here too');
        ChDir(argv[0]);
    end;
    if IOresult <> 0 then
        WriteLn('Unable to change to ' + argv[1] + ' directory');
end;

procedure PushDir(const argc: integer; argv: array of string);
begin

end;

procedure PopDir(const argc: integer; argv: array of string);
begin

end;


procedure Quit(const argc: integer; argv: array of string);
begin
    if argv[0] <> '' then
        halt(StrToInt(argv[0]))
    else
        halt(0);
end;

end.