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
{$ifdef FPOS}
    // system units - we have no reason to not include them
    console, rtc, strutils, types,
{$endif}
    credits;

(* Internal commands *)

{$ifdef FPOS}

{ Restarts the operating system. }
procedure Restart(var argc: integer; argv: array of string);
begin
    asm
        mov al, $FE
        out 64h, al
    end ['eax'];
end;

{ Halts the OS }
procedure OSHalt(var argc: integer; argv: array of string);
begin
    ClearScreen;
    SetTextColor(scBlack, scLightGreen);
    WriteLn('OS halted itself. It''s now safe to turn off your computer/emulator.');
    asm
        cli
        hlt
    end;
end;

{$endif}

{$ifdef FPOS}
procedure ClearScreen(var argc: integer; argv: array of string);
begin
    ClearScreen;
end;
{$else}
procedure ClearScreen(var argc: integer; argv: array of string);
begin
    WriteLn('Not implemented.');
end;
{$endif}

{ Dumps registerations - not implemented for 64bit }
{$ifdef CPU64}
procedure DumpRegs(var argc: integer; argv: array of string);
begin
    WriteLn('Sorry, this is not implemented yet.');
end;
{$else}
procedure DumpRegs(var argc: integer; argv: array of string);

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
{$endif}

{ Shows the current date }
procedure ShowDate(var argc: integer; argv: array of string);
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

begin
    with GetTime do begin
        Str(DayOfMonth,s);
        Write(Days[DayOfWeek + 1] + ', ' + Months[Month] + ' ' + s);
        if DayOfMonth in [11..13] then
            Write('th')
        else
            case DayOfMonth mod 10 of
                1: Write('st');
                2: Write('nd');
                3: Write('rd');
                else Write('th');
            end;
        Write(' ');
        if Year < 10 then
            Write('0');
        WriteLn(Year);
    end;
end;

type
    TFSHCommands = record
        Name: String;
        Alias: String = '';
        Description: String;
        Proc: procedure (var argc: Integer; argv: array of String);
    end;

    TCommandsHistory = record
        Commands: array [1..255] of String;
        Current: Byte;
    end;

var
    CommandsHistory: TCommandsHistory;

const
    MaxCmdLen = 8; // TBD, probably I need to research much more about OSes
    ShellCommands: array [1..9] of TCommands = (
        (Name: 'clear'; Alias: 'cls'; Description: 'Clears the screen'; Proc: ClearScreen)
    );

begin

end.