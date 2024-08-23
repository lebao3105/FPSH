{
    This file is a part of the FPOS (Free Pascal Operating System) Shell,
    which known as the fpsh project, a part of the FPOS core component.

    Copyright (C) 2010-2021 Yacine REZGUI.
    Copyright (C) 2024 Le Bao Nguyen.

    This project is licensed in the same license as the FPOS project.
    *********************************************************************
}

unit fpshlang;

{$mode objfpc} {$h+}

interface

uses sysutils;

/// General exception class for FPSH scripts.
type EScript = class(Exception)
    constructor Create(lineno: integer; col: integer; line: string; msg: string);
end;

/// Syntax error exception class
type ESyntaxError = class(EScript)
    constructor Create(lineno: integer; col: integer; line: string);
end;

implementation

constructor EScript.Create(lineno: integer; col: integer; line: string; msg: string);
var
    chars_before_line: string;

begin
    chars_before_line := Format('%d (character %d)      | ', [lineno, col]);

    inherited CreateFmt(
        '%s' + sLineBreak + // error message
        '%s%s' + sLineBreak + // error line
        
        // 2 lines below are the error indicator: points to the column of the error.
        stringofchar(' ', length(chars_before_line)) +
        stringofchar(' ', col - 1) + '^ here',
        
        [ msg, chars_before_line, line ]
    );
end;

constructor ESyntaxError.Create(lineno: integer; col: integer; line: string);
begin
    inherited Create(lineno, col, line, 'Invalid syntax.');
end;

end.