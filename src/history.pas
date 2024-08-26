{
    This file is a part of the FPOS (Free Pascal Operating System) Shell,
    which known as the fpsh project, a part of the FPOS core component.

    Copyright (C) 2010-2021 Yacine REZGUI.
    Copyright (C) 2024 Le Bao Nguyen.

    This project is licensed in the same license as the FPOS project.
    *********************************************************************

    Provides command history support (NOT about pressing up/down arrow),
    and of course, a command for that.
}

unit history;
{$mode objFPC} {$H+}

interface

uses dateutils, sysutils, classes, utilities;

var
    CommandsHistory: TStringList;

function History(const argv: array of string): integer;

implementation

function History(const argv: array of string): integer;
begin
    // TBD
    if Length(argv) = 0 then begin
        writeln('history command');
    end;

end;

initialization

try
    CommandsHistory := TStringList.Create;
    CommandsHistory.LoadFromFile(GetUserDir + '.fsh_history');
except
    on E: Exception do
        printdebug(E.Message + '. Will try to create/override the file.')
else
    printdebug('Completed reading the history file');
end;

printdebug('Sucessfully initialized history unit');

finalization

CommandsHistory.SaveToFile(GetUserDir + '.fsh_history');
printdebug('Saved commands history.');
CommandsHistory.Free;

printdebug('Completed history unit finalization tasks');

end.