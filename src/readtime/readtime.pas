{
    This file is a part of the FPOS (Free Pascal Operating System) Shell,
    which known as the fpsh project, a part of the FPOS core component.

    Copyright (C) 2010-2021 Yacine REZGUI.
    Copyright (C) 2024 Le Bao Nguyen.

    This project is licensed in the same license as the FPOS project.
    *********************************************************************

    GNU ReadLine calls.
}

unit readtime;
{$mode objFPC} {$H+}

{$linklib c}
{$linklib readline}

interface

uses ctypes;

function readline(const prompt: pchar): pchar; external;
procedure add_history(const line: pchar); external;
function write_history(const path: pchar): cint; external;

implementation

end.