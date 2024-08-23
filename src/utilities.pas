{
    This file is a part of the FPOS (Free Pascal Operating System) Shell,
    which known as the fpsh project, a part of the FPOS core component.

    Copyright (C) 2010-2021 Yacine REZGUI.
    Copyright (C) 2024 Le Bao Nguyen.

    This project is licensed in the same license as the FPOS project.
    *********************************************************************
}

unit utilities;
{$mode objFPC} {$H+}

interface

uses
    sysutils;

{ Logging functions }

procedure printinfo(const message: string);
procedure printdebug(const message: string);
procedure printerror(const message: string);
procedure printsuccess(const message: string);

{ Some more things }

implementation

procedure printinfo(const message: string);
begin
    writeln('[Shell - Info] ', message);
end;

procedure printdebug(const message: string);
begin
    if GetEnvironmentVariable('DEBUG') = '1' then
        writeln('[Shell - Debug] ', message);
end;

procedure printerror(const message: string);
begin
    writeln('[Shell - Error] ', message);
end;

procedure printsuccess(const message: string);
begin
    writeln('[Shell - Success] ', message);
end;

end.