{
    This file is a part of the FPOS (Free Pascal Operating System) Shell,
    which known as the fpsh project, a part of the FPOS core component.

    Copyright (C) 2010-2021 Yacine REZGUI.
    Copyright (C) 2024 Le Bao Nguyen.

    This project is licensed in the same license as the FPOS project.
    *********************************************************************
}

unit matches;
{$mode objFPC} {$H+}

interface

uses strutils, sysutils;

type
    MatchTypes = (
        LEFT_PAREN, LEFT_BRACE
    );

    TUnMatched = class(TObject)
    
    private
        token_type: MatchTypes;
        line: integer;
        position: integer;
    
    public
        hasAMatch: boolean;

        constructor Create(token_type_: MatchTypes; line_: integer; position_: integer);
    end;

implementation

constructor TUnMatched.Create(token_type_: MatchTypes; line_: integer; position_: integer);
begin
    token_type = token_type_;
    line = line_;
    position = position_;
end;

end.