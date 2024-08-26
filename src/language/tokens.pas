{
    This file is a part of the FPOS (Free Pascal Operating System) Shell,
    which known as the fpsh project, a part of the FPOS core component.

    Copyright (C) 2010-2021 Yacine REZGUI.
    Copyright (C) 2024 Le Bao Nguyen.

    This project is licensed in the same license as the FPOS project.
    *********************************************************************
}

unit tokens;
{$mode objFPC} {$H+}

interface

uses sysutils, variants, typinfo;

type
    TokenTypes = (
        LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
        COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

        BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL,
        LESS, LESS_EQUAL,

        IDENTIFIER, STR, NUMBER,

        AND_, CLASS_, ELSE_, FALSE_, FUN, FOR_, IF_, NIL_, OR_,
        GIVE, SUPER, THIS, TRUE_, VAR_, WHILE_,

        EOF
    );

    Token = class (TObject)
    private
        token_type: TokenTypes;
        lexeme: string;
        literal: variant;
        line: integer;

    public
        constructor Create(token_type_: TokenTypes; lexeme_: string; literal_: variant; line_: integer);
        function toString(): string;
    end;

implementation

constructor Token.Create(token_type_: TokenTypes; lexeme_: string; literal_: variant; line_: integer);
begin
    self.token_type := token_type_;
    self.lexeme := lexeme_;
    self.literal := literal_;
    self.line := line_;
end;

function Token.toString(): string;
var stringofliteral: string;
begin
    if Vartype(literal) = varnull then stringofliteral := '' else stringofliteral := VarToStr(literal);
    // maybe use TStringHelper.join?
    Result := Format('%s %s %s', [
        GetEnumName(typeinfo(TokenTypes), ord(token_type)), lexeme, stringofliteral ]);
end;

end.