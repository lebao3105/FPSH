{
    This file is a part of the FPOS (Free Pascal Operating System) Shell,
    which known as the fpsh project, a part of the FPOS core component.

    Copyright (C) 2010-2021 Yacine REZGUI.
    Copyright (C) 2024 Le Bao Nguyen.

    This project is licensed in the same license as the FPOS project.
    *********************************************************************
}

unit scanner;
{$mode objFPC} {$H+}

interface

uses tokens, fpshlang, character, variants;

type
    TScanner = class(TObject)
    private
        source: string;
        tokens: array of Token;
        start: integer;
        current: integer;
        line: integer;

        { Returns whether we've reached EOLine or not }
        function isAtEnd: boolean;

        { Scans for tokens, line by line. }
        procedure scanTokens;

        { Returns peek(), and jumps to the next character. }
        function nextChar: char;

        { Returns the current character in line. #0 (null character) if we're in EOL. }
        function peek: char;

        { Checks if the current character matches the expected one. }
        function matchCurrent(expected: char): boolean;

        { Adds a new token. }
        procedure addToken(token_type: TokenTypes);

        { Adds a new token (overload) }
        procedure addToken(token_type: TokenTypes; literal: variant); overload;

        { Finds string (starts and ends with ") }
        procedure getString;

        // function isDigit(character: char): boolean;

        { Same as getString but for numbers. }
        procedure getNumber;

        { Returns the next character. Won't change the current line index. }
        function peekNext: char;

        { Finds an identifier (keywords included). }
        procedure getIdentifier;

    public
        constructor Create(source_: string);

    end;

implementation

constructor TScanner.Create(source_: string);
var i: integer;
begin
    self.source := source;
    start := 1;
    current := 1;
    line := 1;
    scanTokens;
    for i := low(tokens) to high(tokens) do
        writeln(tokens[i].toString);
end;

function TScanner.isAtEnd: boolean;
begin
    Result := current >= length(source);
end;

function TScanner.nextChar: char;
begin
    Result := peek;
    inc(current);
end;

procedure TScanner.addToken(token_type: TokenTypes);
begin
    addToken(token_type, nil);
end;

procedure TScanner.addToken(token_type: TokenTypes; literal: variant); overload;
begin
    setLength(tokens, Length(tokens) + 1);
    insert(Token.Create(token_type, copy(source, start, current), literal, line),
           tokens, High(tokens));
end;

function TScanner.matchCurrent(expected: char): boolean;
begin
    if isAtEnd() then Result := false
    else Result := (nextChar() = expected);
end;

function TScanner.peek: char;
begin
    if isAtEnd() then Result := #0
    else Result := source[current];
end;

function TScanner.peekNext: char;
begin
    if (current + 1) >= length(source) then Result := #0
    else Result := source[current + 1];
end;

// function TScanner.isDigit(character: char): boolean;
// begin
//     Result := '9' >= character >= '0';
// end;

procedure TScanner.getNumber;
begin
    while isDigit(peek) do nextChar;

    if (peek() = '.') and isDigit(peekNext) then
    begin
        nextChar;
        while (isDigit(peek)) do nextChar;
    end;

    addToken(NUMBER, Double(copy(source, start, current)));
end;

procedure TScanner.getIdentifier;
begin
    while (isLetter(peek) or isDigit(peek)) do nextChar;
    // TODO: keywords
    addToken(IDENTIFIER);
end;

procedure TScanner.getString;
begin
    while (peek() <> '"') and not isAtEnd do
    begin
        if (peek() = sLineBreak) then Inc(line);
        nextChar;
    end;

    if isAtEnd then
        raise ESyntaxError.Create(line, high(source), source);
    
    nextChar;

    addToken(TokenTypes.STR, copy(source, start + 1, current - 1));
end;

procedure TScanner.scanTokens;
var
    character: char;

begin
    character := nextChar();

    case character of
        '(': addToken(LEFT_PAREN);
        ')': addToken(RIGHT_PAREN);
        '{': addToken(LEFT_BRACE);
        '}': addToken(RIGHT_BRACE);
        ',': addToken(COMMA);
        '.': addToken(DOT);
        '-': addToken(MINUS);
        '+': addToken(PLUS);
        ';': addToken(SEMICOLON);
        '*': addToken(STAR);
        '!':
            if matchCurrent('=') then
                addToken(BANG_EQUAL)
            else
                addToken(BANG);
        '=':
            if matchCurrent('=') then
                addToken(EQUAL_EQUAL)
            else
                addToken(EQUAL);
        '<':
            if matchCurrent('=') then
                addToken(LESS_EQUAL)
            else
                addToken(LESS);
        '>':
            if matchCurrent('=') then
                addToken(GREATER_EQUAL)
            else
                addToken(GREATER);
        '/':
            if matchCurrent('/') then
                while (peek() <> sLineBreak) and (not isAtEnd()) do nextChar
            else
                addToken(SLASH);
        ' ', #13, #9: // spaces, tabs, \r
            ;
        #10:
            inc(line);
        '"': getString;
        
    else
        if isDigit(character) then getNumber()
        else if isLetter(character) then getIdentifier()
        else raise ESyntaxError.Create(line, current, source);
    end;
end;

end.