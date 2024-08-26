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

uses tokens, fpshlang, character, variants, strutils, sysutils;

type
    TScanner = class(TObject)
    private

        { The string to be scanned }
        source: string;

        { The current line of the source string }
        line: string;

        { Tokens }
        tokens: array of Token;

        { Start index of the object (operator, identifier etc) }
        start: integer;

        { The current index of the source string }
        current: integer;

        { The current line number of the source string }
        lineno: integer;

        { Times ( or ) got encountered and not matched. }
        paren_times: integer;

        { Times one of 2 braces got encountered and not matched. }
        braces_times: integer;

        { The source string, splited by new lines character }
        source_lines: array of ansistring;

        { IsEOL's getter }
        function getIsEOL: boolean;

        { IsEOF's getter }
        function getIsEOF: boolean;

        { currentLine's getter }
        function getCurrentLine: string;

        { currentLineNo's getter }
        function getCurrentLineNo: integer;

        { currentLineNo's setter}
        procedure setCurrentLineNo(num: integer);

        { Scans for tokens, line by line. }
        procedure scanTokens;

        { Returns peek(), and jumps to the next character. }
        function nextChar: char;

        { Returns the current character in line. #0 (null character) if we're in EOL. }
        function peek: char;

        { Checks if the next character matches the expected one. }
        function matchNext(expected: char): boolean;

        { Adds a new token. }
        procedure addToken(token_type: TokenTypes);

        { Adds a new token (overload) }
        procedure addToken(token_type: TokenTypes; literal: variant); overload;

        function KeywordToTokenType(lexeme: string): TokenTypes;

        { Finds string (starts and ends with ") }
        procedure getString;

        // function isDigit(character: char): boolean;

        { Same as getString but for numbers. }
        procedure getNumber;

        { Returns the next character. Won't change the current line index. }
        function peekNext: char;

        { Finds an identifier (keywords included). }
        procedure getIdentifier;

        { Increase the current line, if the source string has more than 1 line. }
        function incLine: boolean;

    public

        { Constructor. }
        constructor Create(source_: string);

        { Returns whether we've reached EOLine or not }
        property isEOL: boolean read getIsEOL;

        { Returns whether we're in EOFile }
        property isEOF: boolean read getIsEOF;

        { The current line of source string, followed by its line number. }
        property currentLine: ansistring read getCurrentLine;

        { The current line number }
        property currentLineNo: integer read getCurrentLineNo write setCurrentLineNo;
    end;

implementation

/// Constructor

constructor TScanner.Create(source_: string);
var i: integer;
begin
    self.source := source_;
    source_lines := SplitString(source, sLineBreak);
    if Length(source_lines) = 0 then
        source_lines := [source];

    start := 1;
    current := 1;
    lineno := 1;

    paren_times := 0;
    braces_times := 0;

    scanTokens;
    for i := low(tokens) to high(tokens) do
        writeln(tokens[i].toString);
end;


/// Property getters and setters

function TScanner.getIsEOL: boolean;
begin
    Result := current > Length(currentLine);
end;

function TScanner.getIsEOF: boolean;
begin
    Result := (lineno = length(source_lines)) and IsEOL;
end;

function TScanner.getCurrentLine: ansistring;
begin
    Result := source_lines[lineno];
end;

function TScanner.getCurrentLineNo: integer;
begin
    Result := lineno;
end;

procedure TScanner.setCurrentLineNo(num: integer);
begin
    lineno := num;
    current := 1;
    start := 1;
end;

/// Utility functions

function TScanner.peek: char;
begin
    if isEOL or isEOF then Result := #0
    else Result := currentLine[current];
end;

function TScanner.peekNext: char;
begin
    Result := currentLine[current + 1];
end;

function TScanner.nextChar: char;
begin
    Result := peekNext;
    inc(current);
end;

function TScanner.matchNext(expected: char): boolean;
begin
    Result := peekNext = expected;
end;

function TScanner.incLine: boolean;
begin
    Result := Length(source_lines) > 1;
    if Result then setCurrentLineNo(lineno + 1);
end;

/// addToken()

procedure TScanner.addToken(token_type: TokenTypes);
begin
    addToken(token_type, nil);
end;

procedure TScanner.addToken(token_type: TokenTypes; literal: variant); overload;
begin
    SetLength(tokens, Length(tokens) + 1);
    Insert(
        Token.Create(
            token_type,
            copy(currentLine, start, current - start),
            literal, lineno
        ),
        tokens, High(tokens)
    );
    inc(current);
    start := current;
end;

/// Scanners

procedure TScanner.getNumber;
var hasDecimal: boolean = false;
begin
    while IsDigit(peek) do nextChar;

    if peek = '.' then begin
        hasDecimal := true;
        nextChar;
        while IsDigit(peek) do nextChar;
    end;

    if hasDecimal then
        addToken(NUMBER, StrToFloat(copy(source, start, current - start)))
    else
        addToken(NUMBER, StrToInt(copy(source, start, current - start)));
end;

procedure TScanner.getString;
begin
    while peek <> '"' do nextChar;
    addToken(TokenTypes.STR, copy(source, start + 1, current - 1 - start))
end;

procedure TScanner.getIdentifier;
begin
    while IsLetterOrDigit(peek) do nextChar;
  
    addToken(KeywordToTokenType(copy(currentLine, start, current - start)));
end;

function TScanner.KeywordToTokenType(lexeme: string): TokenTypes;
begin
    case lexeme of
        'and': Result := AND_;
        'class': Result := CLASS_;
        'else': Result := ELSE_;
        'false': Result := FALSE_;
        'fun': Result := FUN;
        'for': Result := FOR_;
        'if': Result := IF_;
        'nil': Result := NIL_;
        'or': Result := OR_;
        'give': Result := GIVE;
        'super': Result := SUPER;
        'this': Result := THIS;
        'true': Result := TRUE_;
        'var': Result := VAR_;
        'while': Result := WHILE_;
    else
        Result := IDENTIFIER;
    end;
end;

procedure TScanner.scanTokens;
var
    character: char;
    ln: integer;
begin
    for ln := Low(source_lines) to High(source_lines) do
    begin
        currentLineNo := ln;
        character := nextChar;

        case character of
            '(': begin addToken(LEFT_PAREN); inc(paren_times); end;
            ')': begin addToken(RIGHT_PAREN); dec(paren_times); end;
            '{': begin addToken(LEFT_BRACE); inc(braces_times); end;
            '}': begin addToken(RIGHT_BRACE); dec(braces_times); end;
            '+': addToken(PLUS);
            '-': addToken(MINUS);
            '"': getString;
            '/': if matchNext('/') then continue else addToken(SLASH);
            '#', #10: continue;
            '0'..'9': getNumber;
            '*': addToken(STAR);
            ';': addToken(SEMICOLON);
            '=': if matchNext('=') then addToken(EQUAL_EQUAL) else addToken(EQUAL);
            '!': if matchNext('=') then addToken(BANG_EQUAL) else addToken(BANG);
            ' ', #13, #9: ;
            // #10: self.incLine;
            'a'..'z', 'A'..'Z': getIdentifier;
        else
            if paren_times > 0 then raise EScript.Create(lineno, current, currentLine, 'Unmatched parentheses');
            if braces_times > 0 then raise EScript.Create(lineno, current, currentLine, 'Unmatched braces');
            raise EScript.Create(lineno, current, currentLine, 'Unknown character');
        end;
    end;
end;

end.