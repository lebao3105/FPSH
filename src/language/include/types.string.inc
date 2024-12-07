{ TString implementation }

function TString.valueToString: string;
begin
    Result := value;
end;

// Region: Operators

function TString.plus(right: TCharacter): TString;
begin
    Self.copy(Result);
    AddChar(right.value, Result.value, Result.count.value + 1);
end;

function TString.minus(right: TString): TString;
begin
    raise NotSupportedError.Create('What to do? Remove all occurences or only the first one seen from the string end?');
end;

function TString.multiply(right: TInteger): TString;
begin
    Result := TString.Create(DupeString(value, right.value));
end;

function TString.divide(right: TString): TString;
begin
    // Result := SplitString(value, right.value);
    { TList is not implemented }
end;

function TString.contains(what: TString; caseSensitive: boolean = false): boolean;
begin
    if caseSensitive then
        Result := ContainsStr(value, what.value)
    else
        Result := ContainsText(value, what.value);
end;

// End region

function TString.count: TInteger;
begin
    Result := TInteger.Create(Length(value));
end;

function TString.startsWith(what: TString; caseSensitive: boolean = false): boolean;
begin
    if caseSensitive then
        Result := StartsStr(value, what.value)
    else
        Result := StartsText(value, what.value);
end;

function TString.indexOf(what: TString): TInteger;
begin
    Result := TInteger.Create(Pos(what.value, value));
end;

function TString.fromIndex(what: TInteger): TCharacter;
begin
    if (what.value = 0) then
        raise Exception.Create('Arrays start at 1!')
    else
        Result := TCharacter.Create(char(value[what.value]));
end;

function TString.quoted: TString;
begin
    Result := TString.Create(QuotedStr(value));
end;

function TString.upper: TString;
begin
    Result := TString.Create(UpperCase(value));
end;

function TString.lower: TString;
begin
    Result := TString.Create(LowerCase(value));
end;

{ TCharacter implementation }

constructor TCharacter.Create(charCode: TInteger);
begin
    value := Chr(charCode.value);
end;

function TCharacter.code: TInteger;
begin
    Result := TInteger.Create(Ord(value));
end;

function TCharacter.upper: TCharacter;
begin
    Result := TCharacter.Create(UpperCase(value)[1]);
end;

function TCharacter.lower: TCharacter;
begin
    Result := TCharacter.Create(LowerCase(value));
end;

function TCharacter.valueToString: string;
begin
    Result := string(value);
end;