function TInteger.valueToString: string;
begin
    Result := IntToStr(value);
end;

constructor TInteger.Create(str: string);
begin
    Create(StrToInt(str));
end;