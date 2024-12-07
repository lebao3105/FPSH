{ TFPSHObject implementation }

constructor TFPSHObject.Create(obj: T);
begin
    value := obj;
end;

function TFPSHObject.ToString(): string;
begin
    Result := Format('%s at %n, holding %s', [ ClassName, InstanceSize, ValueToString() ]);
end;
function TFPSHObject.copy: TFPSHObject;
begin
    Result := TFPSHObject.Create(value);
end;

procedure TFPSHObject.copy(destination: TFPSHObject);
begin
    destination := TFPSHObject.Create(value);
end;

function TFPSHObject.valueToString: string;
begin
    raise UnImplementedError.Create('');
end;