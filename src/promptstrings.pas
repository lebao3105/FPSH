{
    This file is a part of the FPOS (Free Pascal Operating System) Shell,
    which known as the fpsh project, a part of the FPOS core component.

    Copyright (C) 2010-2021 Yacine REZGUI.
    Copyright (C) 2024 Le Bao Nguyen.

    This project is licensed in the same license as the FPOS project.
    *********************************************************************
}

unit promptstrings;

{$mode ObjFPC} {$H+}

interface

function PSOne: string;
function PSTwo: string;

implementation

uses internals, sysutils, fpjson, jsonparser, classes;

var
    file_read: TStringList;
    jData: TJSONData;
    jObject: TJSONObject;

    ps1_format: string;
    ps2_string: string;
    type_normal: string;
    type_root: string;
    currdir_name_only: boolean;
    time_format: string;

function PSOne: string;
var
    dollarOrHashtag: string;

begin
    if AmIRoot then
        dollarOrHashtag := type_root
    else
        dollarOrHashtag := type_normal;

    Result := StringReplace(ps1_format, '%(user)s', GetEnvironmentVariable('USERNAME'), [rfReplaceAll]);
    Result := StringReplace(Result, '%(currdir)s', GetCurrentDir, [rfReplaceAll]);
    Result := StringReplace(Result, '%(code)s', IntToStr(Status), [rfReplaceAll]);
    Result := StringReplace(Result, '%(type)s', dollarOrHashtag, [rfReplaceAll]);
    Result := StringReplace(Result, '%(time)s', FormatDateTime(time_format, Now), [rfReplaceAll]);
    Result := Result + ' ';
end;

function PSTwo: string;
begin
    Result := ps2_string + ' ';
end;

initialization

file_read := TStringList.Create;
file_read.LoadFromFile(GetUserDir + '.fshrc');
jData := GetJSON(file_read.Text, true);
jObject := (jData as TJSONObject).Objects['prompt-strings'];

// the second argument of jObject.Get is the default value if
// the key is not here with wanted type
// keep this up-to-date with ../data/fshrc.schema.
ps1_format := jObject.Get('ps1-format', '%(user)s %(currdir)s %(code)s %(type)s');
ps2_string := jObject.Get('ps2-string', '...');
type_normal := jObject.Get('type-normal', '$');
type_root := jObject.Get('type-root', '#');
currdir_name_only := jObject.Get('currdir-name-only', true);
time_format := jObject.Get('time-format', 'dd/mm/yy hh/nn/ss');

jObject.Free;
file_read.Free;
end.