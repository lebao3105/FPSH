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

uses internals, sysutils, pasyaml, classes;

var
    file_read: TStringList;
    YamlFile: TYamlFile;

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

YamlFile := TYamlFile.Create;
YamlFile.Parse(file_read.Text);

with YamlFile.Value['settings'] do
begin
    with Value['prompt-strings'] do begin
        ps1_format := Value['ps1-format'].AsString;
        ps2_string := Value['ps2'].AsString;
        type_normal := Value['type-normal'].AsString;
        type_root := Value['type-root'].AsString;
        currdir_name_only := boolean(Value['currdir-name-only'].AsInteger);
        time_format := Value['time-format'].AsString;
    end;
end;

file_read.Free;
end.