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

uses internals, sysutils, fpjson, jsonparser, classes, utilities
     {$if defined(WINDOWS) or defined(MSWINDOWS)}, shellapi {$else}, baseunix {$endif};

var
    file_read: TStringList;
    jData: TJSONData;
    jObject: TJSONObject;

    ps1_format: string = '%(user)s %(currdir)s %(code)s %(type)s';
    ps2_string: string = '...';
    currdir_name_only: boolean = true;
    time_format: string = 'dd/mm/yy hh/nn/ss';

    amIRoot: boolean;

function PSOne: string;
var bang: string;
begin

    // Current user
    Result := StringReplace(ps1_format, '%(user)s', GetEnvironmentVariable('USER'), [rfReplaceAll]);

    // Current directory (name only if asked to)
    if currdir_name_only then
        Result := StringReplace(Result, '%(currdir)s', GetCurrentDir, [rfReplaceAll])
    else
        Result := StringReplace(Result, '%(currdir)s', ExtractFileName(GetCurrentDir), [rfReplaceAll]);

    // Last exit code
    Result := StringReplace(Result, '%(code)s', IntToStr(Status), [rfReplaceAll]);

    // Current time
    Result := StringReplace(Result, '%(time)s', FormatDateTime(time_format, Now), [rfReplaceAll]);

    // That indicator, which shows you're root or not
    if amIRoot then bang := '#' else bang := '$';
    Result := StringReplace(Result, '%(type)s', bang, [rfReplaceAll]);

    // An extra space for an easier look. Should this be optional?
    Result := Result + ' ';
end;

function PSTwo: string;
begin
    Result := ps2_string + ' ';
end;

{$if defined(WINDOWS) or defined(MSWINDOWS)}
function IsUserAnAdmin: boolean; external shell32;
{$endif}

initialization

{$if defined(WINDOWS) or defined(MSWINDOWS)}
amIRoot := IsUserAnAdmin();
{$else}
amIRoot := (FpGetEUID() = 0);
{$endif}

try
    file_read := TStringList.Create;
    file_read.LoadFromFile(GetUserDir + '.fshrc');
    jData := GetJSON(file_read.Text, true);
    jObject := (jData as TJSONObject).Objects['prompt-strings'];

    // the second argument of jObject.Get is the default value if
    // the key is not here with wanted type
    // keep this up-to-date with ../data/fshrc.schema.
    ps1_format := jObject.Get('ps1-format', '%(user)s %(currdir)s %(code)s %(type)s');
    ps2_string := jObject.Get('ps2-string', '...');
    currdir_name_only := jObject.Get('currdir-name-only', true);
    time_format := jObject.Get('time-format', 'dd/mm/yy hh/nn/ss');

    jObject.Free;
    file_read.Free;
except
    on E: Exception do
    begin
        printdebug(E.Message);
        printdebug('Will use default options.');
    end;
end;

printdebug(Format('Prompt string 1: %s', [ps1_format]));
printdebug(Format('Prompt string 2: %s', [ps2_string]));
printdebug(Format('Only show the current directory NAME - NOT path in PS1: %s', [booltostr(currdir_name_only, true)]));
printdebug(Format('Time format: %s', [time_format]));

end.