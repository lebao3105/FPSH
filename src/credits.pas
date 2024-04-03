unit credits;

interface

const ThanksList: array [1..8] of String = (
    'Brendan       (http://www.osdever.net\bkerndev)',
    'Mike          (http://www.brokenthorn.com)',
    'JamesM        (http://www.jamesmolloy.co.uk)',
    'SirStorm25    (???)',
    'Uranium-239   (???)',
    'Napalm        (???)',
    'Xiaoming      (http://en.skelix.org)',
    'Yacine REZGUI (yacine.rezgui@gmail.com)');

procedure ShowThanks;

implementation

procedure ShowThanks;
var
    i: Byte;

begin
    WriteLn('Thanks for many contributors who have made FPOS better!');
    for i := Low(ThanksList) to High(ThanksList) do
        WriteLn(ThanksList[i] + LineEnding);
end;

end.