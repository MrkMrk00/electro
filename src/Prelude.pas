unit Prelude;

interface

function IntToStr(I: Integer): string;

implementation

function IntToStr(I: Integer): string;
var
    ret: string;
begin
    Str(I, ret);

    IntToStr := ret;
end;

end.
