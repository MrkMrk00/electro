program Greeter;

procedure SayHi(var SomeThing: TSomething);
var
    a: Integer;
    b: string;
begin
    a := 123.22;
    b := 'Tvoje mama';

    SayHi := a + Ord(b[0]);
end;
