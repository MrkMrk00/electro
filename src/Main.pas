program Electro;

uses
    FileUtils,
    BufferUtils,
    Tokenizer;

var
    result: TBuffer;
    s: string;
    toks, t: PToken;

begin
    if ParamCount() < 1 then
    begin
        WriteLn(StdErr, 'Provide a file name');
        Halt(1);
    end;

    result := ReadEntireFile(ParamStr(1));

    toks := TokenizeUnit(result);
    if toks = nil then
    begin
        WriteLn(StdErr, 'failed to tokenize file ', ParamStr(1));
        Halt(1);
    end;

    t := toks;

    repeat
        WriteLn(TokenToString(t^));

        t := t^.Next;
    until t = nil;

    BufferDispose(result);
end.
