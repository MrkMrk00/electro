program Electro;

uses
    FileUtils,
    BufferUtils,
    Tokenizer;

var
    unitSource: TBuffer;
    toks, t: PToken;

begin
    if ParamCount() < 1 then
    begin
        WriteLn(StdErr, 'Provide a file name');
        Halt(1);
    end;

    unitSource := ReadEntireFile(ParamStr(1));
    toks := TokenizeUnit(ParamStr(1), unitSource);

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

    BufferDispose(unitSource);
    TokenListDispose(toks);
end.
