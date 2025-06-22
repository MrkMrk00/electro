program Electro;

uses
    FileUtils,
    BufferUtils,
    Tokenizer,
    Parser;

var
    unitSource: TBuffer;
    toks, t: PToken;
    fileName: string;

begin
    if ParamCount() < 1 then
    begin
        WriteLn(StdErr, 'Provide a file name');
        Halt(1);
    end;

    fileName := ParamStr(1);

    unitSource := ReadEntireFile(fileName);
    toks := TokenizeUnit(fileName, unitSource);

    if toks = nil then
    begin
        WriteLn(StdErr, 'failed to tokenize file ', fileName);
        Halt(1);
    end;


    t := toks;
    repeat
        WriteLn(TokenToString(t^));

        t := t^.Next;
    until t = nil;


    WriteLn;
    WriteLn('====================================');
    WriteLn;

    ParseTokens(fileName, toks);

    BufferDispose(unitSource);
    TokenListDispose(toks);
end.
