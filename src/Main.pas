program Electro;

uses
    FileUtils,
    BufferUtils,
    Tokenizer,
    Parser;

var
    unitSource: TBuffer;
    toks: PToken;
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

    ParseTokens(fileName, toks);

    BufferDispose(unitSource);
    TokenListDispose(toks);
end.
