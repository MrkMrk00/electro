program Electro;

uses
    FileUtils,
    BufferUtils,
    Tokenizer,
    Parser,
    Interpreter;

var
    unitSource: TBuffer;
    toks: PToken;
    fileName: string;
    expression: PExpression;

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

    expression := ParseTokens(fileName, toks);
    while expression <> nil do
    begin
        PrintExpression(expression);
        WriteLn('= ', Trunc(EvaluateExpression(expression^).FloatVal));

        expression := expression^.Next;
    end;

    BufferDispose(unitSource);
    TokenListDispose(toks);
end.
