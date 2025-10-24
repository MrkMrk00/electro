program Electro;

uses
    FileUtils,
    BufferUtils,
    Tokenizer,
    Parser,
    Interpreter;

var
    unitSource: string;
    toks: PToken;
    fileName: string;
    expression, next: PAstNode;
    buf: TBuffer;

begin
    if ParamCount() < 1 then
    begin
        WriteLn(StdErr, 'Provide a file name');
        Halt(1);
    end;

    fileName := ParamStr(1);

    buf := ReadEntireFile(fileName);
    unitSource := BinaryToString(buf);
    toks := TokenizeUnit(fileName, unitSource);

    if toks = nil then
    begin
        WriteLn(StdErr, 'failed to tokenize file ', fileName);
        Halt(1);
    end;

    expression := ParseTokens(fileName, toks);

    while expression <> nil do
    begin
        WriteLn('================================================');
        WriteLn;
        PrintExpression(expression);
        WriteLn('= ', Trunc(EvaluateExpression(expression^).NumVal));
        WriteLn;

        next := expression^.Next;

        expression := next;
    end;

    WriteLn('================================================');

    BufferDispose(buf);
    TokenListDispose(toks);
end.
