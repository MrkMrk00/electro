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
    expression, next: PExpression;
    buf: TBuffer;

begin
    if ParamCount() < 1 then
    begin
        WriteLn(StdErr, 'Provide a file name');
        Halt(1);
    end;

    if (ParamStr(1) = '-') and (ParamCount() >= 2) then
    begin
        unitSource := ParamStr(2);
        toks := TokenizeUnit('REPL', unitSource);
        expression := ParseTokens('REPL', toks);

        while expression <> nil do
        begin
            PrintExpression(expression);
            WriteLn('= ', Trunc(EvaluateExpression(expression^).NumVal));

            next := expression^.Next;
            ExpressionDispose(expression);

            expression := next;
        end;

        TokenListDispose(toks);

        Exit;
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
        PrintExpression(expression);
        WriteLn('= ', Trunc(EvaluateExpression(expression^).NumVal));

        expression := expression^.Next;
    end;

    BufferDispose(buf);
    TokenListDispose(toks);
end.
