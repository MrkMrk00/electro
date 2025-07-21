program ElectroRepl;

uses Tokenizer, Parser, Interpreter;

var
    shouldEnd: Boolean;
    line: string;
    tokenLst: PToken;
    ast: PAstNode;
    result: TValue;

label
    cleanup;

begin
    shouldEnd := false;
    line := '';
    tokenLst := nil;
    ast := nil;

    while not shouldEnd do
    begin
        Write('Electro REPL > ');

        // Read
        ReadLn(line);
        if line = '' then
            continue;

        tokenLst := TokenizeUnit('[REPL]', line);
        if tokenLst = nil then
        begin
            WriteLn(StdErr, 'failed to parse expression');

            goto cleanup;
        end;

        ast := ParseTokens('[REPL]', tokenLst);
        if ast = nil then
            goto cleanup;

        result := EvaluateExpression(ast^);

        Write('==> ');

        case result.Kind of
            tyString: WriteLn(result.StrVal);
            tyNumber: begin
                WriteLn(result.NumVal:0:2);
            end;
            tyInvalid: begin
                WriteLn('invalid expression: ', result.ErrorMessage);
            end;
        end;

cleanup:
        TokenListDispose(tokenLst);
        AstDispose(ast);
    end;
end.
