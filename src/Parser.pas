unit Parser;

interface

uses Tokenizer;

type
    TExpressionKind = ( exprLiteral, exprUnary, exprBinary, exprGrouping );

    PExpression = ^TExpression;
    TExpression = record
        Kind: TExpressionKind;
        Next: PExpression;

        // This is kind of hell. If this was slightly better, I believe this language
        // would still be used today :/
        // 1) Cannot have fields with the same name in multiple cases :/
        // 2) Accessing the fields is not checked :/ -> this would put it on par with Rust and Zig.
        case TExpressionKind of
            exprLiteral:  (Literal: TToken);
            exprUnary:    (PrefixOperator: TToken; Expression: PExpression);
            exprBinary:   (Left, Right: PExpression; Operator: TToken);
            exprGrouping: (Inner: PExpression);
    end;

function  ParseTokens(UnitName: string; TokenList: PToken): PExpression;
procedure PrintExpression(Expression: PExpression);
procedure ExpressionDispose(var Expr: PExpression);

implementation

{
    === Recursive descent parser
    https://craftinginterpreters.com/parsing-expressions.html

    Expression  := Equality ;
    Equality    := Comparison ( ( "=" | "<>" ) Comparison )* ;  // This is wrong for Pascal, since equality has low precedence.
    Comparison  := Term ( ( "<" | ">" | "<=" | ">=" ) Term )* ;
    Term        := Factor ( ( "+" | "-" ) Factor )* ;
    Factor      := Unary ( ( "div" | "mod" | "*" ) Unary )* ;
    Unary       := ( "-" | "+" ) Unary
                | Primary;

    Primary     := NUMBER | STRING | CHARCODE | IDENTIFIER
                | ( "(" Expression ")" ) ;

                // true, false and nil are enum constructors - (IDENTIFIER)
                // part of the type system of the language not keywords
}

type
    TParserState = record
        TokenList: PToken;
        Expressions: PExpression;
    end;

function EatExpression(var T: TParserState): PExpression; forward;

function NewBinaryExpression(Left: PExpression; Operator: TToken; Right: PExpression): PExpression;
var
    ret: PExpression;
begin
    New(ret);
    ret^.Kind := exprBinary;
    ret^.Left := Left;
    ret^.Operator := Operator;
    ret^.Right := Right;

    NewBinaryExpression := ret;
end;

function Match(var T: TParserState; Kind: TTokenKind): Boolean;
begin
    if t.TokenList = nil then
        Exit(false);

    Match := t.TokenList^.Kind = Kind;
end;

function MatchKW(var T: TParserState; Keyword: string): Boolean;
var
    token: PToken;
begin
    token := T.TokenList;

    if (token = nil) or (token^.Kind <> tokIdentifier) then
        Exit(false);

    MatchKW := token^.Literal = Keyword;
end;

function Advance(var T: TParserState): PToken;
var
    cur: PToken;
begin
    cur := T.TokenList;
    T.TokenList := cur^.Next;

    Advance := cur;
end;

function EatPrimary(var T: TParserState): PExpression;
var
    expr: PExpression;
    literal: TToken;
begin
    case T.TokenList^.Kind of
        tokIdentifier, tokString, tokNumber,
        tokCharcode: begin
            literal := Advance(T)^;
            literal.Next := nil;

            New(expr);
            expr^.Kind := exprLiteral;
            expr^.Literal := literal;

            EatPrimary := expr;
        end;

        tokLParen: begin
            // eat "("
            Advance(T);

            New(expr);
            expr^.Kind := exprGrouping;
            expr^.Inner := EatExpression(T);

            // TODO: handle gracefully
            if not Match(T, tokRParen) then
            begin
                WriteLn(StdErr,
                    'unexpected token "',
                    TokenToString(T.TokenList^),
                    ' expected "tokRParen".'
                );

                Halt(1);
            end;

            // eat ")"
            Advance(T);

            EatPrimary := expr;
        end;
    else
    begin
        // TODO: handle gracefully
        WriteLn(StdErr, 'unexpected token "', TokenToString(T.TokenList^), '"');
        Halt(1);
    end
    end;
end;

function EatUnary(var T: TParserState): PExpression;
var
    expr: PExpression;
    prefixOperator: TToken;
begin
    // Is unary with sign prefix
    if Match(T, tokMinus) or Match(T, tokPlus) then
    begin
        prefixOperator := Advance(T)^;

        New(expr);
        expr^.Kind := exprUnary;
        expr^.PrefixOperator := prefixOperator;
        expr^.Expression := EatUnary(T);

        EatUnary := expr;
    end
    else
        EatUnary := EatPrimary(T);
end;

function EatFactor(var T: TParserState): PExpression;
var
    expr, right: PExpression;
    operator: TToken;
begin
    expr := EatUnary(T);

    while Match(T, tokStar) or MatchKW(T, 'mod') or MatchKW(T, 'div') do
    begin
        operator := Advance(T)^;
        operator.Next := nil;
        right := EatUnary(T);

        expr := NewBinaryExpression(expr, operator, right);
    end;

    EatFactor := expr;
end;

function EatTerm(var T: TParserState): PExpression;
var
    expr, right: PExpression;
    operator: TToken;
begin
    expr := EatFactor(T);

    while Match(T, tokMinus) or Match(T, tokPlus) do
    begin
        operator := Advance(T)^;
        operator.Next := nil;
        right := EatFactor(T);

        expr := NewBinaryExpression(expr, operator, right);
    end;

    EatTerm := expr;
end;

function EatComparison(var T: TParserState): PExpression;
var
    expr, right: PExpression;
    operator: TToken;
begin
    expr := EatTerm(T);

    while Match(T, tokLt) or Match(T, tokLte) or
        Match(T, tokGt) or Match(T, tokGte) do
    begin
        operator := Advance(T)^;
        operator.Next := nil;
        right := EatTerm(T);

        expr := NewBinaryExpression(expr, operator, right);
    end;

    EatComparison := expr;
end;

function EatEquality(var T: TParserState): PExpression;
var
    expr, right: PExpression;
    operator: TToken;
begin
    expr := EatComparison(T);

    while Match(T, tokEq) or Match(T, tokNeq) do
    begin
        operator := Advance(T)^;
        operator.Next := nil;
        right := EatComparison(T);

        expr := NewBinaryExpression(expr, operator, right);
    end;

    EatEquality := expr;
end;

function EatExpression(var T: TParserState): PExpression;
begin
    EatExpression := EatEquality(T);
end;

procedure ExpressionDispose(var Expr: PExpression);
begin
    if Expr = nil then
        Exit;

    case Expr^.Kind of
        exprUnary: ExpressionDispose(Expr^.Expression);
        exprBinary: begin
            ExpressionDispose(Expr^.Left);
            ExpressionDispose(Expr^.Right);
        end;
        exprGrouping: ExpressionDispose(Expr^.Inner);
    end;

    Dispose(Expr);
    Expr := nil;
end;

function ParseTokens(UnitName: string; TokenList: PToken): PExpression;
var
    t: TParserState;
    expr: PExpression;
begin
    t.TokenList := TokenList;
    t.Expressions := nil;

    if t.TokenList = nil then
    begin
        WriteLn(StdErr, '[Parser] token list was empty while parsing ', UnitName, ', exiting...');

        Exit;
    end;

    repeat
    begin
        if t.TokenList^.Kind = tokSemi then
        begin
            t.TokenList := t.TokenList^.Next;
            continue;
        end;

        expr := EatExpression(t);
        expr^.Next := t.Expressions;
        t.Expressions := expr;
    end
    until t.TokenList = nil;

    ParseTokens := t.Expressions;
end;

function IndentString(Str: string; Depth: Integer): string;
var
    ret: string;
begin
    ret := '';

    for Depth := Depth downto 1 do
    begin
        ret := ret + '    ';
    end;

    IndentString := ret + Str;
end;

function PrintExpressionImpl(Expression: PExpression; Depth: Integer): string;
begin
    case Expression^.Kind of
        exprLiteral: begin
            PrintExpressionImpl := IndentString(
                'Literal(' + TokenToString(Expression^.Literal) + ')',
                Depth
            );
        end;
        exprUnary:
            PrintExpressionImpl := IndentString(
                'Unary('
                    + IndentString(TokenToString(Expression^.PrefixOperator), Depth)
                    + NEWLINE
                    + PrintExpressionImpl(Expression^.Expression, Depth + 1)
                    + NEWLINE
                    + ')',
                Depth
            );
        exprBinary:
            PrintExpressionImpl := IndentString(
                'Binary('
                    + NEWLINE
                    + PrintExpressionImpl(Expression^.Left, Depth + 1)
                    + NEWLINE
                    + IndentString(TokenToString(Expression^.Operator), Depth + 1)
                    + NEWLINE
                    + PrintExpressionImpl(Expression^.Right, Depth + 1)
                    + NEWLINE
                    + IndentString(')', Depth),
                Depth
            );
        exprGrouping:
            PrintExpressionImpl := IndentString(
                '('
                    + NEWLINE
                    + PrintExpressionImpl(Expression^.Inner, Depth + 1)
                    + NEWLINE
                    + ')',
                Depth
            );
    end;
end;

procedure PrintExpression(Expression: PExpression);
begin
    WriteLn(PrintExpressionImpl(Expression, 0));
end;

end.
