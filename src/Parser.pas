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

procedure ParseTokens(UnitName: string; TokenList: PToken);

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

// can return nil -> end of list
// function Peek(var T: TParserState): PToken;
// begin
//     if T.TokenList = nil then
//         Exit(nil);
//
//     Peek := T.TokenList^.Next;
// end;

function Match(var T: TParserState; Kind: TTokenKind): Boolean;
var
    next: PToken;
begin
    next := T.TokenList^.Next;

    if next = nil then
        Exit(false);

    Match := next^.Kind = Kind;
end;

function Advance(var T: TParserState): PToken;
var
    cur: PToken;
begin
    cur := T.TokenList;
    T.TokenList := cur^.Next;

    Advance := cur;
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

procedure ParseTokens(UnitName: string; TokenList: PToken);
var
    t: TParserState;
    expressions: array of TExpression;
begin
    t.TokenList := TokenList;
    t.Expressions := nil;

    if t.TokenList = nil then
    begin
        WriteLn(StdErr, '[Parser] token list was empty while parsing ', UnitName, ', exiting...');

        Exit;
    end;

    repeat

    until t.TokenList = nil;
end;

end.
