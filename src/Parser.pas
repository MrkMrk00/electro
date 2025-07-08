unit Parser;

interface

uses Tokenizer;

const MAX_VAR_DECLS = 100;

type
    TAstNodeKind = (
        astStmtVarDecl,
        astExprLiteral, astExprUnary, astExprBinary, astExprGrouping
    );

    TVariableBind = record
        Name: string;
        TypeName: string;
    end;
    TVariableBindArray = array [0..0] of TVariableBind;
    TVarList = record
        Variables: ^TVariableBindArray;
        Count, Capacity: Integer;
    end;

    PAstNode = ^TAstNode;
    TAstNode = record
        Kind: TAstNodeKind;
        Next: PAstNode;

        // This is kind of hell. If this was slightly better, I believe this language
        // would still be used today :/
        // 1) Cannot have fields with the same name in multiple cases :/
        // 2) Accessing the fields is not checked :/ -> this would put it on par with Rust and Zig.
        case TAstNodeKind of
            astExprLiteral:  (Literal: TToken);
            astExprUnary:    (PrefixOperator: TToken; Expression: PAstNode);
            astExprBinary:   (Left, Right: PAstNode; Operator: TToken);
            astExprGrouping: (Inner: PAstNode);
            astStmtVarDecl:  (VarDecls: TVarList);
    end;

function  ParseTokens(UnitName: string; TokenList: PToken): PAstNode;
procedure PrintExpression(Expression: PAstNode);
procedure ExpressionDispose(var Expr: PAstNode);

implementation

{
    === Recursive descent parser
    https://craftinginterpreters.com/parsing-expressions.html

    StmtVarDecl := "var" ( VarBind ; )*
    VarBind     := IDENTIFIER : IDENTIFIER

    Expression  := Equality ;
    Equality    := Comparison ( ( "=" | "<>" ) Comparison )* ;  // This is wrong for Pascal, since equality has low precedence.
    Comparison  := Term ( ( "<" | ">" | "<=" | ">=" ) Term )* ;
    Term        := Factor ( ( "+" | "-" ) Factor )* ;
    Factor      := Unary ( ( "div" | "mod" | "*" ) Unary )* ;
    Unary       := ( "-" | "+" ) Unary
                | Primary ;

    Primary     := NUMBER | STRING | CHARCODE | IDENTIFIER
                | ( "(" Expression ")" ) ;

                // true, false and nil are enum constructors - (IDENTIFIER)
                // part of the type system of the language not keywords
}

type
    TParserState = record
        TokenList: PToken;
        Expressions: PAstNode;
    end;

function EatExpression(var T: TParserState): PAstNode; forward;

function NewBinaryExpression(Left: PAstNode; Operator: TToken; Right: PAstNode): PAstNode;
var
    ret: PAstNode;
begin
    New(ret);
    ret^.Kind := astExprBinary;
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
    T.TokenList := T.TokenList^.Next;

    Advance := cur;
end;

function EatPrimary(var T: TParserState): PAstNode;
var
    expr: PAstNode;
    literal: TToken;
begin
    case T.TokenList^.Kind of
        tokIdentifier, tokString, tokNumber,
        tokCharcode: begin
            literal := Advance(T)^;
            literal.Next := nil;

            New(expr);
            expr^.Kind := astExprLiteral;
            expr^.Literal := literal;

            EatPrimary := expr;
        end;

        tokLParen: begin
            // eat "("
            Advance(T);

            New(expr);
            expr^.Kind := astExprGrouping;
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

function EatUnary(var T: TParserState): PAstNode;
var
    expr: PAstNode;
    prefixOperator: TToken;
begin
    // Is unary with sign prefix
    if Match(T, tokMinus) or Match(T, tokPlus) then
    begin
        prefixOperator := Advance(T)^;

        New(expr);
        expr^.Kind := astExprUnary;
        expr^.PrefixOperator := prefixOperator;
        expr^.Expression := EatUnary(T);

        EatUnary := expr;
    end
    else
        EatUnary := EatPrimary(T);
end;

function EatFactor(var T: TParserState): PAstNode;
var
    expr, right: PAstNode;
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

function EatTerm(var T: TParserState): PAstNode;
var
    expr, right: PAstNode;
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

function EatComparison(var T: TParserState): PAstNode;
var
    expr, right: PAstNode;
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

function EatEquality(var T: TParserState): PAstNode;
var
    expr, right: PAstNode;
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

function EatExpression(var T: TParserState): PAstNode;
begin
    EatExpression := EatEquality(T);
end;

function EatVarDecl(var T: TParserState): PAstNode;
var
    astNode: PAstNode;
    variables: TVarList;
    varName, varType: string;
begin
    Advance(T);

    variables.Count := 0;
    variables.Capacity := 4;
    variables.Variables := GetMem(SizeOf(TVariableBind) * variables.Capacity);

    while true do
    begin
        if variables.Count = variables.Capacity then
        begin
             variables.Capacity := variables.Capacity * 2;
             variables.Variables := ReAllocMem(variables.Variables, SizeOf(TVariableBind) * variables.Capacity);
        end;

        if not Match(T, tokIdentifier) then
            break;

        varName := Advance(T)^.Literal;

        if not Match(T, tokColon) then begin WriteLn(StdErr, '0 invalid statement var decl'); Halt(1); end;
        Advance(T);

        if not Match(T, tokIdentifier) then begin WriteLn(StdErr, '1 invalid statement var decl'); Halt(1); end;
        varType := Advance(T)^.Literal;

        variables.Variables^[variables.Count].Name := varName;
        variables.Variables^[variables.Count].TypeName := varType;
        variables.Count := variables.Count + 1;

        if not Match(T, tokSemi) then begin WriteLn(StdErr, 'tokSemi expected after statement'); Halt(1) end;
    end;

    if variables.Count = 0 then
    begin
        WriteLn(StdErr, 'invalid statement var decl');
        Halt(1);
    end;

    New(astNode);
    astNode^.Kind := astStmtVarDecl;
    astNode^.VarDecls := variables;

    EatVarDecl := astNode;
end;

function EatStatement(var T: TParserState): PAstNode;
begin
    if (T.TokenList^.Kind = tokIdentifier)
        and (T.TokenList^.Literal = 'var') then
        EatStatement := EatVarDecl(T)
    else
        EatStatement := EatExpression(T);
end;

procedure ExpressionDispose(var Expr: PAstNode);
begin
    if Expr = nil then
        Exit;

    case Expr^.Kind of
        astExprUnary: ExpressionDispose(Expr^.Expression);
        astExprBinary: begin
            ExpressionDispose(Expr^.Left);
            ExpressionDispose(Expr^.Right);
        end;
        astExprGrouping: ExpressionDispose(Expr^.Inner);
    end;

    Dispose(Expr);
    Expr := nil;
end;

procedure ReverseExpressions(var Head: PAstNode);
var
    cur, prev, next: PAstNode;
begin
    if (Head = nil) or (Head^.Next = nil) then
        Exit;

    prev := nil;
    next := nil;
    cur := Head;

    while cur <> nil do
    begin
        next := cur^.Next;
        cur^.Next := prev;

        prev := cur;
        cur := next;
    end;

    Head := prev;
end;

function ParseTokens(UnitName: string; TokenList: PToken): PAstNode;
var
    t: TParserState;
    expr: PAstNode;
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

        case t.TokenList^.Kind of
            tokIdentifier: begin
                if t.TokenList^.Literal = 'var' then
                    expr := EatVarDecl(t);
            end;
        else
            expr := EatExpression(t);
        end;

        expr^.Next := t.Expressions;
        t.Expressions := expr;
    end
    until t.TokenList = nil;

    ReverseExpressions(t.Expressions);

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

function PrintExpressionImpl(Expression: PAstNode; Depth: Integer): string;
var
    i: Integer;
begin
    case Expression^.Kind of
        astExprGrouping: PrintExpressionImpl(Expression^.Inner, Depth);
        astExprLiteral:
            WriteLn(IndentString('Literal(' + TokenToString(Expression^.Literal) + ')', Depth));
        astExprUnary: begin
            WriteLn(IndentString('Unary(', Depth));
            WriteLn(IndentString(TokenToString(Expression^.PrefixOperator), Depth + 1));
            PrintExpressionImpl(Expression^.Expression, Depth + 1);
            WriteLn(IndentString(')', Depth));
        end;
        astExprBinary: begin
            WriteLn(IndentString('Binary(', Depth));
            PrintExpressionImpl(Expression^.Left, Depth + 1);
            WriteLn(IndentString(TokenToString(Expression^.Operator), Depth + 1));
            PrintExpressionImpl(Expression^.Right, Depth + 1);
            WriteLn(IndentString(')', Depth));
        end;
        astStmtVarDecl: begin
            for i := 0 to Expression^.VarDecls.Count - 1 do
            begin
                WriteLn('(variable "', Expression^.VarDecls.Variables^[i].Name, '" of type "', Expression^.VarDecls.Variables^[i].TypeName, '")');
            end;
        end;
    end;
end;

procedure PrintExpression(Expression: PAstNode);
begin
    WriteLn(PrintExpressionImpl(Expression, 0));
end;

end.
