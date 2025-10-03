unit Parser;

interface

uses Prelude, Tokenizer, Containers;

const
    MAX_VAR_DECLS = 100;

    KW_PROGRAM = 'program';
    KW_INTERFACE = 'interface';
    KW_IMPLEMENTATION = 'implementation';

    KW_TYPE = 'type';
    KW_RECORD = 'record';
    KW_ARRAY = 'array';

    KW_BEGIN = 'begin';
    KW_END = 'end';
    KW_CASE = 'case';
    KW_OF = 'of';
    KW_FOR = 'for';
    KW_TO = 'to';
    KW_DO = 'do';
    KW_WHILE = 'while';
    KW_REPEAT = 'repeat';
    KW_UNTIL = 'until';
    KW_IF = 'if';
    KW_ELSE = 'else';
    KW_THEN = 'then';
    KW_GOTO = 'goto';
    KW_LABEL = 'label';
    KW_CONTINUE = 'continue';
    KW_BREAK = 'break';
    KW_EXIT = 'exit';

    KW_VAR = 'var';
    KW_CONST = 'const';
    KW_PROCEDURE = 'procedure';
    KW_FUNCTION = 'function';

    KW_AND = 'and';
    KW_OR = 'or';
    KW_NOT = 'not';
    KW_XOR = 'xor';
    KW_DIV = 'div';
    KW_MOD = 'mod';

    KEYWORDS: array of string = (
        KW_PROGRAM, KW_INTERFACE, KW_IMPLEMENTATION,
        KW_TYPE, KW_RECORD, KW_ARRAY,
        KW_BEGIN, KW_END, KW_CASE,
        KW_OF, KW_FOR, KW_TO,
        KW_DO, KW_WHILE, KW_REPEAT,
        KW_UNTIL, KW_IF, KW_ELSE,
        KW_THEN, KW_GOTO, KW_LABEL,
        KW_CONTINUE, KW_BREAK, KW_EXIT,
        KW_VAR, KW_CONST, KW_PROCEDURE,
        KW_FUNCTION,
        KW_AND, KW_OR, KW_NOT,
        KW_XOR, KW_DIV, KW_MOD
    );

type
    TAstNodeKind = (
        // Non-statement expressions
        astStmtEmpty,           // Empty statement = just ;
        astStmtVarDecl,         // var A: TypeA; B: TypeB; ...

        // Expressions
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

        case TAstNodeKind of
            astStmtEmpty:    ();
            astExprLiteral:  (Literal: TToken);
            astExprUnary:    (PrefixOperator: TToken; Expression: PAstNode);
            astExprBinary:   (Left, Right: PAstNode; Operator: TToken);
            astExprGrouping: (Inner: PAstNode);
            astStmtVarDecl:  (VarDecls: TVarList);
    end;

function  ParseTokens(UnitName: string; TokenList: PToken): PAstNode;
procedure PrintExpression(Expression: PAstNode);
procedure AstDispose(var ast: PAstNode);

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
        UnitName: string;
        TokenList: PToken;
        Ast: PAstNode;
        Errors: TStringArray;
    end;

function ParserStateCreate(unitName: string; tokens: PToken): TParserState;
var
    parserState: TParserState;
begin
    parserState.UnitName := unitName;
    parserState.TokenList := tokens;
    parserState.Ast := nil;

    StringArrayInit(parserState.Errors);

    ParserStateCreate := parserState;
end;

function EatExpression(var T: TParserState): PAstNode; forward;

function IsAtEnd(var T: TParserState): Boolean;
begin
    IsAtEnd := T.TokenList = nil;
end;

function Peek(var t: TParserState): TToken;
var
    tok: TToken;
begin
    if IsAtEnd(t) then
        tok.Kind := tokInvalid
    else
        tok := t.TokenList^;

    Peek := tok;
end;

function Advance(var T: TParserState): PToken;
var
    cur: PToken;
begin
    cur := T.TokenList;
    T.TokenList := T.TokenList^.Next;

    Advance := cur;
end;

function IsKeyword(str: string): Boolean;
var
    i: Integer;
begin
    for i := 0 to Length(KEYWORDS) - 1 do
    begin
        if str = KEYWORDS[i] then
            Exit(true);
    end;

    IsKeyword := false;
end;

function Match(var t: TParserState; kind: TTokenKind): Boolean;
begin
    if t.TokenList = nil then
        Exit(false);

    if (kind = tokIdentifier) and IsKeyword(t.TokenList^.Literal) then
        Exit(false);

    Match := t.TokenList^.Kind = kind;
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

procedure PushError(var t: TParserState; errorMessage: String);
var
    cur: TToken;
    line, col: Integer;
begin
    cur := Peek(t);
    if cur.Kind = tokInvalid then
    begin
        line := 0;
        col := 0;
    end
    else
    begin
        line := cur.Line;
        col := cur.Col;
    end;

    StringArrayPush(
        t.Errors,
            t.UnitName
            + '(' + IntToStr(line)
            + ',' + IntToStr(col) + '): '
            + errorMessage
    );
end;


function Consume(var t: TParserState; kind: TTokenKind; errorMessage: String): PToken;
begin
    Consume := nil;

    if Match(t, kind) then
    begin
        Exit(Advance(t));
    end;

    PushError(t, errorMessage);

    // Synchronize the parser state
    while not IsAtEnd(t) do
    begin
        if Match(t, tokSemi) then
            break;

        if MatchKW(t, KW_VAR)
            or MatchKW(t, KW_CONST)
            or MatchKW(t, KW_TYPE)
            or MatchKW(t, KW_FUNCTION)
            or MatchKW(t, KW_PROCEDURE)
            or MatchKW(t, KW_IF)
            or MatchKW(t, KW_WHILE)
            or MatchKW(t, KW_FOR)
            or MatchKW(t, KW_IMPLEMENTATION)
            or MatchKW(t, KW_INTERFACE) then break;

        Advance(t);
    end;
end;

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

function EatPrimary(var T: TParserState): PAstNode;
var
    expr: PAstNode;
    literal: PToken;
    litCopy: TToken;
begin
    if T.TokenList = nil then
        Exit(nil);

    case T.TokenList^.Kind of
        tokIdentifier, tokString, tokInteger,
        tokFloat, tokCharcode: begin
            literal := Advance(T);
            if literal = nil then
                Exit(nil);

            litCopy := literal^;
            litCopy.Next := nil;

            New(expr);
            expr^.Kind := astExprLiteral;
            expr^.Literal := litCopy;

            EatPrimary := expr;
        end;

        tokLParen: begin
            if Consume(t, tokLParen, '"(" expected') = nil then
                Exit(nil);

            New(expr);
            expr^.Kind := astExprGrouping;
            expr^.Inner := EatExpression(T);

            if Consume(t, tokRParen, '")" expected') = nil then
                Exit(nil);

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
    expr, sub: PAstNode;
    prefixOperator: PToken;
    tokenKindAsStr: string;
begin
    // Is unary with sign prefix
    if Match(T, tokMinus) or Match(T, tokPlus) then
    begin
        prefixOperator := Advance(T);

        sub := EatUnary(T);
        if sub = nil then
        begin
            Str(prefixOperator^.Kind, tokenKindAsStr);
            PushError(t, 'invalid statement - expected an unary expression after "' + tokenKindAsStr + '"');
            Exit(nil);
        end;

        New(expr);
        expr^.Kind := astExprUnary;
        expr^.PrefixOperator := prefixOperator^;
        expr^.Expression := sub;

        EatUnary := expr;
    end
    else
        EatUnary := EatPrimary(T);
end;

function EatFactor(var T: TParserState): PAstNode;
var
    expr, right: PAstNode;
    operator: PToken;
    operatorCopy: TToken;
begin
    expr := EatUnary(T);
    if expr = nil then
        Exit(nil);

    while Match(T, tokStar) or MatchKW(T, 'mod') or MatchKW(T, 'div') do
    begin
        operator := Advance(T);
        if operator = nil then
            Exit(nil);

        operatorCopy := operator^;
        operatorCopy.Next := nil;

        right := EatUnary(T);
        if right = nil then
            Exit(nil);

        expr := NewBinaryExpression(expr, operatorCopy, right);
    end;

    EatFactor := expr;
end;

function EatTerm(var T: TParserState): PAstNode;
var
    expr, right: PAstNode;
    operator: PToken;
    operatorCopy: TToken;
begin
    expr := EatFactor(T);
    if expr = nil then
        Exit(nil);

    while Match(T, tokMinus) or Match(T, tokPlus) do
    begin
        operator := Advance(T);
        if operator = nil then
            Exit(nil);

        operatorCopy := operator^;
        operatorCopy.Next := nil;

        right := EatFactor(T);
        if right = nil then
            Exit(nil);

        expr := NewBinaryExpression(expr, operatorCopy, right);
    end;

    EatTerm := expr;
end;

function EatComparison(var T: TParserState): PAstNode;
var
    expr, right: PAstNode;
    operator: PToken;
    operatorCopy: TToken;
begin
    expr := EatTerm(T);
    if expr = nil then
        Exit(nil);

    while Match(T, tokLt) or Match(T, tokLte) or
        Match(T, tokGt) or Match(T, tokGte) do
    begin
        operator := Advance(T);
        if operator = nil then
            Exit(nil);

        operatorCopy := operator^;
        operatorCopy.Next := nil;

        right := EatTerm(T);
        if right = nil then
            Exit(nil);

        expr := NewBinaryExpression(expr, operatorCopy, right);
    end;

    EatComparison := expr;
end;

function EatEquality(var T: TParserState): PAstNode;
var
    expr, right: PAstNode;
    operator: PToken;
    operatorCopy: TToken;
begin
    expr := EatComparison(T);
    if expr = nil then
        Exit(nil);

    while Match(T, tokEq) or Match(T, tokNeq) do
    begin
        operator := Advance(T);
        if operator = nil then
            Exit(nil);

        operatorCopy := operator^;
        operatorCopy.Next := nil;

        right := EatComparison(T);
        if right = nil then
            Exit(nil);

        expr := NewBinaryExpression(expr, operatorCopy, right);
    end;

    EatEquality := expr;
end;

function EatExpression(var t: TParserState): PAstNode;
var
    expr: PAstNode;
begin
    expr := EatEquality(t);
    if expr = nil then
        Exit(nil);

    EatExpression := expr;
end;

function EatVarDecl(var T: TParserState): PAstNode;
var
    astNode: PAstNode;
    variables: TVarList;
    varName, varType: string;
    tok: PToken;
begin
    Advance(T);

    variables.Count := 0;
    variables.Capacity := 4;
    variables.Variables := GetMem(SizeOf(TVariableBind) * variables.Capacity);

    while Match(t, tokIdentifier) do
    begin
        if variables.Count = variables.Capacity then
        begin
             variables.Capacity := variables.Capacity * 2;
             variables.Variables := ReallocMem(variables.Variables, SizeOf(TVariableBind) * variables.Capacity);
        end;

        tok := Consume(T, tokIdentifier, 'expected identifier as variable name');
        if tok = nil then
            Exit;

        varName := tok^.Literal;

        Consume(t, tokColon, 'expected colon as variable name and type separator');
        tok := Consume(t, tokIdentifier, 'expected type name');
        if tok = nil then
            Exit;

        varType := tok^.Literal;

        variables.Variables^[variables.Count].Name := varName;
        variables.Variables^[variables.Count].TypeName := varType;
        variables.Count := variables.Count + 1;

        Consume(t, tokSemi, 'expected semicolon after statement');
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

function EatStatement(var t: TParserState): PAstNode;
var
    astNode: PAstNode;
begin
    // Empty statement
    if Match(t, tokSemi) then
    begin
        Advance(t);

        New(astNode);
        astNode^.Kind := astStmtEmpty;

        Exit(astNode);
    end;

    if MatchKW(t, KW_VAR) then
        EatStatement := EatVarDecl(t)
    else
    begin
        EatStatement := EatExpression(t);
        Consume(t, tokSemi, 'expected semicolon after statement');
    end;
end;

procedure AstDispose(var ast: PAstNode);
begin
    if ast = nil then
        Exit;

    case ast^.Kind of
        astStmtVarDecl: begin
            Dispose(ast^.VarDecls.Variables);
            ast^.VarDecls.Count := 0;
            ast^.VarDecls.Capacity := 0;
        end;

        astExprUnary: AstDispose(ast^.Expression);
        astExprBinary: begin
            AstDispose(ast^.Left);
            AstDispose(ast^.Right);
        end;
        astExprGrouping: AstDispose(ast^.Inner);
    end;

    if ast^.Next <> nil then
        AstDispose(ast^.Next);

    Dispose(ast);

    ast := nil;
end;

procedure ReverseAstList(var Head: PAstNode);
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

function ParseTokens(unitName: string; tokenList: PToken): PAstNode;
var
    expr: PAstNode;
    t: TParserState;
    i: Integer;
begin
    t := ParserStateCreate(unitName, tokenList);

    if t.TokenList = nil then
    begin
        WriteLn(StdErr, '[Parser] token list was empty while parsing ', UnitName, ', exiting...');

        Exit;
    end;

    repeat
    begin
        expr := EatStatement(t);
        if expr = nil then
            continue;

        expr^.Next := t.Ast;
        t.Ast := expr;
    end
    until t.TokenList = nil;

    if t.Errors.Count > 0 then
    begin
        for i := 0 to t.Errors.Count - 1 do
            WriteLn(StdErr, t.Errors.Items^[i]);

        AstDispose(t.Ast);
        Exit(nil);
    end;

    ReverseAstList(t.Ast);

    ParseTokens := t.Ast;
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

procedure PrintExpressionImpl(Expression: PAstNode; Depth: Integer);
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
    PrintExpressionImpl(Expression, 0);
end;

end.
