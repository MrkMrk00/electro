unit Tokenizer;

interface

uses BufferUtils;

type
    TTokenKind = (
        tokLParen,
        tokRParen,
        tokLBrace,
        tokRBrace,
        tokPlus,
        tokMinus,
        tokStar,        // *
        tokSlash,       // /
        tokComma,       // ,
        tokDot,         // .
        tokCircumflex,  // ^
        tokColon,       // :
        tokSemi,        // ;

        tokLt,          // <
        tokGt,          // >
        tokEq,          // =

        tokRangeCtor,   // .. (as in 0..127)
        tokAssign,      // :=

        tokNeq,         // <>
        tokLte,         // <=
        tokGte,         // >=

        tokIdentifier,
        tokString,
        tokNumber,
        tokCharcode,

        tokNewline
    );

    PTokenList = ^TToken;

    TToken = record
        Line, Col: Integer;
        Next: PTokenList;
        case Kind: TTokenKind of
            tokIdentifier, tokString, tokNumber: (
                Literal: string;
                Lexeme: string;
            );
    end;

const
    NEWLINE = #10;
    EOF = #0;

function TokenizeUnit(const Buffer: TBuffer): PTokenList;

implementation

type
    TTokenizerState = record
        Idx: UInt32;
        Buffer: TBuffer;
        Line, Col: UInt32;
        Tokens: PTokenList;
    end;

procedure AppendSymbol(var T: TTokenizerState; Kind: TTokenKind);
var
    newToken: PTokenList;
begin
    New(newToken);
    newToken^.Kind := Kind;
    newToken^.Line := T.Line;
    newToken^.Col := T.Col;
    newToken^.Next := T.Tokens;

    T.Tokens := newToken;
end;

procedure ReverseTokenList(var Head: PTokenList);
var
    cur, prev, next: PTokenList;
begin
    if Head^.Next = nil then
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

function Peek(const T: TTokenizerState): char;
begin
    if T.Idx >= T.Buffer.Size then
        Exit(EOF);

    Peek := char(T.Buffer.Data^[T.Idx]);
end;

function Advance(var T: TTokenizerState): char;
var
    ch: char;
begin
    ch := Peek(T);
    if ch <> EOF then
        Inc(T.Idx);

    Advance := ch;
end;

function Match(var T: TTokenizerState; Expected: char): Boolean;
var
    ch: char;
begin
    ch := Peek(T);

    if (ch = EOF) or (ch <> Expected) then
        Exit(false);

    Inc(T.Idx);

    Match := true;
end;

function TokenizeUnit(const Buffer: TBuffer): PTokenList;
var
    t: TTokenizerState;
    ch: char;
begin
    t.Buffer := Buffer;
    t.Line := 0;
    t.Col := 0;
    t.Tokens := nil;

    repeat
        ch := Advance(t);

        case ch of
            '(': AppendSymbol(t, tokLParen);
            ')': AppendSymbol(t, tokRParen);
            '[': AppendSymbol(t, tokLBrace);
            ']': AppendSymbol(t, tokRBrace);
            '+': AppendSymbol(t, tokPlus);
            '-': AppendSymbol(t, tokMinus);
            '*': AppendSymbol(t, tokStar);
            ',': AppendSymbol(t, tokComma);
            '^': AppendSymbol(t, tokCircumflex);
            ';': AppendSymbol(t, tokSemi);

            '.': begin
                if Match(t, '.') then
                    AppendSymbol(t, tokRangeCtor)
                else
                    AppendSymbol(t, tokDot);
            end;

            ':': begin
                if Match(t, '=') then
                    AppendSymbol(t, tokAssign)
                else
                    AppendSymbol(t, tokColon);
            end;

            NEWLINE: begin
                AppendSymbol(t, tokNewline);
                Inc(t.Line);
                t.Col := 0;
            end;
        else
            WriteLn(StdErr, 'unmatched "', ch, '"');
        end;

        Inc(t.Col);
    until ch = EOF;

    if t.Tokens <> nil then
    begin
        // Shift the EOF
        t.Tokens := t.Tokens^.Next;

        ReverseTokenList(t.Tokens);
    end;


    TokenizeUnit := t.Tokens;
end;

end.
