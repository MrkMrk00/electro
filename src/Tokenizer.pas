unit Tokenizer;

interface

uses BufferUtils;

type
    TTokenKind = (
        tokLParen,      // (
        tokRParen,      // )
        tokLBrace,      // [
        tokRBrace,      // ]
        tokPlus,        // +
        tokMinus,       // -
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
        tokCharcode,    // e.g. #10 -> NEWLINE literal

        tokInvalid
    );

    PToken = ^TToken;

    TToken = record
        Line, Col: Integer;
        Next: PToken;
        case Kind: TTokenKind of
            tokIdentifier, tokString, tokNumber: (Literal: string);
    end;

const
    TAB = #9;
    NEWLINE = #10;
    EOF = #0;

function  TokenizeUnit(UnitName: string; const Buffer: TBuffer): PToken;
procedure TokenListDispose(var TokenList: PToken);

function TokenToString(const Token: TToken): string;

implementation

type
    TTokenizerState = record
        UnitName: string;
        Idx: UInt32;
        Buffer: TBuffer;
        Line, Col: UInt32;
        Tokens: PToken;
        Errors: array [0..15] of string;
        ErrIdx: Integer;
    end;

procedure AppendError(var T: TTokenizerState; ErrorMessage: string);
var
    i: Integer;
begin
    // is full -> short circuit
    if T.ErrIdx >= High(T.Errors) then
    begin
        WriteLn(StdErr, 'too many encountered while parsing ', T.UnitName);
        Exit;
    end;

    T.Errors[T.ErrIdx] := ErrorMessage;
    Inc(T.ErrIdx);
end;

function TokenToString(const Token: TToken): string;
var
    kind, line, col, ret: string;
begin
    Str(Token.Kind, kind);
    Str(Token.Line, line);
    Str(Token.Col, col);

    ret := line + ':' + col + ': ';

    case Token.Kind of
        tokNumber, tokIdentifier, tokString,
        tokCharcode, tokInvalid:
            ret := ret + kind + '(' + Token.Literal + ')';
    else
        ret := ret + kind;
    end;

    TokenToString := ret;
end;

procedure AppendToken(var T: TTokenizerState; Token: PToken);
begin
    Token^.Next := T.Tokens;
    T.Tokens := Token;
end;

procedure AppendSymbol(var T: TTokenizerState; Kind: TTokenKind);
var
    token: PToken;
begin
    New(token);
    token^.Kind := Kind;
    token^.Line := T.Line;
    token^.Col := T.Col;

    AppendToken(T, token);
end;

procedure AppendInvalid(var T: TTokenizerState; Lit: string);
var
    token: PToken;
begin
    New(token);

    token^.Kind := tokInvalid;
    token^.Line := T.Line;
    token^.Col := T.Col;
    token^.Literal := Lit;

    AppendToken(T, token);
end;

function SourceSubstr(const T: TTokenizerState; StartIdx, EndIdx: UInt32): string;
var
    ret: string;
    i: UInt32;
begin
    ret := '';
    for i := StartIdx to EndIdx do
        ret := ret + char(T.Buffer.Data^[i]);

    SourceSubstr := ret;
end;

procedure ReverseTokenList(var Head: PToken);
var
    cur, prev, next: PToken;
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

function Peek(const T: TTokenizerState): char;
begin
    if T.Idx >= T.Buffer.Size then
        Exit(EOF);

    Peek := char(T.Buffer.Data^[T.Idx]);
end;

function PeekN(const T: TTokenizerState; N: UInt32): char;
begin
    // PeekN(T, 1) should be equivalent to Peek(T)
    N := N - 1;

    if (T.Idx + N) >= T.Buffer.Size then
        Exit(EOF);

    PeekN := char(T.Buffer.Data^[T.Idx + N]);
end;

function Advance(var T: TTokenizerState): char;
var
    ch: char;
begin
    ch := Peek(T);
    if ch <> EOF then
    begin
        Inc(T.Idx);

        if ch = NEWLINE then
        begin
            Inc(T.Line);
            T.Col := 0;
        end
        else
            Inc(T.Col);
    end;

    Advance := ch;
end;

function Match(var T: TTokenizerState; Expected: char): Boolean;
var
    ch: char;
begin
    ch := Peek(T);

    if (ch = EOF) or (ch <> Expected) then
        Exit(false);

    Advance(T);

    Match := true;
end;

function IsNumeric(Ch: char): Boolean;
var
    charCode: Integer;
begin
    charCode := Ord(Ch);

    IsNumeric := (charCode >= Ord('0')) and (charCode <= Ord('9'));
end;

procedure EatNumber(var T: TTokenizerState);
var
    startPos, endPos: UInt32;
    lookahead: char;
    literal: string;
    token: ^TToken;
begin
    startPos := T.Idx - 1;
    lookahead := Peek(T);

    while IsNumeric(lookahead) or (lookahead = '.') do
    begin
        Advance(T);
        lookahead := Peek(T);
    end;

    endPos := T.Idx - 1;
    literal := SourceSubstr(T, startPos, endPos);

    New(token);
    token^.Line := T.Line;
    token^.Col := T.Col - Length(literal);
    token^.Kind := tokNumber;
    token^.Literal := literal;

    AppendToken(T, token);
end;

function IsIdentifierStart(Ch: char): Boolean;
var
    charCode: Integer;
begin
    charCode := Ord(Ch);

    IsIdentifierStart := ((charCode >= Ord('a')) and (charCode <= Ord('z')))
        or ((charCode >= Ord('A')) and (charCode <= Ord('Z')))
        or (Ch = '_');
end;

procedure EatIdentifier(var T: TTokenizerState);
var
    startPos, endPos: UInt32;
    lookahead: char;
    literal: string;
    token: ^TToken;
begin
    startPos := T.Idx - 1;
    lookahead := Peek(T);

    while IsIdentifierStart(lookahead) or IsNumeric(lookahead) do
    begin
        Advance(T);
        lookahead := Peek(T);
    end;

    endPos := T.Idx - 1;
    literal := SourceSubstr(T, startPos, endPos);

    New(token);
    token^.Line := T.Line;
    token^.Col := T.Col - Length(literal);
    token^.Kind := tokIdentifier;
    token^.Literal := literal;

    AppendToken(T, token);
end;

function IntToStr(I: Integer): string;
var
    ret: string;
begin
    Str(I, ret);

    IntToStr := ret;
end;

procedure EatString(var T: TTokenizerState);
var
    startPos, endPos: UInt32;
    lookahead: char;
    literal: string;
    token: ^TToken;
begin
    startPos := T.Idx - 1;
    lookahead := Peek(T);

    while (lookahead <> '''') or ((lookahead = '''') and (PeekN(T, 2) = '''')) do
    begin
        // unterminated string literal
        if (lookahead = EOF) or (lookahead = NEWLINE) then
        begin
            AppendInvalid(T, SourceSubstr(T, startPos, T.Idx - 1));
            AppendError(T, 'unterminated string literal at ' + IntToStr(T.Line) + ':' + IntToStr(T.Col) + ':');
            Exit;
        end;

        Advance(T);
        lookahead := Peek(T);
    end;

    // eat the final quote
    Advance(T);

    endPos := T.Idx - 1;
    literal := SourceSubstr(T, startPos, endPos);

    New(token);
    token^.Line := T.Line;
    token^.Col := T.Col - Length(literal);
    token^.Kind := tokString;
    token^.Literal := literal;

    AppendToken(T, token);
end;

procedure EatCharcode(var T: TTokenizerState);
var
    startPos, endPos: UInt32;
    literal: string;
    token: ^TToken;
begin
    startPos := T.Idx - 1;

    while IsNumeric(Peek(T)) do
        Advance(T);

    endPos := T.Idx - 1;
    literal := SourceSubstr(T, startPos, endPos);

    New(token);
    token^.Line := T.Line;
    token^.Col := T.Col - Length(literal);
    token^.Kind := tokCharcode;
    token^.Literal := literal;

    AppendToken(T, token);
end;

function TokenizeUnit(UnitName: string; const Buffer: TBuffer): PToken;
var
    t: TTokenizerState;
    ch: char;
    i: Integer;
begin
    t.Idx := 0;
    t.UnitName := UnitName;
    t.Buffer := Buffer;
    t.Line := 0;
    t.Col := 0;
    t.Tokens := nil;
    t.ErrIdx := 0;

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
            '/': AppendSymbol(t, tokSlash);
            ',': AppendSymbol(t, tokComma);
            '.': begin
                if Match(t, '.') then
                    AppendSymbol(t, tokRangeCtor)
                else
                    AppendSymbol(t, tokDot);
            end;
            '^': AppendSymbol(t, tokCircumflex);
            ':': begin
                if Match(t, '=') then
                    AppendSymbol(t, tokAssign)
                else
                    AppendSymbol(t, tokColon);
            end;

            ';': AppendSymbol(t, tokSemi);

            '>': begin
                if Match(t, '=') then
                    AppendSymbol(t, tokGte)
                else
                    AppendSymbol(t, tokGt);
            end;
            '<': begin
                if Match(t, '=') then
                    AppendSymbol(t, tokLte)
                else if Match(t, '>') then
                    AppendSymbol(t, tokNeq)
                else
                    AppendSymbol(t, tokLt)
            end;
            '=': AppendSymbol(t, tokEq);
            '#': EatCharcode(t);
        else
            if IsNumeric(ch) then
                EatNumber(t)
            else if IsIdentifierStart(ch) then
                EatIdentifier(t)
            else if ch = '''' then
                EatString(t)
            else if (ch = ' ') or (ch = TAB) or (ch = EOF) or (ch = NEWLINE) then
                continue
            else
            begin
                AppendInvalid(t, ch);
                AppendError(t, 'invalid token encountered "' + ch + '"');
            end;
        end;
    until (ch = EOF) or (t.ErrIdx >= High(t.Errors));

    if t.ErrIdx > 0 then
    begin
        TokenListDispose(t.Tokens);

        for i := Low(t.Errors) to t.ErrIdx - 1 do
        begin
            WriteLn(StdErr, T.Errors[i]);
        end;

        Exit(nil);
    end;

    ReverseTokenList(t.Tokens);

    TokenizeUnit := t.Tokens;
end;

procedure TokenListDispose(var TokenList: PToken);
var
    cur, next: PToken;
begin
    cur := TokenList;

    while cur <> nil do
    begin
        next := cur^.Next;
        Dispose(cur);

        cur := next;
    end;

    TokenList := nil;
end;

end.
