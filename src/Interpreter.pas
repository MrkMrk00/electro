unit Interpreter;

{
    Extremely sub-optimal interpreter. ESI? :)

    1) Everything is copied in the eval functions (not very fast, from what I know).
    2) Direct eval from AST -> interpreter (no optimizations either).
    3) Dynamic types.

    This may be solved:
        - the dream solution would be to integrate with LLVM backend?
    1) Have language
    2) Emit LLVM IR
    3) ...
    4) profit
}

interface

uses Parser, Tokenizer;

type
    TValueKind = ( tyString, tyNumber, tyInvalid );
    TValue = record
        Kind: TValueKind;

        case TValueKind of
            tyString:  (StrVal: string);
            tyNumber:  (NumVal: Double);
            tyInvalid: (ErrorMessage: string);
    end;

function EvaluateExpression(Expression: TExpression): TValue;

implementation

function ParseFloat(Str: string): Double;
var
    ret: Double;
    inv: Integer;
begin
    Val(Str, ret, inv);
    if inv <> 0 then
    begin
        WriteLn(StdErr, 'failed to convert string to float');
        Halt(1);
    end;

    ParseFloat := ret;
end;

function EvalLiteral(E: TExpression): TValue;
var
    ret: TValue;
begin
    Assert(E.Kind = exprLiteral);

    case E.Literal.Kind of
        tokString: begin
            ret.Kind   := tyString;
            ret.StrVal := E.Literal.Literal;
        end;

        tokNumber: begin
            ret.Kind   := tyNumber;
            ret.NumVal := ParseFloat(E.Literal.Literal);
        end;

        tokCharcode: begin
            ret.Kind   := tyString;
            ret.StrVal := E.Literal.Literal;
        end;

        tokIdentifier: begin
            WriteLn(StdErr, 'indetifiers not yet implemented');
            Halt(1);
        end;
    end;

    EvalLiteral := ret;
end;

function EvalUnary(E: TExpression): TValue;
var
    right: TValue;
    t: string;
begin
    Assert(E.Kind = exprUnary);

    right := EvaluateExpression(E.Expression^);
    
    if right.Kind <> tyNumber then
    begin
        Str(E.Kind, t);
        WriteLn(StdErr, 'unexpected type "', t, '", expected "tyNumber"');
        Halt(1);
    end;

    if E.PrefixOperator.Kind = tokMinus then
        right.NumVal := -right.NumVal;

    EvalUnary := right;
end;

function EvaluateExpression(Expression: TExpression): TValue;
var
    value, vi1, vi2: TValue;
    t: string;
begin
    case Expression.Kind of
        exprLiteral: value := EvalLiteral(Expression);
        exprUnary: value := EvalUnary(Expression);

        exprBinary: begin
            vi1 := EvaluateExpression(Expression.Left^);
            if vi1.Kind = tyInvalid then
                Exit(vi1);

            vi2 := EvaluateExpression(Expression.Right^);
            if vi2.Kind = tyInvalid then
                Exit(vi1);

            if vi1.Kind <> vi2.Kind then
            begin
                value.Kind := tyInvalid;

                Str(vi1.Kind, t);
                value.ErrorMessage := 'operands of a binary expression have to have the same type got "' + t + '"';

                Str(vi2.Kind, t);
                value.ErrorMessage := value.ErrorMessage + ' and "' + t + '"';

                Exit(value);
            end;

            value.Kind := vi1.Kind;

            case Expression.Operator.Kind of
                tokPlus: value.NumVal := vi1.NumVal + vi2.NumVal;
                tokMinus: value.NumVal := vi1.NumVal - vi2.NumVal;
                tokStar: value.NumVal := vi1.NumVal * vi2.NumVal;
                tokIdentifier: begin
                    if Expression.Operator.Literal = 'div' then
                        value.NumVal := vi1.NumVal / vi2.NumVal
                    else if Expression.Operator.Literal = 'div' then
                        value.NumVal := Trunc(vi1.NumVal) mod Trunc(vi2.NumVal)
                    else
                    begin
                        WriteLn(StdErr, 'invalid operator ', Expression.Operator.Literal);
                        Halt(1);
                    end;
                end;
            end;
        end;
    end;

    EvaluateExpression := value;
end;

end.
