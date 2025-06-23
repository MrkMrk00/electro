unit Interpreter;

interface

uses Parser, Tokenizer;

type
    TValueKind = ( kString, kInteger, kFloat, kInvalid );
    TValue = record
        Kind: TValueKind;

        case TValueKind of
            kInteger: (IntVal: Integer);
            kString:  (StrVal: string);
            kFloat:   (FloatVal: Double);
            kInvalid: (ErrorMessage: string);
    end;

function EvaluateExpression(Expression: TExpression): TValue;

implementation

function StrToFloat(Str: string): Double;
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

    StrToFloat := ret;
end;

function EvaluateExpression(Expression: TExpression): TValue;
var
    value, vi1, vi2: TValue;
    t: string;
begin
    case Expression.Kind of
        exprLiteral: begin
            case Expression.Literal.Kind of
                tokString: begin
                    value.Kind   := kString;
                    value.StrVal := Expression.Literal.Literal;
                end;

                tokNumber: begin
                    value.Kind     := kFloat;
                    value.FloatVal := StrToFloat(Expression.Literal.Literal);
                end;
            end;
        end;

        exprUnary: begin
            vi1 := EvaluateExpression(Expression.Expression^);
            if vi1.Kind <> kFloat then
            begin
                Str(vi1.Kind, t);

                value.Kind := kInvalid;
                value.ErrorMessage := 'cannot use operator "' + TokenToString(Expression.PrefixOperator) + '" with type "' + t + '"';

                Exit(value);
            end;

            value := vi1;

            if Expression.PrefixOperator.Kind = tokMinus then
            begin
                value.FloatVal := -value.FloatVal;
            end;
        end;

        exprBinary: begin
            vi1 := EvaluateExpression(Expression.Left^);
            if vi1.Kind = kInvalid then
                Exit(vi1);

            vi2 := EvaluateExpression(Expression.Right^);
            if vi2.Kind = kInvalid then
                Exit(vi1);

            if vi1.Kind <> vi2.Kind then
            begin
                value.Kind := kInvalid;

                Str(vi1.Kind, t);
                value.ErrorMessage := 'operands of a binary expression have to have the same type got "' + t + '"';

                Str(vi2.Kind, t);
                value.ErrorMessage := value.ErrorMessage + ' and "' + t + '"';

                Exit(value);
            end;

            value.Kind := vi1.Kind;

            case Expression.Operator.Kind of
                tokPlus: value.FloatVal := vi1.FloatVal + vi2.FloatVal;
                tokMinus: value.FloatVal := vi1.FloatVal - vi2.FloatVal;
                tokStar: value.FloatVal := vi1.FloatVal * vi2.FloatVal;
                tokIdentifier: begin
                    if Expression.Operator.Literal = 'div' then
                        value.FloatVal := vi1.FloatVal / vi2.FloatVal
                    else if Expression.Operator.Literal = 'div' then
                        value.FloatVal := Trunc(vi1.FloatVal) mod Trunc(vi2.FloatVal)
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
