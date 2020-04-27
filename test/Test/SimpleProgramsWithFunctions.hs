{-# LANGUAGE QuasiQuotes #-}

module Test.SimpleProgramsWithFunctions where

import AST
import Combinators
import Test.Helper
import LLang
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import Text.RawString.QQ


fromResult pr =
    let ~(Success _ _ result) = runParser parseProg pr in result

psome = tail [r|
func some(n) {
    return n;
}

x = some(42);
write x;
|]

unit_some :: Assertion
unit_some = do
    fromResult psome @?= Program {
        functions = [
            Function "some" ["n"] (Seq [
                Return (Ident "n")
            ])
        ],
        main = Seq [
            Assign "x" (FunctionCall "some" [Num 42]),
            Write (Ident "x")
        ]
    }


pfac = tail [r|
func fac(n) {
    if (n <= 1) {
        return 1;
    }
    else {
        return fac(n - 1) * fac(n - 2);
    }
}

read n;
write fac(n);
|]

unit_fac :: Assertion
unit_fac = do
    fromResult pfac @?= Program {
        functions = [
            Function "fac" ["n"] (Seq [
                If (BinOp Le (Ident "n") (Num 1))
                    (Seq [Return (Num 1)])
                    (Seq [Return
                        (BinOp Mult
                            (FunctionCall "fac" [BinOp Minus (Ident "n") (Num 1)])
                            (FunctionCall "fac" [BinOp Minus (Ident "n") (Num 2)]))])
            ])
        ],
        main = Seq [
            Read "n",
            Write (FunctionCall "fac" [Ident "n"])
        ]
    }

psum = tail [r|
func sum(left, right) {
    return left+right;
}

read n;
acc = 0;
while (n >= 0) {
    read value;
    acc = sum(acc, value);
    n = n - 1;
}
write acc;
|]

unit_sum :: Assertion
unit_sum = do
    fromResult psum @?= Program {
        functions = [
            Function "sum" ["left", "right"] (Seq [
                Return (BinOp Plus (Ident "left") (Ident "right"))
            ])
        ],
        main = Seq [
            Read "n",
            Assign "acc" (Num 0),
            While (BinOp Ge (Ident "n") (Num 0)) (Seq [
                Read "value",
                Assign "acc" (FunctionCall "sum" [Ident "acc", Ident "value"]),
                Assign "n" (BinOp Minus (Ident "n") (Num 1))
            ]),
            Write (Ident "acc")
        ]
    }
