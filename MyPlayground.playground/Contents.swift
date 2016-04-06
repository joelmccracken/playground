//: Playground - noun: a place where people can play

import UIKit

var str = "Hello, playground"

enum Expr {
    case Sym(String)
    case Number(integer_t)
    case Str(String)
    case List([Expr])
}

Expr.Sym("foo")


Expr.List([
        Expr.Sym("lambda"),
        Expr.List([Expr.Sym("x")]),
        Expr.Sym("x")
    ])


typealias Env = Dictionary<String, Expr>


func makeEnv() -> Env {
    return Env(dictionaryLiteral: ("ten", Expr.Number(10)))
}

func eval(exp: Expr, env: Env) -> (Expr, Env) {
    return (exp, env)
}


Env(dictionaryLiteral: ("foo", Expr.Sym("foo")))