'use strict';

let vau = require('./lib/vau');
let assert = require('assert');
let _ = require('underscore');


var AST = {
    Symbol: class {
        constructor(name) {
            this.sym_name = name;
        }
    },
    sym: (name) => new AST.Symbol(name)
};

var equal = function(a,b) {
    if(a instanceof AST.Symbol &&
       b instanceof AST.Symbol)
        return a.sym_name == b.sym_name;
}



let test = function(){
    var evalAST = function(ast){
        var fst = _.first(ast);

        if(equal(fst, s('begin'))){
            return evalAST(_.rest(ast))
        } else if(equal(fst, s('end'))) {
        }
    }
    var s = AST.sym;

  evalAST(
    [s('begin'),
     [[s('lambda'), [s('x')], s('x')],
      100
     ]
    ])
}


function testLibraryFunctions()
{
    assert(
        equal(AST.sym('foo'), AST.sym('foo'))
    )
}


// only test the very basics of the data structures
function testASTBits()
{

}

var Tokens = {
    lParen: Symbol('('),
    rParen: Symbol(')'),
    whitespace: Symbol('ws'),
    word: Symbol('word')
}

function read(str) {
    function tokenBuilder(type, value, line, col) {
        return {
            type: type,
            type: value,
            line: line,
            col:  col
        };
    }

    function readStart(str) {
    }


    return readStart();
}


function testReaderBits()
{
    var out = read("()")

}




testLibraryFunctions()
testASTBits()
testReaderBits()
