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
};



let test = function(){
    var evalAST = function(ast){
        var fst = _.first(ast);

        if(equal(fst, s('begin'))){
            return evalAST(_.rest(ast));
        } else if(equal(fst, s('end'))) {
        }
        return true;
    };
    var s = AST.sym;

  evalAST(
    [s('begin'),
     [[s('lambda'), [s('x')], s('x')],
      100
     ]
    ]);
}


var TokenTypes = {
    lParen: '(',
    rParen: ')',
    whitespace: 'space',
    newline: 'newline',
    symbol: 'symbol',
    number: 'number'
};

function tryMatchRe(str, re) {
    var match = str.match(re);
    if(match) {
        var matched = match[0];
        var remaining = str.substring(matched.length);

        return {remaining: remaining, value: matched};
    } else {
        return false;
    }
}

function token(type, value, line, col) {
    return {
        type: type,
        value: value,
        line: line,
        col:  col
    };
}

function read(str) {
    var remainingToRead = str;
    var SPACE_RE = /^[ \t]+/;
    var SYMBOL_RE = /^[\w-]+/;
    var NUMBER_RE = /^[\d]+/;
    var line = 0;
    var col = 0;
    var tokens = [];
    var currentToken = null;

    while(remainingToRead.length > 0) {
        var matched;
        if((matched = tryMatchRe(remainingToRead, SPACE_RE))) {
            tokens.push(token(TokenTypes.whitespace,
                              matched.value,
                              line,
                              col
                             ));
            remainingToRead = matched.remaining;
            col += matched.value.length;
        } else if((matched = tryMatchRe(remainingToRead, SYMBOL_RE))) {
            tokens.push(token(TokenTypes.symbol,
                              matched.value,
                              line,
                              col
                             ));
            remainingToRead = matched.remaining;
            col += matched.value.length;
        } else if((matched = tryMatchRe(remainingToRead, NUMBER_RE))) {
            tokens.push(token(TokenTypes.number,
                              matched.value,
                              line,
                              col
                             ));
            remainingToRead = matched.remaining;
            col += matched.value.length;
        } else if(remainingToRead[0] == "\n") {
            tokens.push(token(TokenTypes.newline,
                              null,
                              line,
                              col
                             ));

            remainingToRead = _.rest(remainingToRead).join('');
            line += 1;
            col  =  0;
        } else if(remainingToRead[0] == "(") {
            tokens.push(token(TokenTypes.lParen,
                              null,
                              line,
                              col
                             ));

            remainingToRead = _.rest(remainingToRead).join('');
            col += 1;
        } else if(remainingToRead[0] == ")") {
            tokens.push(token(TokenTypes.rParen,
                              null,
                              line,
                              col
                             ));

            remainingToRead = _.rest(remainingToRead).join('');
            col += 1;
        } else {
            throw `Parser error, unable to parse starting from: ${remainingToRead}`;
        }
    }
    return tokens;
}

function testLibraryFunctions()
{
    assert(
        equal(AST.sym('foo'), AST.sym('foo'))
    );
}

function testASTBits()
{

}

function testReaderBits() {
    var out = read("()");

    console.log(out);
}

testLibraryFunctions();
testASTBits();
testReaderBits();
