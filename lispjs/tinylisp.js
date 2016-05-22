var _ = require('underscore');

var TinyLisp = {};

var AST = {};

var env = function(additions, base) {
  _.extend({}, additions, base);
};

var Env = function(content, parent) {
  this.content = content;
  this.parent = parent;
};

Env.prototype.lookup = function(symbol) {
  if(this.content[symbol.symbol] != undefined) {
    return this.content[symbol.symbol]
  }
  if(this.parent != undefined) {
    return this.parent.lookup(symbol);
  } else {
    throw "Undefined symbol: " + symbol.print()
  }
};

Env.prototype.define = function(symbol, value) {
  this.content[symbol.symbol] = value;
};

Env.prototype.matchParamsToValues = function(lambdaArgs, args) {
  var mapping = {};
  _.each(lambdaArgs.content, function(arg, index){
    mapping[arg.symbol] = args[index];
  });
  return new Env(mapping, this);
};

Env.base = function() {
  return new Env({
    'list': function(args) {
      return new AST.List(args);
    },
    'add': function(args) {
      added = _.chain(args).map(function(num){
        return parseInt(num.value, 10);
      }).reduce(function(prev, next){
        return prev + next;
      }).value();
      return new AST.Number('' + added);
    }
  });
};

var add = function(item) {
  return new this.constructor(this.content.concat([item]));
};

AST.List = function(content) {
  this.content = content || [];
};

AST.List.prototype.add = add;

AST.List.prototype.print = function() {
  var pieces = _.map(this.content, function(item){
    return item.print();
  });

  return '(' + pieces.join(' ') + ')';
};

AST.Symbol = function(symbol) {
  this.symbol = symbol;
};

AST.Symbol.prototype.print = function() {
  return this.symbol;
};

AST.Number = function(value) {
  this.value = value;
};

AST.Number.prototype.print = function() {
  return this.value;
};

AST.Block = function(content) {
  this.content = content || [];
};

AST.Block.prototype.add = add;
AST.Block.prototype.print = function() {
  var pieces = _.map(this.content, function(item){
    return item.print();
  });
  return pieces.join('\n');
};

function first(array) {
  return array[0];
};

function rest(array) {
  return array.slice(1);
}

function read(originalString) {
  var remaining = originalString;

  var nextChar = function()
  {
    remaining = rest(remaining);
  };

  var isWhitespace = function(c) {
    return c == "\n" || c == " "; // NO TABS
  };

  var skipWhitespace = function() {
    while(isWhitespace(first(remaining))) {
      nextChar();
    }
  };

  var parseAtom = function() {
    var numberRegex = /^(\d+)/;
    var symbolRegex = /^(\w+)/;
    var matched;

    if(matched = remaining.match(numberRegex)) {
      remaining = remaining.slice(matched[0].length);
      return new AST.Number(matched[0]);
    } else if (matched = remaining.match(symbolRegex)) {
      remaining = remaining.slice(matched[0].length);
      return new AST.Symbol(matched[0]);
    }
    return false;
  };

  var readList = function() {
    var sexp = new AST.List();
    var tmp = readExpr(sexp);
    return tmp;
  };

  var readExpr = function(ast) {
    var c, atom, subexpr;

    while(true) {
      skipWhitespace();
      c = first(remaining);
      if(c == '(') {
        nextChar();
        subexpr = readList();
        ast = ast.add(subexpr);
      } else if(c == ')'){
        nextChar();
        return ast;
      } else if(atom = parseAtom()) {
        ast = ast.add(atom);
      } else if(c == null) {
        return ast;
      } else {
        throw "Unexpected character '" + c + "' at '" + remaining + '"';
      }
    }
  }

  var ast = new AST.Block;
  return readExpr(ast);
}

function lambda(args, env) {
  var lambdaArgs = first(args);
  var body = new AST.Block(rest(args));

  return function(args) {
    lambdaEnv = env.matchParamsToValues(lambdaArgs, args);
    var tmp = _eval(body, lambdaEnv);
    return tmp;
  }
}

function evalArgs(rawArgs, env) {
  return _(rawArgs).map(function(arg) {
    return _eval(arg, env);
  });
}

function _eval(sexp, env) {
  if(env == undefined) {
    env = Env.base();
  }

  if(sexp instanceof AST.Block) {
    var lastResult = null;
    _.each(sexp.content, function(expr){
      lastResult = _eval(expr, env);
    });
    return lastResult;
  } else if(sexp instanceof AST.List) {
    var fst = first(sexp.content);
    if(fst.symbol == 'lambda') {
      return lambda(rest(sexp.content), env);
    } else if(fst.symbol == 'quote') {
      return new AST.List(rest(sexp.content));
    } else {
      var args = evalArgs(rest(sexp.content), env)
      var applicative = _eval(fst, env);

      if(applicative instanceof Function) {
        return applicative(args);
      } else {
        throw "" + first + " isn't a function."
      }
    }
    return sexp;
  } else if(sexp instanceof AST.Number) {
    return sexp;
  } else if(sexp instanceof AST.Symbol) {
    return env.lookup(sexp);
  } else {
    throw "Unknown error.";
  }
}

function REP(string){
  return _eval(read(string)).print();
}


TinyLisp.read = read;

TinyLisp.AST = AST;

TinyLisp.eval = _eval;

TinyLisp.REP = REP;

module.exports = TinyLisp;
