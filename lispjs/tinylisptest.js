var TinyLisp = require("../tinylisp")
var AST = TinyLisp.AST;
var assert = require("assert")
describe('Array', function(){
  describe('#indexOf()', function(){
    it('should return -1 when the value is not present', function(){
      assert.equal(-1, [1,2,3].indexOf(5));
      assert.equal(-1, [1,2,3].indexOf(0));
    })
  })
})


describe('TinyLisp', function(){
  describe('AST', function(){
    it('supports something', function(){
      new AST.Symbol();
      new AST.List();
      new AST.Number();
      new AST.Block();
    });
  });

  describe('parsing', function(){
    it('parses a list', function(){
      var ast = TinyLisp.read('()');
      assert(ast.content[0] instanceof AST.List);
    });

    it('parses a number', function(){
      var ast = TinyLisp.read('1');
      var val = ast.content[0]
      assert(val instanceof AST.Number);
      assert(val.value == '1');
    });

    it('parses a symbol', function(){
      var ast = TinyLisp.read('abc');

      var val = ast.content[0]
      assert(val instanceof AST.Symbol);
      assert(val.symbol == 'abc');
    });

    it('parses a list', function(){
      var ast = TinyLisp.read('(add 1 (sub 3 4 ))');

      var val = ast.content[0]
      assert(val instanceof AST.List);
      var items = val.content;

      assert(items[0] instanceof AST.Symbol);
      assert(items[0].symbol == 'add');

      assert(items[1] instanceof AST.Number);
      assert(items[1].value == '1');

      assert(items[2] instanceof AST.List);
      var sub = items[2].content;

      assert(sub[0] instanceof AST.Symbol);
      assert(sub[0].symbol == 'sub');

      assert(sub[1] instanceof AST.Number);
      assert(sub[1].value == '3');

      assert(sub[2] instanceof AST.Number);
      assert(sub[2].value == '4');
    });
  });

  describe('printing', function() {

    var ast = TinyLisp.read('(add 1 (sub 3 4 ))');

    var val = ast.content[0];
    assert(val.content[2].print() == '(sub 3 4)');
  });


  describe('eval', function() {
    it('returns numbers', function(){
      var num = TinyLisp.eval(new AST.Number('1'));
      assert(num.print() == '1');
    });

    it('adds', function(){
      var num = TinyLisp.eval(
        new AST.List([
          new AST.Symbol('add'),
          new AST.Number('1'),
          new AST.Number('2')
        ]));
      assert(num.print() == '3');
    });

    it('lists', function(){
      var num = TinyLisp.eval(
        new AST.List([
          new AST.Symbol('list'),
          new AST.Number('1'),
          new AST.Number('2')
        ]));
      assert(num.print() == '(1 2)');
    });

    it('lambdas', function(){
      var num = TinyLisp.REP('((lambda (x) x) 1)');
      assert(num == '1');

      var num = TinyLisp.REP('((lambda (x) (add x x)) 1)');
      assert(num == '2');
    });

    it('quotes', function(){
      var num = TinyLisp.REP('(quote (lambda (x) x))');
      assert(num == '((lambda (x) x))');
    });


  });

  describe('REP', function() {
    it('works', function(){
      var results = TinyLisp.REP('(add 1 2)');
      assert(results == '3');
    });
  });
});
