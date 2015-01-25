// 21:11-

/*
 (defun asm () (interactive)
 (save-buffer)
 (shell-command "gcc -Wall mine0.s runtime.c -o test0"))

 (global-set-key (kbd "<f12>") 'asm)

 (defun run-comp () (interactive) (shell-command "nodejs compile.js"))

 (global-set-key (kbd "<f11>") 'run-comp)
 */

var fs = require('fs'),
    exec = require('child_process').exec;

var abort = function(msg) {
  throw new Error(msg);
};

var ensure = function(msg, cond) {
  if (!cond) abort('expectation failed! (' + msg + ')');
};

var operators = ['+', '-', '*', '/', '<', '>', '?'];
var bools = ['#f', '#t'];
var nums = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
var alpha = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'];
var ws = [' ', '\n', '\r', '\t'];

var lex = function(str) {
  var tokens = [];
  var i = 0;
  var state = 'DEFAULT';
  var s = -1, e = -1;

  while (true) {
    var c = str.charAt(i);

    if (state == 'DEFAULT') {
      if (c == '(') {
        tokens.push({ type: 'LPAREN', val: c });
      } else if (c == ')') {
        tokens.push({ type: 'RPAREN', val: c });
      } else if (c == '"') {
        s = i;
        state = 'STRING';
      } else if (nums.indexOf(c) != -1 || c == '-') {
        s = i;
        state = 'NUMBER';
      } else if (alpha.concat(operators).indexOf(c) != -1) {
        s = i;
        state = 'NAME';
      } else if (c == '#') {
        state = 'READER';
      } else if (ws.indexOf(c) != -1) {
        // skip
      }

    } else if (state == 'STRING') {
      if (c == '"') {
        tokens.push({ type: 'STRING', val: str.slice(s, i + 1) });
        state = 'DEFAULT';
      } else {
        // eat
      }
    } else if (state == 'NUMBER') {
      if (nums.indexOf(c) == -1) {
        tokens.push({ type: 'NUMBER', val: str.slice(s, i) });
        state = 'DEFAULT';
        continue;
      } else {
        // eat
      }
    } else if (state == 'NAME') {
      if (alpha.concat(operators).concat(nums).indexOf(c) == -1) {
        tokens.push({ type: 'NAME', val: str.slice(s, i) });
        state = 'DEFAULT';
        continue;
      } else {
        // eat
      }
    } else if (state == 'READER') {
      if (c == 't') {
        tokens.push({ type: 'BOOL', val: true });
        state = 'DEFAULT';
      } else if (c == 'f') {
        tokens.push({ type: 'BOOL', val: false });
        state = 'DEFAULT';
      } else if (c == '\\') {
        state = 'CHAR';
      } else {
        abort('unkown reader macro: ' + c);
      }
    } else if (state == 'CHAR') {
      tokens.push({ type: 'CHAR', val: c});
      state = 'DEFAULT';
    }
    i++;

    if (i >= str.length) {
      if (state != 'DEFAULT' && state != 'READER' && state != 'CHAR') {
        tokens.push({ type: state, val: str.slice(s, i) });
      }
      break;
    }
  }

  return tokens;
};

var parse = function(tokens) {
  if (tokens[0].type != 'LPAREN') return tokens[0];

  var stack = [[]];

  var t;
  while ((t = tokens.shift()) !== undefined) {
    if (t.type == 'LPAREN') {
      var newStack = [];
      stack[stack.length - 1].push(newStack);
      stack.push(newStack);
    } else if (t.type == 'RPAREN')
      stack.pop();
    else
      stack[stack.length - 1].push(t);
  }

  if (stack.length > 1) abort('non empty expr stack: ' + stack.length);

  return stack[0][0];
};

var idGen = function() {
  return (function() {
    var _id = 0;
    return function() {
      return _id++;
    };
  })();
};

var id = idGen();

var uniq_label = (function() {
  var lables = idGen();
  return function() {
    return "L_" + lables();
  };
})();

var compile = function(prog) {
  var CHAR_TAG = 0x0F;
  var CHAR_OFFSET = 8;
  var CHAR_MASK = 0xFF;
  var INT_OFFSET = 2;
  var FX_MASK = 0x3;
  var FX_TAG = 0x0;
  var BOOL_TRUE = 0x6F;
  var BOOL_FALSE = 0x2F;
  var BOOL_BIT = 6;
  var NULL_TAG = 0x3F;

  var ast = parse(lex(prog));

  var isAtom = function(e) {
    return !(e instanceof Array) || e.length == 0;
  };

  var isFixnum = function(n) {
    var wordSize = 4;
    var nBits = 8 * wordSize - 2;
    var upperLimit = Math.pow(2, nBits - 1) - 1;
    var lowerLimit = -Math.pow(2, nBits - 1);

    var num = Number(n);
    return lowerLimit <= num && num <= upperLimit;
  };

  var immediate = function(t) {
    if (t.type == 'NUMBER') {
      if (!isFixnum(t.val)) abort('illegal fixnum: ' + t.val);
      return Number(t.val) << INT_OFFSET;
    } else if (t.type == 'BOOL')
      return t.val ? BOOL_TRUE : BOOL_FALSE;
    else if (t.type == 'CHAR')
      return t.val.charCodeAt(0) << CHAR_OFFSET | CHAR_TAG;
    else if (t instanceof Array && t.length == 0)
      return NULL_TAG;

    abort('unkown immediate: ' + JSON.stringify(t, null, 2));
    return null;
  };

  var numToken = function(n) {
    return { type: 'NUMBER', val: String(n) };
  };

  var boolToken = function(v) {
    return { type: 'BOOL', val: v };
  };

  var nameToken = function(v) {
    return { type: 'NAME', val: v };
  };

  var zfToBool = function() {
    return '  sete %al\n' +
      '  movzbl %al, %eax\n' +
      '  shll   $' + BOOL_BIT + ', %eax\n' +
      '  orl    $' + BOOL_FALSE + ', %eax\n';
  };

  var primitives = {
    fxadd1: {
      argCount: 1,
      gen: function() {
        return '  addl $' + immediate(numToken(1)) + ', %eax\n';
      }
    },
    fxsub1: {
      argCount: 1,
      gen: function() {
        return '  subl $' + immediate(numToken(1)) + ', %eax\n';
      }
    },
    'fixnum->char': {
      argCount: 1,
      gen: function() {
        return '  shll $' + (CHAR_OFFSET - INT_OFFSET) + ', %eax\n' +
          '  orl $' + CHAR_TAG + ', %eax\n';
      }
    },
    'char->fixnum': {
      argCount: 1,
      gen: function() {
        return '  shrl $' + (CHAR_OFFSET - INT_OFFSET) + ', %eax\n';
      }
    },
    'fxzero?': {
      argCount: 1,
      gen: function() {
        return '  cmpl $' + FX_TAG + ', %eax\n' +
          zfToBool();
      }
    },
    'fixnum?': {
      argCount: 1,
      gen: function() {
        return '  andl  $' + FX_MASK + ', %eax\n' +
          '  cmpl  $' + FX_TAG + ', %eax\n' +
          '  sete  %al\n' +
          '  shl   $' + BOOL_BIT + ', %eax\n' +
          '  or    $' + BOOL_FALSE + ', %eax\n';
      }
    },
    'null?': {
      argCount: 1,
      gen: function() {
        return '  cmpl  $' + NULL_TAG + ', %eax\n' +
          zfToBool();
      }
    },
    'not': {
      argCount: 1,
      gen: function() {
        return '  cmpl  $' + BOOL_FALSE + ', %eax\n' +
          zfToBool();
      }
    },
    'boolean?': {
      argCount: 1,
      gen: function() {
        return '  orl $' + (1 << BOOL_BIT) + ', %eax\n' +
        '  cmpl  $' + BOOL_TRUE + ', %eax\n' +
          zfToBool();
      }
    },
    'char?': {
      argCount: 1,
      gen: function() {
        return '  andl $' + CHAR_MASK + ', %eax\n' +
        '  cmpl  $' + CHAR_TAG + ', %eax\n' +
          zfToBool();
      }
    },
    'fxlognot': {
      argCount: 1,
      gen: function() {
        return '  xorl $0xFFFFFFFF, %eax\n' +
        '  xorl  $' + FX_MASK + ', %eax\n';
      }
    }
  };

  var primitive = function(name, args) {
    var config = primitives[name.val];
    if (!config) {
      abort('unkown primitive: ' + name.val);
    }

    if (args.length != config.argCount) {
      abort('invalid arg count, expected ' + config.argCount +
            ' but was: ' + args.length);
    }

    var asm = expression(args[0]);
    asm += config.gen();

    return asm;
  };

  var ifConditional = function(ast) {
    if (ast.length != 3) {
      abort("conditional with wrong number of arguments (expected 3), was " + ast.length);
    }

    var asm = '';

    var falseLabel = uniq_label(),
        endLabel = uniq_label();

    // eval condition
    asm += expression(ast[0]);

    // jmp if false
    asm += '  cmp $' + BOOL_FALSE + ', %al\n' +
      '  je ' + falseLabel + '\n';

    // true branch
    asm += expression(ast[1]);

    // jmp to end
    asm += '  jmp ' + endLabel + '\n';

    // false branch
    asm += falseLabel + ':\n';
    asm += expression(ast[2]);

    // end
    asm += endLabel + ':\n';

    return asm;
  };

  var andConditional = function(nodes) {
    if (nodes.length == 0) {
      return expression(boolToken(true));
    } else if (nodes.length == 1) {
      return expression(nodes[0]);
    } else {
      var subExpr = [nameToken('and')];
      subExpr = subExpr.concat(nodes.slice(1));
      return expression(
        [nameToken('if'), nodes[0], subExpr, boolToken(false)]);
    }
  };

  var orConditional = function(nodes) {
    if (nodes.length == 0) {
      return expression(boolToken(true));
    } else if (nodes.length == 1) {
      return expression(nodes[0]);
    } else {
      var subExpr = [nameToken('or')];
      subExpr = subExpr.concat(nodes.slice(1));
      // the result is computed twice!!
      return expression(
        [nameToken('if'), nodes[0], nodes[0], subExpr]);
    }
  };

  var isLiteral = function(node, str) {
    return node.type === 'NAME' && node.val === str;
  };

  var expression = function(ast) {
    if (isAtom(ast)) {
	    return '  movl	$' + immediate(ast) + ', %eax\n';
    } else if (isLiteral(ast[0], 'if')) {
      return ifConditional(ast.slice(1));
    } else if (isLiteral(ast[0], 'and')) {
      return andConditional(ast.slice(1));
    } else if (isLiteral(ast[0], 'or')) {
      return orConditional(ast.slice(1));
    } else {
      return primitive(ast[0], ast.slice(1));
    }
  };

  return '	.text\n' +
	  '  .globl	scheme_entry\n' +
    'scheme_entry:\n' +
    expression(ast) +
	  '  ret\n';
};

var build = function(asm, cb) {
  var curId = id();
  var asmFile = 'mine' + curId + '.s';
  var binFile = 'test' + curId;
  fs.writeFileSync(asmFile, asm);
  exec('gcc -Wall ' + asmFile + ' runtime.c -o ' + binFile, function(err) {
    if (err) throw err;
    cb(binFile);
  });
};
var run = function(file, cb) {
  exec('./' + file, function(err, stdout) {
    if (err) throw err;

    cb(stdout);
  });
};

var compileAndRun = function(prog, cb) {
  build(compile(prog), function(file) {
    run(file, function(res) {
      cb(res);
    });
  });
};

var tests = [];
var test = function(prog, exp) {
  tests.push([prog, exp]);
};

var runTests = function() {
  var pending = 0, nPass = 0, nFailed = 0;
  tests.forEach(function(t) {
    pending++;
    compileAndRun(t[0], function(output) {
      if (output.trim() != t[1]) {
        console.log('failing test, expected "' + t[1] + '" but was "' + output.trim() +
                    '", prog:\n' + t[0] + "\n\n");
        nFailed++;
      } else {
        nPass++;
      }
      pending--;

      if (pending == 0)
        console.log(new Date() + ':\n' + nPass + ' tests passed, ' + nFailed + ' failed.' );
    });
  });

};

test('101', '101');
test('0', '0');
test('42', '42');
test('-1', '-1');
test('100000', '100000');
test('#f', '#f');
test('#t', '#t');
test('#\\a', '#\\a');
test('#\\Z', '#\\Z');
test('#\\Z', '#\\Z');
test('#\\(', '#\\(');
test('#\\*', '#\\*');
test('()', '()');
test('(fxadd1 1)', '2');
test('(fxadd1 (fxsub1 1))', '1');
test('(fxadd1 (fxadd1 (fxadd1 1)))', '4');
test('(fxsub1 0)', '-1');
test('(fixnum->char 106)', '#\\j');
test('(fixnum->char 65)', '#\\A');
test('(fixnum->char 122)', '#\\z');
test('(fixnum? 122)', '#t');
test('(fixnum? #\\a)', '#f');
test('(fixnum? 0)', '#t');
test('(fixnum? -1)', '#t');
test('(fixnum? -1000)', '#t');
test('(fixnum? 1000)', '#t');
test('(fixnum? #t)', '#f');
test('(fixnum? #f)', '#f');
test('(fixnum? -536870912)', '#t');
test('(fixnum? 536870911)', '#t');
test('(char->fixnum #\\j)', '106');
test('(char->fixnum #\\A)', '65');
test('(char->fixnum #\\z)', '122');
test('(fxzero? 536870911)', '#f');
test('(fxzero? -536870912)', '#f');
test('(fxzero? 1)', '#f');
test('(fxzero? -1)', '#f');
test('(fxzero? 42)', '#f');
test('(fxzero? 0)', '#t');
test('(null? ())', '#t');
test('(null? 0)', '#f');
test('(null? 1)', '#f');
test('(null? -1)', '#f');
test('(null? #f)', '#f');
test('(null? #t)', '#f');
test('(null? #\\a)', '#f');
test('(not #t)', '#f');
test('(not #f)', '#t');
test('(not (not #f))', '#f');
test('(not (not #t))', '#t');
test('(not (not (not #t)))', '#f');
test('(not (fxzero? 42))', '#t');
test('(not 42)', '#f');
test('(not ())', '#f');
test('(not #\\a)', '#f');
test('(boolean? #t)', '#t');
test('(boolean? #f)', '#t');
test('(boolean? 42)', '#f');
test('(boolean? ())', '#f');
test('(boolean? #\\a)', '#f');
test('(char? #t)', '#f');
test('(char? #f)', '#f');
test('(char? 42)', '#f');
test('(char? ())', '#f');
test('(char? #\\a)', '#t');
test('(char? #\\Z)', '#t');
test('(char? #\\~)', '#t');
test('(fxlognot 0)', '-1');
test('(fxlognot -1)', '0');
test('(fxlognot 1)', '-2');
test('(fxlognot 42)', '-43');
test('(if #t 2 4)', '2');
test('(if #f 2 4)', '4');
test('(if (not #f) 2 4)', '2');
test('(if (not #f) (fxadd1 2) 4)', '3');
test('(if 42 2 4)', '2');
test('(and)', '#t');
test('(and 42)', '42');
test('(and #t)', '#t');
test('(and #f 42)', '#f');
test('(and #t 42)', '42');
test('(and #t #\\a -1)', '-1');
test('(or)', '#t');
test('(or 42)', '42');
test('(or #t)', '#t');
test('(or #f 42)', '42');
test('(or 42 #f)', '42');
test('(or #f #f)', '#f');
test('(or #t 42)', '#t');
test('(or #t #\\a -1)', '#t');
test('(or 42 #\\a -1)', '42');

runTests();


var prog = '(if #f 2 4)';

/*
compileAndRun(prog, function(output) {
  console.log( 'result: ' + output );
});
*/

console.log( compile(prog) );
