// 13:36 - 15:10


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

var id = (function() {
  var _id = 0;
  return function() {
    return _id++;
  };
})();


var compile = function(prog) {
  var CHAR_TAG = 0x0F;
  var CHAR_OFFSET = 8;
  var INT_OFFSET = 2;

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
      return t.val ? 0x6F : 0x2F;
    else if (t.type == 'CHAR')
      return t.val.charCodeAt(0) << CHAR_OFFSET | CHAR_TAG;
    else if (t instanceof Array && t.length == 0)
      return 0x3F;

    abort('unkown immediate: ' + JSON.stringify(t, null, 2));
    return null;
  };

  var numToken = function(n) {
    return { type: 'NUMBER', val: String(n) };
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

  var expression = function(ast) {
    if (isAtom(ast)) {
	    return '  movl	$' + immediate(ast) + ', %eax\n';
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
        console.log('failing test, expected "' + t[1] + '" but was "' + output + '"');
        nFailed++;
      } else {
        nPass++;
      }
      pending--;

      if (pending == 0)
        console.log( nPass + ' tests passed, ' + nFailed + ' failed.' );
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
runTests();

var prog = '(fixnum->char 106)';

compileAndRun(prog, function(output) {
  console.log( 'result: ' + output );
});
/*
*/

console.log( compile(prog) );
