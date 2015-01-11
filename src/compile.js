// 15:19

var fs = require('fs'),
    exec = require('child_process').exec;

var abort = function(msg) {
  throw new Error(msg);
};

var ensure = function(msg, cond) {
  if (!cond) abort('expectation failed! (' + msg + ')');
};

var operators = ['+', '-', '*', '/', '<', '>'];
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

var id = (function() {
  var _id = 0;
  return function() {
    return _id++;
  };
})();


var compile = function(prog) {
  var tokens = lex(prog);

  var isFixnum = function(n) {
    var wordSize = 4;
    var nBits = 8 * wordSize - 2;
    var upperLimit = Math.pow(2, nBits - 1) - 1;
    var lowerLimit = -Math.pow(2, nBits - 1);

    var num = Number(n);
    return lowerLimit <= num && num <= upperLimit;
  };

  var immediate = function() {
    var v;
    var t = tokens[0];
    if (t.type == 'NUMBER') {
      if (!isFixnum(t.val)) abort('illegal fixnum: ' + t.val);
      v = Number(tokens[0].val) << 2;
    } else if (t.type == 'BOOL')
      v = t.val ? 0x6F : 0x2F;
    else if (t.type == 'CHAR')
      v = t.val.charCodeAt(0) << 8 | 0x0F;
    else if (t.type == 'LPAREN' && tokens[1].type == 'RPAREN')
      v = 0x3F;
    return v;
  };

  return '	.text\n' +
	  '  .globl	scheme_entry\n' +
    'scheme_entry:\n' +
	  '  movl	$' + immediate(tokens) + ', %eax\n' +
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
runTests();

compileAndRun('1111', function(output) {
  console.log( 'result: ' + output );
});
