

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

var operators = ['+', '-', '*', '/', '<', '>', '?', '!', '='];
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
  return function(name) {
    return "L_" + lables() + (name ? '_' + name.replace(/-/g, '_') : '');
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
  var TYPE_OFFSET = 3;
  var PAIR_TAG = 1;
  var PAIR_MASK = 0x7;
  var STRING_MASK = 0x7;
  var SIZE_PAIR = 8;
  var CAR_OFFSET = 0;
  var CDR_OFFSET = 4;
  var VECTOR_TAG = 5;
  var STRING_TAG = 6;


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

  var fxCmp = function(si, op) {
    var trueLbl = uniq_label(),
        endLbl = uniq_label();

    return '  cmp ' + si + '(%esp), %eax\n' +
      '  ' + op + ' ' + trueLbl + '\n' +
      '  movl $' + BOOL_FALSE + ', %eax\n' +
      '  jmp ' + endLbl + '\n' +
      trueLbl + ':\n' +
      '  movl $' + BOOL_TRUE + ', %eax\n' +
      endLbl + ':\n';
  };
  var fxToInt = function(reg) {
    reg = reg || '%eax';
    return '  sar $' + INT_OFFSET + ', ' + reg + '\n';
  };

  var eightByteAlign = function(reg) {
    reg = reg || '%eax';
    return '  addl $7, ' + reg + '  # align\n' +
      '  andl $' + ~7 + ', ' + reg + '\n';
  };

  var firstArg = function(si) {
    return si + '(%esp)';
  };

  var secondArg = function(si) {
    return (si - 4) + '(%esp)';
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
    },
    'fx+': {
      argCount: 2,
      gen: function(si) {
        return '  addl ' + firstArg(si) + ', %eax\n';
      }
    },
    'fx-': {
      argCount: 2,
      gen: function(si) {
        return '  subl %eax, ' + firstArg(si) + '\n' +
          '  movl ' + si + '(%esp), %eax\n';
      }
    },
    'fx=': {
      argCount: 2,
      gen: function(si) {
        return '  cmp ' + firstArg(si) + ', %eax\n' +
          zfToBool();
      }
    },
    'fx<': {
      argCount: 2,
      gen: function(si) {
       return fxCmp(si, 'jg');
      }
    },
    'fx<=': {
      argCount: 2,
      gen: function(si) {
       return fxCmp(si, 'jge');
      }
    },
    'fx>': {
      argCount: 2,
      gen: function(si) {
       return fxCmp(si, 'jl');
      }
    },
    'fx>=': {
      argCount: 2,
      gen: function(si) {
       return fxCmp(si, 'jle');
      }
    },
    'fx*': {
      argCount: 2,
      gen: function(si) {
       return fxToInt() +
        '  imul ' + firstArg(si) + ', %eax\n';
      }
    },
    'fx/': {
      argCount: 2,
      gen: function(si) {
       return fxToInt() +
          '  movl %eax, ' + (si - 4) + '(%esp)\n' +
          '  movl ' + firstArg(si) + ', %eax\n' +
          '  movl %edx, ' + (si - 8) + '(%esp)\n' +
          '  cdq\n' +
          '  idivl ' + (si - 4) + '(%esp)\n' +
          '  movl ' + (si - 8) + '(%esp), %edx\n' +
          '  andl $' + ~FX_MASK + ', %eax\n';
      }
    },
    cons: {
      argCount: 2,
      gen: function(si) {
        return '  movl %eax, ' + CDR_OFFSET + '(%ebp)\n' +
          '  movl ' + firstArg(si) + ', %eax\n' +
          '  movl %eax, ' + CAR_OFFSET + '(%ebp)\n' +
          '  movl %ebp, %eax\n' +
          '  orl $' + PAIR_TAG + ', %eax\n' +
          '  add $' + SIZE_PAIR + ', %ebp\n';
      }
    },
    car: {
      argCount: 1,
      gen: function(si) {
        return '  movl ' + (-1 + CAR_OFFSET) + '(%eax), %eax\n';
      }
    },
    cdr: {
      argCount: 1,
      gen: function(si) {
        return '  movl ' + (-1 + CDR_OFFSET) + '(%eax), %eax\n';
      }
    },
    'pair?': {
      argCount: 1,
      gen: function(si) {
        return '  andl $' + PAIR_MASK + ', %eax\n' +
          '  cmpl $' + PAIR_TAG + ', %eax\n' +
          zfToBool();
      }
    },
    'make-vector': {
      argCount: 2,
      gen: function(si) {
        var endLabel = uniq_label('fill_array_end');
        var startLabel = uniq_label('fill_array_start');
        return '  movl ' + firstArg(si) + ', %ebx\n' +
          // vector length is stored as encoded fx
          '  movl %ebx, (%ebp)\n' +
          '  movl %ebp, ' + (si - 4) + '(%esp)\n' +
          fxToInt('%ebx') +
          '  movl %ebx, ' + (si - 8) + '(%esp)\n' +
          startLabel + ':\n' +
          '  testl %ebx, %ebx\n' +
          '  jz ' + endLabel + '\n' +
          '  subl $1, %ebx\n' +
          '  leal 4(%ebp, %ebx, 4), %edx\n' +
          '  movl %eax, (%edx)\n' +
          '  jmp ' + startLabel + '\n' +
          endLabel + ':\n' +
          '  movl ' + (si - 8) + '(%esp), %ebx\n' +
          '  imul $4, %ebx\n' +
          '  addl $4, %ebx\n' +
          eightByteAlign('%ebx') +
          '  addl %ebx, %ebp\n' +
          '  movl ' + (si - 4) + '(%esp), %eax\n' +
          '  orl $' + VECTOR_TAG + ', %eax\n';
      }
    },
    'vector-length': {
      argCount: 1,
      gen: function(si) {
        return '  movl ' + (0 - VECTOR_TAG) + '(%eax), %eax\n';
      }
    },
    'vector-ref': {
      argCount: 2,
      gen: function(si) {
        return fxToInt() +
          '  movl ' + firstArg(si) + ', %ebx\n' +
          '  leal ' + (4 - VECTOR_TAG) + '(%ebx,%eax,4), %ebx\n' +
          '  movl (%ebx), %eax\n';
      }
    },
    'vector-set!': {
      argCount: 3,
      gen: function(si) {
        return '  movl ' + firstArg(si) + ', %ebx\n' +
          '  movl ' + secondArg(si) + ', %edx\n' +
          fxToInt('%edx') +
          '  leal ' + (4 - VECTOR_TAG) + '(%ebx,%edx,4), %ebx\n' +
          '  movl %eax, (%ebx)\n' +
          '  movl ' + firstArg(si) + ', %eax\n';
      }
    },
    'string?': {
      argCount: 1,
      gen: function(si) {
        return '  andl $' + STRING_MASK + ', %eax\n' +
          '  cmpl $' + STRING_TAG + ', %eax\n' +
          zfToBool();
      }
    },
    'make-string': {
      argCount: 1,
      gen: function(si) {
        return '  movl %ebp, %ebx\n' +
          '  movl %eax, (%ebp)\n' +
          '  addl $4, %ebp\n' +
          fxToInt('%eax') +
          '  addl %eax, %ebp\n' +
          eightByteAlign('%ebp') +
          '  movl %ebx, %eax\n' +
          '  orl $' + STRING_TAG + ', %eax\n';
      }
    },
    'string-length': {
      argCount: 1,
      gen: function(si) {
        return '  movl ' + (0 - STRING_TAG) + '(%eax), %eax\n';
      }
    },
    'string-set!': {
      argCount: 3,
      gen: function(si) {
        return '  movl ' + firstArg(si) + ', %ebx\n' +
          '  movl ' + secondArg(si) + ', %edx\n' +
          fxToInt('%edx') +
          '  leal ' + (4 - STRING_TAG) + '(%ebx, %edx), %edx\n' +
          // characters not encoded, but 8-bit only
          '  shr $' + CHAR_OFFSET + ', %eax\n' +
          '  mov %al, (%edx)\n';
      }
    },
    'string-ref': {
      argCount: 2,
      gen: function(si) {
        return '  movl ' + firstArg(si) + ', %ebx\n' +
          '  ' + fxToInt('%eax') +
          '  leal ' + (4 - STRING_TAG) + '(%ebx, %eax), %ebx\n' +
          '  movl (%ebx), %eax\n' +
          '  shl $' + CHAR_OFFSET + ', %eax\n' +
          '  orl $' + CHAR_TAG + ', %eax\n';
      }
    }
  };

  var map = {
    'char=': 'fx=',
    'char<': 'fx<',
    'char<=': 'fx<=',
    'char>': 'fx>',
    'char>=': 'fx>='
  };
  for (var op in map)
    primitives[op] = primitives[map[op]];

  var primitive = function(env, si, name, args) {
    var config = primitives[name.val];
    if (!config) {
      abort('unkown primitive: ' + name.val);
    }

    if (args.length != config.argCount) {
      abort('invalid arg count, expected ' + config.argCount +
            ' but was: ' + args.length);
    }

    // last argument is in eax
    // second in sp
    // third in sp-4, ...
    var c = config.argCount, tmpSi = si;
    var asm = expression(env, si, args[0]);
    for (var i = 1; i < c; i++) {
      asm += '  movl %eax, ' + tmpSi + '(%esp)\n';
      tmpSi -= 4;
      asm += expression(env, tmpSi, args[i]);
    }

    asm += config.gen(si);

    return asm;
  };

  var ifConditional = function(env, si, ast, tail) {
    if (ast.length != 3) {
      abort("conditional with wrong number of arguments (expected 3), was " + ast.length);
    }

    var asm = '';

    var falseLabel = uniq_label(),
        endLabel = uniq_label();

    // eval condition
    asm += expression(env, si, ast[0]);

    // jmp if false
    asm += '  cmp $' + BOOL_FALSE + ', %al\n' +
      '  je ' + falseLabel + '\n';

    // true branch
    asm += expression(env, si, ast[1], tail);

    // jmp to end
    asm += '  jmp ' + endLabel + '\n';

    // false branch
    asm += falseLabel + ':                      # false-branch\n';
    asm += expression(env, si, ast[2], tail);

    // end
    asm += endLabel + ':\n';

    return asm;
  };

  var andConditional = function(env, si, nodes, tail) {
    if (nodes.length == 0) {
      return expression(env, si, boolToken(true), tail);
    } else if (nodes.length == 1) {
      return expression(env, si, nodes[0], tail);
    } else {
      var subExpr = [nameToken('and')];
      subExpr = subExpr.concat(nodes.slice(1));
      return expression(env, si,
        [nameToken('if'), nodes[0], subExpr, boolToken(false)], tail);
    }
  };

  var orConditional = function(env, si, nodes, tail) {
    if (nodes.length == 0) {
      return expression(env, si, boolToken(true), tail);
    } else if (nodes.length == 1) {
      return expression(env, si, nodes[0], tail);
    } else {
      var subExpr = [nameToken('or')];
      subExpr = subExpr.concat(nodes.slice(1));
      // the result is computed twice!!
      return expression(env, si,
        [nameToken('if'), nodes[0], nodes[0], subExpr], tail);
    }
  };

  var letBlock = function(parentEnv, si, parts, tail) {
    if (parts.length < 2) abort("a let blocks requires definition and" +
                                 " body (2 elements, but was " + parts.length + ")");

    // create new scope
    var env = {};
    for (var name in parentEnv) {
      env[name] = parentEnv[name];
    }

    var defs = parts[0];
    var body = parts.slice(1);

    var asm = '';

    for (var i = 0; i < defs.length; i += 2) {
      asm += expression(env, si, defs[i + 1]);
      asm += '  movl %eax, ' + si + '(%esp) # var ' + defs[i].val  + '\n';
      env[defs[i].val] = { pos: si };
      si -= 4;
    }

    for (var j = 0; j < body.length; j++) {
      asm += expression(env, si, body[j], tail);
    }

    return asm;
  };

  var varAccess = function(env, si, ast) {
    var varElem = env[ast.val];
    if (!varElem) abort('unkown variable: ' + ast.val);
	  return '  movl	' + varElem.pos + '(%esp), %eax\n';
  };

  var dbgStr = function(ast) {
    if (ast instanceof Array) {
      var ret = '( ';
      for (var i = 0; i < ast.length; i++) {
        ret += dbgStr(ast[i]) + ' ';
      }
      return ret + ')';
    } else {
      return ast.val;
    }
  };

  var apply = function(env, si, ast, tail) {
    var fnLabel = env[ast[0].val];

    var asm = '';

    var oldSi = si;
    // space for return address
    si -= 4;

    var arg2Stk = {};
    // push arg values onto the stack
    for (var i = 1; i < ast.length; i++) {
      asm += '                        ';
      asm += '  # arg(' + (i - 1) + ') = ' + dbgStr(ast[i]) + '\n';
      asm += expression(env, si, ast[i]);
      asm += '  movl %eax, ' + si + '(%esp)\n';
      arg2Stk[i] = si;
      si -= 4;
    }

    if (tail) {
      // copy arg values over locals
      var relSi = -4;
      for (var j = 1; j < ast.length; j++) {
        asm += '  movl ' + arg2Stk[j] + '(%esp), %eax\n';
        asm += '  movl %eax, ' + relSi + '(%esp)\n';
        relSi -= 4;
      }

      asm += '  jmp ' + fnLabel + '\n';
    } else {
      var offset = Math.abs(oldSi) - 4;
      asm += '  subl $' + offset + ', %esp\n';
      asm += '  call ' + fnLabel + '\n';
      asm += '  addl $' + offset + ', %esp\n';
    }

    return asm;
  };

  var isLiteral = function(node, str) {
    return node.type === 'NAME' && node.val === str;
  };

  var isImmediate = function(ast) {
    if (!isAtom(ast)) return false;
    if (ast instanceof Array) return true;

    return ['BOOL', 'NUMBER', 'CHAR'].indexOf(ast.type) !== -1;
  };

  var isVariable = function(ast) {
    return isAtom(ast) && ast.type == 'NAME';
  };

  var stringLiteral = function(env, si, ast) {
    var str = ast.val.slice(1, ast.val.length - 1);
    var len = str.length + 4;
    var nextObj = Math.floor((len + 7) / 8) * 8;

    var asm = '  movl %ebp, %eax\n' +
          '  movl $' + (str.length << INT_OFFSET) + ', (%eax)\n';

    for (var i = 0; i < str.length; i++) {
      var c = str.charCodeAt(i);
      asm += '  movb $' + c + ', ' + (4 + i) + '(%eax)\n';
    }

    asm += '  addl $' + nextObj + ', %ebp\n' +
          '  orl $' + STRING_TAG + ', %eax\n';

    return asm;
  };

  var expression = function(env, si, ast, tail) {
    if (isImmediate(ast)) {
	    return '  movl	$' + immediate(ast) + ', %eax\n';
    } else if (ast.type === 'STRING') {
      return stringLiteral(env, si, ast);
    } else if (isVariable(ast)) {
      return varAccess(env, si, ast);
    } else if (isLiteral(ast[0], 'if')) {
      return ifConditional(env, si, ast.slice(1), tail);
    } else if (isLiteral(ast[0], 'and')) {
      return andConditional(env, si, ast.slice(1), tail);
    } else if (isLiteral(ast[0], 'or')) {
      return orConditional(env, si, ast.slice(1), tail);
    } else if (isLiteral(ast[0], 'let')) {
      return letBlock(env, si, ast.slice(1), tail);
    } else if (isLiteral(ast[0], 'app')) {
      return apply(env, si, ast.slice(1), tail);
    } else {
      return primitive(env, si, ast[0], ast.slice(1));
    }
  };

  var lambda = function(env, ast, tail) {
    var si = -4;
    ensure(ast[0].val == 'lambda', 'lambda expected, was: ' + ast[0].val);
    var params = ast[1];

    var asm = '';

    for (var i = 0; i < params.length; i++) {
      env[params[i].val] = { pos: si};
      si -= 4;
    }

    var bodyStatements = ast.slice(2);
    for (var j = 0; j < bodyStatements.length; j++) {
      asm += expression(env, si, bodyStatements[j], tail);
    }

    asm += '  ret\n';

    return asm;
  };

  var letrec = function(env, si, ast) {
    var defs = ast[1];
    if (defs.length % 2 != 0) abort('letrec definitons must be even');

    var asm = '';
    asm += '  # letrec\n';

    var bodyLabel = uniq_label();
    asm += '  jmp ' + bodyLabel + '\n';

    for (var i = 0; i < defs.length; i+=2) {
      var fnName = defs[i].val;
      var label = uniq_label(fnName);
      env[fnName] = label;

      asm += label + ':               # fn ' + fnName + '(..)\n';
      asm += lambda(env, defs[i + 1], true);
    }

    asm += bodyLabel + ':             # letrec-body\n';
    var bodyStatements = ast.slice(2);
    for (var j = 0; j < bodyStatements.length; j++) {
      asm += expression(env, si, bodyStatements[j], false);
    }

    return asm;
  };

  var programme = function(ast) {
    if (!isAtom(ast) && ast[0].val == 'letrec') {
      return letrec({}, -4, ast);
    } else {
      return expression({}, -4, ast);
    }
  };

  return '	.text\n' +
	  '  .globl	scheme_entry\n' +
    'scheme_entry:\n' +
    // pointer to struct
    '  movl 4(%esp), %ecx\n' +
    // save registers
    '  movl %ebx, 4(%ecx)\n' +
    '  movl %esi, 16(%ecx)\n' +
    '  movl %edi, 20(%ecx)\n' +
    '  movl %ebp, 24(%ecx)\n' +
    '  movl %esp, 28(%ecx)\n' +
    // set stack pointer and heap pointer
    '  movl 12(%esp), %ebp\n' +
    '  movl 8(%esp), %esp\n' +

    programme(ast) +

    '  movl 4(%ecx), %ebx\n' +
    '  movl 16(%ecx), %esi\n' +
    '  movl 20(%ecx), %edi\n' +
    '  movl 24(%ecx), %ebp\n' +
    '  movl 28(%ecx), %esp\n' +
	  '  ret\n';
};

var build = function(asm, cb) {
  var curId = id();
  var asmFile = 'mine' + curId + '.s';
  var binFile = 'test' + curId;
  fs.writeFileSync(asmFile, asm);
  exec('gcc -Wall ' + asmFile + ' runtime.c -o ' + binFile, function(err) {
    if (err) throw err;
    if (cb) cb(binFile);
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
    try {
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
    } catch (e) {
      pending--; nFailed++;
      console.log( 'Test failed with exception: ' + t[0]);
      console.log( e );
    }
  });

};

var quickTest = true;


test('101', '101');
test('0', '0');
test('42', '42');
test('-1', '-1');
test('100000', '100000');
if (!quickTest) {
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
test('(fx+ 1 1)', '2');
test('(fx+ 1 (fx+ 1 40))', '42');
test('(fx+ 1 (fx+ (fx+ 1 -1) (fx+ 40 10)))', '51');
test('(fx- 1 1)', '0');
test('(fx- (fx+ 42 1) 1)', '42');
test('(fx+ 1 -1)', '0');
test('(fxzero? (fx+ 1 -1))', '#t');
test('(fx+ 5 -10)', '-5');
test('(fx+ 536870911 1)', '-536870912');
test('(fx= 536870911 1)', '#f');
test('(fx= 536870911 536870911)', '#t');
test('(fx= 5 -10)', '#f');
test('(fx= 5 (fx+ 15 -10))', '#t');
test('(fx< 536870911 1)', '#f');
test('(fx< 536870911 536870911)', '#f');
test('(fx< 1 536870911)', '#t');
test('(fx< 536870911 -536870912)', '#f');
test('(fx< -536870912 536870911)', '#t');
test('(fx< -536870911 -536870910)', '#t');
test('(fx< 5 -10)', '#f');
test('(fx< -10 5)', '#t');
test('(fx< 4 5)', '#t');
test('(fx< 5 5)', '#f');
test('(fx< 5 4)', '#f');
test('(fx< 5 (fx+ 5 1))', '#t');
test('(fx<= 536870911 1)', '#f');
test('(fx<= 536870911 536870911)', '#t');
test('(fx<= 1 536870911)', '#t');
test('(fx<= 536870911 -536870912)', '#f');
test('(fx<= -536870912 536870911)', '#t');
test('(fx<= -536870911 -536870910)', '#t');
test('(fx<= 5 -10)', '#f');
test('(fx<= -10 5)', '#t');
test('(fx<= 4 5)', '#t');
test('(fx<= 5 5)', '#t');
test('(fx<= 5 4)', '#f');
test('(fx<= 5 (fx+ 5 1))', '#t');
test('(fx> 536870911 1)', '#t');
test('(fx> 536870911 536870911)', '#f');
test('(fx> 1 536870911)', '#f');
test('(fx> 536870911 -536870912)', '#t');
test('(fx> -536870912 536870911)', '#f');
test('(fx> -536870911 -536870910)', '#f');
test('(fx> 5 -10)', '#t');
test('(fx> -10 5)', '#f');
test('(fx> 4 5)', '#f');
test('(fx> 5 5)', '#f');
test('(fx> 5 4)', '#t');
test('(fx> 5 (fx+ 5 1))', '#f');
test('(fx>= 536870911 1)', '#t');
test('(fx>= 536870911 536870911)', '#t');
test('(fx>= 1 536870911)', '#f');
test('(fx>= 536870911 -536870912)', '#t');
test('(fx>= -536870912 536870911)', '#f');
test('(fx>= -536870911 -536870910)', '#f');
test('(fx>= 5 -10)', '#t');
test('(fx>= -10 5)', '#f');
test('(fx>= 4 5)', '#f');
test('(fx>= 5 5)', '#t');
test('(fx>= 5 4)', '#t');
test('(fx>= 5 (fx+ 5 1))', '#f');
test('(char= #\\a #\\a)', '#t');
test('(char= #\\a #\\b)', '#f');
test('(char< #\\a #\\a)', '#f');
test('(char< #\\c #\\a)', '#f');
test('(char< #\\a #\\b)', '#t');
test('(char<= #\\a #\\a)', '#t');
}
test('(char<= #\\c #\\a)', '#f');
test('(char<= #\\a #\\b)', '#t');
test('(char> #\\a #\\a)', '#f');
test('(char> #\\c #\\a)', '#t');
test('(char> #\\a #\\b)', '#f');
test('(char>= #\\a #\\a)', '#t');
test('(char>= #\\c #\\a)', '#t');
test('(char>= #\\a #\\b)', '#f');
test('(fx* 2 3)', '6');
test('(fx* 2 268435455)', '536870910');
test('(fx/ 4 2)', '2');
test('(fx/ 5 2)', '2');
test('(fx/ 6 2)', '3');
test('(fx/ -6 2)', '-3');
test('(fx/ -6 -2)', '3');
test('(fx/ 6 -2)', '-3');
test('(fx/ 536870910 2)', '268435455');
test('(let (a 40 b 2) a)', '40');
test('(let (a 40 b 2) b)', '2');
test('(let (a 40 b 2) (fx+ a b))', '42');
test('(let (foo 10) (let (bar 5) (fx+ foo bar)))', '15');
test('(let (foo 10) (let (foo 5) foo))', '5');
test('(fx+ (let (foo 10) foo) (let (foo 5) foo))', '15');
test('(let (foo 10) (fx+ (let (foo 5) foo) foo))', '15');
test('(let (a 40 b (fx+ 2 a)) (fx+ a b))', '82');
test('(letrec (myadd (lambda (a b) (fx+ a b))) (app myadd 2 40))', '42');
test('(letrec (myadd (lambda (a b) (fx+ a b))' +
     '         myplusX (lambda (foo) (fx+ 100 foo)))' +
     '    (app myplusX (app myadd 2 40)))', '142');
test('(letrec (fib (lambda (n) (if (fx= n 1) 1 (if (fx= n 2) 1 ' +
     ' (fx+ (app fib (fx- n 1)) (app fib (fx- n 2)))))))' +
     '  (app fib 30))', '832040');
test('(letrec (fib-help (lambda (a b n) ' +
     '             (if (fx= n 1)' +
     '               b' +
     '               (if (fx= n 2)' +
     '                b' +
     '                (app fib-help b (fx+ a b) (fx- n 1)))))' +
     '        fib (lambda (n) (app fib-help 1 1 n)))' +
     '             (app fib 30))', '832040');
test('(letrec (sum (lambda (n ac)' +
'(if (fxzero? n)' +
'ac' +
'(app sum (fxsub1 n) (fx+ n ac)))))' +
'(app sum 10 0))', '55');
test('(letrec (myadd (lambda (a b) ' +
     '(if (fxzero? a) b (app myadd (fxsub1 a) (fxadd1 b))))) (app myadd 22000 40))', '22040');
test('(letrec (myadd (lambda (a b) ' +
     '(if (fxzero? a) b (let (diff 1) (app myadd (fx- a diff) (fx+ diff b))))))' +
     ' (app myadd 22000 40))', '22040');
test('(letrec (myadd (lambda (a b) ' +
     '(or (and (fxzero? a) b)' +
     '      (app myadd (fxsub1 a) (fxadd1 b)))))' +
     ' (app myadd 5461 40))', '5501');
test('(let (pair (cons 42 -3))' +
     '  (car pair))', '42');
test('(let (pair (cons 42 -3))' +
     '  (cdr pair))', '-3');
test('(pair? 42)', '#f');
test('(pair? #\\x)', '#f');
test('(pair? #t)', '#f');
test('(pair? #f)', '#f');
test('(pair? ())', '#f');
test('(pair? (cons 40 2))', '#t');
test('(pair? (cons #f #t))', '#t');
test('(pair? (cons #f (cons 40 2)))', '#t');
test('(let (ll (cons 42 (cons 2 (cons 101 ()))))' +
     '  (car (cdr (cdr ll))))', '101');
test('(vector-length (make-vector 4 #t))', '4');
// resulting heap position is already aligned (4*1+4 == 8)
test('(let (v (make-vector 1 #t)' +
    '       p (cons 2 3))' +
    '   (pair? p))', '#t');
test('(let (v (make-vector 1 #t)' +
    '       p (cons 2 3))' +
    '   (vector-length v))', '1');
// resulting heap position is not aligned (4*4+4 == 20 -> 24)
test('(let (v (make-vector 4 #t)' +
    '       p (cons 2 3))' +
    '   (pair? p))', '#t');
// access vector
test('(vector-ref (make-vector 101 42) 100)', '42');
test('(vector-ref (make-vector 101 42) 0)', '42');
test('(vector-ref (make-vector 1 42) 0)', '42');
test('(let (v (make-vector 3 1)' +
     '      v2 (vector-set! v 0 2))' +
     '  (vector-ref v 0))', '2');
test('(let (v (make-vector 3 1)' +
     '      v2 (vector-set! v 0 2))' +
     '  (vector-ref v2 0))', '2');
test('(let (v (make-vector 3 1)' +
     '      v2 (vector-set! v 0 2))' +
     '  (vector-ref v 1))', '1');
test('(let (v (make-vector 3 1)' +
     '      v2 (vector-set! v 0 2))' +
     '  (vector-length v2))', '3');
test('(let (v (make-vector 3 0)' +
     '      v2 (vector-set! v 0 (cons 2 3)))' +
     '  (car (vector-ref v2 0)))', '2');
// let, letrec and lambda as do-block with side-effects
test('(let (v (make-vector 3 0))' +
     '  (vector-set! v 0 (cons 2 3))' +
     '  (car (vector-ref v 0)))', '2');
test(' (letrec (fill-help (lambda (vec val i)' +
'                    (vector-set! vec i val)' +
'                    (let (n (fxsub1 i))' +
'                      (if (fx< 0 n)' +
'                          (app fill-help vec val n)' +
'                        vec)))' +
'         fill (lambda (vec val) (app fill-help vec val (fxsub1 (vector-length vec)))))' +
'   (let (v (make-vector 3 1))' +
'     (app fill v 2)' +
'     (vector-ref v 1)))', '2');
test('(string? (make-string 0))', '#t');
test('(string-length (make-string 3))', '3');
test('(make-string 0)', '""');
test('(let (s (make-string 3))' +
     '     (string-set! s 0 #\\a)' +
     '     (string-set! s 1 #\\b)' +
     '     (string-set! s 2 #\\c)' +
     '     s)', '"abc"');
// string literals
test('"foo"', '"foo"');
test('""', '""');
// ref char in string
test('(string-ref "foo" 0)', '#\\f');
test('(string-ref "foo" 2)', '#\\o');


runTests();


// var prog = '(letrec (myadd (lambda (a b) '
//       + '(if (fxzero? a) b (app myadd (fxsub1 a) (fxadd1 b)))))' +
//      ' (app myadd 5461 40))';


//var prog = '(vector-ref (make-vector 101 42) 100)';

var prog = ' (letrec (fill-help (lambda (vec val i)' +
'                    (vector-set! vec i val)' +
'                    (let (n (fxsub1 i))' +
'                      (if (fx< 0 n)' +
'                          (app fill-help vec val n)' +
'                        vec)))' +
'         fill (lambda (vec val) (app fill-help vec val (fxsub1 (vector-length vec)))))' +
'   (let (v (make-vector 3 1))' +
'     (app fill v 2)' +
'     (vector-ref v 1)))';

prog = '"foo"';


/*
compileAndRun(prog, function(output) {
  console.log( 'result: ' + output );
});

var asm = compile(prog);
console.log( asm );
build(asm);
 */
