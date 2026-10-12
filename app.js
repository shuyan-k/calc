/* =========================================================
   1. 表达式求值引擎
   ========================================================= */

const FUNC_NAMES = new Set([
  'sin','cos','tan','asin','acos','atan',
  'sinh','cosh','tanh',
  'ln','log','log10','log2',
  'sqrt','cbrt','abs','exp',
  'floor','ceil','round','sign','deg','rad'
]);

const CONSTANTS = {
  'pi': Math.PI, 'π': Math.PI,
  'e': Math.E,
  'tau': 2 * Math.PI, 'τ': 2 * Math.PI,
  'phi': (1 + Math.sqrt(5)) / 2, 'φ': (1 + Math.sqrt(5)) / 2
};

/* 用户定义函数表：{ f: { body:'sin(x)', rpn:[...], error:null } } */
const userFunctions = {};

const isDigitCh = c => /[0-9]/.test(c);
const isLetterCh = c => /[\p{L}_]/u.test(c);

/* ---------- Tokenizer ---------- */
function tokenize(input) {
  const tokens = [];
  let i = 0;
  const n = input.length;

  while (i < n) {
    const c = input[i];
    if (/\s/.test(c)) { i++; continue; }

    // 数字
    if (isDigitCh(c) || c === '.') {
      let numStr = '';
      while (i < n && (isDigitCh(input[i]) || input[i] === '.')) numStr += input[i++];
      if (i < n && (input[i] === 'e' || input[i] === 'E')) {
        let j = i + 1, sign = '';
        if (j < n && (input[j] === '+' || input[j] === '-')) sign = input[j++];
        if (j < n && isDigitCh(input[j])) {
          numStr += 'e' + sign; i = j;
          while (i < n && isDigitCh(input[i])) numStr += input[i++];
        }
      }
      if (numStr.endsWith('.')) numStr = numStr.slice(0, -1);
      const val = parseFloat(numStr);
      if (isNaN(val)) throw new Error('无效数字：' + numStr);
      const exactInteger = !/[.eE]/.test(numStr) ? BigInt(numStr) : null;
      tokens.push({ type: 'number', value: exactInteger ?? val });
      continue;
    }

    // 标识符
    if (isLetterCh(c)) {
      let name = '';
      while (i < n && (isLetterCh(input[i]) || isDigitCh(input[i]))) name += input[i++];
      const lower = name.toLowerCase();

      // 微积分函数：d(f, x) 求导，i(f, a, b) 求定积分。
      if (lower === 'diff' || lower === 'deriv' || lower === 'd' ||
          lower === 'integral' || lower === 'int' || lower === 'i') {
        let j = i;
        while (j < n && /\s/.test(input[j])) j++;
        if (j < n && input[j] === '(') {
          j++;
          while (j < n && /\s/.test(input[j])) j++;
          let fnName = '';
          while (j < n && (isLetterCh(input[j]) || isDigitCh(input[j]))) fnName += input[j++];
          while (j < n && /\s/.test(input[j])) j++;
          if (fnName && j < n && input[j] === ',') {
            tokens.push({ type: 'specialFn', name: lower, fnName: fnName.toLowerCase() });
            tokens.push({ type: 'leftParen' });
            i = j + 1;
            continue;
          }
        }
      }

      tokens.push({ type: 'identifier', name: lower });
      continue;
    }

    // 运算符 & 括号
    switch (c) {
      case '+': case '-': case '*': case '/': case '%': case '^':
        tokens.push({ type: 'op', value: c }); i++; break;
      case '×': tokens.push({ type: 'op', value: '*' }); i++; break;
      case '÷': tokens.push({ type: 'op', value: '/' }); i++; break;
      case '−': tokens.push({ type: 'op', value: '-' }); i++; break;
      case '(': tokens.push({ type: 'leftParen' }); i++; break;
      case ')': tokens.push({ type: 'rightParen' }); i++; break;
      case ',': tokens.push({ type: 'comma' }); i++; break;
      default: throw new Error('无效字符：' + c);
    }
  }
  return tokens;
}

/* ---------- 隐式乘法 ---------- */
function needsImplicitMul(prev, cur) {
  if (prev.type === 'comma' || cur.type === 'comma') return false;
  let prevOk = false;
  if (prev.type === 'number' || prev.type === 'rightParen') prevOk = true;
  else if (prev.type === 'identifier') {
    if (!FUNC_NAMES.has(prev.name) &&
        !(cur.type === 'leftParen' && userFunctions[prev.name])) prevOk = true;
  }
  const curOk = (cur.type === 'number' || cur.type === 'leftParen' || cur.type === 'identifier');
  return prevOk && curOk;
}
function insertImplicitMul(tokens) {
  const out = [];
  for (let i = 0; i < tokens.length; i++) {
    if (i > 0 && needsImplicitMul(tokens[i - 1], tokens[i])) out.push({ type: 'op', value: '*' });
    out.push(tokens[i]);
  }
  return out;
}

/* ---------- Shunting-yard ---------- */
function precedence(op) {
  switch (op) {
    case '+': case '-': return 1;
    case '*': case '/': case '%': return 2;
    case '~': return 3;
    case '^': return 4;
    default: return 0;
  }
}
const isRightAssoc = op => op === '^' || op === '~';
const isUnaryPos = prev => !prev || prev.type === 'op' || prev.type === 'leftParen';

function toRPN(tokens) {
  tokens = insertImplicitMul(tokens);
  const output = [];
  const stack = [];
  let prev = null;

  for (const token of tokens) {
    if (token.type === 'number') {
      output.push(token);
    } else if (token.type === 'identifier') {
      const name = token.name;
      if (CONSTANTS[name] !== undefined) {
        output.push({ type: 'number', value: CONSTANTS[name] });
      } else if (name === 'x') {
        output.push(token);
      } else {
        // 内置函数 or 用户函数，都压栈（求值时再区分）
        stack.push(token);
      }
    } else if (token.type === 'specialFn') {
      stack.push(token);
    } else if (token.type === 'leftParen') {
      stack.push(token);
    } else if (token.type === 'rightParen') {
      let found = false;
      while (stack.length) {
        const top = stack[stack.length - 1];
        if (top.type === 'leftParen') { stack.pop(); found = true; break; }
        output.push(stack.pop());
      }
      if (!found) throw new Error('括号不匹配');
      const top = stack[stack.length - 1];
      if (top && (top.type === 'identifier' || top.type === 'specialFn')) {
        output.push(stack.pop());
      }
    } else if (token.type === 'comma') {
      let foundLeftParen = false;
      while (stack.length) {
        const top = stack[stack.length - 1];
        if (top.type === 'leftParen') { foundLeftParen = true; break; }
        output.push(stack.pop());
      }
      if (!foundLeftParen) throw new Error('参数分隔符位置错误');
    } else if (token.type === 'op') {
      const c = token.value;
      if (c === '-' && isUnaryPos(prev)) {
        stack.push({ type: 'op', value: '~' });
      } else if (c === '+' && isUnaryPos(prev)) {
        // 忽略
      } else {
        const p1 = precedence(c);
        while (stack.length) {
          const top = stack[stack.length - 1];
          let topP = null;
          if (top.type === 'op') topP = precedence(top.value);
          else if (top.type === 'identifier' || top.type === 'specialFn') topP = 5;
          if (topP === null) break;
          if (topP < p1) break;
          if (topP === p1 && isRightAssoc(c)) break;
          output.push(stack.pop());
        }
        stack.push(token);
      }
    }
    prev = token;
  }
  while (stack.length) {
    const top = stack.pop();
    if (top.type === 'leftParen') throw new Error('括号不匹配');
    output.push(top);
  }
  return output;
}

/* ---------- RPN 求值 ---------- */
const BUILTIN_FNS = angleMode => {
  const toRad = v => angleMode === 'deg' ? v * Math.PI / 180 : v;
  const fromRad = v => angleMode === 'deg' ? v * 180 / Math.PI : v;
  return {
    sin: v => Math.sin(toRad(v)), cos: v => Math.cos(toRad(v)), tan: v => Math.tan(toRad(v)),
    asin: v => { if (v < -1 || v > 1) throw new Error('asin 定义域错误'); return fromRad(Math.asin(v)); },
    acos: v => { if (v < -1 || v > 1) throw new Error('acos 定义域错误'); return fromRad(Math.acos(v)); },
    atan: v => fromRad(Math.atan(v)),
    sinh: Math.sinh, cosh: Math.cosh, tanh: Math.tanh,
    ln: v => { if (v <= 0) throw new Error('ln 定义域错误'); return Math.log(v); },
    log: v => { if (v <= 0) throw new Error('log 定义域错误'); return Math.log10(v); },
    log10: v => { if (v <= 0) throw new Error('log 定义域错误'); return Math.log10(v); },
    log2: v => { if (v <= 0) throw new Error('log2 定义域错误'); return Math.log2(v); },
    sqrt: v => { if (v < 0) throw new Error('sqrt 定义域错误'); return Math.sqrt(v); },
    cbrt: Math.cbrt, abs: Math.abs, exp: Math.exp,
    floor: Math.floor, ceil: Math.ceil, round: Math.round, sign: Math.sign,
    deg: v => v * 180 / Math.PI, rad: v => v * Math.PI / 180
  };
};

function realPower(base, exponent) {
  if (base >= 0 || Number.isInteger(exponent)) return Math.pow(base, exponent);
  for (let denominator = 1; denominator <= 1000; denominator++) {
    const numerator = Math.round(exponent * denominator);
    if (Math.abs(exponent - numerator / denominator) < 1e-12) {
      if (denominator % 2 === 0) return NaN;
      const sign = Math.abs(numerator) % 2 === 1 ? -1 : 1;
      return sign * Math.pow(-base, exponent);
    }
  }
  return NaN;
}

const MAX_EXACT_POWER = 4096n;
const MAX_EXACT_RESULT_DIGITS = 4096;

function toNumber(value) {
  return typeof value === 'bigint' ? Number(value) : value;
}

function arithmeticResult(a, b, operator) {
  if (typeof a === 'bigint' && typeof b === 'bigint') {
    if (operator === '+') return a + b;
    if (operator === '-') return a - b;
    if (operator === '*') return a * b;
    if (operator === '%') {
      if (b === 0n) throw new Error('除以零');
      return a % b;
    }
    if (operator === '/') {
      if (b === 0n) throw new Error('除以零');
      return a % b === 0n ? a / b : Number(a) / Number(b);
    }
    if (operator === '^' && b >= 0n) {
      if (a === 0n && b === 0n) return 1n;
      if (a === 0n || a === 1n || a === -1n) return a ** b;
      if (b > MAX_EXACT_POWER ||
          Math.floor(Math.log10(Math.abs(Number(a)))) * Number(b) > MAX_EXACT_RESULT_DIGITS) {
        throw new Error(`整数幂结果超过 ${MAX_EXACT_RESULT_DIGITS} 位`);
      }
      const result = a ** b;
      if (result.toString().length > MAX_EXACT_RESULT_DIGITS) {
        throw new Error(`整数幂结果超过 ${MAX_EXACT_RESULT_DIGITS} 位`);
      }
      return result;
    }
  }
  const left = toNumber(a), right = toNumber(b);
  switch (operator) {
    case '+': return left + right;
    case '-': return left - right;
    case '*': return left * right;
    case '/': if (right === 0) throw new Error('除以零'); return left / right;
    case '%': if (right === 0) throw new Error('除以零'); return left % right;
    case '^': return realPower(left, right);
  }
}

function isFiniteResult(value) {
  return typeof value === 'bigint' || Number.isFinite(value);
}

function callUserFunction(name, argValue, angleMode, callStack) {
  if (callStack.has(name)) throw new Error('循环引用：' + name);
  const fn = userFunctions[name];
  if (!fn || !fn.rpn) throw new Error('函数未就绪：' + name);
  const newStack = new Set(callStack);
  newStack.add(name);
  return evaluateRPN(fn.rpn, { x: argValue }, angleMode, newStack);
}

function numericalDerivative(fnName, x, angleMode, callStack) {
  const h = 1e-6;
  const f1 = callUserFunction(fnName, x + h, angleMode, callStack);
  const f2 = callUserFunction(fnName, x - h, angleMode, callStack);
  return (f1 - f2) / (2 * h);
}

function numericalIntegral(fnName, a, b, angleMode, callStack) {
  const n = 500;
  const h = (b - a) / n;
  let sum = callUserFunction(fnName, a, angleMode, callStack)
          + callUserFunction(fnName, b, angleMode, callStack);
  for (let i = 1; i < n; i++) {
    const x = a + i * h;
    sum += (i % 2 === 0 ? 2 : 4) * callUserFunction(fnName, x, angleMode, callStack);
  }
  return sum * h / 3;
}

function evaluateRPN(rpn, env, angleMode, callStack) {
  const fns = BUILTIN_FNS(angleMode);
  const stack = [];

  for (const token of rpn) {
    if (token.type === 'number') {
      stack.push(token.value);
    } else if (token.type === 'identifier') {
      const name = token.name;
      if (name === 'x') {
        if (env.x === undefined) throw new Error('未定义变量 x');
        stack.push(env.x);
      } else if (fns[name]) {
        const a = stack.pop();
        if (a === undefined) throw new Error('表达式不完整');
        stack.push(fns[name](toNumber(a)));
      } else if (userFunctions[name]) {
        const a = stack.pop();
        if (a === undefined) throw new Error('表达式不完整');
        stack.push(callUserFunction(name, a, angleMode, callStack));
      } else {
        throw new Error('未定义函数：' + name);
      }
    } else if (token.type === 'specialFn') {
      if (token.name === 'diff' || token.name === 'deriv' || token.name === 'd') {
        const x = stack.pop();
        if (x === undefined) throw new Error('d 参数不完整');
        stack.push(numericalDerivative(token.fnName, toNumber(x), angleMode, callStack));
      } else if (token.name === 'integral' || token.name === 'int' || token.name === 'i') {
        const b = stack.pop();
        const a = stack.pop();
        if (a === undefined || b === undefined) throw new Error('i 参数不完整');
        stack.push(numericalIntegral(token.fnName, toNumber(a), toNumber(b), angleMode, callStack));
      }
    } else if (token.type === 'op') {
      const c = token.value;
      if (c === '~') {
        const a = stack.pop();
        if (a === undefined) throw new Error('表达式不完整');
        stack.push(-a);
      } else {
        const b = stack.pop();
        const a = stack.pop();
        if (a === undefined || b === undefined) throw new Error('表达式不完整');
        switch (c) {
          case '+': case '-': case '*': case '/': case '%': case '^':
            stack.push(arithmeticResult(a, b, c)); break;
        }
      }
    }
  }
  if (stack.length !== 1) throw new Error('表达式不完整');
  return stack[0];
}

function evaluate(expr, env, angleMode, callStack) {
  const s = String(expr).trim();
  if (!s) throw new Error('表达式为空');
  return evaluateRPN(toRPN(tokenize(s)), env || {}, angleMode, callStack || new Set());
}

/* 编译用户函数体为 RPN（缓存） */
function compileUserFunction(name) {
  const fn = userFunctions[name];
  if (!fn) return;
  try {
    fn.rpn = toRPN(tokenize(fn.body || ''));
    fn.error = null;
  } catch (e) {
    fn.rpn = null;
    fn.error = e.message;
  }
}
function compileAllUserFunctions() {
  for (const name in userFunctions) compileUserFunction(name);
}

/* =========================================================
   2. Tab 切换
   ========================================================= */
let angleMode = 'rad';
document.querySelectorAll('.tabbar button').forEach(btn => {
  btn.addEventListener('click', () => {
    document.querySelectorAll('.tabbar button').forEach(b => b.classList.remove('active'));
    document.querySelectorAll('.screen').forEach(s => s.classList.remove('active'));
    btn.classList.add('active');
    document.getElementById('screen-' + btn.dataset.tab).classList.add('active');
    if (btn.dataset.tab === 'graph') requestAnimationFrame(() => setTimeout(drawGraph, 30));
  });
});

/* =========================================================
   3. 科学计算器
   ========================================================= */
const exprEl   = document.getElementById('expr');
const resultEl = document.getElementById('result');
const angleBtn = document.getElementById('angleToggle');
let expr = '';
const MAX_INPUT_DIGITS = 64;

function formatNumber(v) {
  if (typeof v === 'bigint') return v.toString();
  if (!isFinite(v)) return v > 0 ? '∞' : '-∞';
  if (Number.isInteger(v) && Math.abs(v) < 1e15) return v.toString();
  let s = v.toPrecision(12);
  if (s.includes('e')) return parseFloat(s).toString();
  if (s.includes('.')) s = s.replace(/0+$/, '').replace(/\.$/, '');
  return s;
}

function highlightExpr(str) {
  if (!str) return '';
  let html = str.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  return html.replace(/([a-zA-Z\u03b1-\u03c9\u03d5]+)|([0-9.]+)|([\+\-\*\/\^\%\(\)])/g,
    (m, ident, num, op) => {
      if (ident) {
        const lower = ident.toLowerCase();
        if (FUNC_NAMES.has(lower) || lower === 'diff' || lower === 'deriv' ||
            lower === 'integral' || lower === 'int' || lower === 'd' || lower === 'i') {
          return `<span class="hl-func">${m}</span>`;
        }
        if (lower === 'x') return `<span class="hl-var">${m}</span>`;
        if (CONSTANTS[lower] !== undefined) return `<span class="hl-func">${m}</span>`;
        if (userFunctions[lower]) return `<span class="hl-userfn">${m}</span>`;
        return m;
      }
      if (num) return `<span class="hl-num">${m}</span>`;
      if (op) return `<span class="hl-op">${m}</span>`;
      return m;
    }).replace(
      /<span class="hl-op">\^<\/span>(<span class="hl-(?:num|var|userfn)">.*?<\/span>)/g,
      '<sup>$1</sup>'
    );
}

function renderCalc() {
  if (!expr) {
    exprEl.innerHTML = '0';
    resultEl.textContent = '';
    resultEl.classList.remove('error');
    return;
  }
  exprEl.innerHTML = highlightExpr(expr);
  try {
    const v = evaluate(expr, {}, angleMode);
    if (isFiniteResult(v)) {
      resultEl.textContent = '= ' + formatNumber(v);
      resultEl.classList.remove('error');
    } else {
      resultEl.textContent = '结果超出数值范围'; resultEl.classList.add('error');
    }
  } catch (e) {
    resultEl.textContent = e.message || '表达式暂不完整';
    resultEl.classList.add('error');
  }
}

document.querySelectorAll('.pad .key').forEach(btn => {
  btn.addEventListener('click', () => {
    const ins = btn.dataset.ins;
    const action = btn.dataset.action;
    if (ins !== undefined) {
      const digitCount = (expr.match(/[0-9]/g) || []).length;
      const addedDigits = (ins.match(/[0-9]/g) || []).length;
      if (digitCount + addedDigits > MAX_INPUT_DIGITS) {
        resultEl.textContent = `最多输入 ${MAX_INPUT_DIGITS} 位数字`;
        resultEl.classList.add('error');
        return;
      }
      expr += ins;
      renderCalc();
    }
    else if (action === 'clear') { expr = ''; renderCalc(); }
    else if (action === 'back') { expr = expr.slice(0, -1); renderCalc(); }
    else if (action === 'equals') {
      try {
        const v = evaluate(expr, {}, angleMode);
        if (!isFiniteResult(v)) { resultEl.textContent = '结果超出数值范围'; resultEl.classList.add('error'); return; }
        expr = formatNumber(v);
        renderCalc();
      } catch (e) {
        resultEl.textContent = e.message || '错误';
        resultEl.classList.add('error');
      }
    }
  });
});

angleBtn.addEventListener('click', () => {
  angleMode = angleMode === 'rad' ? 'deg' : 'rad';
  angleBtn.textContent = angleMode === 'rad' ? 'RAD' : 'DEG';
  renderCalc(); drawGraph();
});

renderCalc();

/* =========================================================
   4. 函数定义
   ========================================================= */
const fnList = document.getElementById('fnList');
const RESERVED = new Set(['e', 'x', 'd', 'i', ...FUNC_NAMES]);

function nextFnName() {
  for (let i = 0; i < 26; i++) {
    const ch = String.fromCharCode(97 + i);
    if (RESERVED.has(ch)) continue;
    if (userFunctions[ch]) continue;
    return ch;
  }
  return 'f' + Date.now();
}

function renderFnList() {
  fnList.innerHTML = '';
  const names = Object.keys(userFunctions);
  if (names.length === 0) {
    const empty = document.createElement('div');
    empty.style.cssText = 'font-size:13px;color:#C7C7CC;padding:4px 0;';
    empty.textContent = '还没有函数，点下方按钮添加';
    fnList.appendChild(empty);
    return;
  }
  names.forEach(name => {
    const fn = userFunctions[name];
    const row = document.createElement('div');
    row.className = 'fn-row';

    const nameEl = document.createElement('span');
    nameEl.className = 'fn-name';
    nameEl.textContent = name + '(x)';

    const eqEl = document.createElement('span');
    eqEl.className = 'fn-eq';
    eqEl.textContent = '=';

    const input = document.createElement('input');
    input.className = 'fn-input';
    input.type = 'text';
    input.value = fn.body || '';
    input.placeholder = '如 sin(x) + 1';
    input.spellcheck = false;
    input.autocapitalize = 'off';
    input.autocorrect = 'off';
    input.autocomplete = 'off';
    input.addEventListener('input', e => {
      fn.body = e.target.value;
      compileUserFunction(name);
      saveCalculatorState();
      debounceDraw();
    });

    const delBtn = document.createElement('button');
    delBtn.className = 'btn-del';
    delBtn.textContent = '✕';
    delBtn.addEventListener('click', () => {
      delete userFunctions[name];
      saveCalculatorState();
      renderFnList();
      renderGraphInputs();
      drawGraph();
    });

    row.appendChild(nameEl);
    row.appendChild(eqEl);
    row.appendChild(input);
    row.appendChild(delBtn);
    fnList.appendChild(row);
  });
}

document.getElementById('addFn').addEventListener('click', () => {
  const name = nextFnName();
  userFunctions[name] = { body: '', rpn: null, error: null };
  compileUserFunction(name);
  saveCalculatorState();
  renderFnList();
});

/* =========================================================
   5. 多曲线绘图
   ========================================================= */
const canvas   = document.getElementById('graphCanvas');
const ctx      = canvas.getContext('2d');
const graphList = document.getElementById('graphList');
const COLORS = ['#007AFF', '#FF3B30', '#34C759', '#FF9500', '#AF52DE', '#5856D6', '#FF2D55', '#00C7BE'];

let graphFunctions = [
  { id: 1, expr: 'sin(x)', color: COLORS[0] }
];

const STORAGE_KEY = 'calc-app-state-v1';
function saveCalculatorState() {
  try {
    localStorage.setItem(STORAGE_KEY, JSON.stringify({
      userFunctions: Object.fromEntries(Object.entries(userFunctions).map(([name, fn]) => [name, fn.body])),
      graphFunctions: graphFunctions.map(({ id, expr, color }) => ({ id, expr, color }))
    }));
  } catch (error) {
    console.error('计算器数据保存失败：', error);
  }
}

function restoreCalculatorState() {
  try {
    const saved = localStorage.getItem(STORAGE_KEY);
    if (!saved) return;
    const state = JSON.parse(saved);
    if (state.userFunctions && typeof state.userFunctions === 'object' && !Array.isArray(state.userFunctions)) {
      for (const [name, body] of Object.entries(state.userFunctions)) {
        if (/^[a-z][a-z0-9]*$/i.test(name) && typeof body === 'string') {
          userFunctions[name.toLowerCase()] = { body, rpn: null, error: null };
          compileUserFunction(name.toLowerCase());
        }
      }
    }
    if (Array.isArray(state.graphFunctions) && state.graphFunctions.length) {
      graphFunctions = state.graphFunctions
        .filter(fn => fn && typeof fn.expr === 'string' && typeof fn.color === 'string')
        .map((fn, index) => ({
          id: Number.isSafeInteger(fn.id) ? fn.id : Date.now() + index,
          expr: fn.expr,
          color: COLORS.includes(fn.color) ? fn.color : COLORS[index % COLORS.length]
        }));
      if (!graphFunctions.length) graphFunctions = [{ id: 1, expr: 'sin(x)', color: COLORS[0] }];
    }
  } catch (error) {
    console.error('计算器数据读取失败：', error);
  }
}

restoreCalculatorState();

const view = { xMin: -10, xMax: 10, yMin: -6, yMax: 6 };

function renderGraphInputs() {
  graphList.innerHTML = '';
  graphFunctions.forEach(fn => {
    const row = document.createElement('div');
    row.className = 'graph-input-row';

    const dot = document.createElement('span');
    dot.className = 'color-dot';
    dot.style.backgroundColor = fn.color;

    const label = document.createElement('span');
    label.className = 'graph-label';
    label.textContent = 'y =';

    const input = document.createElement('input');
    input.className = 'graph-input';
    input.type = 'text';
    input.value = fn.expr;
    input.placeholder = '如 f(x) + g(x)';
    input.spellcheck = false;
    input.autocapitalize = 'off';
    input.autocorrect = 'off';
    input.autocomplete = 'off';
    input.addEventListener('input', e => {
      fn.expr = e.target.value;
      saveCalculatorState();
      debounceDraw();
    });

    row.appendChild(dot);
    row.appendChild(label);
    row.appendChild(input);

    const error = document.createElement('div');
    error.className = 'graph-input-error';
    error.setAttribute('aria-live', 'polite');

    if (graphFunctions.length > 1) {
      const delBtn = document.createElement('button');
      delBtn.className = 'btn-del';
      delBtn.textContent = '✕';
      delBtn.addEventListener('click', () => {
        graphFunctions = graphFunctions.filter(f => f.id !== fn.id);
        saveCalculatorState();
        renderGraphInputs();
        drawGraph();
      });
      row.appendChild(delBtn);
    }
    graphList.appendChild(row);
    graphList.appendChild(error);
    fn.errorElement = error;
  });
}

document.getElementById('addGraphFn').addEventListener('click', () => {
  const nextColor = COLORS[graphFunctions.length % COLORS.length];
  graphFunctions.push({ id: Date.now(), expr: '', color: nextColor });
  saveCalculatorState();
  renderGraphInputs();
  drawGraph();
});

function niceStep(range, targetCount) {
  const raw = range / targetCount;
  const pow = Math.pow(10, Math.floor(Math.log10(raw)));
  const n = raw / pow;
  let step;
  if (n < 1.5) step = 1; else if (n < 3) step = 2; else if (n < 7) step = 5; else step = 10;
  return step * pow;
}

function validateGraphExpression(src) {
  const rpn = toRPN(tokenize(src));
  let depth = 0;
  for (const token of rpn) {
    if (token.type === 'number' || token.type === 'identifier' && token.name === 'x') {
      depth++;
    } else if (token.type === 'identifier') {
      if (!BUILTIN_FNS(angleMode)[token.name] && !userFunctions[token.name]) {
        throw new Error('未定义函数：' + token.name);
      }
      if (userFunctions[token.name] && !userFunctions[token.name].rpn) {
        throw new Error(userFunctions[token.name].error || '函数未就绪：' + token.name);
      }
      if (depth < 1) throw new Error('表达式不完整');
    } else if (token.type === 'specialFn') {
      if (!userFunctions[token.fnName]?.rpn) {
        throw new Error('未定义函数：' + token.fnName);
      }
      const args = token.name === 'integral' || token.name === 'int' || token.name === 'i' ? 2 : 1;
      if (depth < args) throw new Error('表达式不完整');
      depth += 1 - args;
    } else if (token.type === 'op') {
      const args = token.value === '~' ? 1 : 2;
      if (depth < args) throw new Error('表达式不完整');
      depth += 1 - args;
    }
  }
  if (depth !== 1) throw new Error('表达式不完整');
}

function drawGraph() {
  const rect = canvas.getBoundingClientRect();
  if (rect.width === 0) return;
  const dpr = window.devicePixelRatio || 1;
  const W = rect.width, H = rect.height;
  if (canvas.width !== Math.round(W * dpr) || canvas.height !== Math.round(H * dpr)) {
    canvas.width  = Math.round(W * dpr);
    canvas.height = Math.round(H * dpr);
  }
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  ctx.fillStyle = '#FFFFFF';
  ctx.fillRect(0, 0, W, H);

  const toPx = (x, y) => [
    (x - view.xMin) / (view.xMax - view.xMin) * W,
    H - (y - view.yMin) / (view.yMax - view.yMin) * H
  ];

  const xStep = niceStep(view.xMax - view.xMin, 8);
  const yStep = niceStep(view.yMax - view.yMin, 6);

  // 网格
  ctx.lineWidth = 1;
  ctx.strokeStyle = '#F2F2F7';
  ctx.beginPath();
  for (let x = Math.ceil(view.xMin / xStep) * xStep; x <= view.xMax; x += xStep) {
    const [px] = toPx(x, 0); ctx.moveTo(px, 0); ctx.lineTo(px, H);
  }
  for (let y = Math.ceil(view.yMin / yStep) * yStep; y <= view.yMax; y += yStep) {
    const [, py] = toPx(0, y); ctx.moveTo(0, py); ctx.lineTo(W, py);
  }
  ctx.stroke();

  // 坐标轴
  ctx.strokeStyle = '#D1D1D6';
  ctx.lineWidth = 1.5;
  ctx.beginPath();
  if (view.yMin <= 0 && view.yMax >= 0) {
    const [, py] = toPx(0, 0); ctx.moveTo(0, py); ctx.lineTo(W, py);
  }
  if (view.xMin <= 0 && view.xMax >= 0) {
    const [px] = toPx(0, 0); ctx.moveTo(px, 0); ctx.lineTo(px, H);
  }
  ctx.stroke();

  // 刻度
  ctx.fillStyle = '#C7C7CC';
  ctx.font = '10px -apple-system, system-ui, sans-serif';
  ctx.textAlign = 'center'; ctx.textBaseline = 'top';
  for (let x = Math.ceil(view.xMin / xStep) * xStep; x <= view.xMax; x += xStep) {
    if (Math.abs(x) < 1e-9) continue;
    const [px, py] = toPx(x, 0);
    if (py < 0 || py > H - 12) ctx.fillText(formatTick(x), px, Math.min(Math.max(py + 3, 2), H - 12));
    else ctx.fillText(formatTick(x), px, py + 3);
  }
  ctx.textAlign = 'right'; ctx.textBaseline = 'middle';
  for (let y = Math.ceil(view.yMin / yStep) * yStep; y <= view.yMax; y += yStep) {
    if (Math.abs(y) < 1e-9) continue;
    const [px, py] = toPx(0, y);
    ctx.fillText(formatTick(y), Math.min(Math.max(px - 4, 22), W - 2), py);
  }

  // 曲线
  graphFunctions.forEach(fn => {
    const src = fn.expr.trim();
    if (fn.errorElement) fn.errorElement.textContent = '';
    if (!src) return;
    try {
      validateGraphExpression(src);
    } catch (error) {
      if (fn.errorElement) fn.errorElement.textContent = error.message || '表达式无效';
      return;
    }

    const N = Math.max(Math.round(W * 2), 800);
    ctx.strokeStyle = fn.color;
    ctx.lineWidth = 2.5;
    ctx.lineJoin = 'round';
    ctx.lineCap = 'round';
    ctx.beginPath();

    let started = false;
    let prevPy = null;
    let validPoints = 0;

    for (let i = 0; i <= N; i++) {
      const x = view.xMin + (view.xMax - view.xMin) * i / N;
      let y;
      try { y = evaluate(src, { x }, angleMode); }
      catch (e) { started = false; prevPy = null; continue; }
      if (!isFinite(y)) { started = false; prevPy = null; continue; }
      validPoints++;

      const [px, py] = toPx(x, y);

      if (!started) { ctx.moveTo(px, py); started = true; }
      else {
        if (prevPy !== null && Math.abs(py - prevPy) > H * 0.9) ctx.moveTo(px, py);
        else ctx.lineTo(px, py);
      }
      prevPy = py;
    }
    ctx.stroke();
    if (validPoints === 0 && fn.errorElement) {
      fn.errorElement.textContent = '当前范围内没有有效曲线点';
    }
  });
}

function formatTick(v) {
  if (Math.abs(v) >= 1000 || (Math.abs(v) < 0.01 && v !== 0)) return v.toExponential(1);
  return parseFloat(v.toFixed(4)).toString();
}

/* 交互 */
const pointers = new Map();
let lastPinchDist = null;

canvas.addEventListener('pointerdown', e => {
  canvas.setPointerCapture(e.pointerId);
  pointers.set(e.pointerId, { x: e.clientX, y: e.clientY });
});
canvas.addEventListener('pointermove', e => {
  if (!pointers.has(e.pointerId)) return;
  const prev = pointers.get(e.pointerId);
  const cur = { x: e.clientX, y: e.clientY };
  pointers.set(e.pointerId, cur);
  const rect = canvas.getBoundingClientRect();

  if (pointers.size === 1) {
    const dx = (cur.x - prev.x) / rect.width  * (view.xMax - view.xMin);
    const dy = (cur.y - prev.y) / rect.height * (view.yMax - view.yMin);
    view.xMin -= dx; view.xMax -= dx; view.yMin += dy; view.yMax += dy;
    drawGraph();
  } else if (pointers.size === 2) {
    const pts = Array.from(pointers.values());
    const dist = Math.hypot(pts[0].x - pts[1].x, pts[0].y - pts[1].y);
    if (lastPinchDist !== null && dist > 0) {
      const scale = lastPinchDist / dist;
      const cx = (pts[0].x + pts[1].x) / 2 - rect.left;
      const cy = (pts[0].y + pts[1].y) / 2 - rect.top;
      const u = cx / rect.width, v = cy / rect.height;
      const xc = view.xMin + u * (view.xMax - view.xMin);
      const yc = view.yMax - v * (view.yMax - view.yMin);
      view.xMin = xc + (view.xMin - xc) * scale;
      view.xMax = xc + (view.xMax - xc) * scale;
      view.yMin = yc + (view.yMin - yc) * scale;
      view.yMax = yc + (view.yMax - yc) * scale;
      drawGraph();
    }
    lastPinchDist = dist;
  }
});
function endPointer(e) {
  pointers.delete(e.pointerId);
  if (pointers.size < 2) lastPinchDist = null;
}
canvas.addEventListener('pointerup', endPointer);
canvas.addEventListener('pointercancel', endPointer);
canvas.addEventListener('pointerleave', endPointer);

function zoomView(factor) {
  const cx = (view.xMin + view.xMax) / 2, cy = (view.yMin + view.yMax) / 2;
  view.xMin = cx + (view.xMin - cx) * factor;
  view.xMax = cx + (view.xMax - cx) * factor;
  view.yMin = cy + (view.yMin - cy) * factor;
  view.yMax = cy + (view.yMax - cy) * factor;
  drawGraph();
}
document.getElementById('zoomIn').addEventListener('click', () => zoomView(0.75));
document.getElementById('zoomOut').addEventListener('click', () => zoomView(1.35));
document.getElementById('resetView').addEventListener('click', () => {
  view.xMin = -10; view.xMax = 10; view.yMin = -6; view.yMax = 6; drawGraph();
});

let graphTimer = null;
function debounceDraw() {
  clearTimeout(graphTimer);
  graphTimer = setTimeout(drawGraph, 120);
}
window.addEventListener('resize', () => { clearTimeout(graphTimer); graphTimer = setTimeout(drawGraph, 100); });
window.addEventListener('orientationchange', () => setTimeout(drawGraph, 250));

/* =========================================================
   6. 进制转换
   ========================================================= */
function parseBigInt(str, base) {
  if (!str) return null;
  let s = String(str).replace(/\s+/g, '').toLowerCase();
  if (!s) return null;
  let sign = 1n;
  if (s[0] === '-' || s[0] === '+') {
    if (s[0] === '-') sign = -1n;
    s = s.slice(1);
  }
  s = s.replace(/^0x/, '').replace(/^0o/, '').replace(/^0b/, '');
  if (!s) return null;
  let result = 0n;
  const b = BigInt(base);
  const digits = '0123456789abcdefghijklmnopqrstuvwxyz';
  for (const ch of s) {
    const d = digits.indexOf(ch);
    if (d < 0 || d >= base) return null;
    result = result * b + BigInt(d);
  }
  return sign * result;
}
function toBaseStr(value, base) {
  if (value === 0n) return '0';
  let neg = false;
  if (value < 0n) { neg = true; value = -value; }
  const digits = '0123456789ABCDEF';
  const b = BigInt(base);
  let out = ''; let n = value;
  while (n > 0n) { out = digits[Number(n % b)] + out; n = n / b; }
  return (neg ? '-' : '') + out;
}

function formatBaseValue(value, base) {
  const groupSize = base === 2 || base === 16 ? 4 : 3;
  const sign = value.startsWith('-') ? '-' : '';
  const digits = sign ? value.slice(1) : value;
  return sign + digits.replace(new RegExp(`\\B(?=(\\w{${groupSize}})+(?!\\w))`, 'g'), ' ');
}

function limitBaseInput(value, maxDigits) {
  const compact = value.replace(/\s/g, '');
  const signLength = /^[+-]/.test(compact) ? 1 : 0;
  const prefixLength = /^0[xob]/i.test(compact.slice(signLength)) ? 2 : 0;
  const allowedCharacters = signLength + prefixLength + maxDigits;
  if (compact.length <= allowedCharacters) return value;
  let characters = 0;
  for (let i = 0; i < value.length; i++) {
    if (/\s/.test(value[i])) continue;
    if (++characters > allowedCharacters) return value.slice(0, i);
  }
  return value;
}

const BASE_FIELDS = [
  { id: 'in-dec', base: 10 }, { id: 'in-hex', base: 16 },
  { id: 'in-oct', base: 8 }, { id: 'in-bin', base: 2 }
];
const MAX_BASE_DIGITS = 64;
const baseHint = document.querySelector('.base-hint');
function resizeBaseField(el) {
  el.style.height = 'auto';
  el.style.height = `${el.scrollHeight}px`;
}
let syncing = false;
BASE_FIELDS.forEach(({ id, base }) => {
  const el = document.getElementById(id);
  resizeBaseField(el);
  el.addEventListener('input', () => {
    if (syncing) return;
    syncing = true;
    const compact = el.value.replace(/\s/g, '');
    const signLength = /^[+-]/.test(compact) ? 1 : 0;
    const prefixLength = /^0[xob]/i.test(compact.slice(signLength)) ? 2 : 0;
    const digitCount = compact.length - signLength - prefixLength;
    if (digitCount > MAX_BASE_DIGITS) {
      el.value = limitBaseInput(el.value, MAX_BASE_DIGITS);
      baseHint.textContent = `最多输入 ${MAX_BASE_DIGITS} 位数字`;
      baseHint.classList.add('error');
    }
    const normalized = el.value.replace(/\s+/g, '');
    const isEmpty = !normalized;
    const v = parseBigInt(el.value, base);
    if (!isEmpty && v === null) {
      baseHint.textContent = `无效输入：${base === 2 ? '仅允许 0、1' : base === 8 ? '仅允许 0–7' : base === 10 ? '仅允许 0–9' : '仅允许 0–9、A–F'}，也可输入负数`;
      baseHint.classList.add('error');
    } else if (digitCount <= MAX_BASE_DIGITS) {
      baseHint.textContent = '输入上限 64 位；转换结果可更长，空格用于分组';
      baseHint.classList.remove('error');
    }
    resizeBaseField(el);
    if (v !== null) {
      BASE_FIELDS.forEach(f => {
        if (f.id === id) return;
        const target = document.getElementById(f.id);
        target.value = formatBaseValue(toBaseStr(v, f.base), f.base);
        resizeBaseField(target);
      });
    } else {
      BASE_FIELDS.forEach(f => {
        if (f.id !== id) {
          const target = document.getElementById(f.id);
          target.value = '';
          resizeBaseField(target);
        }
      });
    }
    syncing = false;
  });
});

/* =========================================================
   7. 初始化
   ========================================================= */
renderFnList();
renderGraphInputs();

window.addEventListener('load', () => {
  setTimeout(drawGraph, 200);
  if ('serviceWorker' in navigator) {
    navigator.serviceWorker.register('./service-worker.js')
      .catch(err => console.log('SW 注册失败：', err));
  }
});
