'use strict';
importScripts("fengari-web.js");

const L = fengari.L;
const lua = fengari.lua;
const luaL = fengari.lauxlib;
const interop = fengari.interop;

// We need to set these functions *before* main is loaded, so that they can be
// used by main.
lua.lua_register(L, native_macros.name, native_macros);
fengari.load("require('main')")();

const UNCOMPRESSED_FORMAT = "v0";
const OLD_FORMAT = "v1";
const MODERN_FORMAT = "v2";

const PRIMITIVES = new Set(["string", "number", "boolean"]);
function pushValue(val) {
  if (Array.isArray(val)) {
    lua.lua_createtable(L, val.length, 0);
    for (let i = 0; i < val.length; ++i) {
      pushValue(val[i]);
      lua.lua_rawseti(L, -2, i + 1);
    }
  } else if (val == null || PRIMITIVES.has(typeof val)) {
    interop.push(L, val);
  } else {
    lua.lua_newtable(L);
    for (const [k, v] of Object.entries(val)) {
      pushValue(val[k]);
      lua.lua_setfield(L, -2, k);
    }
  }
}

function runLua(args) {
  const initial_top = lua.lua_gettop(L);
  lua.lua_getglobal(L, "lua_main");
  for (const arg of args) {
    pushValue(arg);
  }

  if (args.hasOwnProperty('scripts')) {
    const scripts = args.scripts;
    delete args.scripts;
    // importFunc, which we must create on this side, because it can't be
    // serialized. It has to be a Lua function, which means Lua semantics.
    lua.lua_pushcfunction(L, function(lua_state) {
      const filename = interop.tojs(lua_state, -1);
      for (const script_arr of scripts) {
        if (script_arr[0] === filename) {
          lua.lua_pushboolean(lua_state, true);
          lua.lua_pushstring(lua_state, script_arr[1]);
          return 2;
        }
      }
      lua.lua_pushboolean(lua_state, false);
      lua.lua_pushfstring(lua_state, 'Script "%s" does not exist!', filename);
      return 2;
    });
  }

  const nargs = lua.lua_gettop(L) - initial_top - 1;
  lua.lua_call(L, nargs, lua.LUA_MULTRET);
  const nresults = lua.lua_gettop(L) - initial_top;
  const res_array = [];
  for (let i = 0; i < nresults; i++) {
    res_array[i] = interop.tojs(L, initial_top + i + 1);
  }
  lua.lua_settop(L, initial_top);
  return res_array;
}

async function importData(importCode) {
  let startPos = 0;
  const results = [];
  let endPos;
  function throwErr(old_msg, new_msg) {
    const msg = `Trying old-style import: ${old_msg}\nTrying new-style import: ${new_msg}`;
    if (startPos === 0 && endPos === importCode.length) {
      // Don't confuse the issue when there's only one chunk
      throw new Error(msg);
    } else {
      throw new Error(`While processing chunk ${results.length + 1}, chars [${startPos},${endPos}): ${msg}`);
    }
  }
  do {
    endPos = importCode.indexOf(";", startPos);
    if (endPos < 0) {
      endPos = importCode.length;
    }

    // Try it as an old-format import code first. The Lua handles the base64
    // decoding, for legacy reasons.
    const chunk = importCode.slice(startPos, endPos);
    const chunkResult = runLua(["import", chunk]);
    if (chunkResult[0]) {
      results.push({name: chunkResult[1], code: chunkResult[2]});
      startPos = endPos + 1;
      continue;
    }

    // Otherwise, there are several possibilities. They all start with base64 decoding.
    let binStr;
    try {
      binStr = atob(chunk);
    } catch (ex) {
      // By this point, it *must* decode correctly.
      throwErr(chunkResult[1], ex.message);
    }

    const uArr = new Uint8Array(binStr.length);
    for (let i = 0; i < binStr.length; ++i) {
      const code = binStr.codePointAt(i);
      uArr[i] = code;
    }

    let textStr = null;
    try {
      textStr = new TextDecoder("utf-8", {fatal: true}).decode(uArr);
    } catch (ex) {
      if (!(ex instanceof TypeError)) {
        throwErr(chunkResult[1], ex.message);
      }
      // This indicates a likely old-style blueprint, but we don't handle those yet.
    }

    if (typeof DecompressionStream === "undefined") {
      throw new Error("Browser compatibility error: DecompressionStream not supported, can't decompress new import format!");
    }
    const stream = new DecompressionStream("deflate-raw");
    // Errors are thrown by both sides of the stream, so we swallow them on the writer side.
    const writer = stream.writable.getWriter();
    writer.write(uArr).catch(x=>0);
    writer.close().catch(x=>0);
    const reader = stream.readable.getReader();
    const decoder = new TextDecoder("utf-8", {fatal: true});
    textStr = "";

    try {
      for (let {value, done} = await reader.read(); !done; {value, done} = await reader.read()) {
        textStr += decoder.decode(value, {stream: true});
      }
      const parsedJson = JSON.parse(textStr);
      const len = parsedJson?.scripts?.length ?? 0;
      for (let i = 0; i < len; ++i) {
        const chunkResult = runLua(["import", parsedJson.scripts[i]]);
        if (chunkResult[0]) {
          results.push({name: chunkResult[1], code: chunkResult[2]});
        } else { // Re-thrown below
          throw new Error(`While importing script ${i+1} of ${len}: ${chunkResult[1]}`);
        }
      }
    } catch (ex) {
      throwErr(chunkResult[1], ex.message);
    }
    startPos = endPos + 1;
  } while (endPos < importCode.length);
  return results;
}

async function doCompile(data_orig) {
  const data = [...data_orig];
  data.scripts = data_orig.scripts;
  const isExport = data[0] === "workspace";
  const format = data_orig[2].format;
  data[0] = "compile";
  const results = runLua(data);
  if (!results[0]) {
    return results;
  }

  let impulses = 0, conditions = 0, actions = 0;
  const compiled = results[1];
  const json = {blueprints:[], scripts:[], style:[], windows:[]};

  for (let i = 1; compiled.has(i); ++i) {
    const scriptData = compiled.get(i);
    const type = scriptData.get("type");
    if (type === "script") {
      const imp = scriptData.get("impulses");
      const cond = scriptData.get("conditions");
      const act = scriptData.get("actions");
      if (imp === 0 && cond === 0 && act === 0 && isExport) {
        continue;
      }
      impulses = imp > impulses ? imp : impulses;
      conditions = cond > conditions ? cond : conditions;
      actions = act > actions ? act : actions;
      json.scripts.push(scriptData.get("code"));
    } else {
      if (isExport && format === UNCOMPRESSED_FORMAT) {
        continue;  // Oldest format can only handle scripts in bundles
      }
      if (type === "blueprint") {
        json.blueprints.push(scriptData.get("code"));
      } else if (type === "window") {
        json.windows.push(scriptData.get("code"));
      } else if (type === "tower") {
        if (json.style.length) {
          let firstName = "NOT FOUND";
          for (let j = 1; j < i; ++j) {
            const sd2 = compiled.get(j);
            if (sd2.get("type") === "tower") {
              firstName = sd2.get("name");
              break;
            }
          }
          throw new Error(`Can't export multiple tower designs at once! (${firstName} and ${scriptData.get("name")})`);
        }
        json.style.push(scriptData.get("code"));
      } else {
        throw new Error(`Unknown type "${type}" returned from compile() for ${scriptData.get("name")}`);
      }
    }
  }
  if (!json.blueprints.length) delete json.blueprints;
  if (!json.scripts.length) delete json.scripts;
  if (!json.style.length) delete json.style;
  if (!json.windows.length) delete json.windows;
  if (!Object.keys(json).length) {
    throw new Error("There are no scripts here, or they are all libraries (produce no code)");
  }
  let fullcode;
  if (format === UNCOMPRESSED_FORMAT) {
    switch (isExport ? "script" : compiled.get(1).get("type")) {
      case "blueprint":
        fullcode = json.blueprint[0];
        break;
      case "script":
        fullcode = json.scripts.join(";");
        break;
      case "tower":
        fullcode = json.style[0];
        break;
      case "window":
        fullcode = json.windows[0];
        break;
    }
  } else {
    if (typeof CompressionStream === "undefined") {
      throw new Error("Browser compatibility error: CompressionStream not supported, can't compress new export format!");
    }
    let jsoned;
    if (format == MODERN_FORMAT) {
      // Scripts is already encoded, we have to prevent double-encoding
      const saved = new Array(json.scripts.length);
      for (let i = 0; i < json.scripts.length; ++i) {
        saved[i] = json.scripts[i];
        // Using a special character so that it's basically impossible for it
        // to appear as part of the rest of the JSON (in a a name).
        json.scripts[i] = `\f${i}\f`;
      }
      jsoned = JSON.stringify(json);
      jsoned = jsoned.replaceAll(/"\\f[0-9]+\\f"/g, k => saved[k.slice(3, -3) | 0]);
    } else {
      jsoned = JSON.stringify(json);
    }
    const uArr = new TextEncoder().encode(jsoned);
    const stream = new CompressionStream("deflate-raw");
    // Errors are thrown by both sides of the stream, so we swallow them on the writer side.
    const writer = stream.writable.getWriter();
    writer.write(uArr).catch(x=>0);
    writer.close().catch(x=>0);
    const blobs = [];
    const reader = stream.readable.getReader();

    for (let {value, done} = await reader.read(); !done; {value, done} = await reader.read()) {
      blobs.push(value);
    }
    // This weird kludge is the fastest u8array -> base64 conversion for
    // medium-to-large data. For small data, there are faster ways, but this
    // will be good enough.
    const str = new FileReaderSync().readAsDataURL(new Blob(blobs));
    fullcode = str.slice(str.indexOf(',') + 1);
  }
  let header1 = "";
  if (isExport) {
    if (Object.hasOwn(json, "blueprints")) header1 += `${json.blueprints.length} blueprint(s), `;
    if (Object.hasOwn(json, "scripts")) header1 += `${json.scripts.length} script(s), `;
    if (Object.hasOwn(json, "style")) header1 += `${json.style.length} tower design, `;
    if (Object.hasOwn(json, "windows")) header1 += `${json.style.length} window(s), `;
    header1 += `${fullcode.length}b`;
    if (format !== UNCOMPRESSED_FORMAT) header1 += " (compressed)";
    header1 += "\n";
  }
  const name = isExport ? data_orig[2].name : compiled.get(1).get("name");
  const header = `${name}\n${header1}${impulses} ${conditions} ${actions}\n`
  return [results[0], header + fullcode, header.length]
}

var pendingWork = null;

function asyncCompile(work) {
  const finish = (res) => postMessage({args: work, results: res});
  return doCompile(work)
    .then(x => finish(x), rej => finish([false, String(rej)]));
}

function deferCompile() {
  // The bulk of the work happens synchronously, and then some trailing
  // compression stuff happens async. We don't know if that async work will
  // finish within this task, so we have to check to see if we need to start
  // another.
  setTimeout(function() {
    const work = pendingWork;
    asyncCompile(work).finally(() => {
      if (work === pendingWork) {
        // The current task is the one we just finished, we can shut down.
        pendingWork = null;
      } else {
        // Something new came in, start a new cycle.
        deferCompile();
      }
    });
  });
}

// Prepare this so we're ready immediately when the main thread asks
const ready_result = [true, fengari.load('return FUNCTION_LIST')()];

onmessage = function(e) {
  // Set a callback to do the actual work. This gives us breathing room to
  // process all pending messages first, before getting in to heavy
  // processing. It also means things will be processed in the order they are
  // recieved. (Compiles won't appear later and screw things up.)
  if (e.data[0] === "ready") {
    postMessage({args: e.data, results: ready_result});
  } else if (e.data[0] === "import") {
    // Importing uses promises, and therefore is JS native.
    importData(e.data[1])
      .then(x => postMessage({args: e.data, results: [true, x]}))
      .catch(x => postMessage({args: e.data, results: [false, String(x)]}));
  } else if (e.data[0] === "workspace") {
    setTimeout(function() {
      asyncCompile(e.data);
    });
  } else if (e.data[0] !== "compile") {
    setTimeout(function() {
      const results = runLua(e.data);
      postMessage({args: e.data, results: results});
    });
  } else {
    // Only have one deferred compilation running at a time.
    if (pendingWork === null) {
      deferCompile();
    }
    pendingWork = e.data;
  }
}

const braceRe = {
  "{(}": /([{(}])/g,
  "{}": /([{}])/g,
  "{(,)}": /([{(,)}])/g,
}

// JS Native implementation of macro parsing, for speed
function native_macros() {
  // This scope encloses the entirety of a compile operation, which can span
  // multiple imports.
  let line_number_start, line_number_end, compile_file;

  // Lua represents strings as Uint8Arrays. For our purposes these are UTF-8
  // data, but it is unnecessary to encode/decode to UTF-8 with all the
  // expensive error-checking that implies. Instead, we encode/decode directly
  // as one-byte-per-character, which perfectly preserves the information and
  // also keeps the ASCII range intact. This is all we need for doing regex
  // searches, and anything else (like UTF-8 sequences) are just opaque blobs
  // in the string that can be copied.
  function toluastr(str) {
    const data = new Uint8Array(str.length);
    for (let i = 0; i < str.length; ++i) {
      data[i] = str.charCodeAt(i);
    }
    return data;
  }
  function fromluastr(data) {
    let str = "";
    for (let i = 0; i < data.length; ++i) {
      str += String.fromCharCode(data[i]);
    }
    return str;
  }

  function error_lexer(msg) {
    let err_msg;
    if (line_number_start === line_number_end) {
      err_msg = `${compile_file}:${line_number_start}: ${msg}`;
    } else {
      err_msg = `${compile_file}:${line_number_start}-${line_number_end}: ${msg}`;
    }
    lua.lua_pushstring(L, toluastr(err_msg));
    return lua.lua_error(L);
  }

  function assert_parser(test, line, msg, ...mpos) {
    if (test) {
      return test;
    }

    let prev = 0;
    const markers = [];
    for (let i = 0; i < mpos.length; ++i) {
      const v = mpos[i];
      markers[i] = " ".repeat(v - prev) + "^";
      prev = v;
    }
    return error_lexer(`${msg}\n\n${line}\n${markers.join("")}`);
  }

  const macros = {
    // This entry is also used for {(} since that looks like an argument-macro
    // with no name, depending on how it is parsed. An expression like {{(}}
    // will parse one way for the inner macro and another (using the later
    // entry) for the outer macro, since the substituted paren doesn't act
    // like a delimiter and instead forms part of the name of a simple macro.
    "":  {args: [], raw: "{}", rawarg: true},
    "[": {args: [], raw: "{"},
    "]": {args: [], raw: "}"},
    "(": {args: [], raw: "("},
    ")": {args: [], raw: ")"},
    ",": {args: [], raw: ","},
    len: {args: ["#"], rawarg: true, func: arg_body => String(arg_body.length)},
    lua: {args: ["#"], rawarg: true, func: lua_text => {
      const lua_bytes = toluastr(lua_text);
      let result = luaL.luaL_loadbufferx(L, lua_bytes, null, lua_bytes, "t");
      if (result !== lua.LUA_OK) {
        const err = fromluastr(lua.lua_tostring(L, -1));
        lua.lua_pop(L, 1);
        error_lexer(err);
      }
      // We are ultimately executing in the lua context of a call to
      // get_line(). This has env as the first upvalue.
      lua.lua_pushvalue(L, lua.lua_upvalueindex(1));
      lua.lua_setupvalue(L, -2, 1);
      result = lua.lua_pcall(L, 0, 1, 0);
      const lua_value = lua.lua_tostring(L, -1);
      const value = lua_value ? fromluastr(lua_value) : "";
      lua.lua_pop(L, 1);
      if (result !== lua.LUA_OK) {
        error_lexer(value);
      }
      return value;
    }},
  };

  const handleOpenBrace = (pattern, macroLine, pos, result, depth, opts) => {
    let _, res, pChar;
    const re = braceRe[pattern];
    while (true) {
      re.lastIndex = pos;
      const found = re.exec(macroLine);
      if (found) {
        result += macroLine.slice(pos, found.index);
        pos = re.lastIndex;
      } else {
        result += macroLine.slice(pos);
        pos = macroLine.length;
        return [null, macroLine, pos, result];
      }
      if (found[1] !== "{") {
        return [found[1], macroLine, pos, result];
      }
      const orig_start = line_number_start;
      line_number_start = line_number_end;
      [macroLine, pos, result] = parseMacro(macroLine, pos, result, depth + 1, opts);
      line_number_start = orig_start;
    }
  }

  // Returns the tuple [macroLine, pos, output] containing the new line and parsing position.
  // The line is typically the same, but may have been advanced.
  function parseMacro(macroLine, pos, output, depth, opts) {
    assert_parser(depth < 100, macroLine, "macro expansion depth reached " + depth + ", probable infinite loop in:");
    let macroName;

    const evalMacro = (macro_obj, ...args) => {
      if (macro_obj.args.length !== args.length) {
        assert_parser(
          false,
          macroLine,
          `macro call {${macroName}} has wrong number of args, expected ${macro_obj.args.length} but got ${args.length}`,
          pos - 1);
      }
      if (macro_obj.raw != null) {
        output += macro_obj.raw;
      } else if (macro_obj.func != null) {
        output += macro_obj.func(...args);
      } else {
        const tmp_args = [];
        for (let i = 0; i < args.length; ++i) {
          tmp_args[macro_obj.args[i]] = {raw: args[i], args: []};
        }
        let posm = 0;
        let text = macro_obj.text;
        while (posm <= text.length) {
          const nposm = text.indexOf("{", posm);
          if (nposm < 0) {
            output += text.slice(posm);
            break;
          }
          output += text.slice(posm, nposm);
          const orig_start = line_number_start;
          line_number_start = line_number_end;
          [text, posm, output] = parseMacro(text, nposm + 1, output, depth + 1, {
            macros: opts.macros,
            arg_macros: tmp_args,
            get_input: () => null,
          });
          line_number_start = orig_start;
        }
      }
    }

    let pChar, result;
    [pChar, macroLine, pos, result] = handleOpenBrace("{(}", macroLine, pos, "", depth, opts);
    if (pChar == null) {
      // End of line. Unterminated macro is just returned as a literal text,
      // which is copied to the output buffer. We have to add the { that
      // *wasn't* included as part of our parsed text.
      output += "{";
      output += result;
      return [macroLine, macroLine.length, output];
    }
    // pChar is "}" or "(", either way we have the complete macro name.
    macroName = result;
    result = "";
    const macro_obj = opts.arg_macros[macroName] ?? opts.macros[macroName];
    assert_parser(opts.no_eval || macro_obj, macroLine, "macro does not exist: {" + macroName + "}", pos - 1);
    if (pChar === "}") {
      evalMacro(macro_obj);
      return [macroLine, pos, output];
    }
    // pChar is "(", we are parsing paramaters
    const args = [];
    let nesting = 1;
    while (true) {
      // The rawarg parsing mode does not count matching parens and always has
      // only a single arg. The same code handles both modes, we simply don't go
      // down the branches to handle parens by never matching those characters.
      [pChar, macroLine, pos, result] = handleOpenBrace(macro_obj.rawarg ? "{}" : "{(,)}", macroLine, pos, result, depth, opts);
      if (pChar == null) {
        // End of line. Get more input, since non-simple macros can span lines.
        if (result === "\n") {
          // If a new param (open paren or comma) is immediately followed by
          // newline, handleOpenBrace will only add that to result.
          // In this case, we want to swallow the initial newline.
          result = "";
        }
        const nextline = opts.get_input();
        assert_parser(nextline != null, macroLine, "unexpected EOF getting args for {" + macroName + "}", macroLine.length);
        macroLine = nextline;
        pos = 0;
      } else {
        if (pChar === "}") {
          if (!macro_obj.rawarg && nesting > 0) {
            assert_parser(
              false,
              macroLine,
              `${nesting} unclosed parenthesis inside macro {${macroName}}`,
              pos - 1);
          }
          // The empty macroName here implies the {(} macro, or at least the
          // beginning of it. It doesn't close in the usual way.
          if (macroName === "") {
            assert_parser(result === "", macroLine, "{(} macro has extra junk in it", pos - 2);
            evalMacro(opts.macros["("]);
          } else {
            if (macro_obj.rawarg) {
              if (macroLine[pos-2] !== ")") {
                // Not an error, for rawarg continue until we find ")}"
                result += "}";
                continue;
              }
              result = result.slice(0, -1);  // Trim the closing paren off, which got added in rawarg mode
            } else {
              assert_parser(macroLine[pos-2] === ")", macroLine, "trailing junk after macro call {" + macroName + "}", pos - 2);
            }
            args.push(result);
            evalMacro(macro_obj, ...args);
          }
          return [macroLine, pos, output];
        } else if (pChar === "(") {
          assert_parser(nesting > 0, macroLine, "tried to re-open macro args calling {" + macroName + "}", pos - 1);
          nesting += 1;
          result += "(";
        } else if (pChar === ",") {
          if (nesting === 1) {
            args.push(result);
            result = "";
          } else {
            result += ",";
          }
        } else if (pChar === ")") {
          assert_parser(nesting > 0, macroLine, "extra closing parens calling {" + macroName + "}", pos - 1);
          nesting -= 1;
          if (nesting > 0) {
            result += ")";
          }
          // We don't add the last paren to args here. Instead, we let the "}"
          // code handle that, allowing both the rawarg and regular code to
          // follow the same path for adding the final arg. This means that any
          // text that comes *after* this paren will get added to the last arg,
          // but that's an error we check for so it won't hurt us.
        } else {
          assert_parser(false, macroLine, "BUG_REPORT: unhandled case in parseMacro {" + macroName + "}", pos - 1);
        }
      }
    }
  }

  function native_create_get_line() {
    let input_split;
    // Locally scoped to this import, as opposed to line_number_end
    let lineno = 0;
    {
      compile_file = fromluastr(lua.lua_tostring(L, 1));
      const input = fromluastr(lua.lua_tostring(L, 2));

      input_split = input.split("\n");
    }

    // Handles stripping backslashes and tracking line-numbers
    function get_input_line() {
      if (lineno >= input_split.length) {
        return null;
      }
      const inp = input_split[lineno];
      lineno++;
      line_number_end = lineno;
      if (lineno == input_split.length) {
        // Last line, no possible terminator or continuation
        return inp;
      }
      if (inp[inp.length-1] !== "\\") {
        return inp + "\n";
      }
      return inp.slice(0, -1);
    }

    const parse_macro_opts = {
      macros: macros,
      arg_macros: {},
      get_input: get_input_line,
    };
    let in_macro_def = false;

    // Handles incremental macro expansion
    // There is feedback between this function and the next stage, via in_macro_def.
    // This is because this function parses macros, but the next stage handles
    // macro definitions. It *must* be arranged this way, because macro
    // definitions can be started from within (the expanded text of) a macro.
    // Why? Because I like making things hard for myself.
    // Since macros aren't parsed when defining a macro, this (earlier) stage
    // needs feedback from the later stage to know when it is or isn't
    // expanding macros. This function passes all the text needed to make that
    // determination (right up to the opening "{"), and then the next part
    // sets the flag appropriately so that parsing can proceed.
    const get_chunk = (() => {
      let pos, line;
      return function get_chunk_inner() {
        if (line == null || pos >= line.length) {
          pos = 0;
          line = get_input_line();
        }
        if (line == null) return [null, null];
        let npos = line.indexOf("{", pos + 1);
        if (npos < 0) {
          npos = line.length;
        }
        if (in_macro_def || line[pos] !== "{") {
          const ret = [line.slice(pos, npos), line_number_end];
          pos = npos;
          return ret;
        } else {
          let output = "";
          const orig_start = line_number_end;
          line_number_start = orig_start;
          [line, pos, output] = parseMacro(line, pos + 1, output, 1, parse_macro_opts);
          return [output, orig_start];
        }
      }
    })();

    const get_line = (() => {
      let line, start, pos;
      // Because the JS class for \s includes 0xa0, we cannot use it. Instead
      // we use [\t-\r ], which is all the normal whitespace characters.
      // If we need to exclude newline, we use [\t \v-\r].
      const re_nonspace = /[^\t \v-\r]/g;
      const re_macro = /^#([a-zA-Z_\x80-\xff][\w.\x80-\xff]*)(\([\w.\x80-\xff\t-\r ,]+\)|)([\t-\r ]|={)(.*)$/s;
      const re_arg = /^[\t-\r ]*([a-zA-Z_\x80-\xff][\w.\x80-\xff]*)[\t-\r ]*$/;
      return function get_line_inner() {
        let result;
        do {
          let match, next_start;
          in_macro_def = false;
          if (line == null) {
            [line, start] = get_chunk();
            next_start = start;
            pos = 0;
          }
          while (true) {
            if (line == null) {
              return [null, line_number_start, line_number_end];
            }
            re_nonspace.lastIndex = pos;
            match = re_nonspace.exec(line);
            if (match) {
              break;
            }
            [line, next_start] = get_chunk();
            pos = 0;
          }
          in_macro_def = (match[0] === "#");
          line = line.slice(match.index);
          pos = 0;
          let output = "";
          while (true) {
            const npos = line.indexOf("\n", pos);
            if (npos < 0) {
              output += line;
              [line, next_start] = get_chunk();
              if (line == null) {
                break;
              }
            } else {
              output += line.slice(pos, npos);
              pos = npos + 1;
              break;
            }
          }
          if (line && pos >= line.length) {
            line = null;
          }
          result = output;
          line_number_start = start;
          start = next_start;
          if (in_macro_def) {
            const match = re_macro.exec(result);
            assert_parser(match, result, "macro definition: #name <text> or #name(args...) <text>", 1);
            const [_, name, macro_args, macro_type] = match;
            let macro = macro_type == "={" ? match[4] : match[4].replace(/[\t-\r ]*$/, "");
            const args = [];
            let arg_begin = 1;
            while (arg_begin < macro_args.length - 1) {
              let pos = macro_args.indexOf(",", arg_begin);
              if (pos < 0) {
                pos = macro_args.length - 1;
              }
              const arg_string = macro_args.slice(arg_begin, pos);
              const match = re_arg.exec(arg_string);
              assert_parser(match, result, "bad macro function argument name: " + arg_string, name.length + 1 + arg_begin);
              if (args.includes(match[1])) {
                assert_parser(false, result, "duplicate function argument name: " + match[1], name.length + 1 + arg_begin)
              }
              args.push(match[1]);
              arg_begin = pos + 1;
            }
            assert_parser(!macros[name], result, "macro already exists: " + name, 1);
            macros[name] = {args: args, text: macro};
          }
        } while (in_macro_def);
        return [result.replace(/[\t-\r ]*$/, ""), line_number_start, line_number_end];
      }
    })();

    // Propogate the "env" upvalue.
    // We keep this in the closure for the lua macro to use.
    lua.lua_pushvalue(L, lua.lua_upvalueindex(1));
    lua.lua_pushcclosure(L, () => {
      try {
        const [line, start, end] = get_line();
        if (line == null) {
          lua.lua_pushnil(L);
        } else {
          lua.lua_pushstring(L, toluastr(line));
        }
        lua.lua_pushinteger(L, start);
        lua.lua_pushinteger(L, end);
        return 3;
      } catch (err) {
        if ("status" in err) {
          // This is a lua throw
          throw err;
        }
        console.error(err);
        lua.lua_pushstring(L, toluastr(err.toString() + "\n" + err.stack));
        return lua.lua_error(L);
      }
    }, 1);
    return 1;  // Returning the closure we just made
  } // native_create_get_line

  function set_native_compile_file() {
    compile_file = fromluastr(lua.lua_tostring(L, 1));
    return 0;
  }

  // The env for lua_load should be the only thing on the lua stack.
  // Stash it in this closure which we return.
  lua.lua_pushcclosure(L, native_create_get_line, 1);
  lua.lua_pushcfunction(L, set_native_compile_file);
  return 2;
} // native_macro
