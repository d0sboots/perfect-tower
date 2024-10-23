'use strict';
importScripts("fengari-web.js");

const L = fengari.L;
const lua = fengari.lua;
const interop = fengari.interop;

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

onmessage = function(e) {
  // Set a callback to do the actual work. This gives us breathing room to
  // process all pending messages first, before getting in to heavy
  // processing. It also means things will be processed in the order they are
  // recieved. (Compiles won't appear later and screw things up.)
  if (e.data[0] === "import") {
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

// Signal that we're done loading.
postMessage({args: ['ready'], results: [true, fengari.load('return FUNCTION_LIST')()]});
