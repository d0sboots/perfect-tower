'use strict';
importScripts("fengari-web.js");

const L = fengari.L;
const lua = fengari.lua;
const interop = fengari.interop;

fengari.load("require('main')")();

function pushValue(val) {
  if (Array.isArray(val)) {
    lua.lua_createtable(L, val.length, 0);
    for (let i = 0; i < val.length; ++i) {
      pushValue(val[i]);
      lua.lua_rawseti(L, -2, i + 1);
    }
  } else {
    interop.push(L, val);
  }
}

function runLua(args) {
  const initial_top = lua.lua_gettop(L);
  lua.lua_getglobal(L, "lua_main");
  for (const arg of args) {
    pushValue(arg);
  }

  if (args.hasOwnProperty('scripts')) {
    let scripts = args.scripts;
    delete args.scripts;
    // importFunc, which we must create on this side, because it can't be
    // serialized. It has to be a Lua function, which means Lua semantics.
    lua.lua_pushcfunction(L, function(lua_state) {
      let filename = interop.tojs(lua_state, -1);
      for (let script_arr of scripts) {
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
  let res_array = [];
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

    let uArr = new Uint8Array(binStr.length);
    for (let i = 0; i < binStr.length; ++i) {
      let code = binStr.codePointAt(i);
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
      console.log("Text: " + textStr);
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

var pendingWork = null;

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
  } else if (e.data[0] !== "compile") {
    setTimeout(function() {
      let results = runLua(e.data);
      postMessage({args: e.data, results: results});
    });
  } else {
    // We drop multiple compile messages, because only the last one is
    // relevant.
    if (pendingWork === null) {
      setTimeout(function() {
        let results = runLua(pendingWork);
        postMessage({args: pendingWork, results: results});
        pendingWork = null;
      });
    }
    pendingWork = e.data;
  }
}

// Signal that we're done loading.
postMessage({args: ['ready'], results: [true, fengari.load('return FUNCTION_LIST')()]});
