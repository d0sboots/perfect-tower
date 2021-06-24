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

var pendingWork = null;

onmessage = function(e) {
  // Set a callback to do the actual work. This gives us breathing room to
  // process all pending messages first, before getting in to heavy
  // processing. It also means things will be processed in the order they are
  // recieved. (Compiles won't appear later and screw things up.)
  if (e.data[0] !== "compile") {
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
