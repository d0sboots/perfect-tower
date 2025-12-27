DEBUG = not fengari
package.path = "scripts/?.lua"
js = nil
if fengari then
  js = require "js"
end

local line_number
local compile_file
local _cache = {}

-- Defined here to pick up compile_file and line_number
function error_lexer(msg, _)
  error(string.format("%s:%s: %s", compile_file, line_number, msg), 0)
end

for _, lib in ipairs {"base64", "lexer-functions", "lexer-operators", "lexer-tokens", "lexer-debug", "lexer", "stdlib"} do
  if DEBUG then
    dofile(package.path:gsub("?", lib))
  else
    require(lib)
  end
end

do
  local assert_old = assert

  local function assert_lexer(test, msg)
    if not test then
      error_lexer(msg)
    end

    return test
  end

  function lua_main(func, arg1, arg2, arg3)
    local status, ret
    if func == "compile" then
      assert = assert_lexer
      local exported = {}
      for i = 1, #arg1 do
        local pair = arg1[i]
        status, ret = pcall(compile, pair.name, pair.text, arg2, arg3)
        if not status then
          assert = assert_old
          if js then
            -- Fengari has bugs where it can return a native JS exception object
            ret = js.tostring(ret)
          end
          return false, pair.name .. "\n" .. ret
        end
        exported[i] = ret
      end
      assert = assert_old
      return true, exported
    elseif func == "import" then
      status, ret = pcall(import, arg1)
      if status then
        return true, ret[1], ret[2]
      end
    elseif func == "unittest" then
      status, ret = pcall(unittest)
    else
      assert(false, "BUG REPORT: unknown lua_main function: " .. func)
    end
    return status, ret
  end
end

local function cache(line, variables)
  local key = {}

  for _, v in pairs (variables) do
    table.insert(key, string.format("%s.%s.%s.%s", v.scope, v.type, v.name, v.value))
  end

  table.sort(key)
  table.insert(key, line)
  key = table.concat(key, "¤")

  if not _cache[key] then
    _cache[key] = lexer(line, variables)
  end

  return _cache[key]
end

local function parseMacro(text, macros, depth, env)
  assert(depth < 100, "macro expansion depth reached " .. depth ..  ", probable infinite loop in: " .. text)
  return text:gsub("%b{}", function(macro)
    if #macro == 2 then
      return
    end
    if macro == "{[}" then
      return "{"
    end
    if macro == "{]}" then
      return "}"
    end
    macro = parseMacro(macro:sub(2,-2), macros, depth + 1, env)
    local arg_body = ""
    local name = macro:match("^([^%(]+)$")
    if not name then
      name, arg_body = macro:match("^([^%(]+)(%b())")
    end
    assert(name, "invalid macro call: " .. macro)
    assert(#name + #arg_body == #macro, "trailing junk after macro call: " .. macro)
    if name == "len" then
      assert(arg_body ~= "", "len is a macro function")
      return tostring(#arg_body - 2)
    elseif name == "lua" then
      assert(arg_body ~= "", "lua is a macro function")
      local lua_text = arg_body:sub(2,-2)
      local chunk, err = load(lua_text, lua_text, "t", env)
      assert(chunk, err)
      local status, result = pcall(chunk)
      if status then
        return tostring(result or "")
      end
      error_lexer(result)
    end
    local args = {}
    local arg_count = 0
    local arg_begin = 2
    local nesting = 0
    for i = 2, #arg_body do
      local char = arg_body:sub(i, i)
      if nesting == 0 and (char == "," or i == #arg_body) then
        args["{#"..arg_count.."#}"] = arg_body:sub(arg_begin, i-1)
        arg_count = arg_count + 1
        arg_begin = i + 1
      elseif char == "(" then
        nesting = nesting + 1
      elseif char == ")" and i ~= #arg_body then
        nesting = nesting - 1
        assert(nesting >= 0, "unbalanced parenthesis inside macro call: " .. macro)
      end
    end
    assert(nesting == 0, "unclosed parenthesis inside macro call: " .. macro)
    local macro_obj = macros[name]
    assert(macro_obj, "macro does not exist: " .. name)
    local unexpanded, arg_len = macro_obj.text, macro_obj.arg_len
    assert(arg_len == arg_count,
    "macro call has wrong number of args, expected " .. arg_len ..
    " but got " .. arg_count .. ": " .. macro)
    return parseMacro(unexpanded:gsub("{#[0-9]+#}", args), macros, depth + 1, env)
  end)
end

-- Whitelist of functions and tables that are allowed in the lua() macro. We
-- store this as a string, so that we can bind the names properly.
local globals_whitelist = {}
for k in string.gmatch([[assert
error
getmetatable
ipairs
load
next
pairs
pcall
print
rawequal
rawget
rawlen
rawset
select
setmetatable
tonumber
tostring
type
_VERSION
xpcall
coroutine
string
utf8
table
math]], "%g+") do globals_whitelist[#globals_whitelist+1] = k end

local os_whitelist = {'clock', 'date', 'difftime', 'time'}

local function filter_table(table_in, whitelist)
  res = {}
  for i = 1, #whitelist do
    local k = whitelist[i]
    local v = table_in[k]
    if type(v) == 'table' then
      local new_tab = {}
      for k2, v2 in pairs(v) do
        new_tab[k2] = v2
      end
      v = new_tab
    end
    res[k] = v
  end
  return res
end

local function clone_global()
  local new_g = filter_table(_G, globals_whitelist)
  new_g.os = filter_table(_G.os, os_whitelist)
  new_g._G = new_g
  return new_g
end

local function is_empty(line)
  return line:find("^%s*$") or line:find("^%s*;.*$")
end

local json_escape_table = {["\\"]=[[\\]], ['"']=[[\"]]}
for i = 0, 31 do
  json_escape_table[string.char(i)] = string.format([[\u%04x]], i)
end
json_escape_table["\b"] = [[\b]]
json_escape_table["\f"] = [[\f]]
json_escape_table["\n"] = [[\n]]
json_escape_table["\r"] = [[\r]]
json_escape_table["\t"] = [[\t]]

local line_encode_table = {}
local utf_decode_table = {}
for k, v in pairs(json_escape_table) do
  line_encode_table[k] = v
end
for i = 0x80, 0xff do
  line_encode_table[string.char(i)] = utf8.char(i)
  utf_decode_table[utf8.char(i)] = string.char(i)
end

-- importFunc takes a string (filename) and returns a pair of (status, string)
-- (the content of the imported file on success, an error message on failure).
function compile(name, input, options, importFunc)
  local variables, impulses, conditions, actions = {}, {}, {}, {}
  local budget, use_budget
  local env = clone_global()
  local macros, imported = {len = '__builtin__', lua = '__builtin__'}, {}
  local ret = {}

  local function import(filename, input, isImport)
    if imported[filename] then
      return {}
    end
    imported[filename] = true
    line_number = 0
    compile_file = filename

    local lines = {}
    local labelCache = {}
    local lineCache = {}

    for real_line in input:gmatch"[^\n]*" do
      line_number = line_number + 1
      if real_line:sub(-1) == "\\" then
        lineCache[#lineCache+1] = real_line:sub(1, -2)
        goto continue
      end
      lineCache[#lineCache+1] = real_line
      line = table.concat(lineCache):gsub("^%s+", ""):gsub("%s+$", "")
      if line:sub(1, 1) ~= "#" then
        line = parseMacro(line, macros, 1, env)
      end
      lineCache = {}

      if line:match"^#" then
        local macro_args = ""
        local name, macro = line:sub(2):match(TOKEN.identifier.pattern .. " (.+)$")
        if not name then
          name, macro_args, macro = line:sub(2):match(TOKEN.identifier.pattern .. "(%([%w%._ ,]+%)) (.+)$")
        end
        assert(name, "macro definition: #name <text> or #name(args...) <text>")
        local args = {}
        local arg_len = 0
        local arg_begin = 2
        while arg_begin <= #macro_args do
          local pos = macro_args:find(",", arg_begin, true)
          if not pos then
            pos = #macro_args
          end
          local arg_string = macro_args:sub(arg_begin, pos - 1)
          local arg = arg_string:match("^ *" .. TOKEN.identifier.patternAnywhere .. " *$")
          assert(arg, "bad macro function argument name: " .. arg_string)
          args["{" .. arg .. "}"] = "{#" .. arg_len .. "#}"
          arg_len = arg_len + 1
          arg_begin = pos + 1
        end
        assert(not macros[name], "macro already exists: " .. name)
        macros[name] = {text = macro:gsub("{[^{}]+}", args), arg_len = arg_len}
      elseif line:match"^:" then
        local token = line:match("^:" .. TOKEN.identifier.patternAnywhere)
        if token == "const" then
          local _, type, name, value = line:sub(2):match("^(%a+) (%a+) " .. TOKEN.identifier.patternAnywhere .. " (.+)$")
          assert(type, "constant definition: const [int/double/string/bool/vector] name value")
          assert(({bool=true, int=true, double=true, string=true, vector=true})[type], "constant types are 'int', 'double', 'string', 'bool', and 'vector'")
          if (type == "int" or type == "double") then
            local x = tonumber(value)
            assert(x, "Can't convert '" .. value .. "' to a number")
            local vtype = math.type(x) == "integer" and "int" or "double"
            assert(vtype == type, "bad argument, " .. type .. " expected, got " .. vtype .. " " .. value)
            value = x
          elseif (type == "bool") then
            assert(value:match"^true$" or value:match"^false$", "bool values are 'true' or 'false'")
            if value:match"^true$" then
              value = true
            else
              value = false
            end
          elseif (type == "string") then
            value = value:match"^%b''$" or value:match'^%b""$'
            assert(value, "strings must be enclosed in either single quotes or double quotes")
            value = value:sub(2,-2)
          elseif type == "vector" then
            local matches = table.pack(value:match"^vec%((.+),(.+)%)$")
            assert(matches.n > 1, "vector constants must use vec(x, y) syntax")
            local x = tonumber(matches[1])
            assert(x, "Can't convert '" .. matches[1] .. "' to a number")
            local y = tonumber(matches[2])
            assert(y, "Can't convert '" .. matches[2] .. "' to a number")
            value = {x = x, y = y}
          end
          assert(not variables[name], "variable/label/constant already exists: " .. name)
          variables[name] = {name = name, scope = "constant", type = type, value = value}
        elseif token == "import" then
          local import_name = line:match("^:%a+ +(.+)")
          assert(import_name, "import directive: :import file")
          local status, import_result = importFunc(import_name)
          assert(status, "Import failed: " .. import_result)
          local saved_lineno, saved_compile_file = line_number, compile_file
          import(import_name, import_result, true)
          -- These got stomped by the import, re-set them
          compile_file = saved_compile_file
          line_number = saved_lineno
        elseif token == "global" or token == "local" then
          local scope, type, name = line:sub(2):gsub(" *;.*", ""):match("^(%a+) +(%a+) +" .. TOKEN.identifier.patternAnywhere .."$")
          assert(scope, "variable definition: [global/local] [bool/int/double/string/vector] name")

          assert(({bool=true, int=true, double=true, string=true, vector=true})[type], "variable types are 'bool', 'int', 'double', 'string', and 'vector'")
          assert(not variables[name], "variable/label already exists: " .. name)
          variables[name] = {name = name, scope = scope, type = type}
        elseif token == "name" then
          local name = line:match("^:%a+ +(.+)")
          assert(name, "name directive: :name script_name")
          compile_file = name
        elseif token == "budget_cap" then
          local cost = line:match("^:budget_cap +(.+)")
          if cost and cost ~= "max" then
            cost = tonumber(cost)
            if cost then
              assert(-2147483648 <= cost and cost < 2147483648,
                "budget_cap must fit in an integer (-2^31 <= cost < 2^31), got " .. cost)
            end
          end
          assert(cost, "budget_cap directive: :budget_cap [max/<integer>]")
          budget = cost
        elseif token == "use_budget" then
          use_budget = line:match("^:use_budget +(.+)")
          assert(use_budget == "true" or use_budget == "false" or use_budget == "default",
            "use_budget directive: :use_budget [true/false/default]")
        else
          assert(false, "Unrecognized directive :" .. token)
        end
      else
        line = line
        :gsub(TOKEN.identifier.pattern .. ":", function(name)
          assert(not variables[name] or labelCache[name], "variable/label already exists: " .. name)
          variables[name] = {name = name, scope = "local", type = "int", label = 0}
          table.insert(labelCache, name)
          return ""
        end)
        :gsub("^%s+", ""):gsub("%s+$", "")

        if not is_empty(line) then
          assert(not isImport,
          "Imported files can't produce output, they must only contain variable and macro declarations")
          table.insert(lines, {text = line, num = line_number, label = labelCache})
          labelCache = {}
        end
      end
      ::continue::
    end
    -- anything left in the label cache points to the end of the script
    for _, label in ipairs (labelCache) do
      variables[label].label = 99
    end

    return lines
  end

  import("__stdlib__", STDLIB, true)
  local lines = import(name, input, false)

  for _, line in ipairs (lines) do
    line_number = line.num
    local node = cache(line.text, variables)

    if node and node.func then
      if node.func.ret == "void" then
        table.insert(actions, node)

        if #(line.label) > 0 then
          for _, label in ipairs (line.label) do
            variables[label].label = #actions
          end
        end
      else
        assert(#(line.label) == 0, "labels cannot be placed before impulses/conditions")

        if node.func.ret == "impulse" then
          table.insert(impulses, node)
        else
          table.insert(conditions, node)
        end
      end
    end
  end

  local function ins(frmt, val)
    ret[#ret+1] = string.pack(frmt, val)
  end

  local function prefix_code(size)
    while size >= 0x80 do
      ret[#ret+1] = string.pack("B", 0x80 + (size & 0x7F))
      size = size >> 7
    end
    ret[#ret+1] = string.pack("B", size)
  end

  local function encode(node)
    if node.func then
      if node.func.name == "label" then
        local var = node.args[1].value
        assert(variables[var], "why are you calling the label function manually?")
        encode{type = "number", value = variables[var].label}
        return
      end

      prefix_code(#node.func.name)
      ret[#ret+1] = node.func.name;

      for _, arg in ipairs (node.args) do
        encode(arg)
      end
    else
      ins("s1", "constant")

      if node.type == "bool" then
        ins("b", 1)
        ins("b", node.value and 1 or 0)
      elseif node.type == "number" then
        if math.type(node.value) == "integer" then
          ins("b", 2)
          ins("i4", node.value)
        else
          ins("b", 3)
          ins("d", node.value)
        end
      elseif node.type == "string" then
        ins("b", 4)
        prefix_code(#node.value);
        ret[#ret+1] = node.value
      elseif node.type == "vector" then
        ins("b", 5)
        ins("f", node.value.x)
        ins("f", node.value.y)
      elseif node.type == "operator" then
        ins("b", 4)

        if node.value == "%" then
          node.value = "mod"
        elseif node.value == "^" then
          node.value = "pow"
        elseif node.value == "//" then
          node.value = "log"
        elseif node.value == "%&" then
          node.value = "and"
        elseif node.value == "%^" then
          node.value = "xor"
        elseif node.value == "%|" then
          node.value = "or"
        end

        ins("s1", node.value)
      else
        assert(false, "BUG REPORT: unknown compile type: " .. node.type)
      end
    end
  end

  local gsub = string.gsub
  local function json_encode(node)
    local current_pos = #ret
    encode(node)
    for i = current_pos+1, #ret do
      -- Need to JSON-escape control characters, backslash and double-quote,
      -- and also UTF-8 encode high-byte chars
      ret[i] = gsub(ret[i], '[\0-\x1f\\"\x80-\xff]', line_encode_table)
    end
  end

  local function json_escape(str)
    ret[#ret+1] = gsub(str, '[\0-\x1f\\"]', json_escape_table)
  end

  local package_name, script_name = compile_file:match("([^:]*):(.*)")
  if not script_name then
    script_name = compile_file
  end

  if options.format ~= "v2" then
    prefix_code(#compile_file)
    ret[#ret+1] = compile_file

    for _, tbl in ipairs {impulses, conditions, actions} do
      ins("i4", #tbl)

      for _, line in ipairs (tbl) do
        encode(line)
      end
    end

    ret = base64.encode(table.concat(ret))
  else
    for num, pair in ipairs({
      {name="actions", tbl=actions},
      {name="conditions", tbl=conditions},
      {name="impulses", tbl=impulses}
    }) do
      ret[#ret+1] = num == 1 and [[{"]] or [[],"]]
      ret[#ret+1] = pair.name
      ret[#ret+1] = [[":[]]
      for num, line in ipairs(pair.tbl) do
        if num ~= 1 then
          ret[#ret+1] = [[,]]
        end
        ret[#ret+1] = [["]]
        if pair.name == "impulses" then
          ret[#ret+1] = line.func.name  -- Impulses are no-arg functions with straight ASCII names
        else
          json_encode(line)
        end
        ret[#ret+1] = [["]]
      end
    end
    ret[#ret+1] = [[],"name":"]]
    json_escape(script_name)
    ret[#ret+1] = [[","package":"]]
    json_escape(package_name or "")
    ret[#ret+1] = [["]]
    if budget then
      ret[#ret+1] = [[,"budget":]]
      -- No quotes because budget is a number in the JSON
      ret[#ret+1] = budget == "max" and "-1" or string.format("%d", budget)
    end
    if use_budget ~= "default" and (use_budget or budget) then
      ret[#ret+1] = [[,"useBudget":]]
      -- No quotes because useBudget is a boolean in the JSON
      ret[#ret+1] = use_budget or "true"
    end
    ret[#ret+1] = [[}]]

    ret = table.concat(ret)
  end

  package_name = package_name and package_name:sub(1, 24) .. ":" or ""
  script_name = script_name:sub(1, 24)
  return {
    name = package_name .. script_name,
    type = "script",
    impulses = #impulses,
    conditions = #conditions,
    actions = #actions,
    code = ret,
  }
end

function import(input)
  local data
  local pos = 1

  local variables = {}
  local num_vars = 0
  local ret = {}

  local function read(frmt)
    local ret, new = string.unpack(frmt, data, pos)
    pos = new
    return ret
  end

  local function stripParens(text)
    text = tostring(text)
    if text:byte() == 40 and text:byte(-1) == 41 then  -- "(" + ")"
      text = text:sub(2, -2)
    end
    return text
  end

  local function parse()
    local func = read"s1"

    if func == "constant" then
      local type = read"b"

      if type == 1 then
        return read"b" == 1 and "true" or "false"
      elseif type == 2 then
        return tostring(read"i4")
      elseif type == 3 then
        local val = read"d"
        if val == 1/0 then return "(1./0.)" end
        if val == -1/0 then return "(-1./0.)" end
        if val ~= val then return "(0./0.)" end
        return tostring(val)
      elseif type == 4 then
        local pos, len = 0, 0

        repeat
          local byte = read"B"
          len = len + ((byte & 0x7F) << 7*pos)
          pos = pos + 1
        until byte & 0x80 == 0

        local str = read("c" .. len)
        -- This must be a single gsub so that we don't re-match substituted curlies
        str = str:gsub("[{}]", {["{"] = "{[}", ["}"] = "{]}"})
        local sq, dq = str:match"'", str:match'"'

        if not dq then
          return '"' .. str .. '"'
        elseif dq and not sq then
          return "'" .. str .. "'"
        end

        str = string.format('"%s"', str:gsub('"', [[" . '"' . "]])):gsub('"" %.', ""):gsub('%. ""', "")
        return str
      elseif type == 5 then
        return string.format("vec(%s, %s)", read"f", read"f")
      else
        assert(false, "BUG REPORT: unknown constant type: " .. type)
      end
    else
      local func = assert(FUNCTION[func], "BUG REPORT: unknown function: " .. func)
      local args = {}
      local dynamicOperator = false

      for i, arg in ipairs (func.args) do
        args[#args+1] = parse()

        if arg.type:match"^op_" then
          dynamicOperator = true
          if args[i]:byte() == 34 then  -- '"'
            local transformed = args[i]:sub(2, -2)
            :gsub("^=$", "==")
            :gsub("mod", "%%")
            :gsub("pow", "^")
            :gsub("log", "//")
            :gsub("and", "%%&")
            :gsub("xor", "%%^")
            :gsub("or",  "%%|")

            if OPERATOR[transformed] then
              args[i] = transformed
              dynamicOperator = false
            end
          end
        end
      end

      local scope, type, func_name = func.name:match"(%a+)%.(%w+)%.(%a+)"

      if (scope == "global" or scope == "local") and args[1]:byte() == 34 then
        local var = args[1]:sub(2,-2)

        if var == var:match(TOKEN.identifier.pattern) then
          if type == "vec2" then
            type = "vector"
          end
          if not variables[var] then
            local key = string.format(":%s %s %s", scope, type, var)
            variables[var] = key
            num_vars = num_vars + 1
          end

          return func_name == "set" and string.format("%s = %s", var, stripParens(args[2])) or var
        end
      elseif not dynamicOperator and (func.name:sub(1,10) == "arithmetic" or func.name:sub(1,10) == "comparison") then
        return string.format("(%s)", table.concat(args, " "))
      elseif func.name == "concat" then
        return string.format("(%s . %s)", table.unpack(args))
      end

      local type, func_name = func.name:match"(%a+)%.(%a+)"; -- rnd, min, max
      local func_short = func.short;

      if func.name:match"^ternary" then
        func_short = "if"
      elseif (type == "int" or type == "double") and (func_name == "rnd" or func_name == "min" or func_name == "max") then
        func_short = func_name
      end

      for k, v in ipairs (args) do
        args[k] = stripParens(v)
      end

      return string.format("%s(%s)", func_short, table.concat(args, ", "))
    end
  end

  local function ins(val)
    local text = stripParens(val)
    ret[#ret+1] = text
  end

  local function oldFormat()
    data = base64.decode(input)
    local name = read"s1"

    for i = 1, 3 do
      local sz = read"I4"
      assert(sz >= 0, string.format("Bad import: Section %d had negative count %d", i, sz))
      for j = 1, sz do
        ins(parse())
      end

      ins""
    end

    assert(pos - 1 == #data, string.format("Bad import: Extra characters after parsing from %d-%d (%s)", pos - 1, #data, string.sub(data, pos)))

    table.insert(ret, 1, "")

    for _, var in pairs (variables) do
      table.insert(ret, 1, var)
    end

    return name
  end

  local function modernFormat()
    local script_name = input.name
    local package_name = input.package
    -- Split packages make our life harder. We need to prefix the script with ":" if it contains a
    -- ":" but otherwise has no package. Note that if the *package* contains a ":", this will be
    -- lost in translation.
    local name = (package_name and package_name ~= "") and
      (package_name .. ":" .. script_name) or
      (script_name:find(":", 1, true) and ":" or "") .. script_name

    ret[1] = ":name " .. name
    -- This complicated set of conditions supports the following goals:
    -- If useBudget is false or missing *and* budget is 0 or missing, don't include any directives.
    --   (This is the default/compatibility case for old scripts.)
    --   The export will only match the import if both directives were missing, but this is deemed
    --   acceptable, since we don't want to clutter scripts up with irrelevant junk for this common case.
    --   The default values are false/0, so functionally it comes out the same.
    -- If useBudget is true, we must include something. Generally it is the budget cap, but in the special case
    --   of a missing budget, we set :use_budget instead so the export matches the import.
    -- If budget is positive, we set the directives so the export will match the import.
    if input.budget and (input.budget ~= 0 or input.useBudget) then
      ret[#ret+1] = ":budget_cap " .. (input.budget == -1 and "max" or string.format("%d", input.budget))
    end
    if not input.useBudget and input.budget and input.budget ~= 0 then
      ret[#ret+1] = ":use_budget " .. (input.useBudget == nil and "default" or "false")
    end
    if input.useBudget and input.budget == nil then
      ret[#ret+1] = ":use_budget true"
    end
    ins""
    local variable_slot = #ret

    for i, tbl in ipairs({input.impulses, input.conditions, input.actions}) do
      for _, line in ipairs(tbl) do
        if i == 1 then
          ins(line .. "()")  -- Impulses are straight ASCII
        else
          -- High-bit chars are UTF8-encoded, have to undo that
          data = line:gsub("[\xC2\xC3].", utf_decode_table)
          pos = 1
          ins(parse())
          if pos - 1 ~= #data then
            error(string.format("Bad import: Extra characters after parsing from %d-%d (%s)", pos - 1, #data, string.sub(data, pos)))
          end
        end
      end

      ins""
    end

    local shift_amount = num_vars + 1
    for i = #ret, variable_slot, -1 do
      ret[i + shift_amount] = ret[i]
    end
    ret[variable_slot] = ""

    for _, var in pairs (variables) do
      variable_slot = variable_slot + 1
      ret[variable_slot] = var
    end

    return name
  end

  local name = type(input) == "string" and oldFormat() or modernFormat()
  ret = table.concat(ret, "\n"):gsub("\n\n+", "\n\n"):gsub("^\n+", ""):gsub("\n+$", "")
  return {name, ret}
end

function unittest()
  local import_tests = {
"C2dsb2JhbF90aWVyAQAAAAVrZXkuMQAAAAABAAAADmdsb2JhbC5pbnQuc2V0CGNvbnN0YW50BAR0aWVyDmFyaXRobWV0aWMuaW50DmFyaXRobWV0aWMuaW50Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BANtb2QIY29uc3RhbnQCCgAAAAhjb25zdGFudAQBKwhjb25zdGFudAIBAAAA",
"EEdMT0JBTF9DT1VOVGRvd24BAAAABWtleS4yAAAAAAYAAAAOZ2VuZXJpYy5nb3RvaWYIY29uc3RhbnQCAwAAABFjb21wYXJpc29uLmRvdWJsZRFnbG9iYWwuZG91YmxlLmdldAhjb25zdGFudAQFY291bnQIY29uc3RhbnQEATwIY29uc3RhbnQDAAAAAAAA8D8QbG9jYWwuZG91YmxlLnNldAhjb25zdGFudAQDcG93DGRvdWJsZS5mbG9vchFhcml0aG1ldGljLmRvdWJsZQhjb25zdGFudAN7FK5H4XqEvwhjb25zdGFudAQBKxFhcml0aG1ldGljLmRvdWJsZRFnbG9iYWwuZG91YmxlLmdldAhjb25zdGFudAQFY291bnQIY29uc3RhbnQEA2xvZwhjb25zdGFudAMAAAAAAAAkQA1sb2NhbC5pbnQuc2V0CGNvbnN0YW50BANpbmMOYXJpdGhtZXRpYy5pbnQIY29uc3RhbnQCCgAAAAhjb25zdGFudAQDcG93A2QyaRBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BANwb3cQbG9jYWwuZG91YmxlLnNldAhjb25zdGFudAQDdG1wEWFyaXRobWV0aWMuZG91YmxlEWdsb2JhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAVjb3VudAhjb25zdGFudAQBLQNpMmQNbG9jYWwuaW50LmdldAhjb25zdGFudAQDaW5jDmdlbmVyaWMuZ290b2lmCGNvbnN0YW50AmMAAAARY29tcGFyaXNvbi5kb3VibGUQbG9jYWwuZG91YmxlLmdldAhjb25zdGFudAQDdG1wCGNvbnN0YW50BAE8CGNvbnN0YW50AwAAAAAAAPA/EWdsb2JhbC5kb3VibGUuc2V0CGNvbnN0YW50BAVjb3VudBBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAN0bXA=",
"DkdMT0JBTF9DT1VOVFVQAQAAAAVrZXkuMwAAAAAGAAAADmdlbmVyaWMuZ290b2lmCGNvbnN0YW50AmMAAAARY29tcGFyaXNvbi5kb3VibGURZ2xvYmFsLmRvdWJsZS5nZXQIY29uc3RhbnQEBWNvdW50CGNvbnN0YW50BAE+CGNvbnN0YW50AwAAAACIKmFBDmdlbmVyaWMuZ290b2lmCGNvbnN0YW50AgQAAAARY29tcGFyaXNvbi5kb3VibGURZ2xvYmFsLmRvdWJsZS5nZXQIY29uc3RhbnQEBWNvdW50CGNvbnN0YW50BAE8CGNvbnN0YW50AwAAAAAAAPA/EGxvY2FsLmRvdWJsZS5zZXQIY29uc3RhbnQEA3Bvdwxkb3VibGUuZmxvb3IRYXJpdGhtZXRpYy5kb3VibGUIY29uc3RhbnQDexSuR+F6hD8IY29uc3RhbnQEASsRYXJpdGhtZXRpYy5kb3VibGURZ2xvYmFsLmRvdWJsZS5nZXQIY29uc3RhbnQEBWNvdW50CGNvbnN0YW50BANsb2cIY29uc3RhbnQDAAAAAAAAJEANbG9jYWwuaW50LnNldAhjb25zdGFudAQDaW5jDmFyaXRobWV0aWMuaW50CGNvbnN0YW50AgoAAAAIY29uc3RhbnQEA3BvdwNkMmkQbG9jYWwuZG91YmxlLmdldAhjb25zdGFudAQDcG93EGxvY2FsLmRvdWJsZS5zZXQIY29uc3RhbnQEA3RtcBFhcml0aG1ldGljLmRvdWJsZRFnbG9iYWwuZG91YmxlLmdldAhjb25zdGFudAQFY291bnQIY29uc3RhbnQEASsDaTJkDWxvY2FsLmludC5nZXQIY29uc3RhbnQEA2luYxFnbG9iYWwuZG91YmxlLnNldAhjb25zdGFudAQFY291bnQQbG9jYWwuZG91YmxlLmdldAhjb25zdGFudAQDdG1w",
"E2dsb2JhbF9jaG9vc2VfY3JhZnQBAAAABWtleS40AAAAAAEAAAAOZ2xvYmFsLmludC5zZXQIY29uc3RhbnQEBk9VVFBVVA5hcml0aG1ldGljLmludA5hcml0aG1ldGljLmludA5nbG9iYWwuaW50LmdldAhjb25zdGFudAQGT1VUUFVUCGNvbnN0YW50BANtb2QIY29uc3RhbnQCBQAAAAhjb25zdGFudAQBKwhjb25zdGFudAIBAAAA",
"D0xPQ19TRVRfRkFDVE9SWQEAAAAMb3Blbi5mYWN0b3J5AAAAAAEAAAAOZ2xvYmFsLmludC5zZXQIY29uc3RhbnQECGxvY2F0aW9uCGNvbnN0YW50AgMAAAA=",
"DWZhY3RvcnlfY3JhZnQBAAAABWtleS4wAQAAAA9jb21wYXJpc29uLmJvb2wOY29tcGFyaXNvbi5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQECGxvY2F0aW9uCGNvbnN0YW50BAI9PQhjb25zdGFudAIDAAAACGNvbnN0YW50BAImJg5jb21wYXJpc29uLmludA5nbG9iYWwuaW50LmdldAhjb25zdGFudAQIY3JhZnRpbmcIY29uc3RhbnQEAj09CGNvbnN0YW50AgAAAAADAAAADmdsb2JhbC5pbnQuc2V0CGNvbnN0YW50BAhjcmFmdGluZwhjb25zdGFudAIBAAAAE2dlbmVyaWMuZXhlY3V0ZXN5bmMGY29uY2F0CGNvbnN0YW50BA5mYWN0b3J5X2NyYWZ0XwNpMnMOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBm91dHB1dA5nbG9iYWwuaW50LnNldAhjb25zdGFudAQIY3JhZnRpbmcIY29uc3RhbnQCAAAAAA==",
"D0ZBQ1RPUllfQ1JBRlRfMQAAAAAAAAAACQAAAA5nZW5lcmljLmdvdG9pZghjb25zdGFudAJjAAAADmNvbXBhcmlzb24uaW50Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BAE+CGNvbnN0YW50AgUAAAATZ2VuZXJpYy5leGVjdXRlc3luYwhjb25zdGFudAQXZmFjdG9yeV9jcmFmdF8xX2luY2hpcHMOZ2xvYmFsLmludC5zZXQIY29uc3RhbnQEDGNyYWZ0MV9zdGF0ZQhjb25zdGFudAIBAAAAD2dlbmVyaWMuZXhlY3V0ZQhjb25zdGFudAQSZmFjdG9yeV9jcmFmdF8xXzFBE2dlbmVyaWMuZXhlY3V0ZXN5bmMIY29uc3RhbnQEEmZhY3RvcnlfY3JhZnRfMV8xQhNnZW5lcmljLmV4ZWN1dGVzeW5jCGNvbnN0YW50BBFmYWN0b3J5X2NyYWZ0XzFfMhFnZW5lcmljLndhaXR3aGlsZRZmYWN0b3J5Lm1hY2hpbmUuYWN0aXZlCGNvbnN0YW50BAlhc3NlbWJsZXIRZ2VuZXJpYy53YWl0d2hpbGUOY29tcGFyaXNvbi5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEDGNyYWZ0MV9zdGF0ZQhjb25zdGFudAQBPAhjb25zdGFudAIPAAAADWZhY3RvcnkuY3JhZnQIY29uc3RhbnQEBGNoaXAOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXIRZ2xvYmFsLmRvdWJsZS5nZXQIY29uc3RhbnQEBWNvdW50",
"F2ZhY3RvcnlfY3JhZnRfMV9pbmNoaXBzAAAAAAAAAAAKAAAADmdlbmVyaWMuZ290b2lmCGNvbnN0YW50AmMAAAAOY29tcGFyaXNvbi5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXIIY29uc3RhbnQEAj09CGNvbnN0YW50AgEAAAAQbG9jYWwuZG91YmxlLnNldAhjb25zdGFudAQGdGFyZ2V0EWFyaXRobWV0aWMuZG91YmxlCmRvdWJsZS5taW4RYXJpdGhtZXRpYy5kb3VibGUIY29uc3RhbnQDAAAAAAAAEEAIY29uc3RhbnQEASoRYXJpdGhtZXRpYy5kb3VibGUDaTJkDmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BAEtCGNvbnN0YW50AwAAAAAAAPA/CGNvbnN0YW50AwAAAAAAAChACGNvbnN0YW50BAEqEWdsb2JhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAVjb3VudA5nZW5lcmljLmdvdG9pZghjb25zdGFudAJjAAAAEWNvbXBhcmlzb24uZG91YmxlE2ZhY3RvcnkuaXRlbXMuY291bnQIY29uc3RhbnQEBGNoaXAOYXJpdGhtZXRpYy5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXIIY29uc3RhbnQEAS0IY29uc3RhbnQCAQAAAAhjb25zdGFudAQCPj0QbG9jYWwuZG91YmxlLmdldAhjb25zdGFudAQGdGFyZ2V0DWxvY2FsLmludC5zZXQIY29uc3RhbnQEBm15dGllcg5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllchBsb2NhbC5kb3VibGUuc2V0CGNvbnN0YW50BAdteWNvdW50EWdsb2JhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAVjb3VudA5nbG9iYWwuaW50LnNldAhjb25zdGFudAQEdGllcg5hcml0aG1ldGljLmludA5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllcghjb25zdGFudAQBLQhjb25zdGFudAIBAAAAEWdsb2JhbC5kb3VibGUuc2V0CGNvbnN0YW50BAVjb3VudBFhcml0aG1ldGljLmRvdWJsZRBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAZ0YXJnZXQIY29uc3RhbnQEAS0TZmFjdG9yeS5pdGVtcy5jb3VudAhjb25zdGFudAQEY2hpcA5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllchNnZW5lcmljLmV4ZWN1dGVzeW5jCGNvbnN0YW50BA9mYWN0b3J5X2NyYWZ0XzEOZ2xvYmFsLmludC5zZXQIY29uc3RhbnQEBHRpZXINbG9jYWwuaW50LmdldAhjb25zdGFudAQGbXl0aWVyEWdsb2JhbC5kb3VibGUuc2V0CGNvbnN0YW50BAVjb3VudBBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAdteWNvdW50",
"EkZBQ1RPUllfY3JhZnRfMV8xYQAAAAABAAAADmNvbXBhcmlzb24uaW50Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAhsb2NhdGlvbghjb25zdGFudAQCPT0IY29uc3RhbnQCAwAAAAkAAAAQbG9jYWwuZG91YmxlLnNldAhjb25zdGFudAQIYm9hcmRfbG8RYXJpdGhtZXRpYy5kb3VibGURZ2xvYmFsLmRvdWJsZS5nZXQIY29uc3RhbnQEBWNvdW50CGNvbnN0YW50BAEqEWFyaXRobWV0aWMuZG91YmxlDGRvdWJsZS5mbG9vchFhcml0aG1ldGljLmRvdWJsZQhjb25zdGFudAMAAAAANGEqQQhjb25zdGFudAQBLxFhcml0aG1ldGljLmRvdWJsZQhjb25zdGFudAMAAAAAAAAkQAhjb25zdGFudAQDcG93A2kyZA5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllcghjb25zdGFudAQDbW9kCGNvbnN0YW50AwAAAAAAACRAEGxvY2FsLmRvdWJsZS5zZXQIY29uc3RhbnQECGJvYXJkX2hpEWFyaXRobWV0aWMuZG91YmxlEWdsb2JhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAVjb3VudAhjb25zdGFudAQBKhFhcml0aG1ldGljLmRvdWJsZQxkb3VibGUuZmxvb3IRYXJpdGhtZXRpYy5kb3VibGUIY29uc3RhbnQDAAAAAARQKkEIY29uc3RhbnQEAS8RYXJpdGhtZXRpYy5kb3VibGUIY29uc3RhbnQDAAAAAAAAJEAIY29uc3RhbnQEA3BvdwNpMmQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXIIY29uc3RhbnQEA21vZAhjb25zdGFudAMAAAAAAAAkQBFnZW5lcmljLndhaXR3aGlsZRZmYWN0b3J5Lm1hY2hpbmUuYWN0aXZlCGNvbnN0YW50BAdwcmVzc2VyD2ZhY3RvcnkucHJvZHVjZQhjb25zdGFudAQFaW5nb3QOYXJpdGhtZXRpYy5pbnQOYXJpdGhtZXRpYy5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXIIY29uc3RhbnQEASoIY29uc3RhbnQCAgAAAAhjb25zdGFudAQBLQhjb25zdGFudAIBAAAAEWFyaXRobWV0aWMuZG91YmxlEWFyaXRobWV0aWMuZG91YmxlEGxvY2FsLmRvdWJsZS5nZXQIY29uc3RhbnQECGJvYXJkX2xvCGNvbnN0YW50BAEtE2ZhY3RvcnkuaXRlbXMuY291bnQIY29uc3RhbnQEDXBsYXRlLmNpcmN1aXQOYXJpdGhtZXRpYy5pbnQOYXJpdGhtZXRpYy5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXIIY29uc3RhbnQEASoIY29uc3RhbnQCAgAAAAhjb25zdGFudAQBLQhjb25zdGFudAIBAAAACGNvbnN0YW50BAEtE2ZhY3RvcnkuaXRlbXMuY291bnQIY29uc3RhbnQEBXBsYXRlDmFyaXRobWV0aWMuaW50DmFyaXRobWV0aWMuaW50Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BAEqCGNvbnN0YW50AgIAAAAIY29uc3RhbnQEAS0IY29uc3RhbnQCAQAAAAhjb25zdGFudAQHcHJlc3NlchFnZW5lcmljLndhaXR3aGlsZRZmYWN0b3J5Lm1hY2hpbmUuYWN0aXZlCGNvbnN0YW50BAdwcmVzc2VyDmdsb2JhbC5pbnQuc2V0CGNvbnN0YW50BAxjcmFmdDFfc3RhdGUOYXJpdGhtZXRpYy5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEDGNyYWZ0MV9zdGF0ZQhjb25zdGFudAQBKwhjb25zdGFudAICAAAAD2ZhY3RvcnkucHJvZHVjZQhjb25zdGFudAQFaW5nb3QOYXJpdGhtZXRpYy5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXIIY29uc3RhbnQEASoIY29uc3RhbnQCAgAAABFhcml0aG1ldGljLmRvdWJsZRFhcml0aG1ldGljLmRvdWJsZRBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAhib2FyZF9oaQhjb25zdGFudAQBLRNmYWN0b3J5Lml0ZW1zLmNvdW50CGNvbnN0YW50BA1wbGF0ZS5jaXJjdWl0DmFyaXRobWV0aWMuaW50Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BAEqCGNvbnN0YW50AgIAAAAIY29uc3RhbnQEAS0TZmFjdG9yeS5pdGVtcy5jb3VudAhjb25zdGFudAQFcGxhdGUOYXJpdGhtZXRpYy5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXIIY29uc3RhbnQEASoIY29uc3RhbnQCAgAAAAhjb25zdGFudAQHcHJlc3NlchFnZW5lcmljLndhaXR3aGlsZRZmYWN0b3J5Lm1hY2hpbmUuYWN0aXZlCGNvbnN0YW50BAdwcmVzc2VyDmdsb2JhbC5pbnQuc2V0CGNvbnN0YW50BAxjcmFmdDFfc3RhdGUOYXJpdGhtZXRpYy5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEDGNyYWZ0MV9zdGF0ZQhjb25zdGFudAQBKwhjb25zdGFudAIEAAAA",
"EkZBQ1RPUllfY3JhZnRfMV8xYgAAAAABAAAADmNvbXBhcmlzb24uaW50Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAhsb2NhdGlvbghjb25zdGFudAQCPT0IY29uc3RhbnQCAwAAAAsAAAAQbG9jYWwuZG91YmxlLnNldAhjb25zdGFudAQKY2lyY3VpdF9sbxFhcml0aG1ldGljLmRvdWJsZRFnbG9iYWwuZG91YmxlLmdldAhjb25zdGFudAQFY291bnQIY29uc3RhbnQEASoIY29uc3RhbnQDAAAAAAAAAEAQbG9jYWwuZG91YmxlLnNldAhjb25zdGFudAQKY2lyY3VpdF9oaRFhcml0aG1ldGljLmRvdWJsZRFnbG9iYWwuZG91YmxlLmdldAhjb25zdGFudAQFY291bnQIY29uc3RhbnQEASoRYXJpdGhtZXRpYy5kb3VibGUMZG91YmxlLmZsb29yEWFyaXRobWV0aWMuZG91YmxlCGNvbnN0YW50AwAAAAAgZQtBCGNvbnN0YW50BAEvEWFyaXRobWV0aWMuZG91YmxlCGNvbnN0YW50AwAAAAAAACRACGNvbnN0YW50BANwb3cDaTJkDmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BANtb2QIY29uc3RhbnQDAAAAAAAAJEARZ2VuZXJpYy53YWl0d2hpbGUWZmFjdG9yeS5tYWNoaW5lLmFjdGl2ZQhjb25zdGFudAQIcmVmaW5lcnkPZmFjdG9yeS5wcm9kdWNlCGNvbnN0YW50BAVpbmdvdA5hcml0aG1ldGljLmludA5hcml0aG1ldGljLmludA5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllcghjb25zdGFudAQBKghjb25zdGFudAICAAAACGNvbnN0YW50BAEtCGNvbnN0YW50AgEAAAALZG91YmxlLmNlaWwRYXJpdGhtZXRpYy5kb3VibGURYXJpdGhtZXRpYy5kb3VibGURYXJpdGhtZXRpYy5kb3VibGUQbG9jYWwuZG91YmxlLmdldAhjb25zdGFudAQKY2lyY3VpdF9sbwhjb25zdGFudAQBLRNmYWN0b3J5Lml0ZW1zLmNvdW50CGNvbnN0YW50BAdjaXJjdWl0DmFyaXRobWV0aWMuaW50DmFyaXRobWV0aWMuaW50Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BAEqCGNvbnN0YW50AgIAAAAIY29uc3RhbnQEAS0IY29uc3RhbnQCAQAAAAhjb25zdGFudAQBLRNmYWN0b3J5Lml0ZW1zLmNvdW50CGNvbnN0YW50BAVjYWJsZQ5hcml0aG1ldGljLmludA5hcml0aG1ldGljLmludA5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllcghjb25zdGFudAQBKghjb25zdGFudAICAAAACGNvbnN0YW50BAEtCGNvbnN0YW50AgEAAAAIY29uc3RhbnQEAS8IY29uc3RhbnQDAAAAAAAAAEAIY29uc3RhbnQECHJlZmluZXJ5EWdlbmVyaWMud2FpdHdoaWxlFmZhY3RvcnkubWFjaGluZS5hY3RpdmUIY29uc3RhbnQECHJlZmluZXJ5D2ZhY3RvcnkucHJvZHVjZQhjb25zdGFudAQFaW5nb3QOYXJpdGhtZXRpYy5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXIIY29uc3RhbnQEASoIY29uc3RhbnQCAgAAAAtkb3VibGUuY2VpbBFhcml0aG1ldGljLmRvdWJsZRFhcml0aG1ldGljLmRvdWJsZRFhcml0aG1ldGljLmRvdWJsZRBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BApjaXJjdWl0X2hpCGNvbnN0YW50BAEtE2ZhY3RvcnkuaXRlbXMuY291bnQIY29uc3RhbnQEB2NpcmN1aXQOYXJpdGhtZXRpYy5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXIIY29uc3RhbnQEASoIY29uc3RhbnQCAgAAAAhjb25zdGFudAQBLRNmYWN0b3J5Lml0ZW1zLmNvdW50CGNvbnN0YW50BAVjYWJsZQ5hcml0aG1ldGljLmludA5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllcghjb25zdGFudAQBKghjb25zdGFudAICAAAACGNvbnN0YW50BAEvCGNvbnN0YW50AwAAAAAAAABACGNvbnN0YW50BAhyZWZpbmVyeRFnZW5lcmljLndhaXR3aGlsZRZmYWN0b3J5Lm1hY2hpbmUuYWN0aXZlCGNvbnN0YW50BAlhc3NlbWJsZXIPZmFjdG9yeS5wcm9kdWNlCGNvbnN0YW50BAVjYWJsZQ5hcml0aG1ldGljLmludA5hcml0aG1ldGljLmludA5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllcghjb25zdGFudAQBKghjb25zdGFudAICAAAACGNvbnN0YW50BAEtCGNvbnN0YW50AgEAAAARYXJpdGhtZXRpYy5kb3VibGUQbG9jYWwuZG91YmxlLmdldAhjb25zdGFudAQKY2lyY3VpdF9sbwhjb25zdGFudAQBLRNmYWN0b3J5Lml0ZW1zLmNvdW50CGNvbnN0YW50BAdjaXJjdWl0DmFyaXRobWV0aWMuaW50DmFyaXRobWV0aWMuaW50Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BAEqCGNvbnN0YW50AgIAAAAIY29uc3RhbnQEAS0IY29uc3RhbnQCAQAAAAhjb25zdGFudAQJYXNzZW1ibGVyEWdlbmVyaWMud2FpdHdoaWxlFmZhY3RvcnkubWFjaGluZS5hY3RpdmUIY29uc3RhbnQECHJlZmluZXJ5EWdlbmVyaWMud2FpdHdoaWxlFmZhY3RvcnkubWFjaGluZS5hY3RpdmUIY29uc3RhbnQECWFzc2VtYmxlcg9mYWN0b3J5LnByb2R1Y2UIY29uc3RhbnQEBWNhYmxlDmFyaXRobWV0aWMuaW50Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BAEqCGNvbnN0YW50AgIAAAARYXJpdGhtZXRpYy5kb3VibGUQbG9jYWwuZG91YmxlLmdldAhjb25zdGFudAQKY2lyY3VpdF9oaQhjb25zdGFudAQBLRNmYWN0b3J5Lml0ZW1zLmNvdW50CGNvbnN0YW50BAdjaXJjdWl0DmFyaXRobWV0aWMuaW50Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BAEqCGNvbnN0YW50AgIAAAAIY29uc3RhbnQECWFzc2VtYmxlcg==",

"EUZBQ1RPUllfY3JhZnRfMV8yAAAAAAEAAAAOY29tcGFyaXNvbi5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQECGxvY2F0aW9uCGNvbnN0YW50BAI9PQhjb25zdGFudAIDAAAACgAAABBsb2NhbC5kb3VibGUuc2V0CGNvbnN0YW50BAhib2FyZF9sbxFhcml0aG1ldGljLmRvdWJsZRFnbG9iYWwuZG91YmxlLmdldAhjb25zdGFudAQFY291bnQIY29uc3RhbnQEASoRYXJpdGhtZXRpYy5kb3VibGUMZG91YmxlLmZsb29yEWFyaXRobWV0aWMuZG91YmxlCGNvbnN0YW50AwAAAAA0YSpBCGNvbnN0YW50BAEvEWFyaXRobWV0aWMuZG91YmxlCGNvbnN0YW50AwAAAAAAACRACGNvbnN0YW50BANwb3cDaTJkDmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BANtb2QIY29uc3RhbnQDAAAAAAAAJEAQbG9jYWwuZG91YmxlLnNldAhjb25zdGFudAQIYm9hcmRfaGkRYXJpdGhtZXRpYy5kb3VibGURZ2xvYmFsLmRvdWJsZS5nZXQIY29uc3RhbnQEBWNvdW50CGNvbnN0YW50BAEqEWFyaXRobWV0aWMuZG91YmxlDGRvdWJsZS5mbG9vchFhcml0aG1ldGljLmRvdWJsZQhjb25zdGFudAMAAAAABFAqQQhjb25zdGFudAQBLxFhcml0aG1ldGljLmRvdWJsZQhjb25zdGFudAMAAAAAAAAkQAhjb25zdGFudAQDcG93A2kyZA5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllcghjb25zdGFudAQDbW9kCGNvbnN0YW50AwAAAAAAACRAEWdlbmVyaWMud2FpdHdoaWxlDmNvbXBhcmlzb24uaW50Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAxjcmFmdDFfc3RhdGUIY29uc3RhbnQEATwIY29uc3RhbnQCAwAAABFnZW5lcmljLndhaXR3aGlsZRZmYWN0b3J5Lm1hY2hpbmUuYWN0aXZlCGNvbnN0YW50BAhyZWZpbmVyeQ9mYWN0b3J5LnByb2R1Y2UIY29uc3RhbnQEBXBsYXRlDmFyaXRobWV0aWMuaW50DmFyaXRobWV0aWMuaW50Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BAEqCGNvbnN0YW50AgIAAAAIY29uc3RhbnQEAS0IY29uc3RhbnQCAQAAABFhcml0aG1ldGljLmRvdWJsZRBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAhib2FyZF9sbwhjb25zdGFudAQBLRNmYWN0b3J5Lml0ZW1zLmNvdW50CGNvbnN0YW50BA1wbGF0ZS5jaXJjdWl0DmFyaXRobWV0aWMuaW50DmFyaXRobWV0aWMuaW50Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BAEqCGNvbnN0YW50AgIAAAAIY29uc3RhbnQEAS0IY29uc3RhbnQCAQAAAAhjb25zdGFudAQIcmVmaW5lcnkRZ2VuZXJpYy53YWl0d2hpbGUOY29tcGFyaXNvbi5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEDGNyYWZ0MV9zdGF0ZQhjb25zdGFudAQBPAhjb25zdGFudAIHAAAAEWdlbmVyaWMud2FpdHdoaWxlFmZhY3RvcnkubWFjaGluZS5hY3RpdmUIY29uc3RhbnQECHJlZmluZXJ5D2ZhY3RvcnkucHJvZHVjZQhjb25zdGFudAQFcGxhdGUOYXJpdGhtZXRpYy5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXIIY29uc3RhbnQEASoIY29uc3RhbnQCAgAAABFhcml0aG1ldGljLmRvdWJsZRBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAhib2FyZF9oaQhjb25zdGFudAQBLRNmYWN0b3J5Lml0ZW1zLmNvdW50CGNvbnN0YW50BA1wbGF0ZS5jaXJjdWl0DmFyaXRobWV0aWMuaW50Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BAEqCGNvbnN0YW50AgIAAAAIY29uc3RhbnQECHJlZmluZXJ5EWdlbmVyaWMud2FpdHdoaWxlFmZhY3RvcnkubWFjaGluZS5hY3RpdmUIY29uc3RhbnQECHJlZmluZXJ5Dmdsb2JhbC5pbnQuc2V0CGNvbnN0YW50BAxjcmFmdDFfc3RhdGUOYXJpdGhtZXRpYy5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEDGNyYWZ0MV9zdGF0ZQhjb25zdGFudAQBKwhjb25zdGFudAIIAAAA",
"D0ZBQ1RPUllfQ1JBRlRfMgAAAAAAAAAABgAAAA5nbG9iYWwuaW50LnNldAhjb25zdGFudAQMY3JhZnQyX3N0YXRlCGNvbnN0YW50AgEAAAAPZ2VuZXJpYy5leGVjdXRlCGNvbnN0YW50BBZmYWN0b3J5X2NyYWZ0XzJfcGxhdGVzD2dlbmVyaWMuZXhlY3V0ZQhjb25zdGFudAQVZmFjdG9yeV9jcmFmdF8yX2NvaWxzD2dlbmVyaWMuZXhlY3V0ZQhjb25zdGFudAQUZmFjdG9yeV9jcmFmdF8yX3JvZHMRZ2VuZXJpYy53YWl0dW50aWwOY29tcGFyaXNvbi5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEDGNyYWZ0Ml9zdGF0ZQhjb25zdGFudAQCPT0IY29uc3RhbnQCDwAAAA1mYWN0b3J5LmNyYWZ0CGNvbnN0YW50BAVtb3Rvcg5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllchFnbG9iYWwuZG91YmxlLmdldAhjb25zdGFudAQFY291bnQ=",
"FkZBQ1RPUllfQ1JBRlRfMl9wbGF0ZXMAAAAAAAAAAAYAAAAQbG9jYWwuZG91YmxlLnNldAhjb25zdGFudAQGdGFyZ2V0EWFyaXRobWV0aWMuZG91YmxlEWdsb2JhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAVjb3VudAhjb25zdGFudAQBKghjb25zdGFudAMAAAAAAAAQQA5nZW5lcmljLmdvdG9pZghjb25zdGFudAIGAAAAEWNvbXBhcmlzb24uZG91YmxlE2ZhY3RvcnkuaXRlbXMuY291bnQIY29uc3RhbnQEBXBsYXRlDmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BAI+PRBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAZ0YXJnZXQRZ2VuZXJpYy53YWl0d2hpbGUWZmFjdG9yeS5tYWNoaW5lLmFjdGl2ZQhjb25zdGFudAQHcHJlc3Nlcg9mYWN0b3J5LnByb2R1Y2UIY29uc3RhbnQEBWluZ290Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyEWFyaXRobWV0aWMuZG91YmxlEGxvY2FsLmRvdWJsZS5nZXQIY29uc3RhbnQEBnRhcmdldAhjb25zdGFudAQBLRNmYWN0b3J5Lml0ZW1zLmNvdW50CGNvbnN0YW50BAVwbGF0ZQ5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllcghjb25zdGFudAQHcHJlc3NlchFnZW5lcmljLndhaXR3aGlsZRZmYWN0b3J5Lm1hY2hpbmUuYWN0aXZlCGNvbnN0YW50BAdwcmVzc2VyDmdsb2JhbC5pbnQuc2V0CGNvbnN0YW50BAxjcmFmdDJfc3RhdGUOYXJpdGhtZXRpYy5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEDGNyYWZ0Ml9zdGF0ZQhjb25zdGFudAQBKwhjb25zdGFudAICAAAA",
"FUZBQ1RPUllfQ1JBRlRfMl9jb2lscwAAAAAAAAAACAAAABBsb2NhbC5kb3VibGUuc2V0CGNvbnN0YW50BApuZWVkX2NvaWxzEWFyaXRobWV0aWMuZG91YmxlEWdsb2JhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAVjb3VudAhjb25zdGFudAQBKghjb25zdGFudAMAAAAAAADwPw5nZW5lcmljLmdvdG9pZghjb25zdGFudAIFAAAAEWNvbXBhcmlzb24uZG91YmxlE2ZhY3RvcnkuaXRlbXMuY291bnQIY29uc3RhbnQEBWNhYmxlDmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BAI+PRBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BApuZWVkX2NvaWxzEWdlbmVyaWMud2FpdHdoaWxlFmZhY3RvcnkubWFjaGluZS5hY3RpdmUIY29uc3RhbnQECHJlZmluZXJ5D2ZhY3RvcnkucHJvZHVjZQhjb25zdGFudAQFaW5nb3QOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXILZG91YmxlLmNlaWwRYXJpdGhtZXRpYy5kb3VibGURYXJpdGhtZXRpYy5kb3VibGUQbG9jYWwuZG91YmxlLmdldAhjb25zdGFudAQKbmVlZF9jb2lscwhjb25zdGFudAQBLRNmYWN0b3J5Lml0ZW1zLmNvdW50CGNvbnN0YW50BAVjYWJsZQ5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllcghjb25zdGFudAQBLwhjb25zdGFudAMAAAAAAAAAQAhjb25zdGFudAQIcmVmaW5lcnkRZ2VuZXJpYy53YWl0d2hpbGUWZmFjdG9yeS5tYWNoaW5lLmFjdGl2ZQhjb25zdGFudAQIcmVmaW5lcnkPZmFjdG9yeS5wcm9kdWNlCGNvbnN0YW50BAVjYWJsZQ5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllchBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BApuZWVkX2NvaWxzCGNvbnN0YW50BAhyZWZpbmVyeRFnZW5lcmljLndhaXR3aGlsZRZmYWN0b3J5Lm1hY2hpbmUuYWN0aXZlCGNvbnN0YW50BAhyZWZpbmVyeQ5nbG9iYWwuaW50LnNldAhjb25zdGFudAQMY3JhZnQyX3N0YXRlDmFyaXRobWV0aWMuaW50Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAxjcmFmdDJfc3RhdGUIY29uc3RhbnQEASsIY29uc3RhbnQCBAAAAA==",
"FEZBQ1RPUllfQ1JBRlRfMl9yb2RzAAAAAAAAAAAMAAAAEGxvY2FsLmRvdWJsZS5zZXQIY29uc3RhbnQEC25lZWRfc2NyZXdzCmRvdWJsZS5tYXgIY29uc3RhbnQDAAAAAAAAAAARYXJpdGhtZXRpYy5kb3VibGURZ2xvYmFsLmRvdWJsZS5nZXQIY29uc3RhbnQEBWNvdW50CGNvbnN0YW50BAEtE2ZhY3RvcnkuaXRlbXMuY291bnQIY29uc3RhbnQEBXNjcmV3Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyEGxvY2FsLmRvdWJsZS5zZXQIY29uc3RhbnQECW5lZWRfcm9kcwpkb3VibGUubWF4CGNvbnN0YW50AwAAAAAAAAAAEWFyaXRobWV0aWMuZG91YmxlEWFyaXRobWV0aWMuZG91YmxlEWFyaXRobWV0aWMuZG91YmxlEWdsb2JhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAVjb3VudAhjb25zdGFudAQBKghjb25zdGFudAMAAAAAAAAAQAhjb25zdGFudAQBKwtkb3VibGUuY2VpbBFhcml0aG1ldGljLmRvdWJsZRBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAtuZWVkX3NjcmV3cwhjb25zdGFudAQBLwhjb25zdGFudAMAAAAAAAAQQAhjb25zdGFudAQBLRNmYWN0b3J5Lml0ZW1zLmNvdW50CGNvbnN0YW50BANyb2QOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXIOZ2VuZXJpYy5nb3RvaWYIY29uc3RhbnQCBwAAABFjb21wYXJpc29uLmRvdWJsZRBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAluZWVkX3JvZHMIY29uc3RhbnQEAj09CGNvbnN0YW50AwAAAAAAAAAAEWdlbmVyaWMud2FpdHdoaWxlFmZhY3RvcnkubWFjaGluZS5hY3RpdmUIY29uc3RhbnQEBnNoYXBlcg9mYWN0b3J5LnByb2R1Y2UIY29uc3RhbnQEBWluZ290Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyC2RvdWJsZS5jZWlsEWFyaXRobWV0aWMuZG91YmxlEGxvY2FsLmRvdWJsZS5nZXQIY29uc3RhbnQECW5lZWRfcm9kcwhjb25zdGFudAQBLwhjb25zdGFudAMAAAAAAAAAQAhjb25zdGFudAQGc2hhcGVyEWdlbmVyaWMud2FpdHVudGlsEWNvbXBhcmlzb24uZG91YmxlE2ZhY3RvcnkuaXRlbXMuY291bnQIY29uc3RhbnQEA3JvZA5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllcghjb25zdGFudAQCPj0RYXJpdGhtZXRpYy5kb3VibGUQbG9jYWwuZG91YmxlLmdldAhjb25zdGFudAQLbmVlZF9zY3Jld3MIY29uc3RhbnQEAS8IY29uc3RhbnQDAAAAAAAAEEAOZ2VuZXJpYy5nb3RvaWYIY29uc3RhbnQCCwAAABFjb21wYXJpc29uLmRvdWJsZRBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAtuZWVkX3NjcmV3cwhjb25zdGFudAQCPT0IY29uc3RhbnQDAAAAAAAAAAARZ2VuZXJpYy53YWl0d2hpbGUWZmFjdG9yeS5tYWNoaW5lLmFjdGl2ZQhjb25zdGFudAQGY3V0dGVyD2ZhY3RvcnkucHJvZHVjZQhjb25zdGFudAQDcm9kDmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyC2RvdWJsZS5jZWlsEWFyaXRobWV0aWMuZG91YmxlEGxvY2FsLmRvdWJsZS5nZXQIY29uc3RhbnQEC25lZWRfc2NyZXdzCGNvbnN0YW50BAEvCGNvbnN0YW50AwAAAAAAABBACGNvbnN0YW50BAZjdXR0ZXIRZ2VuZXJpYy53YWl0d2hpbGUWZmFjdG9yeS5tYWNoaW5lLmFjdGl2ZQhjb25zdGFudAQGY3V0dGVyEWdlbmVyaWMud2FpdHdoaWxlFmZhY3RvcnkubWFjaGluZS5hY3RpdmUIY29uc3RhbnQEBnNoYXBlcg5nbG9iYWwuaW50LnNldAhjb25zdGFudAQMY3JhZnQyX3N0YXRlDmFyaXRobWV0aWMuaW50Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAxjcmFmdDJfc3RhdGUIY29uc3RhbnQEASsIY29uc3RhbnQCCAAAAA==",
"D0ZBQ1RPUllfQ1JBRlRfMwAAAAAAAAAABgAAAA5nbG9iYWwuaW50LnNldAhjb25zdGFudAQMY3JhZnQzX3N0YXRlCGNvbnN0YW50AgEAAAATZ2VuZXJpYy5leGVjdXRlc3luYwhjb25zdGFudAQPZmFjdG9yeV9jcmFmdF8yD2dlbmVyaWMuZXhlY3V0ZQhjb25zdGFudAQWZmFjdG9yeV9jcmFmdF8zX3BsYXRlcw9nZW5lcmljLmV4ZWN1dGUIY29uc3RhbnQEFWZhY3RvcnlfY3JhZnRfM19yaW5ncxFnZW5lcmljLndhaXR1bnRpbA5jb21wYXJpc29uLmludA5nbG9iYWwuaW50LmdldAhjb25zdGFudAQMY3JhZnQyX3N0YXRlCGNvbnN0YW50BAI9PQhjb25zdGFudAIHAAAADWZhY3RvcnkuY3JhZnQIY29uc3RhbnQEBHB1bXAOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXIRZ2xvYmFsLmRvdWJsZS5nZXQIY29uc3RhbnQEBWNvdW50",
"FkZBQ1RPUllfQ1JBRlRfM19wbGF0ZXMAAAAAAAAAAAsAAAAQbG9jYWwuZG91YmxlLnNldAhjb25zdGFudAQGdGFyZ2V0EWFyaXRobWV0aWMuZG91YmxlEWdsb2JhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAVjb3VudAhjb25zdGFudAQBKghjb25zdGFudAMAAAAAAAAAQA5nZW5lcmljLmdvdG9pZghjb25zdGFudAIGAAAAEWNvbXBhcmlzb24uZG91YmxlE2ZhY3RvcnkuaXRlbXMuY291bnQIY29uc3RhbnQEBXBsYXRlDmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BAI+PRBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAZ0YXJnZXQRZ2VuZXJpYy53YWl0d2hpbGUWZmFjdG9yeS5tYWNoaW5lLmFjdGl2ZQhjb25zdGFudAQHcHJlc3Nlcg9mYWN0b3J5LnByb2R1Y2UIY29uc3RhbnQEBWluZ290Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyEWFyaXRobWV0aWMuZG91YmxlEGxvY2FsLmRvdWJsZS5nZXQIY29uc3RhbnQEBnRhcmdldAhjb25zdGFudAQBLRNmYWN0b3J5Lml0ZW1zLmNvdW50CGNvbnN0YW50BAVwbGF0ZQ5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllcghjb25zdGFudAQHcHJlc3NlchFnZW5lcmljLndhaXR3aGlsZRZmYWN0b3J5Lm1hY2hpbmUuYWN0aXZlCGNvbnN0YW50BAdwcmVzc2VyEGxvY2FsLmRvdWJsZS5zZXQIY29uc3RhbnQEBnRhcmdldBFhcml0aG1ldGljLmRvdWJsZRFnbG9iYWwuZG91YmxlLmdldAhjb25zdGFudAQFY291bnQIY29uc3RhbnQEASoIY29uc3RhbnQDAAAAAAAAEEAOZ2VuZXJpYy5nb3RvaWYIY29uc3RhbnQCCwAAABFjb21wYXJpc29uLmRvdWJsZRNmYWN0b3J5Lml0ZW1zLmNvdW50CGNvbnN0YW50BAxwbGF0ZS5ydWJiZXIIY29uc3RhbnQCAQAAAAhjb25zdGFudAQCPj0QbG9jYWwuZG91YmxlLmdldAhjb25zdGFudAQGdGFyZ2V0EWdlbmVyaWMud2FpdHdoaWxlFmZhY3RvcnkubWFjaGluZS5hY3RpdmUIY29uc3RhbnQEB3ByZXNzZXIPZmFjdG9yeS5wcm9kdWNlCGNvbnN0YW50BAZydWJiZXIIY29uc3RhbnQCAQAAABFhcml0aG1ldGljLmRvdWJsZRBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAZ0YXJnZXQIY29uc3RhbnQEAS0TZmFjdG9yeS5pdGVtcy5jb3VudAhjb25zdGFudAQMcGxhdGUucnViYmVyCGNvbnN0YW50AgEAAAAIY29uc3RhbnQEB3ByZXNzZXIRZ2VuZXJpYy53YWl0d2hpbGUWZmFjdG9yeS5tYWNoaW5lLmFjdGl2ZQhjb25zdGFudAQHcHJlc3Nlcg5nbG9iYWwuaW50LnNldAhjb25zdGFudAQMY3JhZnQzX3N0YXRlDmFyaXRobWV0aWMuaW50Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAxjcmFmdDNfc3RhdGUIY29uc3RhbnQEASsIY29uc3RhbnQCAgAAAA==",
"FUZBQ1RPUllfQ1JBRlRfM19yaW5ncwAAAAAAAAAACQAAABBsb2NhbC5kb3VibGUuc2V0CGNvbnN0YW50BAZ0YXJnZXQRYXJpdGhtZXRpYy5kb3VibGURZ2xvYmFsLmRvdWJsZS5nZXQIY29uc3RhbnQEBWNvdW50CGNvbnN0YW50BAEqCGNvbnN0YW50AwAAAAAAAABADmdlbmVyaWMuZ290b2lmCGNvbnN0YW50AgkAAAARY29tcGFyaXNvbi5kb3VibGUTZmFjdG9yeS5pdGVtcy5jb3VudAhjb25zdGFudAQEcmluZw5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllcghjb25zdGFudAQCPj0QbG9jYWwuZG91YmxlLmdldAhjb25zdGFudAQGdGFyZ2V0DmdlbmVyaWMuZ290b2lmCGNvbnN0YW50AgYAAAARY29tcGFyaXNvbi5kb3VibGUTZmFjdG9yeS5pdGVtcy5jb3VudAhjb25zdGFudAQDcm9kDmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BAI+PRBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAZ0YXJnZXQRZ2VuZXJpYy53YWl0d2hpbGUWZmFjdG9yeS5tYWNoaW5lLmFjdGl2ZQhjb25zdGFudAQGc2hhcGVyD2ZhY3RvcnkucHJvZHVjZQhjb25zdGFudAQFaW5nb3QOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXILZG91YmxlLmNlaWwRYXJpdGhtZXRpYy5kb3VibGURYXJpdGhtZXRpYy5kb3VibGUQbG9jYWwuZG91YmxlLmdldAhjb25zdGFudAQGdGFyZ2V0CGNvbnN0YW50BAEtE2ZhY3RvcnkuaXRlbXMuY291bnQIY29uc3RhbnQEA3JvZA5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllcghjb25zdGFudAQBLwhjb25zdGFudAMAAAAAAAAAQAhjb25zdGFudAQGc2hhcGVyEWdlbmVyaWMud2FpdHdoaWxlFmZhY3RvcnkubWFjaGluZS5hY3RpdmUIY29uc3RhbnQEBnNoYXBlcg9mYWN0b3J5LnByb2R1Y2UIY29uc3RhbnQEA3JvZA5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllchFhcml0aG1ldGljLmRvdWJsZRBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAZ0YXJnZXQIY29uc3RhbnQEAS0TZmFjdG9yeS5pdGVtcy5jb3VudAhjb25zdGFudAQEcmluZw5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllcghjb25zdGFudAQGc2hhcGVyEWdlbmVyaWMud2FpdHdoaWxlFmZhY3RvcnkubWFjaGluZS5hY3RpdmUIY29uc3RhbnQEBnNoYXBlcg5nbG9iYWwuaW50LnNldAhjb25zdGFudAQMY3JhZnQyX3N0YXRlDmFyaXRobWV0aWMuaW50Dmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAxjcmFmdDJfc3RhdGUIY29uc3RhbnQEASsIY29uc3RhbnQCBAAAAA==",
"D0ZBQ1RPUllfQ1JBRlRfNAAAAAAAAAAABQAAAA5nbG9iYWwuaW50LnNldAhjb25zdGFudAQMY3JhZnQ0X3N0YXRlCGNvbnN0YW50AgEAAAAPZ2VuZXJpYy5leGVjdXRlCGNvbnN0YW50BBZmYWN0b3J5X2NyYWZ0XzRfY2FibGVzD2dlbmVyaWMuZXhlY3V0ZQhjb25zdGFudAQWZmFjdG9yeV9jcmFmdF80X3J1YmJlchFnZW5lcmljLndhaXR1bnRpbA5jb21wYXJpc29uLmludA5nbG9iYWwuaW50LmdldAhjb25zdGFudAQMY3JhZnQ0X3N0YXRlCGNvbnN0YW50BAI9PQhjb25zdGFudAIHAAAADWZhY3RvcnkuY3JhZnQIY29uc3RhbnQED2NhYmxlLmluc3VsYXRlZA5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllchFnbG9iYWwuZG91YmxlLmdldAhjb25zdGFudAQFY291bnQ=",
"FkZBQ1RPUllfQ1JBRlRfNF9jYWJsZXMAAAAAAAAAAAcAAAAQbG9jYWwuZG91YmxlLnNldAhjb25zdGFudAQGdGFyZ2V0CmRvdWJsZS5tYXgKZG91YmxlLm1heAhjb25zdGFudAMAAAAAAADwPxFhcml0aG1ldGljLmRvdWJsZQNpMmQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXIIY29uc3RhbnQEAS0IY29uc3RhbnQDAAAAAAAAAEAKZG91YmxlLm1heBFhcml0aG1ldGljLmRvdWJsZQhjb25zdGFudAMAAAAAAAAkQAhjb25zdGFudAQBLRFhcml0aG1ldGljLmRvdWJsZQhjb25zdGFudAMAAAAAAAAUQAhjb25zdGFudAQBKhFhcml0aG1ldGljLmRvdWJsZRFhcml0aG1ldGljLmRvdWJsZQNpMmQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXIIY29uc3RhbnQEAS0IY29uc3RhbnQDAAAAAAAAIEAIY29uc3RhbnQEA3Bvdwhjb25zdGFudAMAAAAAAAAAQAtkb3VibGUuY2VpbBFhcml0aG1ldGljLmRvdWJsZRFhcml0aG1ldGljLmRvdWJsZRFhcml0aG1ldGljLmRvdWJsZQNpMmQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXIIY29uc3RhbnQEAS0IY29uc3RhbnQDAAAAAAAA8D8IY29uc3RhbnQEA3Bvdwhjb25zdGFudAMAAAAAAAD4Pwhjb25zdGFudAQBLQhjb25zdGFudAMAAAAAAAAmQBBsb2NhbC5kb3VibGUuc2V0CGNvbnN0YW50BAZ0YXJnZXQRYXJpdGhtZXRpYy5kb3VibGUQbG9jYWwuZG91YmxlLmdldAhjb25zdGFudAQGdGFyZ2V0CGNvbnN0YW50BAEqEWdsb2JhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAVjb3VudA5nZW5lcmljLmdvdG9pZghjb25zdGFudAIHAAAAEWNvbXBhcmlzb24uZG91YmxlE2ZhY3RvcnkuaXRlbXMuY291bnQIY29uc3RhbnQEBWNhYmxlDmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BAI+PRBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAZ0YXJnZXQRZ2VuZXJpYy53YWl0d2hpbGUWZmFjdG9yeS5tYWNoaW5lLmFjdGl2ZQhjb25zdGFudAQIcmVmaW5lcnkPZmFjdG9yeS5wcm9kdWNlCGNvbnN0YW50BAVpbmdvdA5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllcgtkb3VibGUuY2VpbBFhcml0aG1ldGljLmRvdWJsZRFhcml0aG1ldGljLmRvdWJsZRBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAZ0YXJnZXQIY29uc3RhbnQEAS0TZmFjdG9yeS5pdGVtcy5jb3VudAhjb25zdGFudAQFY2FibGUOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEBHRpZXIIY29uc3RhbnQEAS8IY29uc3RhbnQDAAAAAAAAAEAIY29uc3RhbnQECHJlZmluZXJ5EWdlbmVyaWMud2FpdHdoaWxlFmZhY3RvcnkubWFjaGluZS5hY3RpdmUIY29uc3RhbnQECHJlZmluZXJ5Dmdsb2JhbC5pbnQuc2V0CGNvbnN0YW50BAxjcmFmdDRfc3RhdGUOYXJpdGhtZXRpYy5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEDGNyYWZ0NF9zdGF0ZQhjb25zdGFudAQBKwhjb25zdGFudAICAAAA",

"FkZBQ1RPUllfQ1JBRlRfNF9ydWJiZXIAAAAAAAAAAAcAAAAQbG9jYWwuZG91YmxlLnNldAhjb25zdGFudAQGdGFyZ2V0CmRvdWJsZS5tYXgIY29uc3RhbnQDAAAAAAAAAAARYXJpdGhtZXRpYy5kb3VibGURYXJpdGhtZXRpYy5kb3VibGURYXJpdGhtZXRpYy5kb3VibGUIY29uc3RhbnQDAAAAAAAAAEAIY29uc3RhbnQEASoDaTJkDmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BAEtCGNvbnN0YW50AwAAAAAAABBACGNvbnN0YW50BAEtCmRvdWJsZS5tYXgIY29uc3RhbnQDAAAAAAAAAAARYXJpdGhtZXRpYy5kb3VibGUIY29uc3RhbnQDAAAAAAAAAEAIY29uc3RhbnQEAS0RYXJpdGhtZXRpYy5kb3VibGURYXJpdGhtZXRpYy5kb3VibGUDaTJkDmdsb2JhbC5pbnQuZ2V0CGNvbnN0YW50BAR0aWVyCGNvbnN0YW50BAEtCGNvbnN0YW50AwAAAAAAACBACGNvbnN0YW50BAEqEWFyaXRobWV0aWMuZG91YmxlA2kyZA5nbG9iYWwuaW50LmdldAhjb25zdGFudAQEdGllcghjb25zdGFudAQBLQhjb25zdGFudAMAAAAAAAAiQBBsb2NhbC5kb3VibGUuc2V0CGNvbnN0YW50BAZ0YXJnZXQRYXJpdGhtZXRpYy5kb3VibGUQbG9jYWwuZG91YmxlLmdldAhjb25zdGFudAQGdGFyZ2V0CGNvbnN0YW50BAEqEWdsb2JhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAVjb3VudA5nZW5lcmljLmdvdG9pZghjb25zdGFudAIHAAAAEWNvbXBhcmlzb24uZG91YmxlE2ZhY3RvcnkuaXRlbXMuY291bnQIY29uc3RhbnQEDHBsYXRlLnJ1YmJlcghjb25zdGFudAIBAAAACGNvbnN0YW50BAI+PRBsb2NhbC5kb3VibGUuZ2V0CGNvbnN0YW50BAZ0YXJnZXQRZ2VuZXJpYy53YWl0d2hpbGUWZmFjdG9yeS5tYWNoaW5lLmFjdGl2ZQhjb25zdGFudAQHcHJlc3Nlcg9mYWN0b3J5LnByb2R1Y2UIY29uc3RhbnQEBnJ1YmJlcghjb25zdGFudAIBAAAAEWFyaXRobWV0aWMuZG91YmxlEGxvY2FsLmRvdWJsZS5nZXQIY29uc3RhbnQEBnRhcmdldAhjb25zdGFudAQBLRNmYWN0b3J5Lml0ZW1zLmNvdW50CGNvbnN0YW50BAxwbGF0ZS5ydWJiZXIIY29uc3RhbnQCAQAAAAhjb25zdGFudAQHcHJlc3NlchFnZW5lcmljLndhaXR3aGlsZRZmYWN0b3J5Lm1hY2hpbmUuYWN0aXZlCGNvbnN0YW50BAdwcmVzc2VyDmdsb2JhbC5pbnQuc2V0CGNvbnN0YW50BAxjcmFmdDRfc3RhdGUOYXJpdGhtZXRpYy5pbnQOZ2xvYmFsLmludC5nZXQIY29uc3RhbnQEDGNyYWZ0NF9zdGF0ZQhjb25zdGFudAQBKwhjb25zdGFudAIEAAAA",
"BHRlc3QAAAAAAwAAABJ0b3duLndpbmRvdy5pc29wZW4IY29uc3RhbnQEBm11c2V1bRJ0b3duLndpbmRvdy5pc29wZW4IY29uc3RhbnQEBmFyY2FkZRJ0b3duLndpbmRvdy5pc29wZW4IY29uc3RhbnQECHdvcmtzaG9wDgAAAA1mYWN0b3J5LmNyYWZ0CGNvbnN0YW50BBFwcm9kdWNlci5zaGlweWFyZAhjb25zdGFudAIBAAAACGNvbnN0YW50AwAAAAAAAPA/DWZhY3RvcnkuY3JhZnQIY29uc3RhbnQEFnByb2R1Y2VyLnN0YXR1ZW9mY3Vib3MIY29uc3RhbnQCAQAAAAhjb25zdGFudAMAAAAAAADwPw1mYWN0b3J5LmNyYWZ0CGNvbnN0YW50BA1wcm9kdWNlci5nZW1zCGNvbnN0YW50AgEAAAAIY29uc3RhbnQDAAAAAAAA8D8NZmFjdG9yeS5jcmFmdAhjb25zdGFudAQTcHJvZHVjZXIuZXhvdGljZ2Vtcwhjb25zdGFudAIBAAAACGNvbnN0YW50AwAAAAAAAPA/DWZhY3RvcnkuY3JhZnQIY29uc3RhbnQEDG1hY2hpbmUub3Zlbghjb25zdGFudAIBAAAACGNvbnN0YW50AwAAAAAAAPA/DWZhY3RvcnkuY3JhZnQIY29uc3RhbnQED21hY2hpbmUucHJlc3Nlcghjb25zdGFudAIBAAAACGNvbnN0YW50AwAAAAAAAPA/DWZhY3RvcnkuY3JhZnQIY29uc3RhbnQEFW1hY2hpbmUudHJhbnNwb3J0YmVsdAhjb25zdGFudAIBAAAACGNvbnN0YW50AwAAAAAAAPA/DWZhY3RvcnkuY3JhZnQIY29uc3RhbnQED21hY2hpbmUuY3J1c2hlcghjb25zdGFudAIBAAAACGNvbnN0YW50AwAAAAAAAPA/DWZhY3RvcnkuY3JhZnQIY29uc3RhbnQEDW1hY2hpbmUubWl4ZXIIY29uc3RhbnQCAQAAAAhjb25zdGFudAMAAAAAAADwPw1mYWN0b3J5LmNyYWZ0CGNvbnN0YW50BBBtYWNoaW5lLnJlZmluZXJ5CGNvbnN0YW50AgEAAAAIY29uc3RhbnQDAAAAAAAA8D8NZmFjdG9yeS5jcmFmdAhjb25zdGFudAQRbWFjaGluZS5hc3NlbWJsZXIIY29uc3RhbnQCAQAAAAhjb25zdGFudAMAAAAAAADwPw1mYWN0b3J5LmNyYWZ0CGNvbnN0YW50BA5tYWNoaW5lLnNoYXBlcghjb25zdGFudAIBAAAACGNvbnN0YW50AwAAAAAAAPA/DWZhY3RvcnkuY3JhZnQIY29uc3RhbnQEDm1hY2hpbmUuY3V0dGVyCGNvbnN0YW50AgEAAAAIY29uc3RhbnQDAAAAAAAA8D8NZmFjdG9yeS5jcmFmdAhjb25zdGFudAQObWFjaGluZS5ib2lsZXIIY29uc3RhbnQCAQAAAAhjb25zdGFudAMAAAAAAADwPw==",
"BHRlc3QAAAAAAAAAAAoAAAAPZmFjdG9yeS5wcm9kdWNlCGNvbnN0YW50BAZydWJiZXIIY29uc3RhbnQCAQAAAAhjb25zdGFudAMAAAAAAADwPwhjb25zdGFudAQEb3Zlbg9mYWN0b3J5LnByb2R1Y2UIY29uc3RhbnQEA29yZQhjb25zdGFudAIBAAAACGNvbnN0YW50AwAAAAAAAPA/CGNvbnN0YW50BAlhc3NlbWJsZXIPZmFjdG9yeS5wcm9kdWNlCGNvbnN0YW50BARkdXN0CGNvbnN0YW50AgEAAAAIY29uc3RhbnQDAAAAAAAA8D8IY29uc3RhbnQECHJlZmluZXJ5D2ZhY3RvcnkucHJvZHVjZQhjb25zdGFudAQFaW5nb3QIY29uc3RhbnQCAQAAAAhjb25zdGFudAMAAAAAAADwPwhjb25zdGFudAQHY3J1c2hlcg9mYWN0b3J5LnByb2R1Y2UIY29uc3RhbnQEC3BsYXRlLnN0YWNrCGNvbnN0YW50AgEAAAAIY29uc3RhbnQDAAAAAAAA8D8IY29uc3RhbnQEBmN1dHRlcg9mYWN0b3J5LnByb2R1Y2UIY29uc3RhbnQEA3JvZAhjb25zdGFudAIBAAAACGNvbnN0YW50AwAAAAAAAPA/CGNvbnN0YW50BAdwcmVzc2VyD2ZhY3RvcnkucHJvZHVjZQhjb25zdGFudAQFcGxhdGUIY29uc3RhbnQCAQAAAAhjb25zdGFudAMAAAAAAADwPwhjb25zdGFudAQFbWl4ZXIPZmFjdG9yeS5wcm9kdWNlCGNvbnN0YW50BAVjYWJsZQhjb25zdGFudAIBAAAACGNvbnN0YW50AwAAAAAAAPA/CGNvbnN0YW50BAZzaGFwZXIPZmFjdG9yeS5wcm9kdWNlCGNvbnN0YW50BARsdW1wCGNvbnN0YW50AgEAAAAIY29uc3RhbnQDAAAAAAAA8D8IY29uc3RhbnQEBmJvaWxlcg9mYWN0b3J5LnByb2R1Y2UIY29uc3RhbnQEBWJsb2NrCGNvbnN0YW50AgEAAAAIY29uc3RhbnQDAAAAAAAA8D8IY29uc3RhbnQEBG92ZW4=",
"BHRlc3QAAAAAAAAAAAkAAAARZ2xvYmFsLmRvdWJsZS5zZXQIY29uc3RhbnQEABNmYWN0b3J5Lml0ZW1zLmNvdW50CGNvbnN0YW50BAtibG9jay5kZW5zZQhjb25zdGFudAIBAAAAEWdsb2JhbC5kb3VibGUuc2V0CGNvbnN0YW50BAATZmFjdG9yeS5pdGVtcy5jb3VudAhjb25zdGFudAQLcGxhdGUuZGVuc2UIY29uc3RhbnQCAQAAABFnbG9iYWwuZG91YmxlLnNldAhjb25zdGFudAQAE2ZhY3RvcnkuaXRlbXMuY291bnQIY29uc3RhbnQEBXNjcmV3CGNvbnN0YW50AgEAAAARZ2xvYmFsLmRvdWJsZS5zZXQIY29uc3RhbnQEABNmYWN0b3J5Lml0ZW1zLmNvdW50CGNvbnN0YW50BAxwbGF0ZS5ydWJiZXIIY29uc3RhbnQCAQAAABFnbG9iYWwuZG91YmxlLnNldAhjb25zdGFudAQAE2ZhY3RvcnkuaXRlbXMuY291bnQIY29uc3RhbnQEDXBsYXRlLmNpcmN1aXQIY29uc3RhbnQCAQAAABFnbG9iYWwuZG91YmxlLnNldAhjb25zdGFudAQAE2ZhY3RvcnkuaXRlbXMuY291bnQIY29uc3RhbnQEBHJpbmcIY29uc3RhbnQCAQAAABFnbG9iYWwuZG91YmxlLnNldAhjb25zdGFudAQAE2ZhY3RvcnkuaXRlbXMuY291bnQIY29uc3RhbnQEBHBpcGUIY29uc3RhbnQCAQAAABFnbG9iYWwuZG91YmxlLnNldAhjb25zdGFudAQAE2ZhY3RvcnkuaXRlbXMuY291bnQIY29uc3RhbnQEBHdpcmUIY29uc3RhbnQCAQAAABFnbG9iYWwuZG91YmxlLnNldAhjb25zdGFudAQAE2ZhY3RvcnkuaXRlbXMuY291bnQIY29uc3RhbnQEB2NpcmN1aXQIY29uc3RhbnQCAQAAAA==",
"BHRlc3QAAAAAAAAAAA0AAAAQdG93bi53aW5kb3cuc2hvdwhjb25zdGFudAQMdG93ZXJ0ZXN0aW5nCGNvbnN0YW50AQEQdG93bi53aW5kb3cuc2hvdwhjb25zdGFudAQLdHJhZGluZ3Bvc3QIY29uc3RhbnQBARB0b3duLndpbmRvdy5zaG93CGNvbnN0YW50BApwb3dlcnBsYW50CGNvbnN0YW50AQEQdG93bi53aW5kb3cuc2hvdwhjb25zdGFudAQHZmFjdG9yeQhjb25zdGFudAEBEHRvd24ud2luZG93LnNob3cIY29uc3RhbnQECmxhYm9yYXRvcnkIY29uc3RhbnQBARB0b3duLndpbmRvdy5zaG93CGNvbnN0YW50BAhzaGlweWFyZAhjb25zdGFudAEBEHRvd24ud2luZG93LnNob3cIY29uc3RhbnQECHdvcmtzaG9wCGNvbnN0YW50AQEQdG93bi53aW5kb3cuc2hvdwhjb25zdGFudAQGYXJjYWRlCGNvbnN0YW50AQEQdG93bi53aW5kb3cuc2hvdwhjb25zdGFudAQGbXVzZXVtCGNvbnN0YW50AQEQdG93bi53aW5kb3cuc2hvdwhjb25zdGFudAQMaGVhZHF1YXJ0ZXJzCGNvbnN0YW50AQEQdG93bi53aW5kb3cuc2hvdwhjb25zdGFudAQQY29uc3RydWN0aW9uZmlybQhjb25zdGFudAEBEHRvd24ud2luZG93LnNob3cIY29uc3RhbnQEDXN0YXR1ZW9mY3Vib3MIY29uc3RhbnQBARB0b3duLndpbmRvdy5zaG93CGNvbnN0YW50BARtaW5lCGNvbnN0YW50AQE=",
"BFRlc3QAAAAAAAAAAAEAAAAQbG9jYWwuZG91YmxlLnNldAhjb25zdGFudAQBaQhjb25zdGFudAMzp6jVI/ZJOQ==",
"BHRlc3QAAAAAAAAAAAEAAAAMZ2VuZXJpYy53YWl0CGNvbnN0YW50AwAAAAAAAPB/",
"BHRlc3QAAAAAAAAAAAEAAAAMZ2VuZXJpYy53YWl0CGNvbnN0YW50AwAAAAAAAPD/",
"BHRlc3QAAAAAAAAAAAEAAAAMZ2VuZXJpYy53YWl0CGNvbnN0YW50AwAAAAAAAPj/",
"BHRlc3QAAAAAAAAAAAEAAAAMZ2VuZXJpYy53YWl0EWFyaXRobWV0aWMuZG91YmxlCGNvbnN0YW50AwAAAAAAAAAACGNvbnN0YW50BAR0ZXN0CGNvbnN0YW50AwAAAAAAAAAA",
"BHRlc3QAAAAAAAAAAAEAAAAObG9jYWwudmVjMi5zZXQIY29uc3RhbnQEA2Jhcghjb25zdGFudAUAAIBAAACAPw==",
"BHRlc3QAAAAAAAAAAAEAAAAMZ2VuZXJpYy5zdG9wCGNvbnN0YW50BAN7YX0=",
  }
  local compile_tests = {macro_test = {[[
    ; Basic test of macros and macro functions
    #concat(a, b) {a}{b}
    #concat2(a, b) {lua(return [=[{a}]=] .. [=[{b}]=])}
    :global string res
    res = "{concat(1,2)}{co{concat2(ncat,)}(3,4")}
  ]], "Cm1hY3JvX3Rlc3QAAAAAAAAAAAEAAAARZ2xvYmFsLnN0cmluZy5zZXQIY29uc3RhbnQEA3Jlcwhjb25zdGFudAQEMTIzNA=="},
  imported = {[[
    ; This should compile to (almost) no code
    :local int acc
    :global string status
    #output status = "Number is: " . acc
  ]], "CGltcG9ydGVkAAAAAAAAAAAAAAAA"},
  import_test = {[[
    :import imported
    :import imported
    {output}
  ]], "C2ltcG9ydF90ZXN0AAAAAAAAAAABAAAAEWdsb2JhbC5zdHJpbmcuc2V0CGNvbnN0YW50BAZzdGF0dXMGY29uY2F0CGNvbnN0YW50BAtOdW1iZXIgaXM6IANpMnMNbG9jYWwuaW50LmdldAhjb25zdGFudAQDYWNj"},
  folding_test = {[[
    :global string test

    test = 1.."a"
    test = 1 ."a"
    test = 1 .""
    test = "a".1.
    test = "a".1
    test = "".1
  ]], "DGZvbGRpbmdfdGVzdAAAAAAAAAAABgAAABFnbG9iYWwuc3RyaW5nLnNldAhjb25zdGFudAQEdGVzdAhjb25zdGFudAQEMS4wYRFnbG9iYWwuc3RyaW5nLnNldAhjb25zdGFudAQEdGVzdAhjb25zdGFudAQCMWERZ2xvYmFsLnN0cmluZy5zZXQIY29uc3RhbnQEBHRlc3QIY29uc3RhbnQEATERZ2xvYmFsLnN0cmluZy5zZXQIY29uc3RhbnQEBHRlc3QIY29uc3RhbnQEBGExLjARZ2xvYmFsLnN0cmluZy5zZXQIY29uc3RhbnQEBHRlc3QIY29uc3RhbnQEAmExEWdsb2JhbC5zdHJpbmcuc2V0CGNvbnN0YW50BAR0ZXN0CGNvbnN0YW50BAEx"},
  lowercase_test = {[[
    software.toggle("software.criticalWavejump", true)
    craft("producer.constructionFirm", 1, 1.)
  ]], "Dmxvd2VyY2FzZV90ZXN0AAAAAAAAAAACAAAAD3NvZnR3YXJlLnRvZ2dsZQhjb25zdGFudAQZc29mdHdhcmUuY3JpdGljYWxXYXZlanVtcAhjb25zdGFudAEBDWZhY3RvcnkuY3JhZnQIY29uc3RhbnQEGXByb2R1Y2VyLmNvbnN0cnVjdGlvbkZpcm0IY29uc3RhbnQCAQAAAAhjb25zdGFudAMAAAAAAADwPw=="},
  }
  local new_import_tests = {
    default_default = {{actions={}, conditions={}, impulses={}, name="test", package=""}, [[
:name test]], [[
{"actions":[],"conditions":[],"impulses":[],"name":"test","package":""}]]},
    default_0 = {{budget=0, actions={}, conditions={}, impulses={}, name="test", package=""}, [[
:name test]], [[
{"actions":[],"conditions":[],"impulses":[],"name":"test","package":""}]]},
    default_1 = {{budget=1, actions={}, conditions={}, impulses={}, name="test", package=""}, [[
:name test
:budget_cap 1
:use_budget default]], [[
{"actions":[],"conditions":[],"impulses":[],"name":"test","package":"","budget":1}]]},
    false_default = {{useBudget=false, actions={}, conditions={}, impulses={}, name="test", package=""}, [[
:name test]], [[
{"actions":[],"conditions":[],"impulses":[],"name":"test","package":""}]]},
    false_0 = {{useBudget=false, budget=0, actions={}, conditions={}, impulses={}, name="test", package=""}, [[
:name test]], [[
{"actions":[],"conditions":[],"impulses":[],"name":"test","package":""}]]},
    false_1 = {{useBudget=false, budget=1, actions={}, conditions={}, impulses={}, name="test", package=""}, [[
:name test
:budget_cap 1
:use_budget false]], [[
{"actions":[],"conditions":[],"impulses":[],"name":"test","package":"","budget":1,"useBudget":false}]]},
    true_default = {{useBudget=true, actions={}, conditions={}, impulses={}, name="test", package=""}, [[
:name test
:use_budget true]], [[
{"actions":[],"conditions":[],"impulses":[],"name":"test","package":"","useBudget":true}]]},
    true_0 = {{useBudget=true, budget=0, actions={}, conditions={}, impulses={}, name="test", package=""}, [[
:name test
:budget_cap 0]], [[
{"actions":[],"conditions":[],"impulses":[],"name":"test","package":"","budget":0,"useBudget":true}]]},
    true_1 = {{useBudget=true, budget=1, actions={}, conditions={}, impulses={}, name="test", package=""}, [[
:name test
:budget_cap 1]], [[
{"actions":[],"conditions":[],"impulses":[],"name":"test","package":"","budget":1,"useBudget":true}]]},
    high_bytes = {{actions={"\x11global.double.set\bconstant\x04\x03foo\bconstant\x03ÍÌÌÌÌÌô?"}, conditions={}, impulses={}, name="test", package=""}, [[
:name test

:global double foo

foo = 1.3]], [[
{"actions":["\u0011global.double.set\bconstant\u0004\u0003foo\bconstant\u0003ÍÌÌÌÌÌô?"],"conditions":[],"impulses":[],"name":"test","package":""}]]},
    constant_vector = {{actions={"\x0elocal.vec2.set\bconstant\x04\x03bar\bconstant\x05\0\0\x80?\0\0\0@"}, conditions={}, impulses={}, name="test", package=""}, [[
:name test

:local vector bar

bar = vec(1.0, 2.0)]], [[
{"actions":["\u000elocal.vec2.set\bconstant\u0004\u0003bar\bconstant\u0005\u0000\u0000?\u0000\u0000\u0000@"],"conditions":[],"impulses":[],"name":"test","package":""}]]},
  }

  local status, ret

  local function importFunc(name)
    local v = compile_tests[name]
    if v then
      return true, v[1]
    end
    return false, (name .. " not found")
  end
  for k, v in pairs(compile_tests) do
    status, ret = pcall(compile, k, v[1], {format="v0"}, importFunc)
    assert(status, string.format("Failed to compile unit test %s at line %d\n\n%s", k, line_number, ret))
    assert(ret.code == v[2], string.format("Unit test %s failure! Expected:\n%s\nActual:\n%s", k, v[2], ret.code))
  end
  for k, v in pairs(new_import_tests) do
    status, ret = pcall(import, v[1])
    assert(status, string.format("Failed to import unit test %s\n\n%s", k, ret))
    assert(ret[2] == v[2], string.format("Unit test %s import failure! Expected:\n%s\nActual:\n%s", k, v[2], ret[2]))

    status, ret = pcall(compile, ret[1], ret[2], {format="v2"}, importFunc)
    assert(status, string.format("Failed to compile unit test %s\n\n%s", k, ret))
    assert(ret.code == v[3], string.format("Unit test %s compile failure! Expected:\n%s\nActual:\n%s", k, v[3], ret.code))
  end
  for k, v in ipairs (import_tests) do
    status, ret = pcall(import, v)
    assert(status, string.format("Failed to import unit test #%s\n\n%s", k, ret))

    status, ret = pcall(compile, ret[1], ret[2], {format="v0"}, importFunc)
    assert(status, string.format("Failed to compile unit test #%s\n\n%s", k, ret))

    v = ret.code
    status, ret = pcall(import, v)
    assert(status, string.format("Failed to re-import unit test #%s\n\n%s", k, ret))

    status, ret = pcall(compile, ret[1], ret[2], {format="v0"}, nil)
    assert(status, string.format("Failed to re-compile unit test #%s\n\n%s", k, ret))
    assert(ret.code == v, string.format("Failed to match unit test #%s\n\n%s\n\n%s\n\n%s\n\n%s", k, v, ret.code, base64.decode(v), base64.decode(ret.code)))
  end
  return true
end
