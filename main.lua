DEBUG = not fengari;
package.path = "scripts/?.lua";

local line_number;
local _cache = {};

for _, lib in ipairs {"base64", "lexer-functions", "lexer-operators", "lexer-tokens", "lexer-debug", "lexer"} do
	if DEBUG then
		dofile(package.path:gsub("?", lib));
	else
		require(lib);
	end
end

if not DEBUG then
	local js = require"js";
	local window = js.global;
	local document = window.document;

	local output = document:getElementById("output");
	local lua_arg = document:getElementById("lua_arg");

	local assert_old = assert;

	local function assert_lexer(test, msg)
		if not test then
			assert_old(false, string.format("GSUB_HERE%s: %s", line_number, msg));
		end

		return test;
	end

	function lua_main(func)
		-- window.console.clear();

		if func == "compile" then
			assert = assert_lexer;
			local status, ret = pcall(compile, lua_arg.value, window.editor:getValue());
			assert = assert_old;

			if status then
				output.value = ret;
				output.copy = ret:match".*\n.*()\n";
			else
				output.value = ret:gsub(".*GSUB_HERE", "");
				output.copy = nil;
			end
		elseif func == "workspace" then
			assert = assert_lexer;
			local status, ret = pcall(compile, lua_arg.name, lua_arg.text, true);

			if status then
				output.workspace = ret;
			else
				output.workspace = nil;
				output.value = ret:gsub(".*GSUB_HERE", lua_arg.name .. "\n");
			end
		elseif func == "import" then
			local status, ret = pcall(import, lua_arg.value);

			if status then
				return ret;
			else
				output.value = ret;
			end
		elseif func == "unittest" then
			local status, ret = pcall(unittest);

			if not status then
				output.value = ret;
			end
		else
			assert(false, "BUG REPORT: unknown lua_main function: " .. func);
		end
	end

	local elem = document:getElementById("functionList");
	elem.innerHTML = FUNCTION_LIST;
end

local function cache(line, variables)
	local key = {};

	for _, v in pairs (variables) do
		table.insert(key, string.format("%s.%s.%s", v.scope, v.type, v.name));
	end

	table.sort(key);
	table.insert(key, line);
	key = table.concat(key, "¤");

	if not _cache[key] then
		_cache[key] = lexer(line, variables);
	end

	return _cache[key];
end

local function parseMacro(text, macros, depth)
	assert(depth < 100, "macro expansion depth reached " .. depth ..  ", probable infinite loop in: " .. text)
	return text:gsub("%b{}", function(macro)
		if #macro == 2 then
			return
		end
		macro = parseMacro(macro:sub(2,-2), macros, depth + 1);
		local arg_body = "";
		local name = macro:match("^([^%(]+)$");
		if not name then
			name, arg_body = macro:match("^([^%(]+)(%b())");
		end
		assert(name, "invalid macro call: " .. macro);
		assert(#name + #arg_body == #macro, "trailing junk after macro call: " .. macro);
		if name == "len" then
			assert(arg_body ~= "", "len is a macro function")
			return tostring(#arg_body - 2);
		elseif name == "lua" then
			assert(arg_body ~= "", "lua is a macro function")
			local chunk, err = load(arg_body:sub(2,-2));
			assert(chunk, err)
			local result = chunk()
			assert(result ~= nil, "lua() returned nil (did you forget to return?)")
			return tostring(result);
		end
		local args = {};
		local arg_count = 0;
		local arg_begin = 2;
		local nesting = 0;
		for i = 2, #arg_body do
			local char = arg_body:sub(i, i);
			if nesting == 0 and (char == "," or i == #arg_body) then
				args["{#"..arg_count.."#}"] = arg_body:sub(arg_begin, i-1);
				arg_count = arg_count + 1;
				arg_begin = i + 1;
			elseif char == "(" then
				nesting = nesting + 1;
			elseif char == ")" and i ~= #arg_body then
				nesting = nesting - 1;
				assert(nesting >= 0, "unbalanced parenthesis inside macro call: " .. macro);
			end
		end
		assert(nesting == 0, "unclosed parenthesis inside macro call: " .. macro);
		name = name:lower();
		local macro_obj = macros[name];
		assert(macro_obj, "macro does not exist: " .. name);
		local unexpanded, arg_len = macro_obj.text, macro_obj.arg_len;
		assert(arg_len == arg_count,
			"macro call has wrong number of args, expected " .. arg_len ..
			" but got " .. arg_count .. ": " .. macro);
		return parseMacro(unexpanded:gsub("{#[0-9]+#}", args), macros, depth + 1);
	end);
end

function compile(name, input, testing)
	local variables, impulses, conditions, actions = {}, {}, {}, {};
	local ret = {};
	line_number = 0;

	local macros = {len = '__builtin__', lua = '__builtin__'};
	local lines = {};
	local labelCache = {};
	local lineCache = {};

	for real_line in input:gmatch"[^\n]*" do
		line_number = line_number + 1;
		if real_line:sub(-1) == "\\" then
			lineCache[#lineCache+1] = real_line:sub(1, -2);
			goto continue;
		end
		lineCache[#lineCache+1] = real_line;
		line = table.concat(lineCache):gsub("^%s+", ""):gsub("%s+$", "");
		lineCache = {};

		if line:match"^#" then
			local macro_args = "";
			local name, macro = line:sub(2):match(TOKEN.identifier.pattern .. " (.+)$");
			if not name then
				name, macro_args, macro = line:sub(2):match(TOKEN.identifier.pattern .. "(%([%w%._ ,]+%)) (.+)$");
			end
			assert(name, "macro definition: #name <text> or #name(args...) <text>");
			local args = {};
			local arg_len = 0;
			local arg_begin = 2;
			while arg_begin <= #macro_args do
				local pos = macro_args:find(",", arg_begin, true);
				if not pos then
					pos = #macro_args;
				end
				local arg_string = macro_args:sub(arg_begin, pos - 1);
				local arg = arg_string:match("^ *" .. TOKEN.identifier.patternAnywhere .. " *$");
				assert(arg, "bad macro function argument name: " .. arg_string);
				args["{" .. arg .. "}"] = "{#" .. arg_len .. "#}";
				arg_len = arg_len + 1;
				arg_begin = pos + 1;
			end
			name = name:lower();
			assert(not macros[name], "macro already exists: " .. name);
			macros[name] = {text = macro:gsub("{[^{}]+}", args), arg_len = arg_len};
		elseif line:match"^:const" then
			local _, type, name, value = line:sub(2):match("^(%a+) (%a+) " .. TOKEN.identifier.patternAnywhere .. " (.+)$");
			assert(type == "int" or type == "double" or type == "string" or type == "bool", "constant types are 'int', 'double', 'string' and 'bool");
			if (type == "int" or type == "double") then
				assert((value:match"^%d+$" and type == "int") or (value:match"^%d+%.%d*$" and type == "double"), "bad argument, " .. type .. " expected, got " .. value);
				value = tonumber(value);
			elseif (type == "bool") then
				value = value:lower();
				assert(value:match"^true$" or value:match"^false$", "bool values are 'true' or 'false'");
				if value:match"^true$" then
					value = true;
				else
					value = false;
				end
			elseif (type == "string") then
				quote, value = value:match("^%s*([\"\'])([^%1]*)%1%s*$");
				assert(value, "bad argument, string are enclosed in either single quotes or double quotes");
			end
			name = name:lower()
			assert(not variables[name], "variable/label/constant already exists: " .. name);
			variables[name] = {name = name, scope = "constant", type = type, value = value};
		elseif line:match"^:" then
			local scope, type, name = line:sub(2):gsub(" *;.*", ""):match("^(%a+) (%a+) " .. TOKEN.identifier.patternAnywhere .."$");
			assert(scope, "variable definition: [global/local/const] [int/double/string] name");

			name = name:lower();
			assert(scope == "global" or scope == "local", "variable scopes are 'global' and 'local'");
			assert(type == "int" or type == "double" or type == "string", "variable types are 'int', 'double' and 'string'");
			assert(not variables[name], "variable/label already exists: " .. name);
			
			variables[name] = {name = name, scope = scope, type = type};
		else
			line = parseMacro(line, macros, 1)
				:gsub(TOKEN.identifier.pattern .. ":", function(name)
					name = name:lower();
					assert(not variables[name] or labelCache[name], "variable/label already exists: " .. name);
					variables[name] = {name = name, scope = "local", type = "int", label = 0};
					table.insert(labelCache, name);
					return "";
				end)
				:gsub("^%s+", ""):gsub("%s+$", "")
			;

			if #line:gsub("^%s*;.*$", "") > 0 then
				table.insert(lines, {text = line, num = line_number, label = labelCache});
				labelCache = {};
			end
		end
		::continue::
	end

	for _, line in ipairs (lines) do
		line_number = line.num;
		local node = cache(line.text, variables);

		if node and node.func then
			if node.func.ret == "void" then
				table.insert(actions, node);
				
				if #(line.label) > 0 then
					for _, label in ipairs (line.label) do
						variables[label].label = #actions;
					end
				end
			else
					assert(#(line.label) == 0, "labels cannot be placed before impulses/conditions");
			
				if node.func.ret == "impulse" then
					table.insert(impulses, node);
				else
					table.insert(conditions, node);
				end
			end
		end
	end

	-- anything left in the label cache points to the end of the script
	for _, label in ipairs (labelCache) do
		variables[label].label = 99;
	end

	local function ins(frmt, val)
		table.insert(ret, string.pack(frmt, val));
	end

	local function encode(node)
		if node.func then
			if node.func.name == "label" then
				local var = node.args[1].value;
				assert(variables[var], "why are you calling the label function manually?")
				encode{type = "number", value = variables[var].label};
				return;
			elseif node.func.name:match"^constant%." then
				local var = node.args[1].value;
				assert(variables[var], "why are you calling the constant function manually?")
				local type = variables[var].type;
				if (type == "int" or type == "double") then
					type = "number";
				end
				encode{type = type, value = variables[var].value};
				return
			end

			ins("s1", node.func.name);

			for _, arg in ipairs (node.args) do
				encode(arg);
			end
		else
			ins("s1", "constant");

			if node.type == "bool" then
				ins("b", 1);
				ins("b", node.value and 1 or 0);
			elseif node.type == "number" then
				if math.type(node.value) == "integer" then
					ins("b", 2);
					ins("i4", node.value);
				else
					ins("b", 3);
					ins("d", node.value);
				end
			elseif node.type == "string" then
				ins("b", 4);
				local bytes = {};
				local len = #node.value;

				while len > 0 or #bytes == 0 do
					table.insert(bytes, string.pack("B", (len >= 0x80 and 0x80 or 0x00) + (len & 0x7F)))
					len = len >> 7;
				end

				table.insert(ret, table.concat(bytes));
				table.insert(ret, node.value);
			elseif node.type == "vector" then
				ins("b", 5);
				ins("f", node.x);
				ins("f", node.y);
			elseif node.type == "operator" then
				ins("b", 4);

				if node.value == "%" then
					node.value = "mod";
				elseif node.value == "^" then
					node.value = "pow";
				elseif node.value == "//" then
					node.value = "log";
				end

				ins("s1", node.value);
			else
				assert(false, "BUG REPORT: unknown compile type: " .. node.type);
			end
		end
	end

	ins("s1", name);

	for _, tbl in ipairs {impulses, conditions, actions} do
		ins("i4", #tbl);

		for _, line in ipairs (tbl) do
			encode(line);
		end
	end

	ret = base64.encode(table.concat(ret));
	return testing and ret or string.format("%s\n%s %s %s\n%s", name, #impulses, #conditions, #actions, ret);
end

function import(input)
	local data = base64.decode(input);
	local pos = 1;

	local variables = {};
	local ret = {};
	
	local function read(frmt)
		local ret, new = string.unpack(frmt, data, pos);
		pos = new;
		return ret;
	end

	local function stripParens(text)
		return tostring(text):gsub("^%b()", function(a) return a:sub(2,-2); end);
	end
	
	local function parse()
		local func = read"s1";
		
		if func == "constant" then
			local type = read"b";
			
			if type == 1 then
				return string.format("%s", read"b" == 1 and "true" or "false");
			elseif type == 2 then
				return string.format("%s", read"i4");
			elseif type == 3 then
				return string.format("%s", read"d");
			elseif type == 4 then
				local pos, len = 0, 0;

				repeat
					local byte = read"B";
					len = len + ((byte & 0x7F) << 7*pos);
					pos = pos + 1;
				until byte & 0x80 == 0

				local str = read("c" .. len);
				local sq, dq = str:match"'", str:match'"';

				if sq and not dq then
					return string.format('"%s"', str);
				elseif dq and not sq then
					return string.format("'%s'", str);
				end

				str = string.format('"%s"', str:gsub('"', [[" . '"' . "]])):gsub('"" %.', ""):gsub('%. ""', "");
				return str;
			elseif type == 5 then
				return string.format("vec(%s, %s)", read"f", read"f");
			else
				assert(false, "BUG REPORT: unknown constant type: " .. type);
			end
		else
			local func = assert(FUNCTION[func], "BUG REPORT: unknown function: " .. func);
			local args = {};
			local dynamicOperator = false;
			
			for i, arg in ipairs (func.args) do
				table.insert(args, parse());
				
				if arg.type:match"^op_" then
					if args[i]:match'^".*"$' then
						args[i] = args[i]:sub(2, -2):lower()
							:gsub("^=$", "==")
							:gsub("mod", "%%")
							:gsub("pow", "^")
							:gsub("log", "//")
						;
					end

					dynamicOperator = not OPERATOR[ args[i] ];
				end
			end

			local scope, type, func_name = func.name:match"(%a+)%.(%a+)%.(%a+)";

			if (scope == "global" or scope == "local") and args[1]:match'^"' then
				local var = args[1]:sub(2,-2):lower();

				if var == var:match(TOKEN.identifier.pattern) then
					if not variables[var] then
						local key = string.format(":%s %s %s", scope, type, var);
						variables[key] = true;
					end

					return func_name == "set" and string.format("%s = %s", var, stripParens(args[2])) or var;
				end
			elseif not dynamicOperator and (func.name:match"^arithmetic" or func.name:match"^comparison") then
				return string.format("(%s)", table.concat(args, " "));
			elseif func.name == "concat" then
				return string.format("(%s . %s)", table.unpack(args));
			end

			for k, v in ipairs (args) do
				args[k] = stripParens(v);
			end

			return string.format("%s(%s)", func.short, table.concat(args, ", "));
		end
	end

	local function ins(val)
		local text = stripParens(val);
		table.insert(ret, text);

		return val;
	end

	local name = read"s1";

	for i = 1, 3 do
		for j = 1, read"i4" do
			ins(parse());
			
			if i == 3 then
				ins(table.remove(ret));
			end
		end

		ins"";
	end

	table.insert(ret, 1, "");

	for var in pairs (variables) do
		table.insert(ret, 1, var);
	end

	table.remove(ret);
	ret = table.concat(ret, "\n"):gsub("\n\n+", "\n\n"):gsub("^\n", ""):gsub("\n$", "");
	return {name, ret};
end

function unittest()
	local tests = {
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

	};

	local status, ret;

	for k, v in ipairs (tests) do
		status, ret = pcall(import, v);
		assert(status, string.format("Failed to import unit test #%s\n\n%s", k, ret));

		status, ret = pcall(compile, ret[1], ret[2], true);
		assert(status, string.format("Failed to compile unit test #%s\n\n%s", k, ret));

		v = ret;
		status, ret = pcall(import, v);
		assert(status, string.format("Failed to re-import unit test #%s\n\n%s", k, ret));

		status, ret = pcall(compile, ret[1], ret[2], true);
		assert(status, string.format("Failed to re-compile unit test #%s\n\n%s", k, ret));
		assert(ret == v, string.format("Failed to match unit test #%s\n\n%s\n\n%s\n\n%s\n\n%s", k, v, ret, base64.decode(v), base64.decode(ret)));
	end

	return true;
end

LOAD_DONE = true;
