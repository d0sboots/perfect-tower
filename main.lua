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
			else
				output.value = ret:gsub(".*GSUB_HERE", "");
			end
		elseif func == "import" then
			local status, ret = pcall(import, lua_arg.value);

			if status then
				return ret;
			else
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

	for k, v in pairs (variables) do
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

function compile(name, input)
	local labels, variables, impulses, conditions, actions = {}, {}, {}, {}, {};
	local ret = {};
	line_number = 0;

	for line in input:gmatch"[^\n]*" do
		line_number = line_number + 1;

		if line:match"^:" then
			local scope, type, name = line:sub(2):match("^(%a+) (%a+) " .. TOKEN.identifier.patternAnywhere .."$");
			assert(scope, "variable definition: [global/local] [int/double] name");

			name = name:lower();
			assert(scope == "global" or scope == "local", "variable scopes are 'global' and 'local'");
			assert(type == "int" or type == "double", "variable types are 'int' and 'double'");
			assert(not variables[name], "variable already exists: " .. name);
			
			variables[name] = {name = name, scope = scope, type = type};
		else
			local label;
			line = line
				:gsub("^%s*([%w%.]+):", function(a) label = a; return ""; end)
				:gsub("^%s+", ""):gsub("%s+$", "")
			;

			if #line > 0 then
				local node = cache(line, variables);

				if node.func.ret == "impulse" then
					table.insert(impulses, node);
				elseif node.func.ret == "bool" then
					table.insert(conditions, node);
				elseif node.func.ret == "void" then
					table.insert(actions, node);

					if label then
						labels[label] = #actions;
					end
				end
			end
		end
	end

	local function ins(frmt, val)
		table.insert(ret, string.pack(frmt, val));
	end

	local function encode(node)
		if node.func then
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
				ins("s1", node.value);
			elseif node.type == "vector" then
				ins("b", 5);
				ins("f", node.x);
				ins("f", node.y);
			elseif node.type == "operator" then
				ins("b", 4);
				ins("s1", node.value == "//" and "log" or node.value);
			elseif node.type == "label" then
				ins("b", 2);
				ins("i4", assert(labels[node.value], "unknown label: " .. node.value));
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

	ret = string.format("%s\n%s\n%s",
		name,
		string.format("%s %s %s", #impulses, #conditions, #actions),
		base64.encode(table.concat(ret))
	);

	return ret;
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
				return string.format('"%s"', read"s1");
			elseif type == 5 then
				return string.format("vec(%s, %s)", read"f", read"f");
			else
				assert(false, "BUG REPORT: unknown constant type: " .. type);
			end
		else
			local func = assert(FUNCTION[func], "BUG REPORT: unknown function: " .. func);
			local args = {};
			
			for i, arg in ipairs (func.args) do
				table.insert(args, parse());
				
				if arg:match"^op_" then
					args[i] = args[i]:sub(2,-2):lower()
						:gsub("&+", "&")
						:gsub("|+", "|")
						:gsub("=+", "==")
						:gsub("mod", "%")
						:gsub("pow", "^")
						:gsub("log", "//")
					;
				end
			end

			local scope, type, func_name = func.name:match"(%a+).(%a+).(%a+)";

			if scope == "global" or scope == "local" then
				local var = args[1]:sub(2,-2):lower();

				if var == var:match(TOKEN.identifier.pattern) then
					if not variables[var] then
						local key = string.format(":%s %s %s", scope, type, var);
						variables[key] = true;
					end

					return func_name == "set" and string.format("%s = %s", var, stripParens(args[2])) or var;
				end
			elseif func.name:match"^arithmetic" or func.name:match"^comparison" then
				return string.format("(%s)", table.concat(args, " "));
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
				ins(string.format("%s: %s", j, table.remove(ret)));
			end
		end

		ins"";
	end

	local hasVar = false;

	for var in pairs (variables) do
		if not hasVar then
			table.insert(ret, 1, "");
			hasVar = true;
		end

		table.insert(ret, 1, var);
	end

	table.remove(ret);
	return {name, table.concat(ret, "\n")};
end

LOAD_DONE = true;