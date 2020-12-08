local UNIT_TEST;
local print_old = print;
local function print(...)
	if not UNIT_TEST then
		print_old(...);
	end
end

do
	local js = require"js";
	local window = js.global;
	local document = window.document;
	local template = [[

script Template
	option
		strict
	variable
		local int i
	impulse
		wakeup
	condition
		(true == false)
		isfill()
	action
		i = 0
	1:	dig((i % 4), (i / 4))
		i += 1
		goto(1)
		i = (i - 1)
]]

	function lua_main(func)
		window.console.clear();

		local output = document:getElementById("output");
		local arg;

		if func == "compile" then
			func = encode;
			arg = window.editor:getValue();
		elseif func == "import" then
			func = decode;
			arg = document:getElementById("import").value;
		elseif func == "template" then
			window.editor:setValue(window.editor:getValue() .. template);
			return;
		end

		local status, ret = pcall(func, arg);

		if status then
			output.value = ret;
		else
			output.value = ret:gsub(".*GSUB_HERE", "");
		end
	end
end
--[[
local function assert(test, message)
	if not test then
		error(message, 0);
	end
	
	return test;
end
--]]
function reload()
	os.execute"cls";
	dofile"perfect-tower.lua";
end

local script_options = {
	strict = "disables automatic type conversions",
};

local primitives = {void=0, bool=1, int=1, double=1, string=1, vector=1, label=1, op_comp=2, op_mod=2, op_special=2};
local operators = {};

do
	local type = "op_comp";

	for op in string.gmatch("== != && || > >= < <= = + - * / pow mod log ^ % += -= *= /= ^= %=", "[^ ]+") do
		if op == "=" then
			type = "op_mod";
		elseif op == "^" then
			type = "op_special";
		end
		
		operators[op] = type;
	end
end

local functions = {};
local functions_def = [[
void option() section
void variable() section
void impulse() section
void condition() section
void action() section

void wakeup() ! impulse
void key.0() ! impulse
void key.1() ! impulse
void key.2() ! impulse
void key.3() ! impulse
void key.4() ! impulse
void key.5() ! impulse
void key.6() ! impulse
void key.7() ! impulse
void key.8() ! impulse
void key.9() ! impulse
void open.mine() ! impulse
void open.factory() ! impulse
void open.workshop() ! impulse
void open.powerplant() ! impulse
void open.museum() ! impulse

void min(void, void) dynamic
void max(void, void) dynamic
void rnd(void, void) dynamic
void floor(void, void) dynamic
void ceil(void, void) dynamic
void round(void, void) dynamic

bool comparison.bool(bool, op_comp, bool) !

int global.int.get(string) !
int local.int.get(string) !
void global.int.set(string, int) !
void local.int.set(string, int) !
int arithmetic.int(int, op_mod, int) !
bool comparison.int(int, op_comp, int) !

int d2i(double)
int int.min(int, int) !
int int.max(int, int) !
int int.rnd(int, int) !
int int.floor(int) !
int int.ceil(int) !
int int.round(int) !

double global.double.get(string) !
double local.double.get(string) !
void global.double.set(string, double) !
void local.double.set(string, double) !
double arithmetic.double(double, op_mod, double) !
bool comparison.double(double, op_comp, double) !

double const.pi()
double i2d(int)
double double.min(double, double) !
double double.max(double, double) !
double double.rnd(double, double) !
double double.floor(double) !
double double.ceil(double) !
double double.round(double) !

string concat(string, string)
bool comparison.string(string, op_comp, string) !

string i2s(double)
string d2s(double)

double vec2.x(vector) !
double vec2.y(vector) !
vector vec.fromCoords(double, double)

void generic.execute(string)
void generic.executesync(string)
void generic.stop(string)
void generic.wait(double)
void generic.waitwhile(bool)
void generic.waituntil(bool)
void generic.goto(label)
void generic.gotoif(label, bool)
void generic.click(vector)

void tower.module.useinstant(int)

void powerplant.sell(int, int)

void mine.newlayer()
void mine.dig(int, int)

bool factory.machine.active(string)
double factory.items.count(string, int)
void factory.craft(string, int, double)
void factory.produce(string, int, double, string)

bool museum.isfill()
int museum.freeSlots(string)
int museum.stone.tier(string, int)
void museum.fill(bool)
void museum.buy(string)
void museum.combine()
void museum.transmute()
void museum.move(string, int, string)
void museum.delete(string, int)
void museum.stone.element(string, int)
]];
	
for line in functions_def:gsub("\r", ""):gmatch"[^\n]+" do
	local ret, func, arg, opt = line:match"(%a+) ([^%s]+)(%b()) ?(.*)";
	
	assert(not functions[func], "duplicate function: " .. func);
	assert(primitives[ret] and primitives[ret] < 2, "unknown return type: " .. ret);
	
	local args, options = {}, {};
	local expr = false;
	
	for type in arg:sub(2,-2):gmatch"[^%s,]+" do
		expr = expr or 2 == assert(primitives[type], "unknown argument type: " .. type);
		table.insert(args, type);
	end
	
	for option in opt:gmatch"[^%s]+" do
		options[option] = true;
	end
	
	local type, op = func:match"(%a+)%.([gs]et)$";
	local scope = func:match"^global" or func:match"^local";
	local short = not scope and not expr and not options["!"] and func:match"[^%.]+$" or nil;
	local frmt = string.format("%s(%s)", options["!"] and func or short or "", string.rep(expr and "%s " or "%s, ", #args):sub(1, expr and -2 or -3));
	
	if scope then
		frmt = op == "get" and "%s" or "%s = %s";
	end

	functions[func] = {
		name = func,
		short = short,
		options = options,
		
		ret = ret,
		args = args,
		
		type = type or ret,
		scope = scope,
		frmt = frmt,
	};

	if short and short ~= func then
		assert(not functions[short], "duplicate short function: " .. short);
		functions[short] = functions[func];
	end
end

function encode(input)
	input = input:gsub("\r", "");
	local ret = {};

	local current_line = {num = 0, str = ""};
	local script;

	local print_tbl = {};
	local function print_real()
		-- print (table.concat(print_tbl, "\n"));
	end

	local function print(...)
		local out = {...};

		for k, v in ipairs (out) do
			out[k] = tostring(v);
		end

		table.insert(print_tbl, table.concat(out, "\t"));
	end
	
	local function debug(...)
		print (...);
	end

	local assert_old = assert;
	local function assert(test, msg)
		print_real();
		return assert_old(test, string.format("GSUB_HERE%s: %s\n\n%s", current_line.num, current_line.str, msg));
	end

	local function parse(raw)
		local ret = {raw = raw};
	
		if raw == "expr" then
			ret.type = "expr";
			ret.raw = "";
		elseif raw == "true" or raw == "false" then
			ret.type = "bool";
		elseif operators[raw] then
			ret.type = operators[raw];
		elseif tonumber(raw) then
			ret.type = math.type(tonumber(raw));
			ret.type = ret.type == "integer" and "int" or "double";
			ret.super = "number";
		elseif select(2, raw:gsub("string_(%d+)", function(a) a = tonumber(a); ret.raw = script.strings[a]; end)) ~= 0 then
			ret.type = "string";
		elseif script.variable[raw] then
			ret = script.variable[raw];
			ret.raw = raw;
		elseif functions[raw] then
			ret.type = "function";
			ret.func = functions[raw];
		elseif not script.label[raw] then
			assert(false, "failed to parse: " .. raw);
		end
		
		return ret;
	end

	local function compile_script()
		if not script then
			return;
		end

		for _, stage in ipairs {"preprocess", "process", "compile"} do
			debug("STAGE " .. stage);
			
			for section_n, section in ipairs {script.impulse, script.condition, script.action} do
				if #section == 0 then
					goto next_section;
				end
				
				debug(section.name);
				
				for line_n, line in ipairs (section) do
					current_line = line.line;
					
					if #line == 0 then
						goto next_line;
					end
					
					debug(line_n, line.line.num, line.line.str);
						
					if stage == "preprocess" then
						for k, item in ipairs (line) do
							if item.type == "variable" then
								local func = string.format("%s.%s.%s", item.scope, item.subtype, k == 1 and "set" or "get");
								line[k] = parse(func);
								
								if k == 1 then
									table.remove(line, k+1);
								end
								
								table.insert(line, k+1, {raw = item.raw, type = "string"});
							elseif item.type == "function" then
								for _, arg in ipairs (item.func.args) do
									if arg == "vector" then
										local x, y = line[k+1], line[k+2];
										
										if x.type == "double" and y.type == "double" then
											table.remove(line, k+1);
											table.remove(line, k+1);
											
											table.insert(line, k+1, {type = "vector", x = x.raw, y = y.raw, raw = string.format("%s, %s", x, y)});
										end
									end
								end
							end
						end
					elseif stage == "process" then
						local function iter(func, node)
							if not node then
								local tree;
								local nodes, expected = -1, 0;
								
								for k, item in ipairs (line) do
									local node = {
										depth = (tree and tree.depth or -1) + 1,
										parent = tree,
										item = item,
										key = k,
										arg = 0,
										nodes = {},
										expected = tree and tree.expects[#tree.nodes + 1],
									};
									print (string.format("%s %s", string.rep("  ", node.depth), node.item.raw));
									
									if tree then
										table.insert(tree.nodes, node);
										node.arg = #tree.nodes;
									end
									
									if item.type == "function" then
										node.expects = item.func.args;
										node.returns = item.func.ret;
										tree = node;
									elseif item.type == "expr" then
										node.expects = {"void", "void", "void"};
										node.returns = "void";
										tree = node;
									else
										node.expects = {};
										node.returns = item.subtype or item.type;							
									end
									
									nodes = nodes + 1;
									expected = expected + #node.expects;
									
									while tree.parent and #tree.nodes == #tree.expects do
										tree = tree.parent;
									end
								end
								
								assert(nodes == expected, string.format("wrong number of return values (%s expected, got %s)", expected, nodes));
								
								node = tree;
							end
							
							if func(node) then
								return true;
							end
							
							for _, node in ipairs (node.nodes) do
								if iter(func, node) then
									return true;
								end
							end
						end
						
						iter(function(node)
							if node.expected == "label" then
								local label = tostring(node.item.raw);
								line[node.key] = parse(assert(script.label[label], "unknown label: " .. label));
								-- returning from here would cause an infinite loop
							end
						end);
						
						repeat until not iter(function(node)
							-- print"DEBUG";
							-- print (string.format("%s %s", string.rep("  ", node.depth), node.item.raw));
						
							if node.item.type == "expr" then
								local arg, op = table.unpack(node.nodes);
								if not op then print ("DEBUG", line.debug); end
								op = op.item.type;
								
								local type = op == "op_comp" and arg.returns or node.expected;
								
								if type ~= "void" then
									line[node.key] = parse(string.format("%s.%s", op == "op_comp" and "comparison" or "arithmetic", type));
									return true;
								end
							elseif node.item.type == "function" and node.item.func.options.dynamic then
								local type = node.expected or "void";
								
								if type == "void" then
									for _, node in ipairs (node.nodes) do
										if node.returns ~= "void" then
											type = node.returns;
										end
									end
								end
								
								if type ~= "void" then
									line[node.key] = parse(string.format("%s.%s", type, node.item.func.name));
									return true;
								end
							elseif not script.option.strict then
								local a, b = node.expected, node.returns;
								
								if a and b and a ~= b and (a == "int" or a == "double") and (b == "int" or b == "double") then
									table.insert(line, node.key, parse(a == "int" and "d2i" or "i2d"));
									return true;
								end
							end
						end);
						
						iter(function(node)
							-- print (string.format("%s %s", string.rep("  ", node.depth), node.item.raw));
						end);
						
						iter(function(node)
							-- print (string.format("%s%s %s", string.rep("  ", node.depth), node.returns, node.item.raw));
							
							assert(node.depth == 0
								or node.expected == "label"
								or node.expected == node.returns,
								string.format("wrong argument #%s to %s (%s expected, got %s)", node.arg, (node.parent or node).item.raw, node.expected, node.returns)
							);
						end);
					elseif stage == "compile" then
						local function ins(frmt, val)
							table.insert(section.output, string.pack(frmt, val));
						end	
						
						for k, item in ipairs (line) do
							if item.func then
								assert((item.func.options.impulse == true) == (section == script.impulse), string.format("%s used in wrong section", item.func.name));
								
								if k == 1 then
									assert((item.func.ret == "bool") == (section == script.condition), "conditions must return a boolean");
									assert((item.func.ret == "void") ~= (section == script.condition), "actions cannot have a return value");
								end
							end
						
							if item.type == "function" then
								ins("s1", item.func.name);
							else
								ins("s1", "constant");
								
								if item.type == "bool" then
									ins("b", 1);
									ins("b", tonumber(val) == 1 and 1 or 0);
								elseif item.type == "int" then
									ins("b", 2);
									ins("i4", item.raw);
								elseif item.type == "double" then
									ins("b", 3);
									ins("d", item.raw);
								elseif item.type == "string" or item.type:match"^op_" then
									ins("b", 4);
									ins("s1", item.raw);
								elseif item.type == "vector" then
									ins("b", 5);
									ins("f", item.x);
									ins("f", item.y);
								else
									assert(false, "unknown compile type: " .. item.type);
								end
							end
						end
					end
					
					::next_line::
				end
				
				::next_section::
			end
		end
		
		local output = {string.pack("s1", script.name)};
		
		for _, tbl in ipairs {script.impulse, script.condition, script.action} do
			table.insert(output, string.pack("i4", tbl.lines or 0));
			
			for _, data in ipairs (tbl.output or {}) do
				table.insert(output, data);
			end
		end

		table.insert(ret, string.format("%s\n%s\n%s",
			script.name,
			string.format("%s %s %s", #script.impulse, #script.condition, #script.action),
			base64.encode(table.concat(output))
		));
	end
	
	for line in input:gmatch"[^\n]*" do
		line = line:gsub("^%s+", ""):gsub("%s+$", ""):gsub("##.*", "");
		current_line = {num = current_line.num + 1, str = line};

		local name = line:match"^script (.+)";

		if #line == 0 then
			-- next line
		elseif name then
			compile_script();
			
			script = {
				name = name,
				option = {},
				variable = {},
				strings = {},
				label = {},
				impulse = {},
				condition = {},
				action = {},
			};
		elseif assert(script, "first line must be 'script name'") then
			local output = {};
			
			if functions[line] and functions[line].options.section then
				script.section = script[line];
				assert(not script.section.defined, "already defined section: " .. line);
				
				script.section.defined = true;
				script.section.name = line;
				script.section.lines = 0;
				script.section.output = {};
			elseif script.section == script.option then
				line = line:lower();
				
				if not script_options[line] then
					local msg = {"invalid script option: " .. line, ""};
					
					for opt, desc in pairs (script_options) do
						table.insert(msg, string.format("%s: %s", opt, desc));
					end
					
					assert(false, table.concat(msg, "\n"));
				end
				
				script.section[line] = true;
			elseif script.section == script.variable then
				local scope, type, name = line:match"(%a+) (%a+) ([^%s()]+)";
				assert(scope, "variable definition expects: scope type name");

				name = name:lower();
				assert(scope == "global" or scope == "local", "variable scopes are 'global' and 'local'");
				assert(type == "int" or type == "double", "variable types are 'int' and 'double'");
				assert(not functions[name], "variable using reserved name: " .. name);
				
				script.section[name] = {name = name, scope = scope, type = "variable", subtype = type};
			elseif not script.section then
				local msg = {"define a section first", ""};
				local sorted = {};
				
				for _, func in pairs (functions) do
					if func.options.section then
						table.insert(sorted, func.name);
					end
				end
				
				table.sort(sorted);
				
				for _, v in ipairs (sorted) do
					table.insert(msg, v);
				end
				
				assert(false, table.concat(msg, "\n"));
			else
				script.section.lines = script.section.lines + 1;
				
				line = line
					:gsub("^(%w+):%s*", function(a)
						assert(script.section == script.action, "labels can only be defined in the 'actions' section");
						assert(not (script.variable[a] or functions[a]), "label using reserved name: " .. a);
						assert(not script.label[a], "label already exists: " .. a);
						
						script.label[a] = script.section.lines;
						return "";
					end)
					:gsub('%b""', function(a) table.insert(script.strings, a:sub(2,-2)); return "string_" .. #script.strings; end)
					:gsub("([%w_-]+) (..?) (.+)", function(a,b,c)
						-- ^ % += -= *= /= ^= %=
						if operators[b] == "op_special" and #b == 2 then
							return string.format("%s = (%s %s %s)", a, a, b:sub(1,1), c);
						end
					end)
					:gsub("[%^%%]", function(a) return a == "^" and "pow" or "mod"; end)
					:gsub("^%(", "expr(")
				;
				
				local count = 0;
				line:gsub("[()]", function(a)
					if a == "(" then
						count = count + 1;
					else
						count = count - 1;
					end
				end);
				assert(count == 0, string.format("missing %s %s parenthes%ss", math.abs(count), count < 0 and "opening" or "closing", math.abs(count) > 1 and "e" or "i"));
				
				local repl;
				
				repeat
					line, repl = line:gsub("([^%a])%(", "%1expr(");
				until repl == 0;

				local stack = {line = current_line, debug = line};
				
				line:gsub("([^%s(),]+)", function(raw)
					table.insert(stack, parse(raw));
				end);
				
				if #stack > 0 then
					table.insert(script.section, stack);
				end
			end
		end
	end

	compile_script();
	print_real();
	return table.concat(ret, "\n\n");
end

function decode(scripts)
	local ret = {};
	
	for code in scripts:gsub("\r", ""):gmatch"[^\n]+" do
		local txt = base64.decode(code);
		local pos = 1;
		local variables = {};
		
		local function ins(frmt, ...)
			table.insert(ret, select("#", ...) == 0 and tostring(frmt) or frmt:format(...));
		end
		
		local function read(frmt, text)
			local ret, new = string.unpack(frmt, txt, pos);
			pos = new;
			if text then ins(text, ret); end
			return ret;
		end
		
		local function parse()
			local name = read"s1";
			
			if name == "constant" then
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
					return string.format("%s, %s", read"f", read"f");
				else
					assert(false, "unknown constant type: " .. type);
				end
			else
				local func = assert(functions[name], "unknown function: " .. name);
				local args = {};
				
				for i = 1, #func.args do
					table.insert(args, parse());
					
					if func.args[i]:match"^op_" then
						args[i] = args[i]:gsub('["]', ""):gsub("&+", "&&"):gsub("|+", "||");
					end
				end
				
				if func.scope then
					local name = args[1]:sub(2,-2):lower();
					assert(not functions[name], "variable using reserved name: " .. name);
					args[1] = name;
					variables[name] = {scope = func.scope, type = func.type};
				end
				
				return func.frmt:format(table.unpack(args));
			end
		end
		
		ins("Code: %s", code);
		ins("Raw");
		ins(txt);
		read("s1", "script %s");
		
		local ret_vars = #ret + 1;

		for i = 1, read("i4", "\timpulse") do
			ins("\t\t%s", parse());
		end
		
		for i = 1, read("i4", "\tcondition") do
			ins("\t\t%s", parse());
		end
		
		for i = 1, read("i4", "\taction") do
			ins("\t\t%s: %s", i, parse());
		end
		
		for name, var in pairs (variables) do
			table.insert(ret, ret_vars, string.format("\t\t%s %s %s", var.scope, var.type, name));
		end
		table.insert(ret, ret_vars, "\tvariable");
		
		ins"";
	end
	
	return table.concat(ret, "\n");
end

UNIT_TEST = true;
decode'BHRlc3QAAAAAAQAAAA5jb21wYXJpc29uLmludBBtdXNldW0uZnJlZVNsb3RzCGNvbnN0YW50BAlpbnZlbnRvcnkIY29uc3RhbnQEAj09EW11c2V1bS5zdG9uZS50aWVyCGNvbnN0YW50BAlpbnZlbnRvcnkIY29uc3RhbnQCAAAAAAAAAAA=';
encode[[
script MINETABBER
	variable
		local double tab
		local int _mine
	impulse
		key.1
	condition
	action
		1: executesync("START_AUTO_TILERS")
		2: click(fromCoords(((59.0 * tab) + 358.0), 290.0))
		3: _mine = (_mine + 1)
		4: gotoif(12, (_mine >= 15))
		5: tab = (tab + 1.0)
		6: gotoif(10, (tab == 6.0))
		7: executesync("SIMPLE_MINER")
		8: click(606.0, 32.0)
		9: goto(2)
		10: tab = 0.0
		11: goto(2)
		12: click(28.0, 223.0)
		13: stop("AUTO_TILER")	
script mine-main
	variable
		global int miner-a
	impulse
		key.1
	action
		miner-a = 0
foo:	execute("mine-sub") 
		miner-a = (miner-a + 1)
		miner-a += 1
		gotoif(foo (miner-a < 16))
test:	newlayer()
		goto(test)
script mine-sub
	variable
		global int miner-a
		local int a
	action
		a = miner-a
	1:	dig((a / 4), (a % 4))
		goto(1)
script mine main
	variable
		global int mine-sub
	impulse
		key.2
	condition
	action
		1: mine-sub = 0
		2: execute("mine sub")
		3: mine-sub = (mine-sub + 1)
		4: gotoif(2, (mine-sub < 16))
		5: newlayer()
		6: wait(0.5)
		7: goto(5)
]];
UNIT_TEST = false;
