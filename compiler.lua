require"base64";

local function assert(test, message)
	if not test then
		error(message, 0);
	end
	
	return test;
end

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
	input = input or [[
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

script dig!
	variable
		local int a
	impulse
		key.1
	condition
	action
		a = a + 1 + 2 * 3 ^ 4 + 3 * 2 % 5 - 9
		
]];

	input = input:gsub("\n[ \t]+\n", "\n\n");
	local scripts = {};
	local write = {};
	
	for script in input:gmatch"(.-)\n\n+" do
		table.insert(scripts, script);
	end

	for script_num, script_txt in ipairs (scripts) do
		local output = {};
		
		local script = {
			name = nil,
			option = {},
			variable = {},
			strings = {},
			label = {},
			impulse = {},
			condition = {},
			action = {},
		};
		local section;
		
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
		
		script_txt = script_txt
			:gsub("\r", "")
			:gsub("##[^\n]*", "")
		;
		
		for line_raw in script_txt:gmatch"[^\n]+" do
			line_raw = line_raw:gsub("^%s+", ""):gsub("%s+$", "");
			local line = line_raw;
			
			if #line > 0 then
				if not script.name then
					script.name = assert(line:match"script (.+)", "first line must define script name");
				elseif functions[line] and functions[line].options.section then
					section = script[line];
					assert(not section.defined, "already defined section: " .. line);
					
					section.defined = true;
					section.line = 0;
					section.output = {};
				elseif section == script.option then
					line = line:lower();
					
					if not script_options[line] then
						print"Script Options";
						
						for opt, desc in pairs (script_options) do
							print(string.format("  %s: %s", opt, desc));
						end
						
						assert(false, "\ninvalid script option: " .. line);
					end
					
					section[line] = true;
				elseif section == script.variable then
					local scope, type, name = line:match"(%a+) (%a+) ([^%s()]+)";
					assert(not functions[name], "variable using reserved name: " .. name);
					
					section[name:lower()] = {name = name:lower(), scope = scope, type = "variable", subtype = type};
				elseif not section then
					print"Script sections";
					local sorted = {};
					
					for _, func in pairs (functions) do
						if func.options.section then
							table.insert(sorted, func.name);
						end
					end
					
					table.sort(sorted);
					
					for _, v in ipairs (sorted) do
						print ("  " .. v);
					end
					
					assert(false, "\ndefine a section first");
				else
					section.line = section.line + 1;
					
					line = line
						:gsub("^(%w+):%s*", function(a)
							assert(section == script.action, "labels can only be defined in the 'actions' section");
							assert(not (script.variable[a] or functions[a]), "label using reserved name: " .. a);
							assert(not script.label[a], "label already exists: " .. a);
							
							script.label[a] = section.line;
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
					assert(count == 0, string.format("\n%s\nmissing %s %s parenthes%ss\n", line, math.abs(count), count < 0 and "opening" or "closing", math.abs(count) > 1 and "e" or "i"));
					
					local repl;
					
					repeat
						line, repl = line:gsub("([^%a])%(", "%1expr(");
					until repl == 0;

					local stack = {line = line_raw, debug = line};
					
					line:gsub("([^%s(),]+)", function(raw)
						table.insert(stack, parse(raw));
					end);
					
					if #stack > 0 then
						table.insert(section, stack);
					end
				end
			end
		end
		
		for _, stage in ipairs {"preprocess", "process", "compile"} do
			-- print ("STAGE " .. stage);
			
			for section_n, section in ipairs {script.impulse, script.condition, script.action} do
				if #section == 0 then
					goto next_section;
				end
				
				-- print (section.name);
				
				for line_n, line in ipairs (section) do
					if #line == 0 then
						goto next_line;
					end
					
					-- print (line_n, line.line);
					
					local function err(text)
						return string.format("\n%s\n%s\n", line.line, text);
					end
						
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
										nodes = {},
										expected = tree and tree.expects[#tree.nodes + 1],
									};
									
									if tree then
										table.insert(tree.nodes, node);
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
								
								assert(nodes == expected, err(string.format("expected %s tokens, got %s", expected, nodes)));
								
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
								line[node.key] = parse(assert(script.label[label], err("unknown label: " .. label)));
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
								err(string.format("%s expected, got %s", node.expected, node.returns))
							);
						end);
					elseif stage == "compile" then
						local function ins(frmt, val)
							table.insert(section.output, string.pack(frmt, val));
						end	
						
						for k, item in ipairs (line) do
							if item.func then
								assert((item.func.options.impulse == true) == (section == script.impulse), err(string.format("function %s used in wrong section", item.func.name)));
								
								if k == 1 then
									assert((item.func.ret == "bool") == (section == script.condition), err"conditions must return a boolean");
									assert((item.func.ret == "void") ~= (section == script.condition), err"actions cannot have a return value");
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
									assert(false, err("unknown compile type: " .. item.type));
								end
							end
						end
					end
					
					::next_line::
				end
				
				::next_section::
			end
			
			-- print();
		end
		
		section = output;
		
		table.insert(output, string.pack("s1", script.name));
		
		for _, tbl in ipairs {script.impulse, script.condition, script.action} do
			table.insert(output, string.pack("i4", tbl.line or 0));
			
			for _, data in ipairs (tbl.output or {}) do
				table.insert(output, data);
			end
		end

		table.insert(write, script.name);
		table.insert(write, string.format("%s %s %s", #script.impulse, #script.condition, #script.action));
		table.insert(write, base64.encode(table.concat(output)));
		table.insert(write, "");
	end

	-- local f = assert(io.open("perfect-tower.txt", "w+b"));
	-- f:setvbuf"no";
	-- f:write(table.concat(write, "\n"));
	-- f:close();
end

function decode()
	local write = {};
	-- local scripts = [[
-- BHRlc3QCAAAACW9wZW4ubWluZQxvcGVuLmZhY3RvcnkAAAAAAAAAAA==
-- ]];
	
	for code in scripts:gsub("\r", ""):gmatch"[^\n]+" do
		local txt = base64.decode(code);
		local pos = 1;
		local variables = {};
		
		local function ins(frmt, ...)
			table.insert(write, select("#", ...) == 0 and tostring(frmt) or frmt:format(...));
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
		
		local write_vars = #write + 1;

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
			table.insert(write, write_vars, string.format("\t\t%s %s %s", var.scope, var.type, name));
		end
		table.insert(write, write_vars, "\tvariable");
		
		ins"";
	end
	
	f = assert(io.open("perfect-tower.txt", "w+b"));
	f:setvbuf"no";
	f:write(table.concat(write, "\n"));
	f:close();
end
