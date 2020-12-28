FUNCTION_LIST = {};
FUNCTION = {};

local strings = {
	window = {"newround", "tradingpost", "powerplant", "factory", "laboratory", "shipyard", "workshop", "arcade", "museum", "headquarters", "constructionfirm", "statueofcubos", "mine"},

	item = {"block.dense", "plate.dense", "screw", "plate.rubber", "plate.circuit", "ring", "pipe", "wire", "circuit"},
	craft = {"motor", "chip", "cable.insulated", "block", "pump", "plate.stack", "lump", "producer.town", "producer.mine", "producer.powerplant", "producer.factory", "producer.workshop", "producer.constructionFirm", "producer.headquarters", "producer.laboratory", "producer.tradingpost", "producer.arcade", "producer.museum", "producer.shipyard", "producer.statueofcubos", "producer.gems", "producer.exoticgems", "machine.oven", "machine.presser", "machine.transportbelt", "machine.crusher", "machine.mixer", "machine.refinery", "machine.assembler", "machine.shaper", "machine.cutter", "machine.boiler"},
	produce = {"rubber", "ore", "dust", "ingot", "plate.stack", "rod", "plate", "cable", "lump", "block"},
	machine = {"oven", "assembler", "refinery", "crusher", "cutter", "presser", "mixer", "shaper", "boiler"},

	inventory = {"inventory", "equipped", "combinator", "cuboscube"},
	element = {"fire", "water", "earth", "air", "nature", "light", "darkness", "electricity"},
};

for _, tbl in pairs (strings) do
	for _, val in ipairs (tbl) do
		if not tbl[val] then
			tbl[val] = true;
		end

		if tbl == strings.craft or tbl == strings.produce then
			if not strings.item[val] then
				strings.item[val] = true;
				table.insert(strings.item, val);
			end
		end
	end
end

local function stringValid(tbl, str, prefix)
	return strings[tbl][str], string.format("%s: %s", prefix, table.concat(strings[tbl], ", "));
end
local function rangeValid(value, min, max)
	return (value >= min and value <= max), string.format("Range: %s-%s", min, max);
end

VALIDATOR = {
	window = function(value) return stringValid("window", value, "Windows"); end,

	sellx = function(value) return rangeValid(value, 0, 18); end,
	selly = function(value) return rangeValid(value, 0, 12); end,

	dig = function(value) return rangeValid(value, 0, 3); end,

	tier = function(value) return rangeValid(value, 1, 10); end,
	item = function(value) return stringValid("item", value, "Items"); end,
	craft = function(value) return stringValid("craft", value, "Items"); end,
	produce = function(value) return stringValid("produce", value, "Items"); end,
	machine = function(value) return stringValid("machine", value, "Machines"); end,

	inv = function(value) return stringValid("inventory", value, "Inventories"); end,
	element = function(value) return stringValid("element", value, "Elements"); end,
};

local primitives = {void=1, impulse=1, bool=1, int=1, double=1, string=1, vector=1, label=2, op_set=2, op_comp=2, op_mod=2};

local functions = [[
impulse wakeup() Impulse
impulse key.0() Impulse
impulse key.1() Impulse
impulse key.2() Impulse
impulse key.3() Impulse
impulse key.4() Impulse
impulse key.5() Impulse
impulse key.6() Impulse
impulse key.7() Impulse
impulse key.8() Impulse
impulse key.9() Impulse
impulse open.mine() Impulse
impulse open.factory() Impulse
impulse open.workshop() Impulse
impulse open.powerplant() Impulse
impulse open.museum() Impulse

void <scope>.<type>.set(string:variable, <type>)
<type> <scope>.<type>.get(string:variable)
<type> arithmetic.<type>(<type>, op_mod, <type>)
bool comparison.<typeext>(<typeext>, op_comp, <typeext>)

string concat(string, string) Misc
double const.pi() Misc

<type> <type>.min(<type>, <type>)
<type> <type>.max(<type>, <type>)
<type> <type>.rnd(<type>, <type>)
<type> <type>.floor(<type>)
<type> <type>.ceil(<type>)
<type> <type>.round(<type>)

void rnd(void, void) {Misc number rnd (min, max)}
void min(void, void) {Misc number min (a, b)}
void max(void, void) {Misc number max (a, b)}
void floor(void, void) {Misc number floor (a)}
void ceil(void, void) {Misc number ceil (a)}
void round(void, void) {Misc number round (a)}

int d2i(double) Conversion
double i2d(int) Conversion
string i2s(int) Conversion
string d2s(double) Conversion

double vec2.x(vector) Vector
double vec2.y(vector) Vector
vector vec.fromCoords(double:x, double:y) Vector
vector mouse.position() Vector

void generic.execute(string:script) Generic
void generic.executesync(string:script) Generic
void generic.stop(string:script) Generic
void generic.wait(double:seconds) Generic
void generic.waitwhile(bool) Generic
void generic.waituntil(bool) Generic
void generic.goto(label) Generic
void generic.gotoif(label, bool) Generic
void generic.click(vector) Generic

int screen.width() Generic
int screen.height() Generic

bool town.window.isopen(string:window[window]) Town
void town.window.show(string:window[window], bool) Town

void tower.module.useinstant(int:skill) Tower

void powerplant.sell(int:x[sellx], int:y[selly]) Power Plant

void mine.newlayer() Mine
void mine.dig(int:x[dig], int:y[dig]) Mine

bool factory.machine.active(string:machine[machine]) Factory
double factory.items.count(string:item[item], int:tier[tier]) Factory
void factory.craft(string:item[craft], int:tier[tier], double:amount) Factory
void factory.produce(string:item[produce], int:tier[tier], double:amount, string:machine[machine]) Factory

bool museum.isfill() Museum
int museum.freeSlots(string:inventory[inv]) Museum
int museum.stone.tier(string:inventory[inv], int:slot) Museum
string museum.stone.element(string:inventory[inv], int:slot) Museum
void museum.fill(bool:enable) Museum
void museum.buy(string:element[element]) Museum
void museum.combine() Museum
void museum.transmute() Museum
void museum.move(string:from[inv], int:slot, string:to[inv]) Museum
void museum.delete(string:inventory[inv], int:slot) Museum
]]

local function addList(category, display)
	if category and category ~= "" then
		FUNCTION_LIST[category] = FUNCTION_LIST[category] or {};
		table.insert(FUNCTION_LIST[category], display);
	end
end

local function parseFunction(line)
	line = line:gsub("%b{}", function(a)
		a = a:sub(2, -2);
		addList(a:match"(%a+) (.+)");
		return "";
	end);

	local ret, name, arg, category = line:match"([^ ]+) (.-)(%b()) ?(.*)";
	local short, args, display = name, {}, {};

	if line:match"%b<>" then
		local done = {};
		
		for _, scope in ipairs {"global", "local"} do
			for _, typeext in ipairs {"bool", "int", "double", "string"} do
				for _, type in ipairs {"int", "double"} do
					local tbl = {scope=scope, typeext=typeext, type=type};
					local new = line:gsub("%b<>", function(a) return tbl[a:sub(2,-2)]; end);
					
					if not done[new] then
						done[new] = true;
						parseFunction(new);
					end
				end
			end
		end
		
		return;
	end
	
	assert(not FUNCTION[name], "duplicate function: " .. name);
	assert(primitives[ret] and primitives[ret] < 2, "unknown return type: " .. ret);
	
	for arg in arg:sub(2,-2):gmatch"[^%s,]+" do
		local validator;
		local type, name = arg:gsub("%b[]", function(a)
				a = a:sub(2,-2);
				validator = assert(VALIDATOR[a], "unknown validator: " .. a);
				return "";
			end)
			:match"([^:]+):?(.*)"
		;
		
		assert(primitives[type], "unknown argument type: " .. type);
		table.insert(args, {type = type, valid = validator});
		table.insert(display, name == "" and type or string.format("%s: %s", type, name));
	end

	if category ~= "Impulse" and category ~= "" then
		short = name:match"%.(%a+)$" or name;
	end

	if short == "fromCoords" then
		short = "vec";
	end

	FUNCTION[name] = {
		name = name,
		short = short,
		ret = ret,
		args = args,
	};

	if short ~= name then
		assert(not FUNCTION[short], "duplicate short function: " .. name);
		FUNCTION[short] = FUNCTION[name];
	end

	addList(category, string.format("%s%s (%s)", ret == "void" and "" or ret .. " ", short, table.concat(display, ", ")));
end

for line in functions:gsub("\r", ""):gmatch"[^\n]+" do
	parseFunction(line);
end

local functionList = {};

for _, category in ipairs {"Impulse", "Generic", "Town", "Tower", "Power Plant", "Mine", "Factory", "Museum", "Misc", "Conversion", "Vector"} do
	table.insert(functionList, string.format('<optgroup label="%s">', category));

	for _, func in ipairs (FUNCTION_LIST[category]) do
		table.insert(functionList, string.format("<option>%s</option>", func));
	end

	table.insert(functionList, "</optgroup>");
end

FUNCTION_LIST = table.concat(functionList, "");
