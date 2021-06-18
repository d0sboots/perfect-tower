FUNCTION_LIST = {};
FUNCTION = {};

local strings = {
	window = {"towertesting", "tradingpost", "powerplant", "factory", "laboratory", "shipyard", "workshop", "arcade", "museum", "headquarters", "constructionfirm", "statueofcubos", "mine"},

	item = {"block.dense", "plate.dense", "screw", "plate.rubber", "plate.circuit", "ring", "pipe", "wire", "circuit", "hammer"},
	craft = {"hammer", "motor", "chip", "cable.insulated", "block", "pump", "plate.stack", "lump", "producer.town", "producer.mine", "producer.powerplant", "producer.factory", "producer.workshop", "producer.constructionFirm", "producer.headquarters", "producer.laboratory", "producer.tradingpost", "producer.arcade", "producer.museum", "producer.shipyard", "producer.statueofcubos", "producer.gems", "producer.exoticgems", "machine.oven", "machine.presser", "machine.transportbelt", "machine.crusher", "machine.mixer", "machine.refinery", "machine.assembler", "machine.shaper", "machine.cutter", "machine.boiler"},
	produce = {"rubber", "ore", "dust", "ingot", "plate.stack", "rod", "plate", "cable", "lump", "block"},
	machine = {"oven", "assembler", "refinery", "crusher", "cutter", "presser", "mixer", "shaper", "boiler"},

	inventory = {"inventory", "equipped", "combinator", "cuboscube"},
	element = {"fire", "water", "earth", "air", "nature", "light", "darkness", "electricity"},
	elementMarket = {"fire", "water", "earth", "air", "nature", "light", "darkness", "electricity", "universal"},
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
	return (value >= min and value <= max), string.format("Range: %s - %s", min, max);
end

VALIDATOR = {
	["0-1"] = function(value) return rangeValid(value, 0.0, 1.0); end,
	scroll = function(value) local a, b = rangeValid(value, 0.0, 1.0); return value < 0.0 or a, b .. " (negative to ignore)"; end,

	window = function(value) return stringValid("window", value, "Windows"); end,

	sellx = function(value) return rangeValid(value, 0, 18); end,
	selly = function(value) return rangeValid(value, 0, 12); end,

	dig = function(value) return rangeValid(value, 0, 3); end,
	minetab = function(value) return rangeValid(value, 1, 12); end,

	tier = function(value) return rangeValid(value, 1, 10); end,
	item = function(value) return stringValid("item", value, "Items"); end,
	craft = function(value) return stringValid("craft", value, "Items"); end,
	produce = function(value) return stringValid("produce", value, "Items"); end,
	machine = function(value) return stringValid("machine", value, "Machines"); end,

	inv = function(value) return stringValid("inventory", value, "Inventories"); end,
	element = function(value) return stringValid("element", value, "Elements"); end,
	elementMarket = function(value) return stringValid("elementMarket", value, "Elements"); end,
};

local primitives = {void=1, impulse=1, bool=1, int=1, double=1, string=1, vector=1, op_set=2, op_comp=2, op_mod=2};

local functions = [[
impulse wakeup() Impulse
impulse key.<char>() {Impulse impulse key.#() 0-9, a-z}
impulse open.arcade() Impulse
impulse open.constructionFirm() Impulse
impulse open.factory() Impulse
impulse open.headquarters() Impulse
impulse open.laboratory() Impulse
impulse open.mine() Impulse
impulse open.museum() Impulse
impulse open.powerplant() Impulse
impulse open.shipyard() Impulse
impulse open.statueofcubos() Impulse
impulse open.tradingpost() Impulse
impulse open.workshop() Impulse
impulse game.newround() Impulse

int label(string)
void <scope>.<type>.set(string:variable, <type>) {Primitive void [g/l][i/d/s]s(string:variable, type:value)   ;set}
<type> <scope>.<type>.get(string:variable) {Primitive type [g/l][i/d/s]g(string:variable)   ;get}
bool constant.bool.get(string:variable)
void global.unset(string:variable) #gu# {Primitive void gu(string:variable)   ;global.unset}
void local.unset(string:variable) #lu# {Primitive void lu(string:variable)   ;local.unset}
bool comparison.<typeext>(<typeext>, op_comp, <typeext>) {Primitive bool c.[b/i/d/s](type:lhs, op_comp, type:rhs)   ;comparison}
<type> arithmetic.<type>(<type>, op_mod, <type>) {Primitive type a.[i/d/s](type:lhs, op_mod, type:rhs)   ;arithmetic}

bool string.contains(string:str, string:substr) String
int string.length(string) String #len#
int string.indexOf(string:str, string:substr, int:offset) String #index#
string concat(string:lhs, string:rhs) String
string substring(string, int:offset, int:length) String #sub#

double const.pi() Number

<num> <num>.min(<num>, <num>)
<num> <num>.max(<num>, <num>)
<num> <num>.rnd(<num>, <num>)

void min(void, void) {Number number min (a, b)}
void max(void, void) {Number number max (a, b)}
void rnd(void, void) {Number number rnd (min, max)}
double double.floor(double) Number
double double.ceil(double) Number
double double.round(double) Number

void if(bool, void, void) {Generic type if(bool, true, false)}
int ternary.int(bool, int, int)
double ternary.double(bool, double, double)
string ternary.string(bool, string, string)
vector ternary.vec2(bool, vector, vector) #ternary.vector#

int d2i(double) Conversion
double i2d(int) Conversion
string i2s(int) Conversion
string d2s(double) Conversion

double vec2.x(vector) Vector
double vec2.y(vector) Vector
vector vec.fromCoords(double:x, double:y) Vector #vec#
vector mouse.position() Vector

void generic.execute(string:script) Generic
void generic.executesync(string:script) Generic
void generic.stop(string:script) Generic
void generic.wait(double:seconds) Generic
void generic.waitwhile(bool) Generic
void generic.waituntil(bool) Generic
void generic.goto(int) Generic
void generic.gotoif(int, bool) Generic
void generic.click(vector) Generic
void generic.slider(vector:where, double:value[0-1]) Generic
void generic.scrollrect(vector:where, double:horizontal[scroll], double:vertical[scroll]) Generic #scrollbar#

int screen.width() Generic
int screen.height() Generic
double screen.width.d() Generic #width.d#
double screen.height.d() Generic #height.d#

double timestamp.now() Generic

bool town.window.isopen(string:window[window]) Town
void town.window.show(string:window[window], bool) Town

bool tower.stunned() Tower
int tower.buffs.negative() Tower
double tower.health(bool:percent) Tower
double tower.health.max() Tower #health.max#
double tower.health.regeneration() Tower #health.regen#
double tower.energy(bool:percent) Tower
double tower.energy.max() Tower #energy.max#
double tower.energy.regeneration() Tower #energy.regen#
double tower.shield(bool:percent) Tower
double tower.shield.max() Tower #shield.max#
double tower.module.cooldown(int:skill) Tower
void tower.module.useinstant(int:skill) Tower
void tower.restart() Tower

void powerplant.sell(int:x[sellx], int:y[selly]) Power Plant

bool mine.hasLayers() Mine
void mine.newlayer() Mine
void mine.dig(int:x[dig], int:y[dig]) Mine
void mine.tab(int[minetab]) Mine

bool factory.machine.active(string:machine[machine]) Factory
double factory.items.count(string:item[item], int:tier[tier]) Factory
void factory.craft(string:item[craft], int:tier[tier], double:amount) Factory
void factory.produce(string:item[produce], int:tier[tier], double:amount, string:machine[machine]) Factory
void factory.trash(string:item[item], int:tier[tier], double:amount) Factory

bool museum.isfill() Museum
int museum.freeSlots(string:inventory[inv]) Museum
int museum.stone.tier(string:inventory[inv], int:slot) Museum
string museum.stone.element(string:inventory[inv], int:slot) Museum
void museum.fill(bool:enable) Museum
void museum.buy(string:element[element]) Museum
void museum.buyMarket(string:element[elementMarket], int:tierMax) Museum
void museum.combine(int:tierMax) Museum
void museum.transmute() Museum
void museum.move(string:from[inv], int:slot, string:to[inv]) Museum
void museum.delete(string:inventory[inv], int:slot) Museum
void museum.clear(string:inventory[inv]) Museum

int tradingpost.offerCount() Trading Post
void tradingpost.refresh() Trading Post
void tradingpost.trade(int:offer, double:pct[0-1]) Trading Post

void clickrel(double:x[0-1], double:y[0-1]) Shortcut
]]

local function addList(category, display)
	if category and category ~= "" then
		FUNCTION_LIST[category] = FUNCTION_LIST[category] or {};
		table.insert(FUNCTION_LIST[category], display);
	end
end

local function parseFunction(line)
	local short;
	
	line = line:gsub("%b##", function(a)
		short = a:sub(2, -2);
		return "";
	end):gsub("(%a+)%.(%a+)%.(%a+)", function(a,b,c)
		if a == "global" or a == "local" then
			short = a:sub(1,1) .. b:sub(1,1) .. c:sub(1,1);
		end
	end):gsub("(%a+)%.(%a+)", function(a,b)
		if a == "arithmetic" or a == "comparison" then
			short = a:sub(1,1) .. "." .. b:sub(1,1);
		end
	end):gsub("%b{}", function(a)
		a = a:sub(2, -2);
		addList(a:match"(%a+) (.+)");
		return "";
	end):gsub("^%s+", ""):gsub("%s+$", "");

	local ret, name, arg, category = line:match"([^ ]+) (.-)(%b()) ?(.*)";
	local args, display = {}, {};

	if line:match"%b<>" == "<char>" then
		for char in string.gmatch("0123456789abcdefghijklmnopqrstuvwxyz", ".") do
			local new = line:gsub("%b<>", char);
			parseFunction(new);
		end

		return;
	elseif line:match"%b<>" then
		local done = {};
		
		for _, scope in ipairs {"global", "local", "constant"} do
			for _, typeext in ipairs {"bool", "int", "double", "string"} do
				for _, type in ipairs {"int", "double", "string"} do
					for _, num in ipairs {"int", "double"} do
						local tbl = {scope=scope, typeext=typeext, type=type, num=num};
						local new = line:gsub("%b<>", function(a) return tbl[a:sub(2,-2)]; end);
						
						if not done[new] then
							done[new] = true;
							parseFunction(new);
						end
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

	if not short and category ~= "Impulse" and category ~= "" then
		short = name:match"%.(%a+)$" or name;
	end
	
	short = short or name;

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

for _, category in ipairs {"Impulse", "Generic", "Town", "Tower", "Power Plant", "Mine", "Factory", "Museum", "Trading Post", "Primitive", "Number", "String", "Conversion", "Vector", "Shortcut"} do
	table.insert(functionList, string.format('<optgroup label="%s">', category));

	for _, func in ipairs (FUNCTION_LIST[category]) do
		table.insert(functionList, string.format("<option>%s</option>", func));
	end

	table.insert(functionList, "</optgroup>");
end

FUNCTION_LIST = table.concat(functionList, "");
