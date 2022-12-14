FUNCTION_LIST = {};
FUNCTION = {};

local strings = {
	window = {"towertesting", "tradingpost", "powerplant", "factory", "laboratory", "shipyard", "workshop", "arcade", "museum", "headquarters", "constructionfirm", "statueofcubos", "mine"},
	software = {"software.autoskip", "software.wavestreaming", "software.wavesurge", "software.criticalWavejump", "software.wavemomentum", "software.wavestorm", "software.wavepersistence", "software.waveinstability", "software.wavevortex", "software.wavecatalyst", "software.waveendurance", "software.newbounds", "software.wavemarathon", "software.wavecompression", "software.erasurge", "software.eraburst", "software.eraswirl", "software.wavehorizon", "software.nobounds", "software.wavefloor", "software.erafloor", "software.erahorizon", "software.waverestart", "software.infinityhorizon"},

	item = {"block.dense", "plate.dense", "screw", "plate.rubber", "plate.circuit", "ring", "pipe", "wire", "circuit", "hammer"},
	craft = {"hammer", "motor", "chip", "cable.insulated", "block", "pump", "plate.stack", "lump", "producer.town", "producer.mine", "producer.powerplant", "producer.factory", "producer.workshop", "producer.constructionFirm", "producer.headquarters", "producer.laboratory", "producer.tradingpost", "producer.arcade", "producer.museum", "producer.shipyard", "producer.statueofcubos", "producer.gems", "producer.exoticgems", "machine.oven", "machine.presser", "machine.transportbelt", "machine.crusher", "machine.mixer", "machine.refinery", "machine.assembler", "machine.shaper", "machine.cutter", "machine.boiler", "sapling.rubber"},
	produce = {"rubber", "ore", "dust", "ingot", "plate.stack", "rod", "plate", "cable", "lump", "block"},
	machine = {"oven", "assembler", "refinery", "crusher", "cutter", "presser", "mixer", "shaper", "boiler"},

	inventory = {"inventory", "loadout", "combinator", "cuboscube"},
	elementMarket = {"fire", "water", "earth", "air", "nature", "light", "darkness", "electricity", "universal"},
	elementAll = {"fire", "water", "earth", "air", "nature", "light", "darkness", "electricity", "universal", "neutral"},

	workerTask = {"task.towertesting.upgrade", "task.towertesting.upgradeEra", "task.arcade.playLuckyWheel", "task.arcade.playJumble", "shipyard.shipping", "task.museum.buycombine", "task.powerplant.replace", "task.powerplant.restart", "task.dyson.construct", "task.laboratory.prestige", "task.laboratory.nature.water", "task.laboratory.neutral.expand", "task.laboratory.water.freeze", "task.laboratory.darkness.search", "task.laboratory.gems.spin", "factory.oreManagement", "factory.refiningManagement", "factory.dissolveManagement", "task.mine.drill", "task.mine", "task.mine.asteroid", "task.claim.asteroid", "task.scan.asteroid", "task.construct"},

	region = {"forest", "desert", "winter", "underground", "volcano", "highmountain", "jungle", "metallicruins", "beach", "ocean", "neutral", "darkrealm", "heaven", "universe", "chaos"},
	difficulty = {"easy", "medium", "hard", "insane", "nightmare", "impossible"},

	eraDivider = {"damage", "health"},
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
	software = function(value) return stringValid("software", value, "Software"); end,

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
	elementMarket = function(value) return stringValid("elementMarket", value, "Elements"); end,
	elementAll = function(value) return stringValid("elementAll", value, "Elements"); end,

	workerGroup = function(value) return rangeValid(value, 0, 5); end,
	workerTask = function(value) return stringValid("workerTask", value, "Tasks"); end,

	region = function(value) return stringValid("region", value, "Regions"); end,
	difficulty = function(value) return stringValid("difficulty", value, "Difficulties"); end,

	eraDivider = function(value) return stringValid("eraDivider", value, "Era dividers"); end,
};

local primitives = {void=1, impulse=1, bool=1, int=1, double=1, string=1, vector=1, op_set=2, op_comp=2, op_mod=2};

local functions = [[
impulse wakeup() Impulse
impulse key.<char>() {Impulse impulse key.#()   ;0-9, a-z}
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
impulse close.arcade() Impulse
impulse close.constructionFirm() Impulse
impulse close.factory() Impulse
impulse close.headquarters() Impulse
impulse close.laboratory() Impulse
impulse close.mine() Impulse
impulse close.museum() Impulse
impulse close.powerplant() Impulse
impulse close.shipyard() Impulse
impulse close.statueofcubos() Impulse
impulse close.tradingpost() Impulse
impulse close.workshop() Impulse
impulse game.newround() Impulse

int label(string)
void <scope>.<typev>.set(string:variable, <typefull>) {Primitive void [g/l][b/i/d/s/v]s(string:variable, type:value)   ;set}
<typefull> <scope>.<typev>.get(string:variable) {Primitive type [g/l][b/i/d/s/v]g(string:variable)   ;get}
void global.unset(string:variable) #gu# {Primitive void gu(string:variable)   ;global.unset}
void local.unset(string:variable) #lu# {Primitive void lu(string:variable)   ;local.unset}
bool comparison.<typeext>(<typeext>, op_comp, <typeext>) {Primitive bool c.[b/i/d/s](type:lhs, op_comp, type:rhs)   ;comparison}
<type> arithmetic.<type>(<type>, op_mod, <type>) {Primitive type a.[i/d/s](type:lhs, op_mod, type:rhs)   ;arithmetic}

bool string.contains(string:str, string:substr) String
int string.length(string) String #len#
int string.indexOf(string:str, string:substr, int:offset) String #index#
string concat(string:lhs, string:rhs) String
string substring(string, int:offset, int:length) String #sub#
string string.lower(string) String
string string.upper(string) String

double const.pi() Number #const.pi#
double const.e() Number #const.e#

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
int s2i(string:input, int:failureDefault) Conversion
double i2d(int) Conversion
double s2d(string:input, double:failureDefault) Conversion
string i2s(int) Conversion
string d2s(double) Conversion

double vec2.x(vector) Vector
double vec2.y(vector) Vector
vector vec.fromCoords(double:x, double:y) Vector #vec#
vector mouse.position() Vector

string script.impulse() Generic

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
double option.ui.size() Generic #ui.size#

double timestamp.now() Generic
double time.delta() Generic #time.delta#
double time.unscaledDelta() Generic #time.unscaled#
double time.scale() Generic #time.scale#

bool town.window.isopen(string:window[window]) Town
bool town.window.anyopen() Town
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
double tower.attackrange() Tower #tower.range#
void tower.module.useinstant(int:skill) Tower
void tower.module.useposition(int:skill, vector:offset) Tower
void tower.restart() Tower
void tower.exit() Tower

bool game.isBossFight() Game
bool game.isTowerTesting() Game
int game.enemies.count() Game #enemies#
int game.module.active.index(string:moduleId) Game #active.index#
int game.module.active.count() Game #active.count#
double game.wave() Game
double game.era() Game
double game.infinity() Game
double game.waveAcceleration() Game
double player.xp() Game
double highscore.wave(string:region[region], string:difficulty[difficulty]) Game #highscore.wave#
double highscore.era(string:region[region], string:difficulty[difficulty]) Game #highscore.era#
double highscore.infinity(string:region[region], string:difficulty[difficulty]) Game #highscore.infinity#
double game.disable.era.cost(string:element[elementAll]) Game #disable.cost#
double game.module.secure.cost() Game #disable.inf.cost#
string game.module.active.id(int:index) Game #active.id#
void game.disable.era(string:element[elementAll]) Game #disable.era#
void game.upgrade.era(string:divider[eraDivider], int:numTimes) Game #upgrade.era#
void game.module.secure(string:moduleId) Game #disable.inf#

bool software.enabled(string:name[software]) Game #software.enabled#
string game.softwareid.find(string:name) Game #software.find#
void software.toggle(string:name[software], bool:on) Game #software.toggle#

bool worker.paused(string:name) Worker #worker.paused#
int worker.group.get(int:index) Worker #worker.group#
string worker.name.get(int:index) Worker #worker.name#
string worker.task(string:name) Worker #worker.task#
void workers.toggle.group(int:group[workerGroup]) Worker #worker.toggleGroup#
void workers.pause.group(int:group[workerGroup], bool:pause) Worker #worker.pauseGroup#
void workers.toggle.name(string:name) Worker #worker.toggleName#
void workers.pause.name(string:name, bool:pause) Worker #worker.pauseName#
void workers.assign.group(string:task[workerTask], int:subtask, int:group[workerGroup]) Worker #worker.assignGroup#
void workers.assign.name(string:task[workerTask], int:subtask, string:name) Worker #worker.assignName#
void worker.name.set(int:index, string:name) Worker #worker.setName#
void worker.group.set(int:index, int:group[workerGroup]) Worker #worker.setGroup#

void powerplant.sell(int:x[sellx], int:y[selly]) Power Plant

bool mine.hasLayers() Mine
int mine.clusters() Mine
void mine.newlayer() Mine
void mine.dig(int:x[dig], int:y[dig]) Mine
void mine.tab(int[minetab]) Mine
void mine.cluster.remove(int:cluster) Mine

bool arcade.luckywheel.isSpinning() Arcade #wheel.isSpinning#
bool arcade.jumble.isActive() Arcade #jumble.isActive#
void arcade.luckywheel.spin(double:wager) Arcade #wheel.spin#
void arcade.jumble.newGame(double:wager) Arcade #jumble.new#
void arcade.jumble.stop() Arcade #jumble.stop#

void arcade.adventure.move(vector:direction) Arcade #adventure.move#
void arcade.adventure.wait() Arcade #adventure.wait#
void arcade.adventure.placeBomb() Arcade #adventure.placeBomb#
vector arcade.adventure.roomCoords() Arcade #adventure.roomCoords#
vector arcade.adventure.playerPos() Arcade #adventure.playerPos#
int arcade.adventure.playerHealth() Arcade #adventure.playerHealth#
int arcade.adventure.playerArmor() Arcade #adventure.playerArmor#
int arcade.adventure.playerAttack() Arcade #adventure.playerAttack#
int arcade.adventure.bombs() Arcade #adventure.bombs#
int arcade.adventure.keys() Arcade #adventure.keys#
bool arcade.adventure.isWall(vector:position) Arcade #adventure.isWall#
bool arcade.adventure.isBomb(vector:position) Arcade #adventure.isBomb#
bool arcade.adventure.isEnemy(vector:position) Arcade #adventure.isEnemy#
bool arcade.adventure.isCompleted(vector:position) Arcade #adventure.isCompleted#
string arcade.adventure.entityType(vector:position) Arcade #adventure.entityType#

bool factory.machine.active(string:machine[machine]) Factory
double factory.items.count(string:item[item], int:tier[tier]) Factory
string factory.itemid.find(string:name) Factory #factory.find#
void factory.craft(string:item[craft], int:tier[tier], double:amount) Factory
void factory.produce(string:item[produce], int:tier[tier], double:amount, string:machine[machine]) Factory
void factory.trash(string:item[item], int:tier[tier], double:amount) Factory

bool museum.market.preference(string:element[elementMarket]) Museum #museum.preference#
bool museum.market.slotLocked(int:offerSlot) Museum #museum.isSlotLocked#
int museum.freeSlots(string:inventory[inv]) Museum
int museum.stone.tier(string:inventory[inv], int:slot) Museum
int museum.market.preferredTier() Museum #museum.preferredTier#
int museum.market.maxTier(string:element[elementMarket]) Museum #museum.maxTier#
int museum.market.slotTier(int:offerSlot) Museum #museum.slotTier#
int museum.rebuy.tier(int:trashSlot) Museum #museum.trashTier#
double museum.market.timer() Museum #museum.timer#
string museum.stone.element(string:inventory[inv], int:slot) Museum
string museum.market.slotElement(int:offerSlot) Museum #museum.slotElement#
string museum.rebuy.element(int:trashSlot) Museum #museum.trashElement#
void museum.combine(int:tierMax) Museum
void museum.transmute() Museum
void museum.move(string:from[inv], int:slot, string:to[inv]) Museum
void museum.delete(string:inventory[inv], int:slot) Museum
void museum.clear(string:inventory[inv]) Museum
void museum.stone.buy(string:element[elementMarket], int:tier, int:quantity) Museum #museum.buyTier#
void museum.stone.buyRange(string:element[elementMarket], int:tierMin, int:tierMax, int:quantity) Museum #museum.buyRange#
void museum.moveSlot(string:from[inv], int:fromSlot, string:to[inv] int:toSlot) Museum #museum.moveTo#
void museum.swap(string:invA[inv], int:slotA, string:invB[inv] int:slotB) Museum #museum.swap#
void museum.market.set.preferredTier(int:tier) Museum #museum.setPreferredTier#
void museum.market.set.preference(string:element[elementMarket], bool) Museum #museum.setPreference#
void museum.market.refresh() Museum #museum.refresh#
void museum.market.buy(int:offerSlot, int:quantity) Museum #museum.buyOffer#
void museum.market.set.slotLocked(int:offerSlot, bool:locked) Museum #museum.setSlotLocked#
void museum.rebuy.buy(int:trashSlot) Museum #museum.rebuy#

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
	end):gsub("(%a+)%.(%w+)%.(%a+)", function(a,b,c)
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
		
		for _, scope in ipairs {"global", "local"} do
			for _, typefull in ipairs {"bool", "int", "double", "string", "vector"} do
				local typev = ({bool="bool", int="int", double="double", string="string", vector="vec2"})[typefull];
				local typeext = ({bool="bool", int="int", double="double", string="string"})[typefull];
				local type = ({int="int", double="double", string="string"})[typefull];
				local num = ({int="int", double="double"})[typefull];

				local tbl = {scope=scope, typefull=typefull, typev=typev, typeext=typeext, type=type, num=num};
				local bad = false;
				local new = line:gsub("%b<>", function(a)
					local x = tbl[a:sub(2,-2)];
					bad = bad or not x;
					return x;
				end);

				if not bad and not done[new] then
					done[new] = true;
					parseFunction(new);
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

	addList(category, string.format("%s%s(%s)", ret == "void" and "" or ret .. " ", short, table.concat(display, ", ")));
end

for line in functions:gsub("\r", ""):gmatch"[^\n]+" do
	parseFunction(line);
end

local functionList = {};

for _, category in ipairs {"Impulse", "Generic", "Town", "Tower", "Game", "Worker", "Power Plant", "Mine", "Arcade", "Factory", "Museum", "Trading Post", "Primitive", "Number", "String", "Conversion", "Vector", "Shortcut"} do
	table.insert(functionList, string.format('<optgroup label="%s">', category));

	for _, func in ipairs (FUNCTION_LIST[category]) do
		table.insert(functionList, string.format("<option>%s</option>", func));
	end

	table.insert(functionList, "</optgroup>");
end

FUNCTION_LIST = table.concat(functionList, "");
