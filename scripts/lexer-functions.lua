FUNCTION_LIST = {};
FUNCTION = {};

local strings = {
	window = {"towertesting", "tradingpost", "powerplant", "factory", "laboratory", "shipyard", "workshop", "arcade", "museum", "headquarters", "constructionfirm", "statueofcubos", "mine"},
	software = {"software.autoskip", "software.wavestreaming", "software.wavesurge", "software.criticalWavejump", "software.wavemomentum", "software.wavestorm", "software.wavepersistence", "software.waveinstability", "software.wavevortex", "software.wavecatalyst", "software.waveendurance", "software.newbounds", "software.wavemarathon", "software.wavecompression", "software.erasurge", "software.eraburst", "software.eraswirl", "software.wavehorizon", "software.nobounds","software.eratunneling", "software.wavebreach", "software.wavefloor", "software.erafloor", "software.erahorizon", "software.waverestart", "software.infinityhorizon"},

-- item list only needs to include uncraftable items not used to produce
	item = {"plate.rubber", "plate.rainbow", "essence.void", "circuit", "wire", "screw", "pipe", "ring", "block.dense", "plate.dense", "plate.circuit", "pumpkin.plate", "pumpkin.carved", "pumpkin.anti"};
	craft = {"chip.basic", "chip.advanced", "chip.hitech", "chip.nano", "chip.quantum", "chip", "hammer", "sapling.rubber", "sapling.void", "dust.rainbow", "cable.insulated", "plate", "motor", "pump", "block", "plate.stack", "lump", "producer.town", "producer.statueofcubos", "producer.workshop", "producer.shipyard", "producer.laboratory", "producer.constructionFirm", "producer.mine", "producer.powerplant", "producer.arcade", "producer.headquarters", "producer.tradingpost", "producer.museum", "producer.factory", "producer.exoticgems", "producer.gems", "booster.acceleration", "booster.machines", "booster.production.regular", "booster.resource.drops", "booster.trees", "pumpkin.stack", "pumpkin.producer", "machine.oven", "machine.assembler", "machine.refinery", "machine.crusher", "machine.cutter", "machine.presser", "machine.mixer", "machine.transportbelt", "machine.shaper", "machine.boiler"},
	produce = {"rubber", "dust.rainbow", "ingot.rainbow", "cable", "ore", "dust", "ingot", "plate", "block", "plate.stack", "rod", "lump", "pumpkin", "pumpkin.stack"},
	machine = {"oven", "assembler", "refinery", "crusher", "cutter", "presser", "mixer", "shaper", "boiler"},

	inventory = {"inventory", "loadout", "combinator", "cuboscube"},
	elementMarket = {"fire", "water", "earth", "air", "nature", "light", "darkness", "electricity", "universal"},
	elementAll = {"fire", "water", "earth", "air", "nature", "light", "darkness", "electricity", "universal", "neutral"},

	workerTask = {"shipyard.shipping", "task.powerplant.replace", "task.powerplant.restart", "task.dyson.construct", "task.towertesting.upgrade", "task.towertesting.upgradeEra", "task.arcade.playLuckyWheel", "task.arcade.playJumble", "task.museum.buycombine", "task.museum.buyoffshore", "task.workshop.era.research", "task.workshop.era.experiment", "task.workshop.era.essence", "task.laboratory.prestige", "task.laboratory.nature.water", "task.laboratory.neutral.expand", "task.laboratory.water.freeze", "task.laboratory.darkness.search", "task.laboratory.gems.spin", "task.laboratory.gems.stack", "task.laboratory.exotic.gift", "task.construct", "task.mine.drill", "task.mine", "task.mine.asteroid", "task.claim.asteroid", "task.scan.asteroid", "task.tradingpost.trading", "factory.oreManagement", "factory.upTiering", "factory.downTiering", "factory.refiningManagement", "factory.dissolveManagement", "factory.treeHarvesting"},

	region = {"forest", "desert", "winter", "underground", "volcano", "highmountain", "jungle", "metallicruins", "beach", "ocean", "neutral", "darkrealm", "heaven", "universe", "chaos"},
	difficulty = {"easy", "medium", "hard", "insane", "nightmare", "impossible"},

	eraDivider = {"damage", "health"},

	keyItem = {"map", "emeraldMap", "compass", "ultraKey", "lantern", "eodArmor", "thornsArmor", "bootsHaste", "bootsPhasing", "leechSword", "impaler", "manaReaver", "hammer", "holyBomb", "market", "bookSpells", "bag", "masterSword", "masterArmor"},
	marketItem = {"eodArmor", "thornsArmor", "bootsPhasing", "leechSword", "impaler", "manaReaver", "hammer", "holyBomb", "bookSpells"},
	entityType = {"Chest", "Bomb", "Rock", "Door", "Enemy", "Elite", "Mimic"},
	spell = {"identifyRoom", "manaArmor"},
	currency = {"gameTokens", "town.resources", "gems", "gems.exotic", "powerplant.resources", "mine.resources", "factory.resources", "headquarters.resources", "arcade.resources", "laboratory.resources", "shipyard.resources", "tradingpost.resources", "workshop.resources", "museum.resources", "constructionFirm.resources", "statueofcubos.resources", "halloween.pumpkins", "halloween.souls", "halloween.blood", "christmas.cookies", "christmas.presents", "christmas.reindeers.trained", "christmas.reindeers.milk", "christmas.reindeers.raw", "christmas.milk", "christmas.trees", "christmas.wrappings", "christmas.toys", "christmas.candy", "time.offline"},
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

	keyItem = function(value) return stringValid("keyItem", value, "Key Items/Relics"); end,
	marketItem = function(value) return stringValid("marketItem", value, "Market Items"); end,
	entityType = function(value) return stringValid("entityType", value, "Adventure Entity Types"); end,
	spell = function(value) return stringValid("spell", value, "Spells"); end,
	currency = function(value) return stringValid("currency", value, "Resource Types"); end,
};

local primitives = {void=1, impulse=1, bool=1, int=1, double=1, string=1, vector=1, op_set=2, op_comp=2, op_mod=2};

local functions = [[
impulse wakeup() Impulse
impulse key.<char>() {Impulse impulse key.#()   ;0-9, a-z}
impulse mouse.0.down() Impulse
impulse mouse.1.down() Impulse
impulse mouse.2.down() Impulse
impulse mouse.0.up() Impulse
impulse mouse.1.up() Impulse
impulse mouse.2.up() Impulse
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
<typefull> arithmetic.<numv>(<typefull>, op_mod, <typefull>) {Primitive type a.[d/s/v](type:lhs, op_mod, type:rhs)   ;arithmetic}

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
double double.sin(double:radians) Number
double double.cos(double:radians) Number
double double.tan(double:radians) Number
double double.asin(double) Number
double double.acos(double) Number
double double.atan(double) Number
double double.atan2(vector) Number #atan2#

bool not(bool) Generic
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

bool mouse.0.state() Generic #mouse.0.state#
bool mouse.1.state() Generic #mouse.1.state#
bool mouse.2.state() Generic #mouse.2.state#
string script.impulse() Generic

void generic.execute(string:script) Generic
void generic.executesync(string:script) Generic
void generic.stop(string:script) Generic
void generic.wait(double:seconds) Generic
void generic.waitwhile(bool) Generic
void generic.waituntil(bool) Generic
void generic.waitframe() Generic
void generic.goto(int) Generic
void generic.gotoif(int, bool) Generic
void generic.click(vector) Generic
void generic.slider(vector:where, double:value[0-1]) Generic
void generic.scrollrect(vector:where, double:horizontal[scroll], double:vertical[scroll]) Generic #scrollbar#

int generic.budget() Generic
int screen.width() Generic
int screen.height() Generic
double screen.width.d() Generic #width.d#
double screen.height.d() Generic #height.d#
double option.ui.size() Generic #ui.size#

int time.frame() Generic #time.frame#
double timestamp.now() Generic
double timestamp.utcnow() Generic
double time.delta() Generic #time.delta#
double time.unscaledDelta() Generic #time.unscaled#
double time.scale() Generic #time.scale#

void canvas.draw.rect(vector:pos, vector:size, string:rgb_or_rgba) UI #canvas.rect#
void canvas.clear() UI #canvas.clear#
void window.create(string:windowId, string:windowType) UI
void window.destroy(string:windowId) UI
void window.destroy.all() UI #destroy.all#
void window.text.set(string:windowId, string:textElementId, string:value) UI #text.set#
void window.visibility.set(string:windowId, bool:isVisible) UI #visibility.set#
void window.child.visibility.set(string:windowId, string:elementId, bool:isVisible) UI #child.visibility.set#
void window.position.set(string:windowId, vector:position) UI #position.set#
void window.sprite.set(string:windowId, string:elementId, string:spriteId) UI #sprite.set#
bool window.visibility.get(string:windowId) UI #visibility.get#
bool window.child.visibility.get(string:windowId, string:elementId) UI #child.visibility.get#

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
bool game.pause.get() Game #pause.get#
int game.enemies.count() Game #enemies#
int game.module.active.index(string:moduleId) Game #active.index#
int game.module.active.count() Game #active.count#
double game.resource(string:currency[currency]) Game
double game.wave() Game
double game.era() Game
double game.infinity() Game
double game.waveAcceleration() Game
double game.fixedWavesPerInterval() Game
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
void game.pause.set(bool:paused) Game #pause.set#
void game.pause() Game
void game.unpause() Game

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
void arcade.adventure.buyMarketItem(string:item[marketItem]) Arcade #adventure.buyMarketItem#
void arcade.adventure.teleport(vector:roomPos) Arcade #adventure.teleport#
void arcade.adventure.useSpell(string:spell[spell]) Arcade #adventure.useSpell#
vector arcade.adventure.roomCoords() Arcade #adventure.roomCoords#
vector arcade.adventure.playerPos() Arcade #adventure.playerPos#
int arcade.adventure.playerHealth() Arcade #adventure.playerHealth#
int arcade.adventure.playerArmor() Arcade #adventure.playerArmor#
int arcade.adventure.playerAttack() Arcade #adventure.playerAttack#
int arcade.adventure.bombs() Arcade #adventure.bombs#
int arcade.adventure.countEntities(string:type[entityType]) Arcade #adventure.countEntities#
int arcade.adventure.emerald() Arcade #adventure.emeralds#
int arcade.adventure.goldenHeart() Arcade #adventure.goldenHearts#
int arcade.adventure.keys() Arcade #adventure.keys#
int arcade.adventure.mana() Arcade #adventure.mana#
int arcade.adventure.manaArmor() Arcade #adventure.manaArmor#
bool arcade.adventure.hasPhoenixFeather() Arcade
bool arcade.adventure.hasItem(string:item[keyItem]) Arcade #adventure.hasItem#
bool arcade.adventure.isWall(vector:position) Arcade #adventure.isWall#
bool arcade.adventure.isBomb(vector:position) Arcade #adventure.isBomb#
bool arcade.adventure.isEnemy(vector:position) Arcade #adventure.isEnemy#
bool arcade.adventure.isCompleted(vector:position) Arcade #adventure.isCompleted#
string arcade.adventure.entityType(vector:position) Arcade #adventure.entityType#

bool factory.machine.active(string:machine[machine]) Factory
double factory.items.count(string:item[item], int:tier[tier]) Factory
double factory.machine.item.count(string:machine[machine]) Factory #machine.item.count#
string factory.machine.item(string:machine[machine]) Factory #machine.item#
string factory.itemid.find(string:name) Factory #factory.find#
void factory.craft(string:item[craft], int:tier[tier], double:amount) Factory
void factory.produce(string:item[produce], int:tier[tier], double:amount, string:machine[machine]) Factory
void factory.trash(string:item[item], int:tier[tier], double:amount) Factory
void factory.machine.cancel(string:machine[machine]) Factory

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

void bogus() Macros {Macros any {lua(lua_code)}}
void bogus() Macros {Macros int {len(any_characters)}}
void bogus() Macros {Macros vector {pos.relative(double:x_pos[0-1], double:y_pos[0-1], double:x_anchor[0-1], double:y_anchor[0-1])}}
void bogus() Macros {Macros void {click.relative(double:x_pos[0-1], double:y_pos[0-1], double:x_anchor[0-1], double:y_anchor[0-1])}}
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

	if category == "Macros" then return end;

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
				local numv = ({int="int", double="double", vector="vec2"})[typefull];

				local tbl = {scope=scope, typefull=typefull, typev=typev, typeext=typeext, type=type, num=num, numv=numv};
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
		assert(not FUNCTION[short], "duplicate short function: " .. short);
		FUNCTION[short] = FUNCTION[name];
	end

	addList(category, string.format("%s%s(%s)", ret == "void" and "" or ret .. " ", short, table.concat(display, ", ")));
end

for line in functions:gsub("\r", ""):gmatch"[^\n]+" do
	parseFunction(line);
end

local functionList = {};

for _, category in ipairs {"Impulse", "Generic", "UI", "Town", "Tower", "Game", "Worker", "Power Plant", "Mine", "Arcade", "Factory", "Museum", "Trading Post", "Primitive", "Number", "String", "Conversion", "Vector", "Macros"} do
	table.insert(functionList, string.format('<optgroup label="%s">', category));

	for _, func in ipairs (FUNCTION_LIST[category]) do
		table.insert(functionList, string.format("<option>%s</option>", func));
	end

	table.insert(functionList, "</optgroup>");
end

FUNCTION_LIST = table.concat(functionList, "");
