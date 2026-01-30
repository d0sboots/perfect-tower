FUNCTION_LIST = {}
FUNCTION = {}

local strings = {
  window = {"towertesting", "tradingpost", "powerplant", "factory", "laboratory", "shipyard", "workshop", "arcade", "museum", "headquarters", "constructionfirm", "statueofcubos", "mine"},
  software = {"software.autoskip", "software.wavestreaming", "software.wavesurge", "software.criticalWavejump", "software.wavemomentum", "software.wavestorm", "software.wavepersistence", "software.waveinstability", "software.wavevortex", "software.wavecatalyst", "software.waveendurance", "software.newbounds", "software.wavemarathon", "software.wavecompression", "software.erasurge", "software.eraburst", "software.eraswirl", "software.wavehorizon", "software.nobounds","software.eratunneling", "software.wavebreach", "software.wavefloor", "software.erafloor", "software.erahorizon", "software.waverestart", "software.infinityhorizon"},

-- item list only needs to include uncraftable items not used to produce
  item = {"plate.rubber", "plate.rainbow", "essence.void", "circuit", "wire", "screw", "pipe", "ring", "block.dense", "plate.dense", "plate.circuit", "pumpkin.plate", "pumpkin.carved", "pumpkin.anti"},
  craft = {"chip.basic", "chip.advanced", "chip.hitech", "chip.nano", "chip.quantum", "chip", "hammer", "sapling.rubber", "sapling.void", "dust.rainbow", "cable.insulated", "plate", "motor", "pump", "block", "plate.stack", "lump", "producer.town", "producer.statueofcubos", "producer.workshop", "producer.shipyard", "producer.laboratory", "producer.constructionFirm", "producer.mine", "producer.powerplant", "producer.arcade", "producer.headquarters", "producer.tradingpost", "producer.museum", "producer.factory", "producer.exoticgems", "producer.gems", "booster.acceleration", "booster.machines", "booster.production.regular", "booster.resource.drops", "booster.trees", "pumpkin.stack", "pumpkin.producer", "machine.oven", "machine.assembler", "machine.refinery", "machine.crusher", "machine.cutter", "machine.presser", "machine.mixer", "machine.transportbelt", "machine.shaper", "machine.boiler"},
  produce = {"rubber", "dust.rainbow", "ingot.rainbow", "cable", "ore", "dust", "ingot", "plate", "block", "plate.stack", "rod", "lump", "pumpkin", "pumpkin.stack"},
  machine = {"oven", "assembler", "refinery", "crusher", "cutter", "presser", "mixer", "shaper", "boiler"},

  inventory = {"inventory", "loadout", "combinator", "cuboscube"},
  elementMarket = {"fire", "water", "earth", "air", "nature", "light", "darkness", "electricity", "universal"},
  elementAll = {"fire", "water", "earth", "air", "nature", "light", "darkness", "electricity", "universal", "neutral"},

  workerTask = {"shipyard.shipping", "task.powerplant.replace", "task.powerplant.restart", "task.dyson.construct", "task.towertesting.upgrade", "task.towertesting.upgradeEra", "task.arcade.playLuckyWheel", "task.arcade.playJumble", "task.museum.buycombine", "task.museum.buyoffshore", "task.workshop.era.research", "task.workshop.era.experiment", "task.workshop.era.essence", "task.laboratory.prestige", "task.laboratory.nature.water", "task.laboratory.neutral.expand", "task.laboratory.water.freeze", "task.laboratory.darkness.search", "task.laboratory.gems.spin", "task.laboratory.gems.stack", "task.laboratory.exotic.gift", "task.construct", "task.construct.gems", "task.construct.exotics", "task.mine.drill", "task.mine", "task.mine.asteroid", "task.claim.asteroid", "task.scan.asteroid", "task.tradingpost.trading", "factory.oreManagement", "factory.upTiering", "factory.downTiering", "factory.refiningManagement", "factory.dissolveManagement", "factory.treeHarvesting"},

  region = {"forest", "desert", "winter", "underground", "volcano", "highmountain", "jungle", "metallicruins", "beach", "ocean", "neutral", "darkrealm", "heaven", "universe", "chaos"},
  difficulty = {"easy", "medium", "hard", "insane", "nightmare", "impossible"},

  eraDivider = {"damage", "health"},

  keyItem = {"map", "emeraldMap", "compass", "ultraKey", "lantern", "eodArmor", "thornsArmor", "bootsHaste", "bootsPhasing", "leechSword", "impaler", "manaReaver", "hammer", "holyBomb", "market", "bookSpells", "bag", "masterSword", "masterArmor"},
  marketItem = {"eodArmor", "thornsArmor", "bootsPhasing", "leechSword", "impaler", "manaReaver", "hammer", "holyBomb", "bookSpells"},
  entityType = {"Chest", "Bomb", "Rock", "Door", "Enemy", "Elite", "Mimic"},
  spell = {"identifyRoom", "manaArmor"},
  currency = {"gameTokens", "town.resources", "gems", "gems.exotic", "powerplant.resources", "mine.resources", "factory.resources", "headquarters.resources", "arcade.resources", "laboratory.resources", "shipyard.resources", "tradingpost.resources", "workshop.resources", "museum.resources", "constructionFirm.resources", "statueofcubos.resources", "halloween.pumpkins", "halloween.souls", "halloween.blood", "christmas.cookies", "christmas.presents", "christmas.reindeers.trained", "christmas.reindeers.milk", "christmas.reindeers.raw", "christmas.milk", "christmas.trees", "christmas.wrappings", "christmas.toys", "christmas.candy", "time.offline", "crates"},
}

for _, tbl in pairs (strings) do
  for _, val in ipairs (tbl) do
    if not tbl[val] then
      tbl[val] = true
    end

    if tbl == strings.craft or tbl == strings.produce then
      if not strings.item[val] then
        strings.item[val] = true
        table.insert(strings.item, val)
      end
    end
  end
end

local function stringValid(tbl, str, prefix)
  if strings[tbl][str] then
    return true end
  return false, string.format("%s: %s", prefix, table.concat(strings[tbl], ", "))
end
local function rangeValid(value, min, max)
  if value >= min and value <= max then
    return true, "" end
  return false, string.format("Range: %s - %s", min, max)
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
}

local primitives = {void=1, impulse=1, bool=1, int=1, double=1, string=1, vector=1, op_comp=2, op_mod=2}

local functions = {
  Hidden={
    {ret="<>", name="ternary.<>", args={"bool", "<>", "<>"}, short="ternary.<>", expand="idsv"},
    {ret="int", name="label", args={"string"}},
    {ret="void", name="min", args={"void", "void"}},
    {ret="void", name="max", args={"void", "void"}},
    {ret="void", name="rnd", args={"void", "void"}},
  },
  Impulse={
    {ret="impulse", name="wakeup", args={}},
    {ret="impulse", name="key.<>", args={}, display="impulse key.#   ;0-9, a-z", expand="<char>"},
    {ret="impulse", name="mouse.0.down", args={}},
    {ret="impulse", name="mouse.1.down", args={}},
    {ret="impulse", name="mouse.2.down", args={}},
    {ret="impulse", name="mouse.0.up", args={}},
    {ret="impulse", name="mouse.1.up", args={}},
    {ret="impulse", name="mouse.2.up", args={}},
    {ret="impulse", name="open.arcade", args={}},
    {ret="impulse", name="open.constructionFirm", args={}},
    {ret="impulse", name="open.factory", args={}},
    {ret="impulse", name="open.headquarters", args={}},
    {ret="impulse", name="open.laboratory", args={}},
    {ret="impulse", name="open.mine", args={}},
    {ret="impulse", name="open.museum", args={}},
    {ret="impulse", name="open.powerplant", args={}},
    {ret="impulse", name="open.shipyard", args={}},
    {ret="impulse", name="open.statueofcubos", args={}},
    {ret="impulse", name="open.towertesting", args={}},
    {ret="impulse", name="open.tradingpost", args={}},
    {ret="impulse", name="open.workshop", args={}},
    {ret="impulse", name="close.arcade", args={}},
    {ret="impulse", name="close.constructionFirm", args={}},
    {ret="impulse", name="close.factory", args={}},
    {ret="impulse", name="close.headquarters", args={}},
    {ret="impulse", name="close.laboratory", args={}},
    {ret="impulse", name="close.mine", args={}},
    {ret="impulse", name="close.museum", args={}},
    {ret="impulse", name="close.powerplant", args={}},
    {ret="impulse", name="close.shipyard", args={}},
    {ret="impulse", name="close.statueofcubos", args={}},
    {ret="impulse", name="close.towertesting", args={}},
    {ret="impulse", name="close.tradingpost", args={}},
    {ret="impulse", name="close.workshop", args={}},
    {ret="impulse", name="game.newround", args={}},
  },
  Generic={
    {ret="bool", name="not", args={"bool"}},
    {ret="void", name="if", args={"bool", "void", "void"}, display="type if(bool:cond, true_expr, false_expr)"},
    {ret="bool", name="mouse.0.state", args={}, short="mouse.0.state"},
    {ret="bool", name="mouse.1.state", args={}, short="mouse.1.state"},
    {ret="bool", name="mouse.2.state", args={}, short="mouse.2.state"},
    {ret="string", name="script.impulse", args={}},
    {ret="void", name="generic.execute", args={"string:script"}},
    {ret="void", name="generic.executesync", args={"string:script"}},
    {ret="void", name="generic.stop", args={"string:script"}},
    {ret="void", name="generic.wait", args={"double:seconds"}},
    {ret="void", name="generic.waitwhile", args={"bool"}},
    {ret="void", name="generic.waituntil", args={"bool"}},
    {ret="void", name="generic.waitframe", args={}},
    {ret="void", name="generic.goto", args={"int"}},
    {ret="void", name="generic.gotoif", args={"int", "bool"}},
    {ret="void", name="generic.click", args={"vector"}},
    {ret="void", name="generic.slider", args={"vector:where", "double:value[0-1]"}},
    {ret="void", name="generic.scrollrect", args={"vector:where", "double:horizontal[scroll]", "double:vertical[scroll]"}, short="scrollbar"},
    {ret="int", name="generic.budget", args={}},
    {ret="int", name="screen.width", args={}},
    {ret="int", name="screen.height", args={}},
    {ret="double", name="screen.width.d", args={}, short="width.d"},
    {ret="double", name="screen.height.d", args={}, short="height.d"},
    {ret="double", name="option.ui.size", args={}, short="ui.size"},
    {ret="int", name="time.frame", args={}, short="time.frame"},
    {ret="double", name="timestamp.now", args={}},
    {ret="double", name="timestamp.utcnow", args={}},
    {ret="double", name="time.delta", args={}, short="time.delta"},
    {ret="double", name="time.unscaledDelta", args={}, short="time.unscaled"},
    {ret="double", name="time.scale", args={}, short="time.scale"},
  },
  UI={
    {ret="void", name="canvas.draw.rect", args={"vector:pos", "vector:size", "string:rgb_or_rgba"}, short="canvas.rect"},
    {ret="void", name="canvas.clear", args={}, short="canvas.clear"},
    {ret="void", name="window.create", args={"string:windowId", "string:windowType"}},
    {ret="void", name="window.destroy", args={"string:windowId"}},
    {ret="void", name="window.destroy.all", args={}, short="destroy.all"},
    {ret="void", name="window.text.set", args={"string:windowId", "string:textElementId", "string:value"}, short="text.set"},
    {ret="void", name="window.visibility.set", args={"string:windowId", "bool:isVisible"}, short="visibility.set"},
    {ret="void", name="window.child.visibility.set", args={"string:windowId", "string:elementId", "bool:isVisible"}, short="child.visibility.set"},
    {ret="void", name="window.position.set", args={"string:windowId", "vector:position"}, short="position.set"},
    {ret="void", name="window.sprite.set", args={"string:windowId", "string:elementId", "string:spriteId"}, short="sprite.set"},
    {ret="bool", name="window.visibility.get", args={"string:windowId"}, short="visibility.get"},
    {ret="bool", name="window.child.visibility.get", args={"string:windowId", "string:elementId"}, short="child.visibility.get"},
  },
  Town={
    {ret="bool", name="town.window.isopen", args={"string:window[window]"}},
    {ret="bool", name="town.window.anyopen", args={}},
    {ret="void", name="town.window.show", args={"string:window[window]", "bool"}},
  },
  Tower={
    {ret="bool", name="tower.stunned", args={}},
    {ret="int", name="tower.buffs.negative", args={}},
    {ret="double", name="tower.health", args={"bool:percent"}},
    {ret="double", name="tower.health.max", args={}, short="health.max"},
    {ret="double", name="tower.health.regeneration", args={}, short="health.regen"},
    {ret="double", name="tower.energy", args={"bool:percent"}},
    {ret="double", name="tower.energy.max", args={}, short="energy.max"},
    {ret="double", name="tower.energy.regeneration", args={}, short="energy.regen"},
    {ret="double", name="tower.shield", args={"bool:percent"}},
    {ret="double", name="tower.shield.max", args={}, short="shield.max"},
    {ret="double", name="tower.module.cooldown", args={"int:skill"}},
    {ret="double", name="tower.attackrange", args={}, short="tower.range"},
    {ret="void", name="tower.module.useinstant", args={"int:skill"}},
    {ret="void", name="tower.module.useposition", args={"int:skill", "vector:offset"}},
    {ret="void", name="tower.restart", args={}},
    {ret="void", name="tower.exit", args={}},
  },
  Game={
    {ret="bool", name="game.isBossFight", args={}},
    {ret="bool", name="game.isTowerTesting", args={}},
    {ret="bool", name="game.pause.get", args={}, short="pause.get"},
    {ret="int", name="game.enemies.count", args={}, short="enemies"},
    {ret="int", name="game.module.active.index", args={"string:moduleId"}, short="active.index"},
    {ret="int", name="game.module.active.count", args={}, short="active.count"},
    {ret="double", name="game.resource", args={"string:currency[currency]"}},
    {ret="double", name="game.wave", args={}},
    {ret="double", name="game.era", args={}},
    {ret="double", name="game.infinity", args={}},
    {ret="double", name="game.waveAcceleration", args={}},
    {ret="double", name="game.fixedWavesPerInterval", args={}},
    {ret="double", name="game.time", args={}, short="game.time"},
    {ret="double", name="game.realtime", args={}, short="game.realtime"},
    {ret="double", name="player.xp", args={}},
    {ret="double", name="highscore.wave", args={"string:region[region]", "string:difficulty[difficulty]"}, short="highscore.wave"},
    {ret="double", name="highscore.era", args={"string:region[region]", "string:difficulty[difficulty]"}, short="highscore.era"},
    {ret="double", name="highscore.infinity", args={"string:region[region]", "string:difficulty[difficulty]"}, short="highscore.infinity"},
    {ret="double", name="game.disable.era.cost", args={"string:element[elementAll]"}, short="disable.cost"},
    {ret="double", name="game.module.secure.cost", args={}, short="disable.inf.cost"},
    {ret="string", name="game.module.active.id", args={"int:index"}, short="active.id"},
    {ret="void", name="game.disable.era", args={"string:element[elementAll]"}, short="disable.era"},
    {ret="void", name="game.upgrade.era", args={"string:divider[eraDivider]", "int:numTimes"}, short="upgrade.era"},
    {ret="void", name="game.module.secure", args={"string:moduleId"}, short="disable.inf"},
    {ret="void", name="game.pause.set", args={"bool:paused"}, short="pause.set"},
    {ret="void", name="game.pause", args={}},
    {ret="void", name="game.unpause", args={}},
  },
  Software={
    {ret="bool", name="software.enabled", args={"string:name[software]"}, short="software.enabled"},
    {ret="int", name="software.loadout.count", args={}, short="loadout.count"},
    {ret="int", name="software.loadout.index.get", args={"string:loadoutName"}, short="loadout.index"},
    {ret="string", name="software.loadout.name.get", args={"int:index"}, short="loadout.name"},
    {ret="string", name="game.softwareid.find", args={"string:name"}, short="software.find"},
    {ret="void", name="software.toggle", args={"string:name[software]", "bool:on"}, short="software.toggle"},
    {ret="void", name="software.loadout.applyByName", args={"string:loadoutName"}, short="loadout.applyByName"},
    {ret="void", name="software.loadout.applyByIndex", args={"int:index"}, short="loadout.applyByIndex"},
  },
  Worker={
    {ret="bool", name="worker.paused", args={"string:name"}, short="worker.paused"},
    {ret="int", name="worker.group.get", args={"int:index"}, short="worker.group"},
    {ret="string", name="worker.name.get", args={"int:index"}, short="worker.name"},
    {ret="string", name="worker.task", args={"string:name"}, short="worker.task"},
    {ret="void", name="workers.toggle.group", args={"int:group[workerGroup]"}, short="worker.toggleGroup"},
    {ret="void", name="workers.pause.group", args={"int:group[workerGroup]", "bool:pause"}, short="worker.pauseGroup"},
    {ret="void", name="workers.toggle.name", args={"string:name"}, short="worker.toggleName"},
    {ret="void", name="workers.pause.name", args={"string:name", "bool:pause"}, short="worker.pauseName"},
    {ret="void", name="workers.assign.group", args={"string:task[workerTask]", "int:subtask", "int:group[workerGroup]"}, short="worker.assignGroup"},
    {ret="void", name="workers.assign.name", args={"string:task[workerTask]", "int:subtask", "string:name"}, short="worker.assignName"},
    {ret="void", name="worker.name.set", args={"int:index", "string:name"}, short="worker.setName"},
    {ret="void", name="worker.group.set", args={"int:index", "int:group[workerGroup]"}, short="worker.setGroup"},
  },
  ["Power Plant"]={
    {ret="void", name="powerplant.sell", args={"int:x[sellx]", "int:y[selly]"}},
  },
  Mine={
    {ret="bool", name="mine.hasLayers", args={}},
    {ret="int", name="mine.clusters", args={}},
    {ret="void", name="mine.newlayer", args={}},
    {ret="void", name="mine.dig", args={"int:x[dig]", "int:y[dig]"}},
    {ret="void", name="mine.tab", args={"int[minetab]"}},
    {ret="void", name="mine.cluster.remove", args={"int:cluster"}},
  },
  Arcade={
    {ret="bool", name="arcade.luckywheel.isSpinning", args={}, short="wheel.isSpinning"},
    {ret="bool", name="arcade.jumble.isActive", args={}, short="jumble.isActive"},
    {ret="void", name="arcade.luckywheel.spin", args={"double:wager"}, short="wheel.spin"},
    {ret="void", name="arcade.jumble.newGame", args={"double:wager"}, short="jumble.new"},
    {ret="void", name="arcade.jumble.stop", args={}, short="jumble.stop"},
    {ret="void", name="arcade.adventure.move", args={"vector:direction"}, short="adventure.move"},
    {ret="void", name="arcade.adventure.wait", args={}, short="adventure.wait"},
    {ret="void", name="arcade.adventure.placeBomb", args={}, short="adventure.placeBomb"},
    {ret="void", name="arcade.adventure.buyMarketItem", args={"string:item[marketItem]"}, short="adventure.buyMarketItem"},
    {ret="void", name="arcade.adventure.teleport", args={"vector:roomPos"}, short="adventure.teleport"},
    {ret="void", name="arcade.adventure.useSpell", args={"string:spell[spell]"}, short="adventure.useSpell"},
    {ret="vector", name="arcade.adventure.roomCoords", args={}, short="adventure.roomCoords"},
    {ret="vector", name="arcade.adventure.playerPos", args={}, short="adventure.playerPos"},
    {ret="int", name="arcade.adventure.playerHealth", args={}, short="adventure.playerHealth"},
    {ret="int", name="arcade.adventure.playerArmor", args={}, short="adventure.playerArmor"},
    {ret="int", name="arcade.adventure.playerAttack", args={}, short="adventure.playerAttack"},
    {ret="int", name="arcade.adventure.bombs", args={}, short="adventure.bombs"},
    {ret="int", name="arcade.adventure.countEntities", args={"string:type[entityType]"}, short="adventure.countEntities"},
    {ret="int", name="arcade.adventure.emerald", args={}, short="adventure.emeralds"},
    {ret="int", name="arcade.adventure.goldenHeart", args={}, short="adventure.goldenHearts"},
    {ret="int", name="arcade.adventure.keys", args={}, short="adventure.keys"},
    {ret="int", name="arcade.adventure.mana", args={}, short="adventure.mana"},
    {ret="int", name="arcade.adventure.manaArmor", args={}, short="adventure.manaArmor"},
    {ret="bool", name="arcade.adventure.hasPhoenixFeather", args={}},
    {ret="bool", name="arcade.adventure.hasItem", args={"string:item[keyItem]"}, short="adventure.hasItem"},
    {ret="bool", name="arcade.adventure.isWall", args={"vector:position"}, short="adventure.isWall"},
    {ret="bool", name="arcade.adventure.isBomb", args={"vector:position"}, short="adventure.isBomb"},
    {ret="bool", name="arcade.adventure.isEnemy", args={"vector:position"}, short="adventure.isEnemy"},
    {ret="bool", name="arcade.adventure.isCompleted", args={"vector:position"}, short="adventure.isCompleted"},
    {ret="string", name="arcade.adventure.entityType", args={"vector:position"}, short="adventure.entityType"},
  },
  Factory={
    {ret="bool", name="factory.machine.active", args={"string:machine[machine]"}},
    {ret="int", name="factory.machine.tier", args={"string:machine[machine]"}, short="machine.tier"},
    {ret="double", name="factory.items.count", args={"string:item[item]", "int:tier[tier]"}},
    {ret="double", name="factory.machine.item.count", args={"string:machine[machine]"}, short="machine.item.count"},
    {ret="string", name="factory.machine.item", args={"string:machine[machine]"}, short="machine.item"},
    {ret="string", name="factory.itemid.find", args={"string:name"}, short="factory.find"},
    {ret="void", name="factory.craft", args={"string:item[craft]", "int:tier[tier]", "double:amount"}},
    {ret="void", name="factory.produce", args={"string:item[produce]", "int:tier[tier]", "double:amount", "string:machine[machine]"}},
    {ret="void", name="factory.trash", args={"string:item[item]", "int:tier[tier]", "double:amount"}},
    {ret="void", name="factory.machine.cancel", args={"string:machine[machine]"}},
  },
  Museum={
    {ret="bool", name="museum.market.preference", args={"string:element[elementMarket]"}, short="museum.preference"},
    {ret="bool", name="museum.market.slotLocked", args={"int:offerSlot"}, short="museum.isSlotLocked"},
    {ret="int", name="museum.freeSlots", args={"string:inventory[inv]"}},
    {ret="int", name="museum.stone.tier", args={"string:inventory[inv]", "int:slot"}},
    {ret="int", name="museum.market.preferredTier", args={}, short="museum.preferredTier"},
    {ret="int", name="museum.market.maxTier", args={"string:element[elementMarket]"}, short="museum.maxTier"},
    {ret="int", name="museum.market.slotTier", args={"int:offerSlot"}, short="museum.slotTier"},
    {ret="int", name="museum.rebuy.tier", args={"int:trashSlot"}, short="museum.trashTier"},
    {ret="double", name="museum.market.timer", args={}, short="museum.timer"},
    {ret="string", name="museum.stone.element", args={"string:inventory[inv]", "int:slot"}},
    {ret="string", name="museum.market.slotElement", args={"int:offerSlot"}, short="museum.slotElement"},
    {ret="string", name="museum.rebuy.element", args={"int:trashSlot"}, short="museum.trashElement"},
    {ret="void", name="museum.combine", args={"int:tierMax"}},
    {ret="void", name="museum.transmute", args={}},
    {ret="void", name="museum.move", args={"string:from[inv]", "int:slot", "string:to[inv]"}},
    {ret="void", name="museum.delete", args={"string:inventory[inv]", "int:slot"}},
    {ret="void", name="museum.clear", args={"string:inventory[inv]"}},
    {ret="void", name="museum.stone.buy", args={"string:element[elementMarket]", "int:tier", "int:quantity"}, short="museum.buyTier"},
    {ret="void", name="museum.stone.buyRange", args={"string:element[elementMarket]", "int:tierMin", "int:tierMax", "int:quantity"}, short="museum.buyRange"},
    {ret="void", name="museum.moveSlot", args={"string:from[inv]", "int:fromSlot", "string:to[inv] int:toSlot"}, short="museum.moveTo"},
    {ret="void", name="museum.swap", args={"string:invA[inv]", "int:slotA", "string:invB[inv] int:slotB"}, short="museum.swap"},
    {ret="void", name="museum.market.set.preferredTier", args={"int:tier"}, short="museum.setPreferredTier"},
    {ret="void", name="museum.market.set.preference", args={"string:element[elementMarket]", "bool"}, short="museum.setPreference"},
    {ret="void", name="museum.market.refresh", args={}, short="museum.refresh"},
    {ret="void", name="museum.market.buy", args={"int:offerSlot", "int:quantity"}, short="museum.buyOffer"},
    {ret="void", name="museum.market.set.slotLocked", args={"int:offerSlot", "bool:locked"}, short="museum.setSlotLocked"},
    {ret="void", name="museum.rebuy.buy", args={"int:trashSlot"}, short="museum.rebuy"},
  },
  ["Trading Post"]={
    {ret="int", name="tradingpost.offerCount", args={}},
    {ret="void", name="tradingpost.refresh", args={}},
    {ret="void", name="tradingpost.trade", args={"int:offer", "double:pct[0-1]"}},
  },
  Shipyard={
    {ret="int", name="shipyard.order.current", args={}, short="order.current"},
    {ret="double", name="shipyard.seamiles", args={}},
    {ret="double", name="shipyard.shipments", args={}},
    {ret="double", name="shipyard.shipment.countdown", args={}, short="shipment.countdown"},
    {ret="double", name="shipyard.weather.bonus", args={}, short="weather.bonus"},
    {ret="void", name="shipyard.order.start", args={"int:index"}, short="order.start"},
    {ret="void", name="shipyard.order.collect", args={}, short="order.collect"},
    {ret="void", name="shipyard.order.cancel", args={}, short="order.cancel"},
  },
  Primitive={
    {ret="void", name="<scope>.<>.set", args={"string:variable", "<>"}, display="void [g/l][b/i/d/s/v]s(string:variable, type:value)   ;set", expand="bidsv"},
    {ret="<>", name="<scope>.<>.get", args={"string:variable"}, display="type [g/l][b/i/d/s/v]g(string:variable)   ;get", expand="bidsv"},
    {ret="void", name="global.unset", args={"string:variable"}, short="gu", display="void gu(string:variable)   ;global.unset"},
    {ret="void", name="local.unset", args={"string:variable"}, short="lu", display="void lu(string:variable)   ;local.unset"},
    {ret="bool", name="comparison.<>", args={"<>", "op_comp", "<>"}, display="bool c.[b/i/d/s](type:lhs, op_comp, type:rhs)   ;comparison", expand="bids"},
    {ret="<>", name="arithmetic.<>", args={"<>", "op_mod", "<>"}, display="type a.[i/d/v](type:lhs, op_mod, type:rhs)   ;arithmetic", expand="idv"},
  },
  Number={
    {ret="double", name="const.pi", args={}, short="const.pi"},
    {ret="double", name="const.e", args={}, short="const.e"},
    {ret="<>", name="<>.min", args={"<>", "<>"}, display="number min(a, b)", expand="id"},
    {ret="<>", name="<>.max", args={"<>", "<>"}, display="number max(a, b)", expand="id"},
    {ret="<>", name="<>.rnd", args={"<>", "<>"}, display="number rnd(min, max)", expand="id"},
    {ret="double", name="double.floor", args={"double"}},
    {ret="double", name="double.ceil", args={"double"}},
    {ret="double", name="double.round", args={"double"}},
    {ret="double", name="double.sin", args={"double:radians"}},
    {ret="double", name="double.cos", args={"double:radians"}},
    {ret="double", name="double.tan", args={"double:radians"}},
    {ret="double", name="double.asin", args={"double"}},
    {ret="double", name="double.acos", args={"double"}},
    {ret="double", name="double.atan", args={"double"}},
    {ret="double", name="double.atan2", args={"vector"}, short="atan2"},
  },
  String={
    {ret="bool", name="string.contains", args={"string:str", "string:substr"}},
    {ret="int", name="string.length", args={"string"}, short="len"},
    {ret="int", name="string.indexOf", args={"string:str", "string:substr", "int:offset"}, short="index"},
    {ret="string", name="concat", args={"string:lhs", "string:rhs"}},
    {ret="string", name="substring", args={"string", "int:offset", "int:length"}, short="sub"},
    {ret="string", name="string.lower", args={"string"}},
    {ret="string", name="string.upper", args={"string"}},
  },
  Conversion={
    {ret="int", name="d2i", args={"double"}},
    {ret="int", name="s2i", args={"string:input", "int:failureDefault"}},
    {ret="double", name="i2d", args={"int"}},
    {ret="double", name="s2d", args={"string:input", "double:failureDefault"}},
    {ret="string", name="i2s", args={"int"}},
    {ret="string", name="d2s", args={"double"}},
  },
  Vector={
    {ret="double", name="vec2.x", args={"vector"}},
    {ret="double", name="vec2.y", args={"vector"}},
    {ret="vector", name="vec.fromCoords", args={"double:x", "double:y"}, short="vec"},
    {ret="vector", name="mouse.position", args={}},
  },
  Macros={
    {display="any {lua(lua_code)}"},
    {display="int {len(any_characters)}"},
    {display="vector {pos.relative(double:x_pos[0-1], double:y_pos[0-1], double:x_anchor[0-1], double:y_anchor[0-1])}"},
    {display="void {click.relative(double:x_pos[0-1], double:y_pos[0-1], double:x_anchor[0-1], double:y_anchor[0-1])}"},
  },
}

local function addList(category, display)
  if category ~= "Hidden" then
    FUNCTION_LIST[category] = FUNCTION_LIST[category] or {}
    table.insert(FUNCTION_LIST[category], display)
  end
end

local typefull = {b="bool", i="int", d="double", s="string", v="vector"}
local typegame = {b="bool", i="int", d="double", s="string", v="vec2"}

local function parseFunction(def, category)
  if def.display then
    addList(category, def.display)
  end

  if category == "Macros" then return end

  if def.expand == "<char>" then
    for char in string.gmatch("0123456789abcdefghijklmnopqrstuvwxyz", ".") do
      local new = {ret=def.ret, name=def.name:gsub("<>", char), args=def.args}
      parseFunction(new, "Hidden")
    end

    return
  elseif def.expand then
    local done = {}

    for _, scope in ipairs(def.name:match("<scope>") and {"global", "local"} or {""}) do
      for i = 1, #def.expand do
        local typechar = def.expand:sub(i, i)
        local maintype = typefull[typechar]
        local gametype = typegame[typechar]
        local newargs = {}
        for j, arg in ipairs(def.args) do
          newargs[j] = arg:gsub("<>", maintype)
        end
        local short
        if scope == "" then
          local n = def.name:match("^(%a+)%.<>$")
          if n == "arithmetic" or n == "comparison" then
            short = n:sub(1,1) .. "." .. typechar
          else
            short = def.short and def.short:gsub("<>", maintype)
          end
        else
          local n = def.name:match("^<scope>%.<>%.(%a+)$")
          short = scope:sub(1, 1) .. typechar .. n:sub(1, 1)
        end

        local new = {
          ret=def.ret:gsub("<>", maintype),
          name=def.name:gsub("<>", gametype):gsub("<scope>", scope),
          args=newargs,
          short=short,
        }
        parseFunction(new, "Hidden")
      end
    end

    return
  end

  assert(not FUNCTION[def.name], "duplicate function: " .. def.name)
  assert(primitives[def.ret] and primitives[def.ret] < 2, "unknown return type: " .. def.ret)

  local args, display = {}, {}

  for i, arg in ipairs(def.args) do
    local validator
    local type, name = arg:gsub("%b[]", function(a)
        a = a:sub(2,-2)
        validator = assert(VALIDATOR[a], "unknown validator: " .. a)
        return ""
      end)
      :match"([^:]+):?(.*)"

    assert(primitives[type], "unknown argument type: " .. type)
    args[i] = {type = type, valid = validator}
    display[i] = name == "" and type or string.format("%s: %s", type, name)
  end

  local short = def.short
  if not short and category ~= "Impulse" and category ~= "Hidden" then
    short = def.name:match"%.(%a+)$" or def.name
  end

  short = short or def.name

  FUNCTION[def.name] = {
    name = def.name,
    short = short,
    ret = def.ret,
    args = args,
  }

  if short ~= def.name then
    assert(not FUNCTION[short], "duplicate short function: " .. short)
    FUNCTION[short] = FUNCTION[def.name]
  end

  if not def.display then
    addList(category, string.format("%s%s(%s)", def.ret == "void" and "" or def.ret .. " ", short, table.concat(display, ", ")))
  end
end

for category, tbl in pairs(functions) do
  for _, func in ipairs(tbl) do
    parseFunction(func, category)
  end
end

local functionList = {}

for category, tbl in pairs(FUNCTION_LIST) do
  functionList[#functionList+1] = string.format('<optgroup label="%s">', category)

  for _, func in ipairs(tbl) do
    functionList[#functionList+1] = string.format("<option>%s</option>", func)
  end

  functionList[#functionList+1] = "</optgroup>"
end

FUNCTION_LIST = table.concat(functionList)
