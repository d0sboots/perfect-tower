TOKEN = {
	-- pattern, ... (must follow one of these tokens)
	skip = {pattern = "%s+"},
	comment = {pattern = ";[^\2]*"},
	sof = {pattern = "\1"},
	eof = {pattern = "\2", "sof", "close", "identifier", "number", "string", "bool"},
	open = {pattern = "%(", "sof", "open", "next", "identifier", "operator"},
	close = {pattern = "%)", "open", "close", "identifier", "number", "string", "bool"},
	next = {pattern = ",", "close", "identifier", "number", "string", "bool", "label"},
	
	identifier = {pattern = "[%a_][%w%._]*", "sof", "open", "next", "operator"},
	number = {pattern = "%d+%.?%d*", "sof", "open", "next", "operator"},
	string = {pattern = '%b""', "sof", "open", "next", "operator"},
	bool = {"sof", "open", "next", "operator"},
	label = {"open"},
	operator = {pattern = "[%+%-%*%%/^!=<>&|]+", "close", "identifier", "number", "string", "bool"},
};

for _, v in pairs (TOKEN) do
	v.pattern = v.pattern and string.format("^(%s)", v.pattern) or nil;
	v.patternAnywhere = v.pattern and v.pattern:sub(2);
	
	for _, type in ipairs (v) do
		assert(TOKEN[type], "unknown token: " .. type);
		v[type] = true;
	end
end