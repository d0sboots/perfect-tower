TOKEN = {
	-- pattern, ... (must follow one of these tokens)
	{name = "skip", pattern = "%s+"},
	{name = "comment", pattern = ";[^\2]*"},
	{name = "sof", pattern = "\1"},
	{name = "eof", pattern = "\2", "sof", "close", "identifier", "number", "string", "bool"},
	{name = "open", pattern = "%(", "sof", "open", "next", "identifier", "operator"},
	{name = "close", pattern = "%)", "open", "close", "identifier", "number", "string", "bool"},
	{name = "next", pattern = ",", "close", "identifier", "number", "string", "bool", "label"},
	
	{name = "identifier", pattern = "[%a_][%w%._]*", "sof", "open", "next", "operator"},
	{name = "number", pattern = "%-?%d+%.?%d*", "sof", "open", "next", "operator"},
	{name = "string", pattern = '%b""', "sof", "open", "next", "operator"},
	{name = "bool", "sof", "open", "next", "operator"},
	{name = "label", "open"},
	{name = "operator", pattern = "[%+%-%*%%/^!=<>&|]+", "close", "identifier", "number", "string", "bool"},
};

for _, v in ipairs (TOKEN) do
	TOKEN[v.name] = v;
	v.pattern = v.pattern and string.format("^(%s)", v.pattern) or nil;
	v.patternAnywhere = v.pattern and v.pattern:sub(2);
end

for _, v in ipairs (TOKEN) do
	for _, type in ipairs (v) do
		assert(TOKEN[type], "unknown token: " .. type);
		v[type] = true;
	end
end