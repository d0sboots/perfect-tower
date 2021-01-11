local current_line, variables;

local dynamicFunc = {min = true, max = true, rnd =  true};

local function tokenError(...)
	local str, repl = current_line:gsub("^\1(.+)\2$", "%1");
	local pos = {...};
	local msg = table.remove(pos);
	
	for k, v in ipairs (pos) do
		pos[k] = type(v) == "table" and v.pos or v;
	end

	local markers = "";

	for k, v in ipairs (pos) do
		v = type(v) == "table" and v.pos or v;
		markers = markers .. string.rep(" ", v - (repl == 0 and 1 or 2) - #markers) .. "^";
	end

	return string.format("%s\n\n%s\n%s", msg, str, markers);
end

local function newNode(pos, parent, func)
	return {
		pos = pos,
		parent = parent,
		tokens = {},
		func = type(func) == "string" and assert(FUNCTION[func], "trying to call a non-function: " .. tostring(func)) or func;
		args = {},
	};
end

local function nextToken(str, pos, prev)
	pos = pos or 1;
	local ret = {};
	
	for _, token in ipairs (TOKEN) do
		if token.pattern then
			local match = str:sub(pos):match(token.pattern);

			if match then
				ret.pos = pos;
				ret.len = #match;
				ret.type = token.name;
				ret.value = ret.type ~= "number" and match or assert(tonumber(match), tokenError(pos, "invalid number: " .. match));
				ret.op = OPERATOR[match];

				if ret.type == "identifier" and (ret.value == "true" or ret.value == "false") then
					ret.type = "bool";
					ret.value = ret.value == "true";
				elseif ret.type == "string" then
					ret.value = ret.value:sub(2,-2);
				end

				assert(ret.type ~= "operator" or ret.op, tokenError(pos, "invalid operator: " .. match));
				assert(#token == 0 or prev and token[prev.type], tokenError(pos, "unexpected symbol: " .. (ret.type == "eof" and "<eof>" or match)));

				return ret;
			end
		end
	end
	
	assert(false, tokenError(pos, "unexpected symbol: " .. str:sub(pos,pos)));
end

local function resolveID(token)
	if token.type == "identifier" and not token.func then
		token.var = assert(variables[token.value], tokenError(token, "undefined variable: " .. token.value));
		token.type = "string";
		
		local new = newNode(token.pos, nil, string.format("%s.%s.get", token.var.scope, token.var.type));
		new.args = {token};
		return new;
	end
	
	return token;
end

local function resolveType(token)
	if token.var then
		return token.var.type;
	elseif token.op then
		return token.op.type;
	elseif token.func then
		return token.func.ret;
	elseif token.type == "number" then
		return math.type(token.value) == "integer" and "int" or "double";
	end
	
	return token.type;
end

local function typecheck(left, op, right)
	local typeLeft, typeRight = resolveType(left), resolveType(right);
	assert(typeLeft == typeRight, tokenError(left, op, right, string.format("trying to %s different types: %s and %s", op.op.name, typeLeft, typeRight)));
end

local function consumeTokensWorker(node)
	if #node.tokens == 0 then
		return;
	elseif #node.tokens == 1 then
		table.insert(node.args, resolveID(node.tokens[1]));
		node.tokens = {};
		return;
	end

	assert(#node.tokens % 2 == 1, "BUG REPORT: invalid expression");

	for k, token in ipairs (node.tokens) do
		node.tokens[k] = resolveID(token);
	end
	
	for i = 1, OPERATOR.__max do
		local j = 1;
		
		while j+2 <= #node.tokens do
			local left, op, right = node.tokens[j+0], node.tokens[j+1], node.tokens[j+2];
			local type = op.op.type;

			if op.op.order == i then
				table.remove(node.tokens, j); table.remove(node.tokens, j); table.remove(node.tokens, j);
				
				if type == "op_set" then
					local var = left.args[1].var;
					local new = newNode(left.pos, node, string.format("%s.%s.set", var.scope, var.type));

					if op.value ~= "=" then
						op.value = op.value:sub(1, -2);
						op.op = OPERATOR[op.value];
						
						typecheck(left, op, right);
						local new = newNode(left.pos, node, "arithmetic." .. resolveType(left.args[1]));
						new.args = {left, op, right};
						right = new;
					end
					
					typecheck(left.args[1], op, right);
					new.args = {left.args[1], right};
					table.insert(node.tokens, j, new);
				else
					local const = false;
					
					if type == "op_mod" and (left.type == "number" or left.type == "string") and (right.type == "number" or right.type == "string") then
						local status, ret = pcall(load(
							op.value == "."
							and string.format('return "%s" .. "%s"', left.value, right.value)
							or op.value == "//"
							and string.format("return math.log(%s, %s)", left.value, right.value)
							or string.format("return %s %s %s", left.value, op.value, right.value)
						));
						
						if status then
							const = true;
							left.value = resolveType(left) == "int" and math.floor(ret) or ret;
							left.type = op.value == "." and "string" or "number";
							table.insert(node.tokens, j, left);
						end
					end
					
					if not const then
						if op.value == "." then
							local typeLeft, typeRight = resolveType(left), resolveType(right);

							if typeLeft == "int" or typeLeft == "double" then
								local new = newNode(left.pos, node, typeLeft == "int" and "i2s" or "d2s");
								new.args = {left};
								left = new;
							end

							if typeRight == "int" or typeRight == "double" then
								local new = newNode(right.pos, node, typeRight == "int" and "i2s" or "d2s");
								new.args = {right};
								right = new;
							end

							typecheck(left, op, right);
							local new = newNode(left.pos, node, "concat");
							new.args = {left, right};
							table.insert(node.tokens, j, new);
						else
							typecheck(left, op, right);
							local new = newNode(left.pos, node, (type == "op_mod" and "arithmetic." or "comparison.") .. resolveType(left));
							new.args = {left, op, right};
							table.insert(node.tokens, j, new);
						end
					end
				end
			else
				j = j + 2;
			end
		end
	end

	assert(#node.tokens == 1, "invalid expression");
	table.insert(node.args, node.tokens[1]);
	node.tokens = {};
end

local function consumeTokens(node)
	consumeTokensWorker(node);
	local arg = #node.args;
	local last = node.args[arg];

	if last and node.func then
		local type = resolveType(last);
		local expected = node.func.args[arg];
		assert(expected, tokenError(node, last, string.format("function %s expects %s arguments, got %s", node.func.short, #node.func.args, arg)));

		if not dynamicFunc[node.func.name] then
			assert(type == expected.type, tokenError(node, last, string.format("bad argument #%s to %s (%s expected, got %s)", arg, node.func.short, expected.type, type)));

			if expected.valid and (last.type == "number" or last.type == "string") then
				local status, err = expected.valid(last.value);
				assert(status, tokenError(node, last, string.format("bad argument #%s to %s\n\n%s", arg, node.func.short, err)));
			end
		end
	end
end

function lexer(line, vars)
	local debug = {};
	local node;
	local pos = 1;

	node = newNode();
	line = string.format("\1%s\2", assert(line, "no input"));
	
	current_line = line;
	variables = vars;

	while pos <= #line do
		local token = nextToken(line, pos, prev);
		pos = pos + token.len;
		
		if token.type ~= "skip" and token.type ~= "comment" then
			table.insert(debug, string.format("%s %s", token.type, token.value));
			-- print (token.type, token.value);

			if token.type == "close" then
				assert(node.parent, "unmatched parenthesis");
				consumeTokens(node);
				
				if node.args and #node.args == 1 and not node.func then
					table.insert(node.parent.tokens, node.args[1]);
				else
					table.insert(node.parent.tokens, node);
				end
				
				if node.func then
					assert(#node.args == #node.func.args, tokenError(node, token, string.format("function %s expects %s arguments, got %s", node.func.short, #node.func.args, #node.args)));

					if dynamicFunc[node.func.name] then
						local arg = resolveType(node.args[1]);
						assert(arg == "int" or arg == "double", tokenError(node, node.args[1], string.format("bad argument #1 to %s (int or double expected, got %s)", node.func.short, type)));

						for i = 2, #node.args do
							local type = resolveType(node.args[i]);
							assert(type == arg, tokenError(node, node.args[i], string.format("bad argument #%s to %s (%s expected, got %s)", i, node.func.short, arg, type)));
						end

						local name = string.format("%s.%s", arg, node.func.name);
						node.func = FUNCTION[name];
					end
				end

				node = node.parent;
			elseif token.type == "open" then
				local last = table.remove(node.tokens);
				local func = last and last.type == "identifier" and last.value or table.insert(node.tokens, last);
				
				if func then
					func = assert(FUNCTION[func], tokenError(last, "trying to call a non-function: " .. func));
				end
				
				node = newNode(last and last.pos or token.pos, node, func);
			elseif token.type == "next" then
				assert(node.func, tokenError(token, "unexpected symbol: " .. token.value));
				consumeTokens(node);
			elseif token.type == "eof" then
				consumeTokens(node);
			elseif not token.type:match"^.of$" then
				table.insert(node.tokens, token);
				local arg = node.func and node.func.args[#node.args + 1];

				if arg and arg.type == "label" then
					if token.type == "identifier" or token.type == "number" then
						token.type = "label";
						token.value = tostring(token.value);
					end
				elseif token.type == "operator" and token.op.type == "op_set" then
					assert(not node.parent and #node.tokens == 2 and node.tokens[1].type == "identifier", tokenError(token, "unexpected symbol: " .. token.value));
				end
			end
			
			prev = token;
		end
	end

	assert(not node.parent, "unmatched parenthesis");

	node = node.args[1];

	if node then
		local ret = node.func and node.func.ret;
		assert(ret == "impulse" or ret == "bool" or ret == "void", "lines must return impulse, bool or nothing");

		return node, debug;
	end
end

if DEBUG then
	local text =
[[dig(rnd(1,3) + ((dig + ((3+1+2)+5) - dig * (3-dig)^(8-9)) - 2), 3)]];
-- [[foo = isfill("bar", bar)]];
-- [[a = OR(AND(1, 2, 3), AND(4, 5, 6))]];
-- [[foo == dig]];
-- [[gotoif(hello, 3 == 5)]];
-- [[click(vec(3.,4.))]];

	local vars = {
		dig = {type = "int", scope = "local"},
		foo = {type = "int", scope = "global"},
	};

	local node, debug = lexer(text, vars);
	print();
	print(AST(node));
	print();
	print ("  input: " .. text);
	print();
	text = rebuild(node);
	print ("rebuild: " .. text);

	node, debug = lexer(text, vars);
	print();
	print(AST(node));
	print();
	print ("  input: " .. text);
	print();
	text = rebuild(node);
	print ("rebuild: " .. text);
end
