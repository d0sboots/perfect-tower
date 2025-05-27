local current_line, variables;

local dynamicFunc = {min = true, max = true, rnd =  true, ["if"] = true};

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

	return error_lexer(string.format("%s\n\n%s\n%s", msg, str, markers))
end

local function newNode(pos, parent, func)
	local node_func
	if type(func) == "string" then
		node_func = FUNCTION[func]
		if not node_func then error_lexer("trying to call a non-function: " .. tostring(func)) end
	else
		node_func = func
	end
	return {
		pos = pos,
		parent = parent,
		tokens = {},
		func = node_func,
		args = {},
	};
end

local function nextToken(str, pos, prev)
	pos = pos or 1;
	local ret = {};
	
	for _, token in ipairs (TOKEN) do
		if token.pattern then
			local match = str:match(token.pattern, pos);

			if match then
				if token.alias then
					token = TOKEN[token.alias];
				end

				ret.pos = pos;
				ret.len = #match;
				ret.type = token.name;
				if ret.type ~= "number" then
					ret.value = match
				else
					ret.value = tonumber(match)
					if not ret.value then tokenError(pos, "invalid number: " .. match) end
				end
				ret.op = OPERATOR[match];

				if ret.type == "identifier" and (ret.value == "true" or ret.value == "false") then
					ret.type = "bool";
					ret.value = ret.value == "true";
				elseif ret.type == "string" then
					ret.value = ret.value:sub(2,-2);
				end

				if ret.type == "operator" and not ret.op then tokenError(pos, "invalid operator: " .. match) end
				if #token ~= 0 and prev and not token[prev.type] then
					tokenError(pos, "unexpected symbol: " .. (ret.type == "eof" and "<eof>" or match))
				end

				return ret;
			end
		end
	end
	
	tokenError(pos, "unexpected symbol: " .. str:sub(pos,pos))
end

local function resolveID(token)
	if token.type == "identifier" and not token.func then
		token.var = variables[token.value]
		if not token.var then tokenError(token, "undefined variable: " .. token.value) end

		if token.var.scope == "constant" then
			token.type = token.var.type;
			if token.type == "int" or token.type == "double" then
				token.type = "number";
			end
			token.value = token.var.value;
			return token;
		end

		token.type = "string";

		local ttype = token.var.type == "vector" and "vec2" or token.var.type;
		local new = newNode(token.pos, nil, token.var.label and "label" or string.format("%s.%s.get", token.var.scope, ttype));
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

local function resolveTypeOp(token)
	local resolved = resolveType(token);
	return resolved == "vector" and "vec2" or resolved;
end

local function typecheck(left, op, right)
	local typeLeft, typeRight = resolveType(left), resolveType(right);
	if typeLeft ~= typeRight then
		tokenError(left, op, right, string.format("trying to %s different types: %s and %s", op.op.name, typeLeft, typeRight))
	end
end

local function consumeTokensWorker(node)
	if #node.tokens == 0 then
		if not node.func then tokenError(node, "invalid empty parenthesis") end
		return;
	elseif #node.tokens == 1 then
		node.args[#node.args+1] = resolveID(node.tokens[1])
		node.tokens = {};
		return;
	end

	if #node.tokens % 2 ~= 1 then error_lexer("BUG REPORT: invalid expression") end

	for k, token in ipairs (node.tokens) do
		node.tokens[k] = resolveID(token);
	end
	
	for i = 1, OPERATOR.__max do
		local j, k = 1, 1;
		
		while j+2 <= #node.tokens do
			local left, op, right = node.tokens[j+0], node.tokens[j+1], node.tokens[j+2];
			local type = op.op.type;

			if op.op.order == i then
				j = j + 2
				
				if type == "op_set" then
					if not left.args then
						tokenError(left, "You can't assign values to constants");
					end
					local var = left.args[1].var;
					local ttype = var.type == "vector" and "vec2" or var.type
					if var.label then tokenError(left, "You can't assign values to labels") end
					local new = newNode(left.pos, node, string.format("%s.%s.set", var.scope, ttype));

					if op.value ~= "=" then
						op.value = op.value:sub(1, -2);
						op.op = OPERATOR[op.value];
						
						typecheck(left, op, right);

						if op.value == "." then
							local new = newNode(left.pos, node, "concat");
							new.args = {left, right};
							right = new;
						else
							local new = newNode(left.pos, node, "arithmetic." .. resolveTypeOp(left.args[1]));
							new.args = {left, op, right};
							right = new;
						end
					end
					
					typecheck(left.args[1], op, right);
					new.args = {left.args[1], right};
					node.tokens[j] = new;
				else
					local const = false;
					
					if type == "op_mod" and (left.type == right.type or op.value == ".") and (left.type == "number" or left.type == "string") and (right.type == "number" or right.type == "string") then
						local isNegativeRemainder;
						if op.value == "%" then
							right.value = math.abs(right.value);
							if left.value < 0 then
								isNegativeRemainder = true;
								left.value = -left.value;
							end
						end

						local status, ret = pcall(load(
							op.value == "."
							and string.format('return %q .. %q', left.value, right.value)
							or op.value == "//"
							and string.format("return math.log(%s, %s)", left.value, right.value)
							or string.format("return %s %s %s", left.value, op.value, right.value)
						));
						
						if status then
							const = true;
							left.value = resolveType(left) == "int" and op.value ~= "." and math.modf(ret) or ret;
							if isNegativeRemainder then
								left.value = -left.value;
							end
							left.type = op.value == "." and "string" or "number";
							node.tokens[j] = left;
						end
					end
					
					if not const then
						local typeLeft, typeRight = resolveType(left), resolveType(right);

						if op.value == "." then
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

							if left.type == "string" and left.value == "" then
								node.tokens[j] = right;
							elseif right.type == "string" and right.value == "" then
								node.tokens[j] = left;
							else
								local new = newNode(left.pos, node, "concat");
								new.args = {left, right};
								node.tokens[j] = new;
							end
						else
							if type == "op_mod" then
								local types = {int = 1, double = 1, vector = 1};
								if not types[typeLeft] then
									tokenError(left, "arithmetic cannot be performed on a " .. typeLeft)
								end
								if not types[typeRight] then
									tokenError(right, "arithmetic cannot be performed on a " .. typeRight)
								end
								if op.value == "%" and typeLeft == "vector" then
									tokenError(op, "vector doesn't support modulus")
								end
							end
							
							typecheck(left, op, right);
							local new = newNode(left.pos, node, (type == "op_mod" and "arithmetic." or "comparison.") .. resolveTypeOp(left));
							new.args = {left, op, right};
							node.tokens[j] = new;
						end
					end
				end
			else
				node.tokens[k] = node.tokens[j]
				node.tokens[k+1] = node.tokens[j+1]
				j = j + 2
				k = k + 2
			end
		end
		node.tokens[k] = node.tokens[j]
		while k < j do
			k = k + 1
			node.tokens[k] = nil
		end
	end

	if #node.tokens ~= 1 then error_lexer("invalid expression") end
	node.args[#node.args+1] = node.tokens[1]
	node.tokens = {};
end

local function consumeTokens(node)
	consumeTokensWorker(node);
	local arg = #node.args;
	local last = node.args[arg];

	if last and node.func then
		local type = resolveType(last);
		local expected = node.func.args[arg];
		if not expected then
			tokenError(node, last, string.format("function %s expects %s arguments, got %s", node.func.short, #node.func.args, arg))
		end

		if not dynamicFunc[node.func.name] then
			if type ~= expected.type and (type ~= "string" or not expected.type:match"^op") then
				tokenError(node, last, string.format("bad argument #%s to %s (%s expected, got %s)", arg, node.func.short, expected.type, type))
			end

			if expected.valid and (last.type == "number" or last.type == "string") then
				local status, err = expected.valid(last.value);
				if not status then
					tokenError(node, last, string.format("bad argument #%s to %s\n\n%s", arg, node.func.short, err))
				end
			end
		end
	end
end

function lexer(line, vars)
	local debug = {};
	local node, prev;
	local pos = 1;

	node = newNode();
	if not line then error_lexer("no input") end
	line = string.format("\1%s\2", line);
	
	current_line = line;
	variables = vars;

	while pos <= #line do
		local token = nextToken(line, pos, prev);
		pos = pos + token.len;
		
		if token.type ~= "skip" and token.type ~= "comment" then
			table.insert(debug, string.format("%s %s", token.type, token.value));
			-- print (token.type, token.value);

			if token.type == "close" then
				if not node.parent then tokenError(token, "unmatched parenthesis") end
				consumeTokens(node);
				
				if node.args and #node.args == 1 and not node.func then
					table.insert(node.parent.tokens, node.args[1]);
				else
					table.insert(node.parent.tokens, node);
				end
				
				if node.func then
					if #node.args ~= #node.func.args then
						tokenError(node, token, string.format("function %s expects %s arguments, got %s", node.func.short, #node.func.args, #node.args))
					end

					if node.func.name == "clickrel" then
						for k, arg in ipairs (node.args) do
							local new = newNode(arg.pos, node, "arithmetic.double");
							new.args = {arg, nextToken"*", newNode(arg.pos, node, k == 1 and "screen.width.d" or "screen.height.d")};
							node.args[k] = new;
						end

						local new = newNode(node.pos, node, "vec.fromCoords");
						new.args = node.args;
						node.args = {new};
						node.func = FUNCTION.click;

					elseif node.func.short == "vec" then
						-- Represent as a constant if both values are constant
						local x = node.args[1]
						local y = node.args[2]
						if not x.func and x.type == "number" and not y.func and y.type == "number" then
							assert(x.value, "x.value")
							assert(y.value, "y.value")
							node.value = {x = x.value, y = y.value}
							node.type = "vector"
							node.func = nil
						end
					elseif dynamicFunc[node.func.name] then
						if node.func.short == "if" then
							local arg1 = resolveType(node.args[1]);
							if arg1 ~= "bool" then
								tokenError(node, node.args[1], string.format("bad argument #1 to %s (bool expected, got %s)", node.func.short, arg1))
							end
							local arg2 = resolveType(node.args[2]);
							local arg3 = resolveType(node.args[3]);
							local func = FUNCTION["ternary." .. arg2];
							if not func then
								tokenError(node, node.args[2], string.format("bad argument #2 to %s (int, double, string or vector expected, got %s)", node.func.short, arg2))
							end
							if arg3 ~= arg2 then
								tokenError(node, node.args[3], string.format("bad argument #3 to %s (%s expected, got %s)", node.func.short, arg2, arg3))
							end
							node.func = func;
						else
							local arg = resolveType(node.args[1]);
							if arg ~= "int" and arg ~= "double" then
								tokenError(node, node.args[1], string.format("bad argument #1 to %s (int or double expected, got %s)", node.func.short, arg))
							end

							for i = 2, #node.args do
								local type = resolveType(node.args[i]);
								if type ~= arg then
									tokenError(node, node.args[i], string.format("bad argument #%s to %s (%s expected, got %s)", i, node.func.short, arg, type))
								end
							end

							local name = string.format("%s.%s", arg, node.func.name);
							node.func = FUNCTION[name];
						end
					end
				end

				node = node.parent;
			elseif token.type == "open" then
				local last = table.remove(node.tokens);
				local func = last and last.type == "identifier" and last.value or table.insert(node.tokens, last);
				
				if func then
					if not FUNCTION[func] then
						tokenError(last, "trying to call a non-function: " .. func)
					end
					func = FUNCTION[func]
				end
				
				node = newNode(func and last.pos or token.pos, node, func);
			elseif token.type == "next" then
				if not node.func then tokenError(token, "unexpected symbol: " .. token.value) end
				consumeTokens(node);
			elseif token.type == "eof" then
				consumeTokens(node);
			elseif not token.type:match"^.of$" then
				table.insert(node.tokens, token);
				local arg = node.func and node.func.args[#node.args + 1];

				if token.type == "operator" and token.op.type == "op_set" then
					if node.parent or #node.tokens ~= 2 or node.tokens[1].type ~= "identifier" then
						tokenError(token, "unexpected symbol: " .. token.value);
					end
				end
			end
			
			prev = token;
		end
	end

	if node.parent then error_lexer("unmatched parenthesis") end

	node = node.args[1];

	if node then
		local ret = node.func and node.func.ret;
		if ret ~= "impulse" and ret ~= "bool" and ret ~= "void" then
			error_lexer("lines must return impulse, bool or nothing")
		end

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
