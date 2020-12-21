do -- Print table
	local function print_internal(tbl, ignoreKeys, visited, depth)
		local sorted = {};

		for k, v in pairs (tbl) do
			if not ignoreKeys[k] then
				if type(v) ~= "table" or not visited[v] then
					visited[v] = visited[v] or type(v) == "table";
					table.insert(sorted, {k, v});
				end
			end
		end

		table.sort(sorted, function(a,b) return a[1] < b[1]; end);

		for k, v in ipairs (sorted) do
			print(string.format("%s[%s] %s: %s", string.rep("  ", depth), v[1], type(v[2]), v[2]));

			if type(v[2]) == "table" then
				print_internal(v[2], ignoreKeys, visited, depth + 1);
			end
		end
	end

	function table.print(tbl, ignoreKeys)
		local ignore = {};

		for k, v in ipairs (ignoreKeys or {}) do
			ignore[v] = true;
		end

		print_internal(tbl, ignore, {}, 0);
	end
end

do -- Abstract Syntax Tree
	local AST_ret;

	local char_connect = DEBUG and "\179 " or "  ";
	local char_end = DEBUG and "\192 " or "  ";
	local char_split = DEBUG and "\195 " or "  ";

	local function AST_internal(node, prefix, isLast)
		prefix = prefix:gsub("..$", isLast and char_end or char_split);
		
		if node.type then
			table.insert(AST_ret, prefix .. node.type .. " " .. tostring(node.value));
		else
			table.insert(AST_ret, prefix .. (node.func and node.func.name or "()"));
			prefix = prefix:gsub("..$", isLast and "  " or char_connect) .. "  ";

			for k, v in ipairs(node.args) do
				AST_internal(v, prefix, k == #node.args);
			end
		end
	end

	function AST(node)
		AST_ret = {};
		AST_internal(node, "");
		return table.concat(AST_ret, "\n");
	end
end

do -- Rebuild line from tree
	local function strip(str)
		return tostring(str):gsub("^%((.+)%)$", "%1");
	end

	local function rebuild_internal(node, isTop)
		if node.type then
			return node.value;
		end
		
		local args = {};
		
		for _, arg in ipairs (node.args) do
			table.insert(args, rebuild_internal(arg));
		end
		
		local func = node.func.short or node.func.name or "";
		local ret;
		
		if func:match"^arithmetic" or func:match"^comparison" then
			ret = string.format("(%s)", table.concat(args, " "));
		elseif func:match"%.set$" then
			return string.format("%s = %s", args[1]:sub(2,-2), strip(args[2]));
		elseif func:match"%.get$" then
			return args[1]:sub(2, -2);
		else
			for k, v in ipairs (args) do
				args[k] = strip(v);
			end
			
			ret = string.format("%s(%s)", func, table.concat(args, ", "));
		end
		
		-- local ret = set and string.format("%s = %s", table.unpack(args)) or string.format("%s(%s)", func or "", table.concat(args, func and ", " or " "));
		return isTop and strip(ret) or ret;
	end

	function rebuild(node)
		return rebuild_internal(node, true);
	end
end