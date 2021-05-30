OPERATOR = {};

local type = "op_mod";
local order = 1;

local operators = [[
^ exponent
// log

* multiply
/ divide
% mod

+ add
- subtract

. concatenate

== compare
!= compare
< compare
<= compare
> compare
>= compare

&& compare
& compare

|| compare
| compare

= assign
]];

operators:gsub("([^\n ]*) ?([^\n]*)", function(op, name)
	if op == "" then
		order = order + 1;
		return;
	elseif op == "==" then
		type = "op_comp";
	elseif op == "=" then
		type = "op_set";
	end
	
	OPERATOR[op] = {order = order, type = type, name = name};
end);

local final = {};

for k, v in pairs (OPERATOR) do
	final[k] = v;
	
	if v.type == "op_mod" then
		final[k .. "="] = {order = order, type = "op_set", name = "assign"};
	end
end

OPERATOR = final;
OPERATOR.__max = order;