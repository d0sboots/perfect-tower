--[[
	// public method for encoding
	encode : function (input) {
		var output = "";
		var chr1, chr2, chr3, enc1, enc2, enc3, enc4;
		var i = 0;
 
		input = Base64._utf8_encode(input);
 
		while (i < input.length) {
 
			chr1 = input.charCodeAt(i++);
			chr2 = input.charCodeAt(i++);
			chr3 = input.charCodeAt(i++);
 
			enc1 = chr1 >> 2;
			enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
			enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
			enc4 = chr3 & 63;
 
			if (isNaN(chr2)) {
				enc3 = enc4 = 64;
			} else if (isNaN(chr3)) {
				enc4 = 64;
			}
 
			output = output +
			this._keyStr.charAt(enc1) + this._keyStr.charAt(enc2) +
			this._keyStr.charAt(enc3) + this._keyStr.charAt(enc4);
 
		}
 
		return output;
	},
	
	// public method for decoding
	decode : function (input) {
		var output = "";
		var chr1, chr2, chr3;
		var enc1, enc2, enc3, enc4;
		var i = 0;
 
		input = input.replace(/[^A-Za-z0-9\+\/\=]/g, "");
 
		while (i < input.length) {
 
			enc1 = this._keyStr.indexOf(input.charAt(i++));
			enc2 = this._keyStr.indexOf(input.charAt(i++));
			enc3 = this._keyStr.indexOf(input.charAt(i++));
			enc4 = this._keyStr.indexOf(input.charAt(i++));
 
			chr1 = (enc1 << 2) | (enc2 >> 4);
			chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
			chr3 = ((enc3 & 3) << 6) | enc4;
 
			output = output + String.fromCharCode(chr1);
 
			if (enc3 != 64) {
				output = output + String.fromCharCode(chr2);
			}
			if (enc4 != 64) {
				output = output + String.fromCharCode(chr3);
			}
 
		}
 
		output = Base64._utf8_decode(output);
 
		return output;
 
	},
]]

base64 = {};
local _keyStr = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
local _keyStrIndex = {};
for i = 1, #_keyStr do
	_keyStrIndex[_keyStr:sub(i,i)] = i - 1;
end

function base64.encode(input)
	local output = {};
	local chr1, chr2, chr3;
	local enc1, enc2, enc3, enc4;
	local i = 1;
	
	while (i <= #input) do
	
		chr1 = input:byte(i) or nil;
		i = i + 1;
		chr2 = input:byte(i) or nil;
		i = i + 1;
		chr3 = input:byte(i) or nil;
		i = i + 1;
		
		enc1 = chr1 >> 2;
		enc2 = ((chr1 & 3) << 4) | ((chr2 or 0) >> 4);
		enc3 = (((chr2 or 0) & 15) << 2) | ((chr3 or 0) >> 6);
		enc4 = (chr3 or 0) & 63;
		
		if (chr2 == nil) then
			enc3, enc4 = 64, 64;
		elseif (chr3 == nil) then
			enc4 = 64;
		end
		
		table.insert(output, _keyStr:sub(enc1+1, enc1+1));
		table.insert(output, _keyStr:sub(enc2+1, enc2+1));
		table.insert(output, _keyStr:sub(enc3+1, enc3+1));
		table.insert(output, _keyStr:sub(enc4+1, enc4+1));
	end
	
	return table.concat(output);
end

function base64.decode(input)
	local output = {};
	local chr1, chr2, chr3;
	local enc1, enc2, enc3, enc4;
	local i = 1;
	
	input = input:gsub("[^A-Za-z0-9%+%/%=]", "");
	
	while (i <= #input) do
		
		enc1 = _keyStrIndex[input:sub(i,i)] or 0;
		i = i + 1;
		enc2 = _keyStrIndex[input:sub(i,i)] or 0;
		i = i + 1;
		enc3 = _keyStrIndex[input:sub(i,i)] or 0;
		i = i + 1;
		enc4 = _keyStrIndex[input:sub(i,i)] or 0;
		i = i + 1;
		
		chr1 = (enc1 << 2) | (enc2 >> 4);
		chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
		chr3 = ((enc3 & 3) << 6) | enc4;
 
		table.insert(output, string.char(chr1));
 
		if (enc3 ~= 64) then
			table.insert(output, string.char(chr2));
		end
		if (enc4 ~= 64) then
			table.insert(output, string.char(chr3));
		end
	end
	
	return table.concat(output);
end