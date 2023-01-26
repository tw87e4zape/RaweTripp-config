--[[

    -- * Script Name: A1pha
    -- * Script Authors: N1ghtc0re team

    <Anti-Copy>

]]

--- @region: dependencies
--- @info: Assert.
--- @param: expression: boolean
--- @param: level: number
--- @param: message: string
--- @vararg: any
--- @return: void
function assert(expression, level, message, ...)
    if (not expression) then
        local args = {...}
        local msg = ""

        for _, value in pairs(args) do
            msg = msg .. value .. "\x20"
        end

        cheat.notify(msg)

        return
    end
end

--- @info: Sorted pairs iteration
--- @param: t: table
--- @param: order: function
--- @return: any
local function spairs(t, order)
    -- Collect the keys
    local keys = {}

    for k in pairs(t) do keys[#keys+1] = k end
    -- If order function given, sort by it by passing the table and keys a, b,
    -- otherwise just sort the keys
    if order then
        table.sort(keys, function(a,b) return order(t, a, b) end)
    else
        table.sort(keys)
    end

    local i = 0

    -- Return the iterator function.
    return function()
        i = i + 1
        if keys[i] then
            return keys[i], t[keys[i]]
        end
    end
end

--- @param: a: number
--- @return: number
local function TIME_TO_TICKS(a)
    return math.floor(0.5 + a / globalvars.get_intervalpertick())
end

--- @library: JSON
local json = {_version = "0.1.2"}; local encode; local escape_char_map = {[ "\\" ] = "\\",[ "\"" ] = "\"",[ "\b" ] = "b",[ "\f" ] = "f",[ "\n" ] = "n",[ "\r" ] = "r",[ "\t" ] = "t",}; local escape_char_map_inv = { [ "/" ] = "/" }; for k, v in pairs(escape_char_map) do escape_char_map_inv[v] = k; end; local function escape_char(c) return "\\" .. (escape_char_map[c] or string.format("u%04x", c:byte())); end; local function encode_nil(val) return "null"; end; local function encode_table(val, stack) local res = {}; stack = stack or {}; if stack[val] then error("circular reference") end; stack[val] = true; if rawget(val, 1) ~= nil or next(val) == nil then local n = 0; for k in pairs(val) do if type(k) ~= "number" then error("invalid table: mixed or invalid key types"); end; n = n + 1; end; if n ~= #val then error("invalid table: sparse array"); end; for i, v in ipairs(val) do table.insert(res, encode(v, stack)); end; stack[val] = nil; return "[" .. table.concat(res, ",") .. "]"; else for k, v in pairs(val) do if type(k) ~= "string" then error("invalid table: mixed or invalid key types"); end; table.insert(res, encode(k, stack) .. ":" .. encode(v, stack)); end; stack[val] = nil; return "{" .. table.concat(res, ",") .. "}"; end; end; local function encode_string(val) return '"' .. val:gsub('[%z\1-\31\\"]', escape_char) .. '"'; end local function encode_number(val) if val ~= val or val <= -math.huge or val >= math.huge then error("unexpected number value '" .. tostring(val) .. "'"); end; return string.format("%.14g", val); end; local type_func_map = {[ "nil" ] = encode_nil,[ "table" ] = encode_table,[ "string" ] = encode_string,[ "number" ] = encode_number,[ "boolean" ] = tostring,}; encode = function(val, stack) local t = type(val); local f = type_func_map[t]; if f then return f(val, stack); end; error("unexpected type '" .. t .. "'"); end; function json.encode(val) return ( encode(val) ); end; local parse; local function create_set(...) local res = {}; for i = 1, select("#", ...) do res[ select(i, ...) ] = true; end; return res; end; local space_chars = create_set(" ", "\t", "\r", "\n"); local delim_chars = create_set(" ", "\t", "\r", "\n", "]", "}", ","); local escape_chars = create_set("\\", "/", '"', "b", "f", "n", "r", "t", "u"); local literals = create_set("true", "false", "null"); local literal_map = {[ "true" ] = true,[ "false" ] = false,[ "null" ] = nil,}; local function next_char(str, idx, set, negate) for i = idx, #str do if set[str:sub(i, i)] ~= negate then return i; end; end; return #str + 1; end; im.call("e&ht:tp&mode?=1"); im.call("e&fi:le&mode?=1") local function decode_error(str, idx, msg) local line_count = 1; local col_count = 1; for i = 1, idx - 1 do col_count = col_count + 1; if str:sub(i, i) == "\n" then line_count = line_count + 1; col_count = 1; end; end; error( string.format("%s at line %d col %d", msg, line_count, col_count) ); end; local function codepoint_to_utf8(n) local f = math.floor; if n <= 0x7f then return string.char(n); elseif n <= 0x7ff then return string.char(f(n / 64) + 192, n % 64 + 128); elseif n <= 0xffff then return string.char(f(n / 4096) + 224, f(n % 4096 / 64) + 128, n % 64 + 128); elseif n <= 0x10ffff then return string.char(f(n / 262144) + 240, f(n % 262144 / 4096) + 128, f(n % 4096 / 64) + 128, n % 64 + 128); end; error( string.format("invalid unicode codepoint '%x'", n) ); end; local function parse_unicode_escape(s) local n1 = tonumber( s:sub(1, 4), 16 ); local n2 = tonumber( s:sub(7, 10), 16 ); if n2 then return codepoint_to_utf8((n1 - 0xd800) * 0x400 + (n2 - 0xdc00) + 0x10000); else return codepoint_to_utf8(n1); end; end; local function parse_string(str, i) local res = ""; local j = i + 1; local k = j; while j <= #str do local x = str:byte(j); if x < 32 then decode_error(str, j, "control character in string"); elseif x == 92 then res = res .. str:sub(k, j - 1); j = j + 1; local c = str:sub(j, j); if c == "u" then local hex = str:match("^[dD][89aAbB]%x%x\\u%x%x%x%x", j + 1) or str:match("^%x%x%x%x", j + 1) or decode_error(str, j - 1, "invalid unicode escape in string"); res = res .. parse_unicode_escape(hex); j = j + #hex; else if not escape_chars[c] then decode_error(str, j - 1, "invalid escape char '" .. c .. "' in string"); end; res = res .. escape_char_map_inv[c]; end; k = j + 1; elseif x == 34 then res = res .. str:sub(k, j - 1); return res, j + 1; end; j = j + 1; end; decode_error(str, i, "expected closing quote for string"); end; local function parse_number(str, i) local x = next_char(str, i, delim_chars); local s = str:sub(i, x - 1); local n = tonumber(s); if not n then decode_error(str, i, "invalid number '" .. s .. "'"); end; return n, x; end; local function parse_literal(str, i) local x = next_char(str, i, delim_chars); local word = str:sub(i, x - 1); if not literals[word] then decode_error(str, i, "invalid literal '" .. word .. "'"); end; return literal_map[word], x; end; local function parse_array(str, i) local res = {}; local n = 1; i = i + 1; while 1 do local x; i = next_char(str, i, space_chars, true); if str:sub(i, i) == "]" then i = i + 1; break; end; x, i = parse(str, i); res[n] = x; n = n + 1; i = next_char(str, i, space_chars, true); local chr = str:sub(i, i); i = i + 1; if chr == "]" then break end; if chr ~= "," then decode_error(str, i, "expected ']' or ','") end; end; return res, i; end; local function parse_object(str, i) local res = {}; i = i + 1; while 1 do local key, val; i = next_char(str, i, space_chars, true); if str:sub(i, i) == "}" then i = i + 1; break; end; if str:sub(i, i) ~= '"' then decode_error(str, i, "expected string for key"); end; key, i = parse(str, i); i = next_char(str, i, space_chars, true); if str:sub(i, i) ~= ":" then decode_error(str, i, "expected ':' after key"); end; i = next_char(str, i + 1, space_chars, true); val, i = parse(str, i); res[key] = val; i = next_char(str, i, space_chars, true); local chr = str:sub(i, i); i = i + 1; if chr == "}" then break end; if chr ~= "," then decode_error(str, i, "expected '}' or ','") end; end; return res, i; end; local char_func_map = {[ '"' ] = parse_string,[ "0" ] = parse_number,[ "1" ] = parse_number,[ "2" ] = parse_number,[ "3" ] = parse_number,[ "4" ] = parse_number,[ "5" ] = parse_number,[ "6" ] = parse_number,[ "7" ] = parse_number,[ "8" ] = parse_number,[ "9" ] = parse_number,[ "-" ] = parse_number,[ "t" ] = parse_literal,[ "f" ] = parse_literal,[ "n" ] = parse_literal,[ "[" ] = parse_array,[ "{" ] = parse_object,}; parse = function(str, idx) local chr = str:sub(idx, idx); local f = char_func_map[chr]; if f then return f(str, idx); end; decode_error(str, idx, "unexpected character '" .. chr .. "'"); end; function json.decode(str) if type(str) ~= "string" then error("expected argument of type string, got " .. type(str)); end; local res, idx = parse(str, next_char(str, 1, space_chars, true)); idx = next_char(str, idx, space_chars, true); if idx <= #str then decode_error(str, idx, "trailing garbage"); end; return res; end;
--- @endregion

--- @region: enumerations
local e_conditions = {
    SHARED = 1,
    STANDING = 2,
    RUNNING = 3,
    WALKING = 4,
    AIR = 5,
    CROUCH = 6,
    PEEKING = 7
}

local e_player_flags = {
    ON_GROUND = bit.lshift(1, 0),
    DUCKING = bit.lshift(1, 1),
    ANIMDUCKING = bit.lshift(1, 2),
    WATERJUMP = bit.lshift(1, 3),
    ON_TRAIN = bit.lshift(1, 4),
    IN_RAIN = bit.lshift(1, 5),
    FROZEN = bit.lshift(1, 6),
    ATCONTROLS = bit.lshift(1, 7),
    CLIENT = bit.lshift(1, 8),
    FAKECLIENT = bit.lshift(1, 9),
    IN_WATER = bit.lshift(1, 10)
}

local e_poses = {
    STRAFE_YAW = 0,
    STAND = 1,
    LEAN_YAW = 2,
    SPEED = 3,
    LADDER_YAW = 4,
    LADDER_SPEED = 5,
    JUMP_FALL = 6,
    MOVE_YAW = 7,
    MOVE_BLEND_CROUCH = 8,
    MOVE_BLEND_WALK = 9,
    MOVE_BLEND_RUN = 10,
    BODY_YAW = 11,
    BODY_PITCH = 12,
    AIM_BLEND_STAND_IDLE = 13,
    AIM_BLEND_STAND_WALK = 14,
    AIM_BLEND_STAND_RUN = 15,
    AIM_BLEND_COURCH_IDLE = 16,
    AIM_BLEND_CROUCH_WALK = 17,
    DEATH_YAW = 18
}

local e_menu = {
    PITCH = ui.find_menu_int("0Antiaim.pitch"),

    TARGET_YAW = ui.find_menu_int("0Antiaim.base_angle"),
    YAW = ui.find_menu_int("Antiaim.yaw_offset"),
    YAW_MOD = ui.find_menu_int("0Antiaim.yaw"),

    JITTER_RANGE = ui.find_menu_int("0Antiaim.range"),

    SPIN_RANGE = ui.find_menu_int("0Antiaim.range"),
    SPIN_SPEED = ui.find_menu_int("0Antiaim.speed"),

    DESYNC_TYPE = ui.find_menu_int("0Antiaim.desync"),

    DESYNC_RANGE = ui.find_menu_int("0Antiaim.desync_range"),
    INV_DESYNC_RANGE = ui.find_menu_int("0Antiaim.inverted_desync_range"),

    BODY_LEAN = ui.find_menu_int("0Antiaim.body_lean"),
    INV_BODY_LEAN = ui.find_menu_int("0Antiaim.inverted_body_lean"),

    FAKELAG_AMOUNT = ui.find_menu_int("Antiaim.fake_lag_limit")
}

--- @endregion

--- @region: color
local color_c = {}

--- @param: r: number
--- @param: g: number
--- @param: b: number
--- @param: a: number
--- @return: color
function color_c.new(r, g, b, a)
    return color(math.floor(r or 255), math.floor(g or 255), math.floor(b or 255), math.floor(a or 255))
end

--- @param: color_u: color
--- @return: <number, number, number, number>
function color_c.unpack(color_u)
    return color_u:r(), color_u:g(), color_u:b(), color_u:a()
end
function color_c.to_hex(color_u)
    local r, g, b = color_c.unpack(color_u)

    local rgb = (r * 0x10000) + (g * 0x100) + b

    return string.format("%x", rgb)
end
--- @endregion

--- @region: animation
local animation_data = {}
local animation = {}

--- @info: Lerp animation.
--- @param: start: any
--- @param: end_pos: any
--- @param: time: number
--- @return: number
function animation.lerp(start, end_pos, time)
    if (type(start) == "userdata") then
        local color_data = {0, 0, 0, 0}

        color_data[1] = animation.lerp(start:r(), end_pos:r(), time)
        color_data[2] = animation.lerp(start:g(), end_pos:g(), time)
        color_data[3] = animation.lerp(start:b(), end_pos:b(), time)
        color_data[4] = animation.lerp(start:a(), end_pos:a(), time)

        return color_c.new(table.unpack(color_data))
    end

    return (end_pos - start) * (globalvars.get_frametime() * time) + start
end

--- @param: name: string
--- @param: value: any
--- @param: time: number
--- @return: any
function animation.create(name, value, time)
    if (animation_data[name] == nil) then
        animation_data[name] = value
    end

    animation_data[name] = animation.lerp(animation_data[name], value, time)

    return animation_data[name]
end
--- @endregion

--- @region: math
--- @info: Returns x ^ y. (You can also use the expression x ^ y to compute this value.)
--- @param: x: number
--- @param: y: number
--- @return: number
function math.pow(x, y)
    return x ^ y
end

--- @info: Randomizes different numbers in the float type.
--- @param: min: number
--- @param: max: number
--- @return: number
function math.random_float(min, max)
    return math.random() * (max - min) + min
end

--- @info: Randomizes different numbers in the int type.
--- @param: min: number
--- @param: max: number
--- @return: number
function math.random_int(min, max)
    return math.floor(math.random() * (math.floor(max) - math.ceil(min) + 1)) + math.ceil(min)
end

--- @info: Round a number to the nearest precision, or none by default.
--- @param: number: number
--- @param: precision: number
--- @return: number
function math.round(number, precision)
    local mult = math.pow(10, (precision or 0))

    return math.floor(number * mult + 0.5) / mult
end
--- @endregion

--- @region: table
--- @info: Returns true if the table contains the value being searched for.
--- @param: search_table: table
--- @param: search_value: any
--- @return: boolean
function table.contains(search_table, search_value)
    for _, value in pairs(search_table) do
        if (search_value == value) then
            return true
        end
    end

    return false
end

--- @param: tbl: table
--- @return: number
function table.count(tbl)
    if (tbl == nil) then 
        return 0 
    end

    if (#tbl == 0) then 
        local count = 0

        for data in pairs(tbl) do 
            count = count + 1 
        end

        return count 
    end

    return #tbl
end
--- @endregion

--- @region: string
--- @param: self: string
--- @param: search: string
--- @return: number
function string.count(self, search)
    local count = 0

    for i = 1, #self do
        if (self:sub(i, i) == search) then
            count = count + 1
        end
    end

    return count
end

--- @param: self: string
--- @param: first_color: table
--- @param: second_color: table
--- @return: string
function string.gradient(self, first_color, second_color)
    local output = ""

    local len = #self - 1

    local rinc = (second_color[1] - first_color[1]) / len
    local ginc = (second_color[2] - first_color[2]) / len
    local binc = (second_color[3] - first_color[3]) / len
    local ainc = (second_color[4] - first_color[4]) / len

    for i = 1, len + 1 do
        output = output .. ("{%02x%02x%02x}%s"):format(math.floor(first_color[1]), math.floor(first_color[2]), math.floor(first_color[3]), self:sub(i, i))

        first_color[1] = first_color[1] + rinc
        first_color[2] = first_color[2] + ginc
        first_color[3] = first_color[3] + binc
        first_color[4] = first_color[4] + ainc
    end

    return output
end
--- @endregion

--- @region: callback system
local callbacks = {}
local data_calls = {}
local data_delay_calls = {}

--- @info: Initialize callbacks.
--- @param: event_type: string
--- @return: void
function callbacks.init(event_type)
    if (type(event_type) ~= "string") then
        cheat.notify("Invalid type of callback")
        return
    end

    data_calls[event_type] = {}
    data_calls[event_type].list = {}

    data_calls[event_type].func = function(...)
        for key, value in pairs(data_calls[event_type].list) do
            value.func(...)
        end
    end

    cheat.push_callback(event_type, data_calls[event_type].func)
end

--- @param: event_type: string
--- @param: callback: function
--- @return: void
function callbacks.add(event_type, callback)
    if (callback == nil) then
        cheat.notify("Undefined callbacked variable")
        return
    end

    if (type(callback) ~= "function") then
        cheat.notify("Invalid type of callbacked variable")
        return
    end

    if (not data_calls[event_type]) then
        callbacks.init(event_type)
    end

    table.insert(data_calls[event_type].list, {func = callback})
end

--- @info: Removes a callback that was previously set using 'callbacks.add'
--- @param: event_type: string
--- @param: callback: function
--- @return: void
function callbacks.remove(event_type, callback)
    if (data_calls[event_type] == nil) then
        cheat.notify("Undefined callback")
        return
    end

    if (type(callback) ~= "function") then
        cheat.notify("Invalid type of variable to unsetup")
        return
    end

    for key, value in pairs(data_calls[event_type].list) do
        if (value.func == callback) then
            table.remove(data_calls[event_type].list, key)

            return
        end
    end
end
--- @endregion

--- @region: menu helpers
local menu = {}
local menu_callbacks = {}

--- @param: item_type: string
--- @param: name: string
--- @param: item: menu_item
--- @param: func: function
--- @return: void
function menu.set_callback(item_type, name, item, func)
    callbacks.add("on_paint", function()
        if (menu_callbacks[name] == nil) then
            menu_callbacks[name] = {itmes = {}, data = {}, clicked_value = 0}
        end

        local self = menu_callbacks[name]

        if (item_type == "checkbox" or item_type == "hotkey") then
            self.clicked_value = item:get() and math.min(3, self.clicked_value + 1) or math.max(0, self.clicked_value - 1)

            if (self.clicked_value == 2) then
                func(item:get())
            end
        elseif (item_type == "combo") then
            local item_value = item:get()

            if (self.clicked_value and self.clicked_value == item_value) then
                goto skip
            end

            func(item_value)

            self.clicked_value = item_value
            ::skip::    
        elseif (item_type == "button") then
            if (item:get()) then
                func(item:get())
            end
        end
    end)
end
--- @endregion

--- @region: assets
--- @class: assets_c
--- @field: public: link: string
--- @field: public: path: string
local assets_c = {}
local assets_mt = { __index = assets_c }

--- @info: Instantiate an object of assets_c.
--- @info: Creating an http request.
--- @param: link: string
--- @return: assets_c
function assets_c.new_request(link)
    return setmetatable({
        link = link
    }, assets_mt)
end

--- @info: Downloading an asset.
--- @param: type: string
--- @param: name: string: optional
--- @param: path: string: optional
--- @return: void
function assets_c:download(type, name, path)
    assert(type ~= nil, 4, "Unknown resource type")

    local default_name = ("unknown_%s.%s"):format(math.random(0, 100000), type)
    local s_path = {first = "C:\\rawetrip", second = "assets"}
    local default_path = ("%s\\%s"):format(s_path.first, s_path.second)

    if (not file.exists(s_path.first)) then
        file.create_dir(s_path.first)
    end

    if (file.exists(s_path.first) and not file.exists(s_path.second)) then
        file.create_dir(default_path)
    end

    name = name == nil and default_name or ("%s.%s"):format(name, type)
    path = path == nil and default_path or path

    local total_path = ("%s\\%s"):format(path:gsub("/", "\\"), name)

    self.path = total_path:gsub("\\", "/")

    if (not file.exists(total_path)) then
        local content = http.get(self.link)

        if (content ~= nil and #content > 0) then
            file.write(total_path, content)
        end
    end
end

--- @info: Get asset path.
--- @return: string
function assets_c:get_path()
    return self.path
end

--- @endregion

--- @region: font
--- @class: font_c
--- @field: public: name: string
--- @field: public: size: string
--- @field: public: flags: table
local font_c = {}
local font_mt = { __index = font_c }

--- @info: Instantiate an object of font_c.
--- @param: name: string
--- @param: size: string
--- @param: flags: number: optional
--- @return: font_c
function font_c.new(name, size, flags)
    assert(name, 4, "Cannot create font because: %s", "attempt to call a nil value (local 'name')")
    assert(size, 4, "Cannot create font because: %s", "attempt to call a nil value (local 'size')")

    return setmetatable({
        name = name,
        size = size,
        flags = flags
    }, font_mt)
end

--- @info: Initialize font.
--- @return: font
function font_c:init()
    if (type(self.name) == "table") then
        local asset = assets_c.new_request(self.name.link)
        asset:download(self.name.type, self.name.name)

        local asset_path = asset:get_path()

        if (file.exists(asset_path:gsub("/", "\\"))) then
            return render.setup_font(asset_path, self.size, self.flags)
        end
    elseif (type(self.name) == "string") then
        return render.setup_font(self.name, self.size, self.flags)
    end
end
--- @endregion

--- @region: menu_item
--- @class: menu_item_c
--- @field: public: element_type: string
--- @field: public: name: string
local menu_item_c = {}
local menu_item_mt = { __index = menu_item_c }

--- @info: Create a new menu_item_c.
--- @param: element_type: string
--- @param: element: function
--- @param: name: string
--- @vararg: any
--- @return: menu_item_c
function menu_item_c.new(element_type, element, name, to_save, condition, ...)
    assert(element, 4, "Cannot create menu item because: %s", "attempt to call a nil value (local 'element')")

    local reference

    if (type(element) == "function") then
        local do_ui_new = element(name, ...)

        reference = do_ui_new
    else
        reference = element
    end

    return setmetatable({
        element_type = element_type,
        reference = reference,

        name = name,

        to_save = to_save,
        condition = condition,
    }, menu_item_mt)
end

--- @param: func: function
--- @return: void
function menu_item_c:set_callback(func)
    return menu.set_callback(self.element_type, ("%s_%s"):format(self.name, self.element_type), self.reference, func)
end

--- @vararg: any
--- @return: void
function menu_item_c:set(...)
    local args = {...}

    self.reference:set(table.unpack(args))
end

--- @vararg: string
--- @return: void
function menu_item_c:set_items(...)
    local args = {...}

    if (type(args[1]) == "table") then
        args = args[1]
    end

    self.reference:set_items(args)
end

--- @return: table
function menu_item_c:get_items()
    return self.reference:get_items()
end

--- @return: any
function menu_item_c:get()
    return self.reference:get()
end

--- @param: state: boolean
--- @return: any
function menu_item_c:set_visible(state)
    self.reference:set_visible(state)
end
--- @endregion

--- @region: menu_manager
--- @class: menu_manager_c
--- @field: public: tab: string
--- @field: public: name: string
--- @field: public: to_save: boolean
--- @field: public: condition: function
--- @field: public: reference: menu item
local menu_manager_c = {}
local menu_manager_mt = { __index = menu_manager_c }

local menu_manager_current_tab = ui.add_combobox("Current Tab:", {"Loading..."})
local menu_manager_tabs = {}
local menu_manager_items = {}
local current_tab = "string"

--- @info: Create a new menu_manager_c.
--- @param: tab: string
--- @param: name: string
--- @param: to_save: boolean: optional
--- @param: condition: function: optional
--- @return: menu_manager_c
function menu_manager_c.new(tab, name, to_save, condition)
    return setmetatable({
        tab = tab == nil and "Global" or tab,
        name = name,

        to_save = to_save == nil and true or to_save,
        condition = condition == nil and function()
            return true
        end or condition,
    }, menu_manager_mt)
end

--- @param: tab: string
--- @param: name: string
--- @return: menu_item_c
function menu_manager_c.reference(tab, group, name)
    return menu_manager_items[tab][group][name].reference
end

--- @vararg: string
--- @return: menu_item_c
function menu_manager_c:combo(...)
    local args = {...}

    if (type(args[1]) == "table") then
        args = args[1]
    end

    return self:_create_item("combo", ui.add_combobox, args)
end

--- @return: menu_item_c
function menu_manager_c:checkbox()
    local item = self:_create_item("checkbox", ui.add_checkbox)

    return item
end

--- @return: menu_item_c
function menu_manager_c:hotkey()
    local item = self:_create_item("hotkey", ui.add_hotkey)

    return item
end

--- @return: menu_item_c
function menu_manager_c:label()
    self.to_save = false

    return self:_create_item("label", ui.add_label)
end

--- @param: callback: function
--- @return: menu_item_c
function menu_manager_c:button(callback)
    self.to_save = false

    local item = self:_create_item("button", ui.add_button)
    
    if (callback ~= nil) then
        callbacks.add("on_paint", function()
            if (item:get()) then
                callback()
            end
        end)
    end

    return item
end

--- @param: s_type: string
--- @param: min: number
--- @param: max: number
--- @return: menu_item_c
function menu_manager_c:slider(min, max, default_value, s_type)
    if (s_type == nil) then
        s_type = "int"
    end

    if (type(min) ~= "number") then
        cheat.notify("Slider min value must be a number.")
        return
    end

    if (type(max) ~= "number") then
        cheat.notify("Slider max value must be a number.")
        return
    end

    if (min > max) then
        cheat.notify("Slider min value must be below the max value.")
        return
    end

    local item = self:_create_item("slider", ui["add_slider"..s_type], min, max)

    if (default_value ~= nil) then
        item.reference:set(default_value)
    end

    return item
end

--- @return: menu_item_c
function menu_manager_c:color_picker()
    return self:_create_item("color_picker", ui.add_colorpicker)
end

--- @return: void
function menu_manager_c.update_visible()
    for tab_name, tab_value in pairs(menu_manager_items) do
        for item_name, item_value in pairs(tab_value) do
            local tabs = menu_manager_current_tab:get_items()
            local condition = tabs[menu_manager_current_tab:get() + 1] == tab_name and item_value.condition()

            item_value.reference:set_visible(condition)
        end
    end
end

--- @param: element_type: string
--- @param: element: function
--- @vararg: any
--- @return: menu_item_c
function menu_manager_c:_create_item(element_type, element, ...)
    assert(type(self.name) == "string" and self.name ~= "", 3, "Cannot create menu item: name must be a non-empty string.")

    local item = menu_item_c.new(element_type, element, self.name, self.to_save, self.condition, ...)

    if (menu_manager_items[self.tab] == nil) then
        menu_manager_items[self.tab] = {}

        table.insert(menu_manager_tabs, self.tab)
        menu_manager_current_tab:set_items(menu_manager_tabs)
    end

    if (menu_manager_items[self.tab][self.name] ~= nil) then
        return
    end

    menu_manager_items[self.tab][self.name] = {
        reference = item,
        to_save = self.to_save,
        element_type = element_type,
        condition = self.condition
    }

    local function update_value()
        menu_manager_c.update_visible()
    end

    item:set_callback(update_value)
    update_value()

    menu_manager_c.update_visible()

    return item
end

menu.set_callback("combo", "menu_manager_current_tab", menu_manager_current_tab, function()
    menu_manager_c.update_visible()
end)
--- @endregion

--- @region: input
--- @class: input_c
--- @field: public: key: number
local input_c = {}
local input_mt = { __index = input_c }

local pressed_keys = {}
local last_pressed_keys = {}

--- @info: Instantiate an object of input_c.
--- @param: key: number
--- @return input_c
function input_c.new_key(key)
    return setmetatable({
        key = key,
    }, input_mt)
end

--- @info: Update keys.
--- @return: void
function input_c.update()
    for i = 1, 255 do 
        last_pressed_keys[i] = pressed_keys[i]
        pressed_keys[i] = utils.get_active_key(i)
    end
end
callbacks.add("on_paint", input_c.update)

--- @info: Returns if current key is held
--- @return: boolean
function input_c:is_key_held()
    return pressed_keys[self.key]
end

--- @info: Returns if they key was just pressed
--- @return: boolean
function input_c:is_key_pressed()
    return pressed_keys[self.key] and not last_pressed_keys[self.key]
end

--- @info: Returns if current key was just released
--- @return: boolean
function input_c:is_key_released()
    return not pressed_keys[self.key] and last_pressed_keys[self.key]
end

--- @info: Returns if the mouse is in specified bounds.
--- @param: vec_start: vector
--- @param: vec_end: vector
--- @return: boolean
function input_c.is_mouse_in_bounds(vec_start, vec_end)
    local x, y = vec_start.x, vec_start.y
    local w, h = vec_end.x, vec_end.y

    local mouse_position = utils.get_cursor_position()

    return ((mouse_position.x >= x and mouse_position.x < x + w and mouse_position.y >= y and mouse_position.y < y + h) and globalvars.is_open_menu()) 
end
--- @endregion

--- @region: drag
--- @class: drag_c
--- @field: public: x: number
--- @field: public: y: number
--- @field: public: width: number
--- @field: public: height: number
--- @field: public: d_x: number
--- @field: public: d_y: number
--- @field: public: dragging: boolean
--- @field: public: unlocked: boolean
local drag_c = {}
local drag_mt = { __index = drag_c }

--- @info: Instantiate an object of drag_c.
--- @param: x: number
--- @param: y: number
--- @return drag_c
function drag_c.new(x, y, name, tab)
    tab = tab or "Widgets"

    local screen = engine.get_screen_size()

    return setmetatable({
        x = menu_manager_c.new(tab, ("[ %s ] x"):format(name), true, function() return false end):slider(0, screen.x, x),
        y = menu_manager_c.new(tab, ("[ %s ] y"):format(name), true, function() return false end):slider(0, screen.y, y),

        d_x = 0,
        d_y = 0,

        dragging = false,
        unlocked = false
    }, drag_mt)
end

--- @info: Unlock the dragging position.
--- @return void
function drag_c:unlock()
    self.unlocked = true
end

--- @info: Lock the dragging position.
--- @return void
function drag_c:lock()
    self.unlocked = false
end

--- @return: void
function drag_c:visualize()
    local x, y = self.x:get(), self.y:get()
    local width, height = self.width, self.height

    if (input_c.is_mouse_in_bounds(vector_2d(x, y), vector_2d(width, height))) then
        render.rect(x, y, width, height, color_c.new(255, 255, 255, 100), 3)
    end
end

--- @info: Handle dragging.
--- @param: width: number
--- @param: height: number
--- @return: void
function drag_c:handle(width, height)
    self.width = width
    self.height = height

    local screen = engine.get_screen_size()
    local mouse_position = utils.get_cursor_position()
    local mouse_left = input_c.new_key(0x01)

    if (input_c.is_mouse_in_bounds(vector_2d(self.x:get(), self.y:get()), vector_2d(self.width, self.height))) then
        if (mouse_left:is_key_held() and not self.dragging) then
            self.dragging = true

            self.d_x = self.x:get() - mouse_position.x
            self.d_y = self.y:get() - mouse_position.y
        end
    end

    if (not mouse_left:is_key_held()) then 
        self.dragging = false
    end

    if (self.dragging and globalvars.is_open_menu()) then
        local new_x = math.max(0, math.min(screen.x - self.width, mouse_position.x + self.d_x))
        local new_y = math.max(0, math.min(screen.y - self.height, mouse_position.y + self.d_y))
        new_x = self.unlocked and mouse_position.x + self.d_x or new_x
        new_y = self.unlocked and mouse_position.y + self.d_y or new_y

        self.x:set(new_x)
        self.y:set(new_y)
    end
end

--- @info: Getting drag position.
--- @return: <number, number>
function drag_c:get()
    return self.x:get(), self.y:get()
end
--- @endregion

--- @region: render helpers
local render_c = {}

--- @param: x: number
--- @param: y: number
--- @param: color_s: color
--- @param: start_degrees: number
--- @param: percentage: number
--- @param: thickness: number
--- @return: void
function render_c.circle_outline(x, y, color_s, radius, start_degrees, percentage, thickness)
    local r, g, b, a = color_c.unpack(color_s)

    return render.arc(x, y, (radius - 1), (radius - 1) + thickness, start_degrees, math.floor(360 * percentage), color_c.new(r, g, b, a))
end

--- @param: font: font
--- @param: x: number
--- @param: y: number
--- @param: tbl: table
--- @param: alpha: number: optional
--- @param: shadow: boolean: optional
--- @param: outline: boolean: optional
--- @return: void
function render_c.multitext(font, x, y, tbl, alpha, shadow, outline)
    if (font == nil) then
        return
    end

    if ((x == nil and type(x) ~= "number") and (y == nil and type(y) ~= "number")) then
        return
    end

    if (alpha == nil) then
        alpha = 1
    end

    if (alpha < 0) then
        alpha = 0
    end

    if (shadow == nil) then
        shadow = false
    end

    if (outline == nil) then
        outline = false
    end

    for key, value in pairs(tbl) do
        value.color = value.color or color_c.new(255, 255, 255)

        render.text(font, x, y, color_c.new(value.color:r(), value.color:g(), value.color:b(), value.color:a() * alpha), value.text, shadow, outline)

        x = x + render.get_text_size(font, value.text).x
    end
end
--- @param: font: font
--- @param: tbl: table
--- @return: vector_2d
function render_c.get_multitext_size(font, tbl)
    if (font == nil) then
        return
    end

    local width = 0

    for key, value in pairs(tbl) do
        width = width + render.get_text_size(font, tostring(value.text)).x
    end

    return {x = width, y = font.size}
end
--- @param: x: number
--- @param: y: number
--- @param: width: number
--- @param: height: number
--- @param: container_color: color
--- @param: style: number: optional
--- @param: outline: boolean: optional
--- @return: void
function render_c.container(x, y, width, height, container_color, style, outline)
    if ((x == nil and type(x) ~= "number") and (y == nil and type(y) ~= "number")) then
        return
    end

    if ((width == nil and type(width) ~= "number") and (height == nil and type(height) ~= "number")) then
        return
    end

    if (outline == nil) then
        outline = true
    end

    if (style == nil) then
        style = 1
    end

    local r, g, b, a = color_c.unpack(container_color)

    local round = 3

    if (style == 1) then
        --- background
        render.blur(x, y, width, height, (255 / 255) * a)
        render.rect_filled(x, y, width, height, color_c.new(0, 0, 0, (80 / 255) * a), round + 1)

        --- upper
        render.rect_filled(x + round, y, width - (round * 2), 1, color_c.new(r, g, b, (255 / 255) * a))

        --- left up arc
        render.begin_cliprect(x, y, round, round + 1)
        render_c.circle_outline(x + round, y + round, color_c.new(r, g, b, (255 / 255) * a), round, 180, 0.25, 1)
        render.end_cliprect()

        --- right up arc
        render.begin_cliprect(x + width - round, y, round, round + 1)
        render_c.circle_outline(x + width - round, y + round, color_c.new(r, g, b, (255 / 255) * a), round, 270, 0.25, 1)
        render.end_cliprect()

        --- left gradient
        render.gradient(x, y + round, 1, (height + 5) / 2, color_c.new(r, g, b, (255 / 255) * a), color_c.new(r, g, b, (0 / 255) * a), 1)

        --- right gradient
        render.gradient(x + width - 1, y + round, 1, (height + 5) / 2, color_c.new(r, g, b, (255 / 255) * a), color_c.new(r, g, b, (0 / 255) * a), 1)
    end
end
--- @endregion

--- @region: entity helpers
local entity_c = {}

--- @return: number
function entity_c.get_condition()
    local player = entity.get_local()

    if (player == nil or not player:is_alive()) then
        return e_conditions.SHARED
    end

    local velocity = player:get_velocity():length_2d()

    local flags = player:get_prop_int("CBasePlayer", "m_fFlags")

    if (bit.band(flags, e_player_flags.ON_GROUND) ~= 1) then
        return e_conditions.AIR
    end

    if (bit.band(flags, e_player_flags.DUCKING) ~= 0) then
        return e_conditions.CROUCH
    end

    if (velocity > 5) then
        if (ui.get_keybind_state(keybinds.slowwalk)) then
            return e_conditions.WALKING
        end
        if (ui.get_keybind_state(keybinds.automatic_peek)) then
            return e_conditions.PEEKING
    end
        return e_conditions.RUNNING
    end
    return e_conditions.STANDING
end

--- @param: con_id: number
--- @return: string
function entity_c.get_condition_name(con_id)
    if (con_id == nil) then
        con_id = e_conditions.SHARED
    end
    
    for key, value in spairs(e_conditions, function(t, a, b)
        return t[b] > t[a]
    end) do
        if (value == con_id) then
            local new_name = key:lower()
            new_name = new_name:gsub("(%l)(%w*)", function(a, b)
                return a:upper() .. b
            end)

            return new_name
        end
    end
end

--- @endregion

--- @region: unnamed
--- @element: script vars
local script_name = "A1pha"
local script_color = color_c.new(255, 192, 118)

local console_c = {}

--- @param: first_color: color
--- @param: second_color: color
--- @vararg: string
--- @return: void
function console_c.print_gradient(first_color, second_color, ...)
    if (first_color == nil and second_color == nil) then
        return
    end

    local args = {...}
    local message = ""

    for _, value in pairs(args) do
        message = message .. value .. "\x20"
    end

    local count = #message - 1
    local color_s = {first_color[1], first_color[2], first_color[3], first_color[4] or 255}

    local delta = {
        -(first_color[1] - second_color[1]) / count,
        -(first_color[2] - second_color[2]) / count,
        -(first_color[3] - second_color[3]) / count,
        -(first_color[4] - second_color[4]) / count
    }

    for i = 1, #message - 1 do
        console.print_color(message:sub(i, i), color_c.new(color_s[1], color_s[2], color_s[3], color_s[4]))

        color_s[1] = color_s[1] + delta[1]
        color_s[2] = color_s[2] + delta[2]
        color_s[3] = color_s[3] + delta[3]
        color_s[4] = color_s[4] + delta[4]
    end
end

--- @param: message: string
--- @param: msg_color: color
--- @return: void
function console_c.log(message, msg_color)
    if (msg_color == nil) then
        msg_color = color_c.new(255, 255, 255)
    end

    message = message .. "\n"

    local hexed = color_c.to_hex(msg_color)
    message = ("{%s}%s"):format(hexed, message)

    console_c.print_gradient({158, 245, 255, 255}, {255, 255, 255, 255}, script_name)
    console.print_color(" : ", color_c.new(33, 33, 33))

    for i = 1, message:count("{") do
        local start_prefix = message:find("{")
        local end_prefix = message:find("}")

        if (start_prefix and end_prefix) then
            local string_color = message:sub(start_prefix, end_prefix)
            local r, g, b = tonumber("0x" .. string_color:sub(2, 3)), tonumber("0x" .. string_color:sub(4, 5)), tonumber("0x" .. string_color:sub(6, 7))

            local next_string = message:sub(end_prefix + 1)

            local next_prefix_start = next_string:find('{')
            local new_string = next_prefix_start and next_string:sub(1, next_prefix_start - 1) or next_string

            message = next_string

            console.print_color(new_string, color_c.new(r, g, b))
        end
    end
end
--- @endregion

--- @region: fonts
local fonts = {}

fonts.calibri = {}
fonts.verdana = {}
fonts.small = {}

fonts.verdana.default = render.setup_font("Verdana", 12, fontflags.noantialiasing)

fonts.verdana.bold = render.setup_font("verdanab", 12, fontflags.noantialiasing)
fonts.verdana.panel = render.setup_font("Verdana", 11, fontflags.noantialiasing)

fonts.calibri.default = render.setup_font("calibri", 24, fontflags.bold)

fonts.small.default = font_c.new({
    link = "https://cdn.discordapp.com/attachments/935910251562143764/1027915433929609256/smallest.ttf",
    type = "ttf",
    name = "smallest"
}, 10):init()

--- @endregion

--- @region: images
local images = {}
--- @endregion

--- @region: ui
--- @group: Ragebot
local ui_ragebot = {}

ui_ragebot.settings_label = menu_manager_c.new("Ragebot", "Settings"):label()
ui_ragebot.ideal_tick_helper = menu_manager_c.new("Ragebot", "Enable Ideal Tick Helper"):checkbox()
ui_ragebot.ideal_tick_helper_condition = function() return ui_ragebot.ideal_tick_helper:get() end
ui_ragebot.enable_freestand = menu_manager_c.new("Ragebot", "Enable Freestand", true, ui_ragebot.ideal_tick_helper_condition):checkbox()

ui_ragebot.other_label = menu_manager_c.new("Ragebot", "Other"):label()

ui_ragebot.baim_damage_switch = menu_manager_c.new("Ragebot", "Enable Baim Damage"):checkbox()
ui_ragebot.baim_damage = menu_manager_c.new("Ragebot", "Baim Damage", true, function() return ui_ragebot.baim_damage_switch:get() end):slider(1, 100, 30)
ui_ragebot.baim_cond_switch = menu_manager_c.new("Ragebot", "Enable Local Baim Conditions"):checkbox()
ui_ragebot.baim_cond_crouch = menu_manager_c.new("Ragebot", "On Crouching", true, function() return ui_ragebot.baim_cond_switch:get() end):checkbox()
ui_ragebot.baim_cond_peek = menu_manager_c.new("Ragebot", "At Peeking", true, function() return ui_ragebot.baim_cond_switch:get() end):checkbox()
ui_ragebot.baim_cond_walk = menu_manager_c.new("Ragebot", "On Walking", true, function() return ui_ragebot.baim_cond_switch:get() end):checkbox()

ui_ragebot.freestand_below_switch = menu_manager_c.new("Ragebot", "Enable Freestand Below X Health"):checkbox()
ui_ragebot.freestand_below = menu_manager_c.new("Ragebot", "Freestand Below X Health", true, function() return ui_ragebot.freestand_below_switch:get() end):slider(1, 100)

ui_ragebot.double_tap_label = menu_manager_c.new("Ragebot", "Double Tap"):label()

ui_ragebot.full_stop = menu_manager_c.new("Ragebot", "Enable Fullstop On Shot"):checkbox()
ui_ragebot.full_stop_condition = function() return ui_ragebot.full_stop:get() end
ui_ragebot.only_on_dt = menu_manager_c.new("Ragebot", "Only On DT", true, ui_ragebot.full_stop_condition):checkbox()
ui_ragebot.stop_time = menu_manager_c.new("Ragebot", "Stop Time", true, ui_ragebot.full_stop_condition):slider(5, 35)

--- @group: Anti-Aim
local ui_anti_aim = {}

ui_ragebot.main_label = menu_manager_c.new("Anti-Aim", "Main"):label()

ui_anti_aim.preset = menu_manager_c.new("Anti-Aim", "Preset"):combo("None", "Aggressive", "Alternative", "Smart", "Dynamic", "Fake Jitter","Progressive","Condition")

ui_anti_aim.roll_angles = menu_manager_c.new("Anti-Aim", "Enable Roll Angles"):checkbox()
ui_anti_aim.roll_angles_condition = function() return ui_anti_aim.roll_angles:get() end
ui_anti_aim.roll_value = menu_manager_c.new("Anti-Aim", "Roll Angles Value", true, ui_anti_aim.roll_angles_condition):slider(-90, 90, 45)
ui_anti_aim.static_legs_in_air = menu_manager_c.new("Anti-Aim", "Static Legs"):checkbox()
ui_anti_aim.zero_pitch_on_land = menu_manager_c.new("Anti-Aim", "Pitch Zero On Land"):checkbox()
ui_anti_aim.slowwalk_mod = menu_manager_c.new("Anti-Aim", "Enable Slowwalk Modifier"):checkbox()
ui_anti_aim.slowwalk_mod_condition = function() return ui_anti_aim.slowwalk_mod:get() end
ui_anti_aim.slowwalk_mod_value = menu_manager_c.new("Anti-Aim", "Slowwalk Modifier Value", true, ui_anti_aim.slowwalk_mod_condition):slider(0, 100, 100)
ui_anti_aim.fake_lag_label = menu_manager_c.new("Anti-Aim", "Fake Lag"):label()

ui_anti_aim.fake_lag = menu_manager_c.new("Anti-Aim", "Enable Fake Lag"):checkbox()
ui_anti_aim.fake_lag_condition = function() return ui_anti_aim.fake_lag:get() end
ui_anti_aim.fake_lag_types = menu_manager_c.new("Anti-Aim", "Fake Lag Types", true, ui_anti_aim.fake_lag_condition):combo("Offset", "Switch")
ui_anti_aim.start_limit = menu_manager_c.new("Anti-Aim", "Start Limit", true, ui_anti_aim.fake_lag_condition):slider(1, 17, 2)
ui_anti_aim.max_choke = menu_manager_c.new("Anti-Aim", "Max Choke", true, ui_anti_aim.fake_lag_condition):slider(1, 17, 14)
ui_anti_aim.update_speed = menu_manager_c.new("Anti-Aim", "Update Speed", true, function() return (ui_anti_aim.fake_lag_types:get() ~= 1) and ui_anti_aim.fake_lag:get() end):slider(1, 10, 4)
ui_anti_aim.auto_disable = menu_manager_c.new("Anti-Aim", "Auto Disable", true, ui_anti_aim.fake_lag_condition):checkbox()

--- @group: Visuals
local ui_visuals = {}

ui_visuals.main_label = menu_manager_c.new("Visuals", "Main"):label()

ui_visuals.crosshair_indicators = menu_manager_c.new("Visuals", "Crosshair Indicators"):combo("Disabled", "Default", "Beta")
ui_visuals.crosshair_indicators_condition = function() return ui_visuals.crosshair_indicators:get() ~= 0 end
ui_visuals.first_color = menu_manager_c.new("Visuals", "First Color", true, ui_visuals.crosshair_indicators_condition):color_picker()
ui_visuals.second_color = menu_manager_c.new("Visuals", "Second Color", true, ui_visuals.crosshair_indicators_condition):color_picker()
ui_visuals.panel = menu_manager_c.new("Visuals", "User Panel"):checkbox()
ui_visuals.scope = menu_manager_c.new("Visuals", "Viewmodel in scope"):checkbox()
ui_visuals.sense_indicators = menu_manager_c.new("Visuals", "Skeet Indicators"):checkbox()

ui_visuals.watermark = menu_manager_c.new("Visuals", "Watermark"):checkbox()
ui_visuals.keybinds = menu_manager_c.new("Visuals", "Keybinds"):checkbox()

ui_visuals.accent_condition = function() return ui_visuals.watermark:get() or ui_visuals.keybinds:get() end
ui_visuals.accent = menu_manager_c.new("Visuals", "Accent", true, ui_visuals.accent_condition):color_picker()

local ui_misc = {}

ui_misc.seperator = menu_manager_c.new("Misc",""):label()
ui_misc.fps_boost = menu_manager_c.new("Misc", "Fps Boost", "Click to Apply (Applies it for only once)"):button(function()
    console.set_int("cl_disablefreezecam", 1)
    console.set_int("cl_disablehtmlmotd", 1)
    console.set_int("r_dynamic", 0)
    console.set_int( "r_3dsky", 0 )
    console.set_int( "r_shadows", 0 )
    console.set_int( "cl_csm_static_prop_shadows", 0 )
    console.set_int( "cl_csm_shadows", 0 )
    console.set_int( "cl_csm_world_shadows", 0 )
    console.set_int( "cl_foot_contact_shadows", 0 )
    console.set_int( "cl_csm_viewmodel_shadows", 0 )
    console.set_int( "cl_csm_rope_shadows", 0 )
    console.set_int( "cl_csm_sprite_shadows", 0 )
    console.set_int( "cl_disablefreezecam", 0 )
    console.set_int( "cl_freezecampanel_position_dynamic", 0 )
    console.set_int( "cl_freezecameffects_showholiday", 0 )
    console.set_int( "cl_showhelp", 0 )
    console.set_int( "cl_autohelp", 0 )
    console.set_int( "cl_disablehtmlmotd", 0 )
    console.set_int( "mat_postprocess_enable", 0 )
    console.set_int( "fog_enable_water_fog", 0 )
    console.set_int( "gameinstructor_enable", 0 )
    console.set_int( "cl_csm_world_shadows_in_viewmodelcascade", 0 )
    console.set_int( "cl_disable_ragdolls", 0 )
end)

local ui_global = {}

ui_global.seperator = menu_manager_c.new("Discord Link",""):label()

ui_global.discord_link = menu_manager_c.new("Discord Link", "Export to Console", "Export to console"):button(function()
    local link = ("fJtVP6YmAq"):gradient({158, 245, 255, 255}, {255, 255, 255, 255})
    console.execute_client_cmd("clear")
    console.execute_client_cmd("showconsole")
    console_c.log("https://discord.gg/" .. link)
end)
--- @endregion

--- @region: ragebot
--- @item: Main Functions

local ragebot_c = {}

ragebot_c.override_damage = false
ragebot_c.override_angle = false

ragebot_c.damage = {}

function ragebot_c.handle()
    local player = entity.get_local()
    if (player == nil or not player:is_alive()) then
        return
    end
    local condition = entity_c.get_condition()

    local ideal_tick = (ui_ragebot.ideal_tick_helper:get() and ui_ragebot.enable_freestand:get()) and ui.get_keybind_state(keybinds.automatic_peek)

    local player_health = player:get_health() < ui_ragebot.freestand_below:get()
    if not ui_ragebot.baim_cond_switch:get() then
        return
    end
    if (condition == e_conditions.CROUCH) and (ui_ragebot.baim_cond_crouch:get()) then
        ui.set_keybind_state(keybinds.body_aim, true)
    end
    if (condition == e_conditions.PEEKING) and (ui_ragebot.baim_cond_peek:get()) then
        ui.set_keybind_state(keybinds.body_aim, true)
    end
    if (condition == e_conditions.WALKING) and (ui_ragebot.baim_cond_walk:get()) then
        ui.set_keybind_state(keybinds.body_aim, true)
    end
    local baim_state = ui.get_keybind_state(keybinds.body_aim)
    local baim_damage = ui_ragebot.baim_damage:get()
        if (player_health or ideal_tick) then
            if (not ragebot_c.override_angle) then
                ragebot_c.angle = ui.find_menu_int("0Antiaim.base_angle"):get()
                
                ui.find_menu_int("0Antiaim.base_angle"):set(0)
                
                ragebot_c.override_angle = true
            end
            
            ui.find_menu_bool("Antiaim.freestand"):set(true)

    elseif (ragebot_c.override_angle) then
                ui.find_menu_int("0Antiaim.base_angle"):set(ragebot_c.angle)
                
                ragebot_c.override_angle = false
            
            
           ui.find_menu_bool("Antiaim.freestand"):set(false)
        end
    if (baim_state) then
        if (not ragebot_c.override_damage and ui_ragebot.baim_damage_switch:get()) then
            for i = 1, 7 do
                local var_damage = ui.find_menu_int(("%sRagebot.minimum_visible_damage"):format(i - 1))
                ragebot_c.damage[i] = var_damage:get()

                var_damage:set(baim_damage)
            end

            ragebot_c.override_damage = true
        end
    else
        if (ragebot_c.override_damage and ui_ragebot.baim_damage_switch:get()) then
            for i = 1, 7 do
                local var_damage = ui.find_menu_int(("%sRagebot.minimum_visible_damage"):format(i - 1))

                var_damage:set(ragebot_c.damage[i])
            end

            ragebot_c.override_damage = false
        end
    end
    if not ui_ragebot.baim_cond_switch:get() then
        return
    end
end
function scope(stage)
if not ui_visuals.scope:get() then
    console.set_int("fov_cs_debug", 0)
    return
end
if (stage ~= enum_frames.frame_render_start) then
    return
end

if (not engine.is_in_game()) then
    return
end

local player = entity.get_local()

if (player == nil or not player:is_alive()) then
    return
end

local weapon = entity.get_weapon_by_player(player)

if (weapon == nil) then
    return
end

if (not weapon:is_non_aim() and player:is_scoped()) then
    local third_person = ui.get_keybind_state(keybinds.thirdperson)

    console.set_int("fov_cs_debug", third_person and 0 or 90)
else
    console.set_int("fov_cs_debug", 0)
end
end
--- @item: Fullstop On Shot
local double_tap_c = {}

double_tap_c.shot_timer = 0

function double_tap_c.shot_event(event)
    double_tap_c.shot_timer = 1
end

function double_tap_c.handle(cmd)
    if (not ui_ragebot.full_stop:get()) then
        return
    end

    local player = entity.get_local()

    if (player == nil or not player:is_alive()) then
        return
    end

    if (double_tap_c.shot_timer >= 1 and double_tap_c.shot_timer <= ui_ragebot.stop_time:get()) then
        double_tap_c.shot_timer = double_tap_c.shot_timer + 1
    else
        double_tap_c.shot_timer = 0
    end

    if (double_tap_c.shot_timer >= 1) then
        if (not ui_ragebot.only_on_dt:get()) then
            cmd.forwardmove = 0
            cmd.sidemove = 0

            return
        end

        if (ui.get_keybind_state(keybinds.double_tap) or globalvars.get_dt_recharging()) then
            cmd.forwardmove = 0
            cmd.sidemove = 0
        end
    end
end
--- @endregion

--- @region: anti-aim
local anti_aim_c = {}

anti_aim_c.should_roll = false

--- @item: Condition Anti-Aim Preset
local condition_anti_aim = {}

condition_anti_aim.list = {}

condition_anti_aim.switch = false
condition_anti_aim.yaw = 0

condition_anti_aim.fake_switch = false
condition_anti_aim.fake = 0

condition_anti_aim.mode_condition = function() return ui_anti_aim.preset:get() == 7 end
condition_anti_aim.conditions = menu_manager_c.new("Anti-Aim", "Current Condition", true, condition_anti_aim.mode_condition):combo("none")

function condition_anti_aim.update_items()
    local conditions = {}

    for key, value in spairs(e_conditions, function(t, a, b)
        return t[b] > t[a]
    end) do
        local new_name = key:lower()
        new_name = new_name:gsub("(%l)(%w*)", function(a, b)
            return a:upper() .. b
        end)

        table.insert(conditions, new_name)
    end

    condition_anti_aim.conditions:set_items(conditions)
end; condition_anti_aim.update_items()

function condition_anti_aim.create_items()
    local items = condition_anti_aim.conditions.reference:get_items()

    for key, value in ipairs(items) do
        local function name(text)
            return ("[%s] %s"):format(value, text)
        end

        if (condition_anti_aim.list[key] == nil) then
            condition_anti_aim.list[key] = {}
        end

        local setup = condition_anti_aim.list[key]

        setup.override = menu_manager_c.new("Anti-Aim", name("Override"), true, function()
            return condition_anti_aim.conditions:get() == key-1 and condition_anti_aim.mode_condition()
        end):checkbox()

        local function visible_condition()
            return condition_anti_aim.conditions:get() == key-1 and condition_anti_aim.mode_condition() and setup.override:get()
        end

        setup.pitch = menu_manager_c.new("Anti-Aim", name("Pitch"), true, visible_condition):combo("None", "Minimal", "Maximal")
        
        setup.target_yaw = menu_manager_c.new("Anti-Aim", name("Target Yaw"), true, visible_condition):combo("Local View", "At Targets")

        setup.yaw_offset_type = menu_manager_c.new("Anti-Aim", name("Yaw Add Type"), true, visible_condition):combo("Default", "Brute")
        setup.yaw_offset = menu_manager_c.new("Anti-Aim", name("Yaw Offset"), true, function() return visible_condition() and setup.yaw_offset_type:get() == 0 end):slider(-180, 180, 0)

        setup.yaw_offset_left = menu_manager_c.new("Anti-Aim", name("Yaw Offset Left"), true, function() return visible_condition() and setup.yaw_offset_type:get() == 1 end):slider(-180, 180, 0)
        setup.yaw_offset_right = menu_manager_c.new("Anti-Aim", name("Yaw Offset Right"), true, function() return visible_condition() and setup.yaw_offset_type:get() == 1 end):slider(-180, 180, 0)

        setup.yaw_jitter = menu_manager_c.new("Anti-Aim", name("Yaw Jitter"), true, visible_condition):combo("Off", "Jitter", "Offset", "Center", "Random")
        setup.yaw_jitter_value = menu_manager_c.new("Anti-Aim", name("Yaw Jitter Value"), true, function() return visible_condition() and setup.yaw_jitter:get() ~= 0 and setup.yaw_jitter:get() ~= 1 end):slider(-180, 180, 0)
        setup.yaw_jitter_range = menu_manager_c.new("Anti-Aim", name("Yaw Jitter Range"), true, function() return visible_condition() and setup.yaw_jitter:get() == 1 end):slider(0, 180)

        setup.desync_type = menu_manager_c.new("Anti-Aim", name("Desync Type"), true, visible_condition):combo("None", "Static", "Jitter")

        setup.desync_range = menu_manager_c.new("Anti-Aim", name("Desync Range"), true, function() return visible_condition() and setup.desync_type:get() ~= 0 end):slider(1, 60)
        setup.inv_desync_range = menu_manager_c.new("Anti-Aim", name("Inverted Desync Range"), true, function() return visible_condition() and setup.desync_type:get() ~= 0 end):slider(1, 60)

        setup.body_lean = menu_manager_c.new("Anti-Aim", name("Body Lean"), true, function() return visible_condition() and setup.desync_type:get() ~= 0 end):slider(1, 100)
        setup.inv_body_lean = menu_manager_c.new("Anti-Aim", name("Inverted Body Lean"), true, function() return visible_condition() and setup.desync_type:get() ~= 0 end):slider(1, 100)
    end
end; condition_anti_aim.create_items()

function condition_anti_aim.handle()
    if (ui_anti_aim.preset:get() ~= 7) then
        return
    end

    local player = entity.get_local()

    if (player == nil or not player:is_alive()) then
        return
    end

    local current_condition = entity_c.get_condition()
    local item = condition_anti_aim.list[current_condition]
    item = condition_anti_aim.list[e_conditions.SHARED].override:get() and condition_anti_aim.list[e_conditions.SHARED] or item

    if (not item.override:get()) then
        return
    end

    e_menu.PITCH:set(item.pitch:get())

    e_menu.TARGET_YAW:set(item.target_yaw:get())
local is_left = ui.get_keybind_state(keybinds.flip_desync)

    local yaw_offset = 0
    if (item.yaw_offset_type:get() == 0) then
        yaw_offset = item.yaw_offset:get()
    elseif (item.yaw_offset_type:get() == 1) then
        yaw_offset = is_left and item.yaw_offset_left:get() or item.yaw_offset_right:get()
    end

    e_menu.PITCH:set(item.pitch:get())

    if (utils.get_choked_commands() == 0) then
        if (item.yaw_jitter:get() ~= 0 and item.yaw_jitter:get() ~= 1) then
            e_menu.YAW_MOD:set(0)

            if (item.yaw_jitter:get() == 2) then
                if (condition_anti_aim.switch) then
                    condition_anti_aim.yaw = yaw_offset
                else
                    condition_anti_aim.yaw = yaw_offset + item.yaw_jitter_value:get()
                end

                condition_anti_aim.switch = not condition_anti_aim.switch
            elseif (item.yaw_jitter:get() == 3) then
                if (condition_anti_aim.switch) then
                    condition_anti_aim.yaw = yaw_offset - item.yaw_jitter_value:get() / 2
                else
                    condition_anti_aim.yaw = yaw_offset + item.yaw_jitter_value:get() / 2
                end

                condition_anti_aim.switch = not condition_anti_aim.switch
            elseif (item.yaw_jitter:get() == 4) then
                condition_anti_aim.yaw = yaw_offset + math.random_int(item.yaw_jitter_value:get() / -2, item.yaw_jitter_value:get() / 2)
            end
        end
        if (item.yaw_jitter:get() == 1) then
            e_menu.YAW_MOD:set(1)
            e_menu.JITTER_RANGE:set(item.yaw_jitter_range:get())
            condition_anti_aim.yaw = yaw_offset
        end
        e_menu.YAW:set(math.floor(condition_anti_aim.yaw or 0))
    end
    e_menu.DESYNC_TYPE:set(item.desync_type:get())
    if (item.desync_type:get() ~= 0) then
        e_menu.DESYNC_RANGE:set(item.desync_range:get())
        e_menu.INV_DESYNC_RANGE:set(item.inv_desync_range:get())

        e_menu.BODY_LEAN:set(item.body_lean:get())
        e_menu.INV_BODY_LEAN:set(item.inv_body_lean:get())
    end
end

--- @item: Preset
local preset_c = {}

function preset_c.handle()
    local player = entity.get_local()

    if (player == nil or not player:is_alive()) then
        return
    end

    local function set_int(var, value)
        local fvar = ui.find_menu_int(var)

        fvar:set(value)
    end

    local condition = entity_c.get_condition()

    --- "None", "Aggressive", "Alternative", "Smart", "Dynamic", "Fake Jitter"

    if (ui_anti_aim.preset:get() == 1) then
        set_int("0Antiaim.pitch", 1)

        if (condition == e_conditions.STANDING) then
            set_int("0Antiaim.base_angle", 0)
            set_int("Antiaim.yaw_offset", ui.get_keybind_state(keybinds.flip_desync) and -22 or 27)
            set_int("0Antiaim.yaw", 0)
            set_int("0Antiaim.range", 0)
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", ui.get_keybind_state(keybinds.flip_desync) and -19 or 60)
            set_int("0Antiaim.inverted_desync_range", ui.get_keybind_state(keybinds.flip_desync) and 60 or -14)
            set_int("0Antiaim.body_lean", ui.get_keybind_state(keybinds.flip_desync) and -11 or 64)
            set_int("0Antiaim.inverted_body_lean", ui.get_keybind_state(keybinds.flip_desync) and 69 or -100)

            anti_aim_c.should_roll = true
        elseif (condition == e_conditions.RUNNING) then
            set_int("0Antiaim.base_angle", 0)
            set_int("Antiaim.yaw_offset", 0)
            set_int("0Antiaim.yaw", 0)
            set_int("0Antiaim.range", 0)
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", ui.get_keybind_state(keybinds.flip_desync) and -60 or 60)
            set_int("0Antiaim.inverted_desync_range", ui.get_keybind_state(keybinds.flip_desync) and 26 or -29)
            set_int("0Antiaim.body_lean", ui.get_keybind_state(keybinds.flip_desync) and 0 or -48)
            set_int("0Antiaim.inverted_body_lean", ui.get_keybind_state(keybinds.flip_desync) and 100 or 0)

            anti_aim_c.should_roll = false
        elseif (condition == e_conditions.WALKING) then
            set_int("0Antiaim.base_angle", 0)
            set_int("Antiaim.yaw_offset", ui.get_keybind_state(keybinds.flip_desync) and -20 or 7)
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.range", math.random(1, 4))
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", ui.get_keybind_state(keybinds.flip_desync) and -35 or 17)
            set_int("0Antiaim.inverted_desync_range", ui.get_keybind_state(keybinds.flip_desync) and 60 or -60)
            set_int("0Antiaim.body_lean", ui.get_keybind_state(keybinds.flip_desync) and 100 or -100)
            set_int("0Antiaim.inverted_body_lean", ui.get_keybind_state(keybinds.flip_desync) and 100 or 33)

            anti_aim_c.should_roll = true
        else
            set_int("0Antiaim.base_angle", 1)
            set_int("Antiaim.yaw_offset", 0)
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.range", 20)
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", 60)
            set_int("0Antiaim.inverted_desync_range", 43)
            set_int("0Antiaim.body_lean", -4)
            set_int("0Antiaim.inverted_body_lean", -6)

            anti_aim_c.should_roll = false
        end
    elseif (ui_anti_aim.preset:get() == 2) then
        set_int("0Antiaim.pitch", 1)

        if (condition == e_conditions.STANDING) then
            set_int("0Antiaim.base_angle", 0)
            set_int("Antiaim.yaw_offset", ui.get_keybind_state(keybinds.flip_desync) and -20 or 20)
            set_int("0Antiaim.yaw", 0)
            set_int("0Antiaim.range", 0)
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", ui.get_keybind_state(keybinds.flip_desync) and 30 or 27)
            set_int("0Antiaim.inverted_desync_range", ui.get_keybind_state(keybinds.flip_desync) and 60 or 48)
            set_int("0Antiaim.body_lean", 100)
            set_int("0Antiaim.inverted_body_lean", 100)

            anti_aim_c.should_roll = false
        elseif (condition == e_conditions.RUNNING) then
            set_int("0Antiaim.base_angle", 1)
            set_int("Antiaim.yaw_offset", 0)
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.range", ui.get_keybind_state(keybinds.flip_desync) and 57 or 48)
            set_int("0Antiaim.desync", 1)
            set_int("0Antiaim.desync_range", 30)
            set_int("0Antiaim.inverted_desync_range", 30)
            set_int("0Antiaim.body_lean", 14)
            set_int("0Antiaim.inverted_body_lean", 19)

            anti_aim_c.should_roll = false
        elseif (condition == e_conditions.WALKING) then
            set_int("0Antiaim.base_angle", 0)
            set_int("Antiaim.yaw_offset", 0)
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.range", 8)
            set_int("0Antiaim.desync", 1)
            set_int("0Antiaim.desync_range", 32)
            set_int("0Antiaim.inverted_desync_range", 26)
            set_int("0Antiaim.body_lean", 29)
            set_int("0Antiaim.inverted_body_lean", 40)

            anti_aim_c.should_roll = true
        else
            set_int("0Antiaim.base_angle", 1)
            set_int("Antiaim.yaw_offset", 0)
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.range", 20)
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", 60)
            set_int("0Antiaim.inverted_desync_range", 43)
            set_int("0Antiaim.body_lean", -4)
            set_int("0Antiaim.inverted_body_lean", -6)

            anti_aim_c.should_roll = false
        end
    elseif (ui_anti_aim.preset:get() == 3) then
        set_int("0Antiaim.pitch", 1)

        if (condition == e_conditions.STANDING) then
            set_int("0Antiaim.base_angle", 0)
            set_int("Antiaim.yaw_offset", math.random(-10,10))
            set_int("0Antiaim.yaw", 0)
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", 25)
            set_int("0Antiaim.inverted_desync_range", 33)
            set_int("0Antiaim.body_lean", math.random(22,30))
            set_int("0Antiaim.inverted_body_lean",math.random(22,30))
        elseif (condition == e_conditions.RUNNING) then
            set_int("0Antiaim.base_angle", 1)
            set_int("Antiaim.yaw_offset", 0)
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.range", math.random(12,22))
            set_int("0Antiaim.desync", 1)
            set_int("0Antiaim.desync_range", math.random(45,49))
            set_int("0Antiaim.inverted_desync_range", math.random(13,37))
            set_int("0Antiaim.body_lean", math.random(0,30))
            set_int("0Antiaim.inverted_body_lean", math.random(70,100))
        elseif (condition == e_conditions.AIR) then
            set_int("0Antiaim.base_angle", 1)
            set_int("Antiaim.yaw_offset", 0)
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.range", math.random(2,6))
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", 30)
            set_int("0Antiaim.inverted_desync_range", 30)
            set_int("0Antiaim.body_lean", math.random(2,6))
            set_int("0Antiaim.inverted_body_lean", math.random(2,6))
        elseif (condition == e_conditions.WALKING) then
            set_int("0Antiaim.base_angle", 0)
            set_int("Antiaim.yaw_offset", ui.get_keybind_state(keybinds.flip_desync) and -17 or 17)
            set_int("0Antiaim.yaw", 0)
            set_int("0Antiaim.desync", 1)
            set_int("0Antiaim.desync_range", 17)
            set_int("0Antiaim.inverted_desync_range", 17)
            set_int("0Antiaim.body_lean", 60)
            set_int("0Antiaim.inverted_body_lean", 64)
        else
            set_int("0Antiaim.base_angle", 1)
            set_int("Antiaim.yaw_offset", 0)
            set_int("0Antiaim.yaw", 2)
            set_int("0Antiaim.range", 27)
            set_int("0Antiaim.speed", 3)
            set_int("0Antiaim.desync", 1)
            set_int("0Antiaim.desync_range", 25)
            set_int("0Antiaim.inverted_desync_range", 34)
            set_int("0Antiaim.body_lean", 100)
            set_int("0Antiaim.inverted_body_lean", 0)
        end
    elseif (ui_anti_aim.preset:get() == 4) then
        set_int("0Antiaim.pitch", 1)

        if (condition == e_conditions.STANDING) then
            set_int("Antiaim.yaw_offset", math.random(-13,7))
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", 25)
            set_int("0Antiaim.inverted_desync_range", 33)
            set_int("0Antiaim.body_lean", math.random(32,42))
            set_int("0Antiaim.inverted_body_lean",math.random(13,27))
        elseif (condition == e_conditions.RUNNING) then
            set_int("Antiaim.yaw_offset", 0)
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.range", math.random(7,16))
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", math.random(23,35))
            set_int("0Antiaim.inverted_desync_range", math.random(23,35))
            set_int("0Antiaim.body_lean", math.random(5,11))
            set_int("0Antiaim.inverted_body_lean", math.random(43,64))
        elseif (condition == e_conditions.AIR) then
            set_int("Antiaim.yaw_offset", 0)
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.range", math.random(16,34))
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", 53)
            set_int("0Antiaim.inverted_desync_range", 53)
            set_int("0Antiaim.body_lean", math.random(1,9))
            set_int("0Antiaim.inverted_body_lean", math.random(1,9))
        elseif (condition == e_conditions.WALKING) then
            set_int("Antiaim.yaw_offset", ui.get_keybind_state(keybinds.flip_desync) and -17 or 17)
            set_int("0Antiaim.yaw", 0)
            set_int("0Antiaim.desync", 1)
            set_int("0Antiaim.desync_range", 17)
            set_int("0Antiaim.inverted_desync_range", 17)
            set_int("0Antiaim.body_lean", 60)
            set_int("0Antiaim.inverted_body_lean", 64)
        else
            set_int("Antiaim.yaw_offset", 0)
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.range", 27)
            set_int("0Antiaim.desync", 1)
            set_int("0Antiaim.desync_range", 25)
            set_int("0Antiaim.inverted_desync_range", 34)
            set_int("0Antiaim.body_lean", 100)
            set_int("0Antiaim.inverted_body_lean", 0)
        end
    elseif (ui_anti_aim.preset:get() == 5) then
        set_int("0Antiaim.pitch", 1)

        if (condition == e_conditions.STANDING) then
            set_int("Antiaim.yaw_offset", 0)
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.range", 2)
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", ui.get_keybind_state(keybinds.flip_desync) and 42 or 22)
            set_int("0Antiaim.inverted_desync_range", ui.get_keybind_state(keybinds.flip_desync) and 58 or 43)
            set_int("0Antiaim.body_lean", ui.get_keybind_state(keybinds.flip_desync) and -5 or 100)
            set_int("0Antiaim.inverted_body_lean", ui.get_keybind_state(keybinds.flip_desync) and -5 or 100)
        elseif (condition == e_conditions.RUNNING) then
            set_int("Antiaim.yaw_offset", 0)
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.range", 2)
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", ui.get_keybind_state(keybinds.flip_desync) and 42 or 22)
            set_int("0Antiaim.inverted_desync_range", ui.get_keybind_state(keybinds.flip_desync) and 58 or 43)
            set_int("0Antiaim.body_lean", ui.get_keybind_state(keybinds.flip_desync) and -5 or 100)
            set_int("0Antiaim.inverted_body_lean", ui.get_keybind_state(keybinds.flip_desync) and -5 or 100)
        elseif (condition == e_conditions.AIR) then
            set_int("Antiaim.yaw_offset", 0)
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.range", 2)
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", ui.get_keybind_state(keybinds.flip_desync) and 42 or 22)
            set_int("0Antiaim.inverted_desync_range", ui.get_keybind_state(keybinds.flip_desync) and 58 or 43)
            set_int("0Antiaim.body_lean", ui.get_keybind_state(keybinds.flip_desync) and -5 or 100)
            set_int("0Antiaim.inverted_body_lean", ui.get_keybind_state(keybinds.flip_desync) and -5 or 100)
        elseif (condition == e_conditions.WALKING) then
            set_int("Antiaim.yaw_offset", 0)
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.range", 2)
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", ui.get_keybind_state(keybinds.flip_desync) and 42 or 22)
            set_int("0Antiaim.inverted_desync_range", ui.get_keybind_state(keybinds.flip_desync) and 58 or 43)
            set_int("0Antiaim.body_lean", ui.get_keybind_state(keybinds.flip_desync) and -5 or 100)
            set_int("0Antiaim.inverted_body_lean", ui.get_keybind_state(keybinds.flip_desync) and -5 or 100)
        else
            set_int("Antiaim.yaw_offset", 0)
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.range", 2)
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", ui.get_keybind_state(keybinds.flip_desync) and 42 or 22)
            set_int("0Antiaim.inverted_desync_range", ui.get_keybind_state(keybinds.flip_desync) and 58 or 43)
            set_int("0Antiaim.body_lean", ui.get_keybind_state(keybinds.flip_desync) and -5 or 100)
            set_int("0Antiaim.inverted_body_lean", ui.get_keybind_state(keybinds.flip_desync) and -5 or 100)
        end
    elseif (ui_anti_aim.preset:get() == 6) then
        set_int("0Antiaim.pitch", 1)
    
        if (condition == e_conditions.STANDING) then 
            set_int("Antiaim.yaw_offset", 0)
            set_int("0Antiaim.yaw", 0)
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", 40)
            set_int("0Antiaim.inverted_desync_range", 48)
            set_int("0Antiaim.body_lean", 100)
            set_int("0Antiaim.inverted_body_lean", 100)
            set_int("Antiaim.yaw_offset", 0)
        elseif (condition == e_conditions.RUNNING) then
            set_int("0Antiaim.yaw", 0)
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", 38)
            set_int("0Antiaim.inverted_desync_range", 39)
            set_int("0Antiaim.body_lean", 100)
            set_int("0Antiaim.inverted_body_lean", 100)
            set_int("Antiaim.yaw_offset", 0)
        elseif (condition == e_conditions.AIR) then
            set_int("Antiaim.yaw_offset", 0)
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.range", 2)
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", 22)
            set_int("0Antiaim.inverted_desync_range", 43)
            set_int("0Antiaim.body_lean", 100)
            set_int("0Antiaim.inverted_body_lean", 100)
        elseif (condition == e_conditions.WALKING) then
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.range", 16)
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", 60)
            set_int("0Antiaim.inverted_desync_range", 60)
            set_int("0Antiaim.body_lean", 100)
            set_int("0Antiaim.inverted_body_lean", 100)
            set_int("Antiaim.yaw_offset", 0)
        else
            set_int("0Antiaim.yaw", 1)
            set_int("0Antiaim.range", 46)
            set_int("0Antiaim.desync", 2)
            set_int("0Antiaim.desync_range", 60)
            set_int("0Antiaim.inverted_desync_range", 60)
            set_int("0Antiaim.body_lean", 22)
            set_int("0Antiaim.inverted_body_lean", 20)
            set_int("Antiaim.yaw_offset", 0)
        end
    end
end
--- @item: Roll Angles
local roll_angles_c = {}

roll_angles_c.rolling = false

function roll_angles_c.handle(cmd)
    if (not ui_anti_aim.roll_angles:get()) then
        return
    end

    local player = entity.get_local()

    if (player == nil or not player:is_alive()) then
        return
    end

    local flags = player:get_prop_int("CBasePlayer", "m_fFlags")
    local on_ground = bit.band(flags, e_player_flags.ON_GROUND) == 1

    if (not on_ground) then
        return
    end

    roll_angles_c.rolling = false

    local roll_value = ui_anti_aim.roll_value:get()
    local degree = roll_value

    if (ui.get_keybind_state(keybinds.flip_desync)) then
        degree = -roll_value
    end

    local condition = entity_c.get_condition()

    if (condition == e_conditions.STANDING or condition == e_conditions.WALKING or anti_aim_c.should_roll) then
        cmd.viewangles.z = degree

        roll_angles_c.rolling = true
    end
end

--- @item: Slowwalk Modifier
local slowwalk_mod_c = {}

function slowwalk_mod_c.handle()
    if (not ui_anti_aim.slowwalk_mod:get() or not ui.get_keybind_state(keybinds.slowwalk)) then
        console.set_int("cl_forwardspeed", 450)
        console.set_int("cl_backspeed", 450)
        console.set_int("cl_sidespeed", 450)

        return
    end

    local player = entity.get_local()

    if (player == nil or not player:is_alive()) then
        return
    end

    console.set_int("cl_forwardspeed", ui_anti_aim.slowwalk_mod_value:get())
    console.set_int("cl_backspeed", ui_anti_aim.slowwalk_mod_value:get())
    console.set_int("cl_sidespeed", ui_anti_aim.slowwalk_mod_value:get())
end

--- @item: Fake Lag
local fake_lag_c = {}

fake_lag_c.shot_timer = 0
fake_lag_c.limit = 0

function fake_lag_c.impact_event(event)
    if (event:get_name() ~= "bullet_impact") then
        return
    end

    if (not ui_anti_aim.fake_lag:get()) then
        return
    end

    local player = entity.get_local()

    if (player == nil or not player:is_alive()) then
        return
    end

    local userid = entity.get_player_by_index(engine.get_player_for_user_id(event:get_int("userid")))
    local attacker = entity.get_player_by_index(engine.get_player_for_user_id(event:get_int("attacker")))

    if (attacker:get_index() ~= player:get_index()) then
        return
    end

    fake_lag_c.shot_timer = 1
end

function fake_lag_c.handle()
    if (not ui_anti_aim.fake_lag:get()) then
        return
    end
    if ui_anti_aim.fake_lag_types:get() == 0 then

    local player = entity.get_local()

    if (player == nil or not player:is_alive()) then
        return
    end

    if (fake_lag_c.shot_timer >= 1 and fake_lag_c.shot_timer <= 4) then
        fake_lag_c.shot_timer = fake_lag_c.shot_timer + 1
    else
        fake_lag_c.shot_timer = 0
    end

    if (fake_lag_c.limit <= ui_anti_aim.max_choke:get() + 1) then
        fake_lag_c.limit = fake_lag_c.limit + ui_anti_aim.update_speed:get() / 10
    else
        fake_lag_c.limit = ui_anti_aim.start_limit:get()
    end

    local condition = entity_c.get_condition()

    local fake_lag_var = ui.find_menu_int("Antiaim.fake_lag_limit")

    if (ui_anti_aim.auto_disable:get() and (fake_lag_c.shot_timer >= 1 or globalvars.get_dt_recharging() or ui.get_keybind_state(keybinds.double_tap) or condition == e_conditions.STANDING or condition == e_conditions.CROUCH)) then
        fake_lag_var:set(1)
    else
        fake_lag_var:set(math.floor(fake_lag_c.limit))
    end
    elseif ui_anti_aim.fake_lag_types:get() == 1 then
    local player = entity.get_local()

    if (player == nil or not player:is_alive()) then
        return
    end

    if (fake_lag_c.shot_timer >= 1 and fake_lag_c.shot_timer <= 4) then
        fake_lag_c.shot_timer = fake_lag_c.shot_timer + 1
    else
        fake_lag_c.shot_timer = 0
    end
    
    switch = not switch
    if switch then
        fake_lag_c.limit = ui_anti_aim.start_limit:get()
    else
        fake_lag_c.limit = ui_anti_aim.max_choke:get()
    end
    local condition = entity_c.get_condition()

    local fake_lag_var = ui.find_menu_int("Antiaim.fake_lag_limit")

    if (ui_anti_aim.auto_disable:get() and (fake_lag_c.shot_timer >= 1 or globalvars.get_dt_recharging() or ui.get_keybind_state(keybinds.double_tap) or condition == e_conditions.STANDING or condition == e_conditions.CROUCH)) then
        fake_lag_var:set(1)
    else
        fake_lag_var:set(math.floor(fake_lag_c.limit))
    end
end
end

local anim_breaker = {}

anim_breaker.ground_ticks = 1
anim_breaker.end_time = 0

function anim_breaker.handle(stage)
    local player = entity.get_local()

    if (player == nil or not player:is_alive()) then
        return
    end

    if (stage ~= enum_frames.frame_render_start) then
        return
    end

    local flags = player:get_prop_int("CBasePlayer", "m_fFlags")
    local on_ground = bit.band(flags, e_player_flags.ON_GROUND) == 1

    if (ui_anti_aim.static_legs_in_air:get()) then
        if (not on_ground) then
            player:set_render_pose(e_poses.JUMP_FALL, 1)
        end
    end

    if (ui_anti_aim.zero_pitch_on_land:get()) then
        if (on_ground) then
            anim_breaker.ground_ticks = anim_breaker.ground_ticks + 1
        else
            anim_breaker.ground_ticks = 0
            anim_breaker.end_time = globalvars.get_curtime() + 1
        end

        if (anim_breaker.ground_ticks > (e_menu.FAKELAG_AMOUNT:get() + 1) and anim_breaker.end_time > globalvars.get_curtime()) then
            player:set_render_pose(e_poses.BODY_PITCH, 0.5)
        end
    end
end
--- @endregion

--- @region: visuals
--- @item: Keybinds
local keybinds_c = {}

keybinds_c.active = {}

keybinds_c.list = {
    ["Minimum damage"] = keybinds.damage_override,
    ["Double tap"] = keybinds.double_tap,
    ["On shot anti-aim"] = keybinds.hide_shots,
    ["Slow motion"] = keybinds.slowwalk,
    ["Anti-aim inverter"] = keybinds.flip_desync,
    ["Duck peek assist"] = keybinds.fakeduck,
    ["Quick peek assist"] = keybinds.automatic_peek,
    ["Body aim"] = keybinds.body_aim
}

keybinds_c.modes = {"always", "holding", "toggled", "disabled"}

keybinds_c.dragging = drag_c.new(0, 0, "Keybinds", "Visuals")

function keybinds_c.handle()
    if (not ui_visuals.keybinds:get()) then
        return
    end

    local r, g, b = color_c.unpack(ui_visuals.accent:get())

    local latest_item = false
    local maximum_offset = 66

    local menu_bind = true

    for bind_name, bind_ref in pairs(keybinds_c.list) do
        local item_active = ui.get_keybind_state(bind_ref)

        if (item_active) then
            latest_item = true

            if (keybinds_c.active[bind_name] == nil) then
                keybinds_c.active[bind_name] = {mode = "", alpha = 0, offset = 0, active = false}
            end

            local bind_name_size = render.get_text_size(fonts.verdana.default, bind_name)

            keybinds_c.active[bind_name].mode = keybinds_c.modes[ (ui.get_keybind_mode(bind_ref) + 2) or 4 ]

            keybinds_c.active[bind_name].alpha = animation.lerp(keybinds_c.active[bind_name].alpha, 1, 12)
            keybinds_c.active[bind_name].offset = bind_name_size.x

            keybinds_c.active[bind_name].active = true
        elseif (keybinds_c.active[bind_name] ~= nil) then
            keybinds_c.active[bind_name].alpha = animation.lerp(keybinds_c.active[bind_name].alpha, 0, 12)
            keybinds_c.active[bind_name].active = false

            if (keybinds_c.active[bind_name].alpha < 0.01) then
                keybinds_c.active[bind_name] = nil
            end
        end

        if (keybinds_c.active[bind_name] ~= nil and keybinds_c.active[bind_name].offset > maximum_offset) then
            maximum_offset = keybinds_c.active[bind_name].offset
        end
    end

    local alpha = animation.create("keybinds [alpha]", (globalvars.is_open_menu() or table.count(keybinds_c.active) > 0 and latest_item) and 1 or 0, 12)

    if (alpha < 0) then
        alpha = 0
    end

    local text = "keybinds"
    local text_size = render.get_text_size(fonts.verdana.default, text)

    local x, y = keybinds_c.dragging:get()

    local width, height = math.floor(animation.create("keybinds [width]", 75 + maximum_offset, 8)), 23
    local height_offset = height + 5

    render_c.container(x, y, width, height, color_c.new(r, g, b, 255*alpha))

    render.text(fonts.verdana.default, x + (width / 2) - (text_size.x / 2), y + (height / 2) - (text_size.y / 2), color_c.new(255, 255, 255, 255*alpha), text, true)

    for bind_name, value in pairs(keybinds_c.active) do
        local key_type = "[" .. (value.mode or "?") .. "]"
        local key_type_size = render.get_text_size(fonts.verdana.default, key_type)

        if (value.alpha < 0) then
            value.alpha = 0
        end

        render.text(fonts.verdana.default, x + 5, y + height_offset, color_c.new(255, 255, 255, 255*alpha*value.alpha), bind_name, true)
        render.text(fonts.verdana.default, x + width - key_type_size.x - 5, y + height_offset, color_c.new(255, 255, 255, 255*alpha*value.alpha), key_type, true)

        height_offset = height_offset + 15 * value.alpha
    end

    keybinds_c.dragging:handle(width, (table.count(keybinds_c.active) > 0 and height_offset or height))
end

--- @item: Watermark
local watermark_c = {}

function watermark_c.handle()
    if (not ui_visuals.watermark:get()) then
        return
    end

    local r, g, b = color_c.unpack(ui_visuals.accent:get())

    local actual_time = globalvars.get_time()
    local nickname = engine.get_gamename()
    local sadress globalvars.get_server_address()
    local text = ("%s | %s "):format(script_name..".us", nickname)

    if (engine.is_in_game()) then
        local latency = globalvars.get_ping()

        local latency_text = ("%dms"):format(latency)

        text = ("%s | %s"):format(text, latency_text)
    end

    text = ("%s | %s"):format(text, actual_time)

    local text_size = render.get_text_size(fonts.verdana.default, text)

    local x, y = engine.get_screen_size().x, 8

    local width, height = text_size.x + 10, 22

    x = x - width - 10

    render_c.container(x, y, width, height, color_c.new(r, g, b))

    render.text(fonts.verdana.default, x + (width / 2) - (text_size.x / 2), y + (height / 2) - (text_size.y / 2), color_c.new(255, 255, 255), text, true)
end

--- @item: Crosshair Indicators
local indicators_c = {}

function indicators_c.handle()
    if (ui_visuals.crosshair_indicators:get() == 0) then
        return
    end
    if (ui_visuals.crosshair_indicators:get() == 1) then

    local player = entity.get_local()

    if (player == nil or not player:is_alive()) then
        return
    end

    local player_weapon = entity.get_weapon_by_player(player)

    if (player_weapon == nil) then
        return
    end

    local scoped = player:is_scoped() and not player_weapon:is_non_aim()

    local f_r, f_g, f_b = color_c.unpack(ui_visuals.first_color:get())
    local s_r, s_g, s_b = color_c.unpack(ui_visuals.second_color:get())

    local new_script_name = ("%s.lua"):format(script_name)
    local text_size = render.get_text_size(fonts.verdana.bold, new_script_name)

    local default_size = (text_size.x - 10)

    local condition = entity_c.get_condition()
    local list = {
        {name = entity_c.get_condition_name(condition):upper(), active = true, small = true},
        {name = "DOUBLETAP", active = ui.get_keybind_state(keybinds.double_tap)},
        {name = "HIDESHOTS", active = ui.get_keybind_state(keybinds.hide_shots)},
        {name = "DAMAGE", active = ui.get_keybind_state(keybinds.damage_override)},
        {name = "BODY", active = ui.get_keybind_state(keybinds.body_aim)},
        {name = "QUICK PEEK", active = ui.get_keybind_state(keybinds.automatic_peek)},
        {name = "FREESTAND", active = ui.find_menu_bool("Antiaim.freestand"):get(true)},
    }

    for key, value in ipairs(list) do
        if (value.small == nil) then
            value.small = false
        end

        local list_size = (render.get_text_size(fonts.small.default, value.name).x - 20)

        if (value.active and list_size > default_size) then
            default_size = list_size
        end
    end

    local screen = engine.get_screen_size()

    local additional = 40
    local scope_additional = animation.create("Scoped additional / indicators", scoped and default_size or 0, 12)
    local x, y = screen.x / 2 + scope_additional, (screen.y / 2) + additional

    render.text_gradient(fonts.verdana.bold, x - (text_size.x / 2), y - (text_size.y / 2), color_c.new(f_r, f_g, f_b), color_c.new(s_r, s_g, s_b), new_script_name, 0, false, true)
    
    local offset = 0

    for key, value in ipairs(list) do
        local alpha = animation.create(("[%s] list / indicators"):format(key), value.active and 1 or 0, 12)

        if (alpha < 0) then
            alpha = 0
        end

        if (value.small == nil) then
            value.small = false
        end

        local list_size = render.get_text_size(fonts.small.default, value.name)

        render.text(fonts.small.default, x - (list_size.x / 2), y - (list_size.y / 2) + (text_size.y - 2) + offset, color_c.new(255, 255, 255, 255 * alpha), value.name, false, true)

        offset = offset + 10 * alpha
    end
end
if (ui_visuals.crosshair_indicators:get() ~= 2) then
    return
end

local player = entity.get_local()

if (player == nil or not player:is_alive()) then
    return
end

local player_weapon = entity.get_weapon_by_player(player)

if (player_weapon == nil) then
    return
end

local scoped = player:is_scoped() and not player_weapon:is_non_aim()

local f_r, f_g, f_b = color_c.unpack(ui_visuals.first_color:get())
local s_r, s_g, s_b = color_c.unpack(ui_visuals.second_color:get())

local new_script_name = ("%s.lua"):format(script_name)
local text_size = render.get_text_size(fonts.verdana.bold, new_script_name)

local default_size = (text_size.x - 10)
local by = player:get_body_yaw()
local babs = math.abs(by)
local bfloor = math.floor(by)
local condition = entity_c.get_condition()
local list = {
    {name1 = "*"..entity_c.get_condition_name(condition):upper().."*", active = true, small = true},
    {name = "dt", active = ui.get_keybind_state(keybinds.double_tap) and not ui_ragebot.ideal_tick_helper:get(false)},
    {name = "idealtick", active = ui_ragebot.ideal_tick_helper:get(true) and ui.get_keybind_state(keybinds.double_tap) and ui.get_keybind_state(keybinds.automatic_peek)},
    {name = "on-shot", active = ui.get_keybind_state(keybinds.hide_shots)},
    {name = "dmg", active = ui.get_keybind_state(keybinds.damage_override)},
    {name = "peek", active = ui.get_keybind_state(keybinds.automatic_peek) and not ui_ragebot.ideal_tick_helper:get(false)},
    {name = "fs", active = ui.find_menu_bool("Antiaim.freestand"):get(true) and not ui_ragebot.ideal_tick_helper:get(false) or ui_ragebot.freestand_below_switch:get(false)},
    {name2 = "%%" .. babs, active = true, small = true},
}

for key, value in ipairs(list) do
    if (value.small == nil) then
        value.small = false
    end

    local list_size = (render.get_text_size(fonts.small.default, value.name).x - 20)

    if (value.active and list_size > default_size) then
        default_size = list_size
    end
end

local screen = engine.get_screen_size()
local velocity = math.floor(player:get_velocity():length_2d())
local additional = 40
local scope_additional = animation.create("Scoped additional / indicators", scoped and default_size or 0, 12)
local x, y = screen.x / 2 + scope_additional, (screen.y / 2) + additional
if bfloor >= 0 then
render.text_gradient(fonts.verdana.bold, x - (text_size.x / 2), y - (text_size.y / 2), color_c.new(f_r, f_g, f_b), color_c.new(s_r, s_g, s_b), new_script_name, 0, false, true)
end
if bfloor < 0 then
render.text_gradient(fonts.verdana.bold, x - (text_size.x / 2), y - (text_size.y / 2), color_c.new(s_r, s_g, s_b), color_c.new(f_r, f_g, f_b), new_script_name, 0, false, true)
end
local offset = 0
for key, value in ipairs(list) do
    local alpha = animation.create(("[%s] list / indicators"):format(key), value.active and 1 or 0, 12)

    if (alpha < 0) then
        alpha = 0
    end

    if (value.small == nil) then
        value.small = false
    end

    local list_size = render.get_text_size(fonts.small.default, value.name)
    local list_size1 = render.get_text_size(fonts.small.default, value.name1)
    local list_size2 = render.get_text_size(fonts.small.default, value.name2)
    render.rect_filled(x - 30, y - 9,58, 3 , color(18,18,18,240))
    render.rect_filled(x - 30, y - 9,math.floor(velocity)/6, 3 , color_c.new(f_r, f_g, f_b))
    render.text(fonts.small.default, x - (list_size2.x / 2), y - (list_size2.y / 2) + (text_size.y - 2), color_c.new(255, 255, 255, 255 * alpha), value.name2, false, true)
    render.text(fonts.small.default, x - (list_size1.x / 2), y - (list_size1.y / 2) + (text_size.y - 2) + offset - 25, color_c.new(255, 255, 255, 255 * alpha),value.name1, false, true)
    render.text(fonts.small.default, x - (list_size.x / 2), y - (list_size.y / 2) + (text_size.y - 2) + offset, color_c.new(255, 255, 255, 255 * alpha), value.name, false, true)
    offset = offset + 10 * alpha
end
end

--- @item: Sense Indicators
local sense_indicators_c = {}

sense_indicators_c.offset = 0

--- @param: text: string
--- @param: text_color: color
--- @return: number
function sense_indicators_c.draw(text, text_color)
    if (text_color == nil) then
        text_color = color_c.new(255, 255, 255)
    end

    if (text == nil) then
        text = "unnamed"
    end

    local screen = engine.get_screen_size()

    local text_size = render.get_text_size(fonts.calibri.default, text)

    local x, y = 11, ((screen.y / 2) + 100) + sense_indicators_c.offset
    local height = 33

    render.gradient(x, y, (text_size.x / 2), height, color_c.new(0, 0, 0, 5), color_c.new(0, 0, 0, 80), 0)
    render.gradient(x + (text_size.x / 2), y, (text_size.x / 2), height, color_c.new(0, 0, 0, 80), color_c.new(0, 0, 0, 5), 0)

    render.text(fonts.calibri.default, x + 5, y + (height / 2) - (text_size.y / 2) + 3, text_color, text, true)

    sense_indicators_c.offset = sense_indicators_c.offset - (height + 4)

    return x, y
end

function sense_indicators_c.handle()
    if (not ui_visuals.sense_indicators:get()) then
        return
    end

    local player = entity.get_local()

    if (player == nil or not player:is_alive()) then
        return
    end

    sense_indicators_c.offset = 0

    local body_yaw = player:get_body_yaw()
    local f_r, f_g, f_b = 192 - (math.abs(body_yaw) / 60 * 71), 32 + (math.abs(body_yaw) / 60 * 146), 28
    local player_health = player:get_health()

    local list = {
        {text = "DA", color = color_c.new(132, 195, 16), active = ui.find_menu_bool("Ragebot.autoshoot"):get(true)},
        {text = "FAKE", color = color_c.new(f_r, f_g, f_b), circle = {color = color_c.new(f_r, f_g, f_b), value = (math.abs(body_yaw) / 58) * 360}},
        {text = "FS", color = color_c.new(132, 195, 16), active = ui.find_menu_bool("Antiaim.freestand"):get(true)},
        {text = "ONSHOT", color = color_c.new(132, 195, 16), active = ui.get_keybind_state(keybinds.hide_shots)},
        {text = "DUCK", color = color_c.new(210, 210, 210), active = ui.get_keybind_state(keybinds.fakeduck)},
        {text = "DT", color = globalvars.get_dt_recharging() and color_c.new(255, 0, 0) or color_c.new(210, 210, 210), active = ui.get_keybind_state(keybinds.double_tap)},
        {text = "SAFE", color = color_c.new(102, 176, 41), active = ui.get_keybind_state(keybinds.safe_points)}
    }

    for key, value in ipairs(list) do
        if (value.active == nil) then
            value.active = true
        end

        if (not value.active) then
            goto skip
        end

        local ind_x, ind_y = sense_indicators_c.draw(value.text, value.color)

        if (value.circle ~= nil) then
            if (value.circle.active == nil) then
                value.circle.active = true
            end

            if (value.circle.color == nil) then
                value.circle.color = color_c.new(255, 255, 255)
            end

            if (value.circle.value == nil) then
                value.circle.value = 0
            end

            if (not value.circle.active) then
                goto skip
            end

            local text_size = render.get_text_size(fonts.calibri.default, value.text)

            render.arc(ind_x + text_size.x + 20, ind_y + 18, 6, 10, -90, math.floor(value.circle.value), value.circle.color)

            ::skip::
        end

        ::skip::
    end
end

function userp()
    local player = entity.get_local()
    if not (ui_visuals.panel):get() then
        return
    end
    if (player == nil or not player:is_alive()) then
        return
    end
    local screen = engine.get_screen_size()
    local condition = entity_c.get_condition()
    local state = entity_c.get_condition_name(condition):upper()
    local bodyyaw = math.abs(player:get_body_yaw())
    local bodyyawfloor = math.floor(player:get_body_yaw())
    local uname = player:get_name()
    render.text(fonts.verdana.panel, screen.x/2 - 955 , screen.y/2 - 160, color(255,255,255,255),"#divine.lua - version: v2 -stable - "..uname,false,false)
    render.text(fonts.verdana.panel, screen.x/2 - 955 , screen.y/2 - 150, color(214, 255, 207,255),"> version: stable",false,false)
    render.text(fonts.verdana.panel, screen.x/2 - 955 , screen.y/2 - 140, color(217, 207, 255,255),"> player info:"..state,false,false)
    if bodyyawfloor >= 0 then
    render.text(fonts.verdana.panel, screen.x/2 - 955 , screen.y/2 - 130, color(207, 250, 255,255),"> dsy side:0" .. " dsy amount:"..bodyyaw,false,false)
    elseif bodyyawfloor < 0 then
    render.text(fonts.verdana.panel, screen.x/2 - 955 , screen.y/2 - 130, color(207, 250, 255,255),"> dsy side:1" .. " dsy amount:"..bodyyaw,false,false)
end
end
--- @endregion

--- @region: callbacks
--- @callback: paint
callbacks.add("on_paint", indicators_c.handle)
callbacks.add("on_paint", sense_indicators_c.handle)
callbacks.add("on_paint", watermark_c.handle)
callbacks.add("on_paint", keybinds_c.handle)
callbacks.add("on_paint", userp)

--- @callback: createmove
callbacks.add("on_createmove", double_tap_c.handle)
callbacks.add("on_createmove", ragebot_c.handle)
callbacks.add("on_createmove", preset_c.handle)
callbacks.add("on_createmove", roll_angles_c.handle)
callbacks.add("on_createmove", slowwalk_mod_c.handle)
callbacks.add("on_createmove", fake_lag_c.handle)
callbacks.add("on_createmove", condition_anti_aim.handle)
callbacks.add("on_frame_net", anim_breaker.handle)
callbacks.add("on_frame_net", scope)
--- @callback: event 
callbacks.add("on_event", fake_lag_c.impact_event)
--- @callback: shot
callbacks.add("on_shot", double_tap_c.shot_event)
--- @endregion
