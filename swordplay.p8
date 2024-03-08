pico-8 cartridge // http://www.pico-8.com
version 42
__lua__
--swordplay
--by mabbees

-- todos:
--  battle system:
--   - lose after 2 misses?
--  ui:
--   - better title screen
--   - better hud?
--   - show result summary
--  sfx:
--   - sword clash
--  music:
--   - battle loop
--   - boss battle loop
--   - victory/defeat tune

-- sprite layers
-- 1 : ui
-- 2 : characters
-- 3 : battle animations
-- 4 : title

-- global constants
debug = false
g_text_speed = 40

function _init()
	-- global state
	t = 0
	scn = {}

	-- flow
	local game_flow = flow.seq({
	-- debug ★
		presented_by_scn,
		flow.retry(
			flow.loop(
				flow.seq({
					title_scn,
					swordplay_scn,
				})
			)
		),
	})
	
	transition_flow(game_flow).go(
		function(s)
			scn = s
		end,
		noop,
		noop
	)
end

function _draw()
	cls()
	
	scn:draw()
end

function _update()
 local t0 = t
 t = time()
 local dt = t-t0
	
	scn:update(dt)
end
-->8
-- library tools
-- * functions
-- * algebraic functions
-- * list
-- * reader
-- * lens
-- * flow
-- * behavior
-- * drawing
-- * easing
-- * text utils
-- * ui components


-- functions
function id(d) return d end

function const(x)
	return function() return x end
end

function noop() end

function compose(f,g)
	return function(...)
		return g(f(...))
	end
end

function suspend(f)
	return function(...)
		local args = {...}
		return function()
			return f(unpack(args))
		end
	end
end

function call(method, ...)
	local args = {...}
	return function(e)
		return e[method](e,unpack(args))
	end
end

function once(f)
 local done = false
 return function(val)
  if not done then
   done = true
   f(val)
  end
 end
end

function reduce(f,seed)
	return function(list)
		local ret = seed
		for x in all(list) do
			ret = f(ret, x)
		end
		return ret
	end
end

chain = reduce(compose,id)

function lerp(a,b,t)
	return a + t*(b-a)
end

-- memoization ----------------

-- context-aware caching
function memo(f, hash)
	hash = hash or id
	local cache = {}
	return function(x)
		local key = hash(x)
		if cache[key] == nil then
			cache = prune(cache,4)
			cache[key] = f(x)
		end
		return cache[key]
	end
end

function prune(tbl,n)
	local ret = {}
	local i = n
	for k,v in pairs(tbl) do
		ret[k] = v
		i -= 1
		if i <= 0 then break end
	end
	return ret
end

-- algebriac functions --------

function monad_loop(m)
	return m:flatmap(function()
		return monad_loop(m)
	end)
end

function monad_seq(empty)
	return reduce(
		function(a,m)
			return a:flatmap(const(m))
		end,
		empty
	)
end

function lift2(f)
	return function(mx,my)
		return mx:flatmap(function(x)
			return my:map(function(y)
				return f(x,y)
			end)
		end)
	end
end

pair = lift2(pack)


-- list -----------------------

list = {}

function list_copy(list)
	local ret = {}
	for x in all(list) do
		add(ret, x)
	end
	return ret
end

function list_map(l,f)
	local ret = {}
	for i,x in pairs(l) do
		add(ret, f(x,i))
	end
	return list.create(ret)
end

function list_flatmap(l,f)
	return list.concat(list_map(l,f))
end

function list_filter(l,f)
	local ret = {}
	foreach(l, function(x)
		if f(x) then
			add(ret, x)
		end
	end)
	return list.create(ret)
end

function list_concat(l1,l2)
	local ret = {}
	foreach(l1, function(x)
		add(ret, x)
	end)
	foreach(l2, function(x)
		add(ret, x)
	end)
	return list.create(ret)
end

function list_reduce(l,f,seed)
	return reduce(f,seed)(l)
end

function list_any(l,f)
	return list_reduce(l,
		function(acc,x)
			return acc or f(x)
		end,
		false
	)
end

function list_all(l,f)
	return list_reduce(l,
		function(acc,x)
			return acc and f(x)
		end,
		true
	)
end

list_meta = {
	__index= {
		map     = list_map,
		flatmap = list_flatmap,
		filter  = list_filter,
		reduce  = list_reduce,
		concat  = list_concat,
		all     = list_all,
		any     = list_any,
	}
}

function list.create(tbl)
	return setmetatable(tbl,list_meta)
end

function list.from_tbl(tbl)
	return list.create(list_copy(tbl))
end

function list.from_range(lo,hi)
	local ret = {}
	for i=lo,hi do
		add(ret,i)
	end
	return list.create(ret)
end

list.concat = function(ll)
	local ret = {}
	foreach(ll,function(l)
		foreach(l,function(x)
			add(ret,x)
		end)
	end)
	return list.create(ret)
end

list.empty = list.create({})


-- reader ---------------------
function rdr(f)
	return setmetatable(
		{ run=f },
		rdr_meta
	)
end

rdr_meta={
	__index={
		map=function(r,f)
			return rdr(
				compose(r.run, f)
			)
		end,
		flatmap=function(r,f)
			return rdr(
				function(e)
					return f(r.run(e)).run(e)
				end
			)
		end,
		memo=function(r,hash)
			return rdr(memo(r.run, hash))
		end
	}
}

rdr_local=function(f,r)
	return rdr(function(e)
		return r.run(f(e))
	end)
end


-- lens -----------------------
id_lens = {
	get=function(s) return s end,
	set=function(s,x) return x end,
}

function idx_lens(i)
	return {
		get=function(s)
			return s[i]
		end,
		set=function(s,x)
			local c = list_copy(s)
			c[i] = x
			return c
		end,
	}
end

function prop_lens(k)
	return {
		get=function(s)
			return s[k]
		end,
		set=function(s,x)
			local c = tbl_copy(s)
			c[k] = x
			return c
		end,
	}
end

function tbl_copy(tbl)
	local ret = {}
	for k,v in pairs(tbl) do
		ret[k] = v
	end
	return ret
end

-- flow -----------------------

flow = {}

flow_meta={
	__index={
		map=function(m,f)
			return flow.create(
				function(nxt, done, err)
					m.go(nxt, compose(f,done), err)
				end)
		end,
		flatmap=function(m,f)
			return flow.create(
				function(nxt, done, err)
					m:map(f).go(nxt, function(n)
						n.go(nxt, done, err)
					end, err)
				end)
		end,
		handle_err=function(m,f)
			return flow.create(
				function(nxt, done, err)
					m.go(nxt, done, function(e)
						f(e).go(nxt, done, err)
					end)
				end)
		end,
		wrap=function(m,f)
			return flow.create(
				function(nxt,done,err)
					m.go(compose(f,nxt), done, err)
				end
			)
		end
	}
}

function flow.create(go)
	return setmetatable({
		go=go
	},flow_meta)
end

function flow.of(value)
	return flow.create(function(nxt, done)
		done(value)
	end)
end

function flow.err(e)
	return flow.create(function(nxt, done, err)
		err(e)
	end)
end

function flow.once(make_scene)
 return flow.create(
	 function(nxt, done)
	  nxt(make_scene(once(done)))
	 end)
end

flow.seq = monad_seq(flow.of(nil))

flow.loop = monad_loop

function flow.retry(f)
	return f:handle_err(function(e)
		return flow.retry(f)
	end)
end

-- behavior -------------------
-- behavior (pico)

behavior = {}


b_success_meta={
	__index={
		type="success",
		map=function(b,f)
			return behavior.success(f(b.value))
		end,
		flatmap=function(b,f)
			return f(b.value)
		end,
	}
}
function behavior.success(value)
 return setmetatable({
 	value=value,
 }, b_success_meta)
end


b_running_meta={
	__index={
		type="running",
		map=function(b,f)
			return behavior.run(function(actor,dt)
				return b.update(actor,dt):map(f)
			end)
		end,
		flatmap=function(b,f)
			return behavior.run(function(actor,dt)
				return b.update(actor,dt):flatmap(f)
			end)
		end,
	}
}
function behavior.run(update)
	local nxt = {}
	
	local b = setmetatable({
		update=function(actor,dt)
	  return update(actor,dt,nxt)
	 end,
	}, b_running_meta)
	
	nxt.done = behavior.success
	nxt.cont = const(b)
	
 return b
end
 

behavior.seq = monad_seq(behavior.success(nil))

function behavior.par(bs)
	for b in all(bs) do
		if b.type == "success" then return b end
	end
	
	return behavior.run(function(actor,dt)
		return behavior.par(
			list_map(bs, function(b)
				return b.update(actor,dt)
			end)
		)
	end)
end

behavior.loop = monad_loop

function behavior.once(f)
	return behavior.run(function(actor,dt)
		return behavior.success(f(actor,dt))
	end)
end

function behavior.always(f)
	return behavior.run(function(actor,dt,nxt)
		f(actor,dt)
		return nxt.cont()
	end)
end

behavior.never = behavior.always(noop)


-- drawing --------------------
function monochrome_palette(col)
	local palette = {}
	for i=1,16 do
		palette[i]=col
	end
	return palette
end

function draw_with_color(col)
	return suspend(function(draw)
		local c = peek(0x5f25)
		color(col)
		draw()
		poke(0x5f25, c)
	end)
end

function draw_with_layer(b)
	local bits = 0
	for i=0,15 do
		local bit = (i & b == 0)
			and 0
			or  1
		bits = bits | (bit << i)
	end
	return suspend(function(draw)
		palt(bits)
		local c = peek(0x5f25)
		local p = monochrome_palette(c)
		draw_with_palette(p)(draw)()
		palt()
	end)
end


-- in case the draw call itself
-- makes changes to palette
-- we want the "outer" palette
-- to remain active
override_palette = false
function draw_with_palette(palette)
	return suspend(function(draw)
		if override_palette then
			draw()
		else
			override_palette = true
			local p1,p2,p3,p4 = peek4(0x5f00,4)
			pal(palette)
			draw()
			poke4(0x5f00,p1,p2,p3,p4)
			override_palette = false
		end
	end)
end

function draw_with_offset(dx, dy)
	return suspend(function(draw)
		local cx,cy =	peek2(0x5f28, 2)
		camera(cx-dx, cy-dy)
		draw()
		poke2(0x5f28,cx,cy)
	end)
end

skip_draw = const(noop)

function draw_with_outline_4(col)
	local palette = monochrome_palette(col)
	
	return suspend(function(draw)
		draw_with_palette(palette)(
			function()
				draw_with_offset( 1, 0)(draw)()
				draw_with_offset( 0, 1)(draw)()
				draw_with_offset(-1, 0)(draw)()
				draw_with_offset( 0,-1)(draw)()
			end
		)()
		draw()
	end)
end

function draw_with_outline_9(col)
	local palette = monochrome_palette(col)
	
	return suspend(function(draw)
		draw_with_palette(palette)(
			function()
				for i=-1,1 do
					for j=-1,1 do
						draw_with_offset(i,j)(draw)()
					end
				end
			end
		)()
		draw()
	end)
end

function draw_with_shadow(col,dx,dy)
	dx = dx or 0
	dy = dy or 1
	local palette = monochrome_palette(col)
	
	return suspend(function(draw)
		chain({
			draw_with_palette(palette),
			draw_with_offset(dx,dy),
		})(draw)()
		draw()
	end)
end

draw_seq = suspend(function(tbl)
	for d in all(tbl) do
		d()
	end
end)

-- text utils -----------------

function word_width(word)
	-- todo: adjust for special
	-- characters, but don't call
	-- "print" because it is slow
	
	-- hacks for special characters
	if word[1] == "❎" or word[1] == "🅾️" then
		return 3 + 4 * #word
	end
	
	return 4 * #word
end

function text_width(txt)
	local w = print(txt, 0, -1000)
	cursor()
	return w
end

function text_height(txt)	
	-- todo: wrong result
	-- when txt has p8scii
	-- tall mode turned on
	local height=5
	for i=1,#txt do
		local c = txt[i]
		if c=="\n" then
			height += 6
		end
	end
	return height
end

function wrap_line(width)
	return function(lne)
		local buffer, len = "", 0
		local words = text_split(lne," ")
		
		for i, word in ipairs(words) do
			local w = word_width(word)
			if i == 1 then
				buffer = word
				len=w
			else
				if len+4+w > width then
					-- push next word to next line
					buffer = buffer.."\n"..word
					len=w
				else
					-- next word still fits on the line
					buffer = buffer.." "..word
					len += 4+w
				end
			end
		end
		return buffer
	end
end

function wrap_lines(txt, width)
	return text_join(
		text_split(txt, "\n")
			:map(wrap_line(width)),
		"\n"
	)
end

function text_split(txt,sep)
	return list.from_tbl(split(txt, sep, false))
end

function text_join(lst, sep)
	return lst:reduce(function(acc,word)
		return acc == ""
			and word
			or acc .. sep .. word
	end, "")
end


-- ui components --------------
local ui = {}

zeros=rdr(const({0,0}))

no_offset=zeros

empty_dims=zeros

full_dims = rdr(id)

screen_bounds = {0,0,128,128}

function hash_dims(dims)
	local w,h=unpack(dims)
	return w..":"..h
end

function measure_ui(c, bnds)
	local ox,oy,ow,oh = unpack(bnds)
	local x,y = unpack(c.offset.run({ow,oh}))
	local w,h = unpack(c.dims.run({ow,oh}))
	return {x+ox,y+oy,w,h}
end

function draw_ui(c, bnds)
	c:draw(measure_ui(c,bnds))
end

-- constructors
function ui.create(def)
	return setmetatable(def, ui_meta)
end

function ui.from_draw(draw)
	return ui.create({
		offset   = no_offset,
		min_dims = empty_dims,
		dims     = full_dims,
		draw = function(c,bnds)
			draw(unpack(bnds))
		end,
	})
end

function ui.from_ui(c1,c2)
	local keys={
		"draw",
		"offset",
		"min_dims",
		"dims",
		"update",
	}
	local copy={}
	foreach(keys, function(key)
		copy[key] = c2[key] or c1[key]
	end)
	return ui.create(copy)
end

function ui.from_rdr(r)
	return ui.create({
		draw     = function(tbl,bnds)
			local x,y,w,h = unpack(bnds)
			r.run({w,h}).draw(tbl,bnds)
		end,
		update   = function(tbl,dt)
			r.run({0,0}).update(tbl,dt)
		end,
		offset   = r:flatmap(function(c) return c.offset end),
		min_dims = r:flatmap(function(c) return c.min_dims end),
		dims     = r:flatmap(function(c) return c.dims end),
	})
end

-- prims
function ui_text(str,col)
	local w=text_width(str)
	local h=text_height(str)
	local dims = rdr(const({w,h-1}))
	return ui.create({
		offset   = no_offset,
		min_dims = dims,
		dims     = dims,
		draw=function(c,bnds)
			local x,y=unpack(bnds)
			print(str,x,y,col)
		end
	})
end

function ui_wrap_text(str,c)
	return ui.from_rdr(rdr(function(dims)
		local w,h = unpack(dims)
		local txt = wrap_lines(str,w)
		return ui_text(txt,c)
	end):memo(hash_dims))
end

function ui_spr(n,sw,sh,b)
	sw = sw or 1
	sh = sh or 1
	b = b or 0b0001
	local w=8*sw
	local h=8*sh
	return ui.from_draw(function(x,y,w,h)
		spr(n,x,y,sw,sh)
	end)
	:effect(draw_with_layer(b))
	:size(w,h)
end

function ui_clip(c)
	return ui.from_draw(function(x,y,w,h)
		clip(x,y,w,h)
		draw_ui(c, {x,y,w,h})
		clip()
	end)
	:updatable(function(_,dt)
		c:update(dt)
	end)
end

-- layouts
function ui_offset(c,dx,dy)
	return ui.from_ui(c, {
		offset = c.offset:map(function(off)
			local x,y = unpack(off)
			return {x+dx,y+dy}
		end),
	})
end

function ui_inset(c,m)
	local function adjust(dims)
		local w,h=unpack(dims)
		return {
			w-2*m,
			h-2*m,
		}
	end
	local function readjust(dims)
		local w,h=unpack(dims)
		return {
			w+2*m,
			h+2*m,
		}
	end
			
	return ui.from_ui(c, {
		draw     = function(_,bnds)
			local x,y,w,h = unpack(bnds)
			c:draw({
				x+m,
				y+m,
				w-2*m,
				h-2*m,
			})
		end,
		min_dims = rdr_local(adjust, c.min_dims):map(readjust),
		dims     = rdr_local(adjust, c.dims):map(readjust),
	})
end


-- ui_size
--  w_size: "fit" | number | function | "fill"
--  h_size: "fit" | number | function | "fill"
function ui_size(c, w_size, h_size)
	if type(w_size)=="number" and type(h_size) == "number" then
		local dims = rdr(const({w_size,h_size}))
		return ui.from_ui(c, {
			min_dims = dims,
			dims     = dims,
		})
	end
	
	local dims=rdr(function(dims)
		local wmin,hmin = unpack(c.min_dims.run(dims))
		local wmax,hmax = unpack(dims)
		
		-- width
		if type(w_size)=="number" then
			w=w_size
		elseif type(w_size)=="function" then
			w=w_size(dims)
		elseif w_size=="fit" then
			w=wmin
		elseif w_size=="fill" then
			w=wmax
		end
		-- height
		if type(h_size)=="number" then
			h=h_size
		elseif type(h_size)=="function" then
			h=h_size(dims)
		elseif h_size=="fit" then
			h=hmin
		elseif h_size=="fill" then
			h=hmax
		end
		
		return {w,h}
	end):memo(hash_dims)
	return ui.from_ui(c, {
		min_dims = dims,
		dims     = dims,
	})
end

-- ui_align
--  x_align: "left" | "center" | "right"
--  y_align: "top" | "center" | "bottom"
function ui_align(c, x_align, y_align)
	return ui.from_ui(c, {
		offset = rdr(function(dims)
			local bw,bh = unpack(dims)
			local w,h = unpack(c.dims.run(dims))

			local dx = 0
			if x_align == "left" then
				-- noop. dx is already 0
				-- dx = 0
			elseif x_align == "center" then
				dx = (bw - w)/2
			elseif x_align == "right" then
				dx = bw - w
			end
		
			local dy = 0
			if y_align == "top" then
				-- noop. dy is already 0
				-- dy = 0
			elseif y_align == "center" then
				dy = (bh - h)/2
			elseif y_align == "bottom" then
				dy = bh - h
			end
			
			return {dx,dy}
		end):memo(hash_dims),
	})
end

-- groups
function ui_group(cs)
	local bound_all = rdr(function(dims)
		local xmin,ymin,xmax,ymax=0,0,0,0
		for c in all(cs) do
			local x,y=unpack(c.offset.run(dims))
			local w,h=unpack(c.dims.run(dims))
			xmin=min(xmin,max(0,x))
			ymin=min(ymin,max(0,y))
			xmax=max(xmax,x+w)
			ymax=max(ymax,y+h)
		end
		return {xmax-xmin,ymax-ymin}
	end)
	
	return ui.create({
		offset   = no_offset,
		min_dims = rdr_local(
			const({0,0}),
			bound_all
		),
		dims     = bound_all,
		draw = function(g,bnds)
			foreach(cs, function(c)
				draw_ui(c,bnds)
			end)
		end,
		update = function(g,dt)
			foreach(cs, function(c)
				c:update(dt)
			end)
		end
	})
end

-- ui_layout
--  dir: "stack" | "inline"
function ui_layout(cs,dir,m)
	m = m or 0

	local function get_offset(dx,dy,w,h)
		if dir == "stack" then
			return dx, dy+m+h
		elseif dir == "inline" then
			return dx+m+w, dy
		end
	end
	local layout = rdr(function(dims)
		local arr = {}
		local dx,dy = 0,0
		for c in all(cs) do
			add(arr, ui_offset(c,dx,dy))
			-- increment offset
			dx,dy = get_offset(
				dx,dy,
				unpack(c.dims.run(dims))
			)
		end
		return arr
	end):map(ui_group)
	
	return ui.from_rdr(layout)
end

-- ui_effect
--  draw_effect: draw_fn -> draw_fn
function ui_effect(c, draw_effect)
	return ui.from_ui(c, {
		draw = function(o,bnds)
			return draw_effect(function()
				c:draw(bnds)
			end)()
		end,
	})
end

-- metatable
ui_meta = {
	__index = {
		inset=ui_inset,
		align=ui_align,
		translate=ui_offset,
		size=ui_size,
		effect=ui_effect,
		update=noop,
		
		-- updates
		updatable=function(c,f)
			return ui.from_ui(c,{
				update=f,
			})
		end
	}
}

-- more prims
ui_empty = ui.create({
	offset   = no_offset,
	min_dims = zeros,
	dims     = full_dims,
	draw     = noop,
})
-->8
-- game tools

function behavior_wait(t)
	return t <= 0
		and behavior.success()
		or  behavior.run(function(_,dt)
			return behavior_wait(t-dt)
		end)
end

function behavior_await(poll)
	return behavior.run(function(_,dt,nxt)
		return poll()
			and nxt.done()
			or  nxt.cont()
	end)
end

function behavior_frames(f)
	return f <= 0
		and behavior.success()
		or  behavior.run(function()
			return behavior_frames(f-1)
		end)
end

-- input
input_await_not❎ = behavior_await(function()
	return not btn(❎)
end)

input_await_not❎🅾️ = behavior_await(function()
	return not (btn(❎) or btn(🅾️))
end)

input_await_❎ = behavior_await(function()
	return btn(❎)
end)

input_await_🅾️ = behavior_await(function()
	return btn(🅾️)
end)

input_await_❎🅾️ = behavior.par({
	input_await_❎,
	input_await_🅾️,
})


-- grids ----------------------

function diamond_grid(size,rows,cols)
	local s = size*sqrt(2)
	local dx = s
	local dy = s/2
	
	return pair(
		list.from_range(1,cols),
		list.from_range(1,rows)
	)
	:map(function(tbl)
		local i,j = unpack(tbl)
		return {
			(i-1)*dx + (j%2)*0.5*dx,
			(j-0.5)*dy,
			i,
			j,
		}
	end)
end

-- dot fields -----------------

dots = {}

-- dot transforms

function dots.transform(f)
	return function(dots)
		return compose(f,dots)
	end
end

function dots.map(f)
	return function(dots)
		return compose(dots,f)
	end
end

function dots.translate(dx,dy)
	return dots.transform(function(t,x,y)
		return t,x-dx,y-dy
	end)
end

function dots.scale(sx,sy)
	sy = sy or sx
	return dots.transform(function(t,x,y)
		local xx = x/sx
		local yy = y/sy
		return t,xx,yy
	end)
end

function dots.ease(f)
	return dots.transform(function(t,x,y)
		return f(t),x,y
	end)
end

function draw_dots(grid,field)
	return function(t)
		foreach(grid, function(p)
			local x,y = unpack(p)
			local r = field(t,x,y)
			circfill(x,y,r)
		end)
	end
end

-- transitions ----------------

scale_to_grid = chain({
	dots.translate(0.5,0.5),
	dots.scale(128),
	dots.map(function(v)
		return 16 * min(v,1)
	end),
})

wipe_left = chain({
	dots.ease(function(t)
		return (t*3.5)-1.25
	end),
	dots.map(function(r)
		return 0.6 * r
	end)
})(function(t,x,y)
	return t+2*x+y
end)

-- shuffle a list in place
function shuffle(xs)
	for i = #xs, 2, -1 do
		local j = flr(rnd(i)) + 1
		xs[i], xs[j] = xs[j], xs[i]
	end
	return xs
end

-- ui -------------------------

function box9(coords)
	local xs,ys = unpack(coords)
	local sx0,sx1,sx2,sx3 = unpack(xs)
	local sy0,sy1,sy2,sy3 = unpack(ys)
	
	return function(x,y,w,h)
		local sw0,sw1,sw2 = sx1-sx0, sx2-sx1, sx3-sx2
		local sh0,sh1,sh2 = sy1-sy0, sy2-sy1, sy3-sy2
		local dx0,dx1,dx2,dx3 = x,x+sw0,x+w-sw2,x+w
		local dy0,dy1,dy2,dy3 = y,y+sh0,y+h-sh2,y+h
		local dw0,dw1,dw2 = dx1-dx0,dx2-dx1,dx3-dx2
		local dh0,dh1,dh2 = dy1-dy0,dy2-dy1,dy3-dy2
		
		sspr(sx0, sy0, sw0, sh0, dx0, dy0, dw0, dh0)
		sspr(sx1, sy0, sw1, sh0, dx1, dy0, dw1, dh0)
		sspr(sx2, sy0, sw2, sh0, dx2, dy0, dw2, dh0)
		sspr(sx0, sy1, sw0, sh1, dx0, dy1, dw0, dh1)
		sspr(sx1, sy1, sw1, sh1, dx1, dy1, dw1, dh1)
		sspr(sx2, sy1, sw2, sh1, dx2, dy1, dw2, dh1)
		sspr(sx0, sy2, sw0, sh2, dx0, dy2, dw0, dh2)
		sspr(sx1, sy2, sw1, sh2, dx1, dy2, dw1, dh2)
		sspr(sx2, sy2, sw2, sh2, dx2, dy2, dw2, dh2)
	end
end

menu_box = ui.from_draw(
	box9({
		{16,18,22,24},
		{0,2,6,8}
	})
)
:effect(draw_with_layer(0b0001))

menu_box_light = menu_box
	:effect(draw_with_color(7))

menu_box_dark = menu_box
	:effect(draw_with_color(5))
	
battle_bar = ui.from_draw(
	box9({
		{32,35,37,40},
		{0,2,4,7}
	})
)
:effect(draw_with_layer(0b0001))
:effect(draw_with_color(7))

function menu_panel(c, col)
	return ui_group({
		col == "dark"
			and menu_box_dark
			or  menu_box_light,
		ui_clip(c):inset(1),
	})
end

function progress_bar(fac)
	return ui_group({
		battle_bar,
		ui.from_draw(function(x,y,w,h)
			local xx = x + w * fac
			spr(5,xx-4,y-2)
		end)
		:effect(
			chain({
				draw_with_layer(0b0001),
				draw_with_color(7),
				draw_with_outline_4(0),
				draw_with_outline_4(5),
			})
		)
	}):size("fill",6)
end

function swordplay_ui(top,scn,mnu,opts)
	local opts = opts or { col="light", fac=0.5 }
	local bar_height = 22
	local menu_height = 48

	return ui_layout({
			-- progress bar
			progress_bar(opts.fac or 0.5)
				:inset(4),
			-- top part
			ui_group({
				scn,
				top:align("center","center")
			})
			:inset(4)
			:size("fill", 128 - menu_height - bar_height),
			-- menu part
			menu_panel(mnu, opts.col or "light")
				:inset(1)
				:size("fill", menu_height),
		}, "stack")
		:size("fill","fill")
end

function text_reveal(str,col,len)
	return ui.from_rdr(rdr(function(dims)
		local w,h = unpack(dims)
		local txt = wrap_lines(str,w)
		local dims = rdr(const({
			text_width(txt),
			text_height(txt)-1
		}))
		return ui.create({
			offset   = no_offset,
			min_dims = dims,
			dims     = dims,
			draw=function(c,bnds)
				local show_str = sub(txt,0,len())
				local x,y=unpack(bnds)
				print(show_str,x,y,col)
			end
		})
	end):memo(hash_dims))
end

-- scenes & flow --------------

-- wrap text reveal into a flow
function text_flow(text)
	return flow.once(function(nxt)
		local t = 0
		local function len()
			return t*g_text_speed
		end
		return text_reveal(text,7,len)
			:updatable(function(scn,dt)			
				t += dt
				if t > #text/g_text_speed + 0.5 then
					nxt(text)
				end
			end)
	end)
end

-- wrap a flow as a scene flow
-- todo: unify this with transition flow?
function flow_scn(flw)
	return flow.create(function(nxt,done,err)
		local scn = nil
		flw.go(
			function(s)
				scn = s
			end,
			done,
			err
		)
		nxt({
			update=function(_,dt)
				scn:update(dt)
			end,
			draw=function(_)
				scn:draw()
			end,
		})
	end)
end

-- wrap a draw call as a ui
-- component

function ui_draw(draw)
	return ui.from_draw(function(x,y,w,h)
		draw_with_offset(x+w/2,y+h/2)
			(draw)()
	end)
end

-- wrap a ui component as a scn
function ui_scn(c)
	return {
		update=function(scn,dt)
			c:update(dt)
		end,
		draw=function(scn)
			draw_ui(c, screen_bounds)
		end,
	}
end

-->8
-- scenes

presented_by_scn = flow.once(function(nxt)
	local ui = ui_layout({
			ui_group({
				-- mabbees logo
				ui_spr(128,4,4)
				:effect(draw_with_color(7))
				:align("center","center")
			})
			:size("fill","fit"),
			ui_text("mabbees presents...",7)
		}, "stack", 8)
		:size("fit","fit")
		:align("center","center")
	
	return {
		behavior=behavior.seq({
			behavior_wait(2),
			behavior.once(nxt),
			behavior.never
		}),
		update=function(scn,dt)
			scn.behavior = scn.behavior.update(scn,dt)
		end,
		draw=function(scn)
			draw_ui(ui, screen_bounds)
		end,
	}
end)

title_scn = flow.once(function(nxt)	
	local is_ready = false
	
	local ui = ui_group({	
		ui_spr(168,8,2)
		:align("center","center")
		:translate(0,-16)
		:effect(draw_with_color(7)),
		
		ui_spr(136,8,2)
		:align("center","center")
		:translate(0,16)
		:effect(draw_with_color(7)),
		
		ui_text("SwordPLAY",7)
			:align("center","center")
			:effect(
				chain({
					draw_with_shadow(5),
					draw_with_outline_9(5),
				})
			),
		
		ui_text("press ❎ to start",7)
			:inset(4)
			:align("center","bottom")
			:translate(0,-12)
			:effect(function(draw)
				local blink = (t*3) % 1 > 0.5
				local eff = (is_ready and blink)
					and chain({
						draw_with_color(5),
						draw_with_outline_9(7),
					})
					or chain({
						draw_with_color(7),
						draw_with_outline_9(5),
					})
				return eff(draw)
			end),
	})
	:size("fill","fill")
	
	return {
		behavior=behavior.seq({
			behavior_wait(0.5),
			input_await_❎,
			behavior.once(function()
				is_ready = true
			end),
			behavior_wait(1),
			behavior.once(nxt),
			behavior.never
		}),
		update=function(scn,dt)
			scn.behavior = scn.behavior.update(scn,dt)
		end,
		draw=function(scn)
			draw_ui(ui, screen_bounds)
		end,
	}
end)

-- scene transitions

function transition_flow(f)
	local prev
	return flow.create(function(nxt,done,err)
		f.go(function(cur)
			nxt(transition(cur, prev))
			prev = cur
		end, done, err)
	end)
end

function transition(cur,prv)
	local t = 0
	local dur = 1
	
	local field = wipe_left
	local grid = diamond_grid(16,12,9)
	
	return {
		update=function(scn,dt)
			if t < 0.5*dur and prv then
				prv:update(dt)
			else
				cur:update(dt)
			end
			t += dt
		end,
		draw=function(scn)
			if t < 0.5*dur and prv then
				-- draw "out" transition
				local fac = 1 - 2*t/dur
				prv:draw()
				-- transition
				color(5)
				draw_dots(
					grid,
					chain({
						dots.scale(1,-1),
						scale_to_grid,
					})(field)
				)(1-fac)
			elseif t < dur and prv then
				-- draw "in" transition
				local fac = 2*(t-0.5*dur)/dur
				cur:draw()
				-- transition
				color(5)
				draw_dots(
					grid,
					chain({
						dots.scale(-1,1),
						scale_to_grid,
					})(field)
				)(1-fac)
			else
				cur:draw()
			end			
		end,
	}
end
-->8
-- swordplay flows

function battle_intro_flow(text)
	return flow.once(function(nxt)
		local b = behavior.seq({
			behavior_wait(2),
			behavior.once(nxt),
			behavior.never
		})
		return  ui_group({
			ui_text(text, 7)
				:align("center","center")
				:effect(
					chain({
						draw_with_shadow(5),
						draw_with_outline_9(5),
					})
				)
			}):size("fill","fill")
			:updatable(function(scn,dt)
				b = b.update(scn,dt)
			end)
	end)
end

function clash_flow(state,btl)
	return anim_clash(state.enemy,btl)
		:wrap(function(c)
			local top = ui_empty
			local scn = c
			local mnu = ui_empty
			return swordplay_ui(
				top,
				scn,
				mnu,
				{
					col="dark",
					fac=state.tide/state.max_tide,
				}
			)
		end)
end

function dialogue_flow(text,voice,btl,state)
	return say_text(text,voice)
		:wrap(function(c)
			local top = c:effect(draw_with_outline_9(0))
			local scn = ui_draw(
				battle_frame(state.enemy, btl)
			):effect(draw_with_color(5))
			local mnu = ui_empty
			return swordplay_ui(
				top,
				scn,
				mnu,
				{
					col="dark",
					fac=state.tide/state.max_tide,
				}
			)
		end)
		:map(const(text))
end


function say_text(text, beeps)
	return flow.once(function(nxt)
		local t = 0
		local b = behavior.seq({
			behavior.par({
				beeps,
				behavior.always(function(scn,dt)
					t += dt
				end),
				behavior_wait(#text/g_text_speed+0.1),
			}),
			behavior_wait(0.5),
			behavior.once(nxt),
			behavior.never
		})
				
		return text_reveal(text,7,function()
			return t*g_text_speed
		end)
			:updatable(function(scn,dt)			
				b = b.update(scn,dt)
			end)
	end)
end


function victory_menu(state)
	local victory_screen = ui_group({
		ui_text("victory", 7)
			:align("center","center")
			:effect(draw_with_outline_9(0))
	}):size("fill","fill")
	
	return menu_flow({"continue"})
		:wrap(function(mnu)
			local top = victory_screen
			local scn = ui_draw(
				battle_frame(state.enemy, "victory")
			):effect(draw_with_color(5))
			return swordplay_ui(
				top,
				scn,
				mnu,
				{col="light",fac=1}
			)
		end)
end

function defeat_menu(state)
	local defeat_screen = ui_group({
		ui_text("defeat", 7)
			:align("center","center")
			:effect(draw_with_outline_9(0))
	}):size("fill","fill")
	
	return menu_flow({
		"try again",
		"give up",
	})
		:wrap(function(mnu)
			local top = defeat_screen
			local scn = ui_draw(
				battle_frame(state.enemy, "defeat")
			):effect(draw_with_color(5))
			return swordplay_ui(
				top,
				scn,
				mnu,
				{col="light",fac=0}
			)
		end)
end


function battle_menu(remark, retorts, btl, state)
	return menu_flow(retorts)
		:wrap(function(mnu)
			local top = ui_wrap_text(remark, 7)
				:effect(draw_with_outline_9(0))
			local scn = ui_draw(
				battle_frame(state.enemy, btl)
			):effect(draw_with_color(remark == "" and 7 or 5))
			return swordplay_ui(
				top,
				scn,
				mnu,
				{
					col="light",
					fac=state.tide/state.max_tide,
				}
			)
		end)
end


-- menus

function menu_option(txt,is_focused)	
	return ui_layout({
		ui_spr(1)
		:effect(draw_with_color(7))
		:effect(
			is_focused
				and id
				or  skip_draw
		)
		:translate(0,-2),
		ui_wrap_text(txt, is_focused and 7 or 5)
			:size(function(dims)
				local w,h = unpack(dims)
				return w - 12
			end, function(dims)
				local w,h = unpack(dims)
				return text_height(wrap_lines(txt,w - 12))
			end)
	},"inline",4)
end

function menu_list(opts,scroll)
	return ui_layout(opts, "stack", 4)
		:inset(3)
		:translate(0,-scroll)
end

function menu_bounds(dims, menu_opts, sel)
	local ymin,ymax = 0,0
	local w,h = unpack(dims)
	for i=1,sel do
		local m = (i == 1 and 0 or 4)
		local c = menu_opts[i]
		local cx,cy,cw,ch = unpack(
			measure_ui(c,{0,0,w,h})
		)				
		ymin = ymax + m
		ymax = ymin + ch
	end
	return ymin, ymax
end


function menu_flow(opts, dims)
	dims = dims or {116,38}
	return flow.create(function(nxt,done)	
		local function make_menu(sel,scroll)
			local menu_opts = list.from_tbl(opts)
				:map(function(opt,i)
					return menu_option(opt,sel==i)
				end)
			
			-- adjust scroll in case
			-- selected element is
			-- off screen
			local w,h = unpack(dims)
			local ymin, ymax = menu_bounds(
				dims,
				menu_opts,
				sel
			)
			if scroll > ymin then
				scroll = ymin
			end
			if scroll + h < ymax then
				scroll = ymax - h
			end
			
			return menu_list(menu_opts,scroll)
				:updatable(function()
					if btnp(❎) then
						done(opts[sel])
					elseif btnp(⬆️) then
						nxt(make_menu(
							mid(1, sel-1, #opts),
							scroll
						))
					elseif btnp(⬇️) then
						nxt(make_menu(
							mid(1, sel+1, #opts),
							scroll
						))
					end
				end)
		end
		
		nxt(make_menu(1,0))
	end)
end


-->8
-- swordfighting state

dialogue = {
	{
		insult="i'll cut you down to size",
		flirt="you're a colossal pain in my ass",
		retort="aww, am i too big for you?",
	},
	{
		insult="you dare challenge me?",
		flirt="topping me won't be easy",
		retort="i'd hardly call you a challenge",
	},
	{
		insult="i've seen mannequins less stiff than your form",
		flirt="what's the matter? you look tense",
		retort="not as stiff as you'll be when i'm done",
	},
	{
		insult="get on your knees and beg for your life!",
		flirt="missed again! you always aim too high",
		retort="i wouldn't have to kneel if you weren't so tiny",
	},
	{
		insult="come on, is that all you've got?",
		flirt="come on, give it to me harder!",
		retort="just giving you time to prepare yourself",
	},
	{
		insult="you've never seen moves like mine. do try to keep up.",
		flirt="i'll warn you, i like to go all night...",
		retort="i could do this all night long",
	},
	--
	{
		insult="you smell!",
		retort="...is that the best you got?",
	},
}

bad_insults = list.from_tbl({
	"you smell!",
})
bad_retorts = list.from_tbl({
	"...oh yeah?",
})

-- debug
all_retorts = {}
for tbl in all(dialogue) do
	add(all_retorts, tbl.retort)
end

-- index table of comebacks
comebacks = {}
for tbl in all(dialogue) do
	comebacks[tbl.insult] = tbl.retort
	if tbl.flirt then
		comebacks[tbl.flirt] = tbl.retort
	end
end

function set_has(set,x)
	return list_any(set,function(y)
		return x==y
	end)
end

function set_add(set,x)
	if set_has(set,x) then
		return set
	end
	local ret = list_copy(set)
	add(ret,x)
	return ret
end

function lens_set(lens)
	return function(val)
		return function(state)
			return lens.set(state,val)
		end
	end
end

function lens_append(lens)
	return function(val)
		return function(state)
			return lens.set(
				state,
				set_add(
					lens.get(state),
					val
				)
			)
		end
	end
end

-- battle state:
--   * insults - available insults
--   * retorts - available retorts
--   * tide - the "tide" of battle

start_battle_state = {
	insults={},
	-- debug ★
	retorts=all_retorts,
	tide=0,
	max_tide=2,
	enemy=nil,
	used_insults={},
}


function is_victory(state)
	return state.tide >= state.max_tide
end

set_prop     = compose(prop_lens, lens_set)
set_tide     = set_prop("tide")
set_max_tide = set_prop("max_tide")
set_enemy    = set_prop("enemy")
reset_used   = set_prop("used_insults")({})

append_prop  = compose(prop_lens, lens_append)
learn_insult = append_prop("insults")
learn_retort = append_prop("retorts")
use_insult   = append_prop("used_insults")

function start_battle(state,enemy)
	return chain({
		set_tide(0),
		set_max_tide(#enemy.insults),
		set_enemy(enemy),
		reset_used,
	})(state)
end

function resolve_attack(state,insult,retort)
	local success = not (retort == comebacks[insult])
	local new_state = chain({
		-- learn a retort
		success
			and id
			or  learn_retort(retort),
	})(state)
	
	return {true, new_state}
end

function resolve_defend(state,insult,retort)
	local success = retort == comebacks[insult]
	-- todo: player shouldn't
	-- be able to learn
	-- captain's lines

	-- learn new insult,
	-- update the battle
	-- win points after successful
	-- defenses
	local new_state = chain({
		learn_insult(insult),
		set_tide(
			success
				and state.tide + 1
				or  state.tide
		),
	})(state)
	
	return { success, new_state }
end

-- fighters
-- a fighter has:
-- * insults - insults they will attack with
-- * retorts - retorts they can defend with
-- * voice - a behavior to play sfx (beep speak)

function beep_speak(sfx_idx, spd)
	return behavior.loop(behavior.seq({
		behavior.once(function()
			sfx(sfx_idx,-1)
		end),
		behavior_wait(spd)
	}))
end


-- enemy declarations
draw_spr = suspend(spr)

function draw_char_spr(...)
	return draw_with_layer(0b0010)
		(draw_spr(...))
end

function sw_idle(x,y,flp)
	return draw_char_spr(218,x,y,3,3,flp)
end

function sw_block(x,y,flp)
	-- use outlines that respect
	-- palette layering
	return draw_with_outline_9(0)
		(draw_char_spr(221,x,y,3,3,flp))
end

function sw_attack(x,y,flp)
	return draw_char_spr(10,x,y,4,2,flp)
end

hero = {
	idle = draw_seq({
		draw_char_spr(0,0,0,3,4),
		sw_idle(18,0),
	}),
	block = draw_seq({
		draw_char_spr(3,0,0,3,4),
		sw_block(1,-12),
	}),
	attack = draw_seq({
		draw_char_spr(6,0,0,4,4),
		sw_attack(32,0),
	}),
}

pirate_1 = {
	insults = {
		dialogue[1].insult,
		dialogue[2].insult,
	},
	retorts = {
		dialogue[1].retort,
		dialogue[2].retort,
		dialogue[3].retort,
		dialogue[6].retort,
		dialogue[7].retort,
	},
	voice = beep_speak(60,2/60),
	idle = draw_seq({
		draw_char_spr(64,0,0,3,4),
		sw_idle(-18,0,true),
	}),
	block = draw_seq({
		draw_char_spr(67,0,0,3,4),
		sw_block(0,-12,true),
	}),
	attack = draw_seq({
		draw_char_spr(70,0,0,4,4),
		sw_attack(-33,0,true),
	}),
}

pirate_2 = {
	insults = {
		dialogue[3].insult,
		dialogue[4].insult,
	},
	retorts = {
		dialogue[3].retort,
		dialogue[4].retort,
		dialogue[5].retort,
		dialogue[2].retort,
		dialogue[7].retort,
	},
	voice = beep_speak(60,2/60),
	idle = draw_seq({
		draw_char_spr(128,0,0,3,4),
		sw_idle(-20,0,true),
	}),
	block = draw_seq({
		draw_char_spr(131,0,0,4,4),
		sw_block(-1,-8,true),
	}),
	attack = draw_seq({
		draw_char_spr(135,0,0,4,4),
		sw_attack(-33,0,true),
	}),
}

pirate_3 = {
	insults = {
		dialogue[5].insult,
		dialogue[6].insult,
	},
	retorts = {
		dialogue[5].retort,
		dialogue[6].retort,
		dialogue[1].retort,
		dialogue[4].retort,
		dialogue[7].retort,
	},
	voice = beep_speak(60,2/60),
	idle = draw_seq({
		draw_char_spr(192,0,0,3,4),
		sw_idle(-18,0,true),
	}),
	block = draw_seq({
		draw_char_spr(195,0,0,3,4),
		sw_block(-1,-8,true),
	}),
	attack = draw_seq({
		draw_char_spr(198,0,0,4,4),
		sw_attack(-33,0,true),
	}),
}

captain = {
	insults = {
		dialogue[1].flirt,
		dialogue[2].flirt,
		dialogue[3].flirt,
		dialogue[4].flirt,
		dialogue[5].flirt,
		dialogue[6].flirt,
	},
	voice = beep_speak(61,1/60),
	idle = draw_seq({
		draw_char_spr(77,0,0,3,4),
		sw_idle(-19,-3,true),
	}),
	block = draw_seq({
		draw_char_spr(74,0,0,3,4),
		sw_block(-1,-11,true),
	}),
	attack = draw_seq({
		draw_char_spr(139,0,0,4,4),
		sw_attack(-33,-1,true),
	}),
}


-->8
-- battle scene

function unused_insults(is,state)
	return list.from_tbl(is)
		:filter(function(i)
			return not
				set_has(state.used_insults,i)
		end)
end

player_adapter = {
	get_insult = function(state)
		local available_insults =
			unused_insults(state.insults, state)
				:concat(bad_insults)
		return battle_menu(
			"",
			available_insults,
			"attack",
			state
		)
	end,
	get_retort = function(state,insult)
		local available_retorts =
			list.from_tbl(state.retorts)
				:concat(bad_retorts)
		return battle_menu(
			insult,
			available_retorts,
			"defend",
			state
		)
	end,
	dir = "attack",
	voice = beep_speak(61,1/60),
}

function enemy_adapter(enemy)
	return {
		get_insult=function(state)
			local insult = rnd(
				unused_insults(enemy.insults,state)
			)
			return flow.of(insult)
		end,
		get_retort=function(state,insult)
			-- pick the right retort,
			-- if the enemy knows it
			-- otherwise, use a generic response
			local has_retort = set_has(enemy.retorts, comebacks[insult])
			local retort = has_retort
				and comebacks[insult]
				or  "ouch..."
			return flow.of(retort)
		end,
		dir = "defend",
		voice = enemy.voice,
	}
end

function captain_adapter(enemy)
	return {
		get_insult=function(state)
			-- captain always uses his lines in order
			local insult = unused_insults(enemy.insults,state)[1]
			return flow.of(insult)
		end,
		get_retort=function(state,insult)
			-- captain doesn't need to retort
			assert(false, "error")
		end,
		dir = "defend",
		voice = enemy.voice,
	}
end

function battle_turn(state,attacker,defender,resolve)
	return attacker.get_insult(state)
		:flatmap(function(insult)
			local newstate = use_insult(insult)(state)
		
			return dialogue_flow(insult,attacker.voice,attacker.dir,newstate)
				:flatmap(function()
					return defender.get_retort(newstate,insult)
				end)
				:flatmap(function(retort)
					return dialogue_flow(retort,defender.voice,defender.dir,newstate)
				end)
				:map(function(retort)
					return resolve(newstate,insult,retort)
				end)
		end)
end

function player_turn(state,enemy)
	return battle_turn(
		state,
		player_adapter,
		enemy_adapter(enemy),
		resolve_attack
	)
end

function enemy_turn(state,enemy)
	return battle_turn(
		state,
		enemy_adapter(enemy),
		player_adapter,
		resolve_defend
	)
end

function captain_turn(state,enemy)
	return battle_turn(
		state,
		captain_adapter(enemy),
		player_adapter,
		resolve_defend
	)
end

function battle(state,enemy,turn)
	local enemy_insults = unused_insults(
		enemy.insults,
		state
	)
	-- end battle if enemy ran
	-- out of insults
	if is_victory(state)
	or (turn == "enemy" and #enemy_insults == 0)
	then
		return flow.of(state)
	end

	local do_clash = clash_flow(
		state,
		turn == "player"
			and "attack"
			or  "defend"
	)

	local do_turn = turn == "player"
		and player_turn(state,enemy)
		or  enemy_turn(state,enemy)
	
	return do_clash
		:flatmap(const(do_turn))
		:flatmap(function(res)
			local result,newstate = unpack(res)
			local opponent = turn == "player"
				and "enemy"
				or  "player"
			
			if not result then
				return flow.err(newstate)
			end
			
			return battle(newstate,enemy,opponent)
		end)
end

-- instead of trading insults
-- only enemy delivers insults
-- and player must defend
function captain_battle(state,enemy)
	local enemy_insults = unused_insults(
		enemy.insults,
		state
	)
	-- end battle if enemy ran
	-- out of insults
	if is_victory(state) or #enemy_insults == 0 then
		return flow.of(state)
	end

	return captain_turn(state,enemy)
		:flatmap(function(res)
			local result,newstate = unpack(res)
			if not result then
				return flow.err(newstate)
			end
			return captain_battle(newstate,enemy)
		end)
end

-->8
-- swordplay scene

function handle_defeat(state)
	return defeat_menu(state)
		:flatmap(function(r)
			if r == "try again" then			
				return flow.err({"retry", state})
			else
				return flow.err({"restart", nil})
			end
		end)
end

function retry_with(f)
	return function(e)
		local choice, new_state = unpack(e)
		if choice == "retry" then
			return f(new_state)
		end
		return flow.err(e)
	end
end

function enemy_battle(enemy,i)
	return function(state)
		local btl = flow.seq({
			battle_intro_flow("battle "..i),
			battle(
				start_battle(state,enemy),
				enemy,
				"player"
			)
		})
		:flatmap(function(new_state)
			return victory_menu(new_state)
				:map(const(new_state))
			end
		)
		:handle_err(handle_defeat)
		return flow_scn(btl:wrap(ui_scn))
	end
end

function enemy_battles(state)
	-- randomize enemy order
	local enemies = shuffle({
		pirate_1,
		pirate_2,
		pirate_3,
	})
	return flow.of(state)
		:flatmap(enemy_battle(enemies[1],1))
		:flatmap(enemy_battle(enemies[2],2))
		:flatmap(enemy_battle(enemies[3],3))
		:handle_err(retry_with(enemy_battles))
end

function final_battle(state)
	local btl = flow.seq({
		battle_intro_flow("final battle"),
		captain_battle(
			start_battle(state,captain),
			captain
		),
		battle_intro_flow("complete!"),
	})
	:handle_err(handle_defeat)
	return flow_scn(btl:wrap(ui_scn))
		:handle_err(retry_with(final_battle))
end

swordplay_scn = enemy_battles(start_battle_state)
	:flatmap(final_battle)

-->8
-- battle anims

-- draw calls -----------------

draw_btl_hero = chain({
	draw_with_outline_9(0),
	draw_with_offset(-32,-8)
})

draw_btl_enemy = chain({
	draw_with_outline_9(0),
	draw_with_offset(8,-8),
})

function draw_battle_victory(enemy)
	return draw_btl_hero(hero.idle)
end

function draw_battle_defeat(enemy)
	return draw_btl_enemy(enemy.idle)
end

function draw_battle_idle(enemy)
	-- hero on top layer
	return draw_seq({
		draw_btl_enemy(enemy.idle),
		draw_btl_hero(hero.idle),
	})
end

function draw_battle_attack(enemy)
	-- hero on top layer
	return draw_seq({
		draw_btl_enemy(enemy.block),
		draw_btl_hero(hero.attack),
	})
end

function draw_battle_defend(enemy)
	-- enemy on top layer
	return draw_seq({
		draw_btl_hero(hero.block),
		draw_btl_enemy(enemy.attack),
	})
end

function battle_frame(enemy, btl)
	if btl == "idle" then
		return draw_battle_idle(enemy)
	elseif btl == "victory" then
		return draw_battle_victory(enemy)
	elseif btl == "defeat" then
		return draw_battle_defeat(enemy)
	elseif btl == "attack" then
		return draw_battle_attack(enemy)
	elseif btl == "defend" then
		return draw_battle_defend(enemy)
	end
end

-- flippable drawings

function effect_frame(n,w,h)
	return function(flp_x)
		return chain({
			draw_with_layer(0b0100),
			draw_with_offset(-16,-16)
		})(
			draw_spr(n,0,0,w,h,flp_x)
		)
	end
end

function lift_effect(eff)
	return function(draw)
		return function(flp_x)
			return eff(draw(flp_x))
		end
	end
end

slash_1 = effect_frame(64,4,4)
slash_2 = effect_frame(68,4,4)
slash_3 = lift_effect(draw_with_offset(0,-10))
	(effect_frame(72,4,4))
slash_4 = effect_frame(76,4,4)

sparks_1 = effect_frame(128,4,4)
sparks_2 = effect_frame(132,4,4)
sparks_3 = effect_frame(136,4,4)
sparks_4 = effect_frame(140,4,4)


-- animation timing -----------
function anim(draw,beh)
	return flow.once(function(nxt)
		local b = behavior.seq({
			beh,
			behavior.once(nxt),
			behavior.never,
		})
		return ui_draw(draw)
			:updatable(function(a,dt)
				b = b.update(a,dt)
			end)
	end)
end

function anim_frame(f,draw)
	return anim(
		draw,
		behavior_frames(f)
	)
end

function anim_slash(slash,sparks)
	return function(dir)
		local flp_x = dir != "attack"
		local dx = flp_x and -1 or 1
		
		local slash  = slash(flp_x)
		local sparks = chain({
			draw_with_offset(12*dx,0),
			draw_with_outline_9(0),
		})(sparks(flp_x))
		local both = draw_seq({
			draw_with_palette(monochrome_palette(5))
				(slash),
			sparks,
		})
		
		return flow.seq({
			anim_frame(2,slash),
			anim_frame(1,both),
			anim_frame(1,sparks),
		})
	end
end

anim_slash_1 = anim_slash(slash_1, sparks_1)
anim_slash_2 = anim_slash(slash_2, sparks_2)
anim_slash_3 = anim_slash(slash_3, sparks_3)
anim_slash_4 = anim_slash(slash_4, sparks_4)

-- battle anims ---------------

function anim_clash(enemy, btl)
	local atk = btl
	local def = btl == "attack"
		and "defend"
		or  "attack"
	
	return flow.seq({
		anim_frame(15,battle_frame(enemy,"idle")),
		-- flurry 1
		anim_slash_2(atk),
		anim_frame(2,noop),
		anim_slash_3(def),
		anim_frame(2,noop),
		anim_slash_2(atk),
		anim_frame(1,noop),
		-- hold
		anim_frame(4,battle_frame(enemy,atk)),
		-- flurry 2
		anim_slash_1(atk),
		anim_frame(2,noop),
		anim_slash_2(def),
		anim_frame(1,noop),
		-- hold
		anim_frame(4,battle_frame(enemy,def)),
		-- flurry 3
		anim_slash_4(def),
		anim_frame(4,noop),
		anim_slash_1(atk),
	})
end


__gfx__
00000000000004405411110100000000011331100001000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000446646640000000000000111133110011100000000000000000000000000000000000000000000000000000000000000000000000000000000000
00900900040666667664000100000000110002310001000000004404400000000000000000000000000000000000000400000000000000000000000000000000
00099004470666663622400100000000130200330001000000444444440000000002202200000000000000000000444400000000000000200000000000000000
00099046771777375640600100000020333331132023200040444444444000000222222220000000000000004444444000000000000222200000000000000000
00900966630662467660600100002220233333102203224440444440400400020222222222000000000044444444000000000002222222000000000000000000
00000026200264666660200000022220222022200023046440444044440402220222220200200000444444440000000000022222222000000000000000000000
00000002000026667671110100022200220222220221244600440444440422220226462220204444444400000000000222222220000000000000000000000000
00110110001107766620000000002040022222220202226000044444440022200220666264644444002220000002222222200000000000000000000000000000
01001005454012676200000000000440002222220000222000004444440002000462666264440000000222022222222000000000000000000000000000000000
01010547672400232000000000004400002222200020000000004444400044440442666220000000220222022220000000000000000000000000000000000000
01055767270640010000000000046222000222000466644444000444044444440006666200022220220222000000000000000000000000000000000000000000
00567676065264104000000000662220200000444666644440400000266666622444662022222220002222000000000000000000000000000000000000000000
04676762466167446000000006622020220040264666444440440222266662220200000022222200222220000000000000000000000000000000000000000000
06667620666436626400000066220222022446226662440044064026266222220220000022220000000000000000000000000000000000000000000000000000
46662204666206606604440462202222202646222222004444626046260220022022002022000000000000000000000000000000000000000000000000000000
66620046666046602600440422220222002200222220004444266044440002222202022020000000000000000000000000000000000000000000000000000000
66600066662066604640044062200022022200002200444440444004400002222022022220000000000000000000000000000000000000000000000000000000
66200066660066206620404462220002022200000044444440444044400222220222002200000000000000000000000000000000000000000000000000000000
22440466660466006604440442220200022020000444444400440040022222220222022200000000000000000000000000000000000000000000000000000000
04620666620222042604400400220220222020000444444404440040222222200220020000000000000000000000000000000000000000000000000000000000
02204666204444024204404000002220000022000044400000000400222222202220020000000000000000000000000000000000000000000000000000000000
00002262046666406000040000220200222002000000004444440040022200000000200000000000000000000000000000000000000000000000000000000000
00000020466226642000000000022002222202004404444444444040000002222220020000000000000000000000000000000000000000000000000000000000
00000444662046664000000000000222002220044444444440044406602222222222020000000000000000000000000000000000000000000000000000000000
00004666620066666400000000222220022222444444400000000466662222220022202200000000000000000000000000000000000000000000000000000000
00046666200026666600000000222220022266644440000000000266662200000000222220000000000000000000000000000000000000000000000000000000
00066666000046666200000002222200026666644000000000022266660000000000022220000000000000000000000000000000000000000000000000000000
00466662000066662000000002222200046666400000000002222266440000000000022220000000000000000000000000000000000000000000000000000000
00266620000466620000000022222000006662000000000022222644400000000000022220000000000000000000000000000000000000000000000000000000
00466640000666644000000002220000066664000000000002220444444000000000222200000000000000000000000000000000000000000000000000000000
00222220000222222000000022222000022222200000000022222000000000000000222222000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000022000444440000000000000000004444444400000000000000000000220000000000000000022200200000000000
00000000000000000000000000000000000264444444444440000000000000000000044444400000000000000002200000000000000002002002022000000000
00000002222220000000000000000000002200000000444444400000000000000000000004444000000000020022000220000000000002200020000200022200
00000022222222000000000000000000022002000000004444440002222220000000000000044440000000000220000002000000000020022222000020200020
00000022222222000000000000000000220022220000000444444422222222000000000000000444000000002200220002002220000020200000222020000020
00000002222222020000000000022202200222220000000444444466222222000000000000400044400000022000002222020002000002002222000220000200
00000020000000200000000000222022002222220200000044444446622222020000000000044404442220220022220002000002000000022222220000002000
00000022222220020000044446002200000000002000000044444464440000200000000000000444466202200222222200000020000000022222200000200020
00000022222220000000000464460220022222200200000044444466662220020000000000000044644620000222222000000200000000002222200000222220
00000002222200222000000006666020022222200000000044444466666220000000000000040004446462200022222000020446000000000222000200022200
00000000222000022220000002664400022222002200000044444446666200000000000000000444466646200002220020022666444400020000020022000020
00000000000000220222200000044640002220000222000064644444666000000000000000000044466644400200000002002220444444660022022022202220
00000002000022200202220000066640000000002022220464646464444000222020202000000004444444022002202202200002000046664666020022222220
00000000022222220202022000446460000000222020222446464464444002222222222222000004444660222022202002220222000000024664664620222220
00000022002222222002000204444620220022222020206644446666444022222020202020020000444622002022022022222222000002200220664664662200
00000000020022200000220244446202022022222200006444444446640222220020202000222000444022220022022022022222000002220220642264446440
00000022202222222000006644440020202000222000264644444464422222000000000000222004440022222022020222022220000002020222402264004444
00000000000002222204666444400220220022222022464644444444222222222200000000000044400002202022200222000200000000020062002664044440
00000202202222226444446640000022000000022026446444444440000022222000000000000444000000002002200222000000000000020000006662044400
00000000000044666664620000000000000022222066446444444400002222222222000000004440000000002000000222200000000000020022246622444000
00000022444666666000022000000000000000002444464444444000000000222220000000044400000000002002220222200000000000022022640226640000
00444444444440000000000000000000000002266444444444400000000022222222220000444000000000022202222022220000000000220062222066620000
00000000002000002220000000000000000004444444444440000000002000000022200004440000000000022022222202222000000000220222222466222000
00000000222222222222000000000000000664444466644000000000022220000000000044400000000000222022022220222200000002220220226642222000
00000000222200222222000000000000046666446666620000000002222222222000002644000000000000222022022220222200000002220222466202222000
00000002222220222222000000000000466666666622220000000022222200022222226660000000000002220222022220222200000002220266462202222000
00000002222200022222200000000000666664402222220000000022222000000002266622000000000002202220002220222000000002246664022202220000
00000000000000022222220000000000444000002222222000000000000000000000466222220000000000202220000200000044444444646220002000000000
00000000222000000222220000000000022200000022222000000002220000000000440222222200000000020002000022222000000000020002000222220000
00000000000000000020002000000000000000000002000200000000000000000000000002000020000000020002000000200200000000020002000002002000
00000000020000000002220000000000002000000000222000000000200000000000000000022200000000200002000002000200000000200002000020002000
00000000020000000022222000000000002000000002222200000000200000000000000000222220000000222222000002222000000000222220000022220000
00000002222000000000000000000000000000022220000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000022622200000000000000000000000000002222000000000000000000000000000040000000000000000000000000000000000000000000990000000000
00000222222220000000000000000000000002202222200000000000000000002222000000000000000000000000002220020000000000000000990000000000
00000222222220000000000000000000000022002222200000000000000000022222200000000000000000000000200200202200000002220000990000000000
00000002000220000000000000000000000220020002200000000000000000222222bbd999999999999999999999bb999b9999b99999b999b999990000000000
000002022033111111000000000000000022000220220000000000000000002222bb2200999d000000009999999b90222220999b999990002000990000009900
000002222323030450111000000000000220022226620200000000000040000029002299990400000099999999920200009bbb9b9990000200009999999d9990
0000002223211335332301000000000022000022266002202222000004400020bb9bb999999dd99999999d999999b99bbbb999bb999999b99999999999999990
00000002721133773333201000022202200000022640222222222000444400222b222020040044222000000000000022222220000000200020009999999999d0
000022214513317333333105442220220000222000422022222220044400000222bb002220426622222040000000002222220000000022222000990400009900
000222321733133333333354560022000002222202260222222222444000000022209bbbbbbdbfbbbbbbb999bbbb999bbbbb999999999bbb9999994000000000
002223202231333332226731210202220022222022242222222226642222022200002202222266622226220022222000222000200222000420009d0044400000
00223233312322232173323103222022022222222022222220226620222222222022202222222662002022000222002000002002222220262000d94440000000
00232333313233323313332303220002022222222022622222066222022222220222022222222600000220000000022002202220222222222044dd4400000000
00232132233233323201332310100200022220222222222222042222000222222202222222220000000000000000222022202202222222222004400000000000
00012123323233320112332310120000222220622222222200422222000002222202222222200000000000000000002022202066200222220000000000000000
00210333333633301333332111012222222200666222222006222220000000222220222222244000400000000000000022202022224002240000000000000000
00001213332733352333321111010022220000026622222022222200000000002222222222990000000000400000002022202022222000000000000000000000
00021200227723217022210111010000200000002222222020222000000000000222222222b90000004000004000002022202022222244000000000000000000
00001211337732311753310111010000000000002222222002022000000044000022222222bb0000004400000040002002220202226662000000400000000000
00212101176732330177110111101000000000000222222244664400000000000000222222b9b999999dd999999999b9999999b9bfffbbb9999999d999990000
002101121554010031155001111010000000000200000000204444000000000000bb0000009922004004dd99999990020022bd999bfbb22220000099d9009900
00301131033333331131450111110100000000200662222200204400000000000b99bbbbbbbb00200099d9999990000202fbbbb999b222622200999900000090
0010131100101010102055411111010004000200440000000020000000000000299999999999999b9999999d999999bb9bbbbbbbf99bbbbbb999999999999999
00010200110000000013045411110100000002000000000000020000000000002999999999990006000000004000000002220026220026222000000000000090
00003111003311111102004540001000000020000022000000020000000000020099022222b90004200000000400000222222040222202220000000000009900
000020000020222000002004551100000000200000202220000020000000002000002000009bb9999b999d9999d999bbbbb99d99999bdbb99b99999999990000
00002000020000222222200400000000000020000200002222222000000000222220200000990220222200000000002222000000000002002222000000000000
00000222200040000000000000000000000002222000000000000000000000000002000000990002000000000000002222000000000000022222220000000000
00000000000040000222220000000000000000000000000002222200000000222200000000990000022222400000002000200000000000000220002000000000
00000222200000000022220000000000000002222000000000222200000000022220000000000000002222000000002000200000000000000200002000000000
00002222200000000222222000000000000022222000000002222220000002222220000000000000022222200000022222200000000000000022222000000000
00000000000000000000000000000000444444440000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000004444440000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000002222000000000000000000000020022000444400000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000022222200000000000000000000200220000004444000000002222000000000000000000000000000000000000000000000000000000000000000000000
00000222222220000000000000000002002200200000044400000022222200000000000000000000000000000000000000000000000000000000000000000000
00000222222220000000000000000000022002200000004440000222222220000000000000000000000000000000000000000000000000000000000000000000
00000222222220000000000000000000220022200000000444000222222220000000000000000000000000000000000000000000000000000000000000000000
00000222222264444444440000000002200222200000000044400222222220000000000000000000000000000000000000000000000000000000000000000000
00000022222200000000000000022022002222000000000004440222222220000000000000000000000000000000000000000000000000000000000000000000
00000022222000000000000000220220022220000000000000444022222200000000000000000000000000000000000000000000000000000000000000000000
00000002220002202200000000022000022200022000000000044422222000000000000000000000000000000000000000020000000000000000000000000000
00000004444446662220000000202220000000022020000022224442220002202200000000000000000000000000000000220000022000000000000000000000
00000002002202222222000000220202220022022222000022226644000002222222222000000000000000000000000002200000002200000000000000000000
00000022022202222222200000200002220222022222200002222466002202222222222002000000000000000000000022000000000220000000000000000000
44444666646466646666664444446466220220222222220000022666422202222222220022220000000000000000000220000000000022000000000000000000
44444666646466664466666444666664220202220222222000000266602022200022020222222000000000000000002200000000000002200000000000000000
00000022220022222002200000022222022002222002222200000066642022222200000000222000000000000000022000000000000000220000000000000000
00000220222222222000002200002222022222222200220000000026660022222200000000000000000000000000220000000000000000022000000000000000
00000022002222244444466640002200000222220000002200000006662222222220000000000000000000000002200000000000000000002200000000000000
00000022000000000000022200000000000000000000022200000004666222222220000000000000000000000022000000000000000000000220000000000000
00000020200000000200222200000000020000000000022200000004446222222200220000000000000000000220000000000000000000000022000000000000
00000002220000022220022000000000222000002200222200000000444022200002222220000000000000002200000000000000000000000002200000000000
00000002222222222222000000000000222222222220022000000000464400000222222200000000000000022000000000000000000000000000220000000000
00000000022222222220000000000000002222222222000000000000466600022222220000000000000000220000000000000000000000000000022000000000
00000002000266664440000000000000200022222000000000000002444622222220000220000000022202200000000000000000000000000000002202220000
00000022220000000222000000000002222000000022200000000022664422222002222222200000002202000000000000000000000000000000000220222000
00000022222000002222200000000002222200000222200000000022666400000000022222220000000220200000000000000000000000000000000002200200
00000002222000002222200000000002222000002222200000000022664440000000000222222200002022200000000000000000000000000000000222020020
00000000222000000222220000000000222000000222220000000002664440000000000002222220022202200000000000000000000000000000000220222200
00000002222200000022222000000002222200000022222000000022266440000000000000022222022002000000000000000000000000000000000200022200
00000022222200000222222000000022222200000222222000000222266444000000000000222222022020000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000
__label__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000077000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000077000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000077000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000077777777777777777777777777777777777777777777777777000000000000000000000000000000000000
00000000000000000000000000000000007700000077000000007777777770000000777777777000000000777700770000000000000000000000000000000000
00000000000000000000000000000000077777777777000000777777777000000077777777700000000077770000007000000000000000000000000000000000
00000000000000000000000000000000077777777777777777777777777777777777777777777777777777777777777700000000000000000000000000000000
00000000000000000000000000000000077777777777000000000000000000000000000000000000000000000000007000000000000000000000000000000000
00000000000000000000000000000000007700000077000000000000000000000000000000000000000000000000770000000000000000000000000000000000
00000000000000000000000000000000000000000077777777777777777777777777777777777777777777777777000000000000000000000000000000000000
00000000000000000000000000000000000000000077000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000077000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000077000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000005555555555555555000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000005555757557757775775555555500555555550000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000055775757575757575757557757505577575750000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000057555757575757755757575757505757577750000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000055575777575757575757577757555777555750000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000057755777577557575777575555775757577550000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000055555555555555555555555555555555555550000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000055555555555555555555555005555555555500000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000077000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000077000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000077000000000000000000000000000000000000000000
00000000000000000000000000000000000077777777777777777777777777777777777777777777777777000000000000000000000000000000000000000000
00000000000000000000000000000000007700007777000000007777777770000000777777777000000077000000770000000000000000000000000000000000
00000000000000000000000000000000070000777700000000777777777000000077777777700000000077777777777000000000000000000000000000000000
00000000000000000000000000000000777777777777777777777777777777777777777777777777777777777777777000000000000000000000000000000000
00000000000000000000000000000000070000000000000000000000000000000000000000000000000077777777777000000000000000000000000000000000
00000000000000000000000000000000007700000000000000000000000000000000000000000000000077000000770000000000000000000000000000000000
00000000000000000000000000000000000077777777777777777777777777777777777777777777777777000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000077000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000077000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000077000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000555555555555555555555000055555550000555555555000055555555555555555555000000000000000000000000000000
00000000000000000000000000000577757775777557755775000557777755000577755775000557757775777577757775000000000000000000000000000000
00000000000000000000000000000575757575755575557555000577575775000557557575000575555755757575755755000000000000000000000000000000
00000000000000000000000000000577757755775577757775000577757775000057557575000577755755777577555750000000000000000000000000000000
00000000000000000000000000000575557575755555755575000577575775000057557575000555755755757575755750000000000000000000000000000000
00000000000000000000000000000575057575777577557755000557777755000057557755000577555755757575755750000000000000000000000000000000
00000000000000000000000000000555055555555555555550000055555550000055555550000555505555555555555550000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

__sfx__
000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c10a00001c50021500265002650000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c10a000026500215001c5001c50000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010800000d05500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010600001305500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
