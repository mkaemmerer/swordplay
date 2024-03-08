pico-8 cartridge // http://www.pico-8.com
version 42
__lua__
--battle anims
--by mabbees

-- sprite layers
-- 1 : ui
-- 2 : characters
-- 3 : battle animations
-- 4 : title

function _init()
	t = 0
	scn = {}
	
	flow.loop(battle_flow(pirate_2))
		.go(
			function(s) scn = s end
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
function id(...) return ... end

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

-- create a 1-bit palette that
-- sets masked bits to a solid
-- color
-- * b: the bitmask
-- * c: the color
function palette_1bit(b,c)
	local ret = {}
	for i=0,15 do
		ret[i] = (i & b == 0)
			and 0
			or  c
	end
	return ret
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
	local p = monochrome_palette(7)
	return suspend(function(draw)
		palt(bits)
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
		"focus",
		"blur",
		"update",
		"select",
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

function ui_spr(n,sw,sh,b,col)
	sw = sw or 1
	sh = sh or 1
	b = b or 1
	col = col or 7
	local w=8*sw
	local h=8*sh
	return ui.from_draw(function(x,y,w,h)
		spr(n,x,y,sw,sh)
	end)
	:effect(draw_with_palette(palette_1bit(b,col)))
	:size(w,h)
end

function ui_clip(c)
	return ui.from_draw(function(x,y,w,h)
		clip(x,y,w,h)
		draw_ui(c, {x,y,w,h})
		clip()
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
		
		-- focus
		focusable=function(c,f,sel)
			local focus,blur
			focus = function(_)
				return ui.from_ui(f, {
					focus=focus,
					blur=blur,
					select=sel,
				})
			end
			blur = function(_)
				return ui.from_ui(c, {
					focus=focus,
					blur=blur,
				})
			end
			return blur()
		end,
		
		-- selection
		selectable=function(c,f)
			return ui.from_ui(c,{
				select=f,
			})
		end,
		
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
-- pirates

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
	}):wrap(ui_scn)
end

-->8
-- battle scene

function btn_flow(scn)
	return flow.once(function(nxt)
		local b = behavior.seq({
			input_await_not❎,
			input_await_❎,
			behavior.once(nxt),
			behavior.never,
		})
		return {
			update=function(_,dt)
				scn:update(dt)
				b = b.update(nil,dt)
			end,
			draw=function(_)
				scn:draw()
			end,
		}
	end)
end

battle_scn = chain({
	battle_frame,
	ui_draw,
	ui_scn,
	btn_flow,
})

function battle_flow(enemy)
	return flow.seq({
		battle_scn(enemy, "idle"),
		anim_clash(enemy, "attack"),
		battle_scn(enemy, "attack"),
		anim_clash(enemy, "defend"),
		battle_scn(enemy, "defend"),
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
