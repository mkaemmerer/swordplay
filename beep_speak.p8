pico-8 cartridge // http://www.pico-8.com
version 41
__lua__
--beep speak
--by mabbees

-- global constants
debug = false
g_text_speed = 40

function _init()
	-- global state
	t = 0
	scn = nil
	
	text_scn.go(function(s)
		scn = s
	end, noop)
	
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

function copy(list)
	local ret = {}
	for x in all(list) do
		add(ret, x)
	end
	return ret
end

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

function monoid_concat(empty)
	return reduce(
		function(a,m)
			return a:concat(m)
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
	return list.create(copy(tbl))
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

function compose_lens(l1,l2)
	return {
		get=function(s)
			return l2.get(l1.get(s))
		end,
		set=function(s, x)
			local inner = l1.get(s)
			return l1.set(s, l2.set(inner, x))
		end,
	}
end

chain_lens = reduce(compose_lens, id_lens)

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
				function(nxt, done)
					m.go(nxt, compose(f,done))
				end)
		end,
		flatmap=function(m,f)
			return flow.create(
				function(nxt, done)
		   m:map(f).go(nxt, function(n)
		    n.go(nxt, done)
		   end)
		  end)
		end,
		wrap=function(m,f)
			return flow.create(
				function(nxt,done)
					m.go(function(x)
						nxt(f(x))
					end, done)
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

function flow.once(make_scene)
 return flow.create(
	 function(nxt, done)
	  nxt(make_scene(once(done)))
	 end)
end

function flow.defer(f)
	return flow.create(function(nxt, done)
		done(f())
	end)
end

flow.seq = monad_seq(flow.of(nil))

flow.loop = monad_loop


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

function draw_with_pattern(pat)
	return suspend(function(draw)
		local p1,p2,p3 = peek(0x5f31,3)
		fillp(pat)
		draw()
		poke(0x5f31,p1,p2,p3)
	end)
end

function draw_with_transparency(col)
	return suspend(function(draw)
		palt(col,true)
		draw()
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


draw_seq = suspend(
	function(tbl)
		foreach(tbl, function(cmd)
			cmd()
		end)
	end
)

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

-- easing ---------------------

ease = {}

function ease.linear(x)
	return x
end

function	ease.quad_in(x)
	return x * x
end

function ease.quad_out(x)
	return 1 - (1 - x) * (1 - x)
end

function ease.quad_in_out(x)
	return x < 0.5
		and 2 * x * x
		or  1 - (-2*x + 2)^2 / 2
end

-- text utils -----------------
function wrap_lines(txt,width)
	local buffer=""
	local len = 0

	for i, word in words(txt) do
		local w = word_width(word)
		if len+w > width then
			-- push next word to next line
			buffer = buffer.."\n"..word
			len=w
		else
			-- next word still fits on the line
			buffer = buffer..word
			len += w
		end
		
		if txt[i-1] == "\n" then
			-- word break is also a line break
			buffer = buffer.."\n"
			len=0
		else
			-- word break is a space
			buffer = buffer.." "
			len += 4
		end
	end
	
	return buffer
end

function words(txt)
	local function iter(txt, start)
		if start > #txt then return nil end
		for i=start,#txt do
			if txt[i] == " " or txt[i] == "\n" then
				return i+1, sub(txt,start,i-1)
			end
		end
		return #txt+1, sub(txt,start,#txt)
	end
	return iter, txt, 1
end

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
	local height=6
	for i=1,#txt do
		local c = txt[i]
		if c=="\n" then
			height += 6
		end
	end
	return height
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
function ui_box(fill,stroke)
	return ui.from_draw(function(x,y,w,h)
		rectfill(x,y,x+w-1,y+h-1,fill)
		if stroke!=nil then
			rect(x,y,x+w-1,y+h-1,stroke)
		end
	end)
end

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

function ui_spr(n,sw,sh)
	sw = sw or 1
	sh = sh or 1
	local w=8*sw
	local h=8*sh
	return ui.from_draw(function(x,y,w,h)
		spr(n,x,y,sw,sh)
	end)
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
		return ui_group(arr)
	end)
	
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


function list_copy(tbl)
	local ret = {}	
	for x in all(tbl) do
		add(ret, x)
	end
	return ret
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

menu_box_dark = ui.from_draw(
	box9({
		{24,26,30,32},
		{0,2,6,8}
	})
)

battle_bar = ui.from_draw(
	box9({
		{32,35,37,40},
		{0,2,4,7}
	})
)

function menu_panel(c, col)
	return ui_group({
		col == "dark"
			and menu_box_dark
			or  menu_box,
		ui_clip(c:inset(3))
			:inset(1),
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
				draw_with_outline_4(0),
				draw_with_outline_4(5),
			})
		)
	}):size("fill",6)
end

function swordplay_ui(top,mnu,opts)
	local opts = opts or { col="light", fac=0.5 }
	local bar_height = 22
	local menu_height = 48

	return ui_layout({
			-- progress bar
			progress_bar(opts.fac or 0.5)
				:inset(4),
			-- top part
			ui_group({
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
		:selectable(mnu.select)
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

-- wrap a flow as a scene flow
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
-- text flow

-- reveal text and beep speak
behavior_beeps_lo = behavior.loop(behavior.seq({
	behavior.once(function()
		sfx(60)
	end),
	behavior_wait(2/60)
}))

behavior_beeps_hi = behavior.loop(behavior.seq({
	behavior.once(function()
		sfx(61)
	end),
	behavior_wait(1/60)
}))


function say_text(text, beeps)
	return flow.once(function(nxt)
		local t = 0
		local b = behavior.seq({
			behavior.par({
				behavior.always(function(scn,dt)
					t += dt
				end),
				beeps,
				behavior_wait(#text/g_text_speed+0.1),
			}),
			behavior_wait(0.5),
			behavior.once(nxt),
			behavior.never
		})
		
		local function len()
			return t*g_text_speed
		end
		
		return text_reveal(text,7,len)
			:updatable(function(scn,dt)			
				b = b.update(scn,dt)
			end)
	end)
end

function dialogue_flow(text, voice)
	return say_text(text, voice)
		:wrap(function(c)
			return swordplay_ui(
				c,
				ui_empty,
				{col="dark",fac=0.5}
			)
		end)
end

text_scn = flow.seq({
	dialogue_flow("twas brillig and the slithy toves", behavior_beeps_lo),
	dialogue_flow("did gyre and gimble in the wabe", behavior_beeps_hi),
	dialogue_flow("all mimsy were the borogroves", behavior_beeps_lo),
	dialogue_flow("and the mome raths outgrabe", behavior_beeps_hi),
}):wrap(ui_scn)

__gfx__
00000000000000007077770750555505077777700007000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000777777770077700000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000007000000750000005770000770007000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000070000007000000750000005770000770007000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000777777777000000750000005777777770007000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700070000007000000750000005577777750007000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000055555500007000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000007077770750555505000000000007000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
ffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ffffffffff77777777ffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
fffffffff707070070777fffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
fffffffff7077777770707ffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ffffffff707777777777007fffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
fffffff70777777777777707ffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ffffff7077777777777777707fffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
fffff700007777777000077707ffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ffff7077770700070777707707ffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
fff70777777077707777770707ffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
fff707700770777070077707707fffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
fff707077070777007707707707fffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
fff7077777707770777777077707ffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ffff707777077777077770777707ffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ffff700000770707700007077707ffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ffff707777777077777777077707ffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
fff70707770770770777770777707fff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
fff70770777007007777700777707fff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ff7077770777777777770707777707ff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ff7077770070707070007707777707ff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
fff70000770000000077ff70777707ff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ffff7777ff77777777fffff700007fff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ffffffffffffffffffffffff7777ffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ffffffffffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
010a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
001000001900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
