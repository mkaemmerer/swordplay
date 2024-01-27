pico-8 cartridge // http://www.pico-8.com
version 41
__lua__
--swordplay
--by mabbees

-- global constants
debug = false

function _init()
	-- global state
	t = 0
	scn = {}

	-- flow
	local game_flow = flow.seq({
		presented_by_scn,
		flow.loop(
			flow.seq({
				title_scn,
				game_scn,
			})
		),
	})
	
	transition_flow(game_flow).go(
		function(s)
 		scn = s
		end,
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
-- * 2d vector
-- * flow
-- * behavior
-- * drawing
-- * text utils
-- * ui components
-- * follow


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
			f(unpack(args))
		end
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
	}
}

rdr_local=function(f,r)
	return rdr(function(e)
		return r.run(f(e))
	end)
end


-- 2d vector ------------------
function v_len(v)
	return sqrt(v:dot(v))
end

function v_unit(v)
	return v * (1/v:len())
end

v2_meta={
	__mul=function(a,b)
		if type(b) == "number" then
			return a:scale(b)
		end
		if type(a) == "number" then
			return b:scale(a)
		end
	end,
	__add=function(u,v)
		return u:add(v)
	end,
	__sub=function(u,v)
		return u:sub(v)
	end,
	__index={
		len=v_len,
		unit=v_unit,
		add=function(u,v)
			return v2(u.x+v.x, u.y+v.y)
		end,
		sub=function(u,v)
			return v2(u.x-v.x, u.y-v.y)
		end,
		dot=function(u,v)
			return u.x*v.x + u.y*v.y
		end,
		scale=function(v,s)
			return v2(v.x*s, v.y*s)
		end,
		unpack=function(v)
			return v.x,v.y
		end
	},
}

function v2(x,y)
	return setmetatable(
		{x=x,y=y},
		v2_meta
	)
end

v_zero = v2(0,0)
v_x = v2(1,0)
v_y = v2(0,1)


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

flow.seq = monad_seq(flow.of(nil))

flow.loop = monad_loop


-- behavior -------------------
-- behavior (full)

-- a full-featured version of
-- behavior. includes error
-- handling

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
		handle_err=id,
	}
}
function behavior.success(value)
 return setmetatable({
 	value=value,
 }, b_success_meta)
end

b_error_meta={
	__index={
		type="error",
		map = id,
		flatmap = id,
		handle_err=function(b,f)
			return f(b.error)
		end,
	}
}
function behavior.error(error)
 return setmetatable({
 	error=error,
 }, b_error_meta)
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
		handle_err=function(b,f)
			return behavior.run(function(actor,dt)
				return b.update(actor,dt):handle_err(f)
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
	nxt.err  = behavior.error
	nxt.cont = const(b)
	
 return b
end
 
function behavior.guard(f)
	return behavior.run(function(actor,dt)
		return f(actor,dt)
			and behavior.success(nil)
			or  behavior.error(nil)
	end)
end

function behavior.rescue(b, value)
	return b:handle_err(
		const(behavior.success(value))
	)
end

function behavior.retry(b)
	return b:handle_err(function()
		return behavior.retry(b)
	end)
end

behavior.seq = monad_seq(behavior.success(nil))

function behavior.par(bs)
	for b in all(bs) do
		if b.type == "error" then return b end
	end
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

behavior.sel = reduce(
	function(a,b)
		return a:handle_err(const(b))
	end,
	behavior.error(nil)
)

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

behavior.never = behavior.always(function() end)


-- drawing --------------------
draw_spr = suspend(spr)
draw_map = suspend(map)
draw_txt = suspend(print)
draw_rect = suspend(rect)

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
		palt(15,true)
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


-- text utils -----------------
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
		offset   = r:flatmap(function(c) return c.offset end),
		min_dims = r:flatmap(function(c) return c.min_dims end),
		dims     = r:flatmap(function(c) return c.dims end),
	})
end

-- prims
ui_empty = ui.create({
	offset   = no_offset,
	min_dims = zeros,
	dims     = full_dims,
	draw     = noop,
})

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

function ui_spr(n,sw,sh)
	local w=8*sw
	local h=8*sh
	return ui.from_draw(function(x,y,w,h)
		spr(n,x,y,sw,sh)
	end)
	:size(w,h)
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
	end)
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
		end),
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
	}
}

--follow strategies

function follow_fixed(p,t)
	return p
end

function follow_track(p,t)
	return t
end

function follow_smooth(fac)
	return function(p,t)
		return lerp(p,t,fac)
	end
end

function follow_stationary(l)
	return const(l)
end

function follow_offset(off)
	return function(p,t)
		return t+off
	end
end

function follow_steady(dist)
	return function(p,t)
		local d = t-p
		local len = max(0, d:len() - dist)
		local clipped = len * d:unit()
		return p + clipped
	end
end

function follow_steady_rect(w,h)
	return function(p,t)
		local d = t-p
		
		local x_len = max(0, abs(d.x) - w)
		local y_len = max(0, abs(d.y) - h)
		local clipped = v2(
			x_len * sgn(d.x),
			y_len * sgn(d.y)
		)
		
		return p + clipped
	end
end

function follow_bounds(x_min, y_min, x_max, y_max)
	return function(p,t)
		return v2(
			mid(x_min, t.x, x_max),
			mid(y_min, t.y, y_max)
		)
	end
end

function follow_quantize(w,h)
	return function(p,t)
		return v2(w*(t.x\w), h*(t.y\h))
	end
end

function follow_dynamic(cam)
	return function(p,t)
		return cam(p,t)(p,t)
	end
end

function follow_compose(f,g)
	return function(p,t)
		return g(p,f(p,t))
	end
end

follow_chain = reduce(
	follow_compose,
	follow_track
)
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


-->8
-- scenes

game_scn = flow.once(function(nxt)	
	return {
		update=function(scn,dt)
			
		end,
		draw=function(scn)
			map()
		end,
	}
end)

presented_by_scn = flow.once(function(nxt)
	local ui = ui_layout({
			ui_group({
				ui_spr(64,4,4)
				:effect(draw_with_transparency(15))
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
		ui_text("swordplay",7)
			:align("center","center")
			:effect(draw_with_outline_9(5)),
		
		ui_text("press ❎ to start", 5)
			:inset(4)
			:align("center","bottom")
			:translate(0,-12)
			:effect(function(draw)
				local blink = (t*3) % 1 > 0.5
				local eff = (is_ready and blink)
					and draw_with_outline_9(7)
					or  draw_with_outline_9(6)
				return eff(draw)
			end),
	})
	:size("fill","fill")
	
	return {
		behavior=behavior.seq({
			behavior_wait(1),
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
	return flow.create(function(nxt,done)
		f.go(function(cur)
			nxt(transition(cur, prev))
			prev = cur
		end, done)
	end)
end

function transition(cur,prv)
	local t = 0
	local dur = 1
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
			elseif t < dur and prv then
				-- draw "in" transition
				local fac = 2*(t-0.5*dur)/dur
				cur:draw()
			else
				cur:draw()
			end			
		end,
	}
end
__gfx__
00000000d11111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
__sfx__
000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c10a00001c53021530265302653500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
c10a000026530215301c5301c53500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
