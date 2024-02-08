pico-8 cartridge // http://www.pico-8.com
version 41
__lua__
--battle anims
--by mabbees

function _init()
	-- global state
	t = 0
	scn = {}
	
	anim_flow.go(function(s)
		scn = s
	end,noop,noop)
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
-- * flow
-- * behavior
-- * drawing
-- * easing


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

function flow.defer(f)
	return flow.create(function(nxt, done)
		done(f())
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
	for draw in all(tbl) do
		draw()
	end
end)

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


-->8
-- game tools

function behavior_wait(t)
	return t <= 0
		and behavior.success()
		or  behavior.run(function(_,dt)
			return behavior_wait(t-dt)
		end)
end

function behavior_frames(f)
	return f <= 0
		and behavior.success()
		or  behavior.run(function()
			return behavior_frames(f-1)
		end)
end



function anim(draw,beh)
	return flow.once(function(nxt)
		local b = behavior.seq({
			beh,
			behavior.once(nxt),
			behavior.never,
		})
		return {
			update=function(a,dt)
				b = b.update(a,dt)
			end,
			draw=draw,
		}
	end)
end

function draw_spr(n,x,y,sw,sh,flp_x)
	return function()
		spr(n,x-4*sw,y-4*sh,sw,sh,flp_x)
	end
end

function anim_frame(draw,f)
	return anim(
		draw,
		behavior_frames(f)
	)
end
-->8
-- battle anims

function anim_slash_1(flp_x)
	local dx = flp_x and -1 or 1
	return flow.seq({
		-- slash
		anim_frame(
			draw_spr(64,0,0,4,4,flp_x),
			2
		),
		-- slash + sparks
		anim_frame(
			draw_seq({
				draw_spr(64,0,0,4,4,flp_x),
				draw_spr(132,12*dx,0,4,4,flp_x),
			}),
			1
		),
		-- sparks
		anim_frame(
			draw_spr(132,12*dx,0,4,4,flp_x),
			1
		),
	})
end

function anim_slash_2(flp_x)
	local dx = flp_x and -1 or 1
	return flow.seq({
		-- slash
		anim_frame(
			draw_spr(128,0,0,4,4,flp_x),
			2
		),
		-- slash + sparks
		anim_frame(
			draw_seq({
				draw_spr(128,0,0,4,4,flp_x),
				draw_spr(132,12*dx,0,4,4,flp_x),
			}),
			1
		),
		-- sparks
		anim_frame(
			draw_spr(132,12*dx,0,4,4,flp_x),
			1
		),
	})
end


anim_flow = flow.loop(
	flow.seq({
		anim_slash_1(false),
		anim_frame(noop,30),
		
		anim_slash_2(true),
		anim_frame(noop,30),
		
		anim_slash_1(true),
		anim_frame(noop,30),
		
		anim_slash_2(false),
		anim_frame(noop,30),
	}):wrap(function(scn)
		return {
			update = scn.update,
			draw = draw_with_offset(64,64)(scn.draw)
		}
	end)
)
__gfx__
00000000000007707700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000777777770000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000070777777777000000000000000000000000000000007707700000000000000000000000000000000000000700000000000000000000000000000000
00000007770777770700700000000000000000000000000000777777770000000000000000000000000000000000777700000000000000000000000000000000
00000077770777077770700000000000000000000000000070777777777000000000000000000000000000007777777000000000000000000000000000000000
00000077700770777770700000000000000000000000007770777770700700000000000000000000000077777777000000000000000000000000000000000000
00000007000077777770000000000000000000000000077770777077770700000000000000000000777777770000000000000000000000000000000000000000
00000000000007777770000000000000000000000000077700770777770700000007770000007777777700000000000000000000000000000000000000000000
00000000000007777700000000000070000000000000007000077777770000000000777077777777000000000000000000000000000000000000000000000000
00000007777000777000000000000770000000000000000000007777770000000770777077770000000000000000000000000000000000000000000000000000
00000777770700000000000000007700000000000000000000007777700077770770777000000000000000000000000000000000000000000000000000000000
00077777070770000000000000077000000000000777777777000777077777770007777000000000000000000000000000000000000000000000000000000000
00777777077077007000000000770000000000777777777770700000077777700777770000000000000000000000000000000000000000000000000000000000
07777770777077777000000007700000000070077777777770770000077770000000000000000000000000000000000000000000000000000000000000000000
07777700777707707700000077000000000777007770770077077007077000000000000000000000000000000000000000000000000000000000000000000000
77770007777007707707770770000000000777000000007777707077070000000000000000000000000000000000000000000000000000000000000000000000
77700077777077700700770700000000000000000000007777077077770000000000000000000000000000000000000000000000000000000000000000000000
77700077770077707770077070000000000000000000777770777007700000000000000000000000000000000000000000000000000000000000000000000000
77000077770077007700707770000000000000000077777770777077700000000000000000000000000000000000000000000000000000000000000000000000
00770777770777007707770770000000000000000777777700770070000000000000000000000000000000000000000000000000000000000000000000000000
07700777700000070707700700000000000000000777777707770070000000000000000000000000000000000000000000000000000000000000000000000000
00007777007777007007707000000000000000000077700000000700000000000000000000000000000000000000000000000000000000000000000000000000
00000070077777707000070000000000000000000000007777770070000000000000000000000000000000000000000000000000000000000000000000000000
00000000777007770000000000000000000000007707777777777070000000000000000000000000000000000000000000000000000000000000000000000000
00000777770077777000000000000000000000077777777770077707700000000000000000000000000000000000000000000000000000000000000000000000
00007777700077777700000000000000000000777777700000000777770000000000000000000000000000000000000000000000000000000000000000000000
00077777000007777700000000000000000077777770000000000077770000000000000000000000000000000000000000000000000000000000000000000000
00077777000077777000000000000000007777777000000000000077770000000000000000000000000000000000000000000000000000000000000000000000
00777770000077770000000000000000077777700000000000000077770000000000000000000000000000000000000000000000000000000000000000000000
00077700000777700000000000000000007770000000000000000777700000000000000000000000000000000000000000000000000000000000000000000000
00777770000777777000000000000000077777000000000000000777777000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000777777770000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000007777770000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000777700000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000007777000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000077700000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000007770000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000777000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000077777000000000000000000000077700000000000000000000000007777777777000000000000000000000000000000000000000000
00000000000000000000000777770000000000000000000007770000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000007777000000000000000000000777000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000777700000000000000000000077700000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000077770000000000000000000007770000000000000000777777777000000000000000000000000000000000000000000000000
00000000000000000000000000077770000000000000000000007777000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000777770000000000000000000000777000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000007777700000000000000000000000777700000007777777777777777777777777777777700000000000000000000000000000000
00000000000000000000000077777000000000000000000000000077700000007777777777777777777777777777777700000000000000000000000770000000
00000000000000000000007777770000000000000000000000000077770000000000000000000000000000000000000000000000000000000000000777700000
00000000000000000007777777700000000000000000000000000007770000000000000000000000000000000000000000000000000000000000007777000000
00000000000000007777777770000000000000000000000000000007770000000000000000000007777777777000000000000000000000000000007770000000
00000000000077777777700000000000000000000000000000000007777000000000000000000000000000000000000000000000000000000000077700000000
00000000777777777000000000000000000000000000000000000007777000000000000000000000000000000000000000000000000000000000777000000000
00777777777770000000000000000000000000000000000000000000777000000000000000000000000000000000000000000000000000000007770000000000
00000000000000000000000000000000000000000000000000000000777700000000000000000000000000000000000000000000000000000077700000000000
00000000000000000000000000000000000000000000000000000000777700000000000000000000000000000000000000000000000000000777000000000000
00000000000000000000000000000000000000000000000000000000777700000000000000007777777000000000000000000000000000077700000000000000
00000000000000000000000000000000000000000000000000000000777700000000000000000000000000000000000000000000000007777000000000000000
00000000000000000000000000000000000000000000000000000000777700000000000000000000000000000000000000000000007777700000000000000000
00000000000000000000000000000000000000000000000000000000777770000000000000000000000000000000000007777777777700000000000000000000
00000000000000000000000000000000000000000000000000000000777770000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000077770000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000077777000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000077777000000000000000000000000000000000000000000000000000000000007777700000000000000000000000000000000000000000000000000
00007777777777777000000000000000000000007000000000000000000000000000777777777777700000000000000000000000000000000000000000000000
00000000000077777770000000000000000000000000000000000000000000000000000000007777777000000000000000000000000000000000000000000000
00000000000000777777000000000000000000000000000000000000000000000000000000000077777700000007000000000000000000000000000000000000
00000000000000077777770000000000000000000000000000000000000000000000000000000007777777000077000000000000000000000000000000000000
00000000000000077777777700000000000000000000000000000000000000000000000000000007777777770770007000000000000000000000000000000000
00000000000000007777777770000000000000000000000770000000000000000000000000000000777777777770777000000000000000000000000000000000
00000000000000007777777777000000000000000000000700000000000000000000000000000000777777777707770000000000000000000000000000000000
00000000000000007777777777000000000000007000007700000000000000000000000000000000777777777777700000000000000000000000000000000000
00000000000000007777777777700000000000007700007000000007770000000000000000000000777777777777000000000000000000000000000000000000
00000000000000007777777777700000000000000700000000000077770000000000000000000000777777777777700000000000000000000000000000000000
00000000000000007777777777700000000000000000000000007700000000000000000000000000777777777777777700000000000000000000000000000000
00000000000000077777777777700000000000000000000000700000000000000000000000000007777777777777777000000000000000000000000000000000
00000000000000077777777777700000000000000000000000000000000000000000000000000007777777777777000000000000000000000000000000000000
00000000000000777777777777700000000000000000000000000000000000000000000000000077777777777777700000000000000000000000000000000000
00000000000000777777777777000000000000000000000000000000000000000000000000000077777777777777770000000000000000000000000000000000
00000000000007777777777770000000000000000007000000000000000000000000000000000777777777777770007000000000000000000000000000000000
00000000000077777777777700000000000000000007000700000000000000000000000000007777777777770077000000000000000000000000000000000000
00000000000777777777777000000000000000000077000070000000000000000000000000077777777777700007000000000000000000000000000000000000
00000000007777777777770000000000000000000077000007700000000000000000000000777777777777000000000000000000000000000000000000000000
00000000077777777777700000000000000000000777000000770000000000000000000007777777777770000000000000000000000000000000000000000000
00000007777777777770000000000000000000000777000000077000000000000000000777777777777000000000000000000000000000000000000000000000
00000777777777777000000000000000000000000000000000007700000000000000077777777777700000000000000000000000000000000000000000000000
00077777777777700000000000000000000000000000000000007770000000000007777777777770000000000000000000000000000000000000000000000000
07777777777770000000000000000000000000000000000000000777000000000777777777777000000000000000000000000000000000000000000000000000
77777777770000000000000000000000000000000000000000000077700000007777777777000000000000000000000000000000000000000000000000000000
77777770000000000000000000000000000000000000000000000007770000007777777000000000000000000000000000000000000000000000000000000000
77700000000000000000000000000000000000000000000000000007000000007770000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000700000000000000000000000000000000000000000000000000000000000000000000000000000000000
