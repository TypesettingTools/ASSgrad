--[=[ You are not legally allowed to look at this code because it is proprietary. STOP BREAKING THE LAW ]=]--

script_name = "ASSGRAD"
script_description = "THE ULTIMATE METHOD OF SCRIPT BLOATING HAS FINALLY ARRIVED"
script_author = "torque"
script_version = "9001"
require "karaskel"
require "re"

function dcos(a) return math.cos(math.rad(a)) end
function dacos(a) return math.deg(math.acos(a)) end
function dsin(a) return math.sin(math.rad(a)) end
function dasin(a) return math.deg(math.asin(a)) end
function dtan(a) return math.tan(math.rad(a)) end
function datan(x,y) return math.deg(math.atan2(x,y)) end

fix = {}

fix.ali = {
  function(x,y,w,h,a) local r = w/2 return x+r*dcos(a)-h/2*dsin(a), y-r*dsin(a)-h/2*dcos(a) end; -- 1
  function(x,y,w,h,a) local r = h/2 return x-r*dsin(a), y-r*dcos(a) end;                         -- 2
  function(x,y,w,h,a) local r = w/2 return x-r*dcos(a)-h/2*dsin(a), y+r*dsin(a)-h/2*dcos(a) end; -- 3
  function(x,y,w,h,a) local r = w/2 return x+r*dcos(a), y-r*dsin(a) end;                         -- 4
  function(x,y,w,h,a) return x, y end;                                                           -- 5
  function(x,y,w,h,a) local r = w/2 return x-r*dcos(a), y+r*dsin(a) end;                         -- 6
  function(x,y,w,h,a) local r = w/2 return x+r*dcos(a)+h/2*dsin(a), y-r*dsin(a)+h/2*dcos(a) end; -- 7
  function(x,y,w,h,a) local r = h/2 return x+r*dsin(a), y+r*dcos(a) end;                         -- 8
  function(x,y,w,h,a) local r = w/2 return x-r*dcos(a)+h/2*dsin(a), y+r*dsin(a)+h/2*dcos(a) end; -- 9
}

fix.xpos = {
  function(sx,l,r) return sx-r end;
  function(sx,l,r) return l    end;
  function(sx,l,r) return sx/2 end;
}

fix.ypos = {
  function(sy,v) return sy-v end;
  function(sy,v) return sy/2 end;
  function(sy,v) return v    end;
}

ui = {
  {class = "color";
    x = 0; y = 0; height = 1; width = 1;
  value = "#FF00FF"; name = "dicks";},
}

header = {
  ['xscl'] = "scale_x",
  ['yscl'] = "scale_y",
  ['ali']  = "align",
  ['zrot'] = "angle",
  ['bord'] = "outline",
  ['shad'] = "shadow",
  ['_v']   = "margin_t",
  ['_l']   = "margin_l",
  ['_r']   = "margin_r",
  ['fs']   = "fontsize",
  ['fn']   = "fontname",
}

patterns = {
  ['xscl']    = "\\fscx([%d%.]+)",
  ['yscl']    = "\\fscy([%d%.]+)",
  ['ali']     = "\\an([1-9])",
  ['zrot']    = "\\frz?([%-%d%.]+)",
  ['bord']    = "\\bord([%d%.]+)",
  ['xbord']   = "\\xbord([%d%.]+)",
  ['ybord']   = "\\ybord([%d%.]+)",
  ['shad']    = "\\shad([%-%d%.])",
  ['xshad']   = "\\xshad([%-%d%.]+)",
  ['yshad']   = "\\yshad([%-%d%.]+)",
  ['fs']      = "\\fs([%d%.]+)",  
}

function GiantMessyFunction(sub,sel)
  local meta, styles = karaskel.collect_head(sub,false)
  for i,v in ipairs(sel) do
    local line = sub[v]
    karaskel.preproc_line(sub, meta, styles, line)
    GetInfo(sub, line, styles, v)
    line.styleref.scale_y = 100 -- line.yscl
    line.styleref.scale_x = 100 -- line.xscl
    line.styleref.fontname = line.fn
    line.styleref.fontsize = line.fs
    line.width, line.height, line.descent, line.extlead = aegisub.text_extents(line.styleref,line.text_stripped)
    local strs = re.match(line.text,"{.*?\\\\p1.*?}(.+?)({.*?\\\\p0.*?}|$)")
    if strs then
      line.width, line.height, line.descent, line.extlead = GetSizeOfVectorObject(strs[2].str)
    end
    line.height = line.height - line.descent/2
    line.num = v
    if line.margin_v ~= 0 then line._v = line.margin_v end
    if line.margin_l ~= 0 then line._l = line.margin_l end
    if line.margin_r ~= 0 then line._r = line.margin_r end
    if not line.xpos then
      line.xpos = fix.xpos[line.ali%3+1](meta.res_x,line._l,line._r)
      line.ypos = fix.ypos[math.ceil(line.ali/3)](meta.res_y,line._v)
    end
    if not line.xorg then
      line.xorg = line.xpos
      line.yorg = line.ypos
    end
    local xd = line.xpos - line.xorg
    local yd = line.ypos - line.yorg
    local r = math.sqrt(xd^2+yd^2)
    local alpha = datan(yd,xd)
    line.xpos = line.xorg + r*dcos(alpha-line.zrot)
    line.ypos = line.yorg + r*dsin(alpha-line.zrot) --]]
    line.xpos,line.ypos = fix.ali[line.ali](line.xpos,line.ypos,line.width*line.xscl/100,line.height*line.yscl/100,line.zrot)
    if line.ali ~= 5 then
      if line.text:match("\\an[1-9]") then
        line.text = line.text:gsub("\\an[1-9]","\\an5")
      else
        line.text = "{\\an5}"..line.text
      end
    end
    line.layer = 0
    line.text = line.text:gsub("\\pos%([%-%d%.]+,[%-%d%.]+%)","")
    line.text = line.text:gsub("\\org%([%-%d%.]+,[%-%d%.]+%)","")
    line.text = line.text:gsub("\\1?c&H%x+&","")
    local i = 0
    local it = 0
    local OriginalText = line.text
    line.height = line.height*line.yscl/100
    line.width = line.width*line.xscl/100
    local BandOverlap = 3 -- overlapping the bands by 3 or so pixels is important for diagonal gradients
    local BandSize = 1
    local theta = 0
    local Length = math.ceil(line.height/BandSize)
    local ColorTable = { -- put data in table as rgb for no good reason
      {0,0,255,};
      {255,0,0,};
      {0,255,0,};
    }
    local ind = 1
    local PerColorLength = math.ceil(Length/(#ColorTable-1)) -- transition lengths
    -- define vectors
    local origin = {line.xpos, line.ypos, 0}
    local position = {-line.width*0.5,-line.height*0.5+line.descent*0.5,0} -- assuming rectangular by default. Can be easily obtained for a predefined four-corner clip
    local left = {0,line.height,0}
    local top = {line.width,0,0}
    -- rectangular means right = left and bottom = top
    position = vec.s2c(vec.saddaz(vec.c2s(position),math.rad(line.zrot)))
    local topleft = vec.add(origin,position) -- position of top left
    local topright = vec.sadd(vec.c2s(topleft),vec.saddaz(vec.c2s(top),math.rad(line.zrot)))
    --local bottomleft = vec.sadd(vec.c2s(topleft),vec.saddaz(vec.c2s(left),math.rad(line.zrot)))
    --local bottomright = vec.sadd(vec.c2s(topright),vec.saddaz(vec.c2s(left),math.rad(line.zrot)))
    local vertband = vec.saddaz(vec.c2s({0,BandSize+BandOverlap,0}),math.rad(line.zrot))
    for y = 0,math.floor(line.height/BandSize)-1 do
      local tl = vec.sadd(vec.c2s(topleft),vec.sadds(vertband,y*BandSize-(BandSize+BandOverlap)))
      local tr = vec.sadd(vec.c2s(tl),vec.saddaz(vec.c2s(top),math.rad(line.zrot+theta)))
      local br = vec.sadd(vec.c2s(tr),vertband)
      local bl = vec.sadd(vec.c2s(tl),vertband)
      local cur = math.floor(i/PerColorLength)+1 -- because math.ceil(0) == 0
      local red = round(ColorTable[cur][1]+(ColorTable[cur+1][1]-ColorTable[cur][1])*(i%PerColorLength+1)/PerColorLength) -- forward difference
      local gre = round(ColorTable[cur][2]+(ColorTable[cur+1][2]-ColorTable[cur][2])*(i%PerColorLength+1)/PerColorLength)
      local blu = round(ColorTable[cur][3]+(ColorTable[cur+1][3]-ColorTable[cur][3])*(i%PerColorLength+1)/PerColorLength)
      local clip = string.format("m %d %d l %d %d %d %d %d %d",tl[1],tl[2],tr[1],tr[2],br[1],br[2],bl[1],bl[2])
      local color = string.format("%02X%02X%02X",blu,gre,red) -- \c tags are in BGR order
      line.text = string.format("{\\c&H%s&\\clip(%s)\\pos(%.2f,%.2f)}",color,clip,line.xpos,line.ypos)..line.text
      i = i + 1
      sub.insert(v+i,line)
      line.text = OriginalText
    end
  end
end

function GetSizeOfVectorObject(vect)
  local ix, iy = vect:match("^m ([%-%d]+) ([%-%d]+)")
  local xmin, xmax, ymin, ymax
  local function normalize(a,b)
    if not xmax then xmax = tonumber(a)-ix elseif tonumber(a)-ix > xmax then xmax = tonumber(a)-ix end
    if not xmin then xmin = tonumber(a)-ix elseif tonumber(a)-ix < xmin then xmin = tonumber(a)-ix end
    if not ymax then ymax = tonumber(b)-iy elseif tonumber(b)-iy > ymax then ymax = tonumber(b)-iy end
    if not ymin then ymin = tonumber(b)-iy elseif tonumber(b)-iy < ymin then ymin = tonumber(b)-iy end
  end
  vect = vect:gsub("([%-%d]+) ([%-%d]+)",normalize)
  return xmax-xmin+2,ymax-ymin+2,0,0 -- pad out by 2px
end

function GetInfo(sub, line, styles, num) -- because camelcase
  for k, v in pairs(header) do
    line[k] = styles[line.style][v]
    aegisub.log(5,"Line %d: %s set to %s (from header)\n", num, v, tostring(line[k]))
  end
  if line.bord then line.xbord = tonumber(line.bord); line.ybord = tonumber(line.bord); end
  if line.shad then line.xshad = tonumber(line.shad); line.yshad = tonumber(line.shad); end
  if line.text:match("\\pos%([%-%d%.]+,[%-%d%.]+%)") then
    line.xpos, line.ypos = line.text:match("\\pos%(([%-%d%.]+),([%-%d%.]+)%)")
    line.xorg, line.yorg = line.xpos, line.ypos
  end
  if line.text:match("\\org%(([%-%d%.]+),([%-%d%.]+)%)") then
    line.xorg, line.yorg = line.text:match("\\org%(([%-%d%.]+),([%-%d%.]+)%)")
  end
  line.trans = {}
  local a = line.text:match("%{(.-)}")
  if a then
    --aegisub.log(5,"Found a comment/override block in line %d: %s\n",num,a)
    for k, v in pairs(patterns) do
      local _ = a:match(v)
      if _ then 
        line[k] = tonumber(_)
        aegisub.log(5,"Line %d: %s set to %s\n",num,k,tostring(_))
      end
    end
    if a:match("\\fn([^\\}]+)") then line.fn = a:match("\\fn([^\\}]+)") end
    line.clips, line.clip = a:match("\\(i?clip%()([%-%d]+,[%-%d]+,[%-%d]+,[%-%d]+)%)") -- hum
    if not line.clip then
      line.clips, line.clip = a:match("\\(i?clip%([%d]*,?)(.-)%)")
    end
    --if line.clip then aegisub.log(5,"Clip: (%s%s)\n",line.clips,line.clip) end
    for b in line.text:gmatch("%{(.-)%}") do
      for c in b:gmatch("\\t(%b())") do -- this will return an empty string for t_exp if no exponential factor is specified
        t_start,t_end,t_exp,t_eff = c:sub(2,-2):match("([%-%d]+),([%-%d]+),([%d%.]*),?(.+)")
        if t_exp == "" then t_exp = 1 end -- set it to 1 because stuff and things
        table.insert(line.trans,{tonumber(t_start),tonumber(t_end),tonumber(t_exp),t_eff})
        --aegisub.log(5,"Line %d: \\t(%g,%g,%g,%s) found\n",num,t_start,t_end,t_exp,t_eff)
      end
    end
    -- have to run it again because of :reasons: related to bad programming
    if line.bord then line.xbord = tonumber(line.bord); line.ybord = tonumber(line.bord); end
    if line.shad then line.xshad = tonumber(line.shad); line.yshad = tonumber(line.shad); end
  else
    --aegisub.log(5,"No comment/override block found in line %d\n",num)
  end
end

vec = {} -- borrow a bunch of vector functions from jfs's raytracer.lua

vec.null = {0,0,0}

function vec.n(n)
  return {n,n,n}
end

function vec.p2d(v) -- return x/y
  return v[1], v[2]
end

function vec.p(v)
  return v[1],v[2],v[3]
end

function vec.c2s(v) -- cartesian -> spherical
  local r = {}
  r[1] = vec.len(v) -- radial distance
  r[2] = math.acos(v[3]/r[1]) -- elevation - corresponds to z (angle measured from the positive z-axis)
  r[3] = math.atan2(v[2],v[1]) -- azimuth  - corresponds to x and y
  return r
end

function vec.s2c(v) -- spherical -> cartesian
  local r = {}
  r[1] = v[1]*math.cos(v[3])*math.sin(v[2])
  r[2] = v[1]*math.sin(v[3])*math.sin(v[2])
  r[3] = v[1]*math.cos(v[2])
  return r
end

function vec.sadds(v, s) -- add a scalar length
  return {v[1]+s, v[2], v[3]}
end

function vec.saddaz(v, s) -- add to the azimuth
  return {v[1], v[2], v[3]-s}
end

function vec.sadd(v1, v2) -- spherical addition (by converting back into cartesian since I'm cool like that)
  return vec.add(vec.s2c(v1),vec.s2c(v2)) -- return cartesian
end

function vec.add(v1, v2) -- v2 plus v2
	local r = {}
	r[1] = v1[1] + v2[1]
	r[2] = v1[2] + v2[2]
	r[3] = v1[3] + v2[3]
	return r
end

function vec.sub(v1, v2) -- v1 minus v2
	local r = {}
	r[1] = v1[1] - v2[1]
	r[2] = v1[2] - v2[2]
	r[3] = v1[3] - v2[3]
	return r
end

function vec.scale(v, s) -- vector times a scalar
	local r = {}
	r[1] = v[1] * s
	r[2] = v[2] * s
	r[3] = v[3] * s
	return r
end

function vec.len(v) -- vector length
	return math.sqrt(v[1]*v[1] + v[2]*v[2] + v[3]*v[3])
end

function vec.norm(v) -- unit vector in the direction of vector v
	local r, il = {}, 1/vec.len(v)
	r[1] = v[1]*il
	r[2] = v[2]*il
	r[3] = v[3]*il
	return r
end

function vec.snorm(v) -- unit vector in the direction of vector v
	return {1, v[2], v[3]}
end

function vec.dot(v1, v2) -- dot product of vectors
	return v1[1]*v2[1] + v1[2]*v2[2] + v1[3]*v2[3]
end

function vec.cross(v1, v2) -- v1 cross v2
	local r = {}
	r[1] = v1[2]*v2[3] - v1[3]*v2[2]
	r[2] = v1[1]*v2[3] - v1[3]*v2[1]
	r[3] = v1[1]*v2[2] - v1[2]*v2[1]
	return r
end

function vec.normal(p1, p2, p3)
	return vec.cross(vec.sub(p2, p1), vec.sub(p3, p1))
end


function round(num, idp) -- borrowed from the lua-users wiki
  local mult = 10^(idp or 0)
  return math.floor(num * mult + 0.5) / mult
end

aegisub.register_macro("ULTIMATE SUPERGRADIENT","GRAD YOUR ASS LIKE NEVER BEFORE", GiantMessyFunction)