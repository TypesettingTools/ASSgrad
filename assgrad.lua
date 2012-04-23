--[=[ You are not legally allowed to look at this code because it is proprietary. STOP BREAKING THE LAW ]=]--

script_name = "ASSGRAD"
script_description = "THE ULTIMATE METHOD OF SCRIPT BLOATING HAS FINALLY ARRIVED"
script_author = "torque"
script_version = "9001"
require "karaskel"
require "re"

function cosd(a) return math.cos(math.rad(a)) end
function acosd(a) return math.deg(math.acos(a)) end
function sind(a) return math.sin(math.rad(a)) end
function asind(a) return math.deg(math.asin(a)) end
function tand(a) return math.tan(math.rad(a)) end
function atan2d(x,y) return math.deg(math.atan2(x,y)) end

fix = {}

fix.ali = {
  function(x,y,w,h,a) local r = w/2 return x+r*cosd(a)-h/2*sind(a), y-r*sind(a)-h/2*cosd(a) end; -- 1
  function(x,y,w,h,a) local r = h/2 return x-r*sind(a), y-r*cosd(a) end;                         -- 2
  function(x,y,w,h,a) local r = w/2 return x-r*cosd(a)-h/2*sind(a), y+r*sind(a)-h/2*cosd(a) end; -- 3
  function(x,y,w,h,a) local r = w/2 return x+r*cosd(a), y-r*sind(a) end;                         -- 4
  function(x,y,w,h,a) return x, y end;                                                           -- 5
  function(x,y,w,h,a) local r = w/2 return x-r*cosd(a), y+r*sind(a) end;                         -- 6
  function(x,y,w,h,a) local r = w/2 return x+r*cosd(a)+h/2*sind(a), y-r*sind(a)+h/2*cosd(a) end; -- 7
  function(x,y,w,h,a) local r = h/2 return x+r*sind(a), y+r*cosd(a) end;                         -- 8
  function(x,y,w,h,a) local r = w/2 return x-r*cosd(a)+h/2*sind(a), y+r*sind(a)+h/2*cosd(a) end; -- 9
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

vobj = re.compile("{.*?\\\\p1.*?}(.+?)({.*?\\\\p0.*?}|$)")
comma = re.compile(',')
colon = re.compile(':')
semic = re.compile(';')
lf = re.compile('\\\\N') -- is double escaping still required?
-- <Ag>(1c:2c:3c:4c;1a:2a:3a:4a;1o,2o,3o)
-- options to expose: band overlap, band size, theta (unimplemented)
function GatherLines(sub,sel)
  local gradlines = {}
  local len = #sub
  for x = len,1,-1 do
    local line = sub[x] -- loop backwards, so subs are added to the select table from last to first.
    if line.class == "dialogue" then
      if line.effect:match("<Ag>%(.-%)") then -- use lua's own pattern matching as much as possible because it's very fast.
        aegisub.log(0,'x: %d\nline: %s\n\n',x,table.tostring(line))
        table.insert(gradlines,{x,line.effect:match("<Ag>%((.-)%)")})
      end
    end
  end
  Crunch(sub,gradlines)
end

function Crunch(sub,sel)
  local color = {}
  local alpha = {}
  local options = {}
  for i,v in ipairs(sel) do
    aegisub.log(0,'v: %s\n',table.tostring(v))
    local line = sub[v[1]]
    aegisub.log(0,table.tostring(line).."\n")
    local all = semic:split(v[2]) -- {color, alpha, options}
    local colour = colon:split(all[1])
    for ii,x in ipairs(colour) do
      color[ii] = ColorParse(comma:split(x))
    end
    CleanTable(color)
    colour = nil
    if all[2] then
      local alepha = colon:split(all[2])
      for ii,x in ipairs(alepha) do
        alpha[ii] = comma:split(x)
      end
      CleanTable(alpha)
      alepha = nil
    end
    if all[3] then
      options = comma:split(all[3])
      CleanTable(options)
    end
    line.num = v[1]
    GiantMessyFunction(sub,line,color,alpha,options)
  end
end

function CleanTable(table)
  for i,v in ipairs(table) do -- should be sequential integer indices
    if v == "" or v == {} then table[k] = nil end -- strip out blank entries - we can't use "skip empty" when splitting because it's order dependent.
  end
end

function ColorParse(ColorTab) --BGR, RGB
  local ReturnTab = {}
  for i,v in ipairs(ColorTab) do
    if v:match("^&H") then
      local b,g,r = v:match("^&H(%x%x)(%x%x)(%x%x)&")
      aegisub.log(0,"%s,%s,%s\n",r,g,b)
      table.insert(ReturnTab,{tonumber(r,16),tonumber(g,16),tonumber(b,16)})
    else
      local r,g,b = v:match("^#?(%x%x)(%x%x)(%x%x)")
      table.insert(ReturnTab,{tonumber(r,16),tonumber(g,16),tonumber(b,16)})
    end
  end
  return ReturnTab
end

function MultilineExtents(line)
  local SplitTable = lf:split(line.text_stripped) -- I don't remember if text_stripped gets rid of linebreaks
  local height, width, desc, ext = 0,0,0,0
  for i,v in ipairs(SplitTable) do
    local h,w,d,e = NewTextExtents(line.styleref,v)
    height = height + h + d -- each line includes descent
    if w > width then width = w end
    desc,ext = d,e
  end
  return height, width, desc, ext
end

function GiantMessyFunction(sub,line,ColorTable,AlphaTable,OptionsTable)
  local meta, styles = karaskel.collect_head(sub,false)
  karaskel.preproc_line(sub, meta, styles, line)
  GetInfo(sub, line, styles, line.num)
  local OptionsTable = OptionsTable or {}
  local l = OptionsTable[3] or 0
  local t = OptionsTable[4] or 0
  local r = OptionsTable[5] or 0
  local b = OptionsTable[6] or 0
  line.width, line.height, line.descent, line.extlead = MultilineExtents(line) -- handle linebreaks
  local strs = vobj:match(line.text)
  if strs then
    line.width, line.height, line.descent, line.extlead = GetSizeOfVectorObject(strs[2].str)
  end
  line.height = line.height - line.descent/2
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
  local alpha = atan2d(yd,xd)
  line.xpos = line.xorg + r*cosd(alpha-line.zrot)
  line.ypos = line.yorg + r*sind(alpha-line.zrot) --]]
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
  local i = 0
  local it = 0
  local OriginalText = line.text
  line.height = line.height*line.yscl/100
  line.width = line.width*line.xscl/100
  local BandSize = OptionsTable[1] or 4
  local BandOverlap = OptionsTable[2] or BandSize -- important for this to be some factor of BandSize, especially if alpha is involved
  local theta = 0 -- need to figure out some math first
  local Length = math.ceil(line.height/BandSize)
  local ColorTable = ColorTable or { -- put data in table as rgb for no good reason
    {240,240,240,}; -- nice defaults
    {237,142,183,};
    --{0,255,0,};
  }
  for k,v in pairs(ColorTable) do -- baleet relevant color tags
    line.text = line.text:gsub(string.format("\\\\[%d][cC]&[hH]%%x+&",k),"")
  end
  if ColorTable[1] then line.text:gsub("\\c&H%x+&","") end
  local PerColorLength = {}
  for k,v in pairs(ColorTable) do
    PerColorLength[k] = math.ceil(Length/(#v-1))
  end
  --local PerColorLength = math.ceil(Length/(#ColorTable-1)) -- transition lengths
  --[[ this is for edges. I plan to switch to a central difference rather than a forward difference for the loop some time eventually.
  ColorTable[0] = table.copy(ColorTable[1])
  ColorTable[#ColorTable+1] = table.copy(ColorTable[#ColorTable]) --]]
  local ind = 1
  -- define vectors
  local origin = {line.xpos, line.ypos, 0}
  local position = {-line.width*0.5-l,-line.height*0.5+line.descent*0.5-t,0} -- top left corner
  local left = {0,line.height+t+b,0}
  local top = {line.width+l+r,0,0}
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
    --local cur = math.ceil(i/PerColorLength)
    local color = ""
    for ColorNum,ColorSubTable in pairs(ColorTable) do
      local CurrPCL = PerColorLength[ColorNum]
      local cur = math.floor(i/CurrPCL)+1 -- because math.ceil(0) == 0
      local red = round(ColorSubTable[cur][1]+(ColorSubTable[cur+1][1]-ColorSubTable[cur][1])*(i%CurrPCL+1)/CurrPCL) -- forward difference
      local gre = round(ColorSubTable[cur][2]+(ColorSubTable[cur+1][2]-ColorSubTable[cur][2])*(i%CurrPCL+1)/CurrPCL)
      local blu = round(ColorSubTable[cur][3]+(ColorSubTable[cur+1][3]-ColorSubTable[cur][3])*(i%CurrPCL+1)/CurrPCL)
      color = color..string.format("\\%dc&H%02X%02X%02X&",ColorNum,blu,gre,red) -- \c tags are in BGR order
    end
    local clip = string.format("m %.0f %.0f l %.0f %.0f %.0f %.0f %.0f %.0f",tl[1],tl[2],tr[1],tr[2],br[1],br[2],bl[1],bl[2])
    line.text = '{'..color..string.format("\\clip(%s)\\pos(%.2f,%.2f)}",clip,line.xpos,line.ypos)..line.text
    i = i + 1
    sub.insert(line.num+i,line)
    line.text = OriginalText
  end
end

function NewTextExtents(style, text)
  style.scale_y = 100 -- line.yscl
  style.scale_x = 100 -- line.xscl
  style.fontname = line.fn or style.fontname -- avoid setting to nil
  style.fontsize = line.fs or style.fontsize
  return aegisub.text_extents(style, text)
end

function GetSizeOfVectorObject(vect) -- only works with objects consisting of LINES
  local ix, iy = vect:match("^m ([%-%d]+) ([%-%d]+)")
  local xmin, xmax, ymin, ymax = 0,0,0,0
  local function normalize(a,b)
    if tonumber(a)-ix > xmax then xmax = tonumber(a)-ix end
    if tonumber(a)-ix < xmin then xmin = tonumber(a)-ix end
    if tonumber(b)-iy > ymax then ymax = tonumber(b)-iy end
    if tonumber(b)-iy < ymin then ymin = tonumber(b)-iy end
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

function table.tostring(t)
  if type(t) ~= 'table' then
    return tostring(t)
  else
    local s = ''
    local i = 1
    while t[i] ~= nil do
      if #s ~= 0 then s = s..', ' end
      s = s..table.tostring(t[i])
      i = i+1
    end
    for k, v in pairs(t) do
      if type(k) ~= 'number' or k > i then
        if #s ~= 0 then s = s..', ' end
        local key = type(k) == 'string' and k or '['..table.tostring(k)..']'
        s = s..key..'='..table.tostring(v)
      end
    end
    return '{'..s..'}'
  end
end

aegisub.register_macro("ULTIMATE SUPERGRADIENT","GRAD YOUR ASS LIKE NEVER BEFORE", GatherLines)