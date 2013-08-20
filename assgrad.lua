--[=[ You are not legally allowed to look at this code because it is proprietary. STOP BREAKING THE LAW ]=]--

script_name = "ASSGRAD"
script_description = "THE ULTIMATE METHOD OF SCRIPT BLOATING HAS FINALLY ARRIVED"
script_author = "torque"
script_version = "9001"
require "karaskel"
require "utils"
local success, re = pcall(require, "aegisub.re")
if not (success) then
  re = require(aegisub.decode_path("?data/automation/include/re.lua"))
end

function cosd(a) return math.cos(math.rad(a)) end
function acosd(a) return math.deg(math.acos(a)) end
function sind(a) return math.sin(math.rad(a)) end
function asind(a) return math.deg(math.asin(a)) end
function tand(a) return math.tan(math.rad(a)) end
function atan2d(y,x) return math.deg(math.atan2(y,x)) end

fix = {
  xpos = {
    function(sx,l,r) return sx-r end;
    function(sx,l,r) return l    end;
    function(sx,l,r) return sx/2 end;
  },
  ypos = {
    function(sy,v) return sy-v end;
    function(sy,v) return sy/2 end;
    function(sy,v) return v    end;
  }
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
  ['xscl']    = [[\fscx([%d%.]+)]],
  ['yscl']    = [[\fscy([%d%.]+)]],
  ['ali']     = [[\an([1-9])]],
  ['zrot']    = [[\frz?([%-%d%.]+)]],
  ['bord']    = [[\bord([%d%.]+)]],
  ['xbord']   = [[\xbord([%d%.]+)]],
  ['ybord']   = [[\ybord([%d%.]+)]],
  ['shad']    = [[\shad([%-%d%.])]],
  ['xshad']   = [[\xshad([%-%d%.]+)]],
  ['yshad']   = [[\yshad([%-%d%.]+)]],
  ['fax']     = [[\fax([%-%d%.]+)]],
  ['fay']     = [[\fay([%-%d%.]+)]],
  ['fs']      = [[\fs([%d%.]+)]],
}

vobj = re.compile([[\{.*?\\p1.*?\}(.+?)(\{.*?\\p0.*?\}|$)]])
period = re.compile([[\.]])
colon = re.compile(':')
semic = re.compile(';')
lf = re.compile([[\\N]])
-- <Ag>(1c1.1c2.1c3.1c4:2c:3c:4c;1a1.1a2:2a:3a:4a;mode.bandSize.bandOverlap.theta.l.t.r.b)
-- options to expose: band overlap, band size, theta (unimplemented)
function GatherLines(sub,sel)
  local gradlines = {}
  for x = #sel,1,-1 do
    local line = sub[sel[x]] -- loop backwards, so subs are added to the select table from last to first.
    if line.class == "dialogue" then
      local _,com = line.effect:match("<Ag>%((.-)%)")
      if line.effect:match("<Ag>%(.-%)") then -- use lua's own pattern matching as much as possible because it's very fast.
        table.insert(gradlines,{sel[x],line.effect:match("<Ag>%((.-)%)")})
      end
    end
  end
  Crunch(sub,gradlines)
end

function Crunch(sub,sel)
  for i,v in ipairs(sel) do
    local color = {}
    local alpha = {}
    local options = {}
    local line = sub[v[1]]
    line.comment = true; sub[v[1]] = line; line.comment = false
    line.effect = line.effect:gsub("<Ag>%(.-%)","")
    local all = semic:split(v[2]) -- {color, alpha, options}
    local colour = colon:split(all[1])
    for ii,x in ipairs(colour) do
      color[ii] = ParseColors(period:split(x))
    end
    CleanTable(color)
    colour = nil
    if all[2] then
      local alepha = colon:split(all[2])
      for ii,x in ipairs(alepha) do
        alpha[ii] = period:split(x)
      end
      CleanTable(alpha)
      alepha = nil
    end
    if all[3] then
      options = period:split(all[3])
      CleanTable(options)
    end
    ParseOptions(options)
    line.num = v[1]
    options.meta, options.styles = karaskel.collect_head(sub,false)
    GiantMessyFunction(sub,line,color,alpha,options)
  end
end

function CleanTable(tabel)
  for i,v in ipairs(tabel) do -- should be sequential integer indices
    aegisub.log(4,"%d, %s - %s\n",i,table.tostring(v),tostring(#v))
    if v == "" or #v == 0 then
      aegisub.log(4,"baleeted\n")
      tabel[i] = nil
    end -- strip out blank entries - we can't use "skip empty" when splitting because it's order dependent.
  end
end

--[=[ {
  {
    {0,0,0},
    {120,120,120},
  },
  {
    {5,10,15},
    {255,255,255},
  }, ...etc
}
-> color[1][1] == first color of first-color gradient
--]=]

function ParseColors(ColorTab) --BGR, RGB
  local ReturnTab = {}
  for i,v in ipairs(ColorTab) do
    if v:match("^&H") then
      local b,g,r = v:match("^&H(%x%x)(%x%x)(%x%x)&")
      aegisub.log(4,"%s,%s,%s\n",r,g,b)
      table.insert(ReturnTab,{tonumber(r,16),tonumber(g,16),tonumber(b,16)})
    else
      local r,g,b = v:match("^#?(%x%x)(%x%x)(%x%x)")
      aegisub.log(4,"%s,%s,%s\n",r,g,b)
      table.insert(ReturnTab,{tonumber(r,16),tonumber(g,16),tonumber(b,16)})
    end
  end
  return ReturnTab
end

function ParseOptions(options)
  local opts = {'mode','bandSize','bandOverlap','theta','left','top','right','bottom'}
  local def = {0,4,false,0,0,0,0,0}
  for index = 1,#opts do
    options[(opts[index])] = options[index] or def[index]
  end
  options.bandOverlap = options.bandOverlap or options.bandSize
  debug(table.tostring(options)..'\n')
end

function MultilineExtents(line)
  local SplitTable = lf:split(line.text_stripped)
  local height, width, desc, ext = 0,0,0,0
  line.styleref.scale_y = 100 -- line.yscl
  line.styleref.scale_x = 100 -- line.xscl
  line.styleref.fontname = line.fn or style.fontname -- avoid setting to nil
  line.styleref.fontsize = line.fs or style.fontsize
  for i,v in ipairs(SplitTable) do
    local w,h,d,e = aegisub.text_extents(line.styleref,v)
    height = height + h + d -- each line includes descent
    if w > width then width = w end
    desc, ext = d, e
  end
  height = height - desc -- subtract last descender off
  return width, height, desc, ext
end

makePosVec = {
  x = {
    function(line,options) return line.xbord-(line.width+options.left) end, -- 3,6,9
    function(line,options) return -(options.left+line.xbord) end, -- 1,4,7
    function(line,options) return -(line.width*0.5+options.left) end, -- 2,5,8
  },
  y = {
    function(line,options) return line.ybord-(line.height+options.top) end, -- 1,2,3
    function(line,options) return line.descent*0.5-(line.height*0.5+options.top) end, -- 4,5,6
    function(line,options) return -(options.top+line.ybord) end, -- 7,8,9
  }
}

function GiantMessyFunction(sub,line,colors,alphas,options)
  karaskel.preproc_line(sub, options.meta, options.styles, line)
  GetInfo(line,options)
  local xd = line.xpos - line.xorg
  local yd = line.ypos - line.yorg
  local rad = math.sqrt(xd^2+yd^2)
  local alpha = atan2d(yd,xd)
  line.xpos = line.xorg + rad*cosd(alpha-line.zrot)
  line.ypos = line.yorg + rad*sind(alpha-line.zrot) --]]
  line.text = line.text:gsub("\\pos%([%-%d%.]+,[%-%d%.]+%)","")
  line.text = line.text:gsub("\\org%([%-%d%.]+,[%-%d%.]+%)","")
  local i = 0
  line.height = line.height*line.yscl/100
  line.width = line.width*line.xscl/100
  local theta = 0 -- need to figure out some math first
  --[[ define vectors ]]--
  local origin = vec:new(line.xpos, line.ypos, 0)
  local linecopy = table.copy_deep(line)
  linecopy.layer = 999
  linecopy.text = origin:draw()
  --sub.insert(line.num+1,linecopy)
  --log(line.width.." + "..line.xbord.." = "..makePosVec.x[line.ali%3+1](line,options)..'\n')
  local position = vec:new(makePosVec.x[line.ali%3+1](line,options), makePosVec.y[math.ceil(line.ali/3)](line,options), 0):addToAzimuth(math.rad(line.zrot)) -- top left corner from pos
  local topleft = position+origin -- vector from origin to the top left
  local left = vec:new(0, line.height+options.top+options.bottom, 0):addToAzimuth(math.rad(line.zrot))
  local top = vec:new(line.width+options.left+options.right, 0, 0):addToAzimuth(math.rad(line.zrot))
  local Length = math.floor(left:getLength()/options.bandSize)
  for k,v in pairs(colors) do -- baleet relevant color tags
    line.text = line.text:gsub(string.format("\\\\[%d][cC]&[hH]%%x+&",k),"")
  end
  for k,v in pairs(alphas) do -- baleet relevant alpha tags
    line.text = line.text:gsub(string.format("\\\\[%d][aA]&[hH]%%x+&",k),"")
  end
  if #alphas > 0 then
    line.text = line.text:gsub(string.format("\\\\alpha&[hH]%%x+&",k),"")
  end
  if colors[1] then line.text = line.text:gsub("\\[cC]&[hH]%x+&","") end
  local OriginalText = line.text
  local PerColorLength = {}
  for ColorNum,SubTable in pairs(colors) do
    PerColorLength[ColorNum] = math.floor(Length/(#SubTable-1)) -- number of steps per color
    SubTable[0] = table.copy(SubTable[1])
    SubTable[#SubTable+1] = table.copy(SubTable[#SubTable])
  end
  local PerAlphaLength = {}
  for k,v in pairs(alphas) do
    PerAlphaLength[k] = math.ceil(Length/(#v-1))
  end
  local ind = 1
  -- rectangular means right = left and bottom = top
  local vertband = left:unit():setLength(options.bandSize+options.bandOverlap)
  linecopy.text = position:draw(origin)
  --sub.insert(line.num+1,linecopy)
  local max = math.floor(left:getLength()/options.bandSize)-1
  local cur = 1
  for y = 0, max do
    aegisub.log(4,"y: %d\n",y)
    local tl = vertband:copySpherical():addLength(y*options.bandSize-(options.bandSize+options.bandOverlap)):add(topleft)
    local tr = top:copySpherical():addToAzimuth(math.rad(theta)):add(tl)
    local br = tr:copySpherical():add(vertband)
    local bl = tl:copySpherical():add(vertband)
    local color = "";
    for ColorNum,ColorSubTable in pairs(colors) do
      aegisub.log(5,"%d, %s\n",ColorNum,table.tostring(ColorSubTable))
      local CurrPCL = PerColorLength[ColorNum]
      if i == CurrPCL then i = 0; cur = cur+1 end
      debug('%d - PCL %g; cur %g; pct %g\n',i,CurrPCL,cur,i/CurrPCL)
      local red = round(interpolate(i/CurrPCL,ColorSubTable[cur][1],ColorSubTable[cur+1][1]))
      local gre = round(interpolate(i/CurrPCL,ColorSubTable[cur][2],ColorSubTable[cur+1][2]))
      local blu = round(interpolate(i/CurrPCL,ColorSubTable[cur][3],ColorSubTable[cur+1][3]))
      color = color..string.format("\\%dc&H%02X%02X%02X&",ColorNum,blu,gre,red) -- \c tags are in BGR order
    end
    local alpha = ""
    for AlphaNum,AlphaSubTable in pairs(alphas) do
      aegisub.log(5,"%d, %s\n",AlphaNum,table.tostring(AlphaSubTable))
      local CurrPAL = PerAlphaLength[AlphaNum]
      local cur = math.floor(i/CurrPAL)+1 -- because math.ceil(0) == 0
      local calpha = round(AlphaSubTable[cur]+(AlphaSubTable[cur+1]-AlphaSubTable[cur])*(i%CurrPAL+1)/CurrPAL) -- forward difference
      alpha = alpha..string.format("\\%da&H%02X&",AlphaNum,calpha)
    end
    local clip = string.format("m %.0f %.0f l %.0f %.0f %.0f %.0f %.0f %.0f",tl[1],tl[2],tr[1],tr[2],br[1],br[2],bl[1],bl[2])
    line.text = '{'..color..alpha..string.format("\\clip(%s)}",clip)..line.text
    i = i + 1
    sub.insert(line.num+1,line)
    line.layer = line.layer + 1
    line.text = OriginalText
  end
end

function GetSizeOfVectorObject(vect) -- doesn't actually work with an5 stuff silly me
  local ix, iy = vect:match("^m ([%-%d]+) ([%-%d]+)")
  ix, iy = tonumber(ix), tonumber(iy)
  local cursor = {ix, iy}
  local xmin, xmax, ymin, ymax = ix, iy, ix, iy
  local function comparex(x)
    if x > xmax then
      xmax = x; aegisub.log(4,'New: xmax: %g\n',xmax)
    end
    if x < xmin then
      xmin = x; aegisub.log(4,'New: xmin: %g',xmin)
    end
  end
  local function comparey(y)
    if y > ymax then
      ymax = y; aegisub.log(4,'New: ymax: %g\n',ymax)
    end
    if y < ymin then
      ymin = y; aegisub.log(4,'New: ymin: %g',ymin)
    end
  end
  local function linear(str)
    local all, a, b = str:match("^(([%-%d]+) *([%-%d]+) *)")
    aegisub.log(4,'linear: %s %s\n',a,b)
    a,b = tonumber(a), tonumber(b)
    cursor = {a, b}
    comparey(b); comparex(a)
    return all:len()
  end
  local function cubic(str)
    local x, y = {}, {}
    local all
    x[1], y[1] = cursor[1], cursor[2]
    all, x[2],y[2],x[3],y[3],x[4],y[4] = str:match("^(([%-%d]+) *([%-%d]+) *([%-%d]+) *([%-%d]+) *([%-%d]+) *([%-%d]+) *)")
    aegisub.log(4,'Cubic: %s %s %s %s %s %s\n',x[2],y[2],x[3],y[3],x[4],y[4])
    for i = 2,4 do
      x[i], y[i] = tonumber(x[i]), tonumber(y[i])
    end
    -- imma just copy some derivatives yep
    local function cubicmaxima(x)
      local t = {}
      if x[4] == x[1]-3*x[2]+3*x[3] then
        if (x[1]-2*x[2]+x[3]) ~= 0 then -- quadratic case, only one possible maximum/minimum
          table.insert(t,(x[1]-x[2])/(2*(x[1]-2*x[2]+x[3])))
        end
      else
        table.insert(t,(math.sqrt(-x[1]*x[3]+x[1]*x[4]+x[2]^2-x[2]*x[3]-x[2]*x[4]+x[3]^2)-x[1]+2*x[2]-x[3])/(-x[1]+3*x[2]-3*x[3]+x[4]))
        table.insert(t,(-math.sqrt(-x[1]*x[3]+x[1]*x[4]+x[2]^2-x[2]*x[3]-x[2]*x[4]+x[3]^2)-x[1]+2*x[2]-x[3])/(-x[1]+3*x[2]-3*x[3]+x[4]))
      end
      return t
    end
    local t_x, t_y = cubicmaxima(x), cubicmaxima(y)
    if t_x then
      for i,v in ipairs(t_x) do
        if v >= 0 and v <= 1 then
          local a = (1-v)^3*x[1]+3*(1-v)^2*v*x[2]+3*(1-v)*v^2*x[3]+v^3*x[4]
          comparex(a)
        end
      end
    end
    if t_y then
      for i,v in ipairs(t_y) do
        if v >= 0 and v <= 1 then
          local b = (1-v)^3*y[1]+3*(1-v)^2*v*y[2]+3*(1-v)*v^2*y[3]+v^3*y[4]
          comparey(b)
        end
      end
    end
    comparex(x[4]); comparey(y[4])
    cursor = {x[4], y[4]}
    return all:len()
  end
  local carrot = 1
  local command, prevcom
  while carrot < vect:len() do
    prevcom = command
    command = vect:sub(carrot):match("^([bml]) ") -- only care these 3
    if command then
      aegisub.log(4,"Command: %s\n",command)
      carrot = carrot + 2
      aegisub.log(4,vect:sub(carrot)..'\n')
      carrot = carrot + ({b = cubic, m = linear, l = linear})[command](vect:sub(carrot))
    else
      aegisub.log(4,"No command: %s\n",prevcom)
      aegisub.log(4,vect:sub(carrot)..'\n')
      carrot = carrot + ({b = cubic, m = linear, l = linear})[prevcom](vect:sub(carrot))
      command = prevcom
    end
  end
  return xmax-xmin+2,ymax-ymin+2,0,0 -- pad out by 2px
end

function intersect(line1, line2)
  local a1, b1 = line1[4]-line1[2], line1[1]-line1[3]
  local c1 = a1*line1[1], b1*line1[2]
  local a2, b2 = line2[4]-line2[2], line2[1]-line2[3]
  local c2 = a2*line2[1], b2*line2[2]
  local det = a1*b2 - a2*b1
  local x,y
  if det ~= 0 then
    x = (b2*c1 - b1*c2)/det
    y = (a1*c2 - a2*c1)/det
  end
end

function GetInfo(line,options) -- because CamelCase
  for k, v in pairs(header) do
    line[k] = line.styleref[v]
    aegisub.log(5,"Line %d: %s set to %s (from header)\n", line.num, v, tostring(line[k]))
  end
  if line.bord then line.xbord = tonumber(line.bord); line.ybord = tonumber(line.bord); end
  if line.shad then line.xshad = tonumber(line.shad); line.yshad = tonumber(line.shad); end
  line.xpos, line.ypos = line.text:match("\\pos%(([%-%d%.]+),([%-%d%.]+)%)")
  line.xorg, line.yorg = line.text:match("\\org%(([%-%d%.]+),([%-%d%.]+)%)")
  if line.margin_v ~= 0 then line._v = line.margin_v end
  if line.margin_l ~= 0 then line._l = line.margin_l end
  if line.margin_r ~= 0 then line._r = line.margin_r end
  line.trans = {}
  local a = line.text:match("%{(.-)}")
  if a then
    for k, v in pairs(patterns) do
      local _ = a:match(v)
      if _ then
        line[k] = tonumber(_)
        aegisub.log(5,"Line %d: %s set to %g\n",line.num,k,_)
      end
    end
    if a:match("\\fn([^\\}]+)") then line.fn = a:match("\\fn([^\\}]+)") end
    if line.bord then line.xbord = tonumber(line.bord); line.ybord = tonumber(line.bord); end
    if line.shad then line.xshad = tonumber(line.shad); line.yshad = tonumber(line.shad); end
  end
  local strs = vobj:match(line.text)
  if strs then
    line.isVector = true
    line.width, line.height, line.descent, line.extlead = GetSizeOfVectorObject(strs[2].str)
  else
    line.width, line.height, line.descent, line.extlead = MultilineExtents(line) -- handle linebreaks
  end
  if not line.xpos then
    line.xpos = fix.xpos[line.ali%3+1](options.meta.res_x,line._l,line._r)
    line.ypos = fix.ypos[math.ceil(line.ali/3)](options.meta.res_y,line._v)
  end
  if not line.xorg then
    line.xorg, line.yorg = line.xpos, line.ypos
  end
  line.width = line.width + line.xbord*2
  line.height = (line.height + line.ybord*2) - line.descent/2
end

vec = {} -- WE GOING ALL OO ON THIS BITCH
function vec:new(x,y,z,coords) -- create a new vector table with
  local v = {x or 0, y or 0, z or 0, 1}
  v.type = ({cartesian = true})[coords or 'cartesian'] or false
  setmetatable(v,self)
  self.__index = self
  return v
end

function vec.zero()
  return vec:new(0,0,0)
end

function vec.n(n)
  return vec:new(n,n,n)
end

function vec:components() -- probably useless
  return self[1], self[2], self[3]
end

function vec:toSpherical() -- cartesian -> spherical
  if self.type then
    local v = {}
    v[1] = self:getLength() -- radial distance
    v[2] = math.acos(self[3]/v[1]) -- elevation - corresponds to z (angle measured from the positive z-axis)
    v[3] = math.atan2(self[2],self[1]) -- azimuth  - corresponds to x and y
    self[1] = v[1]; self[2] = v[2]; self[3] = v[3]
    self.type = false
  end
  return self
end

function vec:copySpherical()
  if self.type then
    local v = {}
    v[1] = self:getLength() -- radial distance
    v[2] = math.acos(self[3]/v[1]) -- elevation - corresponds to z (angle measured from the positive z-axis)
    v[3] = math.atan2(self[2],self[1]) -- azimuth  - corresponds to x and y
    return vec:new(v[1],v[2],v[3],'spherical')
  else
    return vec:new(self[1],self[2],self[3],'spherical')
  end
end

function vec:toCartesian() -- spherical -> cartesian
  if not self.type then
    local v = {}
    v[1] = self[1]*math.cos(self[3])*math.sin(self[2])
    v[2] = self[1]*math.sin(self[3])*math.sin(self[2])
    v[3] = self[1]*math.cos(self[2])
    self[1] = v[1]; self[2] = v[2]; self[3] = v[3]
    self.type = true
  end
  return self
end

function vec:copyCartesian()
  if not self.type then
    local v = {}
    v[1] = self[1]*math.cos(self[3])*math.sin(self[2])
    v[2] = self[1]*math.sin(self[3])*math.sin(self[2])
    v[3] = self[1]*math.cos(self[2])
    return vec:new(v[1],v[2],v[3])
  else
    return vec:new(self[1],self[2],self[3])
  end
end

function vec:twoPoints(offset)
  local offset = offset or vec.zero()
  return {offset[1], offset[2], self[1]+offset[1], self[2]+offset[2]}
end

function vec:getX() -- sugoi lazycode. Hopefully without memory leaks.
  return (self:copyCartesian())[1]
end

function vec:getY()
  return (self:copyCartesian())[2]
end

function vec:getZ()
  return (self:copyCartesian())[3]
end

function vec:getLength() -- vector length
  if self.type then
    return math.sqrt(self[1]^2 + self[2]^2 + self[3]^3)
  else
    return self[1]
  end
end

function vec:getAzimuth()
  return (self:copySpherical())[3]
end

function vec:setLength(length)
  if self.type then
    self:toSpherical()
    self[1] = length
    self:toCartesian()
  else
    self[1] = length
  end
  return self
end

function vec:addLength(length) -- add a scalar length
  if self.type then
    self:toSpherical()
    self[1] = self[1] + length
    self:toCartesian()
  else
    self[1] = self[1] + length
  end
  return self
end

function vec:addToAzimuth(angle,returnSpherical) -- add to the azimuth
  if self.type then
    self:toSpherical()
    self[3] = self[3] - angle
    self:toCartesian()
  else
    self[3] = self[3] - angle
  end
  if returnSpherical then self:toSpherical() end
  return self
end

function vec:add(v,returnSpherical) -- self + v
  self:toCartesian()
  local a = v:copyCartesian()
  self[1] = self[1] + a[1]
  self[2] = self[2] + a[2]
  self[3] = self[3] + a[3]
  if returnSpherical then self:toSpherical() end
  return self
end

function vec:sub(v,returnSpherical) -- self - v
  self:toCartesian()
  local a = v:copyCartesian()
  self[1] = self[1] - a[1]
  self[2] = self[2] - a[2]
  self[3] = self[3] - a[3]
  if returnSpherical then self:toSpherical() end
  return self
end

function vec:scale(s,returnSpherical) -- self*s
  self:toCartesian()
  local a = v:copyCartesian()
  self[1] = self[1] * a[1]
  self[2] = self[2] * a[2]
  self[3] = self[3] * a[3]
  if returnSpherical then self:toSpherical() end
  return self
end

function vec:unit() -- unit vector in the direction of vector v
  local il = 1/self:toCartesian():getLength()
  return vec:new(self[1]*il, self[2]*il, self[3]*il)
end

function vec:dot(v) -- self*v
  local a, b = self:copyCartesian(), v:copyCartesian() -- being this lazy is probably very bad
  return a[1]*b[1] + a[2]*b[2] + a[3]*b[3]
end

function vec:cross(v) -- selfxv
  local a, b = self:copyCartesian(), v:copyCartesian()
  local x = a[2]*b[3] - a[3]*b[2]
  local y = a[1]*b[3] - a[3]*b[1]
  local z = a[1]*b[2] - a[2]*b[1]
  return vec:new(x,y,z)
end

function vec:draw(offset)
  local offset = offset or vec.zero()
  local x1, y1 = offset:getX(), offset:getY()
  local x2, y2 = self:getX()+x1, self:getY()+y1
  return ("{\\bord1\\an7\\pos(0,0)\\p1\\3c&H000000&}m %d %d l %d %d"):format(x1,y1,x2,y2)
end

vec.__add = function(vec1, vec2)
  local a = vec1:copyCartesian()
  local b = vec2:copyCartesian()
  return vec:new(a[1]+b[1],a[2]+b[2],a[3]+b[3])
end

function round(num, idp)
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

function debug(...)
  if dbg then
    aegisub.log(0,...)
  end
end

aegisub.register_macro("ULTIMATE SUPERGRADIENT","GRAD YOUR ASS LIKE NEVER BEFORE", GatherLines)
