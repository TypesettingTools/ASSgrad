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
function atan2d(y,x) return math.deg(math.atan2(y,x)) end

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
  ['fax']     = "\\fax([%-%d%.]+)",
  ['fay']     = "\\fay([%-%d%.]+)",
  ['fs']      = "\\fs([%d%.]+)",  
}

vobj = re.compile("{.*?\\\\p1.*?}(.+?)({.*?\\\\p0.*?}|$)")
period = re.compile('\\.')
colon = re.compile(':')
semic = re.compile(';')
lf = re.compile('\\\\N') -- is double escaping still required?
-- <Ag>(1c1.1c2.1c3.1c4:2c:3c:4c;1a1.1a2:2a:3a:4a;bandsize.bandoverlap.l.t.r.b)
-- options to expose: band overlap, band size, theta (unimplemented)
function GatherLines(sub,sel)
  local gradlines = {}
  for x = #sel,1,-1 do
    local line = sub[sel[x]] -- loop backwards, so subs are added to the select table from last to first.
    if line.class == "dialogue" then
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
      color[ii] = ColorParse(period:split(x))
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
    line.num = v[1]
    options.meta, options.styles = karaskel.collect_head(sub,false)
    GiantMessyFunction(sub,line,color,alpha,options)
  end
end

function CleanTable(tabel)
  for i,v in ipairs(tabel) do -- should be sequential integer indices
    aegisub.log(0,"%d, %s - %s\n",i,table.tostring(v),tostring(#v))
    if v == "" or #v == 0 then
      aegisub.log(0,"baleeted\n")
      tabel[i] = nil
    end -- strip out blank entries - we can't use "skip empty" when splitting because it's order dependent.
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
      aegisub.log(0,"%s,%s,%s\n",r,g,b)
      table.insert(ReturnTab,{tonumber(r,16),tonumber(g,16),tonumber(b,16)})
    end
  end
  return ReturnTab
end

function MultilineExtents(line)
  local SplitTable = lf:split(line.text:gsub("{.-}","")) -- I don't remember if text_stripped gets rid of linebreaks
  local height, width, desc, ext = 0,0,0,0
  line.styleref.scale_y = 100 -- line.yscl
  line.styleref.scale_x = 100 -- line.xscl
  line.styleref.fontname = line.fn or style.fontname -- avoid setting to nil
  line.styleref.fontsize = line.fs or style.fontsize
  for i,v in ipairs(SplitTable) do
    local h,w,d,e = aegisub.text_extents(line.styleref,v)
    height = height + h + d -- each line includes descent
    if w > width then width = w end
    desc,ext = d,e
  end
  return height, width, desc, ext
end

function GiantMessyFunction(sub,line,ColorTable,AlphaTable,OptionsTable)
  karaskel.preproc_line(sub, OptionsTable.meta, OptionsTable.styles, line)
  GetInfo(sub, line, line.num)
  local OptionsTable = OptionsTable or {}
  local l = tonumber(OptionsTable[3]) or 0
  local t = tonumber(OptionsTable[4]) or 0
  local r = tonumber(OptionsTable[5]) or 0
  local b = tonumber(OptionsTable[6]) or 0
  aegisub.log(0,("l: %d t: %d r: %d b: %d\n"):format(l,t,r,b))
  local strs = vobj:match(line.text)
  if strs then
    line.width, line.height, line.descent, line.extlead = GetSizeOfVectorObject(strs[2].str)
  else
    line.width, line.height, line.descent, line.extlead = MultilineExtents(line) -- handle linebreaks
  end
  line.height = line.height + 2*line.bord
  line.width = line.width + 2*line.bord
  line.height = line.height - line.descent/2
  if line.margin_v ~= 0 then line._v = line.margin_v end
  if line.margin_l ~= 0 then line._l = line.margin_l end
  if line.margin_r ~= 0 then line._r = line.margin_r end
  if not line.xpos then
    line.xpos = fix.xpos[line.ali%3+1](OptionsTable.meta.res_x,line._l,line._r)
    line.ypos = fix.ypos[math.ceil(line.ali/3)](OptionsTable.meta.res_y,line._v)
  end
  if not line.xorg then
    line.xorg = line.xpos
    line.yorg = line.ypos
  end
  local xd = line.xpos - line.xorg
  local yd = line.ypos - line.yorg
  local rad = math.sqrt(xd^2+yd^2)
  local alpha = atan2d(yd,xd)
  line.xpos = line.xorg + rad *cosd(alpha-line.zrot)
  line.ypos = line.yorg + rad *sind(alpha-line.zrot) --]]
  line.xpos,line.ypos = fix.ali[line.ali](line.xpos,line.ypos,line.width*line.xscl/100,line.height*line.yscl/100,line.zrot)
  if line.ali ~= 5 then
    if line.text:match("\\an[1-9]") then
      line.text = line.text:gsub("\\an[1-9]","\\an5")
    else
      line.text = "{\\an5}"..line.text
    end
  end
  line.text = line.text:gsub("\\pos%([%-%d%.]+,[%-%d%.]+%)","")
  line.text = line.text:gsub("\\org%([%-%d%.]+,[%-%d%.]+%)","")
  local i = 0
  line.height = line.height*line.yscl/100
  line.width = line.width*line.xscl/100
  local BandSize = tonumber(OptionsTable[1]) or 4
  local BandOverlap = tonumber(OptionsTable[2]) or BandSize -- important for this to be some factor of BandSize, especially if alpha is involved
  local theta = 0 -- need to figure out some math first
  --[[ define vectors ]]--
  local origin = vec:new(line.xpos, line.ypos, 0)
  local herp = table.copy_deep(line)
  herp.text = origin:draw()
  sub.insert(line.num+1,herp)
  local position = vec:new(-line.width*0.5-l, -line.height*0.5+line.descent*0.5-t, 0) -- top left corner from pos
  local topleft = position+origin -- vector from origin to the top left
  local left = vec:new(0, line.height+t+b, 0):addToAzimuth(math.rad(line.zrot))
  local top = vec:new(line.width+l+r, 0, 0):addToAzimuth(math.rad(line.zrot))
  local Length = math.ceil(left:getLength()/BandSize)
  local ColorTable = ColorTable or { -- put data in table as rgb for no good reason
    {240,240,240,}; -- nice defaults
    {237,142,183,};
  }
  for k,v in pairs(ColorTable) do -- baleet relevant color tags
    line.text = line.text:gsub(string.format("\\\\[%d][cC]&[hH]%%x+&",k),"")
  end
  for k,v in pairs(AlphaTable) do -- baleet relevant alpha tags
    line.text = line.text:gsub(string.format("\\\\[%d][aA]&[hH]%%x+&",k),"")
  end
  if #AlphaTable > 0 then
    line.text = line.text:gsub(string.format("\\\\alpha&[hH]%%x+&",k),"")
  end
  if ColorTable[1] then line.text = line.text:gsub("\\[cC]&[hH]%x+&","") end
  local OriginalText = line.text
  local PerColorLength = {}
  for k,v in pairs(ColorTable) do
    PerColorLength[k] = math.ceil(Length/(#v-1))
  end
  local PerAlphaLength = {}
  for k,v in pairs(AlphaTable) do
    PerAlphaLength[k] = math.ceil(Length/(#v-1))
  end
  --[[ this is for edges. I plan to switch to a central difference rather than a forward difference for the loop some time eventually.
  ColorTable[0] = table.copy(ColorTable[1])
  ColorTable[#ColorTable+1] = table.copy(ColorTable[#ColorTable]) --]]
  local ind = 1
  -- rectangular means right = left and bottom = top
  local vertband = left:unit():setLength(BandSize+BandOverlap)
  herp.text = vertband:draw(topleft)
  sub.insert(line.num+1,herp)
  for y = 0,math.floor(left:getLength()/BandSize)-1 do
    aegisub.log(0,"y: %d\n",y)
    local tl = vertband:copySpherical():addLength(y*BandSize-(BandSize+BandOverlap)):add(topleft)
    local tr = top:copySpherical():addToAzimuth(math.rad(theta)):add(tl)
    local br = tr:copySpherical():add(vertband)
    local bl = tl:copySpherical():add(vertband)
    local color = ""
    for ColorNum,ColorSubTable in pairs(ColorTable) do
      aegisub.log(5,"%d, %s\n",ColorNum,table.tostring(ColorSubTable))
      local CurrPCL = PerColorLength[ColorNum]
      local cur = math.floor(i/CurrPCL)+1 -- because math.ceil(0) == 0
      local red = round(ColorSubTable[cur][1]+(ColorSubTable[cur+1][1]-ColorSubTable[cur][1])*(i%CurrPCL+1)/CurrPCL) -- forward difference
      local gre = round(ColorSubTable[cur][2]+(ColorSubTable[cur+1][2]-ColorSubTable[cur][2])*(i%CurrPCL+1)/CurrPCL)
      local blu = round(ColorSubTable[cur][3]+(ColorSubTable[cur+1][3]-ColorSubTable[cur][3])*(i%CurrPCL+1)/CurrPCL)
      color = color..string.format("\\%dc&H%02X%02X%02X&",ColorNum,blu,gre,red) -- \c tags are in BGR order
    end
    local alpha = ""
    for AlphaNum,AlphaSubTable in pairs(AlphaTable) do
      aegisub.log(5,"%d, %s\n",AlphaNum,table.tostring(AlphaSubTable))
      local CurrPAL = PerAlphaLength[AlphaNum]
      local cur = math.floor(i/CurrPAL)+1 -- because math.ceil(0) == 0
      local calpha = round(AlphaSubTable[cur]+(AlphaSubTable[cur+1]-AlphaSubTable[cur])*(i%CurrPAL+1)/CurrPAL) -- forward difference
      alpha = alpha..string.format("\\%da&H%02X&",AlphaNum,calpha)
    end
    local clip = string.format("m %.0f %.0f l %.0f %.0f %.0f %.0f %.0f %.0f",tl[1],tl[2],tr[1],tr[2],br[1],br[2],bl[1],bl[2])
    line.text = '{'..color..alpha..string.format("\\clip(%s)\\pos(%.2f,%.2f)}",clip,line.xpos,line.ypos)..line.text
    i = i + 1
    sub.insert(line.num+i,line)
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
    if x > xmax then xmax = x end
    if x < xmin then xmin = x end
    aegisub.log(0,'New: xmin: %g, ymin: %g\n',xmin,ymin)
  end
  local function comparey(y)
    if y > ymax then ymax = y end
    if y < ymin then ymin = y end
    aegisub.log(0,'New: xmax: %g, ymax: %g\n',xmax,ymax)
  end
  local function linear(str)
    local errything, a, b = str:match("^(([%-%d]+) *([%-%d]+) *)")
    aegisub.log(0,("linear: %s; %s %s\n"):format(tostring(errything), tostring(a), tostring(b)))
    a, b = tonumber(a), tonumber(b)
    comparex(a); comparey(b)
    cursor = {a, b}
    return errything:len()
  end
  local function cubic(str)
    local x, y = {}, {}
    aegisub.log(0,"Cubic: %s\n",str)
    local errything
    x[1], y[1] = cursor[1], cursor[2]
    errything, x[2],y[2],x[3],y[3],x[4],y[4] = str:match("^(([%-%d]+) *([%-%d]+) *([%-%d]+) *([%-%d]+) *([%-%d]+) *([%-%d]+) *)")
    aegisub.log(0,("Cubic: %s\n%s\n%s\n"):format(errything,table.tostring(x),table.tostring(y)))
    for i = 2,4 do
      x[i], y[i] = tonumber(x[i]), tonumber(y[i])
    end
    -- imma just copy some derivatives yep
    local function cubicmaxima(x)
      local t = {}
      if x[4] == x[1]-3*x[2]+3*x[3] then
        if (x[1]-2*x[2]+x[3]) ~= 0 then -- quadratic case, only one possible maximum/minimum
          table.insert(t,(x[1]-x[2])/(2*(x[1]-2*x[2]+x[3])))
        else
          -- lies on endpoints
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
    return errything:len()
  end
  local carrot = 1
  local command, prevcom
  while carrot < vect:len() do
    prevcom = command
    command = vect:sub(carrot):match("^([bml]) ") -- only care these 3
    if command then
      aegisub.log(0,"Command: %s\n",command)
      carrot = carrot + 2
      aegisub.log(0,vect:sub(carrot)..'\n')
      carrot = carrot + ({b = cubic, m = linear, l = linear})[command](vect:sub(carrot))
    else
      aegisub.log(0,"No command: %s\n",prevcom)
      aegisub.log(0,vect:sub(carrot)..'\n')
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

function GetInfo(sub, line, num) -- because CamelCase
  for k, v in pairs(header) do
    line[k] = line.styleref[v]
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
    for b in line.text:gmatch("%{(.-)%}") do
      for c in b:gmatch("\\t(%b())") do -- this will return an empty string for t_exp if no exponential factor is specified
        t_start,t_end,t_exp,t_eff = c:sub(2,-2):match("([%-%d]+),([%-%d]+),([%d%.]*),?(.+)")
        if t_exp == "" then t_exp = 1 end -- set it to 1 because stuff and things
        table.insert(line.trans,{tonumber(t_start),tonumber(t_end),tonumber(t_exp),t_eff})
      end
    end
    -- have to run it again because of :reasons: related to bad programming
    if line.bord then line.xbord = tonumber(line.bord); line.ybord = tonumber(line.bord); end
    if line.shad then line.xshad = tonumber(line.shad); line.yshad = tonumber(line.shad); end
  end
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

aegisub.register_macro("ULTIMATE SUPERGRADIENT","GRAD YOUR ASS LIKE NEVER BEFORE", GatherLines)