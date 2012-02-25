--[=[ You are not legally allowed to look at this code because it is proprietary. STOP BREAKING THE LAW ]=]--

script_name = "ULTIMATESUPERGRADIENT"
script_description = "THE ULTIMATE METHOD OF SCRIPT BLOATING HAS FINALLY ARRIVED"
script_author = "torque"
script_version = "9001"
require "karaskel"
require "re"

function dcos(a) return math.cos(a*math.pi/180) end
function dacos(a) return 180*math.acos(a)/math.pi end
function dsin(a) return math.sin(a*math.pi/180) end
function dasin(a) return 180*math.asin(a)/math.pi end
function dtan(a) return math.tan(a*math.pi/180) end
function datan(x,y) return 180*math.atan2(x,y)/math.pi end

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
    line.text = line.text:gsub("\\1?c&H%X%X%X%X%X%X&","")
    local i = 0
    local orgtext = line.text
    line.height = line.height*line.yscl/100
    line.width = line.width*line.xscl/100
    local len = round((line.height-line.descent/2))-1
    local colort = {
      {0,0,0,};
      {255,0,0,};
      {50,255,0,};
    }
    local ind = 1
    local pclen = round(len/(#colort-1)) -- transition lengths
    for y = -round((line.height-line.descent/2)/2),round((line.height-line.descent/2)/2)-1 do
      --aegisub.log(1,y.."\n")
      local tlx = line.xpos-((line.width/2)*dcos(line.zrot)-y*dsin(line.zrot))
      local tly = line.ypos+(y*dcos(line.zrot)+(line.width/2)*dsin(line.zrot))
      local trx = line.xpos+((line.width/2)*dcos(line.zrot)+y*dsin(line.zrot))
      local try = line.ypos+(y*dcos(line.zrot)-(line.width/2)*dsin(line.zrot))
      local brx = line.xpos+((line.width/2)*dcos(line.zrot)+y*dsin(line.zrot))+1*dsin(line.zrot)
      local bry = line.ypos+(y*dcos(line.zrot)-(line.width/2)*dsin(line.zrot))+1*dcos(line.zrot)
      local blx = line.xpos-((line.width/2)*dcos(line.zrot)-y*dsin(line.zrot))+1*dsin(line.zrot)
      local bly = line.ypos+(y*dcos(line.zrot)+(line.width/2)*dsin(line.zrot))+1*dcos(line.zrot)
      local clip = string.format("m %.0f %.0f l %.0f %.0f %.0f %.0f %.0f %.0f",tlx,tly,trx,try,brx,bry,blx,bly)
      local cur = math.floor(i/pclen)+1
      local color = string.format("%02X%02X%02X",round(colort[cur][1]+(colort[cur+1][1]-colort[cur][1])*(i%pclen+1)/pclen),round(colort[cur][2]+(colort[cur+1][2]-colort[cur][2])*(i%pclen+1)/pclen),round(colort[cur][3]+(colort[cur+1][3]-colort[cur][3])*(i%pclen+1)/pclen)) -- 255*i/len,0,0
      line.text = string.format("{\\c&H%s&\\clip(%s)\\pos(%.2f,%.2f)}",color,clip,line.xpos,line.ypos)..line.text
      i = i+1
      sub.insert(v+i,line)
      line.text = orgtext
    end
  end
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
    --if line.clip then aegisub.log(5,"Clip: %s%s)\n",line.clips,line.clip) end -- because otherwise it crashes!
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

function round(num, idp) -- borrowed from the lua-users wiki
  local mult = 10^(idp or 0)
  return math.floor(num * mult + 0.5) / mult
end

aegisub.register_macro("ASSgrad","GRAD YOUR ASS LIKE NEVER BEFORE", GiantMessyFunction)