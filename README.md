﻿ASSgrad
-------

Ladies and Mentlegen, I introduce you to YIYAGTFA: YIYAGTFA Isn't Yet Another Gradient Tool For Aegisub!

No, wait, I decided to call it ASSgrad.

And that's because it makes gradients. They are a lot of ASS. Its exciting feature is that it can do diagonal gradients. This is cool, but can mean even more ASS. The amount of ASS generated by this script can make renderers perform like ASS, especially if you're trying to make awesome ASS or avoid banding that totally looks like ASS.

Currently, because I am an ASS, it's completely unusable.

#### Usage ####

Once it is usable, however, here is what the interface shall be: at the beginning of the `Effects` field of each desired line, enter this: `<Ag>(1c:2c:3c:4c;1a:2a:3a:4a;[options])` where `1c` is a comma delimited list of colors in the format of `&HBBGGRR&` or `#RRGGBB`, where e.g. `BB` is a hexadecimal color value in the range `00–FF` for blue. `1a` is a comma delimited list of alpha values, and must be in decimal format from `0–255`. `[options]` is a placeholder for a colon separated list of the options that have to do with the clip areas (band width and band overlap, among others). I haven't decided what order these should go in yet. ASSgrad will iterate backwards across all of the lines in the script, collecting the ones that match the proper pattern in the `Effects` field, namely `^<Ag>\(.+?\)`.

#### Examples ####

It's okay to neglect unused values, as long as you don't care to set anything after them.

`<Ag>(&HFF0000&,&H00FF00&)` is valid, and will create a gradient on color 1, from blue to green.

`<Ag>(&HFF0000&,#0000FF;;[options])` creates the same gradient as above, but with some (or all) options changed from the deafult (option formatting will be decided upon eventually). Also note that `#` is not required for rgb color formatting (that is, `#0000FF` and `0000FF` are equivalent).

`<Ag>(&HFF0000&,&H00FF00&::&H00FF00&,&HFF0000&;:::0,230,255)` should be valid, and will create a gradient on color 1 (main color), from blue to green, a gradient on color 3 (outline), from green to blue, and an alpha gradient on color 4 (shadow) that goes from opaque, to transluscent, to fully transparent.

And so on.