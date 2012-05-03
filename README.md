﻿YIYAGTFA
--------

Ladies and Mentlegen, I introduce you to YIYAGTFA: YIYAGTFA Isn't Yet Another Gradient Tool For Aegisub!

No, wait, I decided to call it ASSgrad.

ASSgrad!
--------

And that's because it makes gradients. They are a lot of ASS. Its exciting feature is that it can do diagonal gradients and it supports alpha as well. This is cool, but can mean even more ASS. The amount of ASS generated by this script can make renderers perform like ASS, especially if you're trying to make awesome ASS or avoid banding that totally looks like ASS.

Currently, because I am an ASS, it only works reasonably with vertical color gradients (and only on main and border at that).

#### Usage ####

Once all the features are usable, however, here is what the interface shall be: at the beginning of the `Effect` field of each desired line, enter this: `<Ag>(1c:2c:3c:4c;1a:2a:3a:4a;[options])` where `1c` is a comma delimited list of colors in the format of `&HBBGGRR&` or `#RRGGBB`, where e.g. `BB` is a hexadecimal color value in the range `00–FF` for blue. `1a` is a comma delimited list of alpha values, and must be in decimal format from `0–255`. `[options]` is a placeholder for a comma separated list of the options that have to do with the clip areas (band width and band overlap, among others). I haven't decided what order these should go in yet. ASSgrad will iterate backwards across all of the lines in the script, collecting the ones that match the proper pattern in the `Effect` field, namely `^<Ag>\(.+?\)`. It will go through each collected line, creating the gradient for each one, and deleting the relevant contents of the `Effect` field. I currently have no plans for a tool to undo gradients, though if the demand is there, I suppose it should be easy enough to write.

The current options are Band Size, Band overlap, and left, top, right, bottom clipbox expansion (in that order).

Oh yeah, and it also requires Aegisub 3.0.0, because it uses its regular expression engine.

#### Examples ####

It's okay to neglect unused values, as long as you don't care to set anything after them.

`<Ag>(&HFF0000&,&H00FF00&)` is valid, and will create a gradient on color 1, from blue to green.

`<Ag>(&HFF0000&,#00FF00;;10,,5)` creates the same gradient as above, but it uses a band size of 10 pixels and has a left side clipbox expansion of 5 pixels. Also note that `#` is not required for rgb color formatting (that is, `#0000FF` and `0000FF` are equivalent (and also very blue)).

`<Ag>(&HFF0000&,&H00FF00&::&H00FF00&,&HFF0000&;:::0,180,255)` should be valid, and will create a gradient on color 1 (main color), from blue to green, a gradient on color 3 (outline), from green to blue, and an alpha gradient on color 4 (shadow) that goes from opaque, to translucent, to fully transparent.

`<Ag>(;;10,5)` will do nothing. (or, well, at this point it will probably do /something/)

And so on.