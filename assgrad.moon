-- #!/usr/local/bin/moon

export script_name = "ASSGRAD"
export script_description = "THE ULTIMATE METHOD OF SCRIPT BLOATING HAS FINALLY ARRIVED"
export script_author = "torque"
export script_version = "0.0.9001"
require "karaskel"
require "utils"
success, re = pcall(require, "aegisub.re")
re = require aegisub.decode_path "?data/automation/include/re.lua" unless success

vobj = re.compile [[\{.*?\\p\d+.*?\}(.+?)(\{.*?\\p0.*?\}|$)]]
period = re.compile [[\.]]
colon = re.compile ':'
semic = re.compile ';'
lf = re.compile [[\\N]]

class Vector
	-- Constructor
	new: ( x = 0, y = 0, z = 0 ) =>
		@x, @y, @z = x, y, z
		@updateSphericalCoords!

	-- Creators
	-- copies current vector to a new vector.
	copy: =>
		Vector @x, @y, @z

	-- because typing is difficult?
	zero: =>
		Vector 0, 0, 0

	-- Cross products are order dependent. This creates self x vector
	crossProduct: ( vector ) =>
		Vector @y * vector.z - @z * vector.y,
		       @x * vector.z - @z * vector.x,
		       @x * vector.y - @y * vector.x

	-- Modifiers
	-- update spherical coordinates from current Cartesian coordinates
	updateSphericalCoords: =>
		@r = @getLength! -- radius
		if @r == 0 -- don't want @e to be NaN
			@e = 0
			@a = 0
		else
			@e = math.acos @z / @r
			@a = math.atan2 @y, @x

	-- update Cartesian coordinates from current spherical coordinates
	updateCartesianCoords: =>
		@evsin = math.sin @e
		@x = @r * math.cos( @a ) * @evsin
		@y = @r * math.sin( @a ) * @evsin
		@z = @r * math.cos @e

	updateCartesianFromRadius: ( ratio ) =>
		@x *= ratio
		@y *= ratio
		@z *= ratio

	updateCartesianFromAzimuth: =>
		@x = @r * math.cos( @a ) * @evsin
		@y = @r * math.sin( @a ) * @evsin

	-- add another vector to this one.
	add: ( vector ) =>
		@x += vector.x
		@y += vector.y
		@z += vector.z
		@updateSphericalCoords!

	-- subtract another vector from this one.
	subtract: ( vector ) =>
		@x -= vector.x
		@y -= vector.y
		@z -= vector.z
		@updateSphericalCoords!

	-- multiply vector by a scalar
	scale: ( scale ) =>
		@x *= scale
		@y *= scale
		@z *= scale
		@updateSphericalCoords!

	-- add some length to the vector without changing its direction
	addLength: ( length = @r ) =>
		originalR = @r
		@r += length
		@updateCartesianFromRadius @r / originalR

	addAzimuth: ( angle = @a ) =>
		@a = ( @a + angle ) % ( 2 * math.pi ) -- keep it bounded
		@updateCartesianFromAzimuth!

	-- turn current vector into a unit vector
	unitize: =>
		@updateCartesianFromRadius 1/@r - radius is known
		@r = 1

	-- Getters
	-- Get length from Cartesian coordinates. Only really needed to initially calculate r
	getLength: =>
		math.sqrt @x^2 + @y^2 + @z^2

	-- dot product of another vector and this one. This returns a scalar.
	dotProduct: ( vector ) =>
		math.sqrt @x * vector.x + @y * vector.y + @z * vector.z

	-- instance variables are public by default so no getters are needed.

class Drawing
	new: ( startingPoint ) =>
		with startingPoint
			@raw = \toString!
			-- don't forget that tables ( and therefore objects ) are passed as references
			-- use a comprehension to copy the table cleanly
			@min = { dim, val for dim, val in pairs .min }
			@max = { dim, val for dim, val in pairs .max }
			@lastType = startingPoint.type -- ALWAYS start on a point
		@components = {}
		table.insert @components, startingPoint

	addComponent: ( component ) =>
		@lastType = @components[ #@components ].type
		table.insert @components, component
		@updateBounds component
		@appendRaw component

	updateBounds: ( component ) => -- bounds = {min:Point, max:Point}
		with component
			for _,dim in ipairs { 'x', 'y' } -- should probably unroll this loop for MASSIVE SPEED ADVANTAGE
				if .min[ dim ] < @min[ dim ]
					@min[ dim ] = .min[ dim ]
				if .max[ dim ] > @max[ dim ]
					@max[ dim ] = .max[ dim ]
	appendRaw: ( component ) =>
		with component
			if .type == @lastType
				@raw ..= ' ' .. \toString!
			else
				@raw ..= ' ' .. .type .. ' ' .. \toString!

class Point
	@type = 'm'
	new: ( x, y ) =>
		@x, @y = x, y
		@min = @ -- min and max are just the point itself
		@max = @

	toString: =>
		return ( "%d %d" )\format @x, @y -- floating point locations are still nonstandard hacks

class Line
	@type = 'l'
	new: ( x1, y1, x2, y2 ) =>
		@start = Point x1, y1
		@stop = Point x2, y2

		with math -- to be honest, I worry that these are somewhat confusing
			@min = Point .min( x1, x2 ), .max( y1, y2 )
			@max = Point .max( x1, x2 ), .max( y1, y2 )

	toString: =>
		return ( "%d %d" )\format @stop.x, @stop.y

class CubicBezier
	@type = 'b' -- lol major league baseball
	new: ( x1, y1, x2, y2, x3, y3, x4, y4 ) =>
		@start = Point x1, y1
		@offCurve1 = Point x2, y2
		@offCurve2 = Point x3, y3
		@stop = Point x4, y4
		@min, @max = @calculateBounds!

	calculateBounds: =>
		f = ( t, i ) ->
			return ( 1 - t ) ^ 3 * @start[ i ] + 3 * ( 1 - t ) ^ 2 * t * @offCurve1[ i ] + 3 * ( 1 - t ) * t ^ 2 * @offCurve2[ i ] + t ^ 3 * @stop[ i ]

		bounds = {
			x: { @start.x, @stop.x },
			y: { @start.y, @stop.y }
		}

		for _,i in ipairs { 'x', 'y' }
			b = 6 * @start[ i ] - 12 * @offCurve1[ i ] + 6 * @offCurve2[ i ]
			a = -3 * @start[ i ] + 9 * @offCurve1[ i ] - 9 * @offCurve2[ i ] + 3 * @stop[ i ]
			c = 3 * @offCurve1[ i ] - 3 * @start[ i ]

			if a == 0
				if b == 0
					continue
				t = -c / b
				if 0 < t and t < 1
					table.insert bounds[ i ], f( t, i ) -- f( t, i ) doesn't need parentheses, but it looks cleaner
				continue

			ar = 1 / a -- get away with performing division only once because LEET SPEED HAX
			b2a = b * ar * 0.5 -- quadratic equation: -b/2a +/- sqrt((b/2a)^2 - c/a)
			discriminant =	b2a ^ 2 - c * ar
			if discriminant < 0
				continue

			t1 = -b2a + math.sqrt discriminant
			t2 = -b2a - math.sqrt discriminant
			if 0 < t1 and t1 < 1
				table.insert bounds[ i ], f( t1, i )
			if 0 < t2 and t2 < 1
				table.insert bounds[ i ], f( t2, i )

		return Point( math.min( unpack bounds.x ), math.min( unpack bounds.y ) ), Point( math.max( unpack bounds.x ), math.max( unpack bounds.y ) )

	toString: =>
		return ( "%d %d %d %d %d %d" )\format @offCurve1.x, @offCurve1.y, @offCurve2.x, @offCurve2.y, @stop.x, @stop.y

calculateDrawingBounds = ( drawingString ) ->
	local drawing
	previousCoords = {}
	drawingString = drawingString\gsub "^m ([%-%d]+) ([%-%d]+) *", ( ix, iy ) ->
		ix, iy = tonumber( ix ), tonumber( iy )
		drawing = Drawing Point ix, iy
		with previousCoords
			.x, .y = ix, iy
		""
	unless drawing
		error "Drawing doesn't start with a point. I don't know how to handle this."

	point = () ->
		success = false
		drawingString = drawingString\gsub "^([%-%d]+) *([%-%d]+) *", ( x, y ) ->
			success = true
			x, y = tonumber( x ), tonumber( y )
			drawing\addComponent Point x, y
			with previousCoords
				.x, .y = x, y
			"" -- dem implicit returns
		success

	linear = () ->
		success = false
		drawingString = drawingString\gsub "^([%-%d]+) *([%-%d]+) *", ( x2, y2 ) ->
			-- aegisub.log 4,'linear: %s %s\n', x2, y2
			success = true
			x2, y2 = tonumber( x2 ), tonumber( y2 )
			with previousCoords
				drawing\addComponent Line .x, .y, x2, y2
				.x, .y = x2, y2
			""
		success

	cubic = () ->
		success = false
		drawingString = drawingString\gsub "^([%-%d]+) *([%-%d]+) *([%-%d]+) *([%-%d]+) *([%-%d]+) *([%-%d]+) *", ( ox1, oy1, ox2, oy2, x2, y2 ) ->
			-- perhaps more elegant
			-- points = {...}
			-- for i,v in ipairs points
				-- points[ i ] = tonumber v
			success = true
			ox1, oy1, ox2, oy2, x2, y2 = tonumber( ox1 ), tonumber( oy1 ), tonumber( ox2 ), tonumber( oy2 ), tonumber( x2 ), tonumber( y2 )
			with previousCoords
				drawing\addComponent CubicBezier .x, .y, ox1, oy1, ox2, oy2, x2, y2
				.x, .y = x2, y2
			""
		success

	funcs = { m: point, l: linear, b: cubic }
	command = 'm'
	while true
		drawingString = drawingString\gsub "^([mlb]) ", ( kind ) -> -- only care about these 3
			command = kind
			""
		unless funcs[ command ]!
			break

	with drawing
		print ( "min: (%g, %g), max: (%g, %g)" )\format .min.x, .min.y, .max.x, .max.y

-- calculateDrawingBounds ...


-- Things that affect the line extents:
-- 	fscx, fscy, bord, shad, fn, fs, blur, fax, fay, frx, fry (anything I'm forgett?)
multilineExtents = ( line ) ->
	height, width, desc, ext = 0, 0, 0, 0

	with line.styleref
		.scale_y = 100 -- line.yscl
		.scale_x = 100 -- line.xscl

		.fontname = line.fn or .fontname -- avoid setting to nil
		.fontsize = line.fs or .fontsize

	splitTable = lf\split line.text_stripped
	for i, v in ipairs splitTable
		w, h, d, e = aegisub.text_extents line.styleref, v
		height = height + h + d -- each line includes descent
		if w > width
			width = w
		desc, ext = d, e

	height = height - desc -- subtract last descender off
	return width, height, desc, ext


