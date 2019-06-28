
### Documentation

###### set_interactivity
```Fortran
call set_interactivity(type)
```
```Fortran
character(*), intent(in) :: type
 ```
This sets the kind of control the user should have over the camera, here __type__ is one of:
 - "none" (user cannot move the camera)
 - "walk" (wasd to move, arrow keys to turn)
 - "fly" (same as walk but can use space/c to go up/down)
 - "orbit" (camera orbits a point, arrow keys to turn, w/s to zoom)

<br>

###### add_stl
```Fortran
call add_stl(filename, pos, index, edgeCol, edgeChar, fillCol, fillChar)
```
```Fortran
real, dimension(3), intent(in) :: pos
character(*), intent(in) :: filename
character(*), intent(in), optional :: edgeChar, fillChar, fillCol, edgeCol
integer, intent(inout), optional :: index
 ```
This loads an stl file and creates an object using it, starting at position __pos__ and storing the ID of the newly generated object in __index__. The __edgeChar__ and __fillChar__ are the characters used to fill the respective sections, whilst __edgeCol__ and __fillCol__ are the colors ("white", "red", "green", "blue", "magenta", "yellow", "cyan") for those sections.

<br>

###### add_cube
```Fortran
call add_cube(pos, s, index, edgeCol, edgeChar, fillCol, fillChar)
```
```Fortran
real, dimension(3), intent(in) :: pos, s
integer, intent(inout), optional :: index
character(*), intent(in), optional :: edgeChar, fillChar, fillCol, edgeCol
 ```
This creates a cube object with size __s__, starting at position __pos__ and storing the ID of the newly generated object in __index__. The __edgeChar__ and __fillChar__ are the characters used to fill the respective sections, whilst __edgeCol__ and __fillCol__ are the colors ("white", "red", "green", "blue", "magenta", "yellow", "cyan") for those sections.

<br>

###### start, stop
```Fortran
call start()
call stop()
```

Start or stop ncurses.

<br>

###### str
```Fortran
call str(vector)
call str(int)
call str(real)
```

Function to convert various data types to string, useful for UI.

<br>

###### draw_line_2d
```Fortran
call draw_line_2d(x1, y1, x2, y2, c)
```
```Fortran
integer, intent(in) :: x1, y1, x2, y2
character, intent(in), optional :: c
```

Draw a 2D line from (__x1__, __y1__) to (__x2__, __y2__), with character __c__. If c is omitted or "/" then the character is calculated based on the angle of the line.

<br>

###### draw_line_3d
```Fortran
call draw_line_3d(v1, v2, c, col, dz)
```
```Fortran
real, dimension(3), intent(in) :: v1, v2
character(*), intent(in) :: c
real, intent(in), optional :: dz
integer, intent(in) :: col
```

Draw a 2D line from __v1__ to __v2__ in 3D space, with character __c__. If c is omitted or "/" then the character is calculated based on the angle of the line. An optional small positive __dz__ is used to prioritize the character in the z-buffer (e.g. when lines should be drawn over fill).

<br>

###### draw_string_2d
```Fortran
call draw_string_2d(x, y, char)
```
```Fortran
integer, intent(in) :: x, y
character(*), intent(in) :: char
```

Draw the string given by __char__ at the location (__x__, __y__) on the screen.

<br>

###### draw_string_3d
```Fortran
call draw_string_3d(v1, char, alwaysShow)
```
```Fortran
real, dimension(3), intent(in) :: v1
logical, intent(in), optional :: alwaysShow
character(*), intent(in) :: char
```

Draw the string given by __char__ at the location __v1__ in 3d space. The string will always face right, but can be hidden by other things unless __alwaysShow__ is set to true.

<br>

###### get_screen_size
```Fortran
call get_screen_size(x, y)
```
```Fortran
integer, intent(inout) :: x, y
```

Puts the size of the terminal into __x__ and __y__.

<br>

###### get_input
```Fortran
call get_input(k)
```
```Fortran
character, intent(inout) :: k
```

Puts the latest key pressed __k__ and processes certain key presses depending on the interactivity.

<br>

###### draw_box_2d
```Fortran
call draw_box_2d(x1, y1, x2, y2)
```
```Fortran
integer, intent(in) :: x1, y1, x2, y2
```

Draws a box to the screen from (__x1__, __y1__) to (__x2__, __y2__).

<br>

###### fill_box_2d
```Fortran
fill_box_2d(x1, y1, x2, y2, char)
```
```Fortran
integer, intent(in) :: x1, y1, x2, y2
character, intent(in) :: char
```

Fills a box to the screen from (__x1__, __y1__) to (__x2__, __y2__) with __char__.

<br>

###### render, draw
```Fortran
render()
draw()
```

render() the 3D scene, saving it to a buffer. Can then draw() the buffer to the screen. The functions are separate so that UI elements can be added to the buffer after 3D rendering but before drawing.

<br>

###### set_orbit_distance
```Fortran
set_orbit_distance(dist)
```
```Fortran
real, intent(in) :: dist
```

Only used in orbit mode. Set the distance from the pivot that the camera should orbit.

###### set_orbit_pivot
```Fortran
set_orbit_pivot(piv)
```
```Fortran
real, dimension(3), intent(in) :: piv
```

Only used in orbit mode. Set the location of the pivot that the camera should orbit.

<br>

###### set_orbit_object
```Fortran
set_orbit_object(objIndex)
```
```Fortran
integer, intent(in) :: objIndex
```

Only used in orbit mode. Set the object the camera should orbit around. Note that this needs to be called again if the target moves.

<br>

###### rotation getters and setters
```Fortran
rotation = get_object_rot(index)
call set_object_rot(index, newRot)
call rotate_object(index, deltaRot)
```
```Fortran
real, dimension(3) :: get_object_rot
real, dimension(3), intent(in) :: deltaRot
real, dimension(3), intent(in) :: newRot
integer, intent(in) :: index
```

Get, set or change the rotation of an object.

<br>

###### position getters and setters
```Fortran
position = get_object_pos(index)
call set_object_pos(index, newPos)
call translate_object(index, deltaPos)
```
```Fortran
real, dimension(3) :: get_object_pos
real, dimension(3), intent(in) :: newPos
real, dimension(3), intent(in) :: deltaPos
integer, intent(in) :: index
```

Get, set or change the position of an object.

<br>

###### camera getters and setters
```Fortran
position = get_camera_pos()
call set_camera_pos(newPos)
rotation = get_camera_rot()
call set_camera_rot(newRot)
```
```Fortran
real, dimension(3) :: get_camera_pos
real, dimension(3), intent(in) :: newPos
real, dimension(3) :: get_camera_rot
real, dimension(3), intent(in) :: newRot
```

Get, set or change the position or rotation of the camera.

<br>

###### set_debug
```Fortran
set_debug(val)
```
```Fortran
logical, intent(in) :: val
```

Enable or disable debug mode. If enabled this displays some debug info in a box in the top left.
