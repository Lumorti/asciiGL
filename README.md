# asciiGL

asciiGL is a lightweight Fortran library allowing the rendering of 3D objects in the terminal using only text.

### Features
 - simple commands to create/transform objects
 - multiple camera control options such as linear WASD movement or an orbiting camera
 - can load and render stl files
 - efficient, since Fortran is very good at processing vectors/matrices
 - supports different colours and characters for the edge/fill of each object

### Dependencies

This requires ncurses, a c library used to draw text to the terminal. Fortran bindings are included as the files "macros.c" and "ncurses.f90", both written by John S. Urban and in the public domain.

To install ncurses on Ubuntu:
```bash
sudo apt install libncurses5-dev
```

To install on Windows using MinGW you need to install PDCurses, which can either be done manually or using MinGW-get.

### Compiling

A makefile is included along with an example, simply type:
```bash
make
```
in the directory and the run the resulting program with:
```bash
./a.out
```

On windows you'll need to use "mingw32-make.exe" instead

### Example Code (example.f90)

```Fortran
program example

    use asciiGL

    character :: userInput
    integer :: chairObject

    ! Initialise the screen
    call start()

    ! Set up the camera and allows the user to orbit using the arrows and zoom using w/s
    call set_interactivity("orbit")

    ! Add a chair model, saving the ID into chairObject
    call add_stl("chair.stl", (/ 0.0, 6.0, 0.0 /), chairObject, edgeCol="red")

    ! Orbit around the chair, setting the initial distance and angle
    call set_orbit_object(chairObject)
    call set_orbit_distance(8.0)
    call set_camera_rot((/ -0.48, 0.0, 0.0 /))

    do

        ! Render the 3D scene and draw to the screen
        call render()
        call draw()

        ! Get the user's key press (will also process inputs like wasd if enabled)
        call get_input(userInput)

        ! Make the chair spin
        call rotate_object(chairObject, (/ 0.0, 0.0, 0.005 /))

    end do

end program

```

### Documentation

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
