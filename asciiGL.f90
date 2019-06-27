! hello.f90
module asciiGL

    use :: ncurses
    implicit none

    real, parameter :: pi = 3.1415926536

    ! System parameters
    real :: moveSpeed = 0.02, rotSpeed = 0.02
    real, parameter :: ez = 60
    real, parameter :: xScale = 2.0
    real, parameter :: nearDistance = 0.5
    integer, parameter :: pointDensityScale = 2, pointDensityLimit = 120
    real, parameter :: lineOffset = 0.05
    integer, parameter :: maxObjects = 50

    private :: ez, xScale, nearDistance, pointDensityLimit, pointDensityScale
    private :: lineOffset, maxObjects, moveSpeed, rotSpeed

    type tri
        real, dimension(3, 3) :: verts
        real, dimension(3) :: norm
        real, dimension(3) :: median
    end type

    type renderObject
        integer :: numTris = 0
        type(tri), allocatable, dimension(:) :: tris
        real, dimension(3) :: centre = (/ 0, 0, 0 /), rot = (/ 0, 0, 0 /)
        character :: fillChar = ".", edgeChar = "/"
        integer :: fillCol = 0, edgeCol = 0
    end type

    interface str
        module procedure vector_to_string, real_to_string, int_to_string
    end interface

    ! Global vars
    real, dimension(3) :: cameraPos, cameraDir, orbitPos = (/ 0.0, 0.0, 0.0 /)
    logical :: wasdMoveEnabled = .false., arrowRotEnabled = .false., orbitEnabled = .false., flightEnabled = .false.
    logical :: debugEnabled = .false.
    type(renderObject), dimension(maxObjects) :: objs
    integer :: numObjs = 0, transformations = 0,  screenWidth = 0, screenHeight = 0, frames
    real :: halfW = 0, halfH = 0, fps, ms, orbitDistance = 3
    character :: lastChar = " "
    real, dimension(:, :), allocatable :: zBuffer
    character, dimension(:, :), allocatable :: buffer
    integer, dimension(:, :), allocatable :: colBuffer

    private :: pi, cameraPos, cameraDir, orbitPos, wasdMoveEnabled, arrowRotEnabled
    private :: objs, numObjs, transformations, screenWidth, screenHeight
    private :: halfW, halfH, lastChar, zBuffer, buffer

    private :: fill_tri, charFromAngle, get_length_needed, calc_medians

contains

    subroutine start()

        integer :: e
        logical*1 :: l

        ! Set up the screen
        stdscr = initscr()
        e = curs_set(0)
        e = start_color()
        e = noecho()

        ! Make it non-blocking
        l = .true.
        e = nodelay(stdscr, l)
        call wtimeout(stdscr, 2)

        ! Get the initial screen size
        call getmaxyx(stdscr, screenHeight, screenWidth)
        halfW = screenWidth / 2
        halfH = screenHeight / 2

        ! Set up the screen buffers
        allocate(buffer(0:screenWidth, 0:screenHeight))
        allocate(colBuffer(0:screenWidth, 0:screenHeight))
        buffer = " "
        colBuffer = 0

        ! Set up the z buffer (in reality it's actually a y buffer, but sticking with the terminology)
        allocate(zBuffer(0:screenWidth, 0:screenHeight))
        zBuffer = 300000.0

        e = attron(A_BOLD)

        ! Set up some color pairs
        e = init_pair(1 _c_short, COLOR_RED, COLOR_BLACK)
        e = init_pair(2 _c_short, COLOR_GREEN, COLOR_BLACK)
        e = init_pair(3 _c_short, COLOR_BLUE, COLOR_BLACK)
        e = init_pair(4 _c_short, COLOR_MAGENTA, COLOR_BLACK)
        e = init_pair(5 _c_short, COLOR_YELLOW, COLOR_BLACK)
        e = init_pair(6 _c_short, COLOR_CYAN, COLOR_BLACK)

    end subroutine

    subroutine set_interactivity(val)

        character(*), intent(in) :: val

        select case (val)
            case ("none")
                wasdMoveEnabled = .false.
                arrowRotEnabled = .false.
                flightEnabled = .false.
                orbitEnabled = .false.
            case ("walk")
                wasdMoveEnabled = .true.
                arrowRotEnabled = .true.
                flightEnabled = .false.
                orbitEnabled = .false.
            case ("fly")
                wasdMoveEnabled = .true.
                arrowRotEnabled = .true.
                flightEnabled = .true.
                orbitEnabled = .false.
            case ("orbit")
                wasdMoveEnabled = .true.
                arrowRotEnabled = .true.
                flightEnabled = .false.
                orbitEnabled = .true.
            case default
                wasdMoveEnabled = .false.
                arrowRotEnabled = .false.
                flightEnabled = .false.
                orbitEnabled = .false.
        end select

    end subroutine

    subroutine set_debug(val)
        logical, intent(in) :: val
        debugEnabled = val
    end subroutine

    function get_camera_pos()
        real, dimension(3) :: get_camera_pos
        get_camera_pos = cameraPos
    end function
    subroutine set_camera_pos(p)
        real, dimension(3), intent(in) :: p
        cameraPos = p
    end subroutine

    function get_camera_rot()
        real, dimension(3) :: get_camera_rot
        get_camera_rot = cameraDir
    end function
    subroutine set_camera_rot(p)
        real, dimension(3), intent(in) :: p
        cameraDir = p
        if (orbitEnabled) call update_orbit()
    end subroutine

    function get_object_pos(index)
        integer, intent(in) :: index
        real, dimension(3) :: get_object_pos
        get_object_pos = objs(index)%centre
    end function
    subroutine set_object_pos(index, p)
        real, dimension(3), intent(in) :: p
        integer, intent(in) :: index
        real, dimension(3) :: delta
        delta = p - objs(index)%centre
        call translate_object_dir(objs(index), delta)
    end subroutine
    subroutine translate_object(index, p)
        real, dimension(3), intent(in) :: p
        integer, intent(in) :: index
        call translate_object_dir(objs(index), p)
    end subroutine

    function get_object_rot(index)
        real, dimension(3) :: get_object_rot
        integer, intent(in) :: index
        get_object_rot = objs(index)%rot
    end function
    subroutine set_object_rot(index, p)
        real, dimension(3), intent(in) :: p
        integer, intent(in) :: index
        real, dimension(3) :: delta
        delta = p - objs(index)%rot
        call rotate_object_dir(objs(index), delta)
    end subroutine
    subroutine rotate_object(index, p)
        real, dimension(3), intent(in) :: p
        integer, intent(in) :: index
        call rotate_object_dir(objs(index), p)
    end subroutine

    subroutine set_orbit_object(objIndex)
        integer, intent(in) :: objIndex
        orbitPos = objs(objIndex)%centre
        call update_orbit()
    end subroutine

    subroutine set_orbit_pivot(p)
        real, dimension(3), intent(in) :: p
        orbitPos = p
        call update_orbit()
    end subroutine

    subroutine set_orbit_distance(p)
        real, intent(in) :: p
        orbitDistance = p
        call update_orbit()
    end subroutine

    ! Rotate coordinates about the origin using camera rotation
    function rotate_coords(pos, v)

        real, dimension(3), intent(in) :: pos, v
        real, dimension(3) :: rotate_coords
        real, dimension(3,3) :: xRot, yRot, zRot

        transformations = transformations + 1

        xRot = reshape((/ 1.0,                    0.0,                    0.0,                &
                        & 0.0,                    cos(v(1)),      -sin(v(1)),  &
                        & 0.0,                    sin(v(1)),     cos(v(1))   /), shape(xRot))

        yRot = reshape((/ cos(v(2)),      0.0,                    sin(v(2)), &
                        & 0.0,                    1.0,                    0.0,                &
                        & -sin(cameraDir(2)),      0.0,                    cos(v(2))   /), shape(yRot))

        zRot = reshape((/ cos(v(3)),     -sin(v(3)),     0.0,                &
                        & sin(v(3)),    cos(v(3)),     0.0,                &
                        & 0.0,                    0.0,                    1.0                 /), shape(zRot))

        rotate_coords = matmul(xRot, matmul(zRot, matmul(yRot, pos)))

    end function

    ! Go from world space to camera space
    function transform_coords(pos)

        real, dimension(3), intent(in) :: pos
        real, dimension(3) :: transform_coords

        real, dimension(3) :: p

        ! Translate bu camera position so rotating about the origin is the same as rotating about the camera
        p(1) = pos(1) - cameraPos(1)
        p(2) = pos(2) - cameraPos(2)
        p(3) = pos(3) - cameraPos(3)

        ! Rotate about the origin
        transform_coords = rotate_coords(p, cameraDir)

    end function

    ! Project 3d coords onto 2d plane
    function project_coords(pos)

        real, dimension(3), intent(in) :: pos
        integer, dimension(2) :: project_coords

        project_coords(1) = nint(xScale * (ez * pos(1) / pos(2)) + halfW)
        project_coords(2) = nint(-ez * pos(3) / pos(2) + halfH)

    end function

    ! Draw all of the triangles
    subroutine render()

        integer :: i, j
        real, dimension(3) :: trans, toCam, a, b
        real :: angle

        integer :: trisDrawn

        ! Reset the buffers
        zBuffer = 300000.0
        buffer(:, :) = " "

        trisDrawn = 0
        transformations = 0

        ! Go through all objects
        do i=1, numObjs

            ! For each primitive triangle
            do j=1, objs(i)%numTris

                ! Only draw if the normal is facing away from the player
                trans = rotate_coords(objs(i)%tris(j)%norm, cameraDir)
                toCam = transform_coords(objs(i)%tris(j)%median)
                angle = acos(DOT_PRODUCT(trans, toCam / norm2(toCam)))

                if (abs(angle) >= pi / 2) then

                    trisDrawn = trisDrawn + 1

                    ! Fill triangle using zBuffer
                    call fill_tri(objs(i)%tris(j), objs(i)%fillChar, objs(i)%fillCol)

                    ! Add the edges using the zBuffer (a and b prevent array temporaries)
                    a = objs(i)%tris(j)%verts(1,:)
                    b = objs(i)%tris(j)%verts(2,:)
                    call draw_line_3d(a, b, objs(i)%edgeChar, objs(i)%edgeCol, dz=lineOffset)
                    a = objs(i)%tris(j)%verts(2,:)
                    b = objs(i)%tris(j)%verts(3,:)
                    call draw_line_3d(a, b, objs(i)%edgeChar, objs(i)%edgeCol, dz=lineOffset)
                    a = objs(i)%tris(j)%verts(3,:)
                    b = objs(i)%tris(j)%verts(1,:)
                    call draw_line_3d(a, b, objs(i)%edgeChar, objs(i)%edgeCol, dz=lineOffset)

                end if

            end do

        end do

        ! Draw debug ui stuff
        if (debugEnabled) then


            call fill_box_2d(2, 1, 47, 6, " ")
            call draw_box_2d(2, 1, 47, 6)
            call draw_string_2d(4, 2, "  tris drawn: " // str(trisDrawn))
            call draw_string_2d(23, 2, "transforms: " // str(transformations))
            call draw_string_2d(4, 3, "  last key: " // lastChar)
            call draw_string_2d(23, 3, "fps: " // str(fps))
            call draw_string_2d(4, 4, "  rotation: " // str(cameraDir))
            call draw_string_2d(4, 5, "  position: " // str(cameraPos))

        end if

    end subroutine

    subroutine draw()

        integer :: i, j, e, ms1
        integer,dimension(8) :: t

        do i=1, screenWidth
            do j=1, screenHeight
                e = attron(COLOR_PAIR(colBuffer(i, j)))                           ! Enable attribute.
                e = mvaddch(j-1, i-1, ichar(buffer(i, j)))
                e = attroff(COLOR_PAIR(colBuffer(i, j)))                           ! Enable attribute.
            end do
        end do

        ! Check if it's been more than a second, if so calculate fps
        frames = frames + 1
        call date_and_time(values = t)
        ms1 = (t(5)*3600 + t(6)*60 + t(7))*1000 + t(8)
        if (ms1 - ms > 1000) then
            fps = frames
            ms = ms1
            frames = 0
        end if

    end subroutine

    subroutine draw_box_2d(x1, y1, x2, y2)

        integer, intent(in) :: x1, y1, x2, y2

        call draw_line_2d(x1, y1, x1, y2)
        call draw_line_2d(x2, y1, x2, y2)
        call draw_line_2d(x1, y1, x2, y1)
        call draw_line_2d(x1, y2, x2, y2)

    end subroutine

    subroutine fill_box_2d(x1, y1, x2, y2, char)

        integer, intent(in) :: x1, y1, x2, y2
        character, intent(in) :: char
        integer :: i

        do i = y1, y2
            call draw_line_2d(x1, i, x2, i, char)
        end do

    end subroutine

    ! Fill triangle using zBuffer
    subroutine fill_tri(tIn, char, col)

        type(tri), intent(in) :: tIn
        type(tri) :: t
        character, intent(in) :: char
        integer, intent(in) :: col

        integer :: i, pointDensity
        real :: highestZVal, lowestZVal, middleZVal, deltaZ, z, dh, dl, s
        integer :: highestZ, lowestZ, middleZ
        real, dimension(3) :: a, b, t1, t2, highV, lowV

        t = tIn

        highestZ = maxloc((/ t%verts(1,3), t%verts(2,3), t%verts(3,3) /), 1)
        lowestZ = minloc((/ t%verts(1,3), t%verts(2,3), t%verts(3,3) /), 1)

        ! Make work for flat surfaces
        if (highestZ == lowestZ) then

            highestZ = 1
            middleZ = 2
            lowestZ = 3
            t%verts(highestZ, 3) = t%verts(highestZ, 3) + 0.01
            t%verts(middleZ, 3) = t%verts(middleZ, 3) + 0.005

        else

            middleZ = 6 - highestZ - lowestZ

        end if

        highestZVal = t%verts(highestZ, 3)
        middleZVal = t%verts(middleZ, 3)
        lowestZVal = t%verts(lowestZ, 3)

        ! Used to prevent "an array temporary was created"
        lowV = t%verts(lowestZ, :)
        highV = t%verts(highestZ, :)

        t1 = transform_coords(highV)
        t2 = transform_coords(lowV)
        pointDensity = ceiling(pointDensityScale * norm2(real(project_coords(t1) - project_coords(t2))))
        if (pointDensity > pointDensityLimit) pointDensity = pointDensityLimit

        deltaZ = (highestZVal - lowestZVal) / pointDensity
        z = highestZVal

        ! Work downwards from the top point
        do i=0, pointDensity

            ! Get the intersection of this plane with longest z edge
            dh = t%verts(highestZ, 3) - z
            dl = t%verts(lowestZ, 3) - z
            s = -dh / (dh - dl)
            if (abs(s) > 1) cycle
            a(1) = t%verts(highestZ, 1) + s*(t%verts(highestZ, 1) - t%verts(lowestZ, 1))
            a(2) = t%verts(highestZ, 2) + s*(t%verts(highestZ, 2) - t%verts(lowestZ, 2))
            a(3) = z

            ! If above the middle z, use the high-middle edge
            if (z > middleZVal) then

                ! Get the intersection of this plane with the high-middle edge
                dh = t%verts(highestZ, 3) - z
                dl = t%verts(middleZ, 3) - z
                s = -dh / (dh - dl)
                if (abs(s) > 1) cycle
                b(1) = t%verts(highestZ, 1) + s*(t%verts(highestZ, 1) - t%verts(middleZ, 1))
                b(2) = t%verts(highestZ, 2) + s*(t%verts(highestZ, 2) - t%verts(middleZ, 2))
                b(3) = z

            else

                ! Get the intersection of this plane with the middle-low edge
                dh = t%verts(middleZ, 3) - z
                dl = t%verts(lowestZ, 3) - z
                s = -dh / (dh - dl)
                if (abs(s) > 1) cycle
                b(1) = t%verts(middleZ, 1) + s*(t%verts(middleZ, 1) - t%verts(lowestZ, 1))
                b(2) = t%verts(middleZ, 2) + s*(t%verts(middleZ, 2) - t%verts(lowestZ, 2))
                b(3) = z

            end if

            ! Draw the line
            call draw_line_3d(a, b, char, col)

            z = z - deltaZ

        end do

    end subroutine

    subroutine process_input(k)

        character, intent(in) :: k

        if (arrowRotEnabled) then

            if (.not. orbitEnabled) then

                select case (k)

                    case ("K") ! Left arrow
                        cameraDir(3) = cameraDir(3) + rotSpeed
                    case ("M") ! Right arrow
                        cameraDir(3) = cameraDir(3) - rotSpeed
                    case ("H") ! Up arrow
                        cameraDir(1) = cameraDir(1) + rotSpeed
                    case ("P") ! Down arrow
                        cameraDir(1) = cameraDir(1) - rotSpeed
                    case ("w")
                        cameraPos(1) = cameraPos(1) - sin(cameraDir(3))*moveSpeed
                        cameraPos(2) = cameraPos(2) + cos(cameraDir(3))*moveSpeed
                    case ("s")
                        cameraPos(1) = cameraPos(1) + sin(cameraDir(3))*moveSpeed
                        cameraPos(2) = cameraPos(2) - cos(cameraDir(3))*moveSpeed
                    case ("a")
                        cameraPos(1) = cameraPos(1) - cos(cameraDir(3))*moveSpeed
                        cameraPos(2) = cameraPos(2) - sin(cameraDir(3))*moveSpeed
                    case ("d")
                        cameraPos(1) = cameraPos(1) + cos(cameraDir(3))*moveSpeed
                        cameraPos(2) = cameraPos(2) + sin(cameraDir(3))*moveSpeed

                end select

                if (flightEnabled) then

                    select case (k)

                        case (" ")
                            cameraPos(3) = cameraPos(3) + moveSpeed
                        case ("c")
                            cameraPos(3) = cameraPos(3) - moveSpeed

                    end select

                end if

            else

                select case (k)

                    case ("K") ! Left arrow
                        cameraDir(3) = cameraDir(3) - rotSpeed
                        call update_orbit()

                    case ("M") ! Right arrow
                        cameraDir(3) = cameraDir(3) + rotSpeed
                        call update_orbit()

                    case ("H") ! Up arrow
                        cameraDir(1) = cameraDir(1) - rotSpeed
                        call update_orbit()

                    case ("P") ! Down arrow
                        cameraDir(1) = cameraDir(1) + rotSpeed
                        call update_orbit()

                    case ("w") ! Zoom in
                        orbitDistance = orbitDistance - moveSpeed
                        call update_orbit()

                    case ("s") ! Zoom out
                        orbitDistance = orbitDistance + moveSpeed
                        call update_orbit()

                end select

            end if

        end if

    end subroutine

    subroutine update_orbit()

        real, dimension(3) :: unit

        unit = (/ -sin(cameraDir(3))*cos(cameraDir(1)), cos(cameraDir(3))*cos(cameraDir(1)), sin(cameraDir(1)) /)
        cameraPos = orbitPos - orbitDistance*unit/norm2(unit)

    end subroutine

    subroutine get_input(k)

        character, intent(inout) :: k

        k = char(getch())
        call process_input(k)
        if (ichar(k) < 255 .and. ichar(k) > 0) lastChar = k

    end subroutine

    subroutine get_screen_size(x, y)

        integer, intent(inout) :: x, y
        x = screenWidth
        y = screenHeight

    end subroutine

    subroutine draw_string_2d(x, y, char)

        integer, intent(in) :: x, y
        character(*), intent(in) :: char
        integer :: i

        do i = 1, len_trim(char)
            buffer(x+i-1, y) = char(i:i)
        end do

    end subroutine

    subroutine draw_string_3d(v1, char, alwaysShow)

        real, dimension(3), intent(in) :: v1
        logical, intent(in), optional :: alwaysShow
        character(*), intent(in) :: char
        real, dimension(3) :: transformed1, transformed2
        real, dimension(3) :: clipped1, clipped2
        real :: angle2d, length3d, distance3d
        real, dimension(3) :: unitVector, v2
        integer, dimension(2) :: startCoords, endCoords, coords
        real :: da, db, s
        real :: zBufferMod

        v2 = v1 + (/ 1.0, 0.0, 0.0 /)

        transformed1 = transform_coords(v1)
        transformed2 = transform_coords(v2)
        clipped1 = transformed1
        clipped2 = transformed2

        ! 3D Clip against near plane
        if (transformed1(2) < nearDistance .and. transformed2(2) < nearDistance) then

            return

        else if (transformed1(2) < nearDistance) then

            da = transformed1(2) - nearDistance
            db = transformed2(2) - nearDistance
            s = da / (da - db)

            clipped1(1) = transformed1(1) + s*(transformed2(1)-transformed1(1))
            clipped1(2) = transformed1(2) + s*(transformed2(2)-transformed1(2))
            clipped1(3) = transformed1(3) + s*(transformed2(3)-transformed1(3))

        else if (transformed2(2) < nearDistance) then

            da = transformed1(2) - nearDistance
            db = transformed2(2) - nearDistance
            s = da / (da - db)

            clipped2(1) = transformed2(1) + s*(transformed2(1)-transformed1(1))
            clipped2(2) = transformed2(2) + s*(transformed2(2)-transformed1(2))
            clipped2(3) = transformed2(3) + s*(transformed2(3)-transformed1(3))

        end if

        length3d = sqrt((clipped2(1)-clipped1(1))**2 + (clipped2(2)-clipped1(2))**2 + (clipped2(3)-clipped1(3))**2)
        unitVector = (clipped2-clipped1) / norm2(clipped2-clipped1)

        startCoords = project_coords(clipped1)
        endCoords = project_coords(clipped2)

        angle2d = atan2(real(endCoords(2)-startCoords(2)), real(endCoords(1)-startCoords(1)))

        zBufferMod = 0
        if (present(alwaysShow) .and. alwaysShow) zBufferMod = 10000

        coords = project_coords(clipped1)
        distance3d = sqrt((transformed1(1))**2 + (transformed1(2))**2 + (transformed1(3))**2)

        ! 2D clipping
        if (coords(1) >= 0 .and. coords(1) <= screenWidth .and. coords(2) >= 0 .and. coords(2) <= screenHeight) then

            zBuffer(coords(1):coords(1)+len_trim(char), coords(2)) = distance3d - zBufferMod
            call draw_string_2d(coords(1), coords(2), char)
            return

        end if

    end subroutine

    function get_color_number(col)

        character(*), intent(in) :: col
        integer :: get_color_number

        select case (col)

            case ("white")
                get_color_number = 0
            case ("red")
                get_color_number = 1
            case ("green")
                get_color_number = 2
            case ("blue")
                get_color_number = 3
            case ("magenta")
                get_color_number = 4
            case ("yellow")
                get_color_number = 5
            case ("cyan")
                get_color_number = 6
            case default
                get_color_number = 0

        end select

    end function

    subroutine draw_line_2d(x1, y1, x2, y2, c)

        integer, intent(in) :: x1, y1, x2, y2
        character, intent(in), optional :: c
        integer :: length, i
        real :: angle
        character :: char

        angle = atan2(real(y2-y1), real(x2-x1))
        length = nint(sqrt(real((y2-y1)**2 + (x2-x1)**2)))

        if (present(c)) then
            char = c
        else
            char = charFromAngle(angle)
        end if

        do i = 0, length
            buffer(nint(x1+i*cos(angle)), nint(y1+i*sin(angle))) = char
        end do

    end subroutine

    subroutine draw_line_3d(v1, v2, c, col, dz)

        real, dimension(3), intent(in) :: v1, v2
        character(*), intent(in) :: c
        real, intent(in), optional :: dz
        integer, intent(in) :: col
        real :: zBufferMod
        real, dimension(3) :: transformed1, transformed2
        real, dimension(3) :: clipped1, clipped2
        integer :: i, pointDensity
        real :: angle2d, length3d, distance3d
        character :: char
        real :: deltaLength = 0.1
        real, dimension(3) :: pos, unitVector
        integer, dimension(2) :: startCoords, endCoords, coords
        real :: da, db, s

        if (present(dz)) then
            zBufferMod = dz
        else
            zBufferMod = 0
        end if

        transformed1 = transform_coords(v1)
        transformed2 = transform_coords(v2)
        clipped1 = transformed1
        clipped2 = transformed2

        ! 3D Clip against near plane
        if (transformed1(2) < nearDistance .and. transformed2(2) < nearDistance) then

            return

        else if (transformed1(2) < nearDistance) then

            da = transformed1(2) - nearDistance
            db = transformed2(2) - nearDistance
            s = da / (da - db)

            clipped1(1) = transformed1(1) + s*(transformed2(1)-transformed1(1))
            clipped1(2) = transformed1(2) + s*(transformed2(2)-transformed1(2))
            clipped1(3) = transformed1(3) + s*(transformed2(3)-transformed1(3))

        else if (transformed2(2) < nearDistance) then

            da = transformed1(2) - nearDistance
            db = transformed2(2) - nearDistance
            s = da / (da - db)

            clipped2(1) = transformed2(1) + s*(transformed2(1)-transformed1(1))
            clipped2(2) = transformed2(2) + s*(transformed2(2)-transformed1(2))
            clipped2(3) = transformed2(3) + s*(transformed2(3)-transformed1(3))

        end if

        length3d = sqrt((clipped2(1)-clipped1(1))**2 + (clipped2(2)-clipped1(2))**2 + (clipped2(3)-clipped1(3))**2)
        unitVector = (clipped2-clipped1) / norm2(clipped2-clipped1)

        startCoords = project_coords(clipped1)
        endCoords = project_coords(clipped2)

        pointDensity = ceiling(pointDensityScale*norm2(real(endCoords - startCoords)))
        if (pointDensity > pointDensityLimit) pointDensity = pointDensityLimit

        angle2d = atan2(real(endCoords(2)-startCoords(2)), real(endCoords(1)-startCoords(1)))

        if (c /= "/") then
            char = c
        else
            char = charFromAngle(angle2d)
        end if

        deltaLength = length3d / pointDensity
        pos = clipped1

        do i = 0, pointDensity

            distance3d = sqrt((pos(1))**2 + (pos(2))**2 + (pos(3))**2) - zBufferMod
            coords = project_coords(pos)

            ! 2D clipping
            if (coords(1) >= 0 .and. coords(1) <= screenWidth .and. coords(2) >= 0 .and. coords(2) <= screenHeight) then

                ! Check if it's closer than the last character drawn at this point
                if (distance3d < zBuffer(coords(1), coords(2))) then

                    zBuffer(coords(1), coords(2)) = distance3d
                    buffer(coords(1), coords(2)) = char
                    colBuffer(coords(1), coords(2)) = col

                end if

            end if

            pos = pos + unitVector*deltaLength

        end do

    end subroutine

    ! Return the best character to use for a line at a certain angle
    function charFromAngle(angle)

        character :: charFromAngle
        real, intent(in) :: angle

        select case (nint(angle*(180/pi)))

            case (-23 : 23)
                charFromAngle = "-"
            case (24 : 68)
                charFromAngle = "\"
            case (69 : 113)
                charFromAngle = "|"
            case (114 : 165)
                charFromAngle = "/"
            case (166 :)
                charFromAngle = "-"
            case (-68 : -24)
                charFromAngle = "/"
            case (-113 : -69)
                charFromAngle = "|"
            case (-165 : -114)
                charFromAngle = "\"
            case (: -166)
                charFromAngle = "-"

        end select

    end function

    ! Returns the numbers of characters an integer would take up (including minus sign)
    pure function get_length_needed(a)

        integer, intent(in) :: a
        integer :: get_length_needed

        if (a < 0) then
            get_length_needed = int(log(real(-a))/log(10.0))+2
        else
            get_length_needed = int(log(real(a))/log(10.0))+1
        end if

    end function

    function int_to_string(a)

        integer :: a
        character(get_length_needed(a)) :: int_to_string

        int_to_string = ""

        if (len(int_to_string) <= 0) then
            return
        end if

        write(int_to_string, "(I0)") a

    end function

    function real_to_string(a)

        real :: a
        character(8) :: real_to_string

        real_to_string = ""

        if (len(real_to_string) <= 0) then
            return
        end if

        write(real_to_string, "(F7.2)") a

    end function

    function vector_to_string(a)

        real, dimension(3), intent(in) :: a
        character(30) :: vector_to_string

        vector_to_string(1:12) = "(" // str(a(1)) // ", "
        vector_to_string(12:21) = str(a(2)) // ", "
        vector_to_string(21:) = str(a(3)) // ")"

    end function

    ! Stop ncurses
    subroutine stop()

        integer :: e
        e = endwin()

    end subroutine

    ! Calculate the medians for all the tris in a render object
    subroutine calc_medians(obj)

        type(renderObject), intent(inout) :: obj

        integer :: i, j
        real, dimension(3) :: avg, objAvg

        objAvg = 0

        ! For each primitive triangle
        do i=1, obj%numTris

            avg = 0

            ! Sum each point
            do j=1, 3
                avg = avg + obj%tris(i)%verts(j,:)
            end do

            ! Divide by 3 to get the average
            obj%tris(i)%median = avg / 3.0

            ! Sum all of the triangle medians
            objAvg = objAvg + obj%tris(i)%median

        end do

        ! Calculate the mean point for the object
        obj%centre = objAvg / obj%numTris

    end subroutine

    ! Create a cube at a certain position with a certain size
    subroutine add_cube(pos, s, index, edgeCol, edgeChar, fillCol, fillChar)

        real, dimension(3), intent(in) :: pos, s
        integer, intent(inout), optional :: index
        real :: w, d, h, x, y, z
        character(*), intent(in), optional :: edgeChar, fillChar, fillCol, edgeCol

        ! Shorthand
        w = s(1) / 2.0
        d = s(2) / 2.0
        h = s(3) / 2.0
        x = pos(1)
        y = pos(2)
        z = pos(3)

        numObjs = numObjs + 1

        if (present(index)) then
            index  = numObjs
        end if

        if (present(edgeCol)) objs(numObjs)%edgeCol = get_color_number(edgeCol)
        if (present(edgeChar)) objs(numObjs)%edgeChar = edgeChar
        if (present(fillCol)) objs(numObjs)%fillCol = get_color_number(fillCol)
        if (present(fillChar)) objs(numObjs)%fillChar = fillChar

        objs(numObjs)%numTris = 12
        objs(numObjs)%centre = pos
        allocate(objs(numObjs)%tris(objs(numObjs)%numTris))

        ! Closest face
        objs(numObjs)%tris(1)%verts(1,:) = (/ x+w, y-d, z-h /)
        objs(numObjs)%tris(1)%verts(2,:) = (/ x-w, y-d, z+h /)
        objs(numObjs)%tris(1)%verts(3,:) = (/ x-w, y-d, z-h /)
        objs(numObjs)%tris(2)%verts(1,:) = (/ x+w, y-d, z-h /)
        objs(numObjs)%tris(2)%verts(2,:) = (/ x-w, y-d, z+h /)
        objs(numObjs)%tris(2)%verts(3,:) = (/ x+w, y-d, z+h /)
        objs(numObjs)%tris(1)%norm(:) = (/ 0, -1, 0 /)
        objs(numObjs)%tris(2)%norm(:) = (/ 0, -1, 0 /)

        ! Furthest face
        objs(numObjs)%tris(3)%verts(1,:) = (/ x+w, y+d, z-h /)
        objs(numObjs)%tris(3)%verts(2,:) = (/ x-w, y+d, z+h /)
        objs(numObjs)%tris(3)%verts(3,:) = (/ x-w, y+d, z-h /)
        objs(numObjs)%tris(4)%verts(1,:) = (/ x+w, y+d, z-h /)
        objs(numObjs)%tris(4)%verts(2,:) = (/ x-w, y+d, z+h /)
        objs(numObjs)%tris(4)%verts(3,:) = (/ x+w, y+d, z+h /)
        objs(numObjs)%tris(3)%norm(:) = (/ 0, 1, 0 /)
        objs(numObjs)%tris(4)%norm(:) = (/ 0, 1, 0 /)

        ! Left face
        objs(numObjs)%tris(5)%verts(1,:) = (/ x-w, y-d, z-h /)
        objs(numObjs)%tris(5)%verts(2,:) = (/ x-w, y+d, z+h /)
        objs(numObjs)%tris(5)%verts(3,:) = (/ x-w, y-d, z+h /)
        objs(numObjs)%tris(6)%verts(1,:) = (/ x-w, y-d, z-h /)
        objs(numObjs)%tris(6)%verts(2,:) = (/ x-w, y+d, z+h /)
        objs(numObjs)%tris(6)%verts(3,:) = (/ x-w, y+d, z-h /)
        objs(numObjs)%tris(5)%norm(:) = (/ -1, 0, 0 /)
        objs(numObjs)%tris(6)%norm(:) = (/ -1, 0, 0 /)

        ! Right face
        objs(numObjs)%tris(7)%verts(1,:) = (/ x+w, y-d, z-h /)
        objs(numObjs)%tris(7)%verts(2,:) = (/ x+w, y+d, z+h /)
        objs(numObjs)%tris(7)%verts(3,:) = (/ x+w, y-d, z+h /)
        objs(numObjs)%tris(8)%verts(1,:) = (/ x+w, y-d, z-h /)
        objs(numObjs)%tris(8)%verts(2,:) = (/ x+w, y+d, z+h /)
        objs(numObjs)%tris(8)%verts(3,:) = (/ x+w, y+d, z-h /)
        objs(numObjs)%tris(7)%norm(:) = (/ 1, 0, 0 /)
        objs(numObjs)%tris(8)%norm(:) = (/ 1, 0, 0 /)

        ! Top face
        objs(numObjs)%tris(9)%verts(1,:) = (/ x-w, y-d, z+h /)
        objs(numObjs)%tris(9)%verts(2,:) = (/ x+w, y+d, z+h /)
        objs(numObjs)%tris(9)%verts(3,:) = (/ x-w, y+d, z+h /)
        objs(numObjs)%tris(10)%verts(1,:) = (/ x-w, y-d, z+h /)
        objs(numObjs)%tris(10)%verts(2,:) = (/ x+w, y+d, z+h /)
        objs(numObjs)%tris(10)%verts(3,:) = (/ x+w, y-d, z+h /)
        objs(numObjs)%tris(9)%norm(:) = (/ 0, 0, 1 /)
        objs(numObjs)%tris(10)%norm(:) = (/ 0, 0, 1 /)

        ! Bottom face
        objs(numObjs)%tris(11)%verts(1,:) = (/ x-w, y-d, z-h /)
        objs(numObjs)%tris(11)%verts(2,:) = (/ x+w, y+d, z-h /)
        objs(numObjs)%tris(11)%verts(3,:) = (/ x-w, y+d, z-h /)
        objs(numObjs)%tris(12)%verts(1,:) = (/ x-w, y-d, z-h /)
        objs(numObjs)%tris(12)%verts(2,:) = (/ x+w, y+d, z-h /)
        objs(numObjs)%tris(12)%verts(3,:) = (/ x+w, y-d, z-h /)
        objs(numObjs)%tris(11)%norm(:) = (/ 0, 0, -1 /)
        objs(numObjs)%tris(12)%norm(:) = (/ 0, 0, -1 /)

        call calc_medians(objs(numObjs))

    end subroutine

    ! Load a model from an STL file at a certain position
    subroutine add_stl(filename, pos, index, edgeCol, edgeChar, fillCol, fillChar)

        real, dimension(3), intent(in) :: pos
        character(*), intent(in) :: filename
        character(*), intent(in), optional :: edgeChar, fillChar, fillCol, edgeCol
        integer, intent(inout), optional :: index
        integer*1, dimension(80) :: header
        integer*4 :: num
        integer*2 :: count
        integer :: i
        real*4, dimension(3) :: vector

        numObjs = numObjs + 1

        if (present(index)) then
            index  = numObjs
        end if

        if (present(edgeCol)) objs(numObjs)%edgeCol = get_color_number(edgeCol)
        if (present(edgeChar)) objs(numObjs)%edgeChar = edgeChar
        if (present(fillCol)) objs(numObjs)%fillCol = get_color_number(fillCol)
        if (present(fillChar)) objs(numObjs)%fillChar = fillChar

        ! Load the file
        open (15, file = filename, form='unformatted', access='stream')

        read (15) header
        read (15) num

        objs(numObjs)%numTris = num
        objs(numObjs)%centre = (/ 0, 0, 0 /)
        allocate(objs(numObjs)%tris(objs(numObjs)%numTris))

        do i = 1, num

            read (15) vector
            objs(numObjs)%tris(i)%norm(:) = vector

            read (15) vector
            objs(numObjs)%tris(i)%verts(1, :) = vector

            read (15) vector
            objs(numObjs)%tris(i)%verts(2, :) = vector

            read (15) vector
            objs(numObjs)%tris(i)%verts(3, :) = vector

            read (15) count

        end do

        close(15)

        call translate_object_dir(objs(numObjs), pos)
        call calc_medians(objs(numObjs))

    end subroutine

    ! Translate an object by a vector
    subroutine translate_object_dir(obj, v)

        type(renderObject), intent(inout) :: obj
        real, dimension(3), intent(in) :: v

        integer :: i, j

        do i=1, obj%numTris
            do j=1, 3
                obj%tris(i)%verts(j,:) = obj%tris(i)%verts(j,:) + v
            end do
            obj%tris(i)%median(:) = obj%tris(i)%median(:) + v
        end do

        obj%centre = obj%centre + v

    end subroutine

    ! Rotate an object by a vector
    subroutine rotate_object_dir(obj, v)

        type(renderObject), intent(inout) :: obj
        real, dimension(3), intent(in) :: v

        integer :: i, j

        do i=1, obj%numTris

            do j=1, 3

                ! Take away the objects central position, rotate (about the origin) and then add it back
                obj%tris(i)%verts(j,:) = rotate_coords(obj%tris(i)%verts(j,:) - obj%centre, v) + obj%centre

            end do

            ! Rotate the normals
            obj%tris(i)%norm(:) = rotate_coords(obj%tris(i)%norm(:), v)

        end do

        obj%rot = obj%rot + v

    end subroutine

end module
