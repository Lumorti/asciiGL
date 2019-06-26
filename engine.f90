! hello.f90
module engine

    use :: ncurses
    implicit none

    real, parameter :: pi = 3.1415926536

    ! System parameters
    real :: moveSpeed = 0.02, rotSpeed = 0.02
    real, parameter :: ez = 60
    real, parameter :: xScale = 2.0
    real, parameter :: nearDistance = 0.5
    integer, parameter :: pointDensityScale = 1, pointDensityLimit = 35
    real, parameter :: lineOffset = 0.05
    integer, parameter :: maxObjects = 50
    integer, parameter :: maxInputs = 4

    private :: ez, xScale, nearDistance, pointDensityLimit, pointDensityScale
    private :: lineOffset, maxObjects, moveSpeed, rotSpeed, maxInputs

    type tri
        real, dimension(3, 3) :: verts
        real, dimension(3) :: norm
        real, dimension(3) :: median
    end type

    type renderObject
        integer :: numTris = 0
        type(tri), allocatable, dimension(:) :: tris
        real, dimension(3) :: centre = (/ 0, 0, 0 /), rot = (/ 0, 0, 0 /)
    end type

    interface str
        module procedure vector_to_string, real_to_string, int_to_string
    end interface

    ! Global vars
    real, dimension(3) :: cameraPos, cameraDir, orbitPos = (/ 0.0, 2.0, 0.0 /)
    logical :: wasdMoveEnabled = .false., arrowRotEnabled = .false., orbitEnabled = .false., flightEnabled = .false.
    type(renderObject), dimension(maxObjects) :: objs
    integer :: numObjs = 0, transformations = 0,  screenWidth = 0, screenHeight = 0, frames
    real :: halfW = 0, halfH = 0, fps, ms, orbitDistance = 3
    character :: lastChar = " "
    real, dimension(:, :), allocatable :: zBuffer
    character, dimension(:, :), allocatable :: buffer

    private :: pi, cameraPos, cameraDir, orbitPos, wasdMoveEnabled, arrowRotEnabled
    private :: objs, numObjs, transformations, screenWidth, screenHeight
    private :: halfW, halfH, lastChar, zBuffer, buffer

    private :: fill_tri, charFromAngle, get_length_needed, calc_medians

contains

    ! Function to sleep for a certain number of milliseconds
    subroutine sleep_milli(milli)

        integer, intent(in) :: milli
        integer,dimension(8) :: t
        integer :: s1, s2, ms1, ms2

        ! Get start time
        call date_and_time(values = t)
        ms1 = (t(5)*3600 + t(6)*60 + t(7))*1000 + t(8)

        ! Keep looping until the difference in milliseconds is the correct number
        do

            call date_and_time(values = t)
            ms2 = (t(5)*3600 + t(6)*60 + t(7))*1000 + t(8)
            if (ms2 - ms1 >= milli) exit

        end do

    end subroutine

    subroutine start()

        integer :: e

        ! Set up the screen
        stdscr = initscr()
        e = curs_set(0)
        e = start_color()
        e = noecho()

        ! Get the initial screen size
        call getmaxyx(stdscr, screenHeight, screenWidth)
        halfW = screenWidth / 2
        halfH = screenHeight / 2

        ! Set up the screen buffer
        allocate(buffer(screenWidth, screenHeight))
        buffer = " "

        ! Set up the z buffer (in reality it's actually a y buffer, but sticking with the terminology)
        allocate(zBuffer(screenWidth, screenHeight))
        zBuffer = 300000.0

        ! Set up some color pairs
        e = init_pair(1_c_short, COLOR_WHITE, COLOR_BLACK)
        e = init_pair(2_c_short, COLOR_RED, COLOR_BLACK)
        e = init_pair(3_c_short, COLOR_GREEN, COLOR_BLACK)
        e = init_pair(4_c_short, COLOR_BLUE, COLOR_BLACK)

    end subroutine

    subroutine enable_real_time()

        logical*1 :: l
        integer :: e
        l = .true.

        e = nodelay(stdscr, l)
        call wtimeout(stdscr, 2)

    end subroutine

    subroutine set_wasd_movement(val)
        logical, intent(in) :: val
        wasdMoveEnabled = val
    end subroutine

    subroutine set_arrows_rotation(val)
        logical, intent(in) :: val
        arrowRotEnabled = val
    end subroutine

    subroutine set_orbit(val)
        logical, intent(in) :: val
        orbitEnabled = val
    end subroutine

    subroutine set_flight(val)
        logical, intent(in) :: val
        flightEnabled = val
    end subroutine

    ! Rotate coordinates about the origin using camera rotation
    function rotate_coords(pos)

        real, dimension(3), intent(in) :: pos
        real, dimension(3) :: rotate_coords
        real, dimension(3,3) :: xRot, yRot, zRot

        transformations = transformations + 1

        xRot = reshape((/ 1.0,                    0.0,                    0.0,                &
                        & 0.0,                    cos(cameraDir(1)),      -sin(cameraDir(1)),  &
                        & 0.0,                    sin(cameraDir(1)),     cos(cameraDir(1))   /), shape(xRot))

        yRot = reshape((/ cos(cameraDir(2)),      0.0,                    sin(cameraDir(2)), &
                        & 0.0,                    1.0,                    0.0,                &
                        & -sin(cameraDir(2)),      0.0,                    cos(cameraDir(2))   /), shape(yRot))

        zRot = reshape((/ cos(cameraDir(3)),     -sin(cameraDir(3)),     0.0,                &
                        & sin(cameraDir(3)),    cos(cameraDir(3)),     0.0,                &
                        & 0.0,                    0.0,                    1.0                 /), shape(zRot))

        rotate_coords = matmul(xRot, matmul(zRot, matmul(yRot, pos)))

    end function

    ! Go from world space to camera space
    function transform_coords(pos)

        real, dimension(3), intent(in) :: pos
        real, dimension(3) :: transform_coords

        real, dimension(3) :: p
        real, dimension(3,3) :: xRot, yRot, zRot

        ! Translate bu camera position so rotating about the origin is the same as rotating about the camera
        p(1) = pos(1) - cameraPos(1)
        p(2) = pos(2) - cameraPos(2)
        p(3) = pos(3) - cameraPos(3)

        ! Rotate about the origin
        transform_coords = rotate_coords(p)

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
        real, dimension(3) :: trans, toCam
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
                trans = rotate_coords(objs(i)%tris(j)%norm)
                toCam = transform_coords(objs(i)%tris(j)%median)
                angle = acos(DOT_PRODUCT(trans, toCam / norm2(toCam)))

                if (abs(angle) >= pi / 2) then

                    trisDrawn = trisDrawn + 1

                    ! Fill triangle using zBuffer
                    if (j < 15) call fill_tri(objs(i)%tris(j), " ")

                    ! Add the edges using the zBuffer
                    call draw_line_3d(objs(i)%tris(j)%verts(1,:), objs(i)%tris(j)%verts(2,:), dz=lineOffset)
                    call draw_line_3d(objs(i)%tris(j)%verts(2,:), objs(i)%tris(j)%verts(3,:), dz=lineOffset)
                    call draw_line_3d(objs(i)%tris(j)%verts(3,:), objs(i)%tris(j)%verts(1,:), dz=lineOffset)

                end if

            end do

        end do

        ! Draw ui stuff
        call draw_box(2, 1, 46, 6)
        call draw_string_2d(4, 2, "tris drawn: " // str(trisDrawn))
        call draw_string_2d(23, 2, "transforms: " // str(transformations))
        call draw_string_2d(4, 3, "  last key: " // lastChar)
        call draw_string_2d(23, 3, "fps: " // str(fps))
        call draw_string_2d(4, 4, "  rotation: " // str((180/pi)*cameraDir))
        call draw_string_2d(4, 5, "  position: " // str(cameraPos))

    end subroutine

    subroutine draw()

        integer :: i, j, e, ms1
        integer,dimension(8) :: t

        do i=1, screenWidth
            do j=1, screenHeight
                e = mvaddch(j-1, i-1, ichar(buffer(i, j)))
            end do
        end do

        frames = frames + 1

        call date_and_time(values = t)
        ms1 = (t(5)*3600 + t(6)*60 + t(7))*1000 + t(8)

        if (ms1 - ms > 1000) then

            fps = frames
            ms = ms1
            frames = 0

        end if

    end subroutine

    subroutine draw_box(x1, y1, x2, y2)

        integer, intent(in) :: x1, y1, x2, y2

        call draw_line_2d(x1, y1, x1, y2)
        call draw_line_2d(x2, y1, x2, y2)
        call draw_line_2d(x1, y1, x2, y1)
        call draw_line_2d(x1, y2, x2, y2)

    end subroutine

    ! Fill triangle using zBuffer
    subroutine fill_tri(t, char)

        type(tri), intent(in) :: t
        character, intent(in) :: char

        integer :: i, pointDensity
        real :: highestZVal, lowestZVal, middleZVal, deltaZ, z, dh, dl, s
        integer :: highestZ, lowestZ, middleZ
        real, dimension(3) :: a, b, t1, t2

        highestZ = maxloc((/ t%verts(1,3), t%verts(2,3), t%verts(3,3) /), 1)
        highestZVal = t%verts(highestZ, 3)
        lowestZ = minloc((/ t%verts(1,3), t%verts(2,3), t%verts(3,3) /), 1)
        lowestZVal = t%verts(lowestZ, 3)
        middleZ = 6 - highestZ - lowestZ
        middleZVal = t%verts(middleZ, 3)

        t1 = transform_coords(t%verts(highestZ, :))
        t2 = transform_coords(t%verts(lowestZ, :))
        pointDensity = ceiling(pointDensityScale * norm2(real(project_coords(t1) - project_coords(t2))))
        if (pointDensity > pointDensityLimit) pointDensity = pointDensityLimit

        deltaZ = (highestZVal - lowestZVal) / pointDensity
        call draw_string_2d(1, 10, str(deltaZ))
        z = highestZVal

        call draw_string_2d(1, 11, str(z))

        ! Work downwards from the top point
        do i=0, pointDensity

            ! Get the intersection of this plane with longest z edge
            dh = t%verts(highestZ, 3) - z
            dl = t%verts(lowestZ, 3) - z
            s = -dh / (dh - dl)
            a(1) = t%verts(highestZ, 1) + s*(t%verts(highestZ, 1) - t%verts(lowestZ, 1))
            a(2) = t%verts(highestZ, 2) + s*(t%verts(highestZ, 2) - t%verts(lowestZ, 2))
            a(3) = z

            ! If above the middle z, use the high-middle edge
            if (z > middleZVal) then

                ! Get the intersection of this plane with the high-middle edge
                dh = t%verts(highestZ, 3) - z
                dl = t%verts(middleZ, 3) - z
                s = -dh / (dh - dl)
                b(1) = t%verts(highestZ, 1) + s*(t%verts(highestZ, 1) - t%verts(middleZ, 1))
                b(2) = t%verts(highestZ, 2) + s*(t%verts(highestZ, 2) - t%verts(middleZ, 2))
                b(3) = z

            else

                ! Get the intersection of this plane with the middle-low edge
                dh = t%verts(middleZ, 3) - z
                dl = t%verts(lowestZ, 3) - z
                s = -dh / (dh - dl)
                b(1) = t%verts(middleZ, 1) + s*(t%verts(middleZ, 1) - t%verts(lowestZ, 1))
                b(2) = t%verts(middleZ, 2) + s*(t%verts(middleZ, 2) - t%verts(lowestZ, 2))
                b(3) = z

            end if

            ! Draw the line
            call draw_line_3d(a, b, char)

            z = z - deltaZ

        end do

        call draw_string_2d(1, 12, str(z))

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

                end select

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

                end select

            end if

        end if

        if (wasdMoveEnabled) then

            select case (k)

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

        end if

        if (flightEnabled) then

            select case (k)

                case (" ")
                    cameraPos(3) = cameraPos(3) + moveSpeed
                case ("c")
                    cameraPos(3) = cameraPos(3) - moveSpeed

            end select

        end if

    end subroutine

    subroutine update_orbit()

        real, dimension(3) :: unit

        unit = (/ -sin(cameraDir(3))*cos(cameraDir(1)), cos(cameraDir(3))*cos(cameraDir(1)), sin(cameraDir(1)) /)
        cameraPos = orbitPos - orbitDistance*unit/norm2(unit)

    end subroutine

    subroutine get_input(k)

        character, dimension(maxInputs) :: c
        character, intent(inout) :: k
        integer :: i

        do i=1, maxInputs
            c(i) = char(getch())
        end do

        do i=1, maxInputs
            call process_input(c(i))
        end do

        if (c(1) /= "ÿ") lastChar = c(1)
        k = c(1)

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
        integer :: i
        real :: angle2d, length3d, distance3d
        real :: deltaLength = 0.1
        real, dimension(3) :: pos, unitVector, v2
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

    ! TODO get working against, need color matrix
    subroutine set_color(col)

        character(*), intent(in) :: col
        integer :: e

        select case (col)

            case ("white")
                e = attron(COLOR_PAIR(1))
            case ("red")
                e = attron(COLOR_PAIR(2))
            case ("green")
                e = attron(COLOR_PAIR(3))
            case ("blue")
                e = attron(COLOR_PAIR(4))

        end select

    end subroutine

    subroutine draw_line_2d(x1, y1, x2, y2)

        integer, intent(in) :: x1, y1, x2, y2
        integer :: length, i
        real :: angle
        character :: char

        angle = atan2(real(y2-y1), real(x2-x1))
        length = nint(sqrt(real((y2-y1)**2 + (x2-x1)**2)))

        char = charFromAngle(angle)

        do i = 0, length
            buffer(nint(x1+i*cos(angle)), nint(y1+i*sin(angle))) = char
        end do

    end subroutine

    subroutine draw_line_3d(v1, v2, c, dz)

        real, dimension(3), intent(in) :: v1, v2
        character(*), intent(in), optional :: c
        real, intent(in), optional :: dz
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

        if (present(c)) then
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
        real, dimension(3) :: avg

        ! For each primitive triangle
        do i=1, obj%numTris

            avg = 0

            ! Sum each point
            do j=1, 3
                avg = avg + obj%tris(i)%verts(j,:)
            end do

            ! Divide by 3 to get the average
            obj%tris(i)%median = avg / 3.0

        end do

    end subroutine

    ! Create a cube at a certain position with a certain size
    subroutine add_cube(x, y, z, width, height, depth)

        real, intent(in) :: x, y, z, width, height, depth
        real :: w, d, h

        ! Shorthand
        w = width / 2.0
        d = height / 2.0
        h = depth / 2.0

        numObjs = numObjs + 1
        objs(numObjs)%numTris = 12
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
    subroutine add_stl(x, y, z, filename)

        real, intent(in) :: x, y, z
        character(*), intent(in) :: filename

        numObjs = numObjs + 1

        ! Load the file TODO

        objs(numObjs)%numTris = 12
        allocate(objs(numObjs)%tris(objs(numObjs)%numTris))



        call calc_medians(objs(numObjs))

    end subroutine

end module engine
