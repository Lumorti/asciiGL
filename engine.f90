! hello.f90
module engine

    use :: ncurses
    implicit none

    real, parameter :: ez = 60
    real, parameter :: xScale = 2.0
    real, parameter :: nearDistance = 1.2
    integer, parameter :: pointDensity = 80
    integer, parameter :: maxObjects = 50
    real :: moveSpeed = 0.01, rotSpeed = 0.01

    type tri
        real, dimension(3, 3) :: verts
        real, dimension(3) :: norm
    end type

    type renderObject
        integer :: numTris = 0
        type(tri), allocatable, dimension(:) :: tris
        real, dimension(3) :: centre = (/ 0, 0, 0 /), rot = (/ 0, 0, 0 /)
    end type

    real, dimension(3) :: cameraPos
    real, dimension(3) :: cameraDir
    logical :: wasdMoveEnabled = .false., arrowRotEnabled = .false.
    type(renderObject), dimension(maxObjects) :: objs
    integer :: numObjs = 0
    integer :: screenWidth = 0, screenHeight = 0
    real :: halfW = 0, halfH = 0
    character :: lastChar = " "
    real, dimension(:, :), allocatable :: zBuffer

contains

    subroutine start()

        integer :: e

        ! Set up the screen, hide the cursor, enable color, don't show the typed character
        stdscr = initscr()
        e = curs_set(0)
        e = start_color()
        e = noecho()

        ! Get the initial screen size
        call getmaxyx(stdscr, screenHeight, screenWidth)
        halfW = screenWidth / 2
        halfH = screenHeight / 2

        ! Set up the z buffer (in reality it's actually a y buffer, but sticking with the terminology)
        allocate(zBuffer(screenWidth, screenHeight))
        zBuffer = 300000.0

        ! Set up some color pairs
        e = init_pair(1_c_short, COLOR_WHITE, COLOR_BLACK)
        e = init_pair(2_c_short, COLOR_RED, COLOR_BLACK)
        e = init_pair(3_c_short, COLOR_GREEN, COLOR_BLACK)
        e = init_pair(4_c_short, COLOR_BLUE, COLOR_BLACK)

    end subroutine

    subroutine enable_wasd_movement()
        wasdMoveEnabled = .true.
    end subroutine

    subroutine enable_arrows_rotation()
        arrowRotEnabled = .true.
    end subroutine

    ! Go from world space to camera space
    function transform_coords(pos)

        real, dimension(3), intent(in) :: pos
        real, dimension(3) :: transform_coords

        real, dimension(3) :: p
        real, dimension(3,3) :: xRot, yRot, zRot

        p(1) = pos(1) - cameraPos(1)
        p(2) = pos(2) - cameraPos(2)
        p(3) = pos(3) - cameraPos(3)

        xRot = reshape((/ 1.0,                    0.0,                    0.0,                &
                        & 0.0,                    cos(cameraDir(1)),      sin(cameraDir(1)),  &
                        & 0.0,                    -sin(cameraDir(1)),     cos(cameraDir(1))   /), shape(xRot))

        yRot = reshape((/ cos(cameraDir(2)),      0.0,                    -sin(cameraDir(2)), &
                        & 0.0,                    1.0,                    0.0,                &
                        & sin(cameraDir(2)),      0.0,                    cos(cameraDir(2))   /), shape(yRot))

        zRot = reshape((/ cos(-cameraDir(3)),     sin(-cameraDir(3)),     0.0,                &
                        & -sin(-cameraDir(3)),    cos(-cameraDir(3)),     0.0,                &
                        & 0.0,                    0.0,                    1.0                 /), shape(zRot))

        transform_coords = matmul(xRot, matmul(zRot, matmul(yRot, p)))

    end function

    ! Project 3d coords onto 2d plane
    function project_coords(pos)

        real, dimension(3), intent(in) :: pos
        integer, dimension(2) :: project_coords

        project_coords(1) = xScale * (ez * pos(1) / pos(2)) + halfW
        project_coords(2) = ez * pos(3) / pos(2) + halfH

    end function

    ! Draw all of the triangles
    subroutine draw()

        integer :: e, i, j
        real :: halfW, halfH
        integer, dimension(2) :: coord1, coord2, coord3

        ! Reset the z buffer
        zBuffer = 300000.0

        ! Wipe the screen
        do i=0, screenWidth
            do j=0, screenHeight
                e = mvaddch(j, i, ichar(" "))
            end do
        end do

        ! Go through all objects
        do i=1, numObjs

            ! Draw each primitive triangle
            do j=1, objs(i)%numTris

                ! Fill triangle using zBuffer
                if (j == 3) call fill_tri(objs(i)%tris(j))

                ! Add the edges using the zBuffer
                call draw_line_3d(objs(i)%tris(j)%verts(1,:), objs(i)%tris(j)%verts(2,:))
                call draw_line_3d(objs(i)%tris(j)%verts(2,:), objs(i)%tris(j)%verts(3,:))
                call draw_line_3d(objs(i)%tris(j)%verts(3,:), objs(i)%tris(j)%verts(1,:))

            end do

        end do

        ! Draw ui stuff
        call draw_box(2, 1, 45, 6)
        call draw_string_2d(4, 2, "term size: " // int_to_string(screenWidth) // " by " // int_to_string(screenHeight))
        call draw_string_2d(4, 3, " last key: " // lastChar)
        call draw_string_2d(4, 4, " rotation: " // vector_to_string((180/3.141592)*cameraDir))
        call draw_string_2d(4, 5, " position: " // vector_to_string(cameraPos))

    end subroutine

    subroutine draw_box(x1, y1, x2, y2)

        integer, intent(in) :: x1, y1, x2, y2

        call draw_line_2d(x1, y1, x1, y2)
        call draw_line_2d(x2, y1, x2, y2)
        call draw_line_2d(x1, y1, x2, y1)
        call draw_line_2d(x1, y2, x2, y2)

    end subroutine

    ! Fill triangle using zBuffer TODO
    subroutine fill_tri(t)

        type(tri), intent(in) :: t

        integer :: i
        real :: highestZVal, lowestZVal, middleZVal, deltaZ, z, dh, dl, s
        integer :: highestZ, lowestZ, middleZ, topZ
        real, dimension(3) :: a, b

        highestZ = maxloc((/ t%verts(1,3), t%verts(2,3), t%verts(3,3) /), 1)
        highestZVal = t%verts(highestZ, 3)
        lowestZ = minloc((/ t%verts(1,3), t%verts(2,3), t%verts(3,3) /), 1)
        lowestZVal = t%verts(lowestZ, 3)
        middleZ = 6 - highestZ - lowestZ
        middleZVal = t%verts(middleZ, 3)

        deltaZ = (highestZVal - lowestZVal) / pointDensity
        z = highestZVal

        ! Work downwards from the top point
        do i=0, pointDensity

            ! Get the intersection of this plane with longest z edge
            dh = t%verts(highestZ, 3) - z
            dl = t%verts(lowestZ, 3) - z
            s = dh / (dh - dl)
            a(1) = t%verts(highestZ, 1) + s*(t%verts(highestZ, 1) - t%verts(lowestZ, 1))
            a(2) = t%verts(highestZ, 2) + s*(t%verts(highestZ, 2) - t%verts(lowestZ, 2))
            a(3) = t%verts(highestZ, 3) + s*(t%verts(highestZ, 3) - t%verts(lowestZ, 3))

            ! If below the middle z, use that edge instead
            if (z > middleZVal) then

                ! Get the intersection of this plane with other appropriate z edge
                dh = t%verts(highestZ, 3) - z
                dl = t%verts(middleZ, 3) - z
                s = dh / (dh - dl)
                b(1) = t%verts(highestZ, 1) + s*(t%verts(highestZ, 1) - t%verts(middleZ, 1))
                b(2) = t%verts(highestZ, 2) + s*(t%verts(highestZ, 2) - t%verts(middleZ, 2))
                b(3) = t%verts(highestZ, 3) + s*(t%verts(highestZ, 3) - t%verts(middleZ, 3))

            else

                ! Get the intersection of this plane with other appropriate z edge
                dh = t%verts(middleZ, 3) - z
                dl = t%verts(lowestZ, 3) - z
                s = dh / (dh - dl)
                b(1) = t%verts(middleZ, 1) + s*(t%verts(middleZ, 1) - t%verts(lowestZ, 1))
                b(2) = t%verts(middleZ, 2) + s*(t%verts(middleZ, 2) - t%verts(lowestZ, 2))
                b(3) = t%verts(middleZ, 3) + s*(t%verts(middleZ, 3) - t%verts(lowestZ, 3))

            end if

            ! Draw the line
            call draw_line_3d(a, b)

            z = z + deltaZ

        end do


    end subroutine

    subroutine get_input(k)

        character, intent(inout) :: k
        k = char(getch())
        lastChar = k

        if (arrowRotEnabled) then

            select case (k)

                case ("K") ! Left arrow
                    cameraDir(3) = cameraDir(3) + rotSpeed
                case ("M") ! Right arrow
                    cameraDir(3) = cameraDir(3) - rotSpeed
                case ("H") ! Up arrow
                    cameraDir(1) = cameraDir(1) + cos(cameraDir(3))*rotSpeed
                    cameraDir(2) = cameraDir(2) + sin(cameraDir(3))*rotSpeed
                case ("P") ! Down arrow
                    cameraDir(1) = cameraDir(1) - cos(cameraDir(3))*rotSpeed
                    cameraDir(2) = cameraDir(2) - sin(cameraDir(3))*rotSpeed

            end select

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

    end subroutine

    subroutine get_screen_size(x, y)

        integer, intent(inout) :: x, y
        x = screenWidth
        y = screenHeight

    end subroutine

    subroutine draw_string_2d(x, y, char)

        integer, intent(in) :: x, y
        character(*), intent(in) :: char
        integer :: e, i

        do i = 1, len_trim(char)
            e = mvaddch(y, x+i-1, ichar(char(i:i)))
        end do

    end subroutine

    subroutine draw_string_3d(x, y, z, char)

        real, intent(in) :: x, y, z
        character(*), intent(in) :: char
        integer :: e, i

        integer, dimension(2) :: coords

        coords = project_coords(transform_coords((/ x, y, z /)))

        do i = 1, len_trim(char)
            e = mvaddch(coords(2), coords(1)+i-1, ichar(char(i:i)))
        end do

    end subroutine

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
        integer :: length, i, e
        real :: angle
        character :: char

        angle = atan2(real(y2-y1), real(x2-x1))
        length = sqrt(real((y2-y1)**2 + (x2-x1)**2))

        char = charFromAngle(angle)

        do i = 0, length
            e = mvaddch(nint(y1+i*sin(angle)), nint(x1+i*cos(angle)), ichar(char))
        end do

    end subroutine

    subroutine draw_line_3d(v1, v2)

        real, dimension(3), intent(in) :: v1, v2
        real, dimension(3) :: transformed1, transformed2
        real, dimension(3) :: clipped1, clipped2
        integer :: i, e
        real :: angle2d, length3d, distance3d
        character :: char
        real :: deltaLength = 0.1
        real, dimension(3) :: pos, unitVector
        integer, dimension(2) :: startCoords, endCoords, coords
        real :: da, db, s

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

        char = charFromAngle(angle2d)

        deltaLength = length3d / pointDensity
        pos = clipped1

        do i = 0, pointDensity

            distance3d = sqrt((pos(1))**2 + (pos(2))**2 + (pos(3))**2)
            coords = project_coords(transform_coords(pos))

            ! 2D clipping
            if (coords(1) >= 0 .and. coords(1) <= screenWidth .and. coords(2) >= 0 .and. coords(2) <= screenHeight) then

                ! Check if it's closer than the last character drawn at this point
                if (distance3d < zBuffer(coords(1), coords(2))) then

                    zBuffer(coords(1), coords(2)) = distance3d
                    e = mvaddch(coords(2), coords(1), ichar(char))

                end if

            end if

            pos = pos + unitVector*deltaLength

        end do

    end subroutine

    function charFromAngle(angle)

        character :: charFromAngle
        real, intent(in) :: angle

        select case (nint(angle*(180/3.141592)))

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

        integer :: a, i, sign
        character(get_length_needed(a)) :: int_to_string

        int_to_string = ""

        if (len(int_to_string) <= 0) then
            return
        end if

        write(int_to_string, "(I0)") a

    end function

    function real_to_string(a)

        real :: a, i, sign
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

        vector_to_string(1:12) = "(" // real_to_string(a(1)) // ", "
        vector_to_string(12:21) = real_to_string(a(2)) // ", "
        vector_to_string(21:) = real_to_string(a(3)) // ")"

    end function

    subroutine stop()

        integer :: e
        e = endwin()

    end subroutine

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

        ! Furthest face
        objs(numObjs)%tris(3)%verts(1,:) = (/ x+w, y+d, z-h /)
        objs(numObjs)%tris(3)%verts(2,:) = (/ x-w, y+d, z+h /)
        objs(numObjs)%tris(3)%verts(3,:) = (/ x-w, y+d, z-h /)
        objs(numObjs)%tris(4)%verts(1,:) = (/ x+w, y+d, z-h /)
        objs(numObjs)%tris(4)%verts(2,:) = (/ x-w, y+d, z+h /)
        objs(numObjs)%tris(4)%verts(3,:) = (/ x+w, y+d, z+h /)

        ! Left face
        objs(numObjs)%tris(5)%verts(1,:) = (/ x-w, y-d, z-h /)
        objs(numObjs)%tris(5)%verts(2,:) = (/ x-w, y+d, z+h /)
        objs(numObjs)%tris(5)%verts(3,:) = (/ x-w, y-d, z+h /)
        objs(numObjs)%tris(6)%verts(1,:) = (/ x-w, y-d, z-h /)
        objs(numObjs)%tris(6)%verts(2,:) = (/ x-w, y+d, z+h /)
        objs(numObjs)%tris(6)%verts(3,:) = (/ x-w, y+d, z-h /)

        ! Right face
        objs(numObjs)%tris(7)%verts(1,:) = (/ x+w, y-d, z-h /)
        objs(numObjs)%tris(7)%verts(2,:) = (/ x+w, y+d, z+h /)
        objs(numObjs)%tris(7)%verts(3,:) = (/ x+w, y-d, z+h /)
        objs(numObjs)%tris(8)%verts(1,:) = (/ x+w, y-d, z-h /)
        objs(numObjs)%tris(8)%verts(2,:) = (/ x+w, y+d, z+h /)
        objs(numObjs)%tris(8)%verts(3,:) = (/ x+w, y+d, z-h /)

        ! Top face
        objs(numObjs)%tris(9)%verts(1,:) = (/ x-w, y-d, z+h /)
        objs(numObjs)%tris(9)%verts(2,:) = (/ x+w, y+d, z+h /)
        objs(numObjs)%tris(9)%verts(3,:) = (/ x-w, y+d, z+h /)
        objs(numObjs)%tris(10)%verts(1,:) = (/ x-w, y-d, z+h /)
        objs(numObjs)%tris(10)%verts(2,:) = (/ x+w, y+d, z+h /)
        objs(numObjs)%tris(10)%verts(3,:) = (/ x+w, y-d, z+h /)

        ! Bottom face
        objs(numObjs)%tris(11)%verts(1,:) = (/ x-w, y-d, z-h /)
        objs(numObjs)%tris(11)%verts(2,:) = (/ x+w, y+d, z-h /)
        objs(numObjs)%tris(11)%verts(3,:) = (/ x-w, y+d, z-h /)
        objs(numObjs)%tris(12)%verts(1,:) = (/ x-w, y-d, z-h /)
        objs(numObjs)%tris(12)%verts(2,:) = (/ x+w, y+d, z-h /)
        objs(numObjs)%tris(12)%verts(3,:) = (/ x+w, y-d, z-h /)

    end subroutine

end module engine
