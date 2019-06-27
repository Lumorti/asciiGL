program main

    use engine

    character :: char

    ! Initialise the screen
    call start()

    ! Enable controls
    call enable_real_time()
    call set_interactive(.true.)
    call set_flight_enabled(.true.)
    call set_orbit_enabled(.true.)

    ! Add test objects
    call add_cube((/ 2.0, 4.0, 0.0 /), (/ 1.0, 1.0, 1.0 /))
    call add_stl("cube.stl", (/ 0.0, 4.0, 0.0 /))
    call set_orbit_object(2)

    do

        ! Render the 3D scene
        call render()

        ! Draw the buffer to the screen
        call draw()

        ! Get the user's key press (will also process inputs like wasd if enabled)
        call get_input(char)

    end do

end program main
