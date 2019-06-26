program main

    use engine

    character :: char

    ! Initialise the screen
    call start()

    ! Enable controls
    call enable_real_time()
    call set_wasd_movement(.false.)
    call set_arrows_rotation(.true.)
    call set_flight(.false.)
    call set_orbit(.true.)

    ! Add test objects
    call add_cube(0.0, 2.0, 0.0, 1.0, 1.0, 1.0)

    do

        ! Render the 3D scene
        call render()

        ! Draw the buffer to the screen
        call draw()

        ! Get the user's key press (will also process inputs like wasd if enabled)
        call get_input(char)

    end do

end program main
