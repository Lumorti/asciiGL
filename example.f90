program main

    use engine

    character :: char

    ! Initialise the screen
    call start()

    ! Enable controls
    call enable_wasd_movement()
    call enable_arrows_rotation()

    ! Add test objects
    call add_cube(0.0, 2.0, 0.0, 1.0, 1.0, 1.0)

    do

        ! Render the scene
        call draw()

        ! Get the user's input (will process certain inputs if enabled)
        call get_input(char)

    end do

end program main
