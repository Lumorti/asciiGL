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
