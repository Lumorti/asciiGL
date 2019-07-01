program example

    use asciiGL

    character :: userInput
    integer :: chairObject

    ! Initialise the screen
    call start()

    ! Set up the camera and allows the user to orbit using the arrows and zoom using w/s
    call set_interactivity("fly")
    call set_camera_pos((/ 0.05, 4.85, 1.7 /))
    call set_camera_rot((/ -0.30, 0.0, 0.05 /))

    ! Add a chair model, saving the ID into chairObject
    chairObject = add_stl("../models/chair.stl", pos=(/ 0.0, 8.0, 0.7 /), scale=(/ 0.3, 0.3, 0.3 /), fillChar=" ")

    do

        ! Render the 3D scene and draw to the screen
        call render()
        call draw()

        ! Get the user's key press (will also process inputs like wasd if enabled)
        call get_input(userInput)

        ! Rotate the chair
        call rotate_object(chairObject, (/ 0.0, 0.0, 0.01 /))

    end do

end program
