!! This is the main program for the flight simulation. It will initialize the simulation, run the main loop, and handle any necessary cleanup.

program FlightAnalysisControlOperationsNetwork
    use units_m
    use vector_m
    use matrix_m
    use euler_angles_m
    implicit none

    type(vector) :: weight, w_rot

    !! ========== Euler Angle Problems ==========
    ! Example 1.2.1
    weight = v_from_3_reals(0.0_rk, 0.0_rk, 100.0_rk)
    ! call w%printvector()
    call rotation_matrix(weight, 60.0_rk, 10.0_rk, 45.0_rk, w_rot)
    ! call w_rot%printvector()


end program FlightAnalysisControlOperationsNetwork