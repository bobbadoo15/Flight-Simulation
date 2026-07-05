!! This is the main program for the flight simulation. It will initialize the simulation, run the main loop, and handle any necessary cleanup.

program FlightAnalysisControlOperationsNetwork
    use units_m
    use vector_m
    use matrix_m
    use euler_angles_m
    implicit none

    type(rot_mat) :: r
    ! type(matrix)  :: m_out
    type(vector)  :: weight, w_inv_rot

    ! ========== Euler Angle Problems ==========
    weight = v_from_3_reals(-17.364817766693033_rk, 85.286853195244319_rk, 49.240387650610415_rk)
    call r%b2e(weight, 60.0_rk, 10.0_rk, 45.0_rk, w_inv_rot)
    ! call m_out%printmat()
    call w_inv_rot%printvector() ! Prints rotated weight in body-fixed coordinates

    !! ========== Euler Axis Problems ==========
    ! Sometime referred to as Eigen Axis/Characteristic Axis



end program FlightAnalysisControlOperationsNetwork