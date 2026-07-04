!! This is the main program for the flight simulation. It will initialize the simulation, run the main loop, and handle any necessary cleanup.

program FlightAnalysisControlOperationsNetwork
    use units_m
    use vector_m
    use matrix_m
    use euler_angles_m
    implicit none

    type(rot_mat) :: r
    type(matrix) :: m_out
    type(vector) :: weight, w_inv_rot

    !! ========== Euler Angle Problems ==========
    weight = v_from_3_reals(0.0_rk, 0.0_rk, 100.0_rk)
    call r%rot(weight, 60.0_rk, 10.0_rk, 45.0_rk, w_inv_rot, m_out)
    call m_out%printmat()
    call w_inv_rot%printvector() ! Prints rotated weight in body-fixed coordinates

end program FlightAnalysisControlOperationsNetwork