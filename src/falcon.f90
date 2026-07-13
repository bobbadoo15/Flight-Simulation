!! This is the main program for the flight simulation. It will initialize the simulation, run the main loop, and handle any necessary cleanup.

program FlightAnalysisControlOperationsNetwork
    use units_m
    use vector_m
    use matrix_m
    use euler_angles_m
    use euler_axis_m
    use quaternion_m
    implicit none

    type(quat)   :: q
    type(vector) :: euler

    q = quaternion(-0.694115_rk, 0.694115_rk, -0.134922_rk, 0.134922_rk)
    euler = q%get_euler_angles()

    write(*,*) "Roll  (phi):  ", euler%x
    write(*,*) "Pitch (theta):", euler%y
    write(*,*) "Yaw   (psi):  ", euler%z

end program FlightAnalysisControlOperationsNetwork