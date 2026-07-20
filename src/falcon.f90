!! This is the main program for the flight simulation. It will initialize the simulation, run the main loop, and handle any necessary cleanup.

program FlightAnalysisforLandingControlandOperationsNetwork
    use units_m
    ! use vector_m
    ! use matrix_m
    ! use euler_angles_m
    ! use quaternion_m
    use atmosphere_m
    implicit none

    ! ===============================================================================================
    !                             CHAPTER 3 - AERODYNAMIC MODELING
    ! ===============================================================================================

    ! ==================================================
    !            PROBLEM 3.C1/C2 - FIND g(H)
    ! ==================================================

    real(rk) :: H_si, H_us, gsi, gus

    H_si = 100000.0_rk
    H_us = 2.0_rk * H_si

    gsi = get_gravity_si(H_si)
    gus = get_gravity_us(H_us)
    write(*,*) ""
    write(*,*) "================================================="
    write(*,*) "        GRAVITY FROM GEOMETRIC ALTITUTDE"
    write(*,*) "================================================="
    write(*,'(A,F15.12)') " Gravity (m/s^2)  :   ", gsi
    write(*,'(A,F15.12)') " Gravity (ft/s^2) :   ", gus
    write(*,*) "================================================="
    write(*,*) ""

end program FlightAnalysisforLandingControlandOperationsNetwork