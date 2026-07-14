!! This is the main program for the flight simulation. It will initialize the simulation, run the main loop, and handle any necessary cleanup.

program FlightAnalysisforLandingControlandOperationsNetwork
    use units_m
    use vector_m
    use matrix_m
    use euler_angles_m
    use euler_axis_m
    use quaternion_m
    implicit none

    !! ==================================================
    !!    EXAMPLE 1.2.1 - FIND BF FROM EF USING ANGLES
    !! ==================================================

    ! real(rk)      :: phi, theta, psi, weight
    ! type(vector)  :: w, body
    ! type(rot_mat) :: r

    ! phi    = 60.0_rk
    ! theta  = 10.0_rk
    ! psi    = 45.0_rk
    ! weight = 100.0_rk

    ! w      = v_from_3_reals(0.0_rk, 0.0_rk, weight)
    ! call r%e2b(w, phi, theta, psi, body)

    ! write(*,*) ""
    ! write(*,*) "============================================="
    ! write(*,*) "            BODY-FIXED COORDINATES           "
    ! write(*,*) "============================================="
    ! write(*,'(A,F18.13)') " Wbx (lbs): ", body%x
    ! write(*,'(A,F18.13)') " Wby (lbs): ", body%y
    ! write(*,'(A,F18.13)') " Wbz (lbs): ", body%z
    ! write(*,*) "============================================="
    ! write(*,*) ""

    !! ==================================================
    !!  EXAMPLE 1.5.1 - FIND BF FROM EF USING QUATERNION
    !! ==================================================

    type(vector)  :: w, body
    type(quat)    :: self, q
    real(rk)      :: weight, phi, theta, psi

    phi    = 60.0_rk
    theta  = 10.0_rk
    psi    = 45.0_rk
    weight = 100.0_rk

    q = create_quaternion_euler_angles(phi, theta, psi)
    w = v_from_3_reals(0.0_rk, 0.0_rk, weight)
    call self%qe2b(q, w, body)

    write(*,*) ""
    write(*,*) "============================================="
    write(*,*) "            BODY-FIXED COORDINATES           "
    write(*,*) "============================================="
    write(*,'(A,F18.13)') " Wbx (lbs): ", body%x
    write(*,'(A,F18.13)') " Wby (lbs): ", body%y
    write(*,'(A,F18.13)') " Wbz (lbs): ", body%z
    write(*,*) "============================================="
    write(*,*) ""

    !! ==================================================
    !!  EXAMPLE 1.6.1 - FIND QUAT/EULER AXIS FROM ANGLES
    !! ==================================================

    ! real(rk) :: phi, theta, psi, TH
    ! type(quat) :: q
    ! type(vector) :: EA

    ! phi   = 60.0_rk
    ! theta = 20.0_rk
    ! psi   = 45.0_rk

    ! q = create_quaternion_euler_angles(phi, theta, psi)
    ! call q%get_euler_axis(TH, EA)

    ! write(*,*) ""
    ! write(*,*) "============================================="
    ! write(*,*) "             STATES OF AIRCRAFT              "
    ! write(*,*) "============================================="
    ! write(*,'(A,F16.13)') " eo          : ", q%o
    ! write(*,'(A,F16.13)') " ex          : ", q%x
    ! write(*,'(A,F16.13)') " ey          : ", q%y
    ! write(*,'(A,F16.13)') " ez          : ", q%z
    ! write(*,'(A,F16.13)') " Theta (deg) : ", TH * r2d
    ! write(*,'(A,F16.13)') " Ex          : ", EA%x
    ! write(*,'(A,F16.13)') " Ey          : ", EA%y
    ! write(*,'(A,F16.13)') " Ez          : ", EA%z
    ! write(*,*) "============================================="
    ! write(*,*) ""

end program FlightAnalysisforLandingControlandOperationsNetwork