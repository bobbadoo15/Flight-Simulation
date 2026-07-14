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
    ! write(*,'(A,F20.13)') " Wbx (lbs): ", body%x
    ! write(*,'(A,F20.13)') " Wby (lbs): ", body%y
    ! write(*,'(A,F20.13)') " Wbz (lbs): ", body%z
    ! write(*,*) "============================================="
    ! write(*,*) ""

    !! ==================================================
    !!  EXAMPLE 1.5.1 - FIND BF FROM EF USING QUATERNION
    !! ==================================================

    ! type(vector)  :: w, body
    ! type(quat)    :: self, q
    ! real(rk)      :: weight, phi, theta, psi

    ! phi    = 60.0_rk
    ! theta  = 10.0_rk
    ! psi    = 45.0_rk
    ! weight = 100.0_rk

    ! q = create_quaternion_euler_angles(phi, theta, psi)
    ! w = v_from_3_reals(0.0_rk, 0.0_rk, weight)
    ! call self%qe2b(q, w, body)

    ! write(*,*) ""
    ! write(*,*) "============================================="
    ! write(*,*) "            BODY-FIXED COORDINATES           "
    ! write(*,*) "============================================="
    ! write(*,'(A,F20.13)') " Wbx (lbs): ", body%x
    ! write(*,'(A,F20.13)') " Wby (lbs): ", body%y
    ! write(*,'(A,F20.13)') " Wbz (lbs): ", body%z
    ! write(*,*) "============================================="
    ! write(*,*) ""

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
    ! write(*,'(A,F20.13)') " eo          : ", q%o
    ! write(*,'(A,F20.13)') " ex          : ", q%x
    ! write(*,'(A,F20.13)') " ey          : ", q%y
    ! write(*,'(A,F20.13)') " ez          : ", q%z
    ! write(*,'(A,F20.13)') " Theta (deg) : ", TH * r2d
    ! write(*,'(A,F20.13)') " Ex          : ", EA%x
    ! write(*,'(A,F20.13)') " Ey          : ", EA%y
    ! write(*,'(A,F20.13)') " Ez          : ", EA%z
    ! write(*,*) "============================================="
    ! write(*,*) ""

    !! ==================================================
    !!    PROBLEM 1.C.8 - FIND EULER ANGLES FROM QUAT
    !! ==================================================

    type(quat)             :: q
    real(rk)               :: e0, ex, ey, ez
    real(rk), dimension(3) :: angles

    ! Case A
    ! e0 = 1.0_rk
    ! ex = 0.0_rk
    ! ey = 0.0_rk
    ! ez = 0.0_rk

    ! Case B
    ! e0 = 0.84462319862_rk
    ! ex = 0.19134171618_rk
    ! ey = 0.46193976626_rk
    ! ez = 0.19134171618_rk

    ! Case C
    ! e0 = -0.35888381816_rk
    ! ex = -0.60926382222_rk
    ! ey = 0.61543441464_rk
    ! ez = -0.34819603857_rk

    ! Case D
    ! e0 = 0.70710678119_rk
    ! ex = 0.0_rk
    ! ey = 0.70710678119_rk
    ! ey = -0.70710678119_rk
    ! ez = 0.0_rk

    ! Personal Use
    e0 = 0.694115_rk
    ex = 0.694115_rk
    ey = -0.134922_rk
    ez = 0.134922_rk

    q = create_quaternion_euler_rodrigues_reals(e0, ex, ey, ez)
    angles = q%get_euler_angles(q)

    write(*,*) ""
    write(*,*) "============================================="
    write(*,*) "        EULER ANGLES FROM QUATERNION         "
    write(*,*) "============================================="
    write(*,'(A,F20.13)') " phi   (deg) : ", angles(1) * r2d
    write(*,'(A,F20.13)') " theta (deg) : ", angles(2) * r2d
    write(*,'(A,F20.13)') " psi   (deg) : ", angles(3) * r2d
    write(*,*) "============================================="
    write(*,*) ""

end program FlightAnalysisforLandingControlandOperationsNetwork