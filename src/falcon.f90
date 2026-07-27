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
    !     EXAMPLE 3.2.1 - FIND AIR PROPERTIES IN SI
    ! ==================================================

    type(atmosphere) :: atmo
    real(rk)         :: H_ft, rho, mu, nu, g, lambda, a, Z, T, P

    H_ft = 150000_rk

    Z = atmo%gm2gp(H_ft)
    call atmo%air_properties_us(H_ft, rho, mu, nu, g, lambda, a)
    call atmo%temp_and_pressure_gm_us(H_ft, T, P)

    write(*,*) ""
    write(*,*) "================================================="
    write(*,*) "          AIR PROPERTIES IN US UNITS"
    write(*,*) "================================================="
    write(*, '(A15, " : ", 1P, E24.16, 0P)') " g (ft/s2)      :      ", g
    write(*, '(A15, " : ", 1P, E24.16, 0P)') " Z (ft)         :      ", Z
    write(*, '(A15, " : ", 1P, E24.16, 0P)') " T (R)          :      ", T
    write(*, '(A15, " : ", 1P, E24.16, 0P)') " P (PSF)        :      ", P
    write(*, '(A15, " : ", 1P, E24.16, 0P)') " rho (slug/ft3) :      ", rho
    write(*, '(A15, " : ", 1P, E24.16, 0P)') " a (ft/s)       :      ", a
    write(*, '(A15, " : ", 1P, E24.16, 0P)') " mu (slug/fts)  :      ", mu
    write(*, '(A15, " : ", 1P, E24.16, 0P)') " lambda (ft)    :      ", lambda
    write(*,*) "================================================="
    write(*,*) ""

end program FlightAnalysisforLandingControlandOperationsNetwork