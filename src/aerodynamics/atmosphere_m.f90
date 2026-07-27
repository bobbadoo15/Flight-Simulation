module atmosphere_m
    use units_m
    implicit none

    !! Standard atmospheric database
    real(rk), parameter, dimension(0:7) :: zrange = &                 !! Includes each boundary end and index starts at 0
        (/0.0_rk, 11000.0_rk, 20000.0_rk, 32000.0_rk, 47000.0_rk, &
        52000.0_rk, 61000.0_rk, 79000.0_rk/)                          !! Geopotential range values in m
    real(rk), parameter, dimension(0:6) :: T_prime = &
        (/-0.0065_rk, 0.0_rk, 0.001_rk, 0.0028_rk, 0.0_rk, &
        -0.002_rk, -0.004_rk/)                                        !! Temperature gradient within a geopotential range in K/m
    real(rk), parameter, dimension(0:6) :: T_i = &
        (/288.15_rk, 216.65_rk, 216.65_rk, 228.65_rk, 270.65_rk, &
        270.65_rk, 252.65_rk/)                                        !! Initial Temperature values within geopotential ranges in K

    real(rk), parameter, dimension(0:7) :: P_initial = &
        (/101325.0_rk, 22632.031822221168_rk, 5474.8735282708267_rk, &
        868.01476908672271_rk, 110.90558898922531_rk, 59.000524278924367_rk, &
        18.209924905017658_rk, 1.0377004548920223_rk/)

    type :: atmosphere
    contains
        ! procedure :: init                 => atmosphere_init
        procedure :: gm2gp                   => geometric2geopotential
        procedure :: gp2gm                   => geopotential2geometric
        procedure :: temp_and_pressure_gm_si => get_temperature_and_pressure_from_geometric_si
        procedure :: temp_and_pressure_gm_us => get_temperature_and_pressure_from_geometric_us
        procedure :: temp_and_pressure_gp_si => get_temperature_and_pressure_from_geopotential_si
        procedure :: temp_and_pressure_gp_us => get_temperature_and_pressure_from_geopotential_us
        procedure :: air_properties_si       => get_air_properties_si
        procedure :: air_properties_us       => get_air_properties_us
    end type atmosphere

contains

    !! ==============================================================
    !!                       HELPER FUNCTIONS
    !! ==============================================================

    function density(P, T) result(rho)
        real(rk), intent(in) :: P, T
        real(rk)             :: rho
        rho = P / (r_air * T) ! R = J/kg-K or Nm/kg-K; T = Kelvin
    end function density

    function dynamicviscosity(T) result(mu)
        real(rk), intent(in) :: T
        real(rk)             :: mu
        ! Summarland's Law
        mu = mus * (T/Ts)**1.5 * ((Ts + ks)/(T + ks))
    end function dynamicviscosity

    function gravity_si(H_m) result(g)
        real(rk), intent(in) :: H_m
        real(rk)             :: g
        g = gssl_si * (r_e_z / (r_e_z + H_m))**2 ! In m/s2
        ! write(*,*) ""
        ! write(*,*) "================================================="
        ! write(*,*) "        GRAVITY FROM GEOMETRIC ALTITUTDE"
        ! write(*,*) "================================================="
        ! write(*,'(A,F15.12)') " Gravity (m/s^2)  :   ", g
        ! write(*,*) "================================================="
        ! write(*,*) ""
    end function gravity_si

    function gravity_us(H_ft) result(g)
        real(rk), intent(in) :: H_ft
        real(rk)             :: g
        ! Need to make sure to convert the H value to m and then 
        ! convert back to US after calculations
        g = gravity_si(H_ft * ft_to_m) * m_to_ft
        ! write(*,*) ""
        ! write(*,*) "================================================="
        ! write(*,*) "        GRAVITY FROM GEOMETRIC ALTITUTDE"
        ! write(*,*) "================================================="
        ! write(*,'(A,F15.12)') " Gravity (ft/s^2) :   ", g
        ! write(*,*) "================================================="
        ! write(*,*) ""
    end function gravity_us

    function kinematicviscosity(mu, rho) result(nu)
        real(rk), intent(in) :: mu, rho
        real(rk)             :: nu
        nu = mu / rho
    end function kinematicviscosity

    function molecularmeanfreepath(nu, a) result(lambda)
        real(rk), intent(in) :: nu, a
        real(rk)             :: lambda
        lambda = (nu/a) * sqrt(0.5_rk * pi * gamma_air)
    end function molecularmeanfreepath

    function pressuregradient(P, T) result(dPpdZ)
        real(rk), intent(in) :: P, T
        real(rk)             :: dPpdZ
        dPpdZ = (-P * gssl_si) / (r_air * T)
    end function pressuregradient

    function speedofsound(T) result(a)
        real(rk), intent(in) :: T
        real(rk)             :: a
        a = sqrt(gamma_air * r_air * T)
    end function speedofsound

    !! ==============================================================
    !!                     PROCEDURE FUNCTIONS
    !! ==============================================================

    subroutine atmosphere_init(atmo)
        class(atmosphere), intent(in) :: atmo
    end subroutine atmosphere_init

    function geometric2geopotential(atmo, H) result(Z)
        class(atmosphere), intent(in) :: atmo
        real(rk), intent(in)          :: H
        real(rk)                      :: Z
        Z = (r_e_z * H) / (r_e_z + H) ! In metric
    end function geometric2geopotential

    function geopotential2geometric(atmo, Z) result(H)
        class(atmosphere), intent(in) :: atmo
        real(rk), intent(in)          :: Z
        real(rk)                      :: H
        H = (Z * r_e_z) / (r_e_z - Z) ! In metric
    end function geopotential2geometric

    subroutine get_air_properties_us(atmo, H_ft, rho, mu, nu, g, lambda, a)
        class(atmosphere), intent(in) :: atmo
        real(rk), intent(in)          :: H_ft
        real(rk), intent (out)        :: rho, mu, nu, g, lambda, a
        call atmo%air_properties_si(H_ft*ft_to_m, rho, mu, nu, g, lambda, a)
        rho    = rho * kgm3_to_slugft3 ! slug/ft3
        mu     = mu * kgms_to_slugfts  ! slug/(ft*s)
        nu     = nu * (m_to_ft)**2     ! ft2/s
        g      = g * mps2_to_fps2      ! ft/s2
        a      = a * m_to_ft           ! ft/s
        lambda = lambda * m_to_ft      ! ft
    end subroutine get_air_properties_us

    subroutine get_air_properties_si(atmo, H_m, rho, mu, nu, g, lambda, a)
        class(atmosphere), intent(in) :: atmo
        real(rk), intent(in)          :: H_m
        real(rk), intent (out)        :: rho, mu, nu, g, lambda, a
        real(rk)                      :: T, P
        call atmo%temp_and_pressure_gm_si(H_m, T, P)
        rho    = density(P, T)                ! kg/m3
        mu     = dynamicviscosity(T)          ! Pa*s, (N/m2)*s, kg/(m*s)
        nu     = kinematicviscosity(mu, rho)  ! m2/s
        g      = gravity_si(H_m)              ! m/s2
        a      = speedofsound(T)              ! m/s
        lambda = molecularmeanfreepath(nu, a) ! m
    end subroutine get_air_properties_si

    subroutine get_temperature_and_pressure_from_geopotential_si(atmo, Z, T, P)
        class(atmosphere), intent(in) :: atmo
        real(rk), intent(in)          :: Z
        real(rk), intent(out)         :: T, P
        real(rk)                      :: P_i
        integer                       :: i
        ! Error handle the validity of the tabulated atmosphere
        if (Z < zrange(0) .or. Z > zrange(7)) then
            error stop "Geopotential altitude is outside atmosphere table"
        end if
        ! Check if Z is at the last boundary point
        if (Z == zrange(7)) then
            T = T_i(6) + T_prime(6) * (zrange(7) - zrange(6))
            P = P_initial(7)
            return
        end if
        ! Find the range Z is in and interpolate/define values
        do i = 0, size(T_prime) - 1
            if ((Z >= zrange(i)) .and. (Z < zrange(i+1))) then
                ! Temperature at Z
                T = T_i(i) + T_prime(i) * (Z - zrange(i))
                P_i = P_initial(i)
                ! Pressure at Z
                if (T_prime(i) == 0.0_rk) then
                    P = P_i * e ** ((-gssl_si * (Z - zrange(i))) / (r_air * T_i(i)))
                else
                    P = P_i * (T / T_i(i)) ** ((-gssl_si) / (r_air * T_prime(i)))
                end if
                return
            end if
        end do
        ! Defensive fallback: should never occur after the checks above.
        error stop "Atmosphere layer lookup failed"
    end subroutine get_temperature_and_pressure_from_geopotential_si

    subroutine get_temperature_and_pressure_from_geopotential_us(atmo, Z_ft, T, P)
        class(atmosphere), intent(in) :: atmo
        real(rk), intent(in)          :: Z_ft
        real(rk), intent(out)         :: T, P
        call atmo%temp_and_pressure_gp_si(Z_ft*ft_to_m, T, P)
        T = k_to_r(T) ! Converts to Rankine
        P = P / psf_to_pa ! Converts to PSF (Pounds per Square Foot)
    end subroutine get_temperature_and_pressure_from_geopotential_us

    subroutine get_temperature_and_pressure_from_geometric_si(atmo, H_m, T, P)
        class(atmosphere), intent(in) :: atmo
        real(rk), intent(in)          :: H_m
        real(rk), intent(out)         :: T, P
        real(rk)                      :: Z, P_i
        integer                       :: i
        ! Find geopotential
        Z = atmo%gm2gp(H_m)
        ! Error handle the validity of the tabulated atmosphere
        if (Z < zrange(0) .or. Z > zrange(7)) then
            error stop "Geopotential altitude is outside atmosphere table"
        end if
        ! Check if Z is at the last boundary point
        if (Z == zrange(7)) then
            T = T_i(6) + T_prime(6) * (zrange(7) - zrange(6))
            P = P_initial(7)
            return
        end if
        ! Find the range Z is in and interpolate/define values
        do i = 0, size(T_prime) - 1
            if ((Z >= zrange(i)) .and. (Z < zrange(i+1))) then
                ! Temperature at Z
                T = T_i(i) + T_prime(i) * (Z - zrange(i))
                P_i = P_initial(i)
                ! Pressure at Z
                if (T_prime(i) == 0.0_rk) then
                    P = P_i * e ** ((-gssl_si * (Z - zrange(i))) / (r_air * T_i(i)))
                else
                    P = P_i * (T / T_i(i)) ** ((-gssl_si) / (r_air * T_prime(i)))
                end if
                return
            end if
        end do
        ! Defensive fallback: should never occur after the checks above.
        error stop "Atmosphere layer lookup failed"
    end subroutine get_temperature_and_pressure_from_geometric_si

    subroutine get_temperature_and_pressure_from_geometric_us(atmo, H_ft, T, P)
        class(atmosphere), intent(in) :: atmo
        real(rk), intent(in)          :: H_ft
        real(rk), intent(out)         :: T, P
        call atmo%temp_and_pressure_gm_si(H_ft*ft_to_m, T, P)
        T = k_to_r(T) ! Converts to Rankine
        P = P / psf_to_pa ! Converts to PSF (Pounds per Square Foot)
    end subroutine get_temperature_and_pressure_from_geometric_us

end module atmosphere_m