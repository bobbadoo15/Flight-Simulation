module atmosphere_m
    use units_m

    type :: atmosphere
    contains
    end type atmosphere

contains

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

    function geometric2geopotential(H) result(Z)
        real(rk), intent(in) :: H
        real(rk)             :: Z
        Z = (r_e_z * H) / (r_e_z + H) ! In metric
    end function geometric2geopotential

    function geopotential2geometric(Z) result(H)
        real(rk), intent(in) :: Z
        real(rk)             :: H
        H = (Z * r_e_z) / (r_e_z - Z) ! In metric
    end function geopotential2geometric

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
        g = get_gravity_si(H_ft * ft_to_m) * m_to_ft
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

end module atmosphere_m