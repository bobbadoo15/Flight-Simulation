module aero_model_m
    use units_m
    use vector_m
    implicit none

    type :: aerodynamic
    contains
        procedure :: aoa    => angleofattack
        procedure :: flank  => flankangle
        procedure :: ssa    => sideslipangle
        procedure :: vmag   => freestreammagnitude
        procedure :: wind   => wind_velocity_from_trad_angles
        procedure :: wind_f => wind_velocity_using_flank
    end type aerodynamic

contains

    !! ==========================================================
    !!              TRADITIONAL AERODYNAMIC ANGLES
    !! ==========================================================

    function angleofattack(aero, u, w) result(alpha)
        class(aerodynamic), intent(in) :: aero
        real(rk), intent(in)           :: u, w
        real(rk)                       :: alpha
        alpha = atan(w/u)
    end function angleofattack

    function sideslipangle(aero, v, Vfs) result(beta)
        class(aerodynamic), intent(in) :: aero
        real(rk), intent(in)           :: v, Vfs
        real(rk)                       :: beta
        beta = asin(v/Vfs)
    end function sideslipangle

    !! ==========================================================
    !!                       OTHER ANGLES
    !! ==========================================================

    function flankangle(aero, u, v) result(beta_f)
        class(aerodynamic), intent(in) :: aero
        real(rk), intent(in)           :: u, v
        real(rk)                       :: beta_f
        ! Acts like the aoa, but in the sideslip direction
        beta_f = atan(v/u)
    end function flankangle

    !! ==========================================================
    !!                      WIND VELOCITY
    !! ==========================================================

    function wind_velocity_from_trad_angles(aero, Vmag, alpha, beta) result(wind)
        class(aerodynamic), intent(in) :: aero
        real(rk), intent(in)           :: Vmag, alpha, beta
        type(vector)                   :: wind
        wind = s_times_v(Vmag, v_from_array([cos(alpha)*cos(beta), sin(beta), sin(alpha)*cos(beta)]))
    end function wind_velocity_from_trad_angles

    function wind_velocity_using_flank(aero, Vmag, alpha, beta_f) result(wind)
        class(aerodynamic), intent(in) :: aero
        real(rk), intent(in)           :: Vmag, alpha, beta_f
        real(rk)                       :: s
        type(vector)                   :: wind, v
        s = Vmag / sqrt(1 - (sin(alpha))**2 * (sin(beta_f))**2)
        v = v_from_array([cos(alpha)*cos(beta_f), cos(alpha)*sin(beta_f), sin(alpha)*cos(beta_f)])
        wind = s_times_v(s, v)
    end function wind_velocity_using_flank

    !! ==========================================================
    !!                      MISCELLANEOUS
    !! ==========================================================

    function freestreammagnitude(aero, u, v, w) result(Vmag)
        class(aerodynamic), intent(in) :: aero
        real(rk), intent(in)           :: u, v, w
        real(rk)                       :: Vmag
        Vmag = sqrt(u**2 + v**2 + w**2)
    end function freestreammagnitude

end module aero_model_m