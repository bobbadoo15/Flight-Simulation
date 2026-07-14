module quaternion_m
    ! Quaternion notation: {Q} = Q0 + Qx*ix + Qy*iy + Qz*iz
    ! Quaternion Algebra:
        ! Addition:
            ! Cummulative
            ! {A}+{B} = {B}+{A} = {A0+B0} + {Ax+Bx}ix + {Ay+By}iy + {Az+Bz}iz
        ! Product:
            ! ix * ix = -1,  ix * iy = iz,  ix * iz = -iy
            ! iy * ix = -iz, iy * iy = -1,  iy * iz = ix   
            ! iz * ix = iy,  iz * iy = -ix, iz * iz = -1

            ! {A}*{B} =  (A0 + Ax*ix + Ay*iy + Az*iz) * (B0 + Bx*ix + By*iy + Bz*iz)
            !         =  (A0B0 - AxBx - AyBy - AzBz)
            !          + (A0Bx + AxB0 + AyBz - AzBy)ix
            !          + (A0By - AxBz + AyB0 + AzBx)iy
            !          + (A0Bz + AxBy - AyBx + AzB0)iz

            ! Not cummulative: {A}*{B} /= {B}*{A}
            ! Product of two quat. scalars are regular mult.
            ! If scalars are zero:
                ! {A}*{B} = -A\dotB + A\crossB
            ! Magnitude:
                ! |{Q}| = sqrt(Q0**2 + Qx**2 + Qy**2 + Qz**2)
            ! Conjugate:
                ! {Q}* = Q0 - Qxix - Qyiy - Qziz
            ! Quat. Product with Conjugate:
                ! {Q}*{Q}* = Q0**2 + Qx**2 + Qy**2 + Qz**2 = |{Q}|**2
    ! Basic Form of a Quaternion (Euler-Rodrigues symmetric parameters):
        ! |e0| = | cos(Theta/2) |
        ! |ex| = |Exsin(Theta/2)|
        ! |ey| = |Eysin(Theta/2)|
        ! |ez| = |Ezsin(Theta/2)|
    
    use units_m
    use vector_m
    use matrix_m
    use euler_angles_m
    implicit none

    type :: quat
        real(rk) :: o, x, y, z
    contains
        procedure :: qb2e             => q_body_to_earth
        procedure :: qe2b             => q_earth_to_body
        procedure :: mag              => q_magnitude
        procedure :: norm             => q_normalize
        procedure :: conj             => q_conjugate
        procedure :: get_euler_angles => q_get_euler_angles
        procedure :: get_euler_axis   => q_get_euler_axis
        ! procedure :: get_rot_mat      => q_get_rotation_matrix
        ! procedure :: q_rate           => q_time_derivative
    end type quat

    interface quaternion
        procedure create_quaternion_euler_rodrigues_reals
        procedure create_quaternion_euler_axis_array
        procedure create_quaternion_euler_axis_vector
        procedure create_quaternion_euler_angles
        ! procedure create_quaternion_rotation_matrix
        procedure create_quaternion_vector
        procedure create_quaternion_real_array
    end interface quaternion

    interface operator (+)
        procedure q_add_q
    end interface operator (+)

    interface operator (-)
        procedure q_sub_q
    end interface operator (-)

    interface operator (*)
        procedure q_times_q
        procedure scalar_times_q
        procedure q_times_scalar
    end interface operator (*)

contains

    !! ============================================
    !!              Quaternion Creation
    !! ============================================

    function create_quaternion_euler_rodrigues_reals(e0, ex, ey, ez) result(q)
        real(rk), intent(in) :: e0, ex, ey, ez
        type(quat) :: q
        q%o = e0
        q%x = ex
        q%y = ey
        q%z = ez
    end function create_quaternion_euler_rodrigues_reals

    function create_quaternion_euler_axis_array(Thetad, Euler) result(q)
        ! Used to create the axis if an array input is desired
        real(rk), intent(in) :: Thetad, Euler(3)
        real(rk)             :: Theta, s, c
        type(quat)           :: q
        Theta = Thetad * d2r
        s = sin(Theta/2.0_rk)
        c = cos(Theta/2.0_rk)
        q%o = c
        q%x = Euler(1) * s
        q%y = Euler(2) * s
        q%z = Euler(3) * s
    end function create_quaternion_euler_axis_array

    function create_quaternion_euler_axis_vector(Thetad, Euler) result(q)
        ! Used to create the axis if a vector input is desired
        real(rk), intent(in)     :: Thetad
        type(vector), intent(in) :: Euler
        real(rk)                 :: Theta, s, c
        type(quat)               :: q
        Theta = Thetad * d2r
        s = sin(Theta/2.0_rk)
        c = cos(Theta/2.0_rk)
        q%o = c
        q%x = Euler%x * s
        q%y = Euler%y * s
        q%z = Euler%z * s
    end function create_quaternion_euler_axis_vector

    function create_quaternion_euler_angles(phid, thetad, psid) result(q)
        real(rk), intent(in) :: phid, thetad, psid
        real(rk)             :: phir, thetar, psir, &
                                cphi, ctheta, cpsi, sphi, stheta, spsi
        type(quat)           :: q
        ! Convert from degrees to radians
        phir   = phid * d2r
        thetar = thetad * d2r
        psir   = psid * d2r
        ! Create half angles
        cphi   = cos(phir   /2.0_rk)
        ctheta = cos(thetar /2.0_rk)
        cpsi   = cos(psir   /2.0_rk)
        sphi   = sin(phir   /2.0_rk)
        stheta = sin(thetar /2.0_rk)
        spsi   = sin(psir   /2.0_rk)
        ! Create Quaternion - This matrix could be negative; positive is typically used
        q%o = (cphi*ctheta*cpsi) + (sphi*stheta*spsi)
        q%x = (sphi*ctheta*cpsi) - (cphi*stheta*spsi)
        q%y = (cphi*stheta*cpsi) + (sphi*ctheta*spsi)
        q%z = (cphi*ctheta*spsi) - (sphi*stheta*cpsi)
    end function create_quaternion_euler_angles

    ! function create_quaternion_rotation_matrix() result(q)
    ! end function create_quaternion_rotation_matrix

    function create_quaternion_vector(v) result(q)
        type(vector), intent(in) :: v
        type(quat)               :: q
        q%o = 0.0_rk
        q%x = v%x
        q%y = v%y
        q%z = v%z
    end function create_quaternion_vector

    function create_quaternion_real_array(a) result(q)
        real(rk), intent(in) :: a(4)
        type(quat)           :: q
        q%o = a(1)
        q%x = a(2)
        q%y = a(3)
        q%z = a(4)
    end function create_quaternion_real_array

    !! ============================================
    !!             Quaternion Operations
    !! ============================================

    function q_add_q(q1, q2) result(q)
        type(quat), intent(in) :: q1, q2
        type(quat)             :: q
        q%o = q1%o + q2%o
        q%x = q1%x + q2%x
        q%y = q1%y + q2%y
        q%z = q1%z + q2%z
    end function q_add_q

    function q_sub_q(q1, q2) result(q)
        type(quat), intent(in) :: q1, q2
        type(quat)             :: q
        q%o = q1%o - q2%o
        q%x = q1%x - q2%x
        q%y = q1%y - q2%y
        q%z = q1%z - q2%z
    end function q_sub_q

    function scalar_times_q(s, q) result(q_out)
        real(rk), intent(in)   :: s
        type(quat), intent(in) :: q
        type(quat)             :: q_out
        q_out%o = s * q%o
        q_out%x = s * q%x
        q_out%y = s * q%y
        q_out%z = s * q%z
    end function scalar_times_q

    function q_times_scalar(q, s) result(q_out)
        type(quat), intent(in) :: q
        real(rk), intent(in)   :: s
        type(quat)             :: q_out
        q_out%o = q%o * s
        q_out%x = q%x * s
        q_out%y = q%y * s
        q_out%z = q%z * s
    end function q_times_scalar

    function q_times_q(q1, q2) result(q)
        type(quat), intent(in) :: q1, q2
        type(quat)             :: q
        q%o = (q1%o * q2%o) - (q1%x * q2%x) - (q1%y * q2%y) - (q1%z * q2%z)
        q%x = (q1%o * q2%x) + (q1%x * q2%o) + (q1%y * q2%z) - (q1%z * q2%y)
        q%y = (q1%o * q2%y) - (q1%x * q2%z) + (q1%y * q2%o) + (q1%z * q2%x)
        q%z = (q1%o * q2%z) + (q1%x * q2%y) - (q1%y * q2%x) + (q1%z * q2%o)
    end function q_times_q

    !! ============================================
    !!               Quaternion Misc.
    !! ============================================

    function q_conjugate(self) result(q)
        class(quat), intent(in) :: self
        type(quat)              :: q
        q%o =  self%o
        q%x = -self%x
        q%y = -self%y
        q%z = -self%z
    end function q_conjugate

    function q_magnitude(self) result(m)
        class(quat), intent(in) :: self
        real(rk)                :: m
        m = sqrt(self%o**2 + self%x**2 + self%y**2 + self%z**2)
    end function q_magnitude

    subroutine q_normalize(self)
        class(quat), intent(inout) :: self
        real(rk)                   :: m
        m = self%mag()
        if (m <= 0.0_rk) then
            write(*,*) "Division by zero, or negative magnitude, in q_normalize."
            return
        end if
        self%o = self%o / m
        self%x = self%x / m
        self%y = self%y / m
        self%z = self%z / m
    end subroutine q_normalize

    !! ============================================
    !!          Quaternion Transformations
    !! ============================================

    subroutine q_body_to_earth(self, q, body, earth)
        class(quat), intent(in)  :: self
        type(quat), intent(in)   :: q
        type(vector), intent(in) :: body
        type(vector)             :: earth
        type(matrix)             :: m
        ! Transformation Matrix
        m = matrix((q%x**2 + q%o**2 - q%y**2 - q%z**2), (2.0_rk * ((q%x * q%y) - (q%z * q%o))), (2.0_rk * ((q%x * q%z) + (q%y * q%o))), &
                   (2.0_rk * ((q%x * q%y) + (q%z * q%o))), (q%y**2 + q%o**2 - q%x**2 - q%z**2), (2.0_rk * ((q%y * q%z) - (q%x * q%o))), &
                   (2.0_rk * ((q%x * q%z) - (q%y * q%o))), (2.0_rk * ((q%y * q%z) + (q%x * q%o))), (q%z**2 + q%o**2 - q%x**2 - q%y**2))
        ! Compute Earth-fixed vector
        earth = m * body
    end subroutine q_body_to_earth

    subroutine q_earth_to_body(self, q, earth, body)
        class(quat), intent(in)   :: self
        type(quat), intent(in)    :: q
        type(vector), intent(in)  :: earth
        type(vector), intent(out) :: body
        type(matrix)              :: m
        ! Transformation matrix
        m = matrix((q%x**2 + q%o**2 - q%y**2 - q%z**2),    (2.0_rk * ((q%x * q%y) + (q%z * q%o))), (2.0_rk * ((q%x * q%z) - (q%y * q%o))), &
                   (2.0_rk * ((q%x * q%y) - (q%z * q%o))), (q%y**2 + q%o**2 - q%x**2 - q%z**2),    (2.0_rk * ((q%y * q%z) + (q%x * q%o))), &
                   (2.0_rk * ((q%x * q%z) + (q%y * q%o))), (2.0_rk * ((q%y * q%z) - (q%x * q%o))), (q%z**2 + q%o**2 - q%x**2 - q%y**2))
        ! Compute Body-fixed vector
        body = m * earth
    end subroutine q_earth_to_body

    !! ============================================
    !!               Quaternion Gets
    !! ============================================

    function q_get_euler_angles(self, q) result(euler)
        class(quat), intent(in) :: self, q
        real(rk), dimension(3)  :: euler
        ! Check for gimbal lock
        if (abs(q%o*q%y - q%x*q%z - 0.5_rk) < eps) then
            euler(2) = pi / 2.0_rk
            euler(3) = 0.0_rk !! Create if statement for using the previous heading
            euler(1) = 2.0_rk*asin(q%x / cos(pi/4.0_rk)) + euler(3)
        else if (abs(q%o*q%y - q%x*q%z + 0.5) < eps) then
            euler(2) = -pi / 2.0_rk
            euler(3) = 0.0_rk !! Create if statement for using the previous heading
            euler(1) = 2.0_rk*asin(q%x / cos(pi/4.0_rk)) - euler(3)
        else
            euler(1) = atan2((2*(q%o*q%x + q%y*q%z)), (q%o**2 + q%z**2 - q%x**2 - q%y**2))
            euler(2) = asin(2*(q%o*q%y - q%x*q%z))
            euler(3) = atan2((2*(q%o*q%z + q%x*q%y)), (q%o**2 + q%x**2 - q%y**2 - q%z**2))
            if (euler(3) < 0.0_rk) then
                euler(3) = euler(3) + 2.0_rk*pi ! Puts psi within range of 0 to 360 CW
            end if
        end if
    end function q_get_euler_angles

    subroutine q_get_euler_axis(q, Theta, axis)
        ! Calculates the axis (Ex, Ey, and Ez) by finding Theta in e0 = cos(Theta/2) => Theta = acos(e0) * 2
        ! This relationship comes from the euler-rodrigues relationship
        class(quat), intent(in)   :: q
        type(vector), intent(out) :: axis
        real(rk), intent(out)     :: Theta
        real(rk)                  :: s
        Theta = 2.0_rk * acos(q%o)
        s = sin(Theta/2.0_rk)
        if (abs(s) > eps) then
            axis = v_from_3_reals(q%x, q%y, q%z)
            axis = axis / s
        else
            ! Theta near 0 or pi: axis is undefined/arbitrary, pick a default
            axis = v_from_3_reals(0.0_rk, 0.0_rk, 1.0_rk)
        end if
    end subroutine q_get_euler_axis

    !! ============================================
    !!              Quaternion Display
    !! ============================================

end module quaternion_m