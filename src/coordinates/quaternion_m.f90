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
        procedure :: qb2qe            => q_body_to_earth
        procedure :: qe2qb            => q_earth_to_body
        procedure :: mag              => q_magnitude
        procedure :: norm             => q_normalize
        procedure :: conj             => q_conjugate
        procedure :: get_euler_angles => q_get_euler_angles
        procedure :: get_euler_axis   => q_get_euler_axis
        procedure :: get_rot_mat      => q_get_rotation_matrix
        procedure :: q_rate           => q_time_derivative
    end type quat

    interface quaternion
        procedure create_euler_rodrigues_reals
        procedure create_euler_axis_array
        procedure create_euler_axis_vector
        procedure create_euler_angles
        procedure create_rotation_matrix
        procedure create_vector
        procedure create_real_array
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

    function create_euler_rodrigues_reals(e0, ex, ey, ez) result(q)
        real(rk), intent(in) :: e0, ex, ey, ez
        type(quat) :: q
        q%o = e0
        q%x = ex
        q%y = ey
        q%z = ez
    end function create_euler_rodrigues_reals

    function create_euler_axis_array(Thetad, Euler) result(q)
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
    end function create_euler_axis_array

    function create_euler_axis_vector(Thetad, Euler) result(q)
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
    end function create_euler_axis_vector

    function create_euler_angles() result()
    end function create_euler_angles

    !! ============================================
    !!             Quaternion Operations
    !! ============================================



    !! ============================================
    !!               Quaternion Misc.
    !! ============================================



    !! ============================================
    !!          Quaternion Transformations
    !! ============================================

    subroutine body_to_earth(self)
        class(quat), intent(in) :: self
    end subroutine body_to_earth

    subroutine earth_to_body(self)
        class(quat), intent(in) :: self
    end subroutine earth_to_body

    !! ============================================
    !!               Quaternion Gets
    !! ============================================

    subroutine q_get_euler_axis(q, Theta, axis)
        ! Calculates the axis (Ex, Ey, and Ez) by finding Theta in e0 = cos(Theta/2) => Theta = acos(e0) * 2
        ! This relationship comes from the euler-rodrigues relationship
        class(quat), intent(in)   :: q
        type(vector), intent(out) :: axis
        real(rk), intent(out)     :: Theta
        real(rk)                  :: Thetar, s
        Theta = 2.0_rk * acos(q%o)
        if (Theta /= 0.0_rk .and. Theta /= pi) then
            s = sin(Theta/2.0_rk)
            axis = v_from_3_reals(q%x, q%y, q%z)
            axis = axis / s
        else
            axis = v_from_3_reals(0.0_rk, 1.0_rk, 0.0_rk) 
        end if
    end subroutine q_get_euler_axis

    !! ============================================
    !!              Quaternion Display
    !! ============================================

end module quaternion_m