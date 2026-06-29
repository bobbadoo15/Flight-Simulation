!! This module contains mathematical operations in relation to vectors

module vector_m
    use units_m, only: rk, eps
    use point_m, only: point
    implicit none
    
    type :: vector
        real(rk) :: x, y, z
    contains
        ! This procedure calls the function whenever its 'nickname is used'
        procedure :: mag           => v_magnitude
        procedure :: init          => v_init
        procedure :: printvector   => v_print
        procedure :: convert2point => v_convert2point
    end type vector

    interface vector
        procedure v_from_3_reals
        procedure v_from_array
        procedure v_from_point
    end interface vector

    ! Whenever + is used with the vector type, this operator calls the following functions
    interface operator (+)
        procedure v_add_v
        procedure v_add_p
        procedure p_add_v
    end interface operator (+)

    ! Whenever - is used with the vector type, this operator calls the following functions
    interface operator (-)
        procedure v_sub_v
        procedure v_negate
        procedure p_sub_v
    end interface operator (-)

    ! Whenever * is used with the vector type, this operator calls the following functions
    interface operator (*)
        procedure v_times_s
        procedure s_times_v
    end interface operator (*)

    ! Whenever / is used with the vector type, this operator calls the following functions
    interface operator (/)
        procedure v_div_s
    end interface operator (/)

    ! Whenever .cross. is used with the vector type, this operator calls the following functions
    interface operator (.cross.)
        procedure v_cross_v
    end interface operator (.cross.)

    ! Whenever .dot. is used with the vector type, this operator calls the following functions
    interface operator (.dot.)
        procedure v_dot_v
    end interface operator (.dot.)

    ! Whenever .create. is used with the vector type, this operator calls the following functions
    interface operator (.create.)
        procedure v_new
    end interface operator (.create.)

contains

    !! ============================================
    !!               Vector Creation
    !! ============================================

    function v_from_3_reals(x, y, z) result(v)
        real(rk), intent(in) :: x, y, z
        type(vector)         :: v
        v%x = x
        v%y = y
        v%z = z
    end function v_from_3_reals

    function v_from_array(a) result(v)
        real(rk), intent(in) :: a(3)
        type(vector)         :: v
        v%x = a(1)
        v%y = a(2)
        v%z = a(3)
    end function v_from_array

    function v_from_point(p) result(v)
        type(point), intent(in) :: p
        type(vector)            :: v
        v%x = p%x
        v%y = p%y
        v%z = p%z
    end function v_from_point

    subroutine v_init(this, x, y, z)
        class(vector), intent(inout) :: this
        real(rk), intent(in) :: x, y, z
        this%x = x
        this%y = y
        this%z = z
    end subroutine v_init

    function v_new(p1, p2) result(v)
        ! Creates a vector between two points
        type(point), intent(in) :: p1, p2
        type(vector)            :: v
        v%x = p2%x - p1%x ! Takes the difference of x for direction
        v%y = p2%y - p1%y ! Takes the difference of y for direction
        v%z = p2%z - p1%z ! Takes the difference of z for direction
    end function v_new

    !! ============================================
    !!        Vector Operations with Vectors
    !! ============================================

    ! Adding vectors
    function v_add_v(v1, v2) result(v_sum)
        type(vector), intent(in) :: v1, v2
        type(vector)             :: v_sum
        v_sum%x = v1%x + v2%x
        v_sum%y = v1%y + v2%y
        v_sum%z = v1%z + v2%z
    end function v_add_v

    ! Cross multiplying vectors
    function v_cross_v(v1, v2) result(v_cross)
        type(vector), intent(in) :: v1, v2
        type(vector)             :: v_cross
        v_cross%x = v1%y*v2%z - v1%z*v2%y
        v_cross%y = v1%z*v2%x - v1%x*v2%z
        v_cross%z = v1%x*v2%y - v1%y*v2%x
    end function v_cross_v

    ! Dot multiplying vectors
    function v_dot_v(v1, v2) result(v_dot)
        type(vector), intent(in) :: v1, v2
        real(rk)                 :: v_dot
        v_dot = v1%x*v2%x + v1%y*v2%y + v1%z*v2%z
    end function v_dot_v

    ! Switch the signs of the vector
    function v_negate(v) result(v_neg)
        type(vector), intent(in) :: v
        type(vector)             :: v_neg
        v_neg%x = -v%x
        v_neg%y = -v%y
        v_neg%z = -v%z
    end function v_negate

    ! Subtracting vectors
    function v_sub_v(v1, v2) result(v_diff)
        type(vector), intent(in) :: v1, v2
        type(vector)             :: v_diff
        v_diff%x = v1%x - v2%x
        v_diff%y = v1%y - v2%y
        v_diff%z = v1%z - v2%z
    end function v_sub_v

    !! ============================================
    !!        Vector Operations with Scalars
    !! ============================================

    ! Dividing a vector with a scalar
    function v_div_s(v, s) result(v_div)
        type(vector), intent(in) :: v
        real(rk), intent(in)     :: s
        type(vector)             :: v_div

        ! Check for if s = 0 to avoid dividing by zero
        if (s == 0.0_rk) then
            write(*,*) "Error: Division by zero in v_div_s in vector_m"
            stop
        end if

        ! Proceed to divide by s
        v_div%x = v%x / s
        v_div%y = v%y / s
        v_div%z = v%z / s
    end function v_div_s

    ! Multiplying a vector with a scalar
    function v_times_s(v, s) result(vs_prod)
        type(vector), intent(in) :: v
        real(rk), intent(in)     :: s
        type(vector)             :: vs_prod
        vs_prod%x = v%x * s
        vs_prod%y = v%y * s
        vs_prod%z = v%z * s
    end function v_times_s

    ! Multiplying a scalar with a vector
    function s_times_v(s, v) result(sv_prod)
        real(rk), intent(in)     :: s
        type(vector), intent(in) :: v
        type(vector)             :: sv_prod
        sv_prod%x = s * v%x
        sv_prod%y = s * v%y
        sv_prod%z = s * v%z
    end function s_times_v

    !! ============================================
    !!        Vector Operations with Points
    !! ============================================

    ! Adding point to a vector
    function p_add_v(p, v) result(p_out)
        type(point), intent(in)  :: p
        type(vector), intent(in) :: v
        type(point)              :: p_out
        p_out%x = p%x + v%x
        p_out%y = p%y + v%y
        p_out%z = p%z + v%z
    end function p_add_v

    ! Subtracting point by vector
    function p_sub_v(p, v) result(p_out)
        type(point),  intent(in) :: p
        type(vector), intent(in) :: v
        type(point)              :: p_out
        p_out%x = p%x - v%x
        p_out%y = p%y - v%y
        p_out%z = p%z - v%z
    end function p_sub_v

    ! Adding vector to point
    function v_add_p(v, p) result(p_out)
        type(vector), intent(in) :: v
        type(point), intent(in)  :: p
        type(point)              :: p_out
        p_out%x = v%x + p%x
        p_out%y = v%y + p%y
        p_out%z = v%z + p%z
    end function v_add_p

    !! ============================================
    !!                 Conversions
    !! ============================================

    function v_convert2point(this) result(p)
        class(vector), intent(in) :: this
        type(point)               :: p
        call p%init(this%x, this%y, this%z)
    end function v_convert2point

    !! ============================================
    !!            Misc. Vector Relations
    !! ============================================

    function v_magnitude(this) result(mag)
        class(vector), intent(in) :: this
        real(rk)                  :: mag
        mag = sqrt(this%x ** 2.0 + this%y ** 2.0 + this%z ** 2.0)
    end function v_magnitude
    
    function v_normalize(this) result(v_norm)
        class(vector), intent(in) :: this
        real(rk)                  :: mag
        type(vector)              :: v_norm

        ! Check for if mag = 0 to avoid dividing by 0
        if (mag == 0.0) then
            write(*,*) "Error: Division by zero in v_normalize in vector_m"
            stop
        end if

        ! Check if the magnitude is smaller than the tolerance
        mag = this%mag()
        if (mag <= eps) then
            write(*,*) "Error: Cannot normalize a zero vector in vector_m"
            stop
        end if

        v_norm = this / mag
    end function v_normalize

    subroutine v_print(this)
        ! Displays the vector
        class(vector), intent(in) :: this
        write(*,'(A,F0.4,A,F0.4,A,F0.4,A)') &
            '(', this%x, ', ', this%y, ', ', this%z, ')'
    end subroutine v_print

end module vector_m