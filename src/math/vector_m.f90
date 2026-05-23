!! This module contains mathematical operations in relation to vectors

module vector_m
    use units_m
    implicit none

    ! ID matrix - reshape converts 1d array into 3d array, filling the columns first
    real, dimension(3,3) :: id_mat = reshape([1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0], shape=[3,3])
    
    type :: vector
        real :: x, y, z
    contains
        ! This procedure calls the function whenever its 'nickname is used'
    end type vector

    ! Whenever + is used with the vector type, this operator calls the following functions
    interface operator (+)
        procedure v_add_v
    end interface operator (+)

    ! Whenever - is used with the vector type, this operator calls the following functions
    interface operator (-)
        procedure v_sub_v
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
    !!              Vector Operations
    !! ============================================

    ! Adding vectors
    function v_add_v(v1, v2)
        class(vector), intent(in) :: v1, v2
    end function v_add_v

    ! Cross multiplying vectors
    function v_cross_v(v1, v2)
        class(vector), intent(in) :: v1, v2
    end function v_cross_v

    ! Dot multiplying vectors
    function v_dot_v(v1, v2)
        class(vector), intent(in) :: v1, v2
    end function v_dot_v

    ! Subtracting vectors
    function v_sub_v(v1, v2)
        class(vector), intent(in) :: v1, v2
    end function v_sub_v

    !! ============================================
    !!        Vector Operations with Scalars
    !! ============================================

    ! Dividing a vector with a scalar
    function v_div_s(v1, v2)
        class(vector), intent(in) :: v1, v2
    end function v_div_s

    ! Multiplying a vector with a scalar
    function v_times_s(v1, v2)
        class(vector), intent(in) :: v1, v2
    end function v_times_s

    ! Multiplying a scalar with a vector
    function s_times_v(v1, v2)
        class(vector), intent(in) :: v1, v2
    end function s_times_v

    !! ============================================
    !!        Vector Operations with Points
    !! ============================================

    !! ============================================
    !!            Misc. Vector Relations
    !! ============================================

    function v_create()
    end function v_create

end module vector_m