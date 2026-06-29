!! This module contains matrix operations
module matrix_m
    use units_m
    use point_m
    use vector_m
    implicit none

    ! ID matrix - reshape converts 1d array into 3d array, filling the columns first
    real, dimension(3,3) :: id_mat = reshape([1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0], shape=[3,3])

    type :: matrix
        real(rk), dimension(3,3) :: mat
    contains
        procedure :: matmult     => m_times_m
        procedure :: transpose   => m_transpose
        procedure :: inv         => m_inverse
        procedure :: init        => m_init
        procedure :: printmat    => m_print
        procedure :: trace       => m_trace
        procedure :: det         => m_determinant
    end type matrix

    interface matrix
        procedure m_from_array
        procedure m_from_9_reals
    end interface matrix

    interface operator (*)
        procedure m_times_s
        procedure s_times_m
        procedure m_times_m
        procedure m_times_v
        procedure v_times_m
    end interface operator (*)

    interface operator (/)
        procedure m_div_s
    end interface operator (/)

    interface operator (+)
        procedure m_add_m
    end interface operator (+)

    interface operator (-)
        procedure m_sub_m
        procedure m_negate
    end interface operator (-)

    interface operator (.inv.)
        procedure m_inverse
    end interface operator (.inv.)

    interface operator (.t.)
        procedure m_transpose
    end interface operator (.t.)

contains

    !! ============================================
    !!               Matrix Creation
    !! ============================================

    subroutine m_init(this, a)
        class(matrix), intent(inout) :: this
        real(rk), intent(in)         :: a(3,3)
        this%mat = a
    end subroutine m_init

    function m_from_array(a) result(mat)
        real(rk), intent(in) :: a(3,3)
        type(matrix)         :: mat
        mat%mat = a
    end function m_from_array

    function m_from_9_reals(m11, m12, m13, m21, m22, m23, m31, m32, m33) result(m)
        real(rk), intent(in) :: m11, m12, m13, m21, m22, m23, m31, m32, m33
        type(matrix)         :: m
        m%mat(1,1) = m11; m%mat(1,2) = m12; m%mat(1,3) = m13
        m%mat(2,1) = m21; m%mat(2,2) = m22; m%mat(2,3) = m23
        m%mat(3,1) = m31; m%mat(3,2) = m32; m%mat(3,3) = m33
    end function m_from_9_reals

    !! ============================================
    !!       Matrix Operations with Matrices
    !! ============================================

    function m_add_m(m1, m2) result(m_sum)
        type(matrix), intent(in) :: m1, m2
        type(matrix)             :: m_sum
        m_sum%mat = m1%mat + m2%mat
    end function m_add_m

    function m_sub_m(m1, m2) result(m_diff)
        type(matrix), intent(in) :: m1, m2
        type(matrix)             :: m_diff
        m_diff%mat = m1%mat - m2%mat
    end function m_sub_m

    function m_negate(mat) result(m_neg)
        type(matrix), intent(in) :: mat
        type(matrix)             :: m_neg
        m_neg%mat = -mat%mat
    end function m_negate

    function m_times_m(m1, m2) result(m_prod)
        class(matrix), intent(in) :: m1, m2
        type(matrix)             :: m_prod
        m_prod%mat = matmul(m1%mat, m2%mat)
    end function m_times_m

    !! ============================================
    !!        Matrix Operations with Vectors
    !! ============================================

    function m_times_v(m, v) result(v_prod)
        type(matrix), intent(in) :: m
        type(vector), intent(in) :: v
        type(vector)             :: v_prod

        v_prod%x = m%mat(1,1)*v%x + m%mat(1,2)*v%y + m%mat(1,3)*v%z
        v_prod%y = m%mat(2,1)*v%x + m%mat(2,2)*v%y + m%mat(2,3)*v%z
        v_prod%z = m%mat(3,1)*v%x + m%mat(3,2)*v%y + m%mat(3,3)*v%z
    end function m_times_v

    function v_times_m(v, m) result(v_prod)
        type(vector), intent(in) :: v
        type(matrix), intent(in) :: m
        type(vector)             :: v_prod

        v_prod%x = v%x*m%mat(1,1) + v%y*m%mat(2,1) + v%z*m%mat(3,1)
        v_prod%y = v%x*m%mat(1,2) + v%y*m%mat(2,2) + v%z*m%mat(3,2)
        v_prod%z = v%x*m%mat(1,3) + v%y*m%mat(2,3) + v%z*m%mat(3,3)
    end function v_times_m

    !! ============================================
    !!        Matrix Operations with Scalars
    !! ============================================

    function m_times_s(m, s) result(ms_prod)
        type(matrix), intent(in) :: m
        real(rk), intent(in)     :: s
        type(matrix)             :: ms_prod
        ms_prod%mat = m%mat * s
    end function m_times_s

    function s_times_m(s, m) result(sm_prod)
        real(rk), intent(in)     :: s
        type(matrix), intent(in) :: m
        type(matrix)             :: sm_prod
        sm_prod%mat = s * m%mat
    end function s_times_m

    function m_div_s(m, s) result(m_div)
        type(matrix), intent(in) :: m
        real(rk), intent(in)     :: s
        type(matrix)             :: m_div

        if (s == 0.0_rk) then
            write(*,*) "Error: Division by zero in m_div_s in matrix_m"
            stop
        end if

        m_div%mat = m%mat / s
    end function m_div_s

    !! ============================================
    !!                  Rotations
    !! ============================================



    !! ============================================
    !!               Matrix Relations
    !! ============================================

    function m_inverse(m) result(m_inv)
        class(matrix), intent(in) :: m
        type(matrix)              :: m_inv

    end function m_inverse

    function m_transpose(m) result(mt)
        ! Flips it over its primary diagonal, effectively swapping its rows and columns
        class(matrix), intent(in) :: m
        type(matrix)              :: mt
        integer                   :: row, col
        do row = 1, 3
            do col = 1, 3
                mt%mat(col, row) = m%mat(row, col)
            end do
        end do
    end function m_transpose

    function m_trace(this) result(tr)
        ! Adding up all the numbers along its main diagonal
        class(matrix), intent(in) :: this
        real(rk)                  :: tr
        tr = this%mat(1,1) + this%mat(2,2) + this%mat(3,3)
    end function m_trace

    function m_determinant(this) result(det)
        ! Scaling factor of a linear transformation
        ! Measures how much a matrix expands or compresses the area (in 2D space) or volume (in 3D space) of a region
        class(matrix), intent(in) :: this
        real(rk)                  :: det
        det = this%mat(1,1)*(this%mat(2,2)*this%mat(3,3) - this%mat(2,3)*this%mat(3,2)) &
            - this%mat(1,2)*(this%mat(2,1)*this%mat(3,3) - this%mat(2,3)*this%mat(3,1)) &
            + this%mat(1,3)*(this%mat(2,1)*this%mat(3,2) - this%mat(2,2)*this%mat(3,1))
    end function m_determinant

    !! ============================================
    !!               Matrix Display
    !! ============================================

    subroutine m_print(this)
        class(matrix), intent(in) :: this
        write(*,'(3F12.4)') this%mat(1,1), this%mat(1,2), this%mat(1,3)
        write(*,'(3F12.4)') this%mat(2,1), this%mat(2,2), this%mat(2,3)
        write(*,'(3F12.4)') this%mat(3,1), this%mat(3,2), this%mat(3,3)
    end subroutine m_print

end module matrix_m