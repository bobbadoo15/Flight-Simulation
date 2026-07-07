module point_m
    use units_m, only: rk, eps
    implicit none

    type :: point
        real(rk) :: x, y, z
    contains
        procedure :: init       => p_init
        procedure :: printpoint => p_print
    end type point

    interface operator (+)
        procedure p_add
    end interface operator (+)

    interface operator (-)
        procedure p_sub
        procedure p_negate
    end interface operator (-)

    interface operator (==)
        procedure p_equal
    end interface operator (==)

    interface operator (/=)
        procedure p_not_equal
    end interface operator (/=)

contains

    !! ============================================
    !!                Point Creation
    !! ============================================

    function p_create_array(a) result(p)
        ! Creates/returns a point in array form
        real(rk), intent(in) :: a(3) ! Rank-1 with 3 elements
        type(point)          :: p
        p%x = a(1)
        p%y = a(2)
        p%z = a(3)
    end function p_create_array

    function p_create_real(x, y, z) result(p)
        ! Creates and returns a scalar point
        real(rk), intent(in) :: x, y, z
        type(point)          :: p
        p%x = x
        p%y = y
        p%z = z
    end function p_create_real

    subroutine p_init(p, x, y, z)
        ! Initializes/modifies an existing point passed in as "p"
        class(point), intent(inout) :: p
        real(rk), intent(in)        :: x, y, z
        p%x = x
        p%y = y
        p%z = z
    end subroutine p_init

    !! ============================================
    !!              Point Operations
    !! ============================================

    function p_add(p1, p2) result(p_sum)
        ! Adds two points together
        type(point), intent(in) :: p1, p2
        type(point)             :: p_sum
        p_sum%x = p1%x + p2%x ! Adds the x-values of the points
        p_sum%y = p1%y + p2%y ! Adds the y-values of the points
        p_sum%z = p1%z + p2%z ! Adds the z-values of the points
    end function p_add

    function p_negate(p) result(p_neg)
        ! Changes the sign of the point's components
        type(point), intent(in) :: p
        type(point)             :: p_neg
        p_neg%x = -p%x ! Changes the x-value's sign
        p_neg%y = -p%y ! Changes the y-value's sign
        p_neg%z = -p%z ! Changes the z-value's sign
    end function p_negate

    function p_sub(p1, p2) result(p_diff)
        ! Subtracts two points
        type(point), intent(in) :: p1, p2
        type(point)             :: p_diff
        p_diff%x = p1%x - p2%x ! Subtracts the x-values of the points
        p_diff%y = p1%y - p2%y ! Subtracts the y-values of the points
        p_diff%z = p1%z - p2%z ! Subtracts the z-values of the points
    end function p_sub

    !! ============================================
    !!               Point Comparison
    !! ============================================

    function p_equal(p1, p2) result(equal)
        ! This compares two points if they are equal
        ! Displays "True" if they are equal
        type(point), intent(in) :: p1, p2
        logical                 :: equal
        equal = (p1%x == p2%x) .and. (p1%y == p2%y) &
                .and. (p1%z == p2%z)
    end function p_equal

    function p_not_equal(p1, p2) result(not_equal)
        ! This compares two points if they are not equal
        ! Displays "True" if they are not equal
        type(point), intent(in) :: p1, p2
        logical                 :: not_equal
        not_equal = .not. p_equal(p1, p2)
    end function p_not_equal

    !! ============================================
    !!               Point Display
    !! ============================================

    subroutine p_print(p)
        ! Displays a given point
        class(point), intent(in) :: p
        write(*,'(A,F20.15,A,F20.15,A,F20.15,A)') &
            '(', p%x, ', ', p%y, ', ', p%z, ')'
    end subroutine p_print

end module point_m