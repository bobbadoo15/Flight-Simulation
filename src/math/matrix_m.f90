!! This module contains matrix operations
module matrix_m
    use vector_m
    implicit none

    type :: matrix
    contains
        procedure :: matmult => m_times_m
        procedure :: mattran => m_transpose
        procedure :: matinv  => m_inverse
    end type matrix

    interface operator (*)
        procedure m_times_m
    end interface operator (*)

    interface operator (/)
    end interface operator (/)

    interface operator (+)
    end interface operator (+)

    interface operator (-)
    end interface operator (-)

    interface operator (.inv.)
        procedure m_inverse
    end interface operator (.inv.)

    interface operator (.t.)
        procedure m_transpose
    end interface operator (.t.)

    ! ID matrix - reshape converts 1d array into 3d array, filling the columns first
    real, dimension(3,3) :: id_mat = reshape([1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0], shape=[3,3])

contains

    !! ============================================
    !!              Matrix Operations
    !! ============================================

    ! Matrix multiplication
    function m_inverse()
    end function m_inverse

    function m_times_m(A, B)
        

    end function m_times_m

    ! Matrix transposition
    function m_transpose()
    end function m_transpose

end module matrix_m