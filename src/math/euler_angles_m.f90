!! This module will contain functions for working with Euler angles.

module euler_angles_m
    use units_m
    use vector_m
    implicit none

    type rot_mat
        real, dimension(3,3) :: mat
    contains        
    end type rot_mat

contains

    !! =========================================
    !!                SUBROUTINES
    !! =========================================

    subroutine rotation_matrix(m)
        class(rot_mat), intent(inout) :: m
        
    end subroutine rotation_matrix

    subroutine rotation_matrix_transpose(m)
        class(rot_mat), intent(inout) :: m
    end subroutine rotation_matrix_transpose

    !! =========================================
    !!                 FUNCTIONS
    !! =========================================

    ! Function that first rotates about the z-axis of the base coordinate system by the azimuth angle, psi = phi_z; 0 <= psi <= 360, order 1
    function z_rotation_matrix(psid) result(z_mat)
        real, intent(in)     :: psid       ! psi in degrees
        real, dimension(3,3) :: z_mat
        real                 :: s, c, psir ! psi in radians
        ! Error handling for angle range
        if ((psid >= 0.0) .and. (psid <= 360.0)) then
            psir = psid * d2r
            s = sin(psir)
            c = cos(psir)
            z_mat = reshape([c, -s, 0.0, s, c, 0.0, 0.0, 0.0, 1.0], shape=[3,3])
        else
            z_mat = 0.0
            write (*,*) "The azimuth angle is not in the range between 0 and 360."            
        end if
    end function z_rotation_matrix

    ! Function that rotates after the z-axis, about the elevation (y-axis), of the base coordinate system by the elevation angle, theta = phi_y; -90 <= psi <= 90, order 2
    function y_rotation_matrix(thetad) result(y_mat)
        real, intent(in)     :: thetad       ! theta in degrees
        real, dimension(3,3) :: y_mat
        real                 :: s, c, thetar ! theta in radians
        ! Error handling for angle range
        if ((thetad >= -90.0) .and. (thetad <= 90.0)) then
            thetar = thetad * d2r
            s = sin(thetar)
            c = cos(thetar)
            y_mat = reshape([c, 0.0, s, 0.0, 1.0, 0.0, -s, 0.0, c], shape=[3,3])
        else
            y_mat = 0.0
            write (*,*) "The elevation angle is not in the range between -90 and 90."            
        end if
    end function y_rotation_matrix

    ! Function that rotates last about the x-axis of the base coordinate system by the bank angle, phi = phi_z; -180 <= phi <= 180, order 3
    function x_rotation_matrix(phid) result(x_mat)
        real, intent(in)     :: phid       ! phi in degrees
        real, dimension(3,3) :: x_mat
        real                 :: s, c, phir ! phi in radians
        ! Error handling for angle range
        if ((phid >= -180.0) .and. (phid <= 180.0)) then
            phir = phid * d2r
            s = sin(phir)
            c = cos(phir)
            x_mat = reshape([1.0, 0.0, 0.0, 0.0, c, -s, 0.0, s, c], shape=[3,3])
        else
            x_mat = 0.0
            write (*,*) "The bank angle is not in the range between -180 and 180."            
        end if
    end function x_rotation_matrix
    
end module euler_angles_m