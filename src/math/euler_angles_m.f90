module euler_angles_m
    use units_m
    use vector_m
    use matrix_m
    implicit none

    type rot_mat
        real(rk), dimension(3,3) :: mat
    contains
        procedure :: z_rot => z_rotation_matrix
        procedure :: y_rot => y_rotation_matrix
        procedure :: x_rot => x_rotation_matrix
    end type rot_mat

contains

    !! =========================================
    !!                SUBROUTINES
    !! =========================================

    subroutine rotation_matrix(v, phid, thetad, psid, mat_rot)
        ! Converts from original to rotated state using euler angles
        class(vector), intent(in)  :: v
        type(vector), intent(out)  :: mat_rot
        ! type(matrix), intent(out)  :: m_out
        type(matrix)               :: m
        real(rk), intent(in)       :: phid, thetad, psid
        real(rk)                   :: phir, thetar, psir, &
                                      c_phi, c_psi, c_theta, &
                                      s_phi, s_psi, s_theta
        ! Convert Deg to Rad
        phir    = phid * d2r
        thetar  = thetad * d2r
        psir    = psid * d2r
        ! Compute angles
        c_phi   = cos(phir)
        c_psi   = cos(psir)
        c_theta = cos(thetar)
        s_phi   = sin(phir)
        s_psi   = sin(psir)
        s_theta = sin(thetar)
        ! Compute Transformation Matrix
        m = matrix((c_theta*c_psi), (c_theta*s_psi), (-s_theta), &
                   ((s_phi*s_theta*c_psi) - (c_phi*s_psi)), ((s_phi*s_theta*s_psi) + (c_phi*c_psi)), (s_phi*c_theta), &
                   ((c_phi*s_theta*c_psi) + (s_phi*s_psi)), ((c_phi*s_theta*s_psi) - (s_phi*c_psi)), (c_phi*c_theta))
        ! Compute New Matrix
        mat_rot = m*v
        ! Export Transformed Matrix
        ! m_out = m
    end subroutine rotation_matrix

    !! =========================================
    !!           INDIVIDUAL ROTATIONS
    !! =========================================

    ! Function that first rotates about the z-axis of the base coordinate system by the azimuth angle, psi = phi_z; 0 <= psi <= 360, order 1
    function z_rotation_matrix(self, psid) result(z_mat)
        class(rot_mat), intent(in) :: self
        real(rk), intent(in)       :: psid       ! psi in degrees
        real(rk)                   :: s, c, psir ! psi in radians
        real(rk), dimension(3,3)   :: z_mat
        ! Error handling for angle range
        if ((psid >= 0.0_rk) .and. (psid <= 360.0_rk)) then
            psir = psid * d2r
            s = sin(psir)
            c = cos(psir)
            z_mat = reshape([c, -s, 0.0_rk, s, c, 0.0_rk, 0.0_rk, 0.0_rk, 1.0_rk], shape=[3,3])
        else
            z_mat = 0.0_rk
            write (*,*) "The azimuth angle is not in the range between 0 and 360."            
        end if
    end function z_rotation_matrix

    ! Function that rotates after the z-axis, about the elevation (y-axis), of the base coordinate system by the elevation angle, theta = phi_y; -90 <= psi <= 90, order 2
    function y_rotation_matrix(self, thetad) result(y_mat)
        class(rot_mat), intent(in) :: self
        real(rk), intent(in)       :: thetad       ! theta in degrees
        real(rk)                   :: s, c, thetar ! theta in radians
        real(rk), dimension(3,3)   :: y_mat
        ! Error handling for angle range
        if ((thetad >= -90.0_rk) .and. (thetad <= 90.0_rk)) then
            thetar = thetad * d2r
            s = sin(thetar)
            c = cos(thetar)
            y_mat = reshape([c, 0.0_rk, s, 0.0_rk, 1.0_rk, 0.0_rk, -s, 0.0_rk, c], shape=[3,3])
        else
            y_mat = 0.0_rk
            write (*,*) "The elevation angle is not in the range between -90 and 90."            
        end if
    end function y_rotation_matrix

    ! Function that rotates last about the x-axis of the base coordinate system by the bank angle, phi = phi_z; -180 <= phi <= 180, order 3
    function x_rotation_matrix(self, phid) result(x_mat)
        class(rot_mat), intent(in) :: self
        real(rk), intent(in)       :: phid       ! phi in degrees
        real(rk)                   :: s, c, phir ! phi in radians
        real(rk), dimension(3,3)   :: x_mat
        ! Error handling for angle range
        if ((phid >= -180.0_rk) .and. (phid <= 180.0_rk)) then
            phir = phid * d2r
            s = sin(phir)
            c = cos(phir)
            x_mat = reshape([1.0_rk, 0.0_rk, 0.0_rk, 0.0_rk, c, -s, 0.0_rk, s, c], shape=[3,3])
        else
            x_mat = 0.0_rk
            write (*,*) "The bank angle is not in the range between -180 and 180."            
        end if
    end function x_rotation_matrix
    
end module euler_angles_m