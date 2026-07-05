module euler_angles_m
    use units_m
    use vector_m
    use matrix_m
    implicit none

    type rot_mat
        real(rk), dimension(3,3) :: mat
    contains
        procedure :: e2b   => earth_to_body
        procedure :: b2e   => body_to_earth
        procedure :: rot_z => z_rotation_matrix
        procedure :: rot_y => y_rotation_matrix
        procedure :: rot_x => x_rotation_matrix
        procedure :: rates => rotation_rates
    end type rot_mat

contains

    !! =========================================
    !!             SYSTEM ROTATIONS
    !! =========================================

    subroutine body_to_earth(self, body, phid, thetad, psid, earth)
        ! Converts from rotated state to original state using euler angles 
        ! (noninertial/body-fixed to inertial/earth-fixed)
        class(rot_mat), intent(in) :: self
        type(vector), intent(in)   :: body
        type(vector), intent(out)  :: earth
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
        m = matrix((c_theta*c_psi), ((s_phi*s_theta*c_psi) - (c_phi*s_psi)), ((c_phi*s_theta*c_psi) + (s_phi*s_psi)), &
                   (c_theta*s_psi), ((s_phi*s_theta*s_psi) + (c_phi*c_psi)), ((c_phi*s_theta*s_psi) - (s_phi*c_psi)), &
                   (-s_theta), (s_phi*c_theta), (c_phi*c_theta))
        ! Compute New Matrix
        earth = m * body
    end subroutine body_to_earth

    subroutine earth_to_body(self, earth, phid, thetad, psid, body, m_out)
        ! Converts from original to rotated state using euler angles 
        ! (inertial/earth-fixed to noninertial/body-fixed)
        class(rot_mat), intent(in) :: self
        class(vector), intent(in)  :: earth
        type(vector), intent(out)  :: body
        type(matrix), intent(out)  :: m_out
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
        body = m * earth
        ! Export Transformed Matrix
        m_out = m
    end subroutine earth_to_body

    !! =========================================
    !!          SYSTEM ROTATIONS RATES
    !! =========================================

    subroutine rotation_rates(self, w, phid, thetad, rates)
        class(rot_mat), intent(in) :: self
        class(vector), intent(in)  :: w                  ! w vector is in p, q, r
        type(vector), intent(out)  :: rates
        type(matrix)               :: m
        real(rk), intent(in)       :: phid, thetad
        real(rk)                   :: phir, thetar, &
                                      c_phi, c_theta, &
                                      s_phi, s_theta
        ! Convert Deg to Rad
        phir    = phid * d2r
        thetar  = thetad * d2r
        ! Compute angles
        c_phi   = cos(phir)
        c_theta = cos(thetar)
        s_phi   = sin(phir)
        s_theta = sin(thetar)
        ! Check for cos()= +-90
        if (abs(c_theta) <= eps) then
            write(*,*) "Error: Singularity in rotation_rates — theta near 90 degrees"
            stop
        end if
        ! Compute Transformation Matrix
        m = matrix((1.0_rk), ((s_phi*s_theta)/c_theta), ((c_phi*s_theta)/c_theta), &
                   (0.0_rk), (c_phi), (-s_phi), &
                   (0.0_rk), (s_phi/c_theta), (c_phi/c_theta))
        ! Compute New Matrix
        rates = m*w
    end subroutine rotation_rates

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

    ! subroutine euler_formula(self, v, Theta)
    !     ! Individually rotates an axis by theta
    !     class(rot_mat), intent(in) :: self
    !     class(vector), intent(in)  :: v
    !     real(rk), intent(in)       :: Theta
    !     real(rk)                   :: unity
    !     ! Check for unity
    !     ! unity = v%x**2 + v%y**2 + v%z**2
    !     ! if (unity == 1) then
    !     ! end if
    ! end subroutine euler_formula
    
end module euler_angles_m