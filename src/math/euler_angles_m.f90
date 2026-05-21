!! This module will contain functions for working with Euler angles.

module euler_angles_m
    use units_m
    use vector_m
    implicit none

contains

    ! Function that first rotates about the z-axis of the base coordinate system by the azimuth angle, psi = phi_z; 0 <= psi <= 360, order 1
    function z_rotation_matrix(psid) result(A)
        real, intent(in)     :: psid       ! psi in degrees
        real, dimension(3,3) :: A
        real                 :: s, c, psir ! psi in radians
        psir = psid * d2r
        s = sin(psir)
        c = cos(psir)
        A = reshape([c, -s, 0.0, s, c, 0.0, 0.0, 0.0, 1.0], shape=[3,3])
    end function z_rotation_matrix

    ! Function that rotates after the z-axis, about the elevation (y-axis), of the base coordinate system by the elevation angle, theta = phi_y; -90 <= psi <= 90, order 2
    function y_rotation_matrix(thetad) result(A)
        real, intent(in)     :: thetad       ! theta in degrees
        real, dimension(3,3) :: A
        real                 :: s, c, thetar ! theta in radians
        thetar = thetad * d2r
        s = sin(thetar)
        c = cos(thetar)
        A = reshape([c, 0.0, s, 0.0, 1.0, 0.0, -s, 0.0, c], shape=[3,3])
    end function y_rotation_matrix

    ! Function that rotates last about the x-axis of the base coordinate system by the bank angle, phi = phi_z; -180 <= phi <= 180, order 3
    function x_rotation_matrix(phid) result(A)
        real, intent(in)     :: phid       ! phi in degrees
        real, dimension(3,3) :: A
        real                 :: s, c, phir ! phi in radians
        phir = phid * d2r
        s = sin(phir)
        c = cos(phir)
        A = reshape([1.0, 0.0, 0.0, 0.0, c, -s, 0.0, s, c], shape=[3,3])
    end function x_rotation_matrix
    
end module euler_angles_m