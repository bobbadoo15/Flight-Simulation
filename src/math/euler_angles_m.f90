!! This module will contain functions for working with Euler angles.

module euler_angles_m
    use units_m
    use vector_m
    implicit none

contains

    ! Function that first rotates about the z-axis of the base coordinate system by the azimuth angle, psi = phi_z; 0 <= psi <= 360, order 1
    function z_rotation_matrix(psi) result(A)
        real, intent(in)     :: psi
        real, dimension(3,3) :: A
        real                 :: s, c
        s = sin(psi)
        c = cos(psi)
        A = reshape([c, -s, 0.0, s, c, 0.0, 0.0, 0.0, 1.0], shape=[3,3])
    end function z_rotation_matrix

    ! Function that rotates after the z-axis, about the elevation (y-axis), of the base coordinate system by the elevation angle, theta = phi_y; -90 <= psi <= 90, order 2
    function y_rotation_matrix(theta) result(A)
        real, intent(in)     :: theta
        real, dimension(3,3) :: A
        real                 :: s, c
        s = sin(theta)
        c = cos(theta)
        A = reshape([c, 0.0, s, 0.0, 1.0, 0.0, -s, 0.0, c], shape=[3,3])
    end function y_rotation_matrix

    ! Function that rotates last about the x-axis of the base coordinate system by the bank angle, phi = phi_z; -180 <= phi <= 180, order 3
    function x_rotation_matrix(phi) result(A)
        real, intent(in)     :: phi
        real, dimension(3,3) :: A
        real                 :: s, c
        s = sin(phi)
        c = cos(phi)
        A = reshape([1.0, 0.0, 0.0, 0.0, c, -s, 0.0, s, c], shape=[3,3])
    end function x_rotation_matrix
    
end module euler_angles_m