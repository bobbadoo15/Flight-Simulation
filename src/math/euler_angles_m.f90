!! This module will contain functions for working with Euler angles.

module euler_angles_m
    implicit none

contains

    ! Function that first rotates about the z-axis of the base coordinate system by the azimuth angle, psi = phi_z; 0 <= psi <= 360, order 1
    function z_rotation_matrix(psi)
        real, intent(in) :: psi
    end function z_rotation_matrix
    
end module euler_angles_m