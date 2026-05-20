! This file contains the unit definitions/conversions for the flight simulation.

module units_m
    implicit none
    !! Constants
    real, parameter :: pi = 3.1415926535897932384626433832795, &
                       g_ssl = 9.80665, &        ! Standard gravity at sea level in m/s^2
                       R_E_Z = 6356766, &        ! Earth radius for Geopotential altitude in m
                       R_E = 6366707.01949371, & ! Mean radius of the earth at sea level in m
                       R = 287.0528, &           ! Gas constant for dry air in J/(kg*K)
                       gamma = 1.4               ! Heat capacity ratio
    
    !! Angle conversions
    real, parameter :: d2r = pi / 180.0, & ! Degrees to radians
                       r2d = 180.0 / pi    ! Radians to degrees
    
    !! English to SI unit conversions
    real, parameter :: ft2m = 0.3048, &           ! Feet to meters
                       lbm2kg = 0.45359237, &     ! Pound-mass to kilograms
                       lbf2N = 4.4482216152605, & ! Pound-force to newtons
                       F2K = 5./9., &             ! Fahrenheit to kelvin
                       slug2kg = 14.5939029, &    ! Slugs to kilograms
                       psf2pa = 47.880258, &      ! Pressure conversion: lbf/ft^2 to pascal, or N/m^2
                       ed2sid = 515.379, &        ! Density conversion: slug/ft^3 to kg/m^3
                       ev2siv = 47.880258         ! Viscosity conversion: slug/(ft*s) to kg/(m*s)

contains
    
end module units_m