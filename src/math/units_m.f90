! This file contains the unit definitions/conversions for the flight simulation.

module units_m
    implicit none
    !! =====================================================
    !!                     CONSTANTS
    !! =====================================================
    real, parameter :: pi    = 3.1415926535897932384626433832795, &
                       e     = 2.718281828459045, &
                       g_si  = 9.80665, &            ! Standard gravity at sea level in m/s^2
                       g_us  = 32.1740485564304, &   ! Standard gravity at sea level in ft/s^2
                       R_E_Z = 6356766, &            ! Earth radius for Geopotential altitude in m
                       R_E   = 6366707.01949371, &   ! Mean radius of the earth at sea level in m
                       R     = 287.0528, &           ! Gas constant for dry air in J/(kg*K)
                       gamma = 1.4                   ! Heat capacity ratio

    !! =====================================================
    !!                 CONVERSION FACTORS
    !! =====================================================
    
    !! Angles
    real, parameter :: d2r = pi / 180.0, &   ! Degrees to radians
                       r2d = 180.0 / pi, &   ! Radians to degrees
                       rev = 0.5 / pi        ! Radians to revolutions
    
    !! US to SI
    real, parameter :: psf2pa  = 47.880258, &        ! Pressure conversion: lbf/ft^2 to pascal, or N/m^2
                       ed2sid  = 515.379, &          ! Density conversion: slug/ft^3 to kg/m^3
                       ev2siv  = 47.880258           ! Viscosity conversion: slug/(ft*s) to kg/(m*s)

    !! Desired time from seconds
    real, parameter :: minute = 1. / 60., &
                       hour   = 1. / 3600., &
                       day    = 1. / 86400., &
                       week   = 1. / 604800., &
                       month  = 1. / 2628000., &
                       year   = 1. / 31536000.

    !! Desired length from Feet
    real, parameter :: m     = 0.3048, &
                       inch  = 12., &
                       yard  = 1. / 3., &
                       mile  = 1. / 5280., &
                       nmile = m / 1852.

    !! Desired velocity from ft/s
    real, parameter :: metps = 0.3048, &
                       kph   = 1.09728, &
                       mph   = 0.68181818, &
                       knots = 0.5924838

    !! Desired acceleration from ft/s^2
    real, parameter :: metpss = 0.3048, &
                       gs     = 1. / 32.174048556430442

    !! Desired mass from US to SI
    real, parameter :: lbm2kg  = 0.45359237, &       ! Pound-mass to kilograms
                       slug2kg = 14.5939029, &       ! Slugs to kilograms
                       slug    = 0.03108095, &       ! lbm to slug
                       lbm     = 32.174048556430442  ! slug to lbm

    !! Desired temperature from Fahrenheit
    real, parameter :: kelvin = 255.927778, &
                       celcius = -17.2222222, &
                       rankine = 460.67

    !! Desired force from lbf = slug * ft/s^2
    real, parameter :: N   = 4.4482216152605, &
                       ton = 1. / 2000.

    !! Desired pressure from PSI
    real, parameter :: Pa   = 6894.75729, &
                       bar  = 0.06894757, &
                       torr = 51.7149327, &
                       atm  = 0.06804596, &
                       psf  = 144.

    !! =====================================================
    !!                    SI Information
    !! =====================================================

    !! Prefixes
    real, parameter ::  yocto = 1.e-24, &   ! y
                        zepto = 1.e-21, &   ! z
                        atto  = 1.e-18, &   ! a
                        femto = 1.e-15, &   ! f
                        pico  = 1.e-12, &   ! p
                        nano  = 1.e-9, &    ! n
                        micro = 1.e-6, &    ! u
                        milli = 1.e-3, &    ! m
                        centi = 1.e-2, &    ! c
                        deci  = 1.e-1, &    ! d
                        deka  = 1.e1, &     ! da
                        hecto = 1.e2, &     ! h
                        kilo  = 1.e3, &     ! k
                        mega  = 1.e6, &     ! M
                        giga  = 1.e9, &     ! G
                        tera  = 1.e12, &    ! T
                        peta  = 1.e15, &    ! P
                        exa   = 1.e18, &    ! E
                        zett  = 1.e21, &    ! Z
                        yotta = 1.e24       ! Y
                       
contains
    
end module units_m