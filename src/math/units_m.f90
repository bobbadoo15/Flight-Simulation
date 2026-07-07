module units_m
    use, intrinsic :: iso_fortran_env, only: rk => real64
    implicit none
    private

    public :: rk
    public :: pi, e, g_si, g_us, r_e_z, r_e, r_air, gamma_air, eps
    public :: d2r, r2d, rev
    public :: ft_to_m, m_to_ft, inch_to_ft, ft_to_inch, yard_to_ft, ft_to_yard
    public :: mile_to_ft, ft_to_mile, nmi_to_ft, ft_to_nmi
    public :: fps_to_mps, mps_to_fps, fps_to_kph, fps_to_mph, fps_to_kts
    public :: fps2_to_mps2, mps2_to_fps2, fps2_to_gs, gs_to_fps2
    public :: lbm_to_kg, kg_to_lbm, slug_to_kg, kg_to_slug, lbm_to_slug, slug_to_lbm
    public :: lbf_to_n, n_to_lbf, psi_to_pa, pa_to_psi, psf_to_pa
    public :: slugft3_to_kgm3, slugfts_to_kgms
    public :: minute, hour, day, week, month, year
    public :: yocto, zepto, atto, femto, pico, nano, micro, milli, centi, deci
    public :: deka, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta
    public :: conversion_factor_to, conversion_factor_from, parse_variable_and_units
    public :: f_to_k, k_to_f, c_to_k, k_to_c, f_to_c, c_to_f, f_to_r, r_to_f

    !! =====================================================
    !!                     CONSTANTS
    !! =====================================================
    !! real(rk), parameter means:
    !!   - real(rk): use the shared real64 precision from iso_fortran_env
    !!   - parameter: this value is a constant and cannot change
    !! Using one shared kind helps every module use the same precision.
    real(rk), parameter :: pi        = acos(-1.0_rk), &
                           e         = exp(1.0_rk), &
                           g_si      = 9.80665_rk, &                  ! Standard gravity in m/s^2
                           g_us      = 32.1740485564304_rk, &         ! Standard gravity in ft/s^2
                           r_e_z     = 6356766.0_rk, &                ! Geopotential Earth radius in m
                           r_e       = 6366707.01949371_rk, &         ! Mean Earth radius in m
                           r_air     = 287.0528_rk, &                 ! Gas constant for dry air in J/(kg*K)
                           gamma_air = 1.4_rk, &
                           eps       = 1.0e-12_rk                     ! Small tolerance for floating-point comparisons

    !! =====================================================
    !!                 CONVERSION FACTORS
    !! =====================================================

    !! Angles
    !! d2r means degrees-to-radians.
    !! r2d means radians-to-degrees.
    real(rk), parameter :: d2r = pi / 180.0_rk, &
                           r2d = 180.0_rk / pi, &
                           rev = 0.5_rk / pi

    !! US to SI
    real(rk), parameter :: psf_to_pa       = 47.880258_rk, &
                           slugft3_to_kgm3 = 515.378818_rk, &
                           slugfts_to_kgms = 47.880258_rk

    !! Desired time from seconds
    !! Example: 120.0_rk * minute converts 120 seconds into minutes.
    real(rk), parameter :: minute = 1.0_rk / 60.0_rk, &
                           hour   = 1.0_rk / 3600.0_rk, &
                           day    = 1.0_rk / 86400.0_rk, &
                           week   = 1.0_rk / 604800.0_rk, &
                           month  = 1.0_rk / 2628000.0_rk, &
                           year   = 1.0_rk / 31536000.0_rk

    !! Desired length from feet
    !! Example: 10.0_rk * ft_to_m converts 10 feet to meters.
    real(rk), parameter :: ft_to_m    = 0.3048_rk, &
                           m_to_ft    = 1.0_rk / ft_to_m, &
                           inch_to_ft = 12.0_rk, &
                           ft_to_inch = 1.0_rk / inch_to_ft, &
                           yard_to_ft = 1.0_rk / 3.0_rk, &
                           ft_to_yard = 1.0_rk / yard_to_ft, &
                           mile_to_ft = 1.0_rk / 5280.0_rk, &
                           ft_to_mile = 1.0_rk / mile_to_ft, &
                           nmi_to_ft  = 1.0_rk / 6076.11549_rk, &
                           ft_to_nmi  = 1.0_rk / nmi_to_ft

    !! Desired velocity from ft/s
    real(rk), parameter :: fps_to_mps = 0.3048_rk, &
                           mps_to_fps = 1.0_rk / fps_to_mps, &
                           fps_to_kph = 1.09728_rk, &
                           fps_to_mph = 0.68181818_rk, &
                           fps_to_kts = 0.5924838_rk

    !! Desired acceleration from ft/s^2
    real(rk), parameter :: fps2_to_mps2 = 0.3048_rk, &
                           mps2_to_fps2 = 1.0_rk / fps2_to_mps2, &
                           fps2_to_gs   = 1.0_rk / g_us, &
                           gs_to_fps2   = g_us

    !! Desired mass from US to SI
    real(rk), parameter :: lbm_to_kg   = 0.45359237_rk, &
                           kg_to_lbm   = 1.0_rk / lbm_to_kg, &
                           slug_to_kg  = 14.5939029_rk, &
                           kg_to_slug  = 1.0_rk / slug_to_kg, &
                           lbm_to_slug = 0.03108095_rk, &
                           slug_to_lbm = 32.174048556430442_rk

    !! Desired force from lbf = slug * ft/s^2
    real(rk), parameter :: lbf_to_n = 4.4482216152605_rk, &
                           n_to_lbf = 1.0_rk / lbf_to_n

    !! Desired pressure from psi
    real(rk), parameter :: psi_to_pa = 6894.75729_rk, &
                           pa_to_psi = 1.0_rk / psi_to_pa

    !! =====================================================
    !!                    SI Information
    !! =====================================================
    real(rk), parameter :: yocto = 1.0e-24_rk, &
                           zepto = 1.0e-21_rk, &
                           atto  = 1.0e-18_rk, &
                           femto = 1.0e-15_rk, &
                           pico  = 1.0e-12_rk, &
                           nano  = 1.0e-9_rk, &
                           micro = 1.0e-6_rk, &
                           milli = 1.0e-3_rk, &
                           centi = 1.0e-2_rk, &
                           deci  = 1.0e-1_rk, &
                           deka  = 1.0e1_rk, &
                           hecto = 1.0e2_rk, &
                           kilo  = 1.0e3_rk, &
                           mega  = 1.0e6_rk, &
                           giga  = 1.0e9_rk, &
                           tera  = 1.0e12_rk, &
                           peta  = 1.0e15_rk, &
                           exa   = 1.0e18_rk, &
                           zetta = 1.0e21_rk, &
                           yotta = 1.0e24_rk

    !! These strings are used by the parser.
    !! Example: if input is "deg", the parser matches it to deg_str.
    character(len=*), parameter :: yocto_str = 'y', &
                                   zepto_str = 'z', &
                                   atto_str  = 'a', &
                                   femto_str = 'f', &
                                   pico_str  = 'p', &
                                   nano_str  = 'n', &
                                   micro_str = 'u', &
                                   milli_str = 'm', &
                                   centi_str = 'c', &
                                   deci_str  = 'd', &
                                   deka_str  = 'da', &
                                   hecto_str = 'h', &
                                   kilo_str  = 'k', &
                                   mega_str  = 'M', &
                                   giga_str  = 'G', &
                                   tera_str  = 'T', &
                                   peta_str  = 'P', &
                                   exa_str   = 'E', &
                                   zetta_str = 'Z', &
                                   yotta_str = 'Y', &
                                   ft_str    = 'ft', &
                                   m_str     = 'm', &
                                   inch_str  = 'in', &
                                   yard_str  = 'yard', &
                                   mile_str  = 'mi', &
                                   nmi_str   = 'nmi', &
                                   s_str     = 's', &
                                   min_str   = 'min', &
                                   hr_str    = 'hr', &
                                   day_str   = 'day', &
                                   week_str  = 'wk', &
                                   year_str  = 'yr', &
                                   fps_str   = 'ft/s', &
                                   mps_str   = 'm/s', &
                                   kts_str   = 'kts', &
                                   mph_str   = 'mph', &
                                   deg_str   = 'deg', &
                                   rad_str   = 'rad', &
                                   rev_str   = 'rev', &
                                   slug_str  = 'slug', &
                                   lbm_str   = 'lbm', &
                                   kg_str    = 'kg', &
                                   lbf_str   = 'lbf', &
                                   n_str     = 'N', &
                                   psi_str   = 'psi', &
                                   pa_str    = 'Pa', &
                                   f_str     = 'F', &
                                   c_str     = 'C', &
                                   k_str     = 'K', &
                                   r_str     = 'R'

    real(rk), dimension(20), parameter :: scaling_factors = [yocto, zepto, atto, femto, pico, nano, micro, milli, centi, deci, &
                                                              deka, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta]

    character(len=2), dimension(20), parameter :: scaling_factors_str = [character(len=2) :: yocto_str, zepto_str, atto_str, femto_str, pico_str, &
                                                                          nano_str, micro_str, milli_str, centi_str, deci_str, deka_str, hecto_str, &
                                                                          kilo_str, mega_str, giga_str, tera_str, peta_str, exa_str, zetta_str, yotta_str]

contains

    !! =====================================================
    !!             TEMPERATURE CONVERSION FUNCTIONS
    !! =====================================================
    !! These are functions, not subroutines, because they take one input
    !! and return one value.

    pure real(rk) function f_to_k(tf)
        !! Converts Fahrenheit to Kelvin.
        !! pure means the function has no side effects.
        !! intent(in) means tf is input-only.
        real(rk), intent(in) :: tf
        f_to_k = (tf - 32.0_rk) * (5.0_rk / 9.0_rk) + 273.15_rk
    end function f_to_k

    pure real(rk) function k_to_f(tk)
        !! Converts Kelvin to Fahrenheit.
        real(rk), intent(in) :: tk
        k_to_f = (tk - 273.15_rk) * (9.0_rk / 5.0_rk) + 32.0_rk
    end function k_to_f

    pure real(rk) function c_to_k(tc)
        !! Converts Celsius to Kelvin.
        real(rk), intent(in) :: tc
        c_to_k = tc + 273.15_rk
    end function c_to_k

    pure real(rk) function k_to_c(tk)
        !! Converts Kelvin to Celsius.
        real(rk), intent(in) :: tk
        k_to_c = tk - 273.15_rk
    end function k_to_c

    pure real(rk) function f_to_c(tf)
        !! Converts Fahrenheit to Celsius.
        real(rk), intent(in) :: tf
        f_to_c = (tf - 32.0_rk) * (5.0_rk / 9.0_rk)
    end function f_to_c

    pure real(rk) function c_to_f(tc)
        !! Converts Celsius to Fahrenheit.
        real(rk), intent(in) :: tc
        c_to_f = tc * (9.0_rk / 5.0_rk) + 32.0_rk
    end function c_to_f

    pure real(rk) function f_to_r(tf)
        !! Converts Fahrenheit to Rankine.
        real(rk), intent(in) :: tf
        f_to_r = tf + 459.67_rk
    end function f_to_r

    pure real(rk) function r_to_f(tr)
        !! Converts Rankine to Fahrenheit.
        real(rk), intent(in) :: tr
        r_to_f = tr - 459.67_rk
    end function r_to_f

    !! =====================================================
    !!                  PARSER SUBROUTINES
    !! =====================================================
    !! A subroutine is useful when you want to modify outputs passed in
    !! through argument lists rather than return one single value.

    subroutine check_type(s, ta, zi, zj)
        !! Checks whether a unit string matches a known unit name.
        !! zi = which base unit matched
        !! zj = which SI prefix matched
        character(len=*), intent(in) :: s
        character(len=*), dimension(:), intent(in) :: ta
        integer, intent(out) :: zi, zj
        integer :: i, j

        zi = 0
        zj = 0

        do i = 1, size(ta)
            if (trim(s) == trim(ta(i))) then
                zi = i
                return
            end if
            do j = 1, size(scaling_factors_str)
                if (trim(s) == trim(scaling_factors_str(j)) // trim(ta(i))) then
                    zi = i
                    zj = j
                    return
                end if
            end do
        end do
    end subroutine check_type

    function get_factor(s) result(f)
        !! Returns the numeric conversion factor for one unit token.
        !! Example idea: "deg" returns radians-to-degrees factor.
        character(len=*), intent(in) :: s
        real(rk) :: f
        integer :: i, j

        call check_type(s, [character(len=4) :: ft_str, m_str, inch_str, yard_str, mile_str, nmi_str], i, j)
        if (i /= 0) then
            select case(i)
            case(1); f = 1.0_rk
            case(2); f = ft_to_m
            case(3); f = inch_to_ft
            case(4); f = yard_to_ft
            case(5); f = mile_to_ft
            case(6); f = nmi_to_ft
            end select
            if (j /= 0) f = f * scaling_factors(j)
            return
        end if

        call check_type(s, [character(len=3) :: s_str, min_str, hr_str, day_str, week_str, year_str], i, j)
        if (i /= 0) then
            select case(i)
            case(1); f = 1.0_rk
            case(2); f = minute
            case(3); f = hour
            case(4); f = day
            case(5); f = week
            case(6); f = year
            end select
            if (j /= 0) f = f * scaling_factors(j)
            return
        end if

        call check_type(s, [character(len=4) :: kts_str, mph_str], i, j)
        if (i /= 0) then
            select case(i)
            case(1); f = fps_to_kts
            case(2); f = fps_to_mph
            end select
            if (j /= 0) f = f * scaling_factors(j)
            return
        end if

        call check_type(s, [character(len=3) :: rad_str, deg_str, rev_str], i, j)
        if (i /= 0) then
            select case(i)
            case(1); f = 1.0_rk
            case(2); f = r2d
            case(3); f = rev
            end select
            if (j /= 0) f = f * scaling_factors(j)
            return
        end if

        call check_type(s, [character(len=4) :: slug_str, lbm_str, kg_str], i, j)
        if (i /= 0) then
            select case(i)
            case(1); f = 1.0_rk
            case(2); f = slug_to_lbm
            case(3); f = slug_to_kg
            end select
            if (j /= 0) f = f * scaling_factors(j)
            return
        end if

        call check_type(s, [character(len=3) :: lbf_str, n_str], i, j)
        if (i /= 0) then
            select case(i)
            case(1); f = 1.0_rk
            case(2); f = lbf_to_n
            end select
            if (j /= 0) f = f * scaling_factors(j)
            return
        end if

        call check_type(s, [character(len=3) :: psi_str, pa_str], i, j)
        if (i /= 0) then
            select case(i)
            case(1); f = 1.0_rk
            case(2); f = psi_to_pa
            end select
            if (j /= 0) f = f * scaling_factors(j)
            return
        end if

        call check_type(s, [character(len=1) :: f_str, c_str, k_str, r_str], i, j)
        if (i /= 0) then
            select case(i)
            case(1); f = 1.0_rk
            case(2); f = 5.0_rk / 9.0_rk
            case(3); f = 5.0_rk / 9.0_rk
            case(4); f = 1.0_rk
            end select
            if (j /= 0) f = f * scaling_factors(j)
            return
        end if

        write(*,*) 'Error finding conversion factor for, ' // trim(s) // '! Quitting...'
        stop
    end function get_factor

    subroutine parse_units(line, numer, denom)
        !! Breaks a unit expression like "ft/s^2" into numerator and denominator tokens.
        !! numer and denom are allocatable output arrays.
        character(len=*), intent(in) :: line
        character(len=:), allocatable, dimension(:), intent(out) :: numer, denom
        character(len=:), allocatable, dimension(:) :: tokens

        integer :: i, j, start_pos, len_line, token_count, cnt
        integer :: token_len, max_token_len
        integer :: cnt_num, cnt_den, ie, e_int, ios
        character(len=:), allocatable :: u, inp, ue
        logical :: num_flag

        len_line = len_trim(line)

        do i = 1, len_line - 1
            if (line(i:i+1) == '^-') then
                write(*,*) 'Error converting units, ' // trim(line) // &
                    ', negative exponents are not allowed! Use / to move a unit to the denominator. Quitting...'
                stop
            end if
        end do

        !! Here I replace '-' with '*' so the parser treats dashed terms consistently.
        inp = line
        do i = 1, len(line)
            if (inp(i:i) == '-') inp(i:i) = '*'
        end do

        start_pos = 1
        token_count = 0
        max_token_len = 0

        do i = 1, len_line + 1
            if (i > len_line) then
                token_len = i - start_pos
                if (token_len > 0) then
                    token_count = token_count + 1
                    if (token_len > max_token_len) max_token_len = token_len
                end if
            elseif (inp(i:i) == '*') then
                token_len = i - start_pos
                if (token_len > 0) then
                    token_count = token_count + 1
                    if (token_len > max_token_len) max_token_len = token_len
                end if
                start_pos = i + 1
            elseif (inp(i:i) == '/') then
                token_len = i - start_pos
                if (token_len > 0) then
                    token_count = token_count + 1
                    if (token_len > max_token_len) max_token_len = token_len
                end if
                start_pos = i
            end if
        end do

        allocate(character(len=max_token_len) :: tokens(token_count), u)

        start_pos = 1
        cnt = 0
        do i = 1, len_line + 1
            if (i > len_line) then
                token_len = i - start_pos
                if (token_len > 0) then
                    cnt = cnt + 1
                    tokens(cnt) = line(start_pos:start_pos + token_len - 1)
                end if
            elseif (inp(i:i) == '*') then
                token_len = i - start_pos
                if (token_len > 0) then
                    cnt = cnt + 1
                    tokens(cnt) = inp(start_pos:start_pos + token_len - 1)
                end if
                start_pos = i + 1
            elseif (inp(i:i) == '/') then
                token_len = i - start_pos
                if (token_len > 0) then
                    cnt = cnt + 1
                    tokens(cnt) = inp(start_pos:start_pos + token_len - 1)
                end if
                start_pos = i
            end if
        end do

        cnt_num = 0
        cnt_den = 0
        do i = 1, size(tokens)
            u = tokens(i)
            if (u(1:1) == '/') then
                num_flag = .false.
            else
                num_flag = .true.
            end if
            ie = index(u, '^')
            if (ie == 0) then
                e_int = 1
            else
                allocate(character(len=len(trim(u(ie+1:)))) :: ue)
                ue = trim(u(ie+1:))
                read(ue, *, iostat=ios) e_int
                deallocate(ue)
                if (ios /= 0) then
                    write(*,*) 'Error parsing units, ' // trim(line) // ', failed to convert exponent, ' // &
                        trim(u(ie+1:)) // ', to an integer! Quitting...'
                    stop
                end if
            end if
            if (num_flag) then
                cnt_num = cnt_num + e_int
            else
                cnt_den = cnt_den + e_int
            end if
        end do

        allocate(character(len=max_token_len) :: numer(cnt_num), denom(cnt_den))
        cnt_num = 0
        cnt_den = 0
        do i = 1, size(tokens)
            u = tokens(i)
            if (u(1:1) == '/') then
                num_flag = .false.
            else
                num_flag = .true.
            end if
            ie = index(u, '^')
            if (ie == 0) then
                e_int = 1
            else
                allocate(character(len=len(trim(u(ie+1:)))) :: ue)
                ue = trim(u(ie+1:))
                read(ue, *) e_int
                deallocate(ue)
            end if
            if (num_flag) then
                do j = 1, e_int
                    cnt_num = cnt_num + 1
                    if (ie == 0) then
                        numer(cnt_num) = trim(u)
                    else
                        numer(cnt_num) = trim(u(:ie-1))
                    end if
                end do
            else
                do j = 1, e_int
                    cnt_den = cnt_den + 1
                    if (ie == 0) then
                        denom(cnt_den) = trim(u(2:))
                    else
                        denom(cnt_den) = trim(u(2:ie-1))
                    end if
                end do
            end if
        end do
    end subroutine parse_units

    function conversion_factor_to(input) result(cf)
        !! Computes the total conversion factor for a unit expression.
        !! Example: conversion_factor_to('ft/s^2')
        character(len=*), intent(in) :: input
        real(rk) :: cf
        integer :: i
        character(len=:), allocatable, dimension(:) :: numer, denom

        call parse_units(input, numer, denom)
        cf = 1.0_rk
        do i = 1, size(numer)
            cf = cf * get_factor(numer(i))
        end do
        do i = 1, size(denom)
            cf = cf / get_factor(denom(i))
        end do
    end function conversion_factor_to

    function conversion_factor_from(input) result(cf)
        !! Inverse of conversion_factor_to.
        !! Helpful when you want to convert from the parsed unit back to the base unit.
        character(len=*), intent(in) :: input
        real(rk) :: cf
        cf = 1.0_rk / conversion_factor_to(input)
    end function conversion_factor_from

    subroutine parse_variable_and_units(mono, var, units)
        !! Splits text like alpha[deg] into:
        !!   var   = 'alpha'
        !!   units = 'deg'
        character(len=*), intent(in) :: mono
        character(len=:), allocatable, intent(out) :: var, units

        integer :: istart, iend, l, cnts, cnte, i

        l = len_trim(mono)
        cnts = 0
        cnte = 0
        istart = 0
        iend = 0
        do i = 1, l
            if (mono(i:i) == '[') then
                istart = i + 1
                cnts = cnts + 1
            end if
            if (mono(i:i) == ']') then
                iend = i - 1
                cnte = cnte + 1
            end if
        end do

        if (cnts > 1 .or. cnte > 1) then
            write(*,*) 'Error parsing term ' // trim(mono) // ': multiple units provided! Quitting...'
            stop
        end if

        if (cnts == 1 .and. cnte == 1) then
            if (iend /= l - 1) then
                write(*,*) 'Error parsing term ' // trim(mono) // ': units should be at the end! Quitting...'
                stop
            end if
            allocate(character(len=istart-2) :: var)
            allocate(character(len=iend-istart+1) :: units)
            var = mono(1:istart-2)
            units = mono(istart:iend)
        else
            allocate(character(len=l) :: var)
            var = mono
        end if
    end subroutine parse_variable_and_units

end module units_m