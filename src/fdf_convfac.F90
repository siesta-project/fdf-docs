    FUNCTION fdf_convfac(from, to)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)           :: from, to

!-------------------------------------------------------------- Output Variables
      real(dp)               :: fdf_convfac

!--------------------------------------------------------------- Local Variables
      character(80)          :: msg
      integer(ip)            :: iu, ifrom, ito

!------------------------------------------------------------------------- BEGIN

!
!     We allow case variations in the units. this could be dangerous
!     (meV --> MeV!!) in real life, but not in this restricted
!     field.

      ! README BEFORE ADDING UNITS:
      !
      ! Units should be added through the small Python code:
      !   fdf_units.py
      ! Add the appropriate unit in the designated unit-type and
      ! run the python script. It will then create (to std-out)
      ! a drop-in replacement for the following lines.

      integer(ip), parameter :: nu = 85
      character(8) :: dimm(nu)
      character(10) :: name(nu)
      real(dp) :: unit(nu)
      data (dimm(iu), name(iu), unit(iu), iu=1, 3) / &
          'mass    ', 'g         ', 1.d-3, &
          'mass    ', 'kg        ', 1.d0, &
          'mass    ', 'amu       ', 1.66054d-27 /

      data (dimm(iu), name(iu), unit(iu), iu=4, 9) / &
          'length  ', 'm         ', 1.d0, &
          'length  ', 'cm        ', 1.d-2, &
          'length  ', 'nm        ', 1.d-9, &
          'length  ', 'pm        ', 1.d-12, &
          'length  ', 'ang       ', 1.d-10, &
          'length  ', 'bohr      ', 0.529177d-10 /

      data (dimm(iu), name(iu), unit(iu), iu=10, 19) / &
          'energy  ', 'j         ', 1.d0, &
          'energy  ', 'kj        ', 1.d3, &
          'energy  ', 'erg       ', 1.d-7, &
          'energy  ', 'mev       ', 1.60219d-22, &
          'energy  ', 'ev        ', 1.60219d-19, &
          'energy  ', 'mry       ', 2.17991d-21, &
          'energy  ', 'ry        ', 2.17991d-18, &
          'energy  ', 'mha       ', 4.35982d-21, &
          'energy  ', 'mhartree  ', 4.35982d-21, &
          'energy  ', 'ha        ', 4.35982d-18 /
      data (dimm(iu), name(iu), unit(iu), iu=20, 29) / &
          'energy  ', 'hartree   ', 4.35982d-18, &
          'energy  ', 'k         ', 1.38066d-23, &
          'energy  ', 'kelvin    ', 1.38066d-23, &
          'energy  ', 'kcal/mol  ', 6.94780d-21, &
          'energy  ', 'kj/mol    ', 1.6606d-21, &
          'energy  ', 'hz        ', 6.6262d-34, &
          'energy  ', 'thz       ', 6.6262d-22, &
          'energy  ', 'cm-1      ', 1.986d-23, &
          'energy  ', 'cm^-1     ', 1.986d-23, &
          'energy  ', 'cm**-1    ', 1.986d-23 /

      data (dimm(iu), name(iu), unit(iu), iu=30, 39) / &
          'time    ', 's         ', 1.d0, &
          'time    ', 'au        ', 2.418884326505e-17, &
          'time    ', 'ns        ', 1.d-9, &
          'time    ', 'ps        ', 1.d-12, &
          'time    ', 'fs        ', 1.d-15, &
          'time    ', 'min       ', 60.d0, &
          'time    ', 'mins      ', 60.d0, &
          'time    ', 'hour      ', 3600.d0, &
          'time    ', 'hours     ', 3600.d0, &
          'time    ', 'day       ', 86400.d0 /
      data (dimm(iu), name(iu), unit(iu), iu=40, 40) / &
          'time    ', 'days      ', 86400.d0 /

      data (dimm(iu), name(iu), unit(iu), iu=41, 44) / &
          'force   ', 'n         ', 1.d0, &
          'force   ', 'ev/ang    ', 1.60219d-9, &
          'force   ', 'ry/bohr   ', 4.11943d-8, &
          'force   ', 'ha/bohr   ', 8.23886d-08 /

      data (dimm(iu), name(iu), unit(iu), iu=45, 54) / &
          'pressure', 'pa        ', 1.d0, &
          'pressure', 'gpa       ', 1.d9, &
          'pressure', 'atm       ', 1.01325d5, &
          'pressure', 'bar       ', 1.d5, &
          'pressure', 'mbar      ', 1.d11, &
          'pressure', 'ev/ang**3 ', 1.60219d11, &
          'pressure', 'ev/ang^3  ', 1.60219d11, &
          'pressure', 'ry/bohr**3', 1.47108d13, &
          'pressure', 'ry/bohr^3 ', 1.47108d13, &
          'pressure', 'ha/bohr^3 ', 2.94216d13 /
      data (dimm(iu), name(iu), unit(iu), iu=55, 55) / &
          'pressure', 'ha/bohr**3', 2.94216d13 /

      data (dimm(iu), name(iu), unit(iu), iu=56, 59) / &
          'surftens', 'n/m       ', 1.d0, &
          'surftens', 'mn/m      ', 1.d3, &
          'surftens', 'dyn/cm    ', 1.d3, &
          'surftens', 'erg/cm**2 ', 1.d3 /

      data (dimm(iu), name(iu), unit(iu), iu=60, 61) / &
          'charge  ', 'c         ', 1.d0, &
          'charge  ', 'e         ', 1.602177d-19 /

      data (dimm(iu), name(iu), unit(iu), iu=62, 66) / &
          'dipole  ', 'c*m       ', 1.d0, &
          'dipole  ', 'd         ', 3.33564d-30, &
          'dipole  ', 'debye     ', 3.33564d-30, &
          'dipole  ', 'e*bohr    ', 8.47835d-30, &
          'dipole  ', 'e*ang     ', 1.602177d-29 /

      data (dimm(iu), name(iu), unit(iu), iu=67, 68) / &
          'mominert', 'kg*m**2   ', 1.d0, &
          'mominert', 'ry*fs**2  ', 2.17991d-48 /

      data (dimm(iu), name(iu), unit(iu), iu=69, 75) / &
          'efield  ', 'v/m       ', 1.d0, &
          'efield  ', 'v/nm      ', 1.d9, &
          'efield  ', 'v/ang     ', 1.d10, &
          'efield  ', 'v/bohr    ', 1.8897268d10, &
          'efield  ', 'ry/bohr/e ', 2.5711273d11, &
          'efield  ', 'ha/bohr/e ', 5.1422546d11, &
          'efield  ', 'har/bohr/e', 5.1422546d11 /

      data (dimm(iu), name(iu), unit(iu), iu=76, 77) / &
          'angle   ', 'deg       ', 1.d0, &
          'angle   ', 'rad       ', 5.72957795d1 /

      data (dimm(iu), name(iu), unit(iu), iu=78, 85) / &
          'torque  ', 'mev/deg   ', 1.0d-3, &
          'torque  ', 'mev/rad   ', 1.745533d-5, &
          'torque  ', 'ev/deg    ', 1.0d0, &
          'torque  ', 'ev/rad    ', 1.745533d-2, &
          'torque  ', 'mry/deg   ', 13.6058d-3, &
          'torque  ', 'mry/rad   ', 0.237466d-3, &
          'torque  ', 'ry/deg    ', 13.6058d0, &
          'torque  ', 'ry/rad    ', 0.237466d0 /

      ifrom = 0
      ito   = 0
      do iu= 1, nu
        if (leqi(name(iu), from)) ifrom = iu
        if (leqi(name(iu), to))   ito   = iu
      end do

      if (ifrom .eq. 0) then
        write(msg,*) 'unknown unit = ', from
        call die('FDF module: fdf_convfac', msg, THIS_FILE, __LINE__, fdf_err)
      endif

      if (ito .eq. 0) then
        write(msg,*) 'unknown unit = ', to
        call die('FDF module: fdf_convfac', msg, THIS_FILE, __LINE__, fdf_err)
      endif

      if (leqi(dimm(ifrom), dimm(ito))) then
        fdf_convfac = unit(ifrom) / unit(ito)
      else
        write(msg,*) 'unit''s physical dimensions don''t match: ',      &
                     from, ', ', to
        call die('FDF module: fdf_convfac', msg, THIS_FILE, __LINE__, fdf_err)
      endif
!--------------------------------------------------------------------------- END
    END FUNCTION fdf_convfac
