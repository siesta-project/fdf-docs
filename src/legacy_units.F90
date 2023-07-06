module fdf_legacy_units_m
  use fdf_prec, only: dp

  implicit none
  
  public :: inquire_unit
  private

  integer, parameter :: nu = 80
  integer :: iu
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
          'time    ', 'ns        ', 1.d-9, &
          'time    ', 'ps        ', 1.d-12, &
          'time    ', 'fs        ', 1.d-15, &
          'time    ', 'min       ', 60.d0, &
          'time    ', 'mins      ', 60.d0, &
          'time    ', 'hour      ', 3600.d0, &
          'time    ', 'hours     ', 3600.d0, &
          'time    ', 'day       ', 86400.d0, &
          'time    ', 'days      ', 86400.d0 /

      data (dimm(iu), name(iu), unit(iu), iu=40, 43) / &
          'force   ', 'n         ', 1.d0, &
          'force   ', 'ev/ang    ', 1.60219d-9, &
          'force   ', 'ry/bohr   ', 4.11943d-8, &
          'force   ', 'ha/bohr   ', 8.23886d-08 /

      data (dimm(iu), name(iu), unit(iu), iu=44, 52) / &
          'pressure', 'pa        ', 1.d0, &
          'pressure', 'gpa       ', 1.d9, &
          'pressure', 'atm       ', 1.01325d5, &
          'pressure', 'bar       ', 1.d5, &
          'pressure', 'mbar      ', 1.d11, &
          'pressure', 'ev/ang**3 ', 1.60219d11, &
          'pressure', 'ev/ang^3  ', 1.60219d11, &
          'pressure', 'ry/bohr**3', 1.47108d13, &
          'pressure', 'ry/bohr^3 ', 1.47108d13 /

      data (dimm(iu), name(iu), unit(iu), iu=53, 54) / &
          'charge  ', 'c         ', 1.d0, &
          'charge  ', 'e         ', 1.602177d-19 /

      data (dimm(iu), name(iu), unit(iu), iu=55, 59) / &
          'dipole  ', 'c*m       ', 1.d0, &
          'dipole  ', 'd         ', 3.33564d-30, &
          'dipole  ', 'debye     ', 3.33564d-30, &
          'dipole  ', 'e*bohr    ', 8.47835d-30, &
          'dipole  ', 'e*ang     ', 1.602177d-29 /

      data (dimm(iu), name(iu), unit(iu), iu=60, 61) / &
          'mominert', 'kg*m**2   ', 1.d0, &
          'mominert', 'ry*fs**2  ', 2.17991d-48 /

      data (dimm(iu), name(iu), unit(iu), iu=62, 68) / &
          'efield  ', 'v/m       ', 1.d0, &
          'efield  ', 'v/nm      ', 1.d9, &
          'efield  ', 'v/ang     ', 1.d10, &
          'efield  ', 'v/bohr    ', 1.8897268d10, &
          'efield  ', 'ry/bohr/e ', 2.5711273d11, &
          'efield  ', 'ha/bohr/e ', 5.1422546d11, &
          'efield  ', 'har/bohr/e', 5.1422546d11 /

      data (dimm(iu), name(iu), unit(iu), iu=69, 70) / &
          'angle   ', 'deg       ', 1.d0, &
          'angle   ', 'rad       ', 5.72957795d1 /

      data (dimm(iu), name(iu), unit(iu), iu=71, 78) / &
          'torque  ', 'mev/deg   ', 1.0d-3, &
          'torque  ', 'mev/rad   ', 1.745533d-5, &
          'torque  ', 'ev/deg    ', 1.0d0, &
          'torque  ', 'ev/rad    ', 1.745533d-2, &
          'torque  ', 'mry/deg   ', 13.6058d-3, &
          'torque  ', 'mry/rad   ', 0.237466d-3, &
          'torque  ', 'ry/deg    ', 13.6058d0, &
          'torque  ', 'ry/rad    ', 0.237466d0 /

      data (dimm(iu), name(iu), unit(iu), iu=79, 80) / &
          'bfield  ', 'Tesla     ', 1.0d0, &
          'bfield  ', 'G         ', 1.0d-4 /


CONTAINS

  !<
  ! Returns information about a unit in the units table
  !
  ! Unit specifications might include an optional 'physical dimension'
  ! qualifier (e.g. 'bfield:g')
  ! In this case, 'phys_dim' returns the physical dimension, and the
  ! qualifier is used to match the unit.
  ! This version is case-insensitive (e.g. 'g' and 'G' could stand for 'Gauss').
  ! As the above example indicates, in the absence of a physical dimension qualifier,
  ! 'g' might be ambiguous ('bfield' or 'mass'?). The routine will return 'stat=-1'
  ! in this case.
  ! Units might be ambiguous in a more serious way: 'meV' and 'MeV' could both be
  ! present in the table. In this case, it might be advisable to use a case-sensitive
  ! version of this routine (replacing 'leqi' by a 'strict' version).
  ! If the unit is not found in the table, the routine returns 'stat=-2'.

  subroutine inquire_unit(unit_str, stat, phys_dim, unit_name, unit_value)

    use fdf_utils, only: leqi
    use fdf_prec, only: dp

    character(len=*), intent(in)   :: unit_str   !+ unit specification
    character(len=*), intent(out)  :: phys_dim   !+ physical dimension (e.g. 'mass')
    character(len=*), intent(out)  :: unit_name  !+ unit name (e.g. 'g')
    real(dp), intent(out)          :: unit_value !+ actual value (e.g. 1.e-3)
    integer, intent(out)           :: stat       !+ status code

    integer           :: idx_colon, iu, idx
    logical           :: phys_dim_specified, match
    
    idx_colon = index(unit_str,":")
    if (idx_colon /= 0) then
       ! spec includes dimension prefix
       phys_dim = unit_str(1:idx_colon-1)
       unit_name = unit_str(idx_colon+1:)
       phys_dim_specified = .true.
    else
       phys_dim = ""
       unit_name = unit_str
       phys_dim_specified = .false.
    endif

    stat = 0
    idx = 0

    do iu= 1, nu
         match = .false.
         if (leqi(name(iu), unit_name)) then
            if (phys_dim_specified) then
               if (leqi(dimm(iu), phys_dim)) then
                  match = .true.
               endif
            else
               match = .true.
            endif
         endif
         if (match) then
            if (idx /= 0) then  ! ambiguous
               stat = 1
               RETURN
            endif
            idx = iu
         endif
      enddo
      
      if (idx == 0) then
         stat = -2    ! not found
      else
         phys_dim = trim(dimm(idx))
         unit_value = unit(idx)
      endif
      
    end subroutine inquire_unit
    
  end module fdf_legacy_units_m
