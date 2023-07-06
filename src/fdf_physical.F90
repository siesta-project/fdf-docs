    FUNCTION fdf_physical(label, default, defunit, line)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)                        :: label, defunit
      real(dp)                            :: default

!-------------------------------------------------------------- Output Variables
      real(dp)                            :: fdf_physical
      type(line_dlist), pointer, optional :: line

!--------------------------------------------------------------- Local Variables
      character(10)                       :: unitstr
      character(80)                       :: msg
      real(dp)                            :: value
      type(line_dlist), pointer           :: mark

!------------------------------------------------------------------------- BEGIN
!     Prevents using FDF routines without initialize
      if (.not. fdf_started) then
        call die('FDF module: fdf_physical', 'FDF subsystem not initialized', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

!     Label found
      if (fdf_locate(label, mark)) then
        if (.not. match(mark%pline, 'lv')) then
          write(msg,*) 'no real value for ', label
          call die('FDF module: fdf_physical', msg, THIS_FILE, __LINE__, fdf_err)
        endif

!       Label with value
        value = values(mark%pline, 1, 1)
        fdf_physical = value

!       Look for unit
        if (.not. match(mark%pline, 'lvn')) then
          write(msg,*) 'no unit specified for ', label
          call die('FDF module: fdf_physical', msg, THIS_FILE, __LINE__, fdf_err)
        endif

        unitstr = names(mark%pline, 1, 2)
        if (.not. leqi(unitstr, defunit))                               &
          fdf_physical = value * fdf_convfac(unitstr, defunit)

        if (fdf_output) write(fdf_out,'(a,5x,g20.10,1x,a10)') label, fdf_physical, defunit
        if (fdf_output) write(fdf_out,'(a,a,5x,g20.10,1x,a10)')                         &
             '# above item originally: ', label, value, unitstr
      else
        fdf_physical = default
        if (fdf_output) write(fdf_out,'(a,5x,g20.10,1x,a,5x,a)')                        &
             label, default, defunit, '# default value'
      endif

      if (PRESENT(line)) line = mark

      RETURN
!--------------------------------------------------------------------------- END
    END FUNCTION fdf_physical
