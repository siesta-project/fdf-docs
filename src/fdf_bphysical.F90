    FUNCTION fdf_bphysical(pline, defunit, after)
      implicit none
!--------------------------------------------------------------- Input Variables
      type(parsed_line), pointer        :: pline
      character(*)                      :: defunit
      integer(ip), intent(in), optional :: after

!-------------------------------------------------------------- Output Variables
      real(dp)                            :: fdf_bphysical

!--------------------------------------------------------------- Local Variables
      character(10)                       :: unitstr
      character(80)                       :: msg
      real(dp)                            :: value
      type(line_dlist), pointer           :: mark

!------------------------------------------------------------------------- BEGIN
!     Prevents using FDF routines without initialize
      if (.not. fdf_started) then
         call die('FDF module: fdf_bphysical', 'FDF subsystem not initialized', &
              THIS_FILE, __LINE__, fdf_err)
      endif

      if (.not. match(pline, 'vn', after)) then
         write(msg,*) 'no real value for line: '// trim(pline%line)
         call die('FDF module: fdf_bphysical', msg, THIS_FILE, &
              __LINE__, fdf_err)
      endif

      ! get value in block-line
      value = values(pline, 1, after)

      ! get unit in block-line
      unitstr = names(pline, 1, after)
      if ( leqi(unitstr, defunit) ) then
         fdf_bphysical = value
      else
         fdf_bphysical = value * fdf_convfac(unitstr, defunit)
      end if

!--------------------------------------------------------------------------- END
    END FUNCTION fdf_bphysical
