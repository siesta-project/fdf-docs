    FUNCTION fdf_integer(label, default, line)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)                        :: label
      integer(ip)                         :: default

!-------------------------------------------------------------- Output Variables
      integer(ip)                         :: fdf_integer
      type(line_dlist), pointer, optional :: line

!--------------------------------------------------------------- Local Variables
      character(80)                       :: msg
      type(line_dlist), pointer           :: mark

!------------------------------------------------------------------------- BEGIN
!     Prevents using FDF routines without initialize
      if (.not. fdf_started) then
        call die('FDF module: fdf_integer', 'FDF subsystem not initialized', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      if (fdf_locate(label, mark)) then
        if (.not. match(mark%pline, 'li')) then
          write(msg,*) 'no integer value for ', label
          call die('FDF module: fdf_integer', msg, THIS_FILE, __LINE__, fdf_err)
        endif

        fdf_integer = integers(mark%pline, 1, 1)
        if (fdf_output) write(fdf_out,'(a,5x,i10)') label, fdf_integer
      else
        fdf_integer = default
        if (fdf_output) write(fdf_out,'(a,i10,5x,a)') label, default, '# default value'
      endif

      if (PRESENT(line)) line = mark

      RETURN
!--------------------------------------------------------------------------- END
    END FUNCTION fdf_integer
