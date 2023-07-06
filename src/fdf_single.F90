    FUNCTION fdf_single(label, default, line)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)                        :: label
      real(sp)                            :: default

!-------------------------------------------------------------- Output Variables
      real(sp)                            :: fdf_single
      type(line_dlist), pointer, optional :: line

!--------------------------------------------------------------- Local Variables
      character(80)                       :: msg
      type(line_dlist), pointer           :: mark

!------------------------------------------------------------------------- BEGIN
!     Prevents using FDF routines without initialize
      if (.not. fdf_started) then
        call die('FDF module: fdf_single', 'FDF subsystem not initialized', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      if (fdf_locate(label, mark)) then
        if (.not. match(mark%pline, 'lv')) then
          write(msg,*) 'no real value for ', label
          call die('FDF module: fdf_single', msg, THIS_FILE, __LINE__,  fdf_err)
        endif
        fdf_single = values(mark%pline, 1, 1)
        if (fdf_output) write(fdf_out,'(a,5x,g20.10)') label, fdf_single
      else
        fdf_single = default
        if (fdf_output) write(fdf_out,'(a,5x,g20.10,5x,a)') label, default, '# default value'
      endif

      if (PRESENT(line)) line = mark

      RETURN
!--------------------------------------------------------------------------- END
    END FUNCTION fdf_single
