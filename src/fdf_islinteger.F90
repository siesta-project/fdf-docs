    FUNCTION fdf_islinteger(label)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)                        :: label

!-------------------------------------------------------------- Output Variables
      logical                             :: fdf_islinteger

!--------------------------------------------------------------- Local Variables
      type(line_dlist), pointer           :: mark

!------------------------------------------------------------------------- BEGIN
!     Prevents using FDF routines without initialize
      if (.not. fdf_started) then
         call die('FDF module: fdf_islinteger', 'FDF subsystem not initialized', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      if (fdf_locate(label, mark)) then
        ! if it is an integer list:
        fdf_islinteger = match(mark%pline, 'la')
      else
         fdf_islinteger = .false.
      endif
      if (fdf_output) write(fdf_out,'(a,5x,l10)') "#:linteger? " // label, &
          fdf_islinteger

      RETURN
!--------------------------------------------------------------------------- END
    END FUNCTION fdf_islinteger
