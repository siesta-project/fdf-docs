    FUNCTION fdf_islreal(label)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)                        :: label

!-------------------------------------------------------------- Output Variables
      logical                             :: fdf_islreal

!--------------------------------------------------------------- Local Variables
      type(line_dlist), pointer           :: mark

!------------------------------------------------------------------------- BEGIN
!     Prevents using FDF routines without initialize
      if (.not. fdf_started) then
         call die('FDF module: fdf_islreal', 'FDF subsystem not initialized', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      if (fdf_locate(label, mark)) then
        ! if it is a reallist:
        fdf_islreal = match(mark%pline, 'lc')
      else
         fdf_islreal = .false.
      endif
      if (fdf_output) write(fdf_out,'(a,5x,l10)') "#:lreal? " // label, &
          fdf_islreal

      RETURN
!--------------------------------------------------------------------------- END
    END FUNCTION fdf_islreal
