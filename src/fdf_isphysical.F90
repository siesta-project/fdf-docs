    FUNCTION fdf_isphysical(label)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)                        :: label

!-------------------------------------------------------------- Output Variables
      logical                             :: fdf_isphysical

!--------------------------------------------------------------- Local Variables
      type(line_dlist), pointer           :: mark

!------------------------------------------------------------------------- BEGIN
!     Prevents using FDF routines without initialize
      if (.not. fdf_started) then
         call die('FDF module: fdf_isphysical', 'FDF subsystem not initialized', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      if (fdf_locate(label, mark)) then
         fdf_isphysical = match(mark%pline, 'lvn')
      else
         fdf_isphysical = .false.
      endif
      if (fdf_output) write(fdf_out,'(a,5x,l10)') "#:physical? " // label, fdf_isphysical

      RETURN
!--------------------------------------------------------------------------- END
    END FUNCTION fdf_isphysical
