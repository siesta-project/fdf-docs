    FUNCTION fdf_islist(label)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)                        :: label

!-------------------------------------------------------------- Output Variables
      logical                             :: fdf_islist

!--------------------------------------------------------------- Local Variables
      type(line_dlist), pointer           :: mark

!------------------------------------------------------------------------- BEGIN
!     Prevents using FDF routines without initialize
      if (.not. fdf_started) then
         call die('FDF module: fdf_islist', 'FDF subsystem not initialized', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      if (fdf_locate(label, mark)) then
        ! if it is a list:
        fdf_islist = match(mark%pline, 'le')
      else
         fdf_islist = .false.
      endif
      if (fdf_output) write(fdf_out,'(a,5x,l10)') "#:list? " // label, fdf_islist

      RETURN
!--------------------------------------------------------------------------- END
    END FUNCTION fdf_islist
