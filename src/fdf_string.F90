    FUNCTION fdf_string(label, default, line)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)                        :: label
      character(*)                        :: default

!-------------------------------------------------------------- Output Variables
      character(80)                       :: fdf_string
      type(line_dlist), pointer, optional :: line

!--------------------------------------------------------------- Local Variables
      type(line_dlist), pointer           :: mark

!------------------------------------------------------------------------- BEGIN
!     Prevents using FDF routines without initialize
      if (.not. fdf_started) then
        call die('FDF module: fdf_string', 'FDF subsystem not initialized', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      if (fdf_locate(label, mark)) then
         if (ntokens(mark%pline) < 2) then
            fdf_string = ""
            if (fdf_output) write(fdf_out,'(a,5x,a)') label, &
             "#  *** Set to empty string *** "
         else
            ! Get all the characters spanning the space from the second to
            ! the last token
            fdf_string = characters(mark%pline, ind_init=2, ind_final=-1)
            if (fdf_output) write(fdf_out,'(a,5x,a)') label, fdf_string
         endif
      else
        fdf_string = default
        if (fdf_output) write(fdf_out,'(a,5x,a,5x,a)') label, default, '# default value'
      endif

      if (PRESENT(line)) line = mark

      RETURN
!--------------------------------------------------------------------------- END
    END FUNCTION fdf_string
