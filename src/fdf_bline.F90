    FUNCTION fdf_bline(bfdf, pline)
      implicit none
!--------------------------------------------------------------- Input Variables
      type(block_fdf)            :: bfdf

!-------------------------------------------------------------- Output Variables
      logical                    :: fdf_bline
      type(parsed_line), pointer :: pline

!--------------------------------------------------------------- Local Variables
      character(80)              :: strlabel

!------------------------------------------------------------------------- BEGIN
!     Prevents using FDF routines without initialize
      if (.not. fdf_started) then
        call die('FDF module: fdf_bline', 'FDF subsystem not initialized', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      if (.not. ASSOCIATED(bfdf%mark)) then
        call die('FDF module: fdf_bline', 'block_fdf structure not initialized', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      fdf_bline = .TRUE.

!     If we are in the head of the block move to the content
      if (match(bfdf%mark%pline, 'bl')) then
        strlabel = blocks(bfdf%mark%pline)

        if (labeleq(strlabel, bfdf%label, fdf_log)) then
          bfdf%mark => bfdf%mark%next

          if (fdf_output) write(fdf_out,'(a,a)') '%block ', TRIM(bfdf%label)
        endif
      endif

      if (match(bfdf%mark%pline, 'el')) then
        strlabel = endblocks(bfdf%mark%pline)

        if (labeleq(strlabel, bfdf%label, fdf_log)) then
          fdf_bline = .FALSE.
          NULLIFY(pline)

          if (fdf_output) write(fdf_out,'(a,a)') '%endblock ', TRIM(bfdf%label)
        endif
      endif

      if (fdf_bline) then
        if (fdf_output) write(fdf_out,'(1x,a)') TRIM(bfdf%mark%str)

        pline     => bfdf%mark%pline
        bfdf%mark => bfdf%mark%next
      endif

      RETURN
!--------------------------------------------------------------------------- END
    END FUNCTION fdf_bline
