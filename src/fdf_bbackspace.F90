    FUNCTION fdf_bbackspace(bfdf,pline)
      implicit none
!--------------------------------------------------------------- Input Variables
      type(block_fdf)            :: bfdf

!-------------------------------------------------------------- Output Variables
      logical                    :: fdf_bbackspace
      type(parsed_line), pointer, optional :: pline

!--------------------------------------------------------------- Local Variables
      character(80)              :: strlabel

!------------------------------------------------------------------------- BEGIN
!     Prevents using FDF routines without initialize
      if (.not. fdf_started) then
        call die('FDF module: fdf_bbackspace', 'FDF subsystem not initialized', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      if (.not. ASSOCIATED(bfdf%mark)) then
        call die('FDF module: fdf_bbackspace', 'block_fdf structure not initialized', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      fdf_bbackspace = .TRUE.

!     If we are in the bottom of the block move to the content

      if (match(bfdf%mark%pline, 'el')) then

        strlabel = endblocks(bfdf%mark%pline)

        if (labeleq(strlabel, bfdf%label, fdf_log)) then
          bfdf%mark => bfdf%mark%prev

          if (fdf_output) write(fdf_out,'(1x,a)') "#:(Backspace to) " // "|" //  &
                                TRIM(bfdf%mark%str) // "|"
        endif

!     If we are at the head we cannot backspace

      else if (match(bfdf%mark%pline, 'bl')) then
        strlabel = blocks(bfdf%mark%pline)

        if (labeleq(strlabel, bfdf%label, fdf_log)) then
          fdf_bbackspace = .FALSE.
          if (fdf_output) write(fdf_out,'(1x,a)') "#:(Cannot backspace) " // "|" //  &
                                TRIM(bfdf%mark%str) // "|"
        endif

      else

        bfdf%mark => bfdf%mark%prev
        if (fdf_output) write(fdf_out,'(1x,a)') "#:(Backspace to) " // "|" //  &
                                TRIM(bfdf%mark%str) // "|"
      endif

      if ( present(pline) ) pline => bfdf%mark%pline

      RETURN
!--------------------------------------------------------------------------- END
    END FUNCTION fdf_bbackspace
