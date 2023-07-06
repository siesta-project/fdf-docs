    FUNCTION fdf_block(label, bfdf)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)    :: label

!-------------------------------------------------------------- Output Variables
      logical         :: fdf_block
      type(block_fdf) :: bfdf

!--------------------------------------------------------------- Local Variables
      character(80)   :: strlabel

!------------------------------------------------------------------------- BEGIN
!     Prevents using FDF routines without initialize
      if (.not. fdf_started) then
        call die('FDF module: fdf_block', 'FDF subsystem not initialized', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      fdf_block = .FALSE.

      bfdf%mark => file_in%first
      do while ((.not. fdf_block) .and. (ASSOCIATED(bfdf%mark)))

        if (match(bfdf%mark%pline, 'bl')) then
          strlabel = blocks(bfdf%mark%pline)

          if (labeleq(strlabel, label, fdf_log)) then
            fdf_block = .TRUE.
            bfdf%label = label

            if (fdf_output) write(fdf_out,'(a,a)') '%block ', TRIM(label)
          endif
        endif

        bfdf%mark => bfdf%mark%next
      enddo

      if (.not. fdf_block) NULLIFY(bfdf%mark)

      RETURN
!--------------------------------------------------------------------------- END
    END FUNCTION fdf_block
