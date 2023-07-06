    SUBROUTINE fdf_dump(label)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)               :: label

!--------------------------------------------------------------- Local Variables
      character(80)              :: msg
      type(block_fdf)            :: bfdf
      type(parsed_line), pointer :: pline

!------------------------------------------------------------------------- BEGIN
      fdf_started = .TRUE.

      if (.not. fdf_block(label, bfdf)) then
        write(msg,*) 'block ', label, 'to dump not found'
        call die('FDF module: fdf_dump', msg, THIS_FILE, __LINE__, fdf_err)
      endif

!     fdf_bline prints each block line in fdf_out
      do while(fdf_bline(bfdf, pline))
      enddo

      fdf_started = .FALSE.

      RETURN
!--------------------------------------------------------------------------- END
    END SUBROUTINE fdf_dump
