    SUBROUTINE fdf_bclose(bfdf)
      implicit none
!-------------------------------------------------------------- Output Variables
      type(block_fdf) :: bfdf

!--------------------------------------------------------------- Local Variables
      type(parsed_line), pointer :: pline
      integer(ip) :: i
      character(80) :: msg

!------------------------------------------------------------------------- BEGIN
!     Prevents using FDF routines without initialize
      if (.not. fdf_started) then
        call die('FDF module: fdf_bclose', 'FDF subsystem not initialized', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      ! Quick return (no need for errors)
      if ( .not. associated(bfdf%mark) ) return

      ! This should hopefully discourage compilers to optimize the loop away...
      i = 0
      do while ( fdf_bline(bfdf, pline) )
        i = i + fdf_bnvalues(pline)
      end do
      write(msg,'(a,i10)') 'Block ', i

      RETURN
!--------------------------------------------------------------------------- END
    END SUBROUTINE fdf_bclose
