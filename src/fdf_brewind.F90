    SUBROUTINE fdf_brewind(bfdf)
      implicit none
!-------------------------------------------------------------- Output Variables
      type(block_fdf) :: bfdf

!--------------------------------------------------------------- Local Variables
      character(80)   :: msg

!------------------------------------------------------------------------- BEGIN
!     Prevents using FDF routines without initialize
      if (.not. fdf_started) then
        call die('FDF module: fdf_brewind', 'FDF subsystem not initialized', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      if (.not. ASSOCIATED(bfdf%mark)) then
        call die('FDF module: fdf_brewind', 'block_fdf structure not initialized', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      if (.not. fdf_block(bfdf%label, bfdf)) then
        write(msg,*) 'Block ', bfdf%label, ' not found in FDF structure'
        call die('FDF module: fdf_brewind', msg, &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      RETURN
!--------------------------------------------------------------------------- END
    END SUBROUTINE fdf_brewind
