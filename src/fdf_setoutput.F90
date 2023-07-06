    SUBROUTINE fdf_setoutput(level,fileout_in)
      implicit none
!------------------------------------------------------------- Input Variables
      integer(ip)                  :: level
      character(len=*), intent(in) :: fileout_in


      character(len=256) :: fileout

      fileout = fileout_in
      if (level .le. 0) then
        if (fdf_output) then
          call io_close(fdf_out)
          fdf_output = .FALSE.
        endif
      else
        if (.not. fdf_output) then
          call io_assign(fdf_out)
          open(fdf_out, file=fileout, form='formatted',               &
               status='unknown')
          REWIND(fdf_out)
          fdf_output = .TRUE.
        endif
      endif
!----------------------------------------------------------------------- END
    END SUBROUTINE fdf_setoutput
