    SUBROUTINE fdf_close()
      implicit none
!------------------------------------------------------------------------- BEGIN
      if (ndepth .ge. 1) then
        call io_close(fdf_in(ndepth))
        if (fdf_debug)                                                  &
          write(fdf_log,'(a,i1,a)') '---> Closed [DEPTH:', ndepth,']'
        ndepth = ndepth - 1
      endif

      RETURN
!--------------------------------------------------------------------------- END
    END SUBROUTINE fdf_close
