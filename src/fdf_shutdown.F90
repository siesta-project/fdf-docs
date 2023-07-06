    SUBROUTINE fdf_shutdown()
      implicit none
!------------------------------------------------------------------------- BEGIN
!$OMP SINGLE
      if (fdf_started) then
        call fdf_destroy(file_in)
        fdf_started = .FALSE.

        call io_close(fdf_out)
      endif
!$OMP END SINGLE

      RETURN
!--------------------------------------------------------------------------- END
    END SUBROUTINE fdf_shutdown
