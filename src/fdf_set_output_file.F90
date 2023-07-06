    SUBROUTINE fdf_set_output_file(fileout)
      implicit none
!----------------------------------------------------- Input Variables
      character(len=*), intent(in)   :: fileout

!----------------------------------------------------- Local Variables
      character(256) :: fileouttmp
!----------------------------------------------------- BEGIN
      call io_assign(fdf_out)

      open( unit=fdf_out, file=TRIM(fileout), form='formatted', &
           access='sequential', status='replace' )

      RETURN
!--------------------------------------------------------------------------- END
    END SUBROUTINE fdf_set_output_file
