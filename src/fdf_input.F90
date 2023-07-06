    SUBROUTINE fdf_input(filein)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)  :: filein

!------------------------------------------------------------------------- BEGIN

      call fdf_read(filein)

      if (fdf_output) write(fdf_out,'(a,a,a,i3)') '#FDF module: Opened ', filein,   &
                                  ' for input. Unit:', fdf_in(1)

      RETURN
!--------------------------------------------------------------------------- END
    END SUBROUTINE fdf_input
