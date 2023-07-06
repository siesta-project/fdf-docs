    SUBROUTINE fdf_initdata()
      implicit none
!--------------------------------------------------------------- Local Variables
      integer(ip) :: ierr

!------------------------------------------------------------------------- BEGIN
      ndepth = 0

      file_in%nlines = 0
      NULLIFY(file_in%first)
      NULLIFY(file_in%last)

      RETURN
!--------------------------------------------------------------------------- END
    END SUBROUTINE fdf_initdata
