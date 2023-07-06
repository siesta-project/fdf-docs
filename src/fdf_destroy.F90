    SUBROUTINE fdf_destroy(fdfp)
      implicit none
!-------------------------------------------------------------- Output Variables
      type(fdf_file) :: fdfp

!------------------------------------------------------------------------- BEGIN
      if (ASSOCIATED(fdfp%first)) call fdf_destroy_dl(fdfp%first)

      RETURN
!--------------------------------------------------------------------------- END
    END SUBROUTINE fdf_destroy
