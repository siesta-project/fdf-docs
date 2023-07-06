    subroutine fdf_obsolete(label)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)           :: label

!------------------------------------------------------------------------- BEGIN
      if ( fdf_defined(label) ) then
         if (fdf_output) write(fdf_out,'(a)') "#**Warning: FDF symbol '"//trim(label)// &
              "' is obsolete."
      end if

!--------------------------------------------------------------------------- END
    end subroutine fdf_obsolete
