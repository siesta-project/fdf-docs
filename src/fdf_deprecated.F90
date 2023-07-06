    subroutine fdf_deprecated(label,newlabel)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)           :: label
      character(*)           :: newlabel

!------------------------------------------------------------------------- BEGIN
      if ( fdf_defined(label) ) then
         if (fdf_output) write(fdf_out,'(a)') "#**Warning: FDF symbol '"//trim(label)// &
              "' is deprecated."
         if ( fdf_defined(newlabel) ) then
            if (fdf_output) write(fdf_out,'(a)') "#           FDF symbol '"//trim(newlabel)// &
                 "' will be used instead."
         else
            if (fdf_output) write(fdf_out,'(a)') "#           FDF symbol '"//trim(newlabel)// &
                 "' replaces '"//trim(label)//"'."
         end if
      end if

!--------------------------------------------------------------------------- END
    end subroutine fdf_deprecated
