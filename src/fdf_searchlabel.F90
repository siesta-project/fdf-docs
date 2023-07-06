    FUNCTION fdf_searchlabel(found, nelem, label, plabel)
      implicit none
!--------------------------------------------------------------- Input Variables
      integer(ip)                :: nelem
      logical                    :: found(nelem)
      character(*)               :: label
      type(parsed_line), pointer :: plabel

!-------------------------------------------------------------- Output Variables
      integer(ip)                :: fdf_searchlabel

!--------------------------------------------------------------- Local Variables
      logical                    :: found_elem
      integer(ip)                :: i

!------------------------------------------------------------------------- BEGIN
      i = 1
      found_elem = .FALSE.
      fdf_searchlabel = -1
      do while ((i .le. nelem) .and. (.not. found_elem))

        if (.not. found(i)) then
          if (labeleq(label, tokens(plabel, i))) then
            found_elem      = .TRUE.
            fdf_searchlabel = i
          endif
        endif
        i = i + 1
      enddo

      RETURN
!--------------------------------------------------------------------------- END
    END FUNCTION fdf_searchlabel
