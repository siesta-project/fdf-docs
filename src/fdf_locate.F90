    FUNCTION fdf_locate(label, mark)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)              :: label

!-------------------------------------------------------------- Output Variables
      logical                   :: fdf_locate
      type(line_dlist), pointer :: mark

!--------------------------------------------------------------- Local Variables
      character(80)             :: strlabel

!------------------------------------------------------------------------- BEGIN
      fdf_locate = .FALSE.

!      if (fdf_donothing) return

      mark => file_in%first
      do while ((.not. fdf_locate) .and. (ASSOCIATED(mark)))

        if (match(mark%pline, 'l')) then
          strlabel = labels(mark%pline)

          if (labeleq(strlabel, label, fdf_log)) then
            fdf_locate = .TRUE.
          else
            mark => mark%next
          endif
        else
          mark => mark%next
        endif
      enddo

      if (.not. fdf_locate) NULLIFY(mark)

      RETURN
!--------------------------------------------------------------------------- END
    END FUNCTION fdf_locate
