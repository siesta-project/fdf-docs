    SUBROUTINE fdf_addline(line)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(len=*) :: line

!--------------------------------------------------------------- Local Variables
      integer(ip)                :: ntok
      type(parsed_line), pointer :: pline

!------------------------------------------------------------------------- BEGIN

!     Check if valid data (tokens, non-blank)
      pline => digest(line)

      call setmorphol(1, 'l', pline)
      call fdf_addtoken(line, pline)

      if (fdf_debug2) then
         write(fdf_log,*) '***FDF_ADDLINE********************************'
         write(fdf_log,*) 'Line:', TRIM(line)
         write(fdf_log,*) '**********************************************'
      endif

    END SUBROUTINE fdf_addline
