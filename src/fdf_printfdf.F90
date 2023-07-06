    SUBROUTINE fdf_printfdf()
      implicit none
!--------------------------------------------------------------- Local Variables
      integer(ip)               :: i, ntokens
      character*1               :: id
      type(line_dlist), pointer :: dlp
      character(len=MAX_LENGTH) :: tok

!------------------------------------------------------------------------- BEGIN
      dlp => file_in%first

      write(fdf_log,*) '*** FDF Memory Structure Summary: ************'
      do while (ASSOCIATED(dlp))
        ntokens = dlp%pline%ntokens
        write(fdf_log,*) 'Line:', TRIM(dlp%str)
        write(fdf_log,*) 'Ntokens:', ntokens
        do i= 1, ntokens
          tok = tokens(dlp%pline,i)
          id  = dlp%pline%id(i)
          write(fdf_log,*) '  Token:', trim(tok), '(', dlp%pline%id(i), ')'
        enddo
        dlp => dlp%next
      enddo
      write(fdf_log,*) '**********************************************'

      RETURN
!--------------------------------------------------------------------------- END
    END SUBROUTINE fdf_printfdf
