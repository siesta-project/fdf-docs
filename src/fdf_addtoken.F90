    SUBROUTINE fdf_addtoken(line, pline)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(len=*) :: line
      type(parsed_line), pointer :: pline

!--------------------------------------------------------------- Local Variables
      integer(ip)                :: i, ierr
      type(line_dlist), pointer  :: mark

!------------------------------------------------------------------------- BEGIN
      ALLOCATE(mark, stat=ierr)
      if (ierr .ne. 0) then
        call die('FDF module: fdf_addtoken', 'Error allocating mark',   &
                 THIS_FILE, __LINE__, fdf_err, rc=ierr)
      endif

      mark%str   =  line
      mark%pline => pline
      NULLIFY(mark%next)

      ! Add entry at the END of structure
      if (ASSOCIATED(file_in%first)) then
        mark%prev         => file_in%last
        file_in%last%next => mark
      else
        NULLIFY(mark%prev)
        file_in%first => mark
      endif

      file_in%last => mark
      file_in%nlines = file_in%nlines + 1

      if (fdf_debug2) then
        write(fdf_log,*) '***FDF_ADDTOKEN*******************************'
        write(fdf_log,*) 'Line:', TRIM(mark%str)
        write(fdf_log,*) 'Ntokens:', mark%pline%ntokens
        do i= 1, mark%pline%ntokens
          write(fdf_log,*) '  Token:', trim(tokens(pline,i)), &
                           ' (', mark%pline%id(i), ')'
        enddo
        write(fdf_log,*) '**********************************************'
      endif

      RETURN
!--------------------------------------------------------------------------- END
    END SUBROUTINE fdf_addtoken
