!   Read an input file (and include files) and builds memory
!   structure that will contain the data and will help in searching
!
    RECURSIVE SUBROUTINE fdf_read(filein, blocklabel)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)               :: filein
      character(*), optional     :: blocklabel

!--------------------------------------------------------------- Local Variables
      logical                    :: dump
      logical, allocatable       :: found(:)
      character(80)              :: msg
      character(len=MAX_LENGTH)  :: label, inc_file
      character(len=:), allocatable :: line
      integer(ip)                :: i, ierr, ntok, ind_less, nlstart
      type(parsed_line), pointer :: pline

!------------------------------------------------------------------------- BEGIN
!     Open reading input file
      call fdf_open(filein)

!     Read each input data line
      if (PRESENT(blocklabel)) then
        label = blocklabel
      else
        label = ' '
      endif
      do while (fdf_readline(line))

!       Check if valid data (tokens, non-blank)
        pline => digest(line)
        ntok = ntokens(pline)
        if (ntok .ne. 0) then

!         Find different special cases in the input files
!         (%block, %endblock, %include, Label1 Label2 ... < Filename)

!         %block directive
          ind_less = search('<', pline)
          if (search('%block', pline) .eq. 1) then

!           No label found in %block directive
            if (ntok .eq. 1) then
              write(msg,*) '%block label not found in ', TRIM(filein)
              call die('FDF module: fdf_read', msg,                     &
                       THIS_FILE, __LINE__, fdf_err)
            endif

!           %block Label < Filename [ %dump ]
            if (ind_less .eq. 3) then

              if (ntok .ge. 4) then
!               Test if %dump is present
                if (search('%dump', pline) .eq. 5) then
                  dump = .TRUE.
                else
                  dump = .FALSE.
                endif

!               Add begin, body and end sections of block
                label = tokens(pline, 2)
                inc_file  = tokens(pline, 4)
                call destroy(pline)
                line = '%block ' // label
                pline => digest(line)
                call setmorphol(1, 'b', pline)
                call setmorphol(2, 'l', pline)
                call fdf_addtoken(line, pline)
                nullify(pline) ! it is stored in line

                nlstart = file_in%nlines
                call fdf_read(inc_file, label)

!               Warn if block 'label' is empty
                if ((nlstart - file_in%nlines) .eq. 0) then
                  write(msg,*) 'FDF module: fdf_read: block ',          &
                               TRIM(label), ' is empty...'
                  call warn(msg)
                endif

                line = '%endblock ' // label
                pline => digest(line)
                call setmorphol(1, 'e', pline)
                call setmorphol(2, 'l', pline)
                call fdf_addtoken(line, pline)
                nullify(pline) ! it is stored in line

!               Dump included file to fileout
                if (dump) call fdf_dump(label)
                label = ' '

!             Filename not found in %block directive
              else
                write(msg,*) '%block filename not found in ', TRIM(filein)
                call die('FDF module: fdf_read', msg,                   &
                         THIS_FILE, __LINE__, fdf_err)
              endif

!           %block Label
            elseif (ind_less .eq. -1) then
              label = tokens(pline, 2)
              call setmorphol(1, 'b', pline)
              call setmorphol(2, 'l', pline)
              call fdf_addtoken(line, pline)
              nullify(pline) ! it is stored in line
              nlstart = file_in%nlines

!           Bad format in %block directive
            else
              write(msg,*) 'Bad ''<'' %block format in ', TRIM(filein)
              call die('FDF module: fdf_read', msg,                     &
                       THIS_FILE, __LINE__, fdf_err)
            endif

!         %endblock directive
          elseif (search('%endblock', pline) .eq. 1) then
!           Check if %block exists before %endblock
            if (label .eq. ' ') then
              write(msg,*) 'Bad %endblock found in ', TRIM(filein)
              call die('FDF module: fdf_read', msg,                     &
                       THIS_FILE, __LINE__, fdf_err)
            else
!             Warn if block 'label' is empty
              if ((nlstart - file_in%nlines) .eq. 0) then
                write(msg,*) 'FDF module: fdf_read: block ',            &
                             TRIM(label), ' is empty...'
                call warn(msg)
              endif

              call destroy(pline)
              line = '%endblock ' // label
              pline => digest(line)
              call setmorphol(1, 'e', pline)
              call setmorphol(2, 'l', pline)
              call fdf_addtoken(line, pline)
              nullify(pline) ! it is stored in line
              label = ' '
            endif

!         %include Filename directive
          elseif (search('%include', pline) .eq. 1) then
!           Check if include filename is specified
            if (ntok .eq. 1) then
              write(msg,*) 'Filename on %include not found in ', TRIM(filein)
              call die('FDF module: fdf_read', msg,                     &
                       THIS_FILE, __LINE__, fdf_err)
            else
              inc_file = tokens(pline, 2)
              call fdf_read(inc_file)
            endif

            ! Clean pline (we simply insert the next file)
            call destroy(pline)

!         Label1 Label2 ... < Filename directive
          elseif (ind_less .ne. -1) then
!           Check if '<' is in a valid position
            if (ind_less .eq. 1) then
              write(msg,*) 'Bad ''<'' found in ', TRIM(filein)
              call die('FDF module: fdf_read', msg,                     &
                       THIS_FILE, __LINE__, fdf_err)

!           Check if '<' filename is specified
            elseif (ind_less .eq. ntok) then
              write(msg,*) 'Filename not found after ''<'' in ', TRIM(filein)
              call die('FDF module: fdf_read', msg,                     &
                       THIS_FILE, __LINE__, fdf_err)
            else
!             Search label(s) in Filename
              inc_file = tokens(pline, ind_less+1)
              ALLOCATE(found(ind_less-1), stat=ierr)
              if (ierr .ne. 0) then
                call die('FDF module: fdf_read', 'Error allocating found', &
                         THIS_FILE, __LINE__, fdf_err, rc=ierr)
              endif

!             If label(s) not found in such Filename throw an error
              found = .FALSE.
              if (.not. fdf_readlabel(ind_less-1, pline,                &
                                      inc_file, found)) then
                 i = 1
                 do while ((i .le. ind_less-1) .and. (found(i)))
                    i = i + 1
                 enddo
                 label = tokens(pline, i)
                 write(msg,*) 'Label ', TRIM(label),                     &
                             ' not found in ', TRIM(inc_file)
                 call die('FDF module: fdf_read', msg,                   &
                         THIS_FILE, __LINE__, fdf_err)
              endif

              call destroy(pline)
              DEALLOCATE(found)
            endif

!         Add remaining kind of tokens to dynamic list as labels
          else
            if (label .eq. ' ') call setmorphol(1, 'l', pline)
            call fdf_addtoken(line, pline)
            nullify(pline) ! it is stored in line
          endif
        else
!         Destroy parsed_line structure if no elements
          call destroy(pline)
        endif
      enddo

!     Close one level of input file
      if ((.not. PRESENT(blocklabel)) .and. (label .ne. ' ')) then
        write(msg,*) '%endblock ', TRIM(label),                         &
                     ' not found in ', TRIM(filein)
        call die('FDF module: fdf_read', msg, THIS_FILE, __LINE__, fdf_err)
      endif
      call fdf_close()

      RETURN
!--------------------------------------------------------------------------- END
    END SUBROUTINE fdf_read
