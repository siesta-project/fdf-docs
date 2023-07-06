    RECURSIVE FUNCTION fdf_readlabel(nelem, plabel, filein, found) result(readlabel)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)               :: filein
      integer(ip)                :: nelem
      type(parsed_line), pointer :: plabel

!-------------------------------------------------------------- Output Variables
      logical                    :: readlabel
      logical                    :: found(nelem)

!--------------------------------------------------------------- Local Variables
      logical                    :: dump, found_elem
      logical, pointer           :: found_loc(:)
      character(80)              :: msg
      character(len=:), allocatable :: line
      character(len=MAX_LENGTH)  :: inc_file, label
      integer(ip)                :: i, ierr, ntok, ind_less, nlstart
      integer(ip)                :: elem, nelem_loc
      integer(ip), pointer       :: found_index(:)
      type(parsed_line), pointer :: pline

!------------------------------------------------------------------------- BEGIN
!     Open input file with labels
      call fdf_open(filein)

!     While not reach to end of file and found all labels
      do while (fdf_readline(line) .and. (.not. ALL(found)))

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
              call die('FDF module: fdf_readlabel', msg,                &
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

                label = tokens(pline, 2)
                elem  = fdf_searchlabel(found, nelem, label, plabel)

                inc_file = tokens(pline, 4)
                call destroy(pline)

!               If match with any label add [begin, body, end] of block
                if (elem .ne. -1) then
                  line = '%block ' // label
                  pline => digest(line)
                  call setmorphol(1, 'b', pline)
                  call setmorphol(2, 'l', pline)
                  call fdf_addtoken(line, pline)

                  nlstart = file_in%nlines
                  call fdf_read(inc_file, label)

!                 Warn if block 'label' is empty
                  if ((nlstart - file_in%nlines) .eq. 0) then
                    write(msg,*) 'FDF module: fdf_readlabel: block ',   &
                                 TRIM(label), ' is empty...'
                    call warn(msg)
                  endif

                  line = '%endblock ' // label
                  pline => digest(line)
                  call setmorphol(1, 'e', pline)
                  call setmorphol(2, 'l', pline)
                  call fdf_addtoken(line, pline)

!                 Dump included file to fileout
                  if (dump) call fdf_dump(label)

                  found(elem) = .TRUE.
                  label = ' '
                endif

!             Filename not found in %block directive
              else
                write(msg,*) 'Filename on %block not found in ', TRIM(filein)
                call die('FDF module: fdf_readlabel', msg,              &
                         THIS_FILE, __LINE__, fdf_err)
              endif

!           %block Label
            elseif (ind_less .eq. -1) then
              label = tokens(pline, 2)
              elem  = fdf_searchlabel(found, nelem, label, plabel)
              found_elem = .TRUE.

!             If match with any label add [begin,body,end] of block
              if (elem .ne. -1) then
                call setmorphol(1, 'b', pline)
                call setmorphol(2, 'l', pline)
                call fdf_addtoken(line, pline)
                nlstart = file_in%nlines

                found_elem = .FALSE.
                do while (fdf_readline(line) .and. (.not. found_elem))
                  pline => digest(line)
                  if (ntokens(pline) .ne. 0) then
                    if (search('%endblock', pline) .eq. 1) then
!                     Warn if block 'label' is empty
                      if ((nlstart - file_in%nlines) .eq. 0) then
                        write(msg,*) 'FDF module: fdf_readlabel: block ', &
                                     TRIM(label), ' is empty...'
                        call warn(msg)
                      endif

                      call destroy(pline)
                      line = '%endblock ' // label
                      pline => digest(line)
                      call setmorphol(1, 'e', pline)
                      call setmorphol(2, 'l', pline)
                      label = ' '

                      found_elem  = .TRUE.
                      found(elem) = .TRUE.
                    endif
                    call fdf_addtoken(line, pline)
                  endif
                enddo

!             Move to the end of the block
              else
                call destroy(pline)

                found_elem = .FALSE.
                do while (fdf_readline(line) .and. (.not. found_elem))
                  pline => digest(line)
                  if (search('%endblock', pline) .eq. 1) then
                    label = ' '
                    found_elem = .TRUE.
                  endif
                  call destroy(pline)
                enddo
              endif

!             Error due to %endblock not found
              if (.not. found_elem) then
                write(msg,*) '%endblock ', TRIM(label),                 &
                             ' not found in ', TRIM(filein)
                call die('FDF module: fdf_readlabel', msg,              &
                         THIS_FILE, __LINE__, fdf_err)
              endif

!           Bad format in %block directive
            else
              write(msg,*) 'Bad ''<'' %block format in ', TRIM(filein)
              call die('FDF module: fdf_readlabel', msg,                &
                       THIS_FILE, __LINE__, fdf_err)
            endif

!         %endblock directive
          elseif (search('%endblock', pline) .eq. 1) then
!           Bad if %endblock exists before %block
            write(msg,*) 'Bad %endblock found in ', TRIM(filein)
            call die('FDF module: fdf_readlabel', msg,                  &
                     THIS_FILE, __LINE__, fdf_err)

!         %include Filename directive
          elseif (search('%include', pline) .eq. 1) then
!           Check if include filename is specified
            if (ntok .eq. 1) then
              write(msg,*) 'Filename on %include not found in ', TRIM(filein)
              call die('FDF module: fdf_readlabel', msg,                &
                       THIS_FILE, __LINE__, fdf_err)
            else
              inc_file = tokens(pline, 2)
              call destroy(pline)
              readlabel = fdf_readlabel(nelem, plabel, inc_file, found)
            endif

!         Label1 Label2 ... < Filename directive
          elseif (ind_less .ne. -1) then
!           Check if '<' is in a valid position
            if (ind_less .eq. 1) then
              write(msg,*) 'Bad ''<'' found in ', TRIM(filein)
              call die('FDF module: fdf_readlabel', msg,                &
                       THIS_FILE, __LINE__, fdf_err)

!           Check if '<' filename is specified
            elseif (ind_less .eq. ntok) then
              write(msg,*) 'Filename not found after ''<'' in ', TRIM(filein)
              call die('FDF module: fdf_readlabel', msg,                &
                       THIS_FILE, __LINE__, fdf_err)
            else
!             Search label(s) in Filename
              line = ' '
              nelem_loc = 0
              ALLOCATE(found_index(ind_less-1), stat=ierr)
              if (ierr .ne. 0) then
                call die('FDF module: fdf_readlabel', 'Error allocating found_index', &
                         THIS_FILE, __LINE__, fdf_err, rc=ierr)
              endif
              do i= 1, ind_less-1
                label = tokens(pline, i)
                elem = fdf_searchlabel(found, nelem, label, plabel)
                if (elem .ne. -1) then
                  line = TRIM(line) // ' ' // TRIM(label)
                  nelem_loc = nelem_loc + 1
                  found_index(nelem_loc) = elem
                endif
              enddo

!             Process Filename if any label found
              if (nelem_loc .ge. 1) then
                inc_file = tokens(pline, ind_less+1)
                call destroy(pline)

                ALLOCATE(found_loc(nelem_loc), stat=ierr)
                if (ierr .ne. 0) then
                  call die('FDF module: fdf_readlabel', 'Error allocating found_loc', &
                           THIS_FILE, __LINE__, fdf_err, rc=ierr)
                endif

                found_loc = .FALSE.

!               If label(s) not found in such Filename throw an error
                pline => digest(line)
                if (.not. fdf_readlabel(nelem_loc, pline,               &
                                        inc_file, found_loc)) then
                  i = 1
                  do while ((i .le. nelem_loc) .and. (found_loc(i)))
                    i = i + 1
                  enddo
                  label = tokens(pline, i)
                  write(msg,*) 'Label ', TRIM(label), ' not found in ', TRIM(inc_file)
                  call die('FDF module: fdf_readlabel', msg,            &
                           THIS_FILE, __LINE__, fdf_err)
                else
!                 Merge results if all labels found
                  do i= 1, nelem_loc
                    found(found_index(i)) = found_loc(i)
                  enddo
                endif

                DEALLOCATE(found_index)
              endif

              DEALLOCATE(found_loc)
              call destroy(pline)
            endif

!         Label [ Value ] directive
          else
            elem = fdf_searchlabel(found, nelem, tokens(pline, 1), plabel)

!           If match with any label add it
            if (elem .ne. -1) then
              call setmorphol(1, 'l', pline)
              call fdf_addtoken(line, pline)
              found(elem) = .TRUE.
            else
!             Destroy parsed_line structure if no label found
              call destroy(pline)
            endif
          endif

        else
!         Destroy parsed_line structure if no label found
          call destroy(pline)
        endif
      enddo

!     Close input file with labels
      call fdf_close()

      readlabel = ALL(found)
      RETURN
!--------------------------------------------------------------------------- END
    END FUNCTION fdf_readlabel
