      SUBROUTINE set_file_names( fileIn, fileOut, &
                                 optFileIn, optFileOut, unitIn )
      ! If present, copies input arguments optFileIn/Out to fileIn/Out.
      ! If absent, generates In/Out file names. If unitIn is present, and it is
      ! a named file, returns it as fileIn. If not, it copies input to a new
      ! file and returns its name. If .not.present(unitIn) => unitIn=5.
      ! If optFileIn is present, unitIn is ignored.
      implicit none
      character(len=*),intent(out):: &
        fileIn,    &! Name of file to be used as input
        fileOut     ! Name of file to be used as output
      character(len=*),optional,intent(in):: &
        optFileIn, &! Optional argument with input file name
        optFileOut  ! Optional argument with output file name
      integer,optional,intent(in):: &
        unitIn      ! Optional input file unit (not used if present(optFileIn))

      integer:: count, ierr, iostat, iu, iuIn
      logical:: named, opened
      character(len=MAX_LENGTH*2) line
      character(len=maxFileNameLength) fileName

!------------------------------------------------------------------------- BEGIN
      ! Find a job-specific number
      call system_clock( count )
      count = mod(count,100000)

      ! Set output file name
      if (present(optFileOut)) then
        if (len(trim(optFileOut)) > len(fileOut)) &
          call die('FDF module: set_file_names', &
                   'Parameter maxFileNameLength too small.' // &
                   'Terminating.', THIS_FILE, __LINE__, fdf_err, rc=ierr)
        fileOut = optFileOut
      else                  ! set a job-specific file name
        write(fileOut,'(a,i5.5,a)') 'fdf_',count,'.log'
      endif

      ! Set input file
      if (present(optFileIn)) then     ! just copy the file name
        if (len(trim(optFileIn)) > len(fileIn)) &
          call die('FDF module: set_file_names', &
                   'Parameter maxFileNameLength too small.' // &
                   'Terminating.', THIS_FILE, __LINE__, fdf_err, rc=ierr)
        fileIn = optFileIn
      else                             ! find or set a file name

        ! Find input file unit
        if (present(unitIn)) then      ! use given unit (possibly 5)
          iuIn = unitIn
        else                           ! assume standard input
          iuIn = 5
        endif

        ! Find file name associated with given unit
        if (iuIn==5) then              ! no valid file name
           fileName = ' '
        else                           ! check if this is a named file
          inquire(unit=iuIn,opened=opened)
          if (opened) then
            inquire(unit=iuIn,named=named)
            if (named) then            ! inquire file name
              inquire(unit=iuIn,name=fileName)
            else                       ! no valid file name
              fileName = ' '
            endif ! (named)
          else
            call die('FDF module: set_file_names', 'Input unit not opened.' // &
                     'Terminating.', THIS_FILE, __LINE__, fdf_err, rc=ierr)
          endif ! (opened)
        endif ! (iuIn==5)

        ! Set input file name, possibly after copying input to it
        if (fileName==' ') then                       ! not a valid file
          write(fileIn,'(a,i5.5,a)') &
            'INPUT_TMP_',count,'.fdf'                 ! new file's name
          call io_assign(iu)                          ! new file's unit
          open(iu,file=trim(fileIn),form='formatted') ! open new file
          do
            read(iuIn,iostat=iostat,fmt='(a)') line   ! read line from old unit
            if (iostat/=0 ) exit
            write(iu,'(a)') trim(line)                ! write line to new file
          enddo
          call io_close(iu)                           ! close new file
        else                                          ! valid file
          fileIn = fileName
        endif ! (fileName=='stdin')

      endif ! (present(optFileIn))
!--------------------------------------------------------------------------- END
      END SUBROUTINE set_file_names
