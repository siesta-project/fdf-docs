    SUBROUTINE fdf_open(filename)
      implicit none
!-------------------------------------------------------------- Output Variables
      character(*)  :: filename

!--------------------------------------------------------------- Local Variables
      logical       :: file_exists
      character(80) :: msg
      integer(ip)   :: lun

!------------------------------------------------------------------------- BEGIN
      ndepth = ndepth + 1
      if (ndepth .gt. maxdepth) then
        call die('FDF module: fdf_open', 'Too many nested fdf files...', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      if (leqi(filename, 'stdin')) then
        lun = INPUT_UNIT
        if (fdf_debug) write(fdf_log,'(a,i1,a)')                        &
          '---> Reading from standard input [DEPTH:', ndepth,'] '
      else
        call io_assign(lun)

        INQUIRE(file=filename, exist=file_exists)
        if (file_exists) then
          open(unit=lun, file=filename, status='old', form='formatted')
          REWIND(lun)
          if (fdf_debug) write(fdf_log,'(a,i1,a,a)')                    &
            '---> Opened [DEPTH:', ndepth,'] ', TRIM(filename)
        else
          write(msg,'(a,a)') 'Cannot open ', TRIM(filename)
          call die('FDF module: fdf_open', msg, THIS_FILE, __LINE__, fdf_err)
        endif
      endif

      fdf_in(ndepth) = lun
      REWIND(fdf_in(ndepth))

      RETURN
!--------------------------------------------------------------------------- END
    END SUBROUTINE fdf_open
