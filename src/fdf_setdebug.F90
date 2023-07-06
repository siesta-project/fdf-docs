    SUBROUTINE fdf_setdebug(level,filedebug)
      implicit none
!------------------------------------------------------------- Input Variables
      integer(ip)      :: level
      character(len=*) :: filedebug

!----------------------------------------------------------------------- BEGIN
      if (level .le. 0) then
        if (fdf_debug) then
          call io_close(fdf_log)
          fdf_debug = .FALSE.
        endif
      else
        if (.not. fdf_debug) then
          call io_assign(fdf_log)
          open(fdf_log, file=filedebug, form='formatted',               &
               status='unknown')
          REWIND(fdf_log)
          fdf_debug = .TRUE.

!         Set logging/debugging info for PARSE module also
          call setlog(fdf_log)
          call setdebug(1)
        endif
      endif

      fdf_debug2 = (level .ge. 2)

      RETURN
!----------------------------------------------------------------------- END
    END SUBROUTINE fdf_setdebug
