    FUNCTION fdf_readline(line)
      use m_read, only: get_line
      implicit none
!-------------------------------------------------------------- Output Variables
      logical      :: fdf_readline
      character(len=:), allocatable, intent(out)    :: line

!--------------------------------------------------------------- Local Variables
      integer(ip)  :: stat
      character(len=256) :: iomsg
!------------------------------------------------------------------------- BEGIN

      call get_line(fdf_in(ndepth), line, stat, iomsg)

      if (stat .eq. 0) then
        fdf_readline = .TRUE.
        if (fdf_debug2) write(fdf_log, '(a,a76)') 'fdf_readline > ', line
      else
        fdf_readline = .FALSE.
        if (fdf_debug2) write(fdf_log, '(a,a)') 'fdf_readline iomsg:> ', trim(iomsg)
      endif

      RETURN
!--------------------------------------------------------------------------- END
    END FUNCTION fdf_readline
