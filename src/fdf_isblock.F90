    FUNCTION fdf_isblock(label)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)                        :: label

!-------------------------------------------------------------- Output Variables
      logical                             :: fdf_isblock

!--------------------------------------------------------------- Local Variables
      type(line_dlist), pointer :: mark
      character(80) :: strlabel

!------------------------------------------------------------------------- BEGIN
!     Prevents using FDF routines without initialize
      if (.not. fdf_started) then
         call die('FDF module: fdf_isblock', 'FDF subsystem not initialized', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      fdf_isblock = .false.

      mark => file_in%first
      do while ( associated(mark) )

!!$        if ( match(mark%pline, 'l') ) then
!!$          strlabel = labels(mark%pline)
!!$
!!$          if ( labeleq(strlabel, label, fdf_log) ) then
!!$            ! fdf has first-encounter acceptance.
!!$            ! I.e. for an input
!!$            !   Label_Name 1
!!$            !   %block Label_Name
!!$            !     1
!!$            !   %endblock Label_Name
!!$            ! the former will be accepted first.
!!$            exit
!!$          end if

        if ( match(mark%pline, 'bl') ) then
          strlabel = blocks(mark%pline)

          if ( labeleq(strlabel, label, fdf_log) ) then
            fdf_isblock = .true.
            exit
          end if
        end if

        mark => mark%next
      end do

      if (fdf_output) write(fdf_out,'(a,5x,l10)') "#:block? " // label, fdf_isblock

      RETURN
!--------------------------------------------------------------------------- END
    END FUNCTION fdf_isblock
