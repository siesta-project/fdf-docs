    SUBROUTINE fdf_linteger(label,ni,list,line)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)                        :: label
      integer(ip)                         :: ni

!-------------------------------------------------------------- Output Variables
      integer(ip)                         :: list(ni)
      type(line_dlist), pointer, optional :: line

!--------------------------------------------------------------- Local Variables
      character(80)                       :: msg
      type(line_dlist), pointer           :: mark
      integer(ip)                         :: lni, llist(1)

!------------------------------------------------------------------------- BEGIN
!     Prevents using FDF routines without initialize
      if (.not. fdf_started) then
         call die('FDF module: fdf_linteger', 'FDF subsystem not initialized', &
              THIS_FILE, __LINE__, fdf_err)
      endif

      if (fdf_locate(label, mark)) then
         if (.not. match(mark%pline, 'la')) then
            write(msg,*) 'no list value for ', label
            call die('FDF module: fdf_linteger', msg, THIS_FILE, __LINE__, fdf_err)
         endif

         ! Retrieve length of list
         lni = -1
         call integerlists(mark%pline,1,lni,llist)
         if ( ni <= 0 ) then
            ! the user has requested size...
            ni = lni
         else
            ! the list is not long enough
            if ( ni < lni ) then
              write(msg, '(2a,2(a,i0))')'List ', trim(label), &
                  ' container too small: ', ni, ' versus ', lni
              call die('FDF module: fdf_linteger', trim(msg), &
                  THIS_FILE, __LINE__, fdf_err)
            end if
            call integerlists(mark%pline,1,ni,list)
         end if

         ! find a way to write out the list anyway
         if (fdf_output) write(fdf_out,'(a,5x,i10)') label, lni
      else
         write(msg,*) 'no list value for ', label
         call die('FDF module: fdf_linteger', msg, THIS_FILE, __LINE__, fdf_err)
      endif

      if (PRESENT(line)) line = mark

      RETURN
!--------------------------------------------------------------------------- END
    END SUBROUTINE fdf_linteger
