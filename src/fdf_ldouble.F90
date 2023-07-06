    SUBROUTINE fdf_ldouble(label,nv,list,line)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)                        :: label
      integer(ip)                         :: nv

!-------------------------------------------------------------- Output Variables
      real(dp)                            :: list(nv)
      type(line_dlist), pointer, optional :: line

!--------------------------------------------------------------- Local Variables
      character(80)                       :: msg
      type(line_dlist), pointer           :: mark
      integer(ip)                         :: lnv
      real(dp)                            :: llist(1)

!------------------------------------------------------------------------- BEGIN
!     Prevents using FDF routines without initialize
      if (.not. fdf_started) then
         call die('FDF module: fdf_ldouble', 'FDF subsystem not initialized', &
              THIS_FILE, __LINE__, fdf_err)
      endif

      if (fdf_locate(label, mark)) then
         if (.not. match(mark%pline, 'le')) then
            write(msg,*) 'no list value for ', label
            call die('FDF module: fdf_ldouble', msg, THIS_FILE, __LINE__, fdf_err)
         endif

         ! Retrieve length of list
         lnv = -1
         call valuelists(mark%pline,1,lnv,llist)
         if ( nv <= 0 ) then
            ! the user has requested size...
            nv = lnv
         else
            ! the list is not long enough
            if ( nv < lnv ) then
              write(msg, '(2a,2(a,i0))')'List ', trim(label), &
                  ' container too small: ', nv, ' versus ', lnv
              call die('FDF module: fdf_ldouble', trim(msg), &
                  THIS_FILE, __LINE__, fdf_err)
            end if
            call valuelists(mark%pline,1,nv,list)
         end if

         if (fdf_output) write(fdf_out,'(a,5x,i10)') label, lnv
      else
         write(msg,*) 'no list value for ', label
         call die('FDF module: fdf_ldouble', msg, THIS_FILE, __LINE__, fdf_err)
      endif

      if (PRESENT(line)) line = mark

      RETURN
!--------------------------------------------------------------------------- END
    END SUBROUTINE fdf_ldouble
