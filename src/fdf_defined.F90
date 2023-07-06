    logical FUNCTION fdf_defined(label)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(*)              :: label

!--------------------------------------------------------------- Local Variables
      type(line_dlist), pointer :: mark

!--------------------------------------------------------------------- BEGIN
      ! First, check whether a single label exists:
      fdf_defined = fdf_locate(label, mark)
      if (.not. fdf_defined) then
         ! Check whether there is a block with that label
         fdf_defined = fdf_isblock(label)
      endif
      if ( fdf_output ) then
        write(fdf_out,'(a,5x,l10)') '#:defined? ' // label, fdf_defined
      endif

      RETURN
!----------------------------------------------------------------------- END
    END FUNCTION fdf_defined
