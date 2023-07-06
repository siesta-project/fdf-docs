    FUNCTION fdf_block_linecount(label, morph)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(len=*) :: label
      character(len=*), optional :: morph
!-------------------------------------------------------------- Output Variables
      integer(ip) :: fdf_block_linecount

!--------------------------------------------------------------- Local Variables
      type(block_fdf) :: bfdf
      type(parsed_line), pointer :: pline
      logical :: orig_fdf_output

!------------------------------------------------------------------------- BEGIN
!     Prevents using FDF routines without initialize
      if (.not. fdf_started) then
        call die('FDF module: fdf_block_linecount', 'FDF subsystem not initialized', &
                 THIS_FILE, __LINE__, fdf_err)
      endif

      ! Store the fdf_output variable (suppress writing to log)
      orig_fdf_output = fdf_output
      fdf_output = .false.

      ! Find the block and search for morhp
      fdf_block_linecount = 0
      if ( fdf_block(label, bfdf) ) then

        do while ( fdf_bline(bfdf, pline) )
          if ( present(morph) ) then
            if ( fdf_bmatch(pline, morph) ) then
              fdf_block_linecount = fdf_block_linecount + 1
            end if
          else
            fdf_block_linecount = fdf_block_linecount + 1
          end if
        end do

        call fdf_bclose(bfdf)

      end if

      ! Restore output
      fdf_output = orig_fdf_output

      if ( fdf_output ) then
        if ( present(morph) ) then
          write(fdf_out,'(3a,3x,i0)') "#:block-line-count? ", &
              trim(label), ' ('//trim(morph)//')', fdf_block_linecount
        else
          write(fdf_out,'(2a,3x,i0)') "#:block-line-count? ", &
              trim(label), fdf_block_linecount
        end if
      end if

      RETURN
!--------------------------------------------------------------------------- END
    END FUNCTION fdf_block_linecount
