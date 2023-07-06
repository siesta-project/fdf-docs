    SUBROUTINE fdf_overwrite(line)
!--------------------------------------------------------------- Input Variables
      character(len=*) :: line

!--------------------------------------------------------------- Local Variables
      type(parsed_line), pointer  :: pline
      character(len=MAX_LENGTH)   :: label

      integer :: ierr

      pline => digest(line)
      if ( search('%block', pline) == 1 .or. &
          search('%endblock', pline) == 1 ) then

        ! We do not allow this in a single line
        call die('FDF module: fdf_overwrite', 'Error overwriting block (not implemented)',   &
            THIS_FILE, __LINE__, fdf_err, rc=ierr)

      else if ( search('%include', pline) == 1 ) then

        ! We do not allow this in a single line
        call die('FDF module: fdf_overwrite', 'Error overwriting flags from input file (not implemen
            THIS_FILE, __LINE__, fdf_err, rc=ierr)

      else if ( search('<', pline) /= -1 ) then

        ! We do not allow this in a single line
        call die('FDF module: fdf_overwrite', 'Error piping in overwriting (not implemented)',   &
            THIS_FILE, __LINE__, fdf_err, rc=ierr)

      else

        label = tokens(pline,1)
        call setmorphol(1, 'l', pline)
        call fdf_removelabel(label)

        ! Add token to the list of fdf-flags
        ! Since we add it directly we shouldn't destroy the pline
        call fdf_addtoken(line, pline)
        if ( fdf_debug ) then
          write(fdf_log,'(2a)') '---> Overwriting token: ', trim(label)
        end if

      end if

    END SUBROUTINE fdf_overwrite
