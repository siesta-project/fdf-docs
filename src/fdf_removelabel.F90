    SUBROUTINE fdf_removelabel(label)
      implicit none
!--------------------------------------------------------------- Input Variables
      character(len=*)  :: label

!--------------------------------------------------------------- Local Variables
      type(line_dlist), pointer  :: mark

!------------------------------------------------------------------------- BEGIN

      do while ( fdf_locate(label,mark) )

         if (fdf_debug2) then
            write(fdf_log,*) '***FDF_REMOVELABEL*******************************'
            write(fdf_log,*) 'Line:', TRIM(mark%str)
            write(fdf_log,*) 'Label:', trim(label)
            write(fdf_log,*) '**********************************************'
         endif

         ! To circumvent the first/last line in the fdf-file
         ! we have to check for the existence of the
         ! first/last mark being the one removed.
         ! That special case *must* correct the first/last
         ! tokens.
         if ( associated(mark,target=file_in%first) ) then
            file_in%first => mark%next
         end if
         if ( associated(mark,target=file_in%last) ) then
            file_in%last => mark%prev
         end if

         ! Remove the label from the dynamic list
         call destroy(mark%pline)
         if ( associated(mark%prev) ) then
            mark%prev%next => mark%next
         end if
         if ( associated(mark%next) ) then
            mark%next%prev => mark%prev
         end if
         DEALLOCATE(mark)

         NULLIFY(mark)
      end do

    END SUBROUTINE fdf_removelabel
