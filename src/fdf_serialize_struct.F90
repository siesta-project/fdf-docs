    subroutine fdf_serialize_struct(buffer)
    character(len=1), intent(inout), allocatable   :: buffer(:)

    character(len=:), allocatable ::  bufline
    type(line_dlist), pointer :: mark
    integer(ip) :: i, length, init, final

    integer :: nchars ! total size of serialized content

    if (allocated(buffer)) deallocate(buffer)

    ! Determine total length of buffer
    nchars = 0
    mark => file_in%first
    do i= 1, file_in%nlines
       call serialize_pline(mark%pline,length=length)
       nchars = nchars + length
       mark => mark%next
    enddo

    allocate(buffer(nchars))

    mark => file_in%first
    init = 1
    do i= 1, file_in%nlines
       call serialize_pline(mark%pline,bufline,length)
       final = init + length - 1
       call convert_string_to_array_of_chars(bufline,buffer(init:final))
       init = init + length
       mark => mark%next
    enddo

  end subroutine fdf_serialize_struct
