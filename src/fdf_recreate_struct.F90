  subroutine fdf_recreate_struct(bufferFDF)
    character(len=1), intent(in)    :: bufferFDF(:)

    character(len=:), allocatable ::  bufline, header
    type(parsed_line), pointer    :: pline
    integer(ip) :: pos, length, serial_length, ntok

    pos = 0
    allocate(character(len=11) :: header)
    do
       call convert_array_of_chars_to_string(bufferFDF(pos+1:pos+11),header)
       read(header,"(1x,i5,1x,i4)") length, ntok
       serial_length =  length  + 12 + 10*ntok
       allocate(character(len=serial_length) :: bufline)
       call convert_array_of_chars_to_string(bufferFDF(pos+1:pos+serial_length),bufline)
       allocate(pline)
       call recreate_pline(pline,bufline)
       deallocate(bufline)
       call fdf_addtoken(pline%line,pline)
       pos = pos+serial_length
       if (pos >= size(bufferFDF)) EXIT
    enddo
    deallocate(header)

  end subroutine fdf_recreate_struct
