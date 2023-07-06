      logical function is_false(valstr)  result(a)
      character(len=*), intent(in) :: valstr
      a = leqi(valstr, 'no')      .or. leqi(valstr, 'false') .or. &
          leqi(valstr, '.false.') .or. leqi(valstr, 'f')     .or. &
          leqi(valstr, 'n')
      end function is_false
