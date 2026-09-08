program main
  use iso_c_binding
  type (c_ptr) :: devaddr
  real(c_double), target :: data(10)
  integer(c_intptr_t) :: ip
  !$omp requires self_maps

  !$omp target enter data map(data)

  !$omp target map(from: devaddr)
  block
    devaddr = c_loc (data)
  end block
  if (transfer(devaddr, ip) /= transfer(c_loc(data), ip)) stop 1

  !$omp target map(from: devaddr)
  block
    devaddr = get_addr_from_function ()
  end block
  if (transfer(devaddr, ip) /= transfer(c_loc(data), ip)) stop 2

contains
  type (c_ptr) function get_addr_from_function ()
    get_addr_from_function = c_loc (data)
  end function get_addr_from_function

end program main
