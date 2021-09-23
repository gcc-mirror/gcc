program main
  use omp_lib
  use iso_c_binding
  implicit none (type, external)

  integer :: d, id
  integer(kind=1), target :: a(4)
  integer(kind=1), pointer :: p, q

  d = omp_get_default_device ()
  id = omp_get_initial_device ()

  if (d < 0 .or. d >= omp_get_num_devices ()) &
    d = id

  a = transfer (int(z'cdcdcdcd'), mold=a)

  !$omp target enter data map (to:a)

  a = transfer (int(z'abababab'), mold=a)
  p => a(1)
  q => a(3)

  !$omp target enter data map (alloc:p, q)

  if (d /= id) then
    if (omp_target_is_present (c_loc(a), d) == 0) &
      stop 1
    if (omp_target_is_present (c_loc(p), d) == 0) &
      stop 2
    if (omp_target_is_present (c_loc(q), d) == 0) &
      stop 3
  end if

  !$omp target exit data map (release:a)

    if (d /= id) then
      if (omp_target_is_present (c_loc(a), d) == 0) &
        stop 4
      if (omp_target_is_present (c_loc(p), d) == 0) &
        stop 5
      if (omp_target_is_present (c_loc(q), d) == 0) &
        stop 6
    end if

  !$omp target exit data map (from:q)

    if (d /= id) then
      if (omp_target_is_present (c_loc(a), d) /= 0) &
        stop 7
      if (omp_target_is_present (c_loc(p), d) /= 0) &
        stop 8
      if (omp_target_is_present (c_loc(q), d) /= 0) &
        stop 9

      if (q /= int(z'cd', kind=1)) &
        stop 10
      if (p /= int(z'ab', kind=1)) &
        stop 11
    end if
end program main
