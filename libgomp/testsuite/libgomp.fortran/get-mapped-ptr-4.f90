program main
  use omp_lib
  use iso_c_binding
  implicit none (external, type)
  integer :: d, id
  type(c_ptr) :: p1, p2

  type t
    integer :: m1, m2
  end type t
  type(t), target :: s

  d = omp_get_default_device ()
  id = omp_get_initial_device ()

  if (d < 0 .or. d >= omp_get_num_devices ()) &
    d = id

  if (d /= id) then
    !$omp target data map(alloc: s, s%m2) device(d)
      !$omp target map(from: p1, p2) map(alloc: s, s%m2) device(d)
      p1 = c_loc (s);
      p2 = c_loc (s%m2);
      !$omp end target

      if (.not. c_associated (omp_get_mapped_ptr (c_loc (s), d), p1) &
          .or. .not. c_associated (omp_get_mapped_ptr (c_loc (s%m2), d), p2)) &
        stop 0
    !$omp end target data

    if (c_associated (omp_get_mapped_ptr (c_loc (s), d)) &
        .or. c_associated (omp_get_mapped_ptr (c_loc (s%m2), d))) &
      stop 1

    !$omp target enter data map (alloc: s, s%m2) device (d)
      !$omp target map(from: p1, p2) map(alloc: s, s%m2) device(d)
      p1 = c_loc (s);
      p2 = c_loc (s%m2);
      !$omp end target

      if (.not. c_associated (omp_get_mapped_ptr (c_loc (s), d), p1) &
          .or. .not. c_associated (omp_get_mapped_ptr (c_loc (s%m2), d), p2)) &
        stop 2
    !$omp target exit data map (delete: s, s%m2) device (d)

    if (c_associated (omp_get_mapped_ptr (c_loc (s), d)) &
        .or. c_associated (omp_get_mapped_ptr (c_loc (s%m2), d))) &
      stop 3

  else ! d == id

    !$omp target data map(alloc: s, s%m2) device(d)
      !$omp target map(from: p1, p2) map(alloc: s, s%m2) device(d)
      p1 = c_loc (s);
      p2 = c_loc (s%m2);
      !$omp end target

      if (.not. c_associated (omp_get_mapped_ptr (c_loc (s), d), c_loc (s)) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (s%m2), d), c_loc (s%m2))) &
        stop 4
    !$omp end target data

    if (.not. c_associated (omp_get_mapped_ptr (c_loc (s), d), c_loc (s)) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (s%m2), d), c_loc (s%m2))) &
      stop 5

    !$omp target enter data map (alloc: s, s%m2) device (d)
      !$omp target map(from: p1, p2) map(alloc: s, s%m2) device(d)
      p1 = c_loc (s);
      p2 = c_loc (s%m2);
      !$omp end target

      if (.not. c_associated (omp_get_mapped_ptr (c_loc (s), d), c_loc (s)) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (s%m2), d), c_loc (s%m2))) &
        stop 6

    !$omp target exit data map (delete: s, s%m2) device (d)

    if (.not. c_associated (omp_get_mapped_ptr (c_loc (s), d), c_loc (s)) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (s%m2), d), c_loc (s%m2))) &
      stop 7
  end if

end program main
