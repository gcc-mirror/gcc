program main
  use omp_lib
  use iso_c_binding
  implicit none (external, type)
  integer :: d, id, i, j
  integer, target :: a, b(1:2), c(1:2), e(0:127)
  type(c_ptr) :: p1, p2, p3, q, devptrs(0:63)

  a = 42;
  q = c_loc (a);
  e = [(i, i = 0, 127)]

  d = omp_get_default_device ()
  id = omp_get_initial_device ()

  if (d < 0 .or. d >= omp_get_num_devices ()) &
    d = id

  if (d /= id) then
    !$omp target data map(alloc: a, b, c(2), e(32:95)) device(d)
      !$omp target map(from: p1, p2, p3, devptrs) map(alloc: a, b, c(2), e(32:95)) device(d)
      p1 = c_loc (a);
      p2 = c_loc (b);
      p3 = c_loc (c(2))
      devptrs = [(c_loc (e(i)), i = 32, 95)]
      !$omp end target

      if (.not. c_associated (omp_get_mapped_ptr (c_loc (a), d), p1) &
          .or. .not. c_associated (omp_get_mapped_ptr (q, d), p1) &
          .or. .not. c_associated (omp_get_mapped_ptr (c_loc (b), d), p2) &
          .or. .not. c_associated (omp_get_mapped_ptr (c_loc (b(1)), d), p2) &
          .or. .not. c_associated (omp_get_mapped_ptr (c_loc (c(2)), d), p3) &
          .or. c_associated (omp_get_mapped_ptr (c_loc (c(1)), d))) &
        stop 0

      do j = 0, 31
        if (c_associated (omp_get_mapped_ptr (c_loc (e(j)), d))) &
          stop 1
      end do
      do j = 32, 95
        if (.not. c_associated (omp_get_mapped_ptr (c_loc (e(j)), d), devptrs(j-32))) &
          stop 2
      end do
      do j = 96, 128
        if (c_associated (omp_get_mapped_ptr (c_loc (e(j)), d))) &
          stop 3
      end do
    !$omp end target data

    if (c_associated (omp_get_mapped_ptr (c_loc (a), d)) &
        .or. c_associated (omp_get_mapped_ptr (q, d)) &
        .or. c_associated (omp_get_mapped_ptr (c_loc (b), d)) &
        .or. c_associated (omp_get_mapped_ptr (c_loc (b(1)), d)) &
        .or. c_associated (omp_get_mapped_ptr (c_loc (c(2)), d)) &
        .or. c_associated (omp_get_mapped_ptr (c_loc (c(1)), d))) &
      stop 4
      do j = 0, 127
        if (c_associated (omp_get_mapped_ptr (c_loc (e(j)), d))) &
          stop 5
      end do

    !$omp target enter data map (alloc: a, b, c(2), e(32:95)) device (d)
      !$omp target map(from: p1, p2, p3, devptrs) map(alloc: a, b, c(2), e(32:95)) device(d)
      p1 = c_loc (a);
      p2 = c_loc (b);
      p3 = c_loc (c(2))
      devptrs = [(c_loc (e(i)), i = 32, 95)]
      !$omp end target

      if (.not. c_associated (omp_get_mapped_ptr (c_loc (a), d), p1) &
          .or. .not. c_associated (omp_get_mapped_ptr (q, d), p1) &
          .or. .not. c_associated (omp_get_mapped_ptr (c_loc (b), d), p2) &
          .or. .not. c_associated (omp_get_mapped_ptr (c_loc (c(2)), d), p3) &
          .or. c_associated (omp_get_mapped_ptr (c_loc (c(1)), d))) &
        stop 6

      do j = 0, 31
        if (c_associated (omp_get_mapped_ptr (c_loc (e(j)), d))) &
          stop 7
      end do
      do j = 32, 95
        if (.not. c_associated (omp_get_mapped_ptr (c_loc (e(j)), d), devptrs(j-32))) &
          stop 8
      end do
      do j = 96, 128
        if (c_associated (omp_get_mapped_ptr (c_loc (e(j)), d))) &
          stop 9
      end do
    !$omp target exit data map (delete: a, b, c(2), e(32:95)) device (d)

    if (c_associated (omp_get_mapped_ptr (c_loc (a), d)) &
        .or. c_associated (omp_get_mapped_ptr (q, d)) &
        .or. c_associated (omp_get_mapped_ptr (c_loc (b), d)) &
        .or. c_associated (omp_get_mapped_ptr (c_loc (b(1)), d)) &
        .or. c_associated (omp_get_mapped_ptr (c_loc (c(1)), d)) &
        .or. c_associated (omp_get_mapped_ptr (c_loc (c(2)), d))) &
      stop 10
    do j = 0, 127
      if (c_associated (omp_get_mapped_ptr (c_loc (e(j)), d))) &
        stop 11
    end do

  else ! d == id

    !$omp target data map(alloc: a, b, c(2), e(32:95)) device(d)
      !$omp target map(from: p1, p2, p3, devptrs) map(alloc: a, b, c(2), e(32:95)) device(d)
      p1 = c_loc (a);
      p2 = c_loc (b);
      p3 = c_loc (c(2))
      devptrs = [(c_loc (e(i)), i = 32, 95)]
      !$omp end target

      if (.not. c_associated (omp_get_mapped_ptr (c_loc (a), d), c_loc (a)) &
        .or. .not. c_associated (omp_get_mapped_ptr (q, d), q) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (b), d), c_loc (b)) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (b(1)), d), c_loc (b(1))) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (c(2)), d), c_loc (c(2))) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (c(1)), d), c_loc (c(1)))) &
      stop 12

      do j = 0, 127
        if (.not. c_associated (omp_get_mapped_ptr (c_loc (e(j)), d), c_loc (e(j)))) &
          stop 13
      end do
    !$omp end target data

    if (.not. c_associated (omp_get_mapped_ptr (c_loc (a), d), c_loc (a)) &
        .or. .not. c_associated (omp_get_mapped_ptr (q, d), q) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (b), d), c_loc (b)) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (b(1)), d), c_loc (b(1))) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (c(2)), d), c_loc (c(2))) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (c(1)), d), c_loc (c(1)))) &
      stop 14
    do j = 0, 127
      if (.not. c_associated (omp_get_mapped_ptr (c_loc (e(j)), d))) &
        stop 15
    end do

    !$omp target enter data map (alloc: a, b, c(2), e(32:95)) device (d)
      !$omp target map(from: p1, p2, p3, devptrs) map(alloc: a, b, c(2), e(32:95)) device(d)
      p1 = c_loc (a);
      p2 = c_loc (b);
      p3 = c_loc (c(2))
      devptrs = [(c_loc (e(i)), i = 32, 95)]
      !$omp end target

      if (.not. c_associated (omp_get_mapped_ptr (c_loc (a), d), c_loc (a)) &
        .or. .not. c_associated (omp_get_mapped_ptr (q, d), q) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (b), d), c_loc (b)) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (b(1)), d), c_loc (b(1))) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (c(2)), d), c_loc (c(2))) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (c(1)), d), c_loc (c(1)))) &
        stop 16

      do j = 0, 127
        if (.not. c_associated (omp_get_mapped_ptr (c_loc (e(j)), d), c_loc (e(j)))) &
          stop 17
      end do
    !$omp target exit data map (delete: a, b, c(2), e(32:95)) device (d)

    if (.not. c_associated (omp_get_mapped_ptr (c_loc (a), d), c_loc (a)) &
        .or. .not. c_associated (omp_get_mapped_ptr (q, d), q) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (b), d), c_loc (b)) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (b(1)), d), c_loc (b(1))) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (c(2)), d), c_loc (c(2))) &
        .or. .not. c_associated (omp_get_mapped_ptr (c_loc (c(1)), d), c_loc (c(1)))) &
      stop 18

    do j = 0, 127
      if (.not. c_associated (omp_get_mapped_ptr (c_loc (e(j)), d), c_loc (e(j)))) &
        stop 19
    end do
  end if

end program main
