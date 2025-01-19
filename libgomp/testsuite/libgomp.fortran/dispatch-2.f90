module m
  use iso_c_binding
  implicit none (type, external)
  type(c_ptr) :: ref1, ref2, ref3, ref4
contains
  subroutine foo(v, w, x, y)
    type(C_ptr) :: v, w, x, y
    value :: w, y
    optional :: x, y
    !$omp declare variant(bar) match ( construct = { dispatch } )   &
    !$omp&                     adjust_args(need_device_ptr : v, w, x, y )
    stop 1  ! should not get called
  end
  subroutine bar(a, b, c, d)
    type(C_ptr) :: a, b, c, d
    value :: b, d
    optional :: c, d
    if (.not. c_associated (a, ref1)) stop 2
    if (.not. c_associated (b, ref2)) stop 3
    if (.not. c_associated (c, ref3)) stop 3
    if (.not. c_associated (d, ref4)) stop 3
  end
end

program main
  use omp_lib
  use m
  implicit none (type, external)
  integer, target :: a, b, c, d
  type(c_ptr) :: v, w, y, z
  integer :: dev

  do dev = -1, omp_get_num_devices ()
    print *, 'dev ', dev

    ! Cross check (1)
    ref1 = omp_target_alloc (32_c_size_t, dev)
    ref2 = omp_target_alloc (32_c_size_t, dev)
    ref3 = omp_target_alloc (32_c_size_t, dev)
    ref4 = omp_target_alloc (32_c_size_t, dev)
    call bar (ref1, ref2, ref3, ref4)
    call omp_target_free (ref1, dev)
    call omp_target_free (ref2, dev)
    call omp_target_free (ref3, dev)
    call omp_target_free (ref4, dev)

    v = c_loc(a)
    w = c_loc(b)
    y = c_loc(b)
    z = c_loc(b)

    !$omp target enter data device(dev) map(a, b, c, d)

    ! Cross check (2)
    ! This should be effectively identical to 'dispatch'
    !$omp target data device(dev) use_device_ptr(v, w, y, z)
      ref1 = v
      ref2 = w
      ref3 = y
      ref4 = z
      call bar (v, w, y, z)
    !$omp end target data

    !$omp dispatch device(dev)
      call foo (v, w, y, z)

    !$omp target exit data device(dev) map(a, b, c, d)
  end do
end
