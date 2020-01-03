! Check whether absent optional arguments are properly
! handled with use_device_{addr,ptr}.
program main
 use iso_c_binding, only: c_ptr, c_loc, c_associated
 implicit none (type, external)
 integer, allocatable :: a_w, a_x(:)
 integer, pointer :: p_w, p_x(:)

 nullify (p_w, p_x)
 call foo()

 ! unallocated/disassociated actual arguments to nonallocatable, nonpointer
 ! dummy arguments are regarded as absent
 call foo (w=a_w, x=a_x)
 call foo (w=p_w, x=p_x)

contains

  subroutine foo(v, w, x, y, z, cptr, cptr_in)
    integer, target, optional, value :: v
    integer, target, optional :: w
    integer, target, optional :: x(:)
    integer, target, optional, allocatable :: y
    integer, target, optional, allocatable :: z(:)
    type(c_ptr), target, optional, value :: cptr
    type(c_ptr), target, optional, value, intent(in) :: cptr_in
    integer :: d

    ! Need to map per-VALUE arguments, if present
    if (present(v)) then
      !$omp target enter data map(to:v)
      stop 1  ! – but it shall not be present in this test case.
    end if
    if (present(cptr)) then
      !$omp target enter data map(to:cptr)
      stop 2  ! – but it shall not be present in this test case.
    end if
    if (present(cptr_in)) then
      !$omp target enter data map(to:cptr_in)
      stop 3  ! – but it shall not be present in this test case.
    end if

    !$omp target data map(d) use_device_addr(v, w, x, y, z, cptr, cptr_in)
      if (present(v)) then; v    = 5; stop 11; endif
      if (present(w)) then; w    = 5; stop 12; endif
      if (present(x)) then; x(1) = 5; stop 13; endif
      if (present(y)) then; y    = 5; stop 14; endif
      if (present(z)) then; z(1) = 5; stop 15; endif
      if (present(cptr)) then; cptr = c_loc(v); stop 16; endif
      if (present(cptr_in)) then
        if (c_associated(cptr_in, c_loc(x))) stop 17
        stop 18
      endif
    !$omp end target data

! Using 'v' in use_device_ptr gives an ICE
! TODO: Find out what the OpenMP spec permits for use_device_ptr

    !$omp target data map(d) use_device_ptr(w, x, y, z, cptr, cptr_in)
      if (present(w)) then; w    = 5; stop 21; endif
      if (present(x)) then; x(1) = 5; stop 22; endif
      if (present(y)) then; y    = 5; stop 23; endif
      if (present(z)) then; z(1) = 5; stop 24; endif
      if (present(cptr)) then; cptr = c_loc(x); stop 25; endif
      if (present(cptr_in)) then
        if (c_associated(cptr_in, c_loc(x))) stop 26
        stop 27
      endif
    !$omp end target data
  end subroutine foo
end program main
