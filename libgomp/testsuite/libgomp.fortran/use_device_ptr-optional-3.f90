! Check whether absent optional arguments are properly
! handled with use_device_{addr,ptr}.
program main
  use iso_c_binding, only: c_ptr, c_loc, c_associated, c_f_pointer
  implicit none (type, external)

  integer, target :: u
  integer, target :: v
  integer, target :: w
  integer, target :: x(4)
  integer, target, allocatable :: y
  integer, target, allocatable :: z(:)
  type(c_ptr), target :: cptr
  type(c_ptr), target :: cptr_in
  integer :: dummy

  u = 42
  v = 5
  w = 7
  x = [3,4,6,2]
  y = 88
  z = [1,2,3]

  !$omp target enter data map(to:u)
  !$omp target data map(to:dummy) use_device_addr(u)
   cptr_in = c_loc(u) ! Has to be outside 'foo' due to 'intent(in)'
  !$omp end target data

  call foo (u, v, w, x, y, z, cptr, cptr_in)
  deallocate (y, z)
contains
  subroutine foo (u, v, w, x, y, z, cptr, cptr_in)
    integer, target, optional, value :: v
    integer, target, optional :: u, w
    integer, target, optional :: x(:)
    integer, target, optional, allocatable :: y
    integer, target, optional, allocatable :: z(:)
    type(c_ptr), target, optional, value :: cptr
    type(c_ptr), target, optional, value, intent(in) :: cptr_in
    integer :: d

    type(c_ptr) :: p_u, p_v, p_w, p_x, p_y, p_z, p_cptr, p_cptr_in

    !$omp target enter data map(to:w, x, y, z)
    !$omp target data map(dummy) use_device_addr(x)
      cptr = c_loc(x)
    !$omp end target data

    ! Need to map per-VALUE arguments, if present
    if (present(v)) then
      !$omp target enter data map(to:v)
    else
      stop 1
    end if
    if (present(cptr)) then
      !$omp target enter data map(to:cptr)
    else
      stop 2
    end if
    if (present(cptr_in)) then
      !$omp target enter data map(to:cptr_in)
    else
      stop 3
    end if

    !$omp target data map(d) use_device_addr(u, v, w, x, y, z)
    !$omp target data map(d) use_device_addr(cptr, cptr_in)
      if (.not. present(u)) stop 10
      if (.not. present(v)) stop 11
      if (.not. present(w)) stop 12
      if (.not. present(x)) stop 13
      if (.not. present(y)) stop 14
      if (.not. present(z)) stop 15
      if (.not. present(cptr)) stop 16
      if (.not. present(cptr_in)) stop 17
      p_u = c_loc(u)
      p_v = c_loc(v)
      p_w = c_loc(w)
      p_x = c_loc(x)
      p_y = c_loc(y)
      p_z = c_loc(z)
      p_cptr = c_loc(cptr)
      p_cptr_in = c_loc(cptr_in)
    !$omp end target data
    !$omp end target data
    call check(p_u, p_v, p_w, p_x, p_y, p_z, p_cptr, p_cptr_in, size(x), size(z))
  end subroutine foo

  subroutine check(p_u, p_v, p_w, p_x, p_y, p_z, p_cptr, p_cptr_in, Nx, Nz)
    type(c_ptr), value :: p_u, p_v, p_w, p_x, p_y, p_z, p_cptr, p_cptr_in
    integer, value :: Nx, Nz
    integer, pointer :: c_u(:), c_v(:), c_w(:), c_x(:), c_y(:), c_z(:)
    type(c_ptr), pointer :: c_cptr(:), c_cptr_in(:)

    ! As is_device_ptr does not handle scalars, we map them to a size-1 array
    call c_f_pointer(p_u, c_u, shape=[1])
    call c_f_pointer(p_v, c_v, shape=[1])
    call c_f_pointer(p_w, c_w, shape=[1])
    call c_f_pointer(p_x, c_x, shape=[Nx])
    call c_f_pointer(p_y, c_y, shape=[1])
    call c_f_pointer(p_z, c_z, shape=[Nz])
    call c_f_pointer(p_cptr, c_cptr, shape=[1])
    call c_f_pointer(p_cptr_in, c_cptr_in, shape=[1])
    call run_target(c_u, c_v, c_w, c_x, c_y, c_z, c_cptr, c_cptr_in, Nx, Nz)
  end subroutine check

  subroutine run_target(c_u, c_v, c_w, c_x, c_y, c_z, c_cptr, c_cptr_in, Nx, Nz)
    integer, target :: c_u(:), c_v(:), c_w(:), c_x(:), c_y(:), c_z(:)
    type(c_ptr) :: c_cptr(:), c_cptr_in(:)
    integer, value :: Nx, Nz
    !$omp target is_device_ptr(c_u, c_v, c_w, c_x, c_y, c_z, c_cptr, c_cptr_in) map(to:Nx, Nz)
      call target_fn(c_u(1), c_v(1), c_w(1), c_x, c_y(1), c_z, c_cptr(1), c_cptr_in(1), Nx, Nz)
    !$omp end target
  end subroutine run_target

  subroutine target_fn(c_u, c_v, c_w, c_x, c_y, c_z, c_cptr, c_cptr_in, Nx, Nz)
    !$omp declare target
    integer, target :: c_u, c_v, c_w, c_x(:), c_y, c_z(:)
    type(c_ptr), value :: c_cptr, c_cptr_in
    integer, value :: Nx, Nz
    integer, pointer :: u, x(:)
    if (c_u /= 42) stop 30
    if (c_v /= 5) stop 31
    if (c_w /= 7) stop 32
    if (Nx /= 4) stop 33
    if (any (c_x /= [3,4,6,2])) stop 34
    if (c_y /= 88) stop 35
    if (Nz /= 3) stop 36
    if (any (c_z /= [1,2,3])) stop 37
    if (.not. c_associated (c_cptr)) stop 38
    if (.not. c_associated (c_cptr_in)) stop 39
    if (.not. c_associated (c_cptr, c_loc(c_x))) stop 40
    if (.not. c_associated (c_cptr_in, c_loc(c_u))) stop 41
    call c_f_pointer(c_cptr_in, u)
    call c_f_pointer(c_cptr, x, shape=[Nx])
    if (u /= c_u .or. u /= 42)  stop 42
    if (any (x /= c_x))  stop 43
    if (any (x /= [3,4,6,2]))  stop 44
  end subroutine target_fn
end program main
