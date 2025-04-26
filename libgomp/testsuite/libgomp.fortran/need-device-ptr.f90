! Comprehensive non-array testcase for need_device_ptr / need_device_addr

module m
  use iso_c_binding
  implicit none (type, external)

  integer :: case = 0
contains
  subroutine var_ptr_f(n, x, y, z)
    integer, value :: n
    type(c_ptr) :: x
    type(c_ptr), value :: y
    type(c_ptr), optional :: z
    !$omp target is_device_ptr(x,y,z)
    block
      integer, pointer :: ix, iy, iz
      call c_f_pointer(x, ix)
      call c_f_pointer(y, iy)
      call c_f_pointer(z, iz)
      if (ix /= 52) stop n*10 + 1
      if (iy /= 85) stop n*10 + 2
      if (iz /= 52) stop n*10 + 5
    end block
  end
  subroutine base_ptr_f(n, x, y, z)
    !$omp declare variant(var_ptr_f) match(construct={dispatch}) adjust_args(need_device_ptr : x, y, z)
    integer, value :: n
    type(c_ptr) :: x
    type(c_ptr), value :: y
    type(c_ptr), optional :: z
    error stop n
  end

  subroutine var_caddr_f(x, y)
    type(c_ptr) :: x
    type(c_ptr), optional :: y
    !$omp target has_device_addr(x, y)
    block
      integer, pointer :: ix, iy
      call c_f_pointer(x, ix)
      call c_f_pointer(x, iy)
      if (ix /= 52) stop 3
      if (iy /= 85) stop 6
    end block
  end
! FIXME: optional args give a "sorry".
!  subroutine base_caddr_f(x, y)
!    !$omp declare variant(var_caddr_f) match(construct={dispatch}) adjust_args(need_device_addr : x, y)
!    type(c_ptr) :: x
!    type(c_ptr), optional :: y
!    error stop
!  end

  subroutine var_iaddr_f(x,y)
    integer :: x
    integer, optional :: y
    !$omp target has_device_addr(x, y)
    block
      if (x /= 52) stop 4
      if (y /= 85) stop 4
    end block
  end

! FIXME: optional args give a "sorry".
!  subroutine base_iaddr_f(x,y)
!    !$omp declare variant(var_iaddr_f) match(construct={dispatch}) adjust_args(need_device_addr : x, y)
!    integer :: x
!    integer, optional :: y
!    error stop
!  end

  subroutine test_f(carg1, carg2, carg1v, carg2v, iarg1, iarg2)
    type(c_ptr) :: carg1, carg2
    type(c_ptr), value :: carg1v, carg2v
    integer, target :: iarg1, iarg2
    type(c_ptr) :: cptr1, cptr2
    integer, target :: ivar1, ivar2


    ivar1 = 52
    ivar2 = 85

    !$omp target enter data map(to: ivar1, ivar2)

    cptr1 = c_loc(ivar1)
    cptr2 = c_loc(ivar2)

    !$omp dispatch
       call base_ptr_f (1, carg1, carg2, carg1)
    !$omp dispatch
       call base_ptr_f (2, carg1v, carg2v, carg1v)
    !$omp dispatch
       call base_ptr_f (3, cptr1, cptr2, cptr1)
    !$omp dispatch
       call base_ptr_f (4, c_loc(iarg1), c_loc(iarg2), c_loc(iarg1))
    !$omp dispatch
       call base_ptr_f (6, c_loc(ivar1), c_loc(ivar2), c_loc(ivar1))

! FIXME: optional argument functions not supported yet.
!    !$omp dispatch
!       call base_caddr_f (carg1, carg2)
!    !$omp dispatch
!       call base_caddr_f (carg1v, carg2v)
!    !$omp dispatch
!       call base_caddr_f (cptr1, cptr2)
!    !$omp dispatch
!       call base_caddr_f (c_loc(iarg1), c_loc(iarg2))
!    !$omp dispatch
!       call base_caddr_f (c_loc(ivar1), c_loc(ivar2))
!    !$omp dispatch
!       call base_iaddr_f (iarg1, iarg2)
!    !$omp dispatch
!       call base_iaddr_f (ivar1, iarg2)

    !$omp target exit data map(release: ivar1, ivar2)
  end
end module m

use m
implicit none
integer, target :: mx, my
type(c_ptr) :: cptr1, cptr2
mx = 52
my = 85

cptr1 = c_loc(mx)
cptr2 = c_loc(my)

!$omp target data map(to: mx, my)
  call test_f (cptr1, cptr2, cptr1, cptr2, mx, my)
!$omp end target data
end
