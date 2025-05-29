! { dg-prune-output "using .vector_length \\(32\\)" }

! PR libgomp/93226  */

module m
  use iso_c_binding
  use openacc
  implicit none (external, type)

  integer, parameter :: N = 1024

  integer :: D(N)
  !$acc declare device_resident(D)

contains

  integer(c_intptr_t) function init_d()
    !$acc routine
    integer :: i
    do i = 1, N
      D(i) = 27*i
    end do
    init_d = loc(D)
  end
end module

program main
  use m
  implicit none (external, type)

  integer, allocatable, target :: a(:), b(:), e(:)
  type(c_ptr) :: d_a, d_b, d_c, d_d, d_e, d_f
  integer(c_intptr_t) intptr
  integer :: i
  logical fail

  fail = .false.

  allocate(a(N), b(N), e(N))
  d_c = acc_malloc (N*c_sizeof (i))
  d_f = acc_malloc (N*c_sizeof (i))

  e = huge(e)
  call acc_copyin (e, N*c_sizeof (i));
  d_e = acc_deviceptr (e);

  !$acc serial copyout(intptr)
    intptr = init_d ()
  !$acc end serial
  d_d = transfer(intptr, d_d)
  call acc_memcpy_device (d_c, d_d, N*c_sizeof (i))

  !$acc serial copy(fail) copy(a) deviceptr(d_c, d_d) firstprivate(intptr)
    block
      integer, pointer :: cc(:), dd(:)
      call c_f_pointer (d_c, cc, [N])
      call c_f_pointer (d_d, dd, [N])
      a = cc
      do i = 1, N
        if (dd(i) /= 27*i .or. cc(i) /= 27*i) then
          fail = .true.
          stop 1
        end if
      end do
    end block
  !$acc end serial
  if (fail) error stop 1

  do i = 1, N
    a(i) = 11*i
    b(i) = 31*i
  end do

  call acc_copyin (a, N*c_sizeof (i))
  d_a = acc_deviceptr (a)
  call acc_copyin_async (b, N*c_sizeof (i), acc_async_noval)

  !$acc parallel deviceptr(d_c) private(i) async
    block
      integer, pointer :: cc(:)
      call c_f_pointer (d_c, cc, [N])
      !$acc loop
      do i = 1, N
        cc(i) = -17*i
      end do
    end block
  !$acc end parallel

  call acc_memcpy_device_async (d_d, d_a, N*c_sizeof (i), acc_async_noval)
  call acc_memcpy_device_async (d_f, d_c, N*c_sizeof (i), acc_async_noval)
  call acc_wait (acc_async_noval)
  d_b = acc_deviceptr (b)
  call acc_memcpy_device_async (d_e, d_b, N*c_sizeof (i), acc_async_noval)
  call acc_wait (acc_async_noval)

  !$acc serial deviceptr(d_d, d_e, d_f) private(i) copy(fail)
    block
    integer, pointer :: dd(:), ee(:), ff(:)
    call c_f_pointer (d_d, dd, [N])
    call c_f_pointer (d_e, ee, [N])
    call c_f_pointer (d_f, ff, [N])
    do i = 1, N
      if (dd(i) /= 11*i        &
          .or. ee(i) /= 31*i   &
          .or. ff(i) /= -17*i) then
        fail = .true.
        stop 2
      end if
    end do
    end block
  !$acc end serial
  if (fail) error stop 2
end
