! { dg-do run }

module vars
  implicit none
  real b
  !$acc declare device_resident (b)

  integer :: x, y, z
  common /block/ x, y, z
  !$acc declare device_resident (/block/)
end module vars

subroutine set()
  use openacc
  implicit none
  integer :: a(5), b(1), c, vals(7)
  common /another/ a, b, c
  !$acc declare device_resident (/another/)
  if (.not. acc_is_present (a)) stop 10
  if (.not. acc_is_present (b)) stop 11
  if (.not. acc_is_present (c)) stop 12

  vals = 99
  ! NOTE: The current (Nov 2019) implementation requires the 'present'
  ! as it tries to otherwises map the device_resident variables;
  ! following OpenMP 4.0 semantic: 'a' + 'b' are 'copy' (map fromto) and
  ! 'c' is firstprivate.
  !$acc parallel copyout(vals) present(a, b, c)
    a = [11,12,13,14,15]
    b = 16
    c = 47
    vals(1:5) = a
    vals(6:6) = b
    vals(7) = c
  !$acc end parallel

  if (.not. acc_is_present (a)) stop 13
  if (.not. acc_is_present (b)) stop 14
  if (.not. acc_is_present (c)) stop 15

  if (any (vals /= [11,12,13,14,15,16,47])) stop 16
end subroutine set

subroutine check()
  use openacc
  implicit none
  integer :: g, h(3), i(3)
  common /another/ g, h, i
  integer :: val(7)
  !$acc declare device_resident (/another/)
  if (.not. acc_is_present (g)) stop 20
  if (.not. acc_is_present (h)) stop 21
  if (.not. acc_is_present (i)) stop 22

  val = 99
  !$acc parallel copyout(val) present(g, h, i)
    val(5:7) = i
    val(1) = g
    val(2:4) = h
  !$acc end parallel

  if (.not. acc_is_present (g)) stop 23
  if (.not. acc_is_present (h)) stop 24
  if (.not. acc_is_present (i)) stop 25


  !print *, val
  if (any (val /= [11,12,13,14,15,16,47])) stop 26
end subroutine check


program test
  use vars
  use openacc
  implicit none
  real a
  integer :: k

  call set()
  call check()

  if (.not. acc_is_present (b)) stop 1
  if (.not. acc_is_present (x)) stop 2
  if (.not. acc_is_present (y)) stop 3
  if (.not. acc_is_present (z)) stop 4

  a = 2.0
  k = 42

  !$acc parallel copy (a, k)
    b = a
    a = 1.0
    a = a + b
    x = k
    y = 7*k - 2*x
    z = 3*y
    k = k - z + y
   !$acc end parallel

  if (.not. acc_is_present (b)) stop 5
  if (.not. acc_is_present (x)) stop 6
  if (.not. acc_is_present (y)) stop 7
  if (.not. acc_is_present (z)) stop 8

  if (a /= 3.0) stop 3
  if (k /= -378) stop 3
end program test
