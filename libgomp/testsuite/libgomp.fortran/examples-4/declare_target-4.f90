! { dg-do run }

module e_53_4_mod
  !$omp declare target (N, Q)
  integer, parameter :: N = 10
  real :: Q(N,N)
contains
  real function Pfun (i, k)
    !$omp declare target
    integer, intent(in) :: i, k
    Pfun = (Q(i,k) * Q(k,i))
  end function
end module

real function accum (k) result (tmp)
  use e_53_4_mod
  integer :: i, k
  tmp = 0.0e0
  !$omp target map(tmp)
    !$omp parallel do reduction(+:tmp)
    do i = 1, N
      tmp = tmp + Pfun (k, i)
    end do
  !$omp end target
end function

real function accum_ref (k) result (tmp)
  use e_53_4_mod
  integer :: i, k
  tmp = 0.0e0
  do i = 1, N
    tmp = tmp + Pfun (k, i)
  end do
end function

subroutine init ()
  use e_53_4_mod
  integer :: i, j
  do i = 1, N
    do j = 1, N
      Q(i,j) = 0.001 * i * j
    end do
  end do
end subroutine

subroutine check (a, b)
  real :: a, b, err
  real, parameter :: EPS = 0.00001
  if (b == 0.0) then
    err = a
  else if (a == 0.0) then
    err = b
  else
    err = (a - b) / b
  end if
  if (err > EPS .or. err < -EPS) stop 1
end subroutine

program e_53_4
  use e_53_4_mod
  integer :: i
  real :: accum, accum_ref
  call init ()
  !$omp target update to(Q)
  do i = 1, N
    call check (accum (i), accum_ref (i))
  end do
end program
