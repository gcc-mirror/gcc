! { dg-do run }

module e_55_1_mod
  integer, parameter :: N = 100000, CHUNKSZ = 10000
  real :: Y(N), Z(N)
end module

subroutine init ()
  use e_55_1_mod, only : Y, Z, N
  integer :: i
  do i = 1, N
    Y(i) = 0.1 * i
    Z(i) = Y(i)
  end do
end subroutine

subroutine check ()
  use e_55_1_mod, only : Y, Z, N
  real :: err
  real, parameter :: EPS = 0.00001
  integer :: i
  do i = 1, N
    if (Y(i) == 0.0) then
      err = Z(i)
    else if (Z(i) == 0.0) then
      err = Y(i)
    else
      err = (Y(i) - Z(i)) / Z(i)
    end if
    if (err > EPS .or. err < -EPS) stop 1
  end do
end subroutine

real function F (z)
  !$omp declare target
  real, intent(in) :: z
  F = -z
end function

subroutine pipedF ()
  use e_55_1_mod, only: Z, N, CHUNKSZ
  integer :: C, i
  real :: F
  do C = 1, N, CHUNKSZ
    !$omp task
      !$omp target map(Z(C:C+CHUNKSZ-1))
        !$omp parallel do
        do i = C, C+CHUNKSZ-1
          Z(i) = F (Z(i))
        end do
      !$omp end target
    !$omp end task
  end do
end subroutine

subroutine pipedF_ref ()
  use e_55_1_mod, only: Y, N
  integer :: i
  real :: F
  do i = 1, N
    Y(i) = F (Y(i))
  end do
end subroutine

program e_55_1
  call init ()
  call pipedF ()
  call pipedF_ref ()
  call check ()
end program
