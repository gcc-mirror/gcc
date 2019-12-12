! { dg-do run }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

module work

integer :: P(1000)
real    :: A(1000)

contains
function do_work(arr) result(pri)
  implicit none
  real, dimension(*) :: arr

  real :: pri
  integer :: i, j

  !$omp simd private(j) lastprivate(pri)
  do i = 1, 999
    j = P(i)

    pri = 0.5
    if (mod(j-1, 2) == 0) then
      pri = A(j+1) + arr(i)
    endif
    A(j) = pri * 1.5
    pri = pri + A(j)
  end do

end function do_work

end module work

program simd_8f
  use work
  implicit none
  real :: pri, arr(1000), diff
  integer :: i
  real, parameter :: EPS = 0.005

  do i = 1, 1000
     P(i)   = i
     A(i)   = (i-1) * 1.5
     arr(i) = (i-1) * 1.8
  end do
  pri = do_work(arr)

  diff = pri - 8237.25

  if (diff > EPS .or. -diff > EPS) stop 1

end program
