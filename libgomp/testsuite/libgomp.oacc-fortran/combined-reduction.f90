! Test a combined acc parallel loop reduction.

! { dg-do run }

program test
  implicit none
  integer i, n, var

  n = 100
  var = 0

  !$acc parallel loop reduction(+:var)
  do i = 1, 100
     var = var + 1
  end do
  !$acc end parallel loop

  if (var .ne. n) call abort
end program test
