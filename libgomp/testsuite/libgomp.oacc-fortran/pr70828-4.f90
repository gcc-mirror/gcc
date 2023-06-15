! Subarrays declared on data construct: assumed-size array.

subroutine s1(n, arr)
  integer :: n
  integer :: arr(*)

  !$acc data copy(arr(5:n-10))
  !$acc parallel loop
  do i = 10, n - 10
     arr(i) = i
  end do
  !$acc end parallel loop
  !$acc end data
end subroutine s1

program test
  integer, parameter :: n = 100
  integer i, data(n)

  data(:) = 0

  call s1(n, data)

  do i = 1, n
     if ((i < 10 .or. i > n-10)) then
        if ((data(i) .ne. 0)) call abort
     else if (data(i) .ne. i) then
        call abort
     end if
  end do
end program test

! { dg-xfail-run-if "PR70828" { ! openacc_host_selected } }
