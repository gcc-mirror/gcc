! Subarrays on data construct: explicit-shape array.

program test
  integer, parameter :: n = 100
  integer i, data(n)

  data(:) = 0

  !$acc data copy(data(5:n-10))
  !$acc parallel loop
  do i = 10, n - 10
     data(i) = i
  end do
  !$acc end parallel loop
  !$acc end data

  do i = 1, n
     if ((i < 10 .or. i > n-10)) then
        if ((data(i) .ne. 0)) call abort
     else if (data(i) .ne. i) then
        call abort
     end if
  end do
end program test
