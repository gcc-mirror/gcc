! { dg-do run }

program foo
  integer :: count
  call s1
contains

  subroutine s1 ()
    integer :: i, count

    count = 0

    !$omp target parallel do map(tofrom:count) reduction(+:count) private(i)
    !$omp unroll partial
    do i = 1, 100
      count = count + 1
    end do

    if (count .ne. 100) stop 1

  end subroutine

end program
