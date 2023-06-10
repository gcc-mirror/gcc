! { dg-do run }

program foo
  integer :: count
contains

subroutine s1 ()
  integer :: i, count

  count = 0

  !$omp target parallel do
  !$omp unroll partial
  do i = 1, 100
  end do

end subroutine

end program
