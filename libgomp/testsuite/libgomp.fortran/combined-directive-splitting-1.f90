module m
  integer :: a = 1
  !$omp declare target enter(a)
end module m

module m2
contains
subroutine bar()
  use m
  implicit none
  !$omp declare target
  a = a + 5
end subroutine bar
end module m2

program p
  use m
  use m2
  implicit none
  integer :: b, i

  !$omp target parallel do map(always, tofrom: a) firstprivate(a)
    do i = 1, 1
      a = 7
      call bar()
      if (a /= 7) error stop 1
      a = a + 8
    end do
  if (a /= 6) error stop 2

  b = 3
  !$omp target parallel do map(always, tofrom: a) firstprivate(b)
    do i = 1, 1
      a = 3
      call bar ()
      if (a /= 8) error stop 3
      a = a + b
    end do
  if (a /= 11) error stop 4
end program p

