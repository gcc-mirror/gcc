! { dg-do run }
! PR middle-end/90779

program pr90779
  implicit none
  integer :: v(4), i

  !$omp target map(from:v)
    v(:) = (/ (i, i=1,4) /)
  !$omp end target

  if (any (v .ne. (/ (i, i=1,4) /))) stop 1
end program
