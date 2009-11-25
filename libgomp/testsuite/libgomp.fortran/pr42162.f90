! PR fortran/42162
! { dg-do run }

subroutine sub1(k, a)
  implicit none
  integer :: k, a(3)
  !$omp do
    do k=1,3
      a(k) = a(k) + 1
    enddo
  !$omp end do
end subroutine sub1

subroutine sub2(k, a)
  implicit none
  integer :: k, a(3)
  !$omp do private (k)
    do k=1,3
      a(k) = a(k) + 1
    enddo
  !$omp end do
end subroutine sub2

subroutine sub3(k, a)
  implicit none
  integer :: k, a(3)
  !$omp do lastprivate (k)
    do k=1,3
      a(k) = a(k) + 1
    enddo
  !$omp end do
end subroutine sub3

program pr42162
  implicit none
  integer :: k, a(3), b(3), c(3)
  a = 1
  b = 2
  c = 3
  k = 3
  !$omp parallel num_threads(3)
  call sub1 (k, a)
  !$omp end parallel
  k = 4
  !$omp parallel num_threads(3)
  call sub2 (k, b)
  !$omp end parallel
  k = 10
  !$omp parallel num_threads(3)
  call sub3 (k, c)
  !$omp end parallel
  if (k.ne.4.or.any(a.ne.2).or.any(b.ne.3).or.any(c.ne.4)) call abort
end
