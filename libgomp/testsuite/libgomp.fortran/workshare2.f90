! { dg-do run }
subroutine f1
  integer a(20:50,70:90)
!$omp parallel workshare
  a(:,:) = 17
!$omp end parallel workshare
  if (any (a.ne.17)) stop 1
end subroutine f1
subroutine f2
  integer a(20:50,70:90),d(15),e(15),f(15)
  integer b, c, i
!$omp parallel workshare
  c = 5
  a(:,:) = 17
  b = 4
  d = (/ 0, 1, 2, 3, 4, 0, 6, 7, 8, 9, 10, 0, 0, 13, 14 /)
  forall (i=1:15, d(i) /= 0)
     d(i) = 0
  end forall
  e = (/ 4, 5, 2, 6, 4, 5, 2, 6, 4, 5, 2, 6, 4, 5, 2 /)
  f = 7
  where (e.ge.5) f = f + 1
!$omp end parallel workshare
  if (any (a.ne.17)) stop 2
  if (c.ne.5.or.b.ne.4) stop 3
  if (any(d.ne.0)) stop 4
  do i = 1, 15
    if (e(i).ge.5) then
      if (f(i).ne.8) stop 5
    else
      if (f(i).ne.7) stop 6
    end if
  end do
end subroutine f2

  call f1
  call f2
end
