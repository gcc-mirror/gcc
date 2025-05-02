! { dg-do run }
! { dg-additional-options "-cpp" }

#define ARRAY_BODY(ARRAY, MIN, MAX)	\
  do i = 1, 10;				\
     do j = MIN, MAX;			\
        ARRAY(j) = ARRAY(j) + 1;	\
     end do;				\
  end do

program main
  implicit none
  integer :: i, j, max = 6, two = 2, three = 3, four = 4, five = 5, six = 6
  integer :: a(6) = (/ 5, 1, 1, 5, 9, 9 /)
  integer :: o(6)
  o = a

  !$acc parallel
  !$acc loop reduction(+:a(2:3))
  ARRAY_BODY (a, 2, 3)
  !$acc end parallel
  ARRAY_BODY (o, 2, 3)
  do i = 1, max
     if (a(i) .ne. o(i)) STOP 1
  end do

  !$acc parallel copy(a(4:6))
  !$acc loop reduction(+:a(4:6))
  ARRAY_BODY (a, 4, 6)
  !$acc end parallel
  ARRAY_BODY (o, 4, 6)
  do i = 1, max
     if (a(i) .ne. o(i)) STOP 2
  end do

  !$acc parallel copy(a)
  !$acc loop reduction(+:a(1:6))
  ARRAY_BODY (a, 1, 6)
  !$acc end parallel
  ARRAY_BODY (o, 1, 6)
  do i = 1, max
     if (a(i) .ne. o(i)) STOP 3
  end do

  !$acc parallel
  !$acc loop reduction(+:a)
  ARRAY_BODY (a, 4, 4)
  !$acc end parallel
  ARRAY_BODY (o, 4, 4)
  do i = 1, max
     if (a(i) .ne. o(i)) STOP 4
  end do

  !$acc parallel copy(a)
  !$acc loop reduction(+:a)
  ARRAY_BODY (a, 4, 6)
  !$acc end parallel
  ARRAY_BODY (o, 4, 6)
  do i = 1, max
     if (a(i) .ne. o(i)) STOP 5
  end do

#if !defined(ACC_DEVICE_TYPE_host)

  !$acc parallel loop reduction(+:a)
  ARRAY_BODY (a, 2, 4)
  !$acc end parallel loop
  ARRAY_BODY (o, 2, 4)
  do i = 1, max
     if (a(i) .ne. o(i)) STOP 6
  end do

  !$acc parallel loop reduction(+:a(2:4))
  ARRAY_BODY (a, 2, 4)
  !$acc end parallel loop
  ARRAY_BODY (o, 2, 4)
  do i = 1, max
     if (a(i) .ne. o(i)) STOP 7
  end do

  !$acc parallel reduction(+:a)
  ARRAY_BODY (a, 3, 4)
  !$acc end parallel
  ARRAY_BODY (o, 3, 4)
  do i = 1, max
     if (a(i) .ne. o(i)) STOP 8
  end do

  !$acc parallel reduction(+:a(2:3))
  ARRAY_BODY (a, 2, 3)
  !$acc end parallel
  ARRAY_BODY (o, 2, 3)
  do i = 1, max
     if (a(i) .ne. o(i)) STOP 9
  end do
#endif

end program main
