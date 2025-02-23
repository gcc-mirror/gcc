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
  integer :: i, j, max = 6, one = 1, two = 2, three = 3, four = 4, five = 5, six = 6
  integer :: a(6) = (/ 5, 1, 1, 5, 9, 9 /)
  integer :: o(6)
  o = a

  !$acc parallel
  !$acc loop reduction(+:a(two:three))
  ARRAY_BODY (a, two, three)
  !$acc end parallel

  ARRAY_BODY (o, two, three)
  do i = 1, max
     if (a(i) .ne. o(i)) STOP 1
  end do

  !$acc parallel copy(a(four:six))
  !$acc loop reduction(+:a(four:six))
  ARRAY_BODY (a, four, six)
  !$acc end parallel
  ARRAY_BODY (o, four, six)
  do i = 1, max
     if (a(i) .ne. o(i)) STOP 2
  end do

  !$acc parallel copy(a)
  !$acc loop reduction(+:a(one:six))
  ARRAY_BODY (a, one, six)
  !$acc end parallel
  ARRAY_BODY (o, one, six)
  do i = 1, max
     if (a(i) .ne. o(i)) STOP 3
  end do

  !$acc parallel
  !$acc loop reduction(+:a)
  ARRAY_BODY (a, four, four)
  !$acc end parallel
  ARRAY_BODY (o, four, four)
  do i = 1, max
     if (a(i) .ne. o(i)) STOP 4
  end do

  !$acc parallel copy(a)
  !$acc loop reduction(+:a)
  ARRAY_BODY (a, four, six)
  !$acc end parallel
  ARRAY_BODY (o, four, six)
  do i = 1, max
     if (a(i) .ne. o(i)) STOP 5
  end do

#if !defined(ACC_DEVICE_TYPE_host)

  !$acc parallel loop reduction(+:a)
  ARRAY_BODY (a, two, four)
  !$acc end parallel loop
  ARRAY_BODY (o, two, four)
  do i = 1, max
     if (a(i) .ne. o(i)) STOP 6
  end do

  !$acc parallel loop reduction(+:a(two:four))
  ARRAY_BODY (a, two, four)
  !$acc end parallel loop
  ARRAY_BODY (o, two, four)
  do i = 1, max
     if (a(i) .ne. o(i)) STOP 7
  end do

  !$acc parallel reduction(+:a)
  ARRAY_BODY (a, three, four)
  !$acc end parallel
  ARRAY_BODY (o, three, four)
  do i = 1, max
     if (a(i) .ne. o(i)) STOP 8
  end do

  !$acc parallel reduction(+:a(two:three))
  ARRAY_BODY (a, two, three)
  !$acc end parallel
  ARRAY_BODY (o, two, three)
  do i = 1, max
     if (a(i) .ne. o(i)) STOP 9
  end do
#endif

end program main
