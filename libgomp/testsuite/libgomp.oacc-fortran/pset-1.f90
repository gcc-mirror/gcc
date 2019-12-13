! { dg-do run }

program test
  implicit none
  integer, allocatable :: a1(:)
  integer, allocatable :: b1(:)
  integer, allocatable :: c1(:)
  integer, allocatable :: b2(:,:)
  integer, allocatable :: c3(:,:,:)

  allocate (a1(5))
  if (.not.allocated (a1)) STOP 49

  a1 = 10

  !$acc parallel copy(a1(1:5))
  a1(1) = 1
  a1(2) = 2
  a1(3) = 3
  a1(4) = 4
  a1(5) = 5
  !$acc end parallel

  if (a1(1) .ne. 1) STOP 1
  if (a1(2) .ne. 2) STOP 2
  if (a1(3) .ne. 3) STOP 3
  if (a1(4) .ne. 4) STOP 4
  if (a1(5) .ne. 5) STOP 5

  deallocate(a1)

  allocate (a1(0:4))
  if (.not.allocated (a1)) STOP 50

  a1 = 10

  !$acc parallel copy(a1(0:4))
  a1(0) = 1
  a1(1) = 2
  a1(2) = 3
  a1(3) = 4
  a1(4) = 5
  !$acc end parallel

  if (a1(0) .ne. 1) STOP 6
  if (a1(1) .ne. 2) STOP 7
  if (a1(2) .ne. 3) STOP 8
  if (a1(3) .ne. 4) STOP 9
  if (a1(4) .ne. 5) STOP 10

  deallocate(a1)

  allocate (b2(5,5))
  if (.not.allocated (b2)) STOP 51

  b2 = 11

  !$acc parallel copy(b2(1:5,1:5))
  b2(1,1) = 1
  b2(2,2) = 2
  b2(3,3) = 3
  b2(4,4) = 4
  b2(5,5) = 5
  !$acc end parallel

  if (b2(1,1) .ne. 1) STOP 11
  if (b2(2,2) .ne. 2) STOP 12
  if (b2(3,3) .ne. 3) STOP 13
  if (b2(4,4) .ne. 4) STOP 14
  if (b2(5,5) .ne. 5) STOP 15

  deallocate(b2)

  allocate (b2(0:4,0:4))
  if (.not.allocated (b2)) STOP 52

  b2 = 11

  !$acc parallel copy(b2(0:4,0:4))
  b2(0,0) = 1
  b2(1,1) = 2
  b2(2,2) = 3
  b2(3,3) = 4
  b2(4,4) = 5
  !$acc end parallel

  if (b2(0,0) .ne. 1) STOP 16
  if (b2(1,1) .ne. 2) STOP 17
  if (b2(2,2) .ne. 3) STOP 18
  if (b2(3,3) .ne. 4) STOP 19
  if (b2(4,4) .ne. 5) STOP 20

  deallocate(b2)

  allocate (c3(5,5,5))
  if (.not.allocated (c3)) STOP 53

  c3 = 12

  !$acc parallel copy(c3(1:5,1:5,1:5))
  c3(1,1,1) = 1
  c3(2,2,2) = 2
  c3(3,3,3) = 3
  c3(4,4,4) = 4
  c3(5,5,5) = 5
  !$acc end parallel

  if (c3(1,1,1) .ne. 1) STOP 21
  if (c3(2,2,2) .ne. 2) STOP 22
  if (c3(3,3,3) .ne. 3) STOP 23
  if (c3(4,4,4) .ne. 4) STOP 24
  if (c3(5,5,5) .ne. 5) STOP 25

  deallocate(c3)

  allocate (c3(0:4,0:4,0:4))
  if (.not.allocated (c3)) STOP 54

  c3 = 12

  !$acc parallel copy(c3(0:4,0:4,0:4))
  c3(0,0,0) = 1
  c3(1,1,1) = 2
  c3(2,2,2) = 3
  c3(3,3,3) = 4
  c3(4,4,4) = 5
  !$acc end parallel

  if (c3(0,0,0) .ne. 1) STOP 26
  if (c3(1,1,1) .ne. 2) STOP 27
  if (c3(2,2,2) .ne. 3) STOP 28
  if (c3(3,3,3) .ne. 4) STOP 29
  if (c3(4,4,4) .ne. 5) STOP 30

  deallocate(c3)

  allocate (a1(5))
  if (.not.allocated (a1)) STOP 55

  allocate (b1(5))
  if (.not.allocated (b1)) STOP 56

  allocate (c1(5))
  if (.not.allocated (c1)) STOP 57

  a1 = 10
  b1 = 3
  c1 = 7
   
  !$acc parallel copyin(a1(1:5)) create(c1(1:5)) copyout(b1(1:5))
  c1(1) = a1(1)
  c1(2) = a1(2)
  c1(3) = a1(3)
  c1(4) = a1(4)
  c1(5) = a1(5)

  b1(1) = c1(1)
  b1(2) = c1(2)
  b1(3) = c1(3)
  b1(4) = c1(4)
  b1(5) = c1(5)
  !$acc end parallel

  if (b1(1) .ne. 10) STOP 31
  if (b1(2) .ne. 10) STOP 32
  if (b1(3) .ne. 10) STOP 33
  if (b1(4) .ne. 10) STOP 34
  if (b1(5) .ne. 10) STOP 35

  deallocate(a1)
  deallocate(b1)
  deallocate(c1)

  allocate (a1(0:4))
  if (.not.allocated (a1)) STOP 58

  allocate (b1(0:4))
  if (.not.allocated (b1)) STOP 59

  allocate (c1(0:4))
  if (.not.allocated (c1)) STOP 60

  a1 = 10
  b1 = 3
  c1 = 7
   
  !$acc parallel copyin(a1(0:4)) create(c1(0:4)) copyout(b1(0:4))
  c1(0) = a1(0)
  c1(1) = a1(1)
  c1(2) = a1(2)
  c1(3) = a1(3)
  c1(4) = a1(4)

  b1(0) = c1(0)
  b1(1) = c1(1)
  b1(2) = c1(2)
  b1(3) = c1(3)
  b1(4) = c1(4)
  !$acc end parallel

  if (b1(0) .ne. 10) STOP 36
  if (b1(1) .ne. 10) STOP 37
  if (b1(2) .ne. 10) STOP 38
  if (b1(3) .ne. 10) STOP 39
  if (b1(4) .ne. 10) STOP 40

  deallocate(a1)
  deallocate(b1)
  deallocate(c1)

  allocate (a1(5))
  if (.not.allocated (a1)) STOP 61

  a1 = 10

  !$acc parallel copy(a1(2:3))
  a1(2) = 2
  a1(3) = 3
  !$acc end parallel

  if (a1(1) .ne. 10) STOP 41
  if (a1(2) .ne. 2) STOP 42
  if (a1(3) .ne. 3) STOP 43
  if (a1(4) .ne. 10) STOP 44
  if (a1(5) .ne. 10) STOP 45

  deallocate(a1)

end program test
