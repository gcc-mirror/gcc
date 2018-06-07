! { dg-do run }

program associate2
  type dl
    integer :: i
  end type
  type dt
    integer :: i
    real :: a(3, 3)
    type(dl) :: c(3, 3)
  end type
  integer :: v(4), i, j, k, l
  type (dt) :: a(3, 3)
  v = 15
  forall (k = 1:3, l = 1:3) a(k, l)%a(:,:) = 4.5
  a(2,1)%a(1,2) = 3.5
  i = 2
  j = 1
  associate(u => v, b => a(i, j)%a)
!$omp parallel private(v, a) default(none)
  v = -1
  forall (k = 1:3, l = 1:3) a(k, l)%a(:,:) = 2.5
  if (v(3) /= -1 .or. u(3) /= 15) STOP 1
  if (a(2,1)%a(1,2) /= 2.5 .or. b(1,2) /= 3.5) STOP 2
  associate(u => v, b => a(2, 1)%a)
  if (u(3) /= -1 .or. b(1,2) /= 2.5) STOP 3
  end associate
  if (u(3) /= 15 .or. b(1,2) /= 3.5) STOP 4
!$omp end parallel
  end associate
  forall (k = 1:3, l = 1:3) a(k, l)%c(:,:)%i = 7
  a(1,2)%c(2,1)%i = 9
  i = 1
  j = 2
  associate(d => a(i, j)%c(2,:)%i)
!$omp parallel private(a) default(none)
  forall (k = 1:3, l = 1:3) a(k, l)%c(:,:)%i = 15
  if (a(1,2)%c(2,1)%i /= 15 .or. d(1) /= 9) STOP 5
  if (a(1,2)%c(2,2)%i /= 15 .or. d(2) /= 7) STOP 6
  associate(d => a(2,1)%c(2,:)%i)
  if (d(1) /= 15 .or. d(2) /= 15) STOP 7
  end associate
  if (d(1) /= 9 .or. d(2) /= 7) STOP 8
!$omp end parallel
  end associate
end program
