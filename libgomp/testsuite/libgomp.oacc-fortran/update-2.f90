! Test ACC UPDATE with derived types.

! { dg-do run }

module dt
  integer, parameter :: n = 10
  type inner
     integer :: d(n)
  end type inner
  type mytype
     integer(8) :: a, b, c(n)
     type(inner) :: in
  end type mytype
end module dt

program derived_acc
  use dt

  implicit none
  integer i, res
  type(mytype) :: var

  var%a = 0
  var%b = 1
  var%c(:) = 10
  var%in%d(:) = 100

  var%c(:) = 10

  !$acc enter data copyin(var)

  !$acc parallel loop present(var)
  do i = 1, 1
     var%a = var%b
  end do
  !$acc end parallel loop

  !$acc update host(var%a)

  if (var%a /= var%b) stop 1

  var%b = 100

  !$acc update device(var%b)

  !$acc parallel loop present(var)
  do i = 1, 1
     var%a = var%b
  end do
  !$acc end parallel loop

  !$acc update host(var%a)

  if (var%a /= var%b) stop 2

  !$acc parallel loop present (var)
  do i = 1, n
     var%c(i) = i
  end do
  !$acc end parallel loop

  !$acc update host(var%c)

  var%a = -1

  do i = 1, n
     if (var%c(i) /= i) stop 3
     var%c(i) = var%a
  end do

  !$acc update device(var%a)
  !$acc update device(var%c)

  res = 0

  !$acc parallel loop present(var) reduction(+:res)
  do i = 1, n
     if (var%c(i) /= var%a) res = res + 1
  end do

  if (res /= 0) stop 4

  var%c(:) = 0

  !$acc update device(var%c)

  !$acc parallel loop present(var)
  do i = 5, 5
     var%c(i) = 1
  end do
  !$acc end parallel loop

  !$acc update host(var%c(5))

  do i = 1, n
     if (i /= 5 .and. var%c(i) /= 0) stop 5
     if (i == 5 .and. var%c(i) /= 1) stop 6
  end do

  !$acc parallel loop present(var)
  do i = 1, n
     var%in%d = var%a
  end do
  !$acc end parallel loop

  !$acc update host(var%in%d)

  do i = 1, n
     if (var%in%d(i) /= var%a) stop 7
  end do

  var%c(:) = 0

  !$acc update device(var%c)

  var%c(:) = -1

  !$acc parallel loop present(var)
  do i = n/2, n
     var%c(i) = i
  end do
  !$acc end parallel loop

  !$acc update host(var%c(n/2:n))

  do i = 1,n
     if (i < n/2 .and. var%c(i) /= -1) stop 8
     if (i >= n/2 .and. var%c(i) /= i) stop 9
  end do

  var%in%d(:) = 0
  !$acc update device(var%in%d)

  !$acc parallel loop present(var)
  do i = 5, 5
     var%in%d(i) = 1
  end do
  !$acc end parallel loop

  !$acc update host(var%in%d(5))

  do i = 1, n
     if (i /= 5 .and. var%in%d(i) /= 0) stop 10
     if (i == 5 .and. var%in%d(i) /= 1) stop 11
  end do

  !$acc exit data delete(var)

  call derived_acc_subroutine(var)
end program derived_acc

subroutine derived_acc_subroutine(var)
  use dt

  implicit none
  integer i, res
  type(mytype) :: var

  var%a = 0
  var%b = 1
  var%c(:) = 10
  var%in%d(:) = 100

  var%c(:) = 10

  !$acc enter data copyin(var)

  !$acc parallel loop present(var)
  do i = 1, 1
     var%a = var%b
  end do
  !$acc end parallel loop

  !$acc update host(var%a)

  if (var%a /= var%b) stop 12

  var%b = 100

  !$acc update device(var%b)

  !$acc parallel loop present(var)
  do i = 1, 1
     var%a = var%b
  end do
  !$acc end parallel loop

  !$acc update host(var%a)

  if (var%a /= var%b) stop 13

  !$acc parallel loop present (var)
  do i = 1, n
     var%c(i) = i
  end do
  !$acc end parallel loop

  !$acc update host(var%c)

  var%a = -1

  do i = 1, n
     if (var%c(i) /= i) stop 14
     var%c(i) = var%a
  end do

  !$acc update device(var%a)
  !$acc update device(var%c)

  res = 0

  !$acc parallel loop present(var) reduction(+:res)
  do i = 1, n
     if (var%c(i) /= var%a) res = res + 1
  end do

  if (res /= 0) stop 15

  var%c(:) = 0

  !$acc update device(var%c)

  !$acc parallel loop present(var)
  do i = 5, 5
     var%c(i) = 1
  end do
  !$acc end parallel loop

  !$acc update host(var%c(5))

  do i = 1, n
     if (i /= 5 .and. var%c(i) /= 0) stop 16
     if (i == 5 .and. var%c(i) /= 1) stop 17
  end do

  !$acc parallel loop present(var)
  do i = 1, n
     var%in%d = var%a
  end do
  !$acc end parallel loop

  !$acc update host(var%in%d)

  do i = 1, n
     if (var%in%d(i) /= var%a) stop 18
  end do

  var%c(:) = 0

  !$acc update device(var%c)

  var%c(:) = -1

  !$acc parallel loop present(var)
  do i = n/2, n
     var%c(i) = i
  end do
  !$acc end parallel loop

  !$acc update host(var%c(n/2:n))

  do i = 1,n
     if (i < n/2 .and. var%c(i) /= -1) stop 19
     if (i >= n/2 .and. var%c(i) /= i) stop 20
  end do

  var%in%d(:) = 0
  !$acc update device(var%in%d)

  !$acc parallel loop present(var)
  do i = 5, 5
     var%in%d(i) = 1
  end do
  !$acc end parallel loop

  !$acc update host(var%in%d(5))

  do i = 1, n
     if (i /= 5 .and. var%in%d(i) /= 0) stop 21
     if (i == 5 .and. var%in%d(i) /= 1) stop 22
  end do

  !$acc exit data delete(var)
end subroutine derived_acc_subroutine
