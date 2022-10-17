! { dg-shouldfail "error directive" }

module m
  implicit none (external, type)
contains
integer function foo (i, x)
  integer, value :: i, x
  if (x /= 0) then
    !$omp error severity(warning)	! { dg-warning ".OMP ERROR encountered at .1." }
    i = i + 1
  end if
  if (x /= 0) then
    ! ...
  else
    !$omp error severity(warning)	! { dg-warning ".OMP ERROR encountered at .1." }
    i = i + 2
  end if
  select case(0)
    !$omp error severity(warning)	! { dg-warning ".OMP ERROR encountered at .1." }
    case default
      !
  end select
  do while (.false.)
    !$omp error message("42 - 1")	severity (warning)  ! { dg-warning ".OMP ERROR encountered at .1.: 42 - 1" }
    i = i + 4
  end do
99 continue
  !$omp error severity(warning) message("bar") at(compilation)	 ! { dg-warning ".OMP ERROR encountered at .1.: bar" }
    i = i + 8
  foo = i
end function
end module

program main
  use m
  implicit none (external, type)
  character(len=13) :: msg
  character(len=:), allocatable :: msg2, msg3

  ! Initialize offloading early, so that any output this may produce doesn't
  ! disturb the 'dg-output' scanning below.
  !$omp target
  !$omp end target

  msg = "my message"
  if (foo (5, 0) /= 15 .or. foo (7, 1) /= 16) &
    stop 1
  msg2 = "Paris"
  msg3 = "To thine own self be true"
  call bar ("Polonius", "Laertes", msg2, msg3)
  msg2 = "Hello World"
  !$omp error at (execution) severity (warning)
  !$omp error at (execution) severity (warning) message(trim(msg(4:)))
  !$omp error at (execution) severity (warning) message ("Farewell")
  !$omp target
  !$omp error at (execution) severity (warning) message ("ffrom a distanceee"(2:16))
  !$omp end target
  !$omp error at (execution) severity (warning) message (msg2)
  !$omp error at (execution) severity (warning) message (msg(4:6))
  !$omp error at (execution) severity (fatal) message (msg)
  ! unreachable due to 'fatal'---------^
  !$omp error at (execution) severity (warning) message ("foobar")
contains
   subroutine bar(x, y, a, b)
     character(len=*) :: x, y
     character(len=:), allocatable :: a, b
     optional :: y, b
     intent(in) :: x, y, a, b
     !$omp error at (execution) severity (warning) message (x)
     !$omp error at (execution) severity (warning) message (y)
     !$omp error at (execution) severity (warning) message (a)
     !$omp error at (execution) severity (warning) message (b)
   end subroutine
end

! { dg-output "(\n|\r|\r\n)" }
! { dg-output "libgomp: error directive encountered: Polonius(\n|\r|\r\n)(\n|\r|\r\n)" }
! { dg-output "libgomp: error directive encountered: Laertes(\n|\r|\r\n)(\n|\r|\r\n)" }
! { dg-output "libgomp: error directive encountered: Paris(\n|\r|\r\n)(\n|\r|\r\n)" }
! { dg-output "libgomp: error directive encountered: To thine own self be true(\n|\r|\r\n)(\n|\r|\r\n)" }
! { dg-output "libgomp: error directive encountered(\n|\r|\r\n)(\n|\r|\r\n)" }
! { dg-output "libgomp: error directive encountered: message(\n|\r|\r\n)(\n|\r|\r\n)" }
! { dg-output "libgomp: error directive encountered: Farewell(\n|\r|\r\n)(\n|\r|\r\n)" }
! { dg-output "libgomp: error directive encountered: from a distance(\n|\r|\r\n)(\n|\r|\r\n)" }
! { dg-output "libgomp: error directive encountered: Hello World(\n|\r|\r\n)(\n|\r|\r\n)" }
! { dg-output "libgomp: error directive encountered: mes(\n|\r|\r\n)(\n|\r|\r\n)" }
! { dg-output "libgomp: fatal error: error directive encountered: my message   (\n|\r|\r\n)" }
