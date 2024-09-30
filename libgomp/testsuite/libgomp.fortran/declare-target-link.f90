! { dg-additional-options "-Wall" }

! PR fortran/115559
! PR middle-end/115637

module m
   integer :: A
   !$omp declare target link(A)
end module m

subroutine f
  implicit none (type, external)
  integer, save :: x, y  ! { dg-warning "Unused variable 'y' declared" }
  !$omp declare target link(x, y)

  ! note: y is not 'link' as gfortran doesn't regard it as used
  x = 6
  call ii

contains
  subroutine k
    !$omp declare target
     use m
     A = 5
  end
  subroutine ii
    integer :: res
    !$omp target map(x) map(from: res)
      call k()
      call ll()
      res = get()
    !$omp end target
    ! print *, res
    if (res /= 6 + 7 + 5) &
      stop 1
  end
  subroutine ll
    !$omp declare target
    x = x + 7
  end
  integer function get()
     use m
     !$omp declare target
     get = x + A
  end
end


subroutine sub
  implicit none (type, external)
  integer, save :: arr(100), arr2(1:4)
  !$omp declare target link(arr,arr2)

  call mapit
  call device1
  call re_mapit
  call device2
contains
  subroutine mapit
    integer :: i
    arr = [(i, i=1,100)]
    !$omp target enter data map(to:arr(10:50)) map(alloc: arr2(1:4))
  end subroutine
  subroutine re_mapit
    integer :: i
    !$omp target exit data map(from:arr(10:50)) map(delete: arr2)

    if (any (arr(1:9) /= [(i, i=1,9)])) stop 2
    if (any (arr(10:50) /= [(3-10*i, i=10,50)])) stop 3
    if (any (arr(51:100) /= [(i, i=51,100)])) stop 4
  end subroutine

  subroutine device1
    integer :: res
    !$omp target map(from:res)
      res = run_device1()
    !$omp end target
    ! print *, res
    if (res /= -11436) stop 5
  end
  integer function run_device1()
    !$omp declare target
    integer :: i
    run_device1 = -99
    arr2 = [11,22,33,44]
    if (any (arr(10:50) /= [(i, i=10,50)])) then
      run_device1 = arr(11)
      return
    end if
    run_device1 = sum(arr(10:13) + arr2)
    do i = 10, 50
      arr(i) = 3 - 10 * arr(i)
    end do
    run_device1 = run_device1 + sum(arr(15:50))
  end
  subroutine device2
  end
  integer function run_device2()
    !$omp declare target
    run_device2 = -99
  end
end


use m
implicit none (type, external)
external f
external sub

!$omp target enter data map(alloc: A)
call f()
call sub
end
