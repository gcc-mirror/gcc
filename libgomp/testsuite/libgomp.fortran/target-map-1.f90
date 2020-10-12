! PR fortran/67311

implicit none
  TYPE myType
    integer :: A
    TYPE(myType), DIMENSION(:), POINTER :: x
    TYPE(myType), DIMENSION(:), contiguous, POINTER :: y
    integer :: B
  END TYPE myType
  call openmp_sub
contains
  subroutine openmp_sub
    type(myType) :: argument

    !$OMP PARALLEL DEFAULT(NONE) PRIVATE(argument)
      argument%a = 5
      argument%b = 7
      call foo(argument)
      if (.not.associated(argument%x) .or. size(argument%x) /= 2) stop 2
      if (argument%a /= 8 .or. argument%b /= 9 &
          .or. any(argument%x(:)%a /= [2, 3]) &
          .or. any(argument%x(:)%b /= [9, 1])) stop 3
      if (.not.associated(argument%y) .or. size(argument%y) /= 3) stop 4
      if (any(argument%y(:)%a /= [11, 22, 33]) &
          .or. any(argument%y(:)%b /= [44, 55, 66])) stop 5
      deallocate (argument%x, argument%y)
    !$OMP END PARALLEL
  end subroutine openmp_sub
  subroutine foo(x)
    type(myType), intent(inout) :: x
    !$omp declare target
    if (x%a /= 5 .or. x%b /= 7) stop 1
    x%a = 8; x%b = 9
    allocate (x%x(2))
    x%x(:)%a = [2, 3]
    x%x(:)%b = [9, 1]
    allocate (x%y(3))
    x%y(:)%a = [11, 22, 33]
    x%y(:)%b = [44, 55, 66]
  end subroutine
end
