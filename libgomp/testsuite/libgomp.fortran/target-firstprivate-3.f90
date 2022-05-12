implicit none
  integer, allocatable :: x(:)
  x = [1,2,3,4]
  call foo(x)
  if (any (x /= [1,2,3,4])) error stop
  call foo()
contains
subroutine foo(c)
  integer, allocatable, optional :: c(:)
  logical :: is_present
  is_present = present (c)
  !$omp target firstprivate(c)
    if (is_present) then
      if (.not. allocated(c)) error stop
      if (any (c /= [1,2,3,4])) error stop
      c = [99,88,77,66]
      if (any (c /= [99,88,77,66])) error stop
    end if
  !$omp end target
  if (is_present) then
    if (any (c /= [1,2,3,4])) error stop
  end if
end
end
