! { dg-do run }
  interface
    integer function foo ()
    end function
    integer function bar ()
    end function
    integer function baz ()
    end function
  end interface
  procedure(foo), pointer :: ptr
  integer :: i
  ptr => foo
!$omp parallel shared (ptr)
  if (ptr () /= 1) call abort
!$omp end parallel
  ptr => bar
!$omp parallel firstprivate (ptr)
  if (ptr () /= 2) call abort
!$omp end parallel
!$omp parallel sections lastprivate (ptr)
!$omp section
  ptr => foo
  if (ptr () /= 1) call abort
!$omp section
  ptr => bar
  if (ptr () /= 2) call abort
!$omp section
  ptr => baz
  if (ptr () /= 3) call abort
!$omp end parallel sections
  if (ptr () /= 3) call abort
  if (.not.associated (ptr, baz)) call abort
end
integer function foo ()
  foo = 1
end function
integer function bar ()
  bar = 2
end function
integer function baz ()
  baz = 3
end function
