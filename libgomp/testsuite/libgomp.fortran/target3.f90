! { dg-do run }

module target3
contains
  subroutine foo (f, g)
    integer :: n
    integer, pointer :: f, g(:)
    integer, pointer :: j, k(:)
    logical :: r
    nullify (j)
    k => null ()
    !$omp target map (tofrom: f, g, j, k) map (from: r)
      r = associated (f) .or. associated (g)
      r = r .or. associated (j) .or. associated (k)
    !$omp end target
    if (r) call abort
    !$omp target
      r = associated (f) .or. associated (g)
      r = r .or. associated (j) .or. associated (k)
    !$omp end target
    if (r) call abort
  end subroutine foo
end module target3
  use target3, only : foo
  integer, pointer :: f, g(:)
  f => null ()
  nullify (g)
  call foo (f, g)
end
