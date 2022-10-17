! With bind(C), the C (CFI) array descriptor is converted to
! a Fortran array descriptor - thus, internally a PARM_DECL is
! converted to a VAR_DECL - check that the optional check still works

module m
contains
subroutine foo(x, y)  bind(C)
  integer, optional :: x,y(:)
  !$omp target map(tofrom:x)
     if (present (x)) x = 5
     if (present (y)) y(1) = 5
  !$omp end target
end
end

use m
call foo()
end
