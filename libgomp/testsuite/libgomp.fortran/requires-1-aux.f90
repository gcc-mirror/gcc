! { dg-skip-if "" { *-*-* } }

module m
  integer x
end module m

subroutine foo
  use m
  implicit none
  !$omp requires unified_address

  x = 1
  !$omp target enter data map(always,to: x)
end
