! { dg-additional-options "-fopenmp-allocators" }
module m
contains
subroutine s(a,b,c,d)
integer, allocatable :: A, B
integer, allocatable :: C(:), D(:)

!$omp allocators allocate(A,B)
allocate(A,B)
call move_alloc(A,B)

!$omp allocators allocate(C,D)
allocate(C(5),D(5))
call move_alloc(C,D)
end

subroutine q()
integer, allocatable :: A, B
integer, allocatable :: C(:), D(:)

call s(a,b,c,d)
end
end

use m
call q
end
