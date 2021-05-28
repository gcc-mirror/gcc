module m
  implicit none (type, external)
  integer, volatile :: v
contains
subroutine foo (p, i)
  integer :: p(0:*)
  integer :: i
  !$omp task depend (out: p(0))
    v = v + 1
  !$omp end task
  !$omp task depend (in: p(0))
    v = v + 1
  !$omp end task
  !$omp task depend (inout: p(0))
    v = v + 1
  !$omp end task
  !$omp task depend (mutexinoutset: p(0))
    v = v + 1
  !$omp end task
  !$omp task depend (out: p(0)) depend (in: p(1))
    v = v + 1
  !$omp end task
  !$omp task depend (in: p(0)) depend (inout: p(1))
    v = v + 1
  !$omp end task
  !$omp task depend (inout: p(0)) depend (mutexinoutset: p(1))
    v = v + 1
  !$omp end task
  !$omp task depend (mutexinoutset: p(0)) depend (out: p(1))
    v = v + 1
  !$omp end task
  !$omp task depend (iterator (j=0:2) , out : p(j))
    v = v + 1
  !$omp end task
  !$omp task depend (iterator (j=0:2) , in : p(j))
    v = v + 1
  !$omp end task
  !$omp task depend (iterator (j=0:2) , inout : p(j))
    v = v + 1
  !$omp end task
  !$omp task depend (iterator (j=0:2) , mutexinoutset : p(j))
    v = v + 1
  !$omp end task
  !$omp task depend (iterator (j=0:2) , out : p(j)) depend (iterator (j=0:2) , in : p(j + 2))
    v = v + 1
  !$omp end task
  !$omp task depend (iterator (j=0:2) , in : p(j)) depend (iterator (j=0:2) , inout : p(j + 2))
    v = v + 1
  !$omp end task
  !$omp task depend (iterator (j=0:2) , inout : p(j)) depend (iterator (j=0:2) , mutexinoutset : p(j + 2))
    v = v + 1
  !$omp end task
  !$omp task depend (iterator (j=0:2) , mutexinoutset : p(j)) depend (iterator (j=0:2) , out : p(j + 2))
    v = v + 1
  !$omp end task
  !$omp task depend (iterator (j=0:i) , out : p(j))
    v = v + 1
  !$omp end task
  !$omp task depend (iterator (j=0:i) , in : p(j))
    v = v + 1
  !$omp end task
  !$omp task depend (iterator (j=0:i) , inout : p(j))
    v = v + 1
  !$omp end task
  !$omp task depend (iterator (j=0:i) , mutexinoutset : p(j))
    v = v + 1
  !$omp end task
  !$omp task depend (iterator (j=0:i) , out : p(j)) depend (iterator (j=0:i) , in : p(j + 2))
    v = v + 1
  !$omp end task
  !$omp task depend (iterator (j=0:i) , in : p(j)) depend (iterator (j=0:i) , inout : p(j + 2))
    v = v + 1
  !$omp end task
  !$omp task depend (iterator (j=0:i) , inout : p(j)) depend (iterator (j=0:i) , mutexinoutset : p(j + 2))
    v = v + 1
  !$omp end task
  !$omp task depend (iterator (j=0:i) , mutexinoutset : p(j)) depend (iterator (j=0:i) , out : p(j + 2))
    v = v + 1
  !$omp end task
end
end module

program main
  use m
  implicit none (external, type)
  integer p(4)
  call foo (p, 2)
  call foo (p, -1)
end
