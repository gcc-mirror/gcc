! PR fortran/104949

implicit none (type,external)
integer, allocatable :: A(:)
A = [1,2,3,4,5,6]

!$omp parallel firstprivate(A)
!$omp master
  if (any (A /= [1,2,3,4,5])) error stop
  A(:) = [99,88,77,66,55]
!$omp end master
!$omp end parallel

!$omp target firstprivate(A)
  if (any (A /= [1,2,3,4,5])) error stop
  A(:) = [99,88,77,66,55]
!$omp end target
if (any (A /= [1,2,3,4,5])) error stop

!$omp parallel default(firstprivate)
!$omp master
  if (any (A /= [1,2,3,4,5])) error stop
  A(:) = [99,88,77,66,55]
!$omp end master
!$omp end parallel
if (any (A /= [1,2,3,4,5])) error stop

!$omp target defaultmap(firstprivate)
  if (any (A /= [1,2,3,4,5])) error stop
  A(:) = [99,88,77,66,55]
!$omp end target
if (any (A /= [1,2,3,4,5])) error stop
end
