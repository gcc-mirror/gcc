! { dg-do run }

program main
  implicit none
  integer, allocatable, dimension(:) :: AA, BB, CC, DD
  integer :: i, N = 20

  allocate(BB(N))
  AA = [(i, i=1,N)]

  !$omp target enter data map(alloc: BB)
  !$omp target enter data map(to: AA)

  !$omp target
    BB = 3 * AA
  !$omp end target

  !$omp target exit data map(delete: AA)
  !$omp target exit data map(from: BB)

  if (any (BB /= [(3*i, i=1,N)])) stop 1
  if (any (AA /= [(i, i=1,N)])) stop 2


  CC = 31 * BB
  DD = [(-i, i=1,N)]

  !$omp target enter data map(to: CC) map(alloc: DD)

  !$omp target
    DD = 5 * CC
  !$omp end target

  !$omp target exit data map(delete: CC) map(from: DD)

  if (any (CC /= [(31*3*i, i=1,N)])) stop 3
  if (any (DD /= [(31*3*5*i, i=1,N)])) stop 4
end
