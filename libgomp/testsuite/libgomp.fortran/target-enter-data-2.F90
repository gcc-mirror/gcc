! { dg-additional-options "-DMEM_SHARED" { target offload_device_shared_as } }
! { dg-do run }
!
! PR middle-end/94635
  implicit none
  integer, parameter :: N = 20
  integer, allocatable, dimension(:) :: my1DPtr
  integer, dimension(N) :: my1DArr
  integer :: i

  allocate(my1DPtr(N))
  my1DPtr = 43

  !$omp target enter data map(alloc: my1DPtr)
    !$omp target
      my1DPtr = [(i , i = 1, N)]
    !$omp end target

    !$omp target map(from: my1DArr) 
      my1DArr = my1DPtr
    !$omp end target
  !$omp target exit data map(delete: my1DPtr)

  if (any (my1DArr /= [(i, i = 1, N)])) stop 1
#if MEM_SHARED
  if (any (my1DArr /= my1DPtr)) stop 2
#else
  if (any (43 /= my1DPtr)) stop 3
#endif

  my1DPtr = [(2*N-i, i = 1, N)]
  my1DArr = 42
 
  !$omp target map(tofrom: my1DArr) map(tofrom: my1DPtr(:))
    my1DArr = my1DPtr
    my1DPtr = 20
  !$omp end target

  if (any (my1DArr /= [(2*N-i, i = 1, N)])) stop 4
  if (any (20 /= my1DPtr)) stop 6
end
