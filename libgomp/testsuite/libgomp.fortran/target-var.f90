! { dg-additional-options "-O3" }
!
! With -O3 the static local variable A.10 generated for
! the array constructor [-2, -4, ..., -20] is optimized
! away - which has to be handled in the offload_vars table.
!
program main
  implicit none (type, external)
  integer :: j
  integer, allocatable :: A(:)

  A = [(3*j, j=1, 10)]
  call bar (A)
  deallocate (A)
contains
  subroutine bar (array)
    integer :: i
    integer :: array(:)

    !$omp target map(from:array)
    !$acc parallel copyout(array)
    array = [(-2*i, i = 1, size(array))]
    !$omp do private(array)
    !$acc loop gang private(array)
    do i = 1, 10
      array(i) = 9*i
    end do
    if (any (array /= [(-2*i, i = 1, 10)])) error stop 2
    !$omp end target
    !$acc end parallel
  end subroutine bar
end
