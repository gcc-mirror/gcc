subroutine one
  implicit none (external, type)
  integer :: i, j
  i = 5
  j = 6
  !$omp parallel
  my_block : block
    !$omp atomic write
    i = 7
    exit my_block

    !$omp atomic write
    j = 99  ! Should be unreachable

    ! exit should jump here - end of block but inside of it.
  end block my_block
  if (i /= 7) stop 1
  if (j /= 6) stop 2
end

 call one
end
