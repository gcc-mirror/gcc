! { dg-do run }

program collapse2
  call test1
  call test2
contains
  subroutine test1
    integer :: i, j, k, a(1:3, 4:6, 5:7)
    logical :: l
    l = .false.
    a(:, :, :) = 0
    !$omp parallel do collapse(4 - 1) schedule(static, 4)
      do 164 i = 1, 3
        do 164 j = 4, 6
          do 164 k = 5, 7
            a(i, j, k) = i + j + k
164      end do
    !$omp parallel do collapse(2) reduction(.or.:l)
firstdo: do i = 1, 3
        do j = 4, 6
          do k = 5, 7
            if (a(i, j, k) .ne. (i + j + k)) l = .true.
          end do
        end do
      end do firstdo
    !$omp end parallel do
    if (l) call abort
  end subroutine test1

  subroutine test2
    integer :: a(3,3,3), k, kk, kkk, l, ll, lll
    !$omp do collapse(3)
      do 115 k=1,3
  dokk: do kk=1,3
          do kkk=1,3
            a(k,kk,kkk) = 1
          enddo
        enddo dokk
115   continue
    if (any(a(1:3,1:3,1:3).ne.1)) call abort

    !$omp do collapse(3)
 dol: do 120 l=1,3
  doll: do ll=1,3
          do lll=1,3
            a(l,ll,lll) = 2
          enddo
        enddo doll
120   end do dol
    if (any(a(1:3,1:3,1:3).ne.2)) call abort
  end subroutine test2

end program collapse2
