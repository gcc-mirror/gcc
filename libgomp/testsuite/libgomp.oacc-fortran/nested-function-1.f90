! Exercise nested function decomposition, gcc/tree-nested.c.

! { dg-do run }
! { dg-options "-std=legacy" }

program collapse2
  call test1
  call test2
contains
  subroutine test1
    integer :: i, j, k, a(1:3, 4:6, 5:7)
    logical :: l
    l = .false.
    a(:, :, :) = 0
    !$acc parallel reduction (.or.:l)
    !$acc loop worker vector collapse(4 - 1)
      do 164 i = 1, 3
        do 164 j = 4, 6
          do 164 k = 5, 7
            a(i, j, k) = i + j + k
164      end do
    !$acc loop worker vector reduction(.or.:l) collapse(2)
firstdo: do i = 1, 3
        do j = 4, 6
          do k = 5, 7
            if (a(i, j, k) .ne. (i + j + k)) l = .true.
          end do
        end do
      end do firstdo
    !$acc end parallel
    if (l) STOP 1
  end subroutine test1

  subroutine test2
    integer :: a(3,3,3), k, kk, kkk, l, ll, lll
    a = 0
    !$acc parallel num_workers(8)
    ! Use "gang(static:1)" here and below to effectively turn gang-redundant
    ! execution mode into something like gang-single.
    !$acc loop gang(static:1) collapse(1)
      do 115 k=1,3
         !$acc loop collapse(2)
  dokk: do kk=1,3
          do kkk=1,3
            a(k,kk,kkk) = 1
          enddo
        enddo dokk
115   continue
    !$acc loop gang(static:1) collapse(1)
      do k=1,3
         if (any(a(k,1:3,1:3).ne.1)) STOP 2
      enddo
    ! Use "gang(static:1)" here and below to effectively turn gang-redundant
    ! execution mode into something like gang-single.
    !$acc loop gang(static:1) collapse(1)
 dol: do 120 l=1,3
    !$acc loop collapse(2)
  doll: do ll=1,3
          do lll=1,3
            a(l,ll,lll) = 2
          enddo
        enddo doll
120   end do dol
    !$acc loop gang(static:1) collapse(1)
     do l=1,3
        if (any(a(l,1:3,1:3).ne.2)) STOP 3
     enddo
    !$acc end parallel
  end subroutine test2

end program collapse2
