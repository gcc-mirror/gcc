! Test the collapse clause inside a kernels region.

! { dg-do run }

program collapse3
  integer :: a(3,3,3), k, kk, kkk, l, ll, lll
  !$acc kernels
  !$acc loop collapse(3)
    do 115 k=1,3
dokk: do kk=1,3
        do kkk=1,3
          a(k,kk,kkk) = 1
        enddo
      enddo dokk
115   continue
  !$acc end kernels
  if (any(a(1:3,1:3,1:3).ne.1)) stop 1

  !$acc kernels
  !$acc loop collapse(3)
dol: do 120 l=1,3
doll: do ll=1,3
        do lll=1,3
          a(l,ll,lll) = 2
        enddo
      enddo doll
120 end do dol
  !$acc end kernels
  if (any(a(1:3,1:3,1:3).ne.2)) stop 2
end program collapse3
