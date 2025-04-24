! { dg-do run }
! { dg-require-effective-target offload_device_nonshared_as }

program p
implicit none
real(kind=4) :: arr(10,10,10,10)

call s(arr,9,9,9,9)

contains

subroutine s(arr,m,n,o,p)
implicit none
integer :: i,m,n,o,p
integer :: a,b,c,d
real(kind=4) :: arr(0:m,0:n,0:o,0:p)

arr = 0

!$omp target enter data map(to: arr)

!$omp target
do i=0,9
  arr(i,i,i,i) = i
end do
!$omp end target

!$omp target update from(arr(0:2,0:2,0:2,0:2))

do a=0,9
  do b=0,9
    do c=0,9
      do d=0,9
        if (a.le.2.and.b.le.2.and.c.le.2.and.d.le.2) then
          if (a.eq.b.and.b.eq.c.and.c.eq.d) then
            if (arr(a,b,c,d).ne.a) stop 1
          else
            if (arr(a,b,c,d).ne.0) stop 2
          end if
        else
          if (arr(a,b,c,d).ne.0) stop 3
        end if
      end do
    end do
  end do
end do

!$omp target exit data map(delete: arr)

end subroutine s
end program p
