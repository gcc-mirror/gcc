! { dg-do run }
! { dg-require-effective-target offload_device_nonshared_as }

type t
  complex(kind=8) :: c
  integer :: i
end type t

type u
  integer :: i, j
  complex(kind=8) :: c
  integer :: k
end type u

type(t), target :: var(10)
type(u), target :: var2(10)
complex(kind=8), pointer :: ptr(:)
integer :: i

do i=1,10
  var(i)%c = dcmplx(i,0)
  var(i)%i = i
end do

ptr => var(:)%c

!$omp target enter data map(to: var)

!$omp target
var(:)%c = dcmplx(0,0)
var(:)%i = 0
!$omp end target

!$omp target update from(ptr)

do i=1,10
  if (var(i)%c.ne.dcmplx(0,0)) stop 1
  if (var(i)%i.ne.i) stop 2
end do

!$omp target exit data map(delete: var)

! Now do it again with a differently-ordered derived type.

do i=1,10
  var2(i)%c = dcmplx(0,i)
  var2(i)%i = i
  var2(i)%j = i * 2
  var2(i)%k = i * 3
end do

ptr => var2(::2)%c

!$omp target enter data map(to: var2)

!$omp target
var2(:)%c = dcmplx(0,0)
var2(:)%i = 0
var2(:)%j = 0
var2(:)%k = 0
!$omp end target

!$omp target update from(ptr)

do i=1,10
  if (mod(i,2).eq.1) then
    if (var2(i)%c.ne.dcmplx(0,0)) stop 3
  else
    if (var2(i)%c.ne.dcmplx(0,i)) stop 4
  end if
  if (var2(i)%i.ne.i) stop 5
  if (var2(i)%j.ne.i * 2) stop 6
  if (var2(i)%k.ne.i * 3) stop 7
end do

!$omp target exit data map(delete: var2)

end
