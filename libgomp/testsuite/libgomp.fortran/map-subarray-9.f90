! { dg-do run }

! PR fortran/120505

! This test case checks that explicit mapping of allocatable DT components
! followed by implicit deep mapping works.

module m
type t
 integer, allocatable :: den1(:,:), den2(:,:)
end type t

type t2
 type(t), allocatable :: tiles(:)
end type t2

type(t2) :: var
end

use m

allocate(var%tiles(1))
var%tiles(1)%den1 = reshape([1,2,3,4],[2,2])
var%tiles(1)%den2 = reshape([11,22,33,44],[2,2])

!$omp target enter data map(var%tiles(1)%den1, var%tiles(1)%den2)

!$omp target
 if (any (var%tiles(1)%den1 /= reshape([1,2,3,4],[2,2]))) stop 1
 if (any (var%tiles(1)%den2 /= reshape([11,22,33,44],[2,2]))) stop 1
 var%tiles(1)%den1 = var%tiles(1)%den1 + 5
 var%tiles(1)%den2 = var%tiles(1)%den2 + 7
!$omp end target

!$omp target exit data map(var%tiles(1)%den1, var%tiles(1)%den2)

if (any (var%tiles(1)%den1 /= 5 + reshape([1,2,3,4],[2,2]))) stop 1
if (any (var%tiles(1)%den2 /= 7 + reshape([11,22,33,44],[2,2]))) stop 1

end
