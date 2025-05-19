!
! Check that mapping with map(var%tiles(1)) works.
!
! This uses deep mapping to handle the allocatable
! derived-type components
!
! The tricky part is that GCC generates intermittently
! an SSA_NAME that needs to be resolved.
!
module m
type t
 integer, allocatable :: den1(:,:), den2(:,:)
end type t

type t2
 type(t), allocatable :: tiles(:)
end type t2
end

use m
use iso_c_binding
implicit none (type, external)
type(t2), target :: var
logical :: is_self_map
type(C_ptr) :: pden1, pden2, ptiles, ptiles1

allocate(var%tiles(1))
var%tiles(1)%den1 = reshape([1,2,3,4],[2,2])
var%tiles(1)%den2 = reshape([11,22,33,44],[2,2])

ptiles = c_loc(var%tiles)
ptiles1 = c_loc(var%tiles(1))
pden1 = c_loc(var%tiles(1)%den1)
pden2 = c_loc(var%tiles(1)%den2)


is_self_map = .false.
!$omp target map(to: is_self_map)
  is_self_map = .true.
!$omp end target

!$omp target enter data map(var%tiles(1))

!$omp target firstprivate(ptiles, ptiles1, pden1, pden2)
 if (any (var%tiles(1)%den1 /= reshape([1,2,3,4],[2,2]))) stop 1
 if (any (var%tiles(1)%den2 /= reshape([11,22,33,44],[2,2]))) stop 2
 var%tiles(1)%den1 = var%tiles(1)%den1 + 5
 var%tiles(1)%den2 = var%tiles(1)%den2 + 7

 if (is_self_map) then
   if (.not. c_associated (ptiles, c_loc(var%tiles))) stop 3
   if (.not. c_associated (ptiles1, c_loc(var%tiles(1)))) stop 4
   if (.not. c_associated (pden1, c_loc(var%tiles(1)%den1))) stop 5
   if (.not. c_associated (pden2, c_loc(var%tiles(1)%den2))) stop 6
 else
   if (c_associated (ptiles, c_loc(var%tiles))) stop 3
   if (c_associated (ptiles1, c_loc(var%tiles(1)))) stop 4
   if (c_associated (pden1, c_loc(var%tiles(1)%den1))) stop 5
   if (c_associated (pden2, c_loc(var%tiles(1)%den2))) stop 6
 endif
!$omp end target

if (is_self_map) then
  if (any (var%tiles(1)%den1 /= 5 + reshape([1,2,3,4],[2,2]))) stop 7
  if (any (var%tiles(1)%den2 /= 7 + reshape([11,22,33,44],[2,2]))) stop 8
else
  if (any (var%tiles(1)%den1 /= reshape([1,2,3,4],[2,2]))) stop 7
  if (any (var%tiles(1)%den2 /= reshape([11,22,33,44],[2,2]))) stop 8
endif

!$omp target exit data map(var%tiles(1))

if (any (var%tiles(1)%den1 /= 5 + reshape([1,2,3,4],[2,2]))) stop 7
if (any (var%tiles(1)%den2 /= 7 + reshape([11,22,33,44],[2,2]))) stop 8
end
