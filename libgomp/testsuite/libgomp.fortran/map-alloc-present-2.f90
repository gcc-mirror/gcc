! Test the 'present' modifier with derived-type allocatable components.

module m
   implicit none
   type field_type
     real(kind=8), allocatable :: density0(:,:), density1(:,:)
   end type field_type

   type tile_type
     type(field_type) :: field
   end type tile_type

   type chunk_type
     real(kind=8), allocatable :: left_rcv_buffer(:)
     type(tile_type), allocatable :: tiles(:)
   end type chunk_type

   type(chunk_type) :: chunk
end

use m
implicit none
allocate(chunk%tiles(1))
chunk%tiles(1)%field%density0 = reshape([1,2,3,4],[2,2])

!$omp target enter data &
!$omp   map(to: chunk%tiles(1)%field%density0) &
!$omp   map(to: chunk%tiles(1)%field%density1)

!$omp target map(present, alloc: chunk%tiles(1)%field%density0)
  if (.not. allocated(chunk%tiles(1)%field%density0)) stop 1
  if (any (chunk%tiles(1)%field%density0 /= reshape([1,2,3,4],[2,2]))) stop 1
   chunk%tiles(1)%field%density0 = chunk%tiles(1)%field%density0 * 2
!$omp end target

chunk%tiles(1)%field%density1 = reshape([11,22,33,44],[2,2])

!$omp target map(alloc: chunk%tiles(1)%field%density0)
  if (.not. allocated(chunk%tiles(1)%field%density0)) stop 1
  if (any (chunk%tiles(1)%field%density0 /= 2*reshape([1,2,3,4],[2,2]))) stop 1
   chunk%tiles(1)%field%density0 = chunk%tiles(1)%field%density0 * 7
!$omp end target

!$omp target exit data &
!$omp   map(from: chunk%tiles(1)%field%density0)

if (any (chunk%tiles(1)%field%density0 /= 7*2*reshape([1,2,3,4],[2,2]))) stop 1
if (any (chunk%tiles(1)%field%density1 /= reshape([11,22,33,44],[2,2]))) stop 2

end
