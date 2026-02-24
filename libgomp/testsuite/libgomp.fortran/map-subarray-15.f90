! { dg-do run }

! PR fortran/120505

! Test 'target enter data' followed by 'target map' with a nested allocatable
! component.

module m

  type field_type
    real(kind=8), allocatable :: density0(:,:), density1(:,:)
  end type field_type

  type tile_type
    type(field_type),allocatable :: field(:)
  end type tile_type

  type chunk_type
    real(kind=8), allocatable :: left_rcv_buffer(:)
    type(tile_type), allocatable :: tiles(:)
  end type chunk_type

  type(chunk_type) :: chunk

end

use m

allocate(chunk%tiles(1))
allocate(chunk%tiles(1)%field(1))
chunk%tiles(1)%field(1)%density1 = reshape([1,2,3,4],[2,2])
allocate(chunk%left_rcv_buffer(1))

!$omp target enter data &
!$omp   map(to: chunk%tiles(1)%field(1)) &
!$omp   map(to: chunk%tiles(1)%field(1)%density1) &
!$omp   map(to: chunk%left_rcv_buffer)

!$omp target map(tofrom: chunk%tiles(1)%field(1)%density1)
  if (any (chunk%tiles(1)%field(1)%density1 /= reshape([1,2,3,4],[2,2]))) stop 1
  chunk%tiles(1)%field(1)%density1 = chunk%tiles(1)%field(1)%density1 + 5
  chunk%left_rcv_buffer(1) = 42.0_8
!$omp end target

!$omp target exit data &
!$omp   map(from: chunk%tiles(1)%field(1)%density1) &
!$omp   map(from: chunk%left_rcv_buffer)

if (any (chunk%tiles(1)%field(1)%density1 /= 5 + reshape([1,2,3,4],[2,2]))) stop 2
if (chunk%left_rcv_buffer(1) /= 42.0_8) stop 3

end
