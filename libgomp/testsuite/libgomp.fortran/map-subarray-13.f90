! { dg-do run }

! PR fortran/120505

! Check that a nested allocatable DT component is mapped properly even when the
! first component is *not* mapped.

module m

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

allocate(chunk%tiles(1))
chunk%tiles(1)%field%density1 = reshape([1,2,3,4],[2,2])
allocate(chunk%left_rcv_buffer(1))

!$omp target enter data &
!$omp   map(to: chunk%tiles(1)%field%density1) &
!$omp   map(to: chunk%left_rcv_buffer)

!$omp target
  if (any (chunk%tiles(1)%field%density1 /= reshape([1,2,3,4],[2,2]))) stop 1
  chunk%tiles(1)%field%density1 = chunk%tiles(1)%field%density1 + 5
  chunk%left_rcv_buffer(1) = 42.0_8
!$omp end target

!$omp target exit data &
!$omp   map(from: chunk%tiles(1)%field%density1) &
!$omp   map(from: chunk%left_rcv_buffer)

if (any (chunk%tiles(1)%field%density1 /= 5 + reshape([1,2,3,4],[2,2]))) stop 1
if (chunk%left_rcv_buffer(1) /= 42.0_8) stop 1

end
