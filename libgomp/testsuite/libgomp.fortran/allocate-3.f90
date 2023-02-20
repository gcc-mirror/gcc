! { dg-do compile }

use omp_lib
implicit none
integer :: q, x,y,z

!$omp parallel allocate(align ( 64 ) x)  ! { dg-error "37:Expected ':' at" }
!$omp parallel allocate(align ( 64 ), x)  ! { dg-error "37:Expected ':' at" }
!$omp parallel allocate(allocator ( omp_high_bw_mem_alloc ) x)  ! { dg-error "60:Expected ':' at" }
!$omp parallel allocate(allocator ( omp_high_bw_mem_alloc ) , x)  ! { dg-error "60:Expected ':' at" }

!$omp parallel allocate( omp_high_bw_mem_alloc, align(12) : x)  ! { dg-error "26:Expected variable list at" }
!$omp parallel allocate( align(12), omp_high_bw_mem_alloc : x)  ! { dg-error "35:Expected ':' at" }

!$omp parallel allocate( omp_high_bw_mem_alloc x)  ! { dg-error "26:Expected variable list at" }

!$omp parallel allocate( omp_high_bw_mem_alloc , x) firstprivate(x) ! { dg-error "'omp_high_bw_mem_alloc' specified in 'allocate' clause at \\(1\\) but not in an explicit privatization clause" }
! { dg-error "Object 'omp_high_bw_mem_alloc' is not a variable" "" { target *-*-* } .-1 }
!$omp end parallel

!$omp parallel allocate( omp_high_bw_mem_alloc , x) firstprivate(x, omp_high_bw_mem_alloc)
! { dg-error "Object 'omp_high_bw_mem_alloc' is not a variable" "" { target *-*-* } .-1 }
!$omp end parallel

!$omp parallel allocate( align(128) : x) firstprivate(x) ! OK
!$omp end parallel

end
