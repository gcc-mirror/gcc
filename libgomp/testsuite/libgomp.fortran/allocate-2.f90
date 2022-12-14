! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

use omp_lib
implicit none
integer :: q, x,y,z

!$omp parallel  &
!$omp&   allocate(omp_low_lat_mem_alloc : x) &
!$omp&   allocate(omp_cgroup_mem_alloc : y) &
!$omp&   allocate(omp_pteam_mem_alloc : z) &
!$omp&   firstprivate(q, x,y,z)
!$omp end parallel

!$omp parallel &
!$omp&   allocate(align ( 64 ), allocator(omp_default_mem_alloc) : x) &
!$omp&   allocate(allocator(omp_large_cap_mem_alloc) : y) &
!$omp&   allocate(allocator ( omp_high_bw_mem_alloc ) , align ( 32 ) : z) &
!$omp&   allocate(align (16 ): q) &
!$omp&   firstprivate(q, x,y,z)
!$omp end parallel
end

! { dg-final { scan-tree-dump-times "#pragma omp parallel firstprivate\\(q\\) firstprivate\\(x\\) firstprivate\\(y\\) firstprivate\\(z\\) allocate\\(allocator\\(5\\):x\\) allocate\\(allocator\\(6\\):y\\) allocate\\(allocator\\(7\\):z\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp parallel firstprivate\\(q\\) firstprivate\\(x\\) firstprivate\\(y\\) firstprivate\\(z\\) allocate\\(allocator\\(1\\),align\\(64\\):x\\) allocate\\(allocator\\(2\\):y\\) allocate\\(allocator\\(4\\),align\\(32\\):z\\) allocate\\(align\\(16\\):q\\)" 1 "original" } }
