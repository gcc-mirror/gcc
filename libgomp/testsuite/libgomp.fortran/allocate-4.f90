! { dg-do compile }


subroutine test()
use iso_c_binding, only: c_intptr_t
implicit none
integer, parameter :: omp_allocator_handle_kind = 1 !! <<<
integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_high_bw_mem_alloc = 4
integer :: q, x,y,z
integer, parameter :: cnst(2) = [64, 101]

!$omp parallel allocate( omp_high_bw_mem_alloc : x)  firstprivate(x) ! { dg-error "Expected integer expression of the 'omp_allocator_handle_kind' kind" }
!$omp end parallel

!$omp parallel allocate( allocator (omp_high_bw_mem_alloc) : x)  firstprivate(x) ! { dg-error "Expected integer expression of the 'omp_allocator_handle_kind' kind" }
!$omp end parallel

!$omp parallel allocate( align (q) : x)  firstprivate(x) ! { dg-error "32:ALIGN requires a scalar positive constant integer alignment expression at \\(1\\) that is a power of two" }
!$omp end parallel

!$omp parallel allocate( align (32) : x)  firstprivate(x) ! OK
!$omp end parallel

!$omp parallel allocate( align(q) : x) firstprivate(x) ! { dg-error "31:ALIGN requires a scalar positive constant integer alignment expression at \\(1\\) that is a power of two" }
!$omp end parallel

!$omp parallel allocate( align(cnst(1)) : x ) firstprivate(x) ! OK
!$omp end parallel

!$omp parallel allocate( align(cnst(2)) : x) firstprivate(x)  ! { dg-error "31:ALIGN requires a scalar positive constant integer alignment expression at \\(1\\) that is a power of two" }
!$omp end parallel

!$omp parallel allocate( align( 31) :x) firstprivate(x)  ! { dg-error "32:ALIGN requires a scalar positive constant integer alignment expression at \\(1\\) that is a power of two" }
!$omp end parallel

!$omp parallel allocate( align (32.0): x) firstprivate(x)  ! { dg-error "32:ALIGN requires a scalar positive constant integer alignment expression at \\(1\\) that is a power of two" }
!$omp end parallel

!$omp parallel allocate( align(cnst ) : x ) firstprivate(x)  ! { dg-error "31:ALIGN requires a scalar positive constant integer alignment expression at \\(1\\) that is a power of two" }
!$omp end parallel
end
