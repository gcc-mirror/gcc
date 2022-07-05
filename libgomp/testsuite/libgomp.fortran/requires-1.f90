! { dg-do link { target offload_target_any } }
! { dg-additional-sources requires-1-aux.f90 }

! Check diagnostic by device-compiler's lto1.
!   Other file uses: 'requires unified_address'.

module m
  integer :: a(10)
  interface
    subroutine foo
    end
  end interface
end

program main
  !$omp requires unified_shared_memory

  !$omp target
    a = 0
  !$omp end target

  call foo ()
end

! { dg-error "OpenMP 'requires' directive with non-identical clauses in multiple compilation units: 'unified_shared_memory' vs. 'unified_address'" "" { target *-*-* } 0 }
! { dg-excess-errors "Ignore messages like: errors during merging of translation units|mkoffload returned 1 exit status" }
