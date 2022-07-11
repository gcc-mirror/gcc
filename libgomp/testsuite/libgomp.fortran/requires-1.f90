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
!   { dg-note {requires-1\.f90' has 'unified_shared_memory'} {} { target *-*-* } 0 }
!   TODO We're currently not streaming location information for the OpenMP
!   directives used in 'requires-7-aux.c', so we're not seeing the source file
!   name here (but a temporary '*.o' instead; for details, see
!   <https://gcc.gnu.org/pipermail/gcc-patches/2022-July/598011.html>):
!   { dg-note {requires-1-aux\.f90' has 'unified_address'} {} { xfail *-*-* } 0 }
!   ..., but we may still verify that the rest of the diagnostic is correct:
!   { dg-note {' has 'unified_address'} {} { target *-*-* } 0 }
! { dg-excess-errors "Ignore messages like: errors during merging of translation units|mkoffload returned 1 exit status" }
