/* { dg-do compile } */
#include <omp.h>

void
f (omp_allocator_handle_t my_alloc)
{
  #pragma omp target
  {
    // The location of this error in C seems wrong.
    int a; /* { dg-error "'my_alloc' in 'allocator' clause inside a target region must be specified in an 'uses_allocators' clause on the 'target' directive" "" { target c } } */
    #pragma omp allocate(a) allocator(my_alloc) /* { dg-error "'my_alloc' in 'allocator' clause inside a target region must be specified in an 'uses_allocators' clause on the 'target' directive" "" { target c++ } } */
    a  = 5;
    void *prt = omp_alloc(32, my_alloc);
    #pragma omp parallel allocate(allocator(my_alloc) : a) firstprivate(a) /* { dg-error "allocator 'my_alloc' in 'allocate' clause inside a target region must be specified in an 'uses_allocators' clause on the 'target' directive" } */
      a = 7;
  }
}

void
g (omp_allocator_handle_t my_alloc)
{
  /* The following defines a default-mem-space allocator with no extra traits. */
  #pragma omp target uses_allocators(my_alloc)
  {
    int a;
    #pragma omp allocate(a) allocator(my_alloc)
    a  = 5;
    void *prt = omp_alloc(32, my_alloc);
    #pragma omp parallel allocate(allocator(my_alloc) : a) firstprivate(a)
      a = 7;
  }
}
