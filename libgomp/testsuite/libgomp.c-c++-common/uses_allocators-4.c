/* { dg-additional-options "-fdump-tree-gimple" } */

#include <stdint.h>
#include <omp.h>

int
main ()
{
  int x, *xbuf[10];
  omp_allocator_handle_t my_alloc;
  const omp_alloctrait_t trait[1]= {{omp_atk_alignment,128}};

  #pragma omp target uses_allocators(omp_low_lat_mem_alloc) map(tofrom: x, xbuf) defaultmap(none)
    #pragma omp parallel allocate(allocator(omp_low_lat_mem_alloc), align(128): x, xbuf) if(0) firstprivate(x, xbuf)
      {
	if ((uintptr_t) &x % 128 != 0)
	  __builtin_abort ();
	if ((uintptr_t) xbuf % 128 != 0)
	  __builtin_abort ();
      }

  my_alloc = (omp_allocator_handle_t) 0xABCD;

  #pragma omp target uses_allocators(traits(trait): my_alloc) defaultmap(none) map(tofrom: x, xbuf)
    #pragma omp parallel allocate(allocator(my_alloc): x, xbuf) if(0) firstprivate(x, xbuf)
      {
	if ((uintptr_t) &x % 128 != 0)
	  __builtin_abort ();
	if ((uintptr_t) xbuf % 128 != 0)
	  __builtin_abort ();
      }

  if (my_alloc != (omp_allocator_handle_t) 0xABCD)
    __builtin_abort ();

  /* The following creates an allocator with empty traits + default mem space. */
  #pragma omp target uses_allocators(my_alloc) map(tofrom: x, xbuf) defaultmap(none)
    #pragma omp parallel allocate(allocator(my_alloc), align(128): x, xbuf) if(0) firstprivate(x, xbuf)
      {
	if ((uintptr_t) &x % 128 != 0)
	  __builtin_abort ();
	if ((uintptr_t) xbuf % 128 != 0)
	  __builtin_abort ();
      }

  if (my_alloc != (omp_allocator_handle_t) 0xABCD)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "#pragma omp target .*private\\(my_alloc\\).*uses_allocators\\(my_alloc: memspace\\(\\), traits\\(trait\\)\\)" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp target .*private\\(my_alloc\\).*uses_allocators\\(my_alloc: memspace\\(\\), traits\\(\\)\\)" 1 "gimple" } } */
