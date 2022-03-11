/* { dg-do run } */
/* { dg-skip-if "Only valid for nvptx" { ! offload_target_nvptx } } */

#include <stdint.h>
#include <stdlib.h>

#include <omp.h>

/* On old systems, the declaraition may not be present in stdlib.h which
   will generate a warning.  This function is going to be replaced with
   omp_aligned_alloc so the purpose of this declaration is to avoid that
   warning.  */
void *aligned_alloc(size_t alignment, size_t size);

#pragma omp requires unified_shared_memory

int
main ()
{
  int *a = (int *) malloc(sizeof(int)*2);
  int *b = (int *) calloc(sizeof(int), 3);
  int *c = (int *) realloc(NULL, sizeof(int) * 4);
  int *d = (int *) aligned_alloc(32, sizeof(int));
  int *e = (int *) omp_target_alloc(sizeof(int), 1);
  if (!a || !b || !c || !d || !e)
    __builtin_abort ();

  a[0] = 42;
  a[1] = 43;
  b[0] = 52;
  b[1] = 53;
  b[2] = 54;
  c[0] = 62;
  c[1] = 63;
  c[2] = 64;
  c[3] = 65;

  uintptr_t a_p = (uintptr_t)a;
  uintptr_t b_p = (uintptr_t)b;
  uintptr_t c_p = (uintptr_t)c;
  uintptr_t d_p = (uintptr_t)d;
  uintptr_t e_p = (uintptr_t)e;

  if (d_p & 31 != 0)
    __builtin_abort ();

#pragma omp target enter data map(to:a[0:2])

#pragma omp target is_device_ptr(c)
    {
      if (a[0] != 42 || a_p != (uintptr_t)a)
	__builtin_abort ();
      if (b[0] != 52 || b[2] != 54 || b_p != (uintptr_t)b)
	__builtin_abort ();
      if (c[0] != 62 || c[3] != 65 || c_p != (uintptr_t)c)
	__builtin_abort ();
      if (d_p != (uintptr_t)d)
	__builtin_abort ();
      if (e_p != (uintptr_t)e)
	__builtin_abort ();
      a[0] = 72;
      b[0] = 82;
      c[0] = 92;
      e[0] = 102;
    }

#pragma omp target
    {
      if (a[1] != 43 || a_p != (uintptr_t)a)
	__builtin_abort ();
      if (b[1] != 53 || b_p != (uintptr_t)b)
	__builtin_abort ();
      if (c[1] != 63 || c[2] != 64 || c_p != (uintptr_t)c)
	__builtin_abort ();
      a[1] = 73;
      b[1] = 83;
      c[1] = 93;
    }

#pragma omp target exit data map(delete:a[0:2])

  if (a[0] != 72 || a[1] != 73
      || b[0] != 82 || b[1] != 83
      || c[0] != 92 || c[1] != 93
      || e[0] != 102)
	__builtin_abort ();
  free(a);
  free(b);
  free(c);
  omp_target_free(e, 1);
  return 0;
}
