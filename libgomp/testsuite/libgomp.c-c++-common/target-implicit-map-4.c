/* PR middle-end/110270 */

/* Same as target-implicit-map-3.c but uses the following requiement
   and for not mapping the stack variables 'A' and 'B' (not mapped
   but accessible -> USM makes this tested feature even more important.)  */

#pragma omp requires unified_shared_memory

/* Ensure that defaultmap(default : pointer) uses correct OpenMP 5.2
   semantic, i.e. keeping the pointer value even if not mapped;
   before OpenMP 5.0/5.1 required that it is NULL. */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <omp.h>

void
test_device (int dev)
{
  int *p1 = (int*) 0x12345;
  int *p1a = (int*) 0x67890;
  int *p2 = (int*) omp_target_alloc (sizeof (int) * 5, dev);
  int *p2a = (int*) omp_target_alloc (sizeof (int) * 10, dev);
  intptr_t ip = (intptr_t) p2;
  intptr_t ipa = (intptr_t) p2a;

  int A[3] = {1,2,3};
  int B[5] = {4,5,6,7,8};
  int *p3 = &A[0];
  int *p3a = &B[0];

  const omp_alloctrait_t traits[]
    = { { omp_atk_alignment, 128 },
	{ omp_atk_pool_size, 1024 }};
  omp_allocator_handle_t a = omp_init_allocator (omp_default_mem_space, 2, traits);

  int *p4 = (int*) malloc (sizeof (int) * 5);
  int *p4a = (int*) omp_alloc (sizeof (int) * 10, a);
  intptr_t ip4 = (intptr_t) p4;
  intptr_t ip4a = (intptr_t) p4a;

  for (int i = 0; i < 5; i++)
    p4[i] = -31*i;

  for (int i = 0; i < 10; i++)
    p4a[i] = -43*i;

  /* Note: 'A' is not mapped but USM accessible.  */
  #pragma omp target device(dev) /* defaultmap(default:pointer) */
  {
    /* The pointees aren't mapped. */
    /* OpenMP 5.2 -> same value as before the target region. */
    if ((intptr_t) p1 != 0x12345) abort ();
    if ((intptr_t) p2 != ip) abort ();
    for (int i = 0; i < 5; i++)
      p2[i] = 13*i;

    for (int i = 0; i < 10; i++)
      ((int *)ipa)[i] = 7*i;

    /* OpenMP: Points to 'A'. */
    if (p3[0] != 1 || p3[1] != 2 || p3[2] != 3)
      abort ();
    p3[0] = -11; p3[1] = -22; p3[2] = -33;

    /* USM accesible allocated host memory. */
    if ((intptr_t) p4 != ip4)
      abort ();
    for (int i = 0; i < 5; i++)
      if (p4[i] != -31*i)
	abort ();
    for (int i = 0; i < 10; i++)
      if (((int *)ip4a)[i] != -43*i)
	abort ();
    for (int i = 0; i < 5; i++)
      p4[i] = 9*i;
    for (int i = 0; i < 10; i++)
      ((int *)ip4a)[i] = 18*i;
  }

  if (p3[0] != -11 || p3[1] != -22 || p3[2] != -33)
    abort ();

  for (int i = 0; i < 5; i++)
    if (p4[i] != 9*i)
      abort ();
  for (int i = 0; i < 10; i++)
    if (p4a[i] != 18*i)
      abort ();
  for (int i = 0; i < 5; i++)
    p4[i] = -77*i;
  for (int i = 0; i < 10; i++)
    p4a[i] = -65*i;

  // With defaultmap:

  /* Note: 'B' is not mapped but USM accessible.  */
  #pragma omp target device(dev) defaultmap(default:pointer)
  {
    /* The pointees aren't mapped. */
    /* OpenMP 5.2 -> same value as before the target region. */
    if ((intptr_t) p1a != 0x67890) abort ();
    if ((intptr_t) p2a != ipa) abort ();

    for (int i = 0; i < 5; i++)
      ((int *)ip)[i] = 13*i;

    for (int i = 0; i < 10; i++)
      p2a[i] = 7*i;

    /* USM accesible allocated host memory. */
    if ((intptr_t) p4a != ip4a) abort ();

    /* OpenMP: Points to 'B'. */
    if (p3a[0] != 4 || p3a[1] != 5 || p3a[2] != 6 || p3a[3] != 7 || p3a[4] != 8)
      abort ();
    p3a[0] = -44; p3a[1] = -55; p3a[2] = -66; p3a[3] = -77; p3a[4] = -88;

    /* USM accesible allocated host memory. */
    if ((intptr_t) p4a != ip4a)
      abort ();
    for (int i = 0; i < 5; i++)
      if (((int *)ip4)[i] != -77*i)
	abort ();
    for (int i = 0; i < 10; i++)
      if (p4a[i] != -65*i)
	abort ();
    for (int i = 0; i < 5; i++)
      p4[i] = 36*i;
    for (int i = 0; i < 10; i++)
      ((int *)ip4a)[i] = 4*i;
  }

  if (p3a[0] != -44 || p3a[1] != -55 || p3a[2] != -66 || p3a[3] != -77 || p3a[4] != -88)
    abort ();

  for (int i = 0; i < 5; i++)
    if (p4[i] != 36*i)
      abort ();
  for (int i = 0; i < 10; i++)
    if (p4a[i] != 4*i)
      abort ();

  omp_target_free (p2, dev);
  omp_target_free (p2a, dev);
  free (p4);
  omp_free (p4a, a);
  omp_destroy_allocator (a);
}

int
main()
{
  int ntgts = omp_get_num_devices();
  for (int i = 0; i <= ntgts; i++)
    test_device (i);
  return 0;
}
