/* This testcase is mostly the same as alloc-8.c.
   However, on systems where the numa and/or memkind libraries are
   installed, libgomp uses those.  This test ensures that the minimal
   features work. Note: No attempt has been made to verify the parition
   hints interleaved and nearest as the kernal purposely ignore them once
   in a while and it would also require a 'dlopen' dance.

   memkind is used for omp_high_bw_mem_space, omp_large_cap_mem_space
   and partition = interleaved, albeit it won't be interleaved for
   omp_large_cap_mem_space.

   numa is used for partition = nearest, unless memkind is used.  */

#include <omp.h>
#include <stdint.h>
#include <stdlib.h>

const omp_alloctrait_t traits2[]
= { { omp_atk_alignment, 16 },
    { omp_atk_sync_hint, omp_atv_default },
    { omp_atk_access, omp_atv_default },
    { omp_atk_pool_size, 1024 },
    { omp_atk_fallback, omp_atv_default_mem_fb },
    { omp_atk_partition, omp_atv_nearest } };
omp_alloctrait_t traits3[]
= { { omp_atk_sync_hint, omp_atv_uncontended },
    { omp_atk_alignment, 32 },
    { omp_atk_access, omp_atv_all },
    { omp_atk_pool_size, 512 },
    { omp_atk_fallback, omp_atv_allocator_fb },
    { omp_atk_fb_data, 0 },
    { omp_atk_partition, omp_atv_interleaved } };
const omp_alloctrait_t traits4[]
= { { omp_atk_alignment, 128 },
    { omp_atk_pool_size, 1024 },
    { omp_atk_fallback, omp_atv_null_fb } };

static void
check_all_zero (void *ptr, size_t len)
{
  char *p = (char *) ptr;
  for (size_t i = 0; i < len; i++)
    if (p[i] != '\0')
      abort ();
}

int
main ()
{
  int *volatile p = (int *) omp_aligned_calloc (sizeof (int), 3, sizeof (int), omp_high_bw_mem_alloc);
  check_all_zero (p, 3*sizeof (int));
  int *volatile q;
  int *volatile r;
  int i;
  omp_alloctrait_t traits[3]
    = { { omp_atk_alignment, 64 },
	{ omp_atk_fallback, omp_atv_null_fb },
	{ omp_atk_pool_size, 4096 } };
  omp_allocator_handle_t a, a2;

  if ((((uintptr_t) p) % __alignof (int)) != 0 || p[0] || p[1] || p[2])
    abort ();
  p[0] = 1;
  p[1] = 2;
  p[2] = 3;
  omp_free (p, omp_high_bw_mem_alloc);
  p = (int *) omp_aligned_calloc (2 * sizeof (int), 1, 2 * sizeof (int), omp_large_cap_mem_alloc);
  check_all_zero (p, 2*sizeof (int));
  if ((((uintptr_t) p) % (2 * sizeof (int))) != 0 || p[0] || p[1])
    abort ();
  p[0] = 1;
  p[1] = 2;
  omp_free (p, omp_null_allocator);
  omp_set_default_allocator (omp_large_cap_mem_alloc);
  p = (int *) omp_aligned_calloc (1, 1, sizeof (int), omp_null_allocator);
  check_all_zero (p, sizeof (int));
  if ((((uintptr_t) p) % __alignof (int)) != 0 || p[0])
    abort ();
  p[0] = 3;
  omp_free (p, omp_get_default_allocator ());

  a = omp_init_allocator (omp_large_cap_mem_space, 3, traits);
  if (a == omp_null_allocator)
    abort ();
  p = (int *) omp_aligned_calloc (32, 3, 1024, a);
  check_all_zero (p, 3*1024);
  if ((((uintptr_t) p) % 64) != 0)
    abort ();
  for (i = 0; i < 3072 / sizeof (int); i++)
    if (p[i])
      abort ();
  p[0] = 1;
  p[3071 / sizeof (int)] = 2;
  if (omp_aligned_calloc (8, 192, 16, a) != NULL)
    abort ();
  omp_free (p, a);
  p = (int *) omp_aligned_calloc (128, 6, 512, a);
  check_all_zero (p, 6*512);
  if ((((uintptr_t) p) % 128) != 0)
    abort ();
  for (i = 0; i < 3072 / sizeof (int); i++)
    if (p[i])
      abort ();
  p[0] = 3;
  p[3071 / sizeof (int)] = 4;
  omp_free (p, omp_null_allocator);
  omp_set_default_allocator (a);
  if (omp_get_default_allocator () != a)
    abort ();
  p = (int *) omp_aligned_calloc (64, 12, 256, omp_null_allocator);
  check_all_zero (p, 12*256);
  for (i = 0; i < 3072 / sizeof (int); i++)
    if (p[i])
      abort ();
  if (omp_aligned_calloc (8, 128, 24, omp_null_allocator) != NULL)
    abort ();
  omp_free (p, a);
  omp_destroy_allocator (a);

  a = omp_init_allocator (omp_default_mem_space,
			  sizeof (traits2) / sizeof (traits2[0]),
			  traits2);
  if (a == omp_null_allocator)
    abort ();
  if (traits3[5].key != omp_atk_fb_data)
    abort ();
  traits3[5].value = (uintptr_t) a;
  a2 = omp_init_allocator (omp_default_mem_space,
			   sizeof (traits3) / sizeof (traits3[0]),
			   traits3);
  if (a2 == omp_null_allocator)
    abort ();
  p = (int *) omp_aligned_calloc (4, 5, 84, a2);
  check_all_zero (p, 5*84);
  for (i = 0; i < 420 / sizeof (int); i++)
    if (p[i])
      abort ();
  if ((((uintptr_t) p) % 32) != 0)
    abort ();
  p[0] = 5;
  p[419 / sizeof (int)] = 6;
  q = (int *) omp_aligned_calloc (8, 24, 32, a2);
  check_all_zero (q, 24*32);
  if ((((uintptr_t) q) % 16) != 0)
    abort ();
  for (i = 0; i < 768 / sizeof (int); i++)
    if (q[i])
      abort ();
  q[0] = 7;
  q[767 / sizeof (int)] = 8;
  r = (int *) omp_aligned_calloc (8, 64, 8, a2);
  check_all_zero (r, 64*8);
  if ((((uintptr_t) r) % 8) != 0)
    abort ();
  for (i = 0; i < 512 / sizeof (int); i++)
    if (r[i])
      abort ();
  r[0] = 9;
  r[511 / sizeof (int)] = 10;
  omp_free (p, omp_null_allocator);
  omp_free (q, a2);
  omp_free (r, omp_null_allocator);
  omp_destroy_allocator (a2);
  omp_destroy_allocator (a);

  a = omp_init_allocator (omp_high_bw_mem_space,
			  sizeof (traits4) / sizeof (traits4[0]),
			  traits4);
  if (a == omp_null_allocator)
    abort ();
  if (traits3[5].key != omp_atk_fb_data)
    abort ();
  traits3[5].value = (uintptr_t) a;
  a2 = omp_init_allocator (omp_high_bw_mem_space,
			   sizeof (traits3) / sizeof (traits3[0]),
			   traits3);
  if (a2 == omp_null_allocator)
    abort ();
  omp_set_default_allocator (a2);
#ifdef __cplusplus
  p = static_cast <int *> (omp_aligned_calloc (4, 21, 20));
#else
  p = (int *) omp_aligned_calloc (4, 21, 20, omp_null_allocator);
#endif
  check_all_zero (p, 21*20);
  if ((((uintptr_t) p) % 32) != 0)
    abort ();
  for (i = 0; i < 420 / sizeof (int); i++)
    if (p[i])
      abort ();
  p[0] = 5;
  p[419 / sizeof (int)] = 6;
  q = (int *) omp_aligned_calloc (64, 12, 64, omp_null_allocator);
  check_all_zero (q, 12*64);
  if ((((uintptr_t) q) % 128) != 0)
    abort ();
  for (i = 0; i < 768 / sizeof (int); i++)
    if (q[i])
      abort ();
  q[0] = 7;
  q[767 / sizeof (int)] = 8;
  if (omp_aligned_calloc (8, 24, 32, omp_null_allocator) != NULL)
    abort ();
#ifdef __cplusplus
  omp_free (p);
  omp_free (q);
  omp_free (NULL);
#else
  omp_free (p, omp_null_allocator);
  omp_free (q, omp_null_allocator);
  omp_free (NULL, omp_null_allocator);
#endif
  omp_free (NULL, omp_null_allocator);
  omp_destroy_allocator (a2);
  omp_destroy_allocator (a);
  return 0;
}
