#include <omp.h>
#include <stdint.h>
#include <stdlib.h>

const omp_alloctrait_t traits2[]
= { { omp_atk_alignment, 16 },
    { omp_atk_sync_hint, omp_atv_default },
    { omp_atk_access, omp_atv_default },
    { omp_atk_pool_size, 1024 },
    { omp_atk_fallback, omp_atv_default_mem_fb },
    { omp_atk_partition, omp_atv_environment } };
omp_alloctrait_t traits3[]
= { { omp_atk_sync_hint, omp_atv_uncontended },
    { omp_atk_alignment, 32 },
    { omp_atk_access, omp_atv_all },
    { omp_atk_pool_size, 512 },
    { omp_atk_fallback, omp_atv_allocator_fb },
    { omp_atk_fb_data, 0 },
    { omp_atk_partition, omp_atv_default } };
const omp_alloctrait_t traits4[]
= { { omp_atk_alignment, 128 },
    { omp_atk_pool_size, 1024 },
    { omp_atk_fallback, omp_atv_null_fb } };

int
main ()
{
  int *volatile p = (int *) omp_alloc (3 * sizeof (int), omp_default_mem_alloc);
  int *volatile q;
  int *volatile r;
  omp_alloctrait_t traits[3]
    = { { omp_atk_alignment, 64 },
	{ omp_atk_fallback, omp_atv_null_fb },
	{ omp_atk_pool_size, 4096 } };
  omp_alloctrait_t traits5[2]
    = { { omp_atk_fallback, omp_atv_null_fb },
	{ omp_atk_pool_size, 4096 } };
  omp_allocator_handle_t a, a2;

  if ((((uintptr_t) p) % __alignof (int)) != 0)
    abort ();
  p[0] = 1;
  p[1] = 2;
  p[2] = 3;
  p = (int *) omp_realloc (p, 4 * sizeof (int), omp_default_mem_alloc, omp_default_mem_alloc);
  if ((((uintptr_t) p) % __alignof (int)) != 0 || p[0] != 1 || p[1] != 2 || p[2] != 3)
    abort ();
  p[0] = 4;
  p[1] = 5;
  p[2] = 6;
  p[3] = 7;
  p = (int *) omp_realloc (p, 2 * sizeof (int), omp_default_mem_alloc, omp_default_mem_alloc);
  if ((((uintptr_t) p) % __alignof (int)) != 0 || p[0] != 4 || p[1] != 5)
    abort ();
  p[0] = 8;
  p[1] = 9;
  if (omp_realloc (p, 0, omp_null_allocator, omp_default_mem_alloc) != NULL)
    abort ();
  p = (int *) omp_realloc (NULL, 2 * sizeof (int), omp_default_mem_alloc, omp_null_allocator);
  if ((((uintptr_t) p) % __alignof (int)) != 0)
    abort ();
  p[0] = 1;
  p[1] = 2;
  p = (int *) omp_realloc (p, 5 * sizeof (int), omp_default_mem_alloc, omp_default_mem_alloc);
  if ((((uintptr_t) p) % __alignof (int)) != 0 || p[0] != 1 || p[1] != 2)
    abort ();
  p[0] = 3;
  p[1] = 4;
  p[2] = 5;
  p[3] = 6;
  p[4] = 7;
  omp_free (p, omp_null_allocator);
  omp_set_default_allocator (omp_default_mem_alloc);
  if (omp_realloc (NULL, 0, omp_null_allocator, omp_null_allocator) != NULL)
    abort ();
  p = (int *) omp_alloc (sizeof (int), omp_null_allocator);
  if ((((uintptr_t) p) % __alignof (int)) != 0)
    abort ();
  p[0] = 3;
  p = (int *) omp_realloc (p, 3 * sizeof (int), omp_null_allocator, omp_null_allocator);
  if ((((uintptr_t) p) % __alignof (int)) != 0 || p[0] != 3)
    abort ();
  p[0] = 4;
  p[1] = 5;
  p[2] = 6;
  if (omp_realloc (p, 0, omp_null_allocator, omp_get_default_allocator ()) != NULL)
    abort ();
  a = omp_init_allocator (omp_default_mem_space, 3, traits);
  if (a == omp_null_allocator)
    abort ();
  p = (int *) omp_alloc (sizeof (int), a);
  if ((((uintptr_t) p) % 64) != 0)
    abort ();
  p[0] = 7;
  p = (int *) omp_realloc (p, 3072, a, a);
  if ((((uintptr_t) p) % 64) != 0 || p[0] != 7)
    abort ();
  p[0] = 1;
  p[3071 / sizeof (int)] = 2;
  q = (int *) omp_alloc (sizeof (int), a);
  if ((((uintptr_t) q) % 64) != 0)
    abort ();
  q[0] = 8;
  if (omp_realloc (q, 3072, a, a) != NULL)
    abort ();
  omp_free (p, a);
  omp_free (q, a);
  p = (int *) omp_alloc (sizeof (int), a);
  p[0] = 42;
  p = (int *) omp_realloc (p, 3072, a, a);
  if (p[0] != 42)
    abort ();
  p[0] = 3;
  p[3071 / sizeof (int)] = 4;
  omp_realloc (p, 0, omp_null_allocator, omp_null_allocator);
  omp_set_default_allocator (a);
  if (omp_get_default_allocator () != a)
    abort ();
  p = (int *) omp_alloc (31, omp_null_allocator);
  if (p == NULL)
    abort ();
  p = (int *) omp_realloc (p, 3072, omp_null_allocator, omp_null_allocator);
  if (p == NULL)
    abort ();
  q = (int *) omp_alloc (sizeof (int), omp_null_allocator);
  if (q == NULL)
    abort ();
  if (omp_realloc (q, 3072, omp_null_allocator, omp_null_allocator) != NULL)
    abort ();
  omp_free (p, a);
  omp_free (q, a);
  omp_destroy_allocator (a);

  a = omp_init_allocator (omp_default_mem_space, 2, traits5);
  if (a == omp_null_allocator)
    abort ();
  omp_set_default_allocator (a);
  if (omp_get_default_allocator () != a)
    abort ();
  p = (int *) omp_alloc (3071, omp_null_allocator);
  if (p == NULL)
    abort ();
  p = (int *) omp_realloc (p, 3072, omp_null_allocator, omp_null_allocator);
  if (p == NULL)
    abort ();
  q = (int *) omp_alloc (sizeof (int), omp_null_allocator);
  if (q == NULL)
    abort ();
  if (omp_realloc (q, 3072, omp_null_allocator, omp_null_allocator) != NULL)
    abort ();
  omp_free (p, a);
  omp_free (q, a);
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
  p = (int *) omp_alloc (sizeof (int), a2);
  if ((((uintptr_t) p) % 32) != 0)
    abort ();
  p[0] = 84;
  p = (int *) omp_realloc (p, 380, a2, a2);
  if ((((uintptr_t) p) % 32) != 0 || p[0] != 84)
    abort ();
  p[0] = 5;
  p[379 / sizeof (int)] = 6;
  q = (int *) omp_alloc (sizeof (int), a2);
  if ((((uintptr_t) q) % 32) != 0)
    abort ();
  q[0] = 42;
  q = (int *) omp_realloc (q, 768, a2, a2);
  if ((((uintptr_t) q) % 16) != 0 || q[0] != 42)
    abort ();
  q[0] = 7;
  q[767 / sizeof (int)] = 8;
  r = (int *) omp_realloc (NULL, 512, a2, omp_null_allocator);
  if ((((uintptr_t) r) % __alignof (int)) != 0)
    abort ();
  r[0] = 9;
  r[511 / sizeof (int)] = 10;
  omp_free (p, omp_null_allocator);
  omp_free (q, a2);
  omp_free (r, omp_null_allocator);
  p = (int *) omp_alloc (sizeof (int), a2);
  if ((((uintptr_t) p) % 32) != 0)
    abort ();
  p[0] = 85;
  p = (int *) omp_realloc (p, 320, a, a2);
  if ((((uintptr_t) p) % 16) != 0 || p[0] != 85)
    abort ();
  p[0] = 5;
  p[319 / sizeof (int)] = 6;
  q = (int *) omp_alloc (sizeof (int), a);
  if ((((uintptr_t) q) % 16) != 0)
    abort ();
  q[0] = 43;
  q = (int *) omp_realloc (q, 320, a2, a);
  if ((((uintptr_t) q) % 32) != 0 || q[0] != 43)
    abort ();
  q[0] = 44;
  q[319 / sizeof (int)] = 8;
  q = (int *) omp_realloc (q, 568, a2, a2);
  if ((((uintptr_t) q) % 16) != 0 || q[0] != 44)
    abort ();
  q[0] = 7;
  q[567 / sizeof (int)] = 8;
  omp_free (p, omp_null_allocator);
  omp_free (q, a2);
  omp_destroy_allocator (a2);
  omp_destroy_allocator (a);

  a = omp_init_allocator (omp_default_mem_space,
			  sizeof (traits4) / sizeof (traits4[0]),
			  traits4);
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
  omp_set_default_allocator (a2);
#ifdef __cplusplus
  p = static_cast <int *> (omp_realloc (NULL, 420));
#else
  p = (int *) omp_realloc (NULL, 420, omp_null_allocator, omp_null_allocator);
#endif
  if ((((uintptr_t) p) % 32) != 0)
    abort ();
  p[0] = 5;
  p[419 / sizeof (int)] = 6;
  q = (int *) omp_realloc (NULL, sizeof (int), omp_null_allocator, omp_null_allocator);
  if ((((uintptr_t) q) % 32) != 0)
    abort ();
  q[0] = 99;
  q = (int *) omp_realloc (q, 700, omp_null_allocator, omp_null_allocator);
  if ((((uintptr_t) q) % 128) != 0 || q[0] != 99)
    abort ();
  q[0] = 7;
  q[699 / sizeof (int)] = 8;
  if (omp_realloc (NULL, 768, omp_null_allocator, omp_null_allocator) != NULL)
    abort ();
#ifdef __cplusplus
  omp_free (p);
  if (omp_realloc (q, 0) != NULL)
    abort ();
  omp_free (NULL);
#else
  omp_free (p, omp_null_allocator);
  if (omp_realloc (q, 0, omp_null_allocator, omp_null_allocator) != NULL)
    abort ();
  omp_free (NULL, omp_null_allocator);
#endif
  omp_free (NULL, omp_null_allocator);
  omp_destroy_allocator (a2);
  omp_destroy_allocator (a);
  return 0;
}
