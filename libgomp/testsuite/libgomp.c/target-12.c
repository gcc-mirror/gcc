#include <omp.h>
#include <stdlib.h>

int
main ()
{
  int d = omp_get_default_device ();
  int id = omp_get_initial_device ();
  int err;
  int q[128], i;
  void *p;

  if (d < 0 || d >= omp_get_num_devices ())
    d = id;

  for (i = 0; i < 128; i++)
    q[i] = i;

  p = omp_target_alloc (130 * sizeof (int), d);
  if (p == NULL)
    return 0;

  if (omp_target_memcpy_rect (NULL, NULL, 0, 0, NULL, NULL, NULL, NULL, NULL,
			      d, id) < 3
      || omp_target_memcpy_rect (NULL, NULL, 0, 0, NULL, NULL, NULL, NULL,
				 NULL, id, d) < 3
      || omp_target_memcpy_rect (NULL, NULL, 0, 0, NULL, NULL, NULL, NULL,
				 NULL, id, id) < 3)
    abort ();

  if (omp_target_associate_ptr (q, p, 128 * sizeof (int), sizeof (int), d) == 0)
    {
      size_t volume[3] = { 128, 0, 0 };
      size_t dst_offsets[3] = { 0, 0, 0 };
      size_t src_offsets[3] = { 1, 0, 0 };
      size_t dst_dimensions[3] = { 128, 0, 0 };
      size_t src_dimensions[3] = { 128, 0, 0 };

      if (omp_target_associate_ptr (q, p, 128 * sizeof (int), sizeof (int), d) != 0)
	abort ();

      if (omp_target_is_present (q, d) != 1
	  || omp_target_is_present (&q[32], d) != 1
	  || omp_target_is_present (&q[127], d) != 1)
	abort ();

      if (omp_target_memcpy (p, q, 128 * sizeof (int), sizeof (int), 0,
			     d, id) != 0)
	abort ();

      #pragma omp target if (d >= 0) device (d >= 0 ? d : 0) map(alloc:q[0:32]) map(from:err)
      {
	int j;
	err = 0;
	for (j = 0; j < 128; j++)
	  if (q[j] != j)
	    err = 1;
	  else
	    q[j] += 4;
      }

      if (err)
	abort ();

      if (omp_target_memcpy_rect (q, p, sizeof (int), 1, volume,
				  dst_offsets, src_offsets, dst_dimensions,
				  src_dimensions, id, d) != 0)
	abort ();

      for (i = 0; i < 128; i++)
	if (q[i] != i + 4)
	  abort ();

      volume[2] = 2;
      volume[1] = 3;
      volume[0] = 6;
      dst_offsets[2] = 1;
      dst_offsets[1] = 0;
      dst_offsets[0] = 0;
      src_offsets[2] = 1;
      src_offsets[1] = 0;
      src_offsets[0] = 3;
      dst_dimensions[2] = 3;
      dst_dimensions[1] = 3;
      dst_dimensions[0] = 6;
      src_dimensions[2] = 3;
      src_dimensions[1] = 4;
      src_dimensions[0] = 9;
      if (omp_target_memcpy_rect (p, q, sizeof (int), 3, volume,
				  dst_offsets, src_offsets, dst_dimensions,
				  src_dimensions, d, id) != 0)
	abort ();

      #pragma omp target if (d >= 0) device (d >= 0 ? d : 0) map(alloc:q[0:32]) map(from:err)
      {
	int j, k, l;
	err = 0;
	for (j = 0; j < 6; j++)
	  for (k = 0; k < 3; k++)
	    for (l = 0; l < 2; l++)
	      if (q[j * 9 + k * 3 + l] != 3 * 12 + 4 + 1 + l + k * 3 + j * 12)
		err = 1;
      }

      if (err)
	abort ();

      if (omp_target_memcpy (p, p, 10 * sizeof (int), 51 * sizeof (int),
			     111 * sizeof (int), d, d) != 0)
	abort ();

      #pragma omp target if (d >= 0) device (d >= 0 ? d : 0) map(alloc:q[0:32]) map(from:err)
	{
	  int j;
	  err = 0;
	  for (j = 0; j < 10; j++)
	    if (q[50 + j] != q[110 + j])
	      err = 1;
	}

      if (err)
	abort ();

      if (omp_target_disassociate_ptr (q, d) != 0)
	abort ();
    }

  omp_target_free (p, d);
  return 0;
}
