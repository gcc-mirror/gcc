/* { dg-do run } */

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <omp.h>

/* Say this is N rows and M columns.  */
#define N 1024
#define M 2048

#define row_offset 256
#define row_length 512
#define col_offset 128
#define col_length 384

int
main ()
{
  int *arr2d = (int *) calloc (N * M, sizeof (int));
  uintptr_t dstptr;
  int hostdev = omp_get_initial_device ();
  int targdev;

#pragma omp target enter data map(to: arr2d[col_offset*M:col_length*M])

#pragma omp target map(from: targdev, dstptr) \
		   map(present, tofrom: arr2d[col_offset*M:col_length*M])
  {
    for (int j = col_offset; j < col_offset + col_length; j++)
      for (int i = row_offset; i < row_offset + row_length; i++)
	arr2d[j * M + i]++;
    targdev = omp_get_device_num ();
    dstptr = (uintptr_t) arr2d;
  }

  /* Copy rectangular block back to the host.  */
  {
    size_t volume[2] = { col_length, row_length };
    size_t offsets[2] = { col_offset, row_offset };
    size_t dimensions[2] = { N, M };
    omp_target_memcpy_rect ((void *) arr2d, (const void *) dstptr,
			    sizeof (int), 2, &volume[0], &offsets[0],
			    &offsets[0], &dimensions[0], &dimensions[0],
			    hostdev, targdev);
  }

#pragma omp target exit data map(release: arr2d[col_offset*M:col_length*M])

  for (int j = 0; j < N; j++)
    for (int i = 0; i < M; i++)
      if (i >= row_offset && i < row_offset + row_length
	  && j >= col_offset && j < col_offset + col_length)
	assert (arr2d[j * M + i] == 1);
      else
	assert (arr2d[j * M + i] == 0);

  free (arr2d);

  return 0;
}
