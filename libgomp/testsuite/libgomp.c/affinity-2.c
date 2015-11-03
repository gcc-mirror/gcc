/* { dg-do run } */
/* { dg-set-target-env-var OMP_PROC_BIND "spread,close" } */
/* { dg-set-target-env-var OMP_PLACES "{6,7}:4:-2,!{2,3}" } */
/* { dg-set-target-env-var OMP_NUM_THREADS "2" } */

#include <omp.h>
#include <stdlib.h>
#include <stdio.h>

int *
get_buf (int nump)
{
  static int *buf;
  static size_t buf_size;
  if ((size_t) nump > buf_size)
    {
      buf_size *= 2;
      if (nump > buf_size)
	buf_size = nump + 64;
      int *bufn = realloc (buf, buf_size * sizeof (int));
      if (bufn == NULL)
	{
	  fprintf (stderr, "memory allocation error\n");
	  exit (1);
	}
      buf = bufn;
    }
  return buf;
}

void
print_place (int count, int *ids)
{
  int i, j;
  printf ("{");
  for (i = 0; i < count; i++)
    {
      for (j = i + 1; j < count; j++)
	if (ids[j] != ids[i] + (j - i))
	  break;
      if (i)
	printf (",");
      if (j == i + 1)
	printf ("%d", ids[i]);
      else
	{
	  printf ("%d:%d", ids[i], j - i);
	  i = j - 1;
	}
    }
  printf ("}\n");
}

void
print_place_var (void)
{
  int place = omp_get_place_num ();
  int num_places = omp_get_partition_num_places ();
  int *ids = get_buf (num_places);
  omp_get_partition_place_nums (ids);
  printf ("place %d\n", place);
  if (num_places)
    printf ("partition %d-%d\n", ids[0], ids[num_places - 1]);
}

int
main ()
{
  int i, num = omp_get_num_places (), nump, *ids;
  printf ("omp_get_num_places () == %d\n", num);
  for (i = 0; i < num; i++)
    {
      printf ("place %d ", i);
      nump = omp_get_place_num_procs (i);
      ids = get_buf (nump);
      omp_get_place_proc_ids (i, ids);
      print_place (nump, ids);
    }
  print_place_var ();
  omp_set_nested (1);
  #pragma omp parallel
    if (omp_get_thread_num () == omp_get_num_threads () - 1)
      {
      #pragma omp parallel
	if (omp_get_thread_num () == omp_get_num_threads () - 1)
	  print_place_var ();
      }
  return 0;
}
