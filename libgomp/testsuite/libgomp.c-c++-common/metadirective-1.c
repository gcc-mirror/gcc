/* { dg-do run { target { ! offload_target_nvptx } } } */
/* { dg-do compile { target offload_target_nvptx } } */

#define N 100

void
f (int x[], int y[], int z[])
{
  int i;

  // The following fails as on the host the target side cannot be
  // resolved - and the 'teams' or not status affects how 'target'
  // is called.
  // Note also the dg-do compile above for offload_target_nvptx
  #pragma omp target map(to: x[0:N], y[0:N]) map(from: z[0:N])
    #pragma omp metadirective \
	when (device={arch("nvptx")}: teams loop) \
	default (parallel loop)
      for (i = 0; i < N; i++)
	z[i] = x[i] * y[i];
  /* { dg-bogus "'target' construct with nested 'teams' construct contains directives outside of the 'teams' construct" "PR118694" { xfail offload_target_nvptx } .-6 }  */
}

int
main (void)
{
  int x[N], y[N], z[N];
  int i;

  for (i = 0; i < N; i++)
    {
      x[i] = i;
      y[i] = -i;
    }

  f (x, y, z);

  for (i = 0; i < N; i++)
    if (z[i] != x[i] * y[i])
      return 1;

  return 0;
}
