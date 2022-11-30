/* { dg-do link { target { offload_target_amdgcn } } } */
/* { dg-additional-options "-foffload=-fdump-tree-omp_expand_metadirective" } */

#define N 100

void f (int x[], int y[], int z[])
{
  int i;

  #pragma omp target map(to: x, y) map(from: z)
    #pragma omp metadirective \
      when (device={isa("gfx803")}: teams num_teams(512)) \
      when (device={isa("gfx900")}: teams num_teams(256)) \
      when (device={isa("gfx906")}: teams num_teams(128)) \
      when (device={isa("gfx908")}: teams num_teams(64)) \
      when (device={isa("gfx90a")}: teams num_teams(32)) \
      default (teams num_teams(4))
	for (i = 0; i < N; i++)
	  z[i] = x[i] * y[i];
}

int main (void)
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

/* The metadirective should be resolved after Gimplification.  */

/* { dg-final { scan-offload-tree-dump "__builtin_GOMP_teams4 \\(512, 512" "omp_expand_metadirective" { target { any-opts "-foffload=-march=fiji" } } } } */
/* { dg-final { scan-offload-tree-dump "__builtin_GOMP_teams4 \\(256, 256" "omp_expand_metadirective" { target { any-opts "-foffload=-march=gfx900" } } } } */
/* { dg-final { scan-offload-tree-dump "__builtin_GOMP_teams4 \\(128, 128" "omp_expand_metadirective" { target { any-opts "-foffload=-march=gfx906" } } } } */
/* { dg-final { scan-offload-tree-dump "__builtin_GOMP_teams4 \\(64, 64" "omp_expand_metadirective" { target { any-opts "-foffload=-march=gfx908" } } } } */
/* { dg-final { scan-offload-tree-dump "__builtin_GOMP_teams4 \\(32, 32" "omp_expand_metadirective" { target { any-opts "-foffload=-march=gfx90a" } } } } */
