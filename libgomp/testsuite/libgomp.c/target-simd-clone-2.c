/* { dg-do link { target { offload_target_amdgcn } } } */
/* { dg-additional-options "-foffload-options=-fdump-ipa-simdclone-details -foffload-options=-fno-openmp-target-simd-clone" } */

/* Test that simd clones for the offload processor are not generated for
   functions with "declare target" when explicitly disabled.  */

#pragma omp declare target
__attribute__ ((__noinline__)) int addit (int a, int b)
{
  return a + b;
}

__attribute__ ((__noinline__))
void callit (int *a, int *b, int *c)
{
  int i;
  #pragma omp for simd
  for (i = 0; i < 16; i++)
    c[i] = addit (a[i], b[i]);
}
#pragma omp end declare target

int main (void)
{
  int aa[16], bb[16], cc[16];
  int i;
  for (i = 0; i < 16; i++)
    {
      aa[i] = i;
      bb[i] = -i;
    }
  callit (aa, bb, cc);
  for (i = 0; i < 16; i++)
    if (cc[i] != 0)
      return 1;
  return 0;
}

/* { dg-final { only_for_offload_target amdgcn-amdhsa scan-offload-ipa-dump-not "Generated .* clone" "simdclone" } } */
