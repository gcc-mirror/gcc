/* { dg-do link { target { offload_target_amdgcn } } } */
/* { dg-additional-options "-O2 -foffload-options=-fdump-ipa-simdclone-details" } */

/* Test that simd clones for the offload processor are generated for
   functions with "declare target" when enabled by default at -O2.  */

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

/* Although addit has external linkage, we expect clones to be generated as
   for a function with internal linkage.  */

/* { dg-final { scan-offload-ipa-dump "Generated local clone _ZGV.*N.*_addit" "simdclone" } } */
/* { dg-final { scan-offload-ipa-dump "Generated local clone _ZGV.*M.*_addit" "simdclone" } } */
