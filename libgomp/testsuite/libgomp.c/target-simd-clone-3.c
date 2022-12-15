/* { dg-do link { target { offload_target_amdgcn } } } */
/* { dg-additional-options "-O2 -foffload-options=-fdump-ipa-simdclone-details" } */

/* Test that device simd clones are not generated for functions with the host
   "declare target" clause only.  */

__attribute__ ((__noinline__)) int addit (int a, int b)
{
  return a + b;
}
#pragma omp declare target to(addit) device_type(host)

#pragma omp declare target
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

/* { dg-final { scan-offload-ipa-dump "device doesn't match" "simdclone" { target x86_64-*-* } } } */
/* { dg-final { scan-offload-ipa-dump-not "Generated .* clone" "simdclone" { target x86_64-*-* } } } */
