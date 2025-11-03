/* { dg-do run }  */
/* { dg-additional-options "-O3" }  */

/* PR libgomp/122281  */
/* PR middle-end/105001  */

/* If SIMT is supported, the inner 'omp simd' is duplicated into
   one SIMT and one SIMD variant. SIMT is currently only supported
   with nvidia GPUs.  (This only happens with -O1 or higher.)

   The duplication failed for the SIMD case as a tree was shared and
   the initialization only happened in the SIMT branch, i.e. when
   compiling for a SIMT-device, all non-SIMD (offload or host devices)
   accesses failed (segfault) for the atomic update.  */

#include <omp.h>

int __attribute__((noinline, noclone))
f(int *A, int n, int dev) {
 int cnt = 0;
 #pragma omp target map(cnt) map(to:A[0:n]) device(dev)
 {
   #pragma omp parallel for simd
   for (int i = 0; i < n; i++)
   if (A[i] != 0)
     {
       #pragma omp atomic
       cnt++;
     }
 }
 return cnt;
}

int main() {
  int n = 10;
  int A[10] = {11,22,33,44,55,66,77,88,99,110};

  /* Run over all devices, including the host; the host should be SIMD,
     some non-host devices might be SIMT.  */
  for (int dev = omp_initial_device; dev <= omp_get_num_devices(); dev++)
    if (f (A, n, dev) != 10)
      __builtin_abort();
}
