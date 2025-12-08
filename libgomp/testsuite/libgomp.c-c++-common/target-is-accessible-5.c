#include <omp.h>

void check (int dev)
{
   constexpr int N = 10;
   constexpr int size = N*sizeof(int);
   int A[N] = {};

   void *ptr = omp_target_alloc (size, dev);

   if (ptr == nullptr || !omp_target_is_accessible (ptr, size, dev))
     __builtin_abort ();

   #pragma omp target device(dev) firstprivate(ptr)
   for (int i = 0; i < N; i++)
     ((int *)ptr)[i] = i + 1;

   if (omp_target_memcpy (A, ptr, size, 0, 0, omp_initial_device, dev) != 0)
     __builtin_abort ();

   for (int i = 0; i < N; i++)
     if (A[i] != i + 1)
       __builtin_abort ();

   omp_target_free (ptr, dev);
}

int main ()
{
   check (omp_default_device);
   for (int dev = 0; dev <= omp_get_num_devices(); dev++)
     check (dev);
}
