/* Test if acc_copyin has present_or_ and reference counting behavior.  */

#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  const int N = 256;
  int i;
  unsigned char *h;

  h = (unsigned char *) malloc (N);

  for (i = 0; i < N; i++)
    {
      h[i] = i;
    }

  (void) acc_copyin (h, N);
  (void) acc_copyin (h, N);

  acc_copyout (h, N);

  if (!acc_is_present (h, N))
    abort ();

  acc_copyout (h, N);

#if !ACC_MEM_SHARED
  if (acc_is_present (h, N))
    abort ();
#endif

  free (h);

  return 0;
}
