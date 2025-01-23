#include <stdlib.h>
#include <string.h>
#include <omp.h>

int main()
{
  const char **strs = (const char **) malloc (sizeof (char*) * (omp_get_num_devices () + 1));
  for (int i = omp_invalid_device - 1; i <= omp_get_num_devices () + 1; i++)
    {
      const char *str = omp_get_uid_from_device (i);
      int dev = omp_get_device_from_uid (str);
// __builtin_printf("%i -> %s -> %d\n", i, str, dev);
      if (i < omp_initial_device || i > omp_get_num_devices ())
	{
	  if (dev != omp_invalid_device || str != NULL)
	    abort ();
	  continue;
	}
      if (i == omp_initial_device || i == omp_get_num_devices ())
	{
	  if ((dev != omp_initial_device && dev != omp_get_num_devices ())
	      || str == NULL
	      || strcmp (str, "OMP_INITIAL_DEVICE") != 0) /* GCC impl. choice */
	    abort ();
	  dev = omp_get_num_devices ();
	}
      else if (dev != i || str == NULL || str[0] == '\0')
	abort ();
      strs[dev] = str;
    }

  for (int i = 0; i < omp_get_num_devices (); i++)
    for (int j = i + 1; j <= omp_get_num_devices (); j++)
      if (strcmp (strs[i], strs[j]) == 0)
	abort ();
  free (strs);
  return 0;
}
