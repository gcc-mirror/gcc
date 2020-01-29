/* Test the `acc_get_property' and '`acc_get_property_string' library
   functions by printing the results of those functions for all devices
   of all device types mentioned in the OpenACC standard.

   See also acc_get_property.f90. */
/* { dg-do run } */

#include <openacc.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Print the values of the properties of all devices of the given type
   and do basic device independent validation. */

void
print_device_properties (acc_device_t type)
{
  const char *s;
  size_t v;

  int dev_count = acc_get_num_devices (type);

  for (int i = 0; i < dev_count; ++i)
    {
      printf ("  Device %d:\n", i+1);

      s = acc_get_property_string (i, type, acc_property_vendor);
      printf ("    Vendor: %s\n", s);
      if (s == NULL || *s == 0)
	{
	  fprintf (stderr, "acc_property_vendor should not be null or empty.\n");
	  abort ();
	}

      v = acc_get_property (i, type,  acc_property_memory);
      printf ("    Total memory: %zu\n", v);

      v = acc_get_property (i, type, acc_property_free_memory);
      printf ("    Free memory: %zu\n", v);

      s = acc_get_property_string (i, type, acc_property_name);
      printf ("    Name: %s\n", s);
      if (s == NULL || *s == 0)
	{
	  fprintf (stderr, "acc_property_name should not be null or empty.\n");
	  abort ();
	}

      s = acc_get_property_string (i, type, acc_property_driver);
      printf ("    Driver: %s\n", s);
      if (s == NULL || *s == 0)
	{
	  fprintf (stderr, "acc_property_string should not be null or empty.\n");
	  abort ();
	}
    }
}

int
main ()
{
  printf ("acc_device_none:\n");
  /* For completness; not expected to print anything since there
     should be no devices of this type. */
  print_device_properties (acc_device_none);

  printf ("acc_device_default:\n");
  print_device_properties (acc_device_default);

  printf ("acc_device_host:\n");
  print_device_properties (acc_device_host);

  printf ("acc_device_not_host:\n");
  print_device_properties (acc_device_not_host);
}
