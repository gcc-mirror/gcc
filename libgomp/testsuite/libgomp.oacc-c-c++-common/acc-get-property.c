/* Test the `acc_get_property' and '`acc_get_property_string' library
   functions. */
/* { dg-do run } */

#include <openacc.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

int main ()
{
  const char *s;
  size_t v;
  int r;

  /* Verify that the vendor is a proper non-empty string.  */
  s = acc_get_property_string (0, acc_device_default, acc_property_vendor);
  r = !s || !strlen (s);
  if (s)
    printf ("OpenACC vendor: %s\n", s);

  /* For the rest just check that they do not crash.  */
  v = acc_get_property (0, acc_device_default, acc_property_memory);
  if (v)
    printf ("OpenACC total memory: %zd\n", v);
  v = acc_get_property (0, acc_device_default, acc_property_free_memory);
  if (v)
    printf ("OpenACC free memory: %zd\n", v);
  s = acc_get_property_string (0, acc_device_default, acc_property_name);
  if (s)
    printf ("OpenACC name: %s\n", s);
  s = acc_get_property_string (0, acc_device_default, acc_property_driver);
  if (s)
    printf ("OpenACC driver: %s\n", s);

  return r;
}
