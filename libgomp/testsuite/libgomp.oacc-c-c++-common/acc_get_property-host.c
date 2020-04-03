/* Test the `acc_get_property' and '`acc_get_property_string' library
   functions for the host device. */
/* { dg-additional-sources acc_get_property-aux.c } */
/* { dg-do run } */

#include <openacc.h>
#include <stdio.h>

void expect_device_properties (acc_device_t dev_type, int dev_num,
			       size_t expected_memory,
			       const char* expected_vendor,
			       const char* expected_name,
			       const char* expected_driver);

int
main ()
{
  printf ("Checking acc_device_host device properties\n");
  expect_device_properties (acc_device_host, 0, 0, "GNU", "GOMP", "1.0");
}
