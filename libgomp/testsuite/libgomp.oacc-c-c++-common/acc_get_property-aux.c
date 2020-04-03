/* Auxiliary functions for acc_get_property tests */
/* { dg-do compile  { target skip-all-targets } } */

#include <openacc.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>


void
expect_device_string_properties (acc_device_t dev_type, int dev_num,
				 const char* expected_vendor,
				 const char* expected_name,
				 const char* expected_driver)
{
  const char *vendor = acc_get_property_string (dev_num, dev_type,
						acc_property_vendor);
  if (strcmp (vendor, expected_vendor))
    {
      fprintf (stderr, "Expected acc_property_vendor to equal \"%s\", "
	       "but was \"%s\".\n", expected_vendor, vendor);
      abort ();
    }

  const char *name = acc_get_property_string (dev_num, dev_type,
					      acc_property_name);
  if (strcmp (name, expected_name))
    {
      fprintf(stderr, "Expected acc_property_name to equal \"%s\", "
	      "but was \"%s\".\n", expected_name, name);
      abort ();
    }

  const char *driver = acc_get_property_string (dev_num, dev_type,
						acc_property_driver);
  if (strcmp (expected_driver, driver))
    {
      fprintf (stderr, "Expected acc_property_driver to equal %s, "
	       "but was %s.\n", expected_driver, driver);
      abort ();
    }

  int unknown_property = 16058;
  size_t v = acc_get_property (dev_num, dev_type, (acc_device_property_t)unknown_property);
  if (v != 0)
    {
      fprintf (stderr, "Expected value of unknown numeric property to equal 0, "
	       "but was %zu.\n", v);
      abort ();
    }

  int unknown_property2 = -16058;
  const char *s = acc_get_property_string (dev_num, dev_type, (acc_device_property_t)unknown_property2);
  if (s != NULL)
    {
      fprintf (stderr, "Expected value of unknown string property to be NULL, "
	       "but was %s.\n", s);
      abort ();
    }
}

void
expect_device_memory (acc_device_t dev_type, int dev_num,
		      size_t expected_total_memory)
{

  size_t total_mem = acc_get_property (dev_num, dev_type,
				       acc_property_memory);

  if (total_mem != expected_total_memory)
    {
      fprintf (stderr, "Expected acc_property_memory to equal %zu, "
	       "but was %zu.\n", expected_total_memory, total_mem);
      abort ();
    }

  size_t free_mem = acc_get_property (dev_num, dev_type,
				      acc_property_free_memory);
  if (free_mem > total_mem)
    {
      fprintf (stderr, "Expected acc_property_free_memory <= acc_property_memory"
	       ", but free memory was %zu and total memory was %zu.\n",
	       free_mem, total_mem);
      abort ();
    }
}

void
expect_device_properties (acc_device_t dev_type, int dev_num,
			  size_t expected_total_memory,
			  const char* expected_vendor,
			  const char* expected_name,
			  const char* expected_driver)
{
  expect_device_string_properties (dev_type, dev_num, expected_vendor,
				   expected_name, expected_driver);
  expect_device_memory (dev_type, dev_num, expected_total_memory);
}
