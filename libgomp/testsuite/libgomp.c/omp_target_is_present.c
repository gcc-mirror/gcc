// { dg-do run }

// Check mainly omp_target_is_present - but also some related functions

/* omp_target_is_present is only 1 if device == host or when there is corresponding
   storage on the device, which implies ptr != omp_get_mapped_ptr (ptr, dev).

   Note that a NULL ptr is regarded as not being present.  */

#include <omp.h>

#ifndef REQ_SELF_MAPS
  #define REQ_SELF_MAPS 0
#endif

// FIXME: change enter to link clause for gLink, once implemented

int gEnter = 3, gLink = 4, gLocal = 5;
#pragma omp declare target enter(gEnter) link(gLink) enter(gLocal)

void check_routines (int dev)
{
  int A = 1, B = 2;

  int dev2 = dev;
  if (dev2 == omp_default_device)
    dev2 = omp_get_default_device ();

  bool initial_dev = dev2 == omp_initial_device || dev2 == omp_get_num_devices();
  bool self_mapping = false;
  bool invalid_dev = dev == omp_invalid_device;
  if (!invalid_dev && !initial_dev)
    {
      #pragma omp target map(to: self_mapping) device(dev)
	self_mapping = true;
      if (REQ_SELF_MAPS && !self_mapping)
	__builtin_abort ();
    }

  if (omp_target_is_present (nullptr, dev) != 0)
    __builtin_abort ();

  if (omp_target_is_accessible (nullptr, 0, dev) != 0)
    __builtin_abort ();


  if (invalid_dev)
    return; // Will otherwise fail with: libgomp: omp_invalid_device encountered


  if (omp_target_is_present (&A, dev) != initial_dev)
    __builtin_abort ();

  if (omp_target_is_present (&gEnter, dev) != (!invalid_dev && (initial_dev || !REQ_SELF_MAPS)))
    __builtin_abort ();

  if (omp_target_is_present (&gLink, dev) != (!invalid_dev && (initial_dev || !REQ_SELF_MAPS)))
    __builtin_abort ();

  if (omp_target_is_present (&gLocal, dev) != (!invalid_dev && (initial_dev || !REQ_SELF_MAPS)))
    __builtin_abort ();

  int *ptr = (int*) 0xDEEDBEEF;
  if (!invalid_dev)
    {
      #pragma omp target enter data map(to: A) device(dev)
      #pragma omp target enter data map(to: gEnter) device(dev)
      #pragma omp target enter data map(to: gLink) device(dev)
      #pragma omp target enter data map(to: gLocal) device(dev)

      ptr = omp_target_alloc (sizeof (int), dev);
      if (ptr == nullptr || !omp_target_is_accessible (ptr, sizeof (int), dev))
	__builtin_abort ();
    }

  // Invalid
  if ((initial_dev || invalid_dev) && omp_target_associate_ptr (ptr, ptr, sizeof (int), 0, dev) == 0)
    __builtin_abort ();
  if ((initial_dev || invalid_dev) && omp_target_associate_ptr (((char*)ptr) + 2, ptr, sizeof (int)-2, 2, dev) == 0)
    __builtin_abort ();

  // Should yield 0/success except for self mapping, host or invalid device
  // use !! to convert the result to 0 or 1, as errors can also be, e.g. EINVAL
  if (!!omp_target_associate_ptr (&B, ptr, sizeof (int), 0, dev)
      != (self_mapping || initial_dev || invalid_dev))
    __builtin_abort ();

  // Try again, should still work as it is the same pointer
  if (!!omp_target_associate_ptr (&B, ptr, sizeof (int), 0, dev)
      != (self_mapping || initial_dev || invalid_dev))
    __builtin_abort ();

  if (!!omp_target_is_present (&A, dev)
      != (initial_dev || (!self_mapping && !invalid_dev)))
    __builtin_abort ();

  if (!!omp_target_is_present (&B, dev)
      != (initial_dev || (!self_mapping && !invalid_dev)))
    __builtin_abort ();

  if (!!omp_target_is_present (&gEnter, dev)
      != (initial_dev || (!self_mapping && !invalid_dev)))
    __builtin_abort ();

  if (!!omp_target_is_present (&gLink, dev)
      != (initial_dev || (!self_mapping && !invalid_dev)))
    __builtin_abort ();

  if (!!omp_target_is_present (&gLocal, dev)
      != (initial_dev || (!self_mapping && !invalid_dev)))
    __builtin_abort ();

  int *ptr2 = omp_get_mapped_ptr (&A, dev);
  if (initial_dev)
    {
      if (ptr2 != &A)
	__builtin_abort ();
    }
  else if (invalid_dev || self_mapping)
    {
      if (ptr2 != nullptr)
	__builtin_abort ();
    }
  else if (ptr2 == &A || ptr2 == nullptr)
    __builtin_abort ();

  ptr2 = omp_get_mapped_ptr (&B, dev);
  if (initial_dev)
    {
      if (ptr2 != &B)
	__builtin_abort ();
    }
  else if (invalid_dev || self_mapping)
    {
      if (ptr2 != nullptr)
	__builtin_abort ();
    }
  else if (ptr2 != ptr)
    __builtin_abort ();

  ptr2 = omp_get_mapped_ptr (&gEnter, dev);
  if (initial_dev)
    {
      if (ptr2 != &gEnter)
	__builtin_abort ();
    }
  else if (invalid_dev || self_mapping)
    {
      if (ptr2 != nullptr)
	__builtin_abort ();
    }
  else if (ptr2 == &gEnter || ptr2 == nullptr)
    __builtin_abort ();

  ptr2 = omp_get_mapped_ptr (&gLink, dev);
  if (initial_dev)
    {
      if (ptr2 != &gLink)
	__builtin_abort ();
    }
  else if (invalid_dev || self_mapping)
    {
      if (ptr2 != nullptr)
	__builtin_abort ();
    }
  else if (ptr2 == ptr || ptr2 == nullptr)
    __builtin_abort ();

  ptr2 = omp_get_mapped_ptr (&gLocal, dev);
  if (initial_dev)
    {
      if (ptr2 != &gLocal)
	__builtin_abort ();
    }
  else if (invalid_dev || self_mapping)
    {
      if (ptr2 != nullptr)
	__builtin_abort ();
    }
  else if (ptr2 == &gLocal || ptr2 == nullptr)
    __builtin_abort ();

  if (!!omp_target_disassociate_ptr (&B, dev)
      != (self_mapping || initial_dev || invalid_dev))
    __builtin_abort ();
  if (!invalid_dev)
    {
      omp_target_free (ptr, dev);
      #pragma omp target exit data map(release: A) device(dev)
      #pragma omp target exit data map(release: gLink) device(dev)
      #pragma omp target exit data map(release: gEnter) device(dev)
      #pragma omp target exit data map(release: gLocal) device(dev)
    }
}

int main()
{
  for (int dev = omp_initial_device; dev <= omp_get_num_devices(); dev++)
    check_routines (dev);

  check_routines (omp_invalid_device);
  check_routines (omp_default_device);

  for (int dev = omp_initial_device; dev <= omp_get_num_devices(); dev++)
    {
      omp_set_default_device (dev);
      check_routines (omp_default_device);
    }
}
