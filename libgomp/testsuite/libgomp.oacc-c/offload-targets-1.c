/* Test what happens for repeated GOMP_set_offload_targets calls, which happens
   when shared libraries are involved, for example.  As in the libgomp
   testsuite infrastructure, it is difficult to build and link against shared
   libraries, we simulate that by replicating some relevant
   GOMP_set_offload_targets calls.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <openacc.h>
#include "libgomp_g.h"

int main ()
{
  /* Before getting here, GOMP_set_offload_targets already got called via a
     constructor.  */

  bool acc_device_types_requested[_ACC_device_hwm];
  for (int i = 0; i < _ACC_device_hwm; ++i)
    acc_device_types_requested[i] = false;

  /* We're building for only one offload target ("-foffload=[...]") which is
     the following.  */
  const char *offload_target_requested;
  acc_device_t acc_device_type_requested;
#if defined ACC_DEVICE_TYPE_nvidia
  offload_target_requested = ACC_DEVICE_TYPE_nvidia;
  acc_device_type_requested = acc_device_nvidia;
#elif defined ACC_DEVICE_TYPE_gcn
  offload_target_requested = ACC_DEVICE_TYPE_gcn;
  acc_device_type_requested = acc_device_gcn;
#elif defined ACC_DEVICE_TYPE_host
  offload_target_requested = ACC_DEVICE_TYPE_host;
  acc_device_type_requested = acc_device_host;
#else
# error Not ported to this ACC_DEVICE_TYPE
#endif
  acc_device_types_requested[acc_device_type_requested] = true;

#ifdef OFFLOAD_TARGETS_SAME_AGAIN
  /* Call again; will have no noticeable difference.  */
  GOMP_set_offload_targets (offload_target_requested);
#endif

#ifdef OFFLOAD_TARGETS_ADD_EARLY
  /* Request a (non-existing) offloading target (which will result in a
     non-fatal diagnostic).  */
  GOMP_set_offload_targets (OFFLOAD_TARGETS_ADD);
#endif

#ifdef OFFLOAD_TARGETS_SAME_AGAIN
  /* Call again; will have no noticeable difference.  */
  GOMP_set_offload_targets (offload_target_requested);
  char *s;
  {
    size_t len = 3 * (strlen (offload_target_requested) + 1);
# ifdef OFFLOAD_TARGETS_ADD_EARLY
    len += 3 * (strlen (OFFLOAD_TARGETS_ADD) + 1);
# endif
    s = malloc (len);
    if (s == NULL)
      __builtin_abort ();
    size_t len_;
# ifndef OFFLOAD_TARGETS_ADD_EARLY
    len_ = sprintf (s, "%s:%s:%s",
		    offload_target_requested,
		    offload_target_requested,
		    offload_target_requested);
# else
    len_ = sprintf (s, "%s:%s:%s:%s:%s:%s",
		    offload_target_requested,
		    offload_target_requested,
		    OFFLOAD_TARGETS_ADD,
		    OFFLOAD_TARGETS_ADD,
		    offload_target_requested,
		    OFFLOAD_TARGETS_ADD);
# endif
    if (len_ + 1 != len)
      __builtin_abort ();
    GOMP_set_offload_targets (s);
  }
#endif

  /* Calling acc_get_num_devices will implicitly initialize offloading.  */
#if defined OFFLOAD_TARGETS_ADD_EARLY
  fprintf (stderr, "CheCKpOInT1\n");
#endif
  /* acc_device_host is always available.  */
  if ((acc_get_num_devices (acc_device_host) > 0) == false)
    __builtin_abort ();
#if defined OFFLOAD_TARGETS_ADD_EARLY
  fprintf (stderr, "WrONg WAy1\n");
#endif
  for (acc_device_t acc_device_type = acc_device_not_host + 1;
       acc_device_type < _ACC_device_hwm;
       ++acc_device_type)
    {
      /* The requested device type must be available.  Any other device types
	 must not be available.  */
      if ((acc_get_num_devices (acc_device_type) > 0)
	  != acc_device_types_requested[acc_device_type])
	__builtin_abort ();
    }

#ifdef OFFLOAD_TARGETS_SAME_AGAIN
  /* Request the same again; will have no noticeable difference.  */
  GOMP_set_offload_targets (offload_target_requested);
#endif
#if defined OFFLOAD_TARGETS_ADD_LATE
  fprintf (stderr, "CheCKpOInT2\n");
  GOMP_set_offload_targets (OFFLOAD_TARGETS_ADD);
  fprintf (stderr, "WrONg WAy2\n");
#endif
#ifdef OFFLOAD_TARGETS_SAME_AGAIN
  GOMP_set_offload_targets (s);

  /* Implementation defail: OK to "free (s)", in this case.  */
  free (s);
#endif

  return 0;
}
