/* { dg-do run } */

#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <omp.h>

int
main ()
{
  omp_interop_t interop = omp_interop_none;

  assert (omp_irc_no_value == 1);
  assert (omp_irc_success == 0);
  assert (omp_irc_empty == -1);
  assert (omp_irc_out_of_range == -2);
  assert (omp_irc_type_int == -3);
  assert (omp_irc_type_ptr == -4);
  assert (omp_irc_type_str == -5);
  assert (omp_irc_other == -6);

  /* Check values, including invalid values.  */
  for (omp_interop_rc_t ret_code3 = (omp_interop_rc_t)((int)omp_irc_other-1);
       ret_code3 <= omp_irc_no_value + 1;
       ret_code3 = (omp_interop_rc_t)((int)ret_code3 + 1))
    {
      const char *msg = omp_get_interop_rc_desc (interop, ret_code3);
      if (ret_code3 < omp_irc_other || ret_code3 > omp_irc_no_value)
	/* Assume NULL for an invalid value.  */
	assert (msg == NULL);
      else if (ret_code3 == omp_irc_other)
	/* Likely not to exist in an implementation; esp. not for
	   omp_interop_none. Thus, expect NULL.  */
	/* In GCC, it is used on the device side, only, to complain about
	   omp_get_interop_{int,ptr,str} usage.  */
	assert (msg == NULL);
      else
	/* Assume that omp_get_interop_rc_desc handles all of those and
	   not only omp_irc_empty (and possibly omp_irc_out_of_range),
	   which do occur for omp_interop_none. */
	assert (msg != NULL && strlen (msg) > 5);  /* Some sensible message.  */
    }

  assert (omp_ifr_last >= omp_ifr_hsa);

  for (omp_interop_fr_t fr = omp_ifr_cuda;
       fr <= omp_ifr_last;
       fr = (omp_interop_fr_t)((int)fr +1))
    {
      switch (fr)
	{
	/* Expect the id values from the additional-definition document.  */
	case omp_ifr_cuda:
	  if (fr != 1)
	    abort ();
	  break;
	case omp_ifr_cuda_driver:
	  if (fr != 2)
	    abort ();
	  break;
	case omp_ifr_opencl:
	  if (fr != 3)
	    abort ();
	  break;
	case omp_ifr_sycl:
	  if (fr != 4)
	    abort ();
	  break;
	case omp_ifr_hip:
	  if (fr != 5)
	    abort ();
	  break;
	case omp_ifr_level_zero:
	  if (fr != 6)
	    abort ();
	  break;
	case omp_ifr_hsa:
	  if (fr != 7)
	    abort ();
	  break;
	default:
	  /* Valid, but unexpected to have more interop types.  */
	  abort ();
	}
    }

  assert (omp_ipr_first <= omp_ipr_targetsync
	  && omp_get_num_interop_properties (interop) > omp_ipr_fr_id);

  for (omp_interop_property_t ipr = omp_ipr_first;
       ipr < omp_get_num_interop_properties (interop);
       ipr = (omp_interop_property_t) ((int) ipr + 1))
    {
      /* As interop == omp_interop_none, NULL is permissible;
	 nonetheless, require != NULL for the GCC implementation.  */
      const char *name = omp_get_interop_name (interop, ipr);
      if (name == NULL)
	abort ();
      switch (ipr)
	{
	case omp_ipr_fr_id:
	  if (ipr != -1 || !!strcmp (name, "fr_id"))
	    abort ();
	  break;
	case omp_ipr_fr_name:
	  if (ipr != -2 || !!strcmp (name, "fr_name"))
	    abort ();
	  break;
	case omp_ipr_vendor:
	  if (ipr != -3 || !!strcmp (name, "vendor"))
	    abort ();
	  break;
	case omp_ipr_vendor_name:
	  if (ipr != -4 || !!strcmp (name, "vendor_name"))
	    abort ();
	  break;
	case omp_ipr_device_num:
	  if (ipr != -5 || !!strcmp (name, "device_num"))
	    abort ();
	  break;
	case omp_ipr_platform:
	  if (ipr != -6 || !!strcmp (name, "platform"))
	    abort ();
	  break;
	case omp_ipr_device:
	  if (ipr != -7 || !!strcmp (name, "device"))
	    abort ();
	  break;
	case omp_ipr_device_context:
	  if (ipr != -8 || !!strcmp (name, "device_context"))
	    abort ();
	  break;
	case omp_ipr_targetsync:
	  if (ipr != -9 || !!strcmp (name, "targetsync"))
	    abort ();
	  break;
	default:
	  /* Valid, but unexpected to have more interop types,
	     especially not for interop == omp_interop_none.  */
	  abort ();
	}

      /* As interop == omp_interop_none, expect NULL.  */
      if (omp_get_interop_type_desc (interop, ipr) != NULL)
	abort ();

      omp_interop_rc_t ret_code;
      const char *err;

      ret_code = omp_irc_success;
      omp_intptr_t ival = omp_get_interop_int (interop, ipr, &ret_code);
      assert (ret_code == omp_irc_empty); /* As interop == omp_interop_none.  */
      assert (ival == 0);  /* Implementation choice.  */
      err = omp_get_interop_rc_desc (interop, ret_code);
      assert (err != NULL && strlen (err) > 5);  /* Some sensible message.  */
      assert (!strcmp (err, "provided interoperability object is equal to "
			    "omp_interop_none"));  /* GCC implementation choice.  */
      omp_intptr_t ival4 = omp_get_interop_int (interop, ipr, NULL);
      assert(ival4 == ival);

      ret_code = omp_irc_success;
      void *ptr = omp_get_interop_ptr (interop, ipr, &ret_code);
      assert (ret_code == omp_irc_empty); /* As interop == omp_interop_none.  */
      assert (ptr == NULL);  /* Obvious implementation choice.  */
      err = omp_get_interop_rc_desc (interop, ret_code);
      assert (err != NULL && strlen (err) > 5);  /* Some sensible message.  */
      assert (!strcmp (err, "provided interoperability object is equal to "
			    "omp_interop_none"));  /* GCC implementation choice.  */
      void *ptr4 = omp_get_interop_ptr (interop, ipr, NULL);
      assert (ptr4 == ptr);

      ret_code = omp_irc_success;
      const char *str = omp_get_interop_str (interop, ipr, &ret_code);
      assert (ret_code == omp_irc_empty); /* As interop == omp_interop_none.  */
      assert (str == NULL);  /* Obvious implementation choice.  */
      err = omp_get_interop_rc_desc (interop, ret_code);
      assert (err != NULL && strlen (err) > 5);  /* Some sensible message.  */
      assert (!strcmp (err, "provided interoperability object is equal to "
			    "omp_interop_none"));  /* GCC implementation choice.  */
      const char *str4 = omp_get_interop_str (interop, ipr, NULL);
      assert (str4 == str);
    }

  /* Invalid ipr.  */
  /* Valid are either omp_irc_empty (due to omp_interop_none) or
     omp_irc_out_of_range; assume omp_irc_out_of_range with GCC.  */

  omp_interop_rc_t ret_code2;
  const char *err2;
  omp_intptr_t ival2;
  void *ptr2;
  const char *str2;

  /* omp_ipr_targetsync-1, i.e < lower bound.  */

  ret_code2 = omp_irc_success;
  ival2  = omp_get_interop_int (interop,
	     (omp_interop_property_t) ((int) omp_ipr_targetsync-1), &ret_code2);
  assert (ret_code2 == omp_irc_out_of_range);
  assert (ival2 == 0);  /* Implementation choice.  */
  err2 = omp_get_interop_rc_desc (interop, ret_code2);
  assert (err2 != NULL && strlen (err2) > 5);  /* Some sensible message.  */
  /* GCC implementation choice.  */
  assert (!strcmp (err2, "property ID is out of range"));
  omp_intptr_t ival5  = omp_get_interop_int (interop,
	     (omp_interop_property_t) ((int) omp_ipr_targetsync-1), NULL);
  assert (ival5 == ival2);
#ifdef __cplusplus
  ival5  = omp_get_interop_int (interop,
	     (omp_interop_property_t) ((int) omp_ipr_targetsync-1));
  assert (ival5 == ival2);
#endif

  ret_code2 = omp_irc_success;
  ptr2 = omp_get_interop_ptr (interop,
	     (omp_interop_property_t) ((int) omp_ipr_targetsync-1), &ret_code2);
  assert (ret_code2 == omp_irc_out_of_range);
  assert (ptr2 == NULL);  /* Obvious implementation choice.  */
  err2 = omp_get_interop_rc_desc (interop, ret_code2);
  assert (err2 != NULL && strlen (err2) > 5);  /* Some sensible message.  */
  /* GCC implementation choice.  */
  assert (!strcmp (err2, "property ID is out of range"));
  void *ptr5 = omp_get_interop_ptr (interop,
	     (omp_interop_property_t) ((int) omp_ipr_targetsync-1), NULL);
  assert (ptr5 == ptr2);
#ifdef __cplusplus
  ptr5 = omp_get_interop_ptr (interop,
	     (omp_interop_property_t) ((int) omp_ipr_targetsync-1));
  assert (ptr5 == ptr2);
#endif

  ret_code2 = omp_irc_success;
  str2 = omp_get_interop_str (interop,
	     (omp_interop_property_t) ((int) omp_ipr_targetsync-1), &ret_code2);
  assert (ret_code2 == omp_irc_out_of_range);
  assert (str2 == NULL);  /* Obvious implementation choice.  */
  err2 = omp_get_interop_rc_desc (interop, ret_code2);
  assert (err2 != NULL && strlen (err2) > 5);  /* Some sensible message.  */
  /* GCC implementation choice.  */
  assert (!strcmp (err2, "property ID is out of range"));
  const char *str5 = omp_get_interop_str (interop,
	     (omp_interop_property_t) ((int) omp_ipr_targetsync-1), NULL);
  assert (str2 == str5);
#ifdef __cplusplus
  str5 = omp_get_interop_str (interop,
	     (omp_interop_property_t) ((int) omp_ipr_targetsync-1));
  assert (str2 == str5);
#endif

  /* omp_get_num_interop_properties (), i.e > upper bound.  */

  ret_code2 = omp_irc_success;
  ival2 = omp_get_interop_int (interop,
	    (omp_interop_property_t) omp_get_num_interop_properties (interop),
	    &ret_code2);
  assert (ret_code2 == omp_irc_out_of_range);
  assert (ival2 == 0);  /* Implementation choice.  */
  err2 = omp_get_interop_rc_desc (interop, ret_code2);
  assert (err2 != NULL && strlen (err2) > 5);  /* Some sensible message.  */
  /* GCC implementation choice.  */
  assert (!strcmp (err2, "property ID is out of range"));

  ret_code2 = omp_irc_success;
  ptr2 = omp_get_interop_ptr (interop,
	    (omp_interop_property_t) omp_get_num_interop_properties (interop),
	    &ret_code2);
  assert (ret_code2 == omp_irc_out_of_range);
  assert (ptr2 == NULL);  /* Obvious implementation choice.  */
  err2 = omp_get_interop_rc_desc (interop, ret_code2);
  assert (err2 != NULL && strlen (err2) > 5);  /* Some sensible message.  */
  /* GCC implementation choice.  */
  assert (!strcmp (err2, "property ID is out of range"));

  ret_code2 = omp_irc_success;
  str2 = omp_get_interop_str (interop,
	    (omp_interop_property_t) omp_get_num_interop_properties (interop),
	    &ret_code2);
  assert (ret_code2 == omp_irc_out_of_range);
  assert (str2 == NULL);  /* Obvious implementation choice.  */
  err2 = omp_get_interop_rc_desc (interop, ret_code2);
  assert (err2 != NULL && strlen (err2) > 5);  /* Some sensible message. */
  /* GCC implementation choice.  */
  assert (!strcmp (err2, "property ID is out of range"));

  return 0;
}
