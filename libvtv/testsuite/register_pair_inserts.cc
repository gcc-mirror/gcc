#include "vtv_utils.h"
#include "vtv_rts.h"

/* This configuration will test mostly inserting of new elements since 
   the number of repeats is 1. It should also do a lot of rehashing */

/* This test case may fail depending on the system configuration.
   Check the value of  /proc/sys/vm/max_map_count and fix by doing
   Ex: sudo sh -c  "echo 131060 > /proc/sys/vm/max_map_count" */

#define NUM_MAPS 40000
#define ELEMENTS_PER_MAP 100
#define NUM_REPEATS 1

/* This variable has to be put in rel.ro */
void * maps[NUM_MAPS] VTV_PROTECTED_VAR;

struct fake_vt {
  void * fake_vfp [4];
};
void * fake_vts [NUM_MAPS * ELEMENTS_PER_MAP];


int main()
{
  __VLTChangePermission(__VLTP_READ_WRITE);

  for (int k = 0; k < NUM_REPEATS; k++)
    {
      int curr_fake_vt = 0;
      for (int i = 0; i < NUM_MAPS; i++)
	for (int j = 0; j < ELEMENTS_PER_MAP; j++)
	  {
#ifdef VTV_DEBUG
	    __VLTRegisterPairDebug(&maps[i], &fake_vts[curr_fake_vt], 0, 0, 0, 0);
#else
	    __VLTRegisterPair(&maps[i], &fake_vts[curr_fake_vt]);
#endif
	    curr_fake_vt++;
	  }
    }

  __VLTChangePermission(__VLTP_READ_ONLY);

  return 0;
}
