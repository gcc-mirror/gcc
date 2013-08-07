#include "vtv_utils.h"
#include "vtv_rts.h"
#include "pthread.h"
#include <stdio.h>


/* Multi-threaded test for calls to RegisterPair */

/* This configuration will test mostly inserting of elements that are already inserted since 
   the number of repeats is 10 */

/* This test case may fail depending on the system configuration.
   Check the value of  /proc/sys/vm/max_map_count and fix by doing
   Ex: sudo sh -c  "echo 131060 > /proc/sys/vm/max_map_count" */

#define NUM_MAPS 200
#define ELEMENTS_PER_MAP 100
#define NUM_REPEATS 10

#define NUM_THREADS 9

/* This variable has to be put in rel.ro */
void * volatile maps[NUM_MAPS] VTV_PROTECTED_VAR;

struct fake_vt {
  void * fake_vfp [4];
};
void * fake_vts [NUM_MAPS * ELEMENTS_PER_MAP];

volatile int current_map = -1;
volatile int threads_completed_it = 0;

void * do_register_pairs(void *)
{
  for (int k = 0; k < NUM_REPEATS; k++)
    {
      int curr_fake_vt = 0;
      for (int i = 0; i < NUM_MAPS; i++)
	{
	  while (current_map < (k*NUM_MAPS + i))
	    ;

	  __VLTChangePermission(__VLTP_READ_WRITE);

	  for (int j = 0; j < ELEMENTS_PER_MAP; j++)
	    {
#ifdef VTV_DEBUG
	      __VLTRegisterPair((void **) &maps[i], &fake_vts[curr_fake_vt], 0, 0, 0, 0);
#else
	      __VLTRegisterPair((void **) &maps[i], &fake_vts[curr_fake_vt]);
#endif
	      __VLTVerifyVtablePointer((void **) &maps[i],  &fake_vts[curr_fake_vt]);
	      curr_fake_vt++;
	    }

	  __VLTChangePermission(__VLTP_READ_ONLY);

	  int old_value;
	  do {
	    old_value = threads_completed_it;
	  } while (!__sync_bool_compare_and_swap(&threads_completed_it, old_value, old_value + 1));

	  if (old_value == (NUM_THREADS - 1)) // Only one thread will do this.
	    {
	      threads_completed_it = 0;
	      printf("%c%d", 13, current_map + 1);
	      fflush(stdout);
	      current_map++;
	    }
	}
    }

  return NULL;
}


int main()
{
  pthread_t thread_ids[NUM_THREADS];
 
  for (int t = 0; t < NUM_THREADS; t++ )
    if (pthread_create(&thread_ids[t], NULL, do_register_pairs, NULL) != 0)
      {
	printf("failed pthread_create\n");
	exit(1);
      }

  current_map = 0; // start the work on the other threads

  for (int t = 0; t < NUM_THREADS; t++)
    if (pthread_join(thread_ids[t], NULL) != 0)
      { 
	printf("failed pthread_join\n");
	exit(2);
      }

  printf("\n");


  
  return 0;
}
