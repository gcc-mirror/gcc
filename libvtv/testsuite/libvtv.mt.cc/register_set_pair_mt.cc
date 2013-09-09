#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "vtv_utils.h"
#include "vtv_rts.h"
#include "pthread.h"


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

#define KEY_TYPE_FIXED_SIZE 8
void *key_buffer = malloc (17);
typedef char * name_string;
name_string fake_names[NUM_MAPS];

/* This variable has to be put in rel.ro */
void * volatile maps[NUM_MAPS] VTV_PROTECTED_VAR;

struct fake_vt {
  void * fake_vfp [4];
};
void * fake_vts [NUM_MAPS * ELEMENTS_PER_MAP];

volatile int current_map = -1;
volatile int threads_completed_it = 0;

void
generate_names (void)
{
  int i;

  for (i = 0; i < NUM_MAPS; ++i)
    {
      fake_names[i] = (char *) malloc (9 * sizeof (char));
      snprintf (fake_names[i], 9, "name%d", i);
    }
}

static uint32_t
vtv_string_hash(const char *in)
{
  const char *s = in;
  uint32_t h = 0;

  for ( ; *s; ++s)
    h = 5 * h + *s;
  return h;
}

void * do_register_pairs(void *)
{
  for (int k = 0; k < NUM_REPEATS; k++)
    {
      int curr_fake_vt = 0;
      for (int i = 0; i < NUM_MAPS; i++)
	{
	  uint32_t *value_ptr = (uint32_t *) key_buffer;
	  uint32_t len1 = strlen (fake_names[i]);
	  uint32_t hash_value = vtv_string_hash (fake_names[i]);
	  void *temp_array[ELEMENTS_PER_MAP];
	  
	  while (current_map < (k*NUM_MAPS + i))
	    ;
	  
	  __VLTChangePermission(__VLTP_READ_WRITE);
	  
	  *value_ptr = len1;
	  value_ptr++;
	  *value_ptr = hash_value;
	  
	  memcpy ((char *) key_buffer + KEY_TYPE_FIXED_SIZE, fake_names[i],
		  len1);


#ifdef VTV_DEBUG
	  __VLTRegisterPairDebug ((void **) &maps[i], (char *) key_buffer, 128,
				  &fake_vts[curr_fake_vt], "", "");
#else
	  __VLTRegisterPair ((void **) &maps[i], (char *) key_buffer, 128,
			     &fake_vts[curr_fake_vt]);
#endif
	  for (int j = 0; j < ELEMENTS_PER_MAP; j++)
	    {
	      temp_array[j] = &fake_vts[curr_fake_vt];
	      curr_fake_vt++;
	    }

#ifdef VTV_DEBUG
	  __VLTRegisterSetDebug ((void **) &maps[i], (char *) key_buffer, 128, 100,
			       (void **) &temp_array);
#else
	  __VLTRegisterSet ((void **) &maps[i], (char *) key_buffer, 128, 100,
			  (void **) &temp_array);
#endif
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

  generate_names ();
 
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
