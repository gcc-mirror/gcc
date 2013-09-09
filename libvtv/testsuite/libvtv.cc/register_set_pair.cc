#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "vtv_utils.h"
#include "vtv_rts.h"

/* This configuration will test mostly inserting of elements that are already inserted since 
   the number of repeats is 200 */

#define NUM_MAPS 4000
#define ELEMENTS_PER_MAP 100
#define NUM_REPEATS 200

#define KEY_TYPE_FIXED_SIZE 8
void *key_buffer = malloc (17);
typedef char * name_string;
name_string fake_names[NUM_MAPS];

/* This variable has to be put in rel.ro */
void * maps[NUM_MAPS] VTV_PROTECTED_VAR;

struct fake_vt {
  void * fake_vfp [4];
};
void * fake_vts [NUM_MAPS * ELEMENTS_PER_MAP];

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

int main()
{
  __VLTChangePermission(__VLTP_READ_WRITE);

  generate_names ();

  for (int k = 0; k < NUM_REPEATS; k++)
    {
      int curr_fake_vt = 0;
      for (int i = 0; i < NUM_MAPS; i++)
	{
	  uint32_t *value_ptr = (uint32_t *) key_buffer;
	  uint32_t len1 = strlen (fake_names[i]);
	  uint32_t hash_value = vtv_string_hash (fake_names[i]);
	  void *temp_array[ELEMENTS_PER_MAP];

	  *value_ptr = len1;
	  value_ptr++;
	  *value_ptr = hash_value;
	  
	  memcpy ((char *) key_buffer + KEY_TYPE_FIXED_SIZE, fake_names[i],
		  len1);


#ifdef VTV_DEBUG
      __VLTRegisterPairDebug (&maps[i], (char *) key_buffer, 128,
			      &fake_vts[curr_fake_vt], "", "");
#else
      __VLTRegisterPair (&maps[i], (char *) key_buffer, 128,
			 &fake_vts[curr_fake_vt]);
#endif
	for (int j = 0; j < ELEMENTS_PER_MAP; j++)
	  {
	    temp_array[j] = &fake_vts[curr_fake_vt];
	    curr_fake_vt++;
	  }
#ifdef VTV_DEBUG
	__VLTRegisterSetDebug (&maps[i], (char *) key_buffer, 128, 100,
			       (void **) &temp_array);
#else
	__VLTRegisterSet (&maps[i], (char *) key_buffer, 128, 100,
			  (void **) &temp_array);
#endif
	}
    }

  __VLTChangePermission(__VLTP_READ_ONLY);
  
  return 0;
}
