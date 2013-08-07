#include <stdlib.h>
#include <dlfcn.h>
#include <stdio.h>

#include "vtv_utils.h"
#include "vtv_rts.h"
#include "pthread.h"

#define NUM_REPEATS 10
#define NUM_THREADS 10
#define NUM_SOS 100
#define NUM_SOS_PER_THREAD (NUM_SOS/NUM_THREADS)

typedef void (*voidfn)(void);

int failures = 0;

void
__vtv_verify_fail (void **data_set_ptr, const void *vtbl_pointer)
{
  failures++;
  return;
}


void do_dlopen(int so_num)
{
  char so_name [sizeof("soxxx.so")];
  sprintf(so_name, "so%d.so", so_num);
  //  printf("dl-opening %s\n", so_name);
  void * dlhandle = dlopen(so_name, RTLD_NOW);
  if (!dlhandle)
    {
      fprintf(stderr, "dlopen so:%s error: %s\n", so_name, dlerror());
      exit(1);
    }
  char so_entry [sizeof("so_entry_xxx")];
  sprintf(so_entry, "so_entry_%d", so_num);
  voidfn so_entry_fn = (voidfn)dlsym(dlhandle, so_entry);
  if (!so_entry_fn)
    {
      fprintf(stderr, "so:%s dlsym error: %s\n", so_name, dlerror());
      exit(2);
    }

  so_entry_fn();

  dlclose(dlhandle);
}

volatile int threads_completed_it = 0;
volatile int current_wave = -1;

void * do_dlopens(void * ptid)
{
  for (int k = 0; k < NUM_REPEATS; k++)
    {

      for (int i = 0; i < NUM_SOS_PER_THREAD; i++)
	{
	  while (current_wave < (k*NUM_SOS_PER_THREAD + i)) /* from 0 to 99 */
	    ;

          do_dlopen((NUM_SOS_PER_THREAD * *(int *)ptid) + i);

	  int old_value;
	  do {
	    old_value = threads_completed_it;
	  } while (!__sync_bool_compare_and_swap(&threads_completed_it, old_value, old_value + 1));

	  if (old_value == (NUM_THREADS - 1)) // Only one thread will do this.
	    {
	      threads_completed_it = 0;
	      printf("%c%d", 13, current_wave + 1);
	      fflush(stdout);
	      current_wave++;
	    }
	}
    }

  return NULL;
}


int main()
{
  pthread_t thread_ids[NUM_THREADS];
  int thread_nids[NUM_THREADS];

  for (int t = 0; t < NUM_THREADS; t++ )
  {
    thread_nids[t] = t;
    if (pthread_create(&thread_ids[t], NULL, do_dlopens, &thread_nids[t]) != 0)
      {
	printf("failed pthread_create\n");
	exit(1);
      }
  }

  current_wave = 0; // start the work on the other threads

  for (int t = 0; t < NUM_THREADS; t++)
    if (pthread_join(thread_ids[t], NULL) != 0)
      { 
	printf("failed pthread_join\n");
	exit(2);
      }

  printf("\n");

  return 0;
}
