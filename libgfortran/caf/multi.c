
/* Pthread based coarray implementation of GNU Fortran Coarray Library
   Copyright (C) 2011-2018 Free Software Foundation, Inc.
   Contributed by Jerry DeLisle <jvdelisle@gcc.gnu.org>, 
    		  Thomas Koenig <tkoenig@gcc.gnu.org>, 
    		  Nicolas Koenig <koenigni@gcc.gnu.org>

This file is part of the GNU Fortran Coarray Runtime Library (libcaf).

Libcaf is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Libcaf is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "libcaf.h"
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>

/* Currently compile programs which call this with

$ gfortran -static-libgfortran -fcoarray=lib foo.f90 -pthread -lcaf_multi

*/

int main(int argc, char **argv);

/* Types.  */

typedef struct {
  int this_image;
  int argc;
  char **argv;
} init_args;

typedef struct {
  void **base_array;
} caf_multi_token_t;

typedef struct cond_t {
  pthread_cond_t cond;
  int signalled;
  pthread_mutex_t mutex;
} cond_t;

/* Static variables.  */

pthread_barrier_t sync_all_barrier;
pthread_t *tidlist;

static int *sim;
static cond_t *cim;

static int cond_init(cond_t *cond);

static void *
init_image (void *p)
{
  init_args args = *(init_args *) p; 
  free(p);

  _gfortrani_caf_this_image = args.this_image;

  pthread_barrier_wait (&sync_all_barrier);

 /* XXX: Must be called since there is no other way to set the
 options for the images since _gfortran_set_option is called after
 _gfortran_caf_init and options is a local variable in main. It would
 be better to switch to calling MAIN__ once we have the new
 interface.  */

  main (args.argc, args.argv);

  return NULL;
}

void
_gfortran_caf_init (int *argcptr, char ***argvptr)
{
  init_args *args;
  
  /* Ensure the function is only executed once after calling main
     recursively.  */
  if (_gfortrani_caf_this_image > 0)
    return; 

  pthread_barrier_init (&sync_all_barrier, NULL, caf_num_images);  
  
  tidlist = calloc (caf_num_images, sizeof(pthread_t));
  sim = calloc(caf_num_images * caf_num_images, sizeof(int));
  cim = calloc(caf_num_images * caf_num_images, sizeof(cond_t));

  for (int i = 0; i < caf_num_images; i++)
    cond_init(cim + i);

  for (int i = 1; i < caf_num_images; i++) {
    args = malloc (sizeof (init_args)); 
    args->this_image = i;
    args->argc = *argcptr;
    args->argv = *argvptr;
    pthread_create (tidlist + i, NULL, init_image, args);
  }
  
  tidlist[0] = pthread_self ();
  _gfortrani_caf_this_image = 0;

  pthread_barrier_wait (&sync_all_barrier);
}

/* Implementation of caf_register - so far only good enough to allow for
   CRITICAL / END CRITICAL.  */

void
_gfortran_caf_register (size_t size, caf_register_t type, caf_token_t *token,
			gfc_descriptor_t *data, int *stat, char *errmsg,
			size_t errmsg_len)
{
  if (type == CAF_REGTYPE_CRITICAL)
    {
      pthread_mutex_t *mutex, **mp;
      mutex = malloc (sizeof *mutex);
      pthread_mutex_init (mutex, NULL);
      mp = token;
      *mp = mutex;
    }
}

/* Implement a lock.  */

void
_gfortran_caf_lock (caf_token_t token, size_t index,
		    int image_index __attribute__ ((unused)),
		    int *aquired_lock, int *stat, char *errmsg, size_t errmsg_len)
{
  pthread_mutex_t *mutex;
  mutex = token;
  pthread_mutex_lock (mutex);
}

/* Implement an unlock.  */

void
_gfortran_caf_unlock (caf_token_t token, size_t index,
		      int image_index __attribute__ ((unused)),
		      int *stat, char *errmsg, size_t errmsg_len)
{
  pthread_mutex_t *mutex;
  mutex = token;
  pthread_mutex_unlock (mutex);
}

/*
Doesn't work
void
_gfortran_caf_register (size_t size, caf_register_t type, caf_token_t *token,
			gfc_descriptor_t *data, int *stat, char *errmsg,
			size_t errmsg_len) {
  caf_multi_token_t **t = (caf_multi_token_t **) token;
  void *temp;
  if (type == CAF_REGTYPE_COARRAY_STATIC)
    {
      static pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;
      pthread_mutex_lock(&lock);
      if(!*t) 
	{
	  *t = malloc(sizeof(caf_multi_token_t)); 
	  (*t)->base_array = malloc(get_num_images()*size);
	}
      data->base_addr = (*t)->base_array+this_image*size;
      pthread_mutex_unlock(&lock);
    }
  else
    printf("Hm");
}

void
_gfortran_caf_send (caf_token_t token, size_t offset,
		    int image_index, gfc_descriptor_t *dest,
		    caf_vector_t *dst_vector, gfc_descriptor_t *src, 
	 	    int dst_kind, int src_kind, bool may_require_tmp, 
		    int *stat)
{
  caf_multi_token_t *t = (caf_multi_token_t *) token;
  *(int *) src->base_addr = ((int *) t->base_array[image_index-1])[offset];
}

void
_gfortran_caf_get (caf_token_t token, size_t offset,
		   int image_index __attribute__ ((unused)),
		   gfc_descriptor_t *src,
		   caf_vector_t *src_vector __attribute__ ((unused)),
		   gfc_descriptor_t *dest, int src_kind, int dst_kind,
		   bool may_require_tmp, int *stat)
{
  caf_multi_token_t *t = (caf_multi_token_t *) token;
  ((int *) t->base_array[image_index-1])[offset] = *(int *) src->base_addr;
}
*/

void
_gfortran_caf_finalize(void) 
{ 
  int i;
  if (_gfortrani_caf_this_image != 0)
    pthread_exit(NULL);
  for (i = 1; i < caf_num_images; i++)
    pthread_join(tidlist[i], NULL);
}

int
_gfortran_caf_this_image(int distance) {
  return _gfortrani_caf_this_image+1;
}


int
_gfortran_caf_num_images (int distance __attribute__ ((unused)),
			  int failed __attribute__ ((unused)))
{
  return caf_num_images;
}

static int
cond_init(cond_t *cond)
{
  pthread_mutex_init (&cond->mutex, NULL);
  pthread_cond_init (&cond->cond, NULL);
  cond->signalled = 0;
  return 0;
}

static int
cond_wait (cond_t * cond)
{
  while (!cond->signalled)
    pthread_cond_wait (&cond->cond, &cond->mutex);

  cond->signalled = 0;
  pthread_mutex_unlock (&cond->mutex);
  return 0;
}

static int
cond_signal (cond_t *cond)
{
  cond->signalled = 1;
  pthread_cond_signal (&cond->cond);
  pthread_mutex_unlock (&cond->mutex);
  return 0;
}

#define A(i,j) (sim[(i) + caf_num_images * (j)])

void
_gfortran_caf_sync_images (int count, int *images,
			   int *stat __attribute__ ((unused)),
			   char *errmsg __attribute__ ((unused)),
			   size_t errmsg_len __attribute__ ((unused)))
{
  pthread_mutex_t *my_mutex;

  if (count < 0)
    count = caf_num_images;

  for (int i=0; i < count; i++)
    {
      int other_img = images == NULL ? i : images[i] - 1;
      if (other_img != _gfortrani_caf_this_image)
	{
	  cond_t *other_thread = cim + other_img;
	  pthread_mutex_lock (&other_thread->mutex);
	  A(_gfortrani_caf_this_image, other_img) ++;
	  cond_signal(other_thread);
	}
    }

  while (1)
    {
      int x;
      int do_wait = 0;

      my_mutex =  &(cim[_gfortrani_caf_this_image].mutex);
      pthread_mutex_lock (my_mutex);

      for (int i = 0; i < count; i++)
	{
	  int other_img = images == NULL ? i : images[i] - 1;
	  if (other_img != _gfortrani_caf_this_image)
	    {
	      x = A(other_img, _gfortrani_caf_this_image)
		< A(_gfortrani_caf_this_image, other_img);
	      if (x)
		{
		  do_wait = 1;
		  break;
		}
	    }
	}

      if (do_wait)
	  cond_wait(cim + _gfortrani_caf_this_image);
      else
	break;
    }
  pthread_mutex_unlock (my_mutex);
}

#undef A

/* Probably has a race condition, if a thread reaches the barrier
   before all have left, but I'm not certain how that works.  */

void
_gfortran_caf_sync_all (int *stat,
			char *errmsg __attribute__ ((unused)),
			size_t errmsg_len __attribute__ ((unused)))
{
  pthread_barrier_wait(&sync_all_barrier);
}
