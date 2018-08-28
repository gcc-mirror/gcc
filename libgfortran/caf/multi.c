
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

#include"libcaf.h"
#include<pthread.h> // gthreads needed here?
#include<stdlib.h>
#include<stdio.h>

// types

int main(int argc, char **argv);

typedef struct {
  int image_num;
  int argc;
  char **argv;
} init_args;

typedef struct {
  void **base_array;
} caf_multi_token_t;

// static vars

__thread int image_num = -1;
int num_images = -1;
pthread_barrier_t sync_all_barrier;
pthread_t *tidlist;

// functions

static void *
init_image (void *p) {
  init_args args = *(init_args *) p; 
  free(p);

  image_num = args.image_num;

  pthread_barrier_wait(&sync_all_barrier);

  main(args.argc, args.argv); //XXX: Must be called since there is no other
  			      //     way to set the options for the images
			      //     since _gfortran_set_option is called
			      //     after _gfortran_caf_init and options
			      //     is a local variable in main. It would
			      //     be better to switch to calling MAIN
			      //     once we have the new interface.

  return NULL;
}

void
_gfortran_caf_init (int *argcptr, char ***argvptr) {
  if (image_num > 0) // to ensure the function is only
    return	     // executed once after calling main
		     // recursively

  int i;
  int nimages = 4; //XXX
  init_args *args;
  pthread_t tid;
  
  num_images = nimages;
   
  pthread_barrier_init(&sync_all_barrier, NULL, nimages);  
  
  tidlist = malloc(nimages*sizeof(pthread_t));

  for(i = 1; i < num_images; i++) {
    args = malloc(sizeof(init_args)); 
    args->image_num = i;
    args->argc = *argcptr;
    args->argv = *argvptr;
    pthread_create(&tid, NULL, init_image, args);
    tidlist[i] = tid;
  }
  
  tidlist[0] = pthread_self();
  image_num = 0;

  pthread_barrier_wait(&sync_all_barrier);
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
      data->base_addr = (*t)->base_array+image_num*size;
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
  if (image_num != 0)
    pthread_exit(NULL);
  for (i = 1; i<num_images; i++)
    pthread_join(tidlist[i], NULL);
}

int
_gfortran_caf_this_image(int distance) {
  return image_num+1;
}


int
_gfortran_caf_num_images (int distance __attribute__ ((unused)),
			  int failed __attribute__ ((unused)))
{
  return num_images;
}

// Probably has a race condition, if a thread reaches the barrier before
// all have left, but I'm not certain how that works
void
_gfortran_caf_sync_all (int *stat,
			char *errmsg __attribute__ ((unused)),
			size_t errmsg_len __attribute__ ((unused)))
{
  pthread_barrier_wait(&sync_all_barrier);
}
