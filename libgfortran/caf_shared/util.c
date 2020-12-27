#include "libgfortran.h"
#include "util.h"
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include <limits.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <assert.h>
#include <errno.h>

/* Shared Memory objects live in their own namspace (usually found under
 * /dev/shm/), so the "/" is needed.  It is for some reason impossible to
 * create a shared memory object without name.
 *
 * Apple, for some reason, only allows 31 characters in memfd names, so we need
 * to make the name a bit shorter in that case.  */
#ifndef __APPLE__
#define MEMOBJ_NAME "/gfortran_coarray_memfd"
#define CUT_INT(x) (x)
#else
#define MEMOBJ_NAME "/gfccas_"
#define CUT_INT(x) (x % 100000)
#endif


size_t
alignto (size_t size, size_t align)
{
  return align * ((size + align - 1) / align);
}

size_t pagesize;

size_t
round_to_pagesize (size_t s)
{
  return alignto (s, pagesize);
}

size_t
next_power_of_two (size_t size)
{
  assert (size);
  return 1 << (PTR_BITS - __builtin_clzl (size - 1));
}

void
initialize_shared_mutex (pthread_mutex_t *mutex)
{
  pthread_mutexattr_t mattr;
  pthread_mutexattr_init (&mattr);
  pthread_mutexattr_setpshared (&mattr, PTHREAD_PROCESS_SHARED);
  pthread_mutex_init (mutex, &mattr);
  pthread_mutexattr_destroy (&mattr);
}

void
initialize_shared_condition (pthread_cond_t *cond)
{
  pthread_condattr_t cattr;
  pthread_condattr_init (&cattr);
  pthread_condattr_setpshared (&cattr, PTHREAD_PROCESS_SHARED);
  pthread_cond_init (cond, &cattr);
  pthread_condattr_destroy (&cattr);
}

int
get_shmem_fd (void)
{
  char buffer[1 << 10];
  int fd, id;
  id = random ();
  do
    {
      snprintf (buffer, sizeof (buffer), MEMOBJ_NAME "_%u_%d",
		(unsigned int)getpid (), CUT_INT(id++));
      fd = shm_open (buffer, O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);
      if (fd == -1 && errno != EEXIST)
	{
	  perror("Failed to create the memfd");
	  exit(1);
	}
    }
  while (fd == -1);
  shm_unlink (buffer);
  return fd;
}

bool
pack_array_prepare (pack_info *restrict pi,
		    const gfc_array_char *restrict source)
{
  index_type dim;
  bool packed;
  index_type span;
  index_type type_size;
  index_type ssize;

  dim = GFC_DESCRIPTOR_RANK (source);
  type_size = GFC_DESCRIPTOR_SIZE (source);
  ssize = type_size;

  pi->num_elem = 1;
  packed = true;
  span = source->span != 0 ? source->span : type_size;
  for (index_type n = 0; n < dim; n++)
    {
      pi->stride[n] = GFC_DESCRIPTOR_STRIDE (source, n) * span;
      pi->extent[n] = GFC_DESCRIPTOR_EXTENT (source, n);
      if (pi->extent[n] <= 0)
	{
	  /* Do nothing.  */
	  packed = 1;
	  pi->num_elem = 0;
	  break;
	}

      if (ssize != pi->stride[n])
	packed = 0;

      pi->num_elem *= pi->extent[n];
      ssize *= pi->extent[n];
    }

  return packed;
}

void
pack_array_finish (pack_info *const restrict pi,
		   const gfc_array_char *const restrict source,
		   char *restrict dest)
{
  index_type dim;
  const char *restrict src;

  index_type size;
  index_type stride0;
  index_type count[GFC_MAX_DIMENSIONS];

  dim = GFC_DESCRIPTOR_RANK (source);
  src = source->base_addr;
  stride0 = pi->stride[0];
  size = GFC_DESCRIPTOR_SIZE (source);

  memset (count, '\0', sizeof (count) * dim);
  while (src)
    {
      /* Copy the data.  */
      memcpy (dest, src, size);
      /* Advance to the next element.  */
      dest += size;
      src += stride0;
      count[0]++;
      /* Advance to the next source element.  */
      index_type n = 0;
      while (count[n] == pi->extent[n])
	{
	  /* When we get to the end of a dimension, reset it and increment
	     the next dimension.  */
	  count[n] = 0;
	  /* We could precalculate these products, but this is a less
	     frequently used path so probably not worth it.  */
	  src -= pi->stride[n] * pi->extent[n];
	  n++;
	  if (n == dim)
	    {
	      src = NULL;
	      break;
	    }
	  else
	    {
	      count[n]++;
	      src += pi->stride[n];
	    }
	}
    }
}

void
unpack_array_finish (pack_info *const restrict pi,
		     const gfc_array_char *restrict d,
		     const char *restrict src)
{
  index_type stride0;
  char *restrict dest;
  index_type size;
  index_type count[GFC_MAX_DIMENSIONS];
  index_type dim;

  size = GFC_DESCRIPTOR_SIZE (d);
  stride0 = pi->stride[0];
  dest = d->base_addr;
  dim = GFC_DESCRIPTOR_RANK (d);

  memset (count, '\0', sizeof (count) * dim);
  while (dest)
    {
      memcpy (dest, src, size);
      src += size;
      dest += stride0;
      count[0]++;
      index_type n = 0;
      while (count[n] == pi->extent[n])
	{
	  count[n] = 0;
	  dest -= pi->stride[n] * pi->extent[n];
	  n++;
	  if (n == dim)
	    {
	      dest = NULL;
	      break;
	    }
	  else
	    {
	      count[n]++;
	      dest += pi->stride[n];
	    }
	}
    }
}
