/* Copyright (C) 2006-2015 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* This is a Linux specific implementation of a CPU affinity setting.  */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
#include "libgomp.h"
#include "proc.h"
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#ifdef HAVE_PTHREAD_AFFINITY_NP

#ifndef CPU_ALLOC_SIZE
#define CPU_ISSET_S(idx, size, set) CPU_ISSET(idx, set)
#define CPU_ZERO_S(size, set) CPU_ZERO(set)
#define CPU_SET_S(idx, size, set) CPU_SET(idx, set)
#define CPU_CLR_S(idx, size, set) CPU_CLR(idx, set)
#endif

void
gomp_init_affinity (void)
{
  if (gomp_places_list == NULL)
    {
      if (!gomp_affinity_init_level (1, ULONG_MAX, true))
	return;
    }

  struct gomp_thread *thr = gomp_thread ();
  pthread_setaffinity_np (pthread_self (), gomp_cpuset_size,
			  (cpu_set_t *) gomp_places_list[0]);
  thr->place = 1;
  thr->ts.place_partition_off = 0;
  thr->ts.place_partition_len = gomp_places_list_len;
}

void
gomp_init_thread_affinity (pthread_attr_t *attr, unsigned int place)
{
  pthread_attr_setaffinity_np (attr, gomp_cpuset_size,
			       (cpu_set_t *) gomp_places_list[place]);
}

void **
gomp_affinity_alloc (unsigned long count, bool quiet)
{
  unsigned long i;
  void **ret;
  char *p;

  if (gomp_cpusetp == NULL)
    {
      if (!quiet)
	gomp_error ("Could not get CPU affinity set");
      return NULL;
    }

  ret = malloc (count * sizeof (void *) + count * gomp_cpuset_size);
  if (ret == NULL)
    {
      if (!quiet)
	gomp_error ("Out of memory trying to allocate places list");
      return NULL;
    }

  p = (char *) (ret + count);
  for (i = 0; i < count; i++, p += gomp_cpuset_size)
    ret[i] = p;
  return ret;
}

void
gomp_affinity_init_place (void *p)
{
  cpu_set_t *cpusetp = (cpu_set_t *) p;
  CPU_ZERO_S (gomp_cpuset_size, cpusetp);
}

bool
gomp_affinity_add_cpus (void *p, unsigned long num,
			unsigned long len, long stride, bool quiet)
{
  cpu_set_t *cpusetp = (cpu_set_t *) p;
  unsigned long max = 8 * gomp_cpuset_size;
  for (;;)
    {
      if (num >= max)
	{
	  if (!quiet)
	    gomp_error ("Logical CPU number %lu out of range", num);
	  return false;
	}
      CPU_SET_S (num, gomp_cpuset_size, cpusetp);
      if (--len == 0)
	return true;
      if ((stride < 0 && num + stride > num)
	  || (stride > 0 && num + stride < num))
	{
	  if (!quiet)
	    gomp_error ("Logical CPU number %lu+%ld out of range",
			num, stride);
	  return false;
	}
      num += stride;
    }
}

bool
gomp_affinity_remove_cpu (void *p, unsigned long num)
{
  cpu_set_t *cpusetp = (cpu_set_t *) p;
  if (num >= 8 * gomp_cpuset_size)
    {
      gomp_error ("Logical CPU number %lu out of range", num);
      return false;
    }
  if (!CPU_ISSET_S (num, gomp_cpuset_size, cpusetp))
    {
      gomp_error ("Logical CPU %lu to be removed is not in the set", num);
      return false;
    }
  CPU_CLR_S (num, gomp_cpuset_size, cpusetp);
  return true;
}

bool
gomp_affinity_copy_place (void *p, void *q, long stride)
{
  unsigned long i, max = 8 * gomp_cpuset_size;
  cpu_set_t *destp = (cpu_set_t *) p;
  cpu_set_t *srcp = (cpu_set_t *) q;

  CPU_ZERO_S (gomp_cpuset_size, destp);
  for (i = 0; i < max; i++)
    if (CPU_ISSET_S (i, gomp_cpuset_size, srcp))
      {
	if ((stride < 0 && i + stride > i)
	    || (stride > 0 && (i + stride < i || i + stride >= max)))
	  {
	    gomp_error ("Logical CPU number %lu+%ld out of range", i, stride);
	    return false;
	  }
	CPU_SET_S (i + stride, gomp_cpuset_size, destp);
      }
  return true;
}

bool
gomp_affinity_same_place (void *p, void *q)
{
#ifdef CPU_EQUAL_S
  return CPU_EQUAL_S (gomp_cpuset_size, (cpu_set_t *) p, (cpu_set_t *) q);
#else
  return memcmp (p, q, gomp_cpuset_size) == 0;
#endif
}

bool
gomp_affinity_finalize_place_list (bool quiet)
{
  unsigned long i, j;

  for (i = 0, j = 0; i < gomp_places_list_len; i++)
    {
      cpu_set_t *cpusetp = (cpu_set_t *) gomp_places_list[i];
      bool nonempty = false;
#ifdef CPU_AND_S
      CPU_AND_S (gomp_cpuset_size, cpusetp, cpusetp, gomp_cpusetp);
      nonempty = gomp_cpuset_popcount (gomp_cpuset_size, cpusetp) != 0;
#else
      unsigned long k, max = gomp_cpuset_size / sizeof (cpusetp->__bits[0]);
      for (k = 0; k < max; k++)
	if ((cpusetp->__bits[k] &= gomp_cpusetp->__bits[k]) != 0)
	  nonempty = true;
#endif
      if (nonempty)
	gomp_places_list[j++] = gomp_places_list[i];
    }

  if (j == 0)
    {
      if (!quiet)
	gomp_error ("None of the places contain usable logical CPUs");
      return false;
    }
  else if (j < gomp_places_list_len)
    {
      if (!quiet)
	gomp_error ("Number of places reduced from %ld to %ld because some "
		    "places didn't contain any usable logical CPUs",
		    gomp_places_list_len, j);
      gomp_places_list_len = j;
    }
  return true;
}

bool
gomp_affinity_init_level (int level, unsigned long count, bool quiet)
{
  unsigned long i, max = 8 * gomp_cpuset_size;

  if (gomp_cpusetp)
    {
      unsigned long maxcount
	= gomp_cpuset_popcount (gomp_cpuset_size, gomp_cpusetp);
      if (count > maxcount)
	count = maxcount;
    }
  gomp_places_list = gomp_affinity_alloc (count, quiet);
  gomp_places_list_len = 0;
  if (gomp_places_list == NULL)
    return false;
  /* SMT (threads).  */
  if (level == 1)
    {
      for (i = 0; i < max && gomp_places_list_len < count; i++)
	if (CPU_ISSET_S (i, gomp_cpuset_size, gomp_cpusetp))
	  {
	    gomp_affinity_init_place (gomp_places_list[gomp_places_list_len]);
	    gomp_affinity_add_cpus (gomp_places_list[gomp_places_list_len],
				    i, 1, 0, true);
	    ++gomp_places_list_len;
	  }
      return true;
    }
  else
    {
      char name[sizeof ("/sys/devices/system/cpu/cpu/topology/"
			"thread_siblings_list") + 3 * sizeof (unsigned long)];
      size_t prefix_len = sizeof ("/sys/devices/system/cpu/cpu") - 1;
      cpu_set_t *copy = gomp_alloca (gomp_cpuset_size);
      FILE *f;
      char *line = NULL;
      size_t linelen = 0;

      memcpy (name, "/sys/devices/system/cpu/cpu", prefix_len);
      memcpy (copy, gomp_cpusetp, gomp_cpuset_size);
      for (i = 0; i < max && gomp_places_list_len < count; i++)
	if (CPU_ISSET_S (i, gomp_cpuset_size, copy))
	  {
	    sprintf (name + prefix_len, "%lu/topology/%s_siblings_list",
		     i, level == 2 ? "thread" : "core");
	    f = fopen (name, "r");
	    if (f != NULL)
	      {
		if (getline (&line, &linelen, f) > 0)
		  {
		    char *p = line;
		    bool seen_i = false;
		    void *pl = gomp_places_list[gomp_places_list_len];
		    gomp_affinity_init_place (pl);
		    while (*p && *p != '\n')
		      {
			unsigned long first, last;
			errno = 0;
			first = strtoul (p, &p, 10);
			if (errno)
			  break;
			last = first;
			if (*p == '-')
			  {
			    errno = 0;
			    last = strtoul (p + 1, &p, 10);
			    if (errno || last < first)
			      break;
			  }
			for (; first <= last; first++)
			  if (CPU_ISSET_S (first, gomp_cpuset_size, copy)
			      && gomp_affinity_add_cpus (pl, first, 1, 0,
							 true))
			    {
			      CPU_CLR_S (first, gomp_cpuset_size, copy);
			      if (first == i)
				seen_i = true;
			    }
			if (*p == ',')
			  ++p;
		      }
		    if (seen_i)
		      gomp_places_list_len++;
		  }
		fclose (f);
	      }
	  }
      if (gomp_places_list_len == 0)
	{
	  if (!quiet)
	    gomp_error ("Error reading %s topology",
			level == 2 ? "core" : "socket");
	  free (gomp_places_list);
	  gomp_places_list = NULL;
	  return false;
	}
      return true;
    }
  return false;
}

void
gomp_affinity_print_place (void *p)
{
  unsigned long i, max = 8 * gomp_cpuset_size, len;
  cpu_set_t *cpusetp = (cpu_set_t *) p;
  bool notfirst = false;

  for (i = 0, len = 0; i < max; i++)
    if (CPU_ISSET_S (i, gomp_cpuset_size, cpusetp))
      {
	if (len == 0)
	  {
	    if (notfirst)
	      fputc (',', stderr);
	    notfirst = true;
	    fprintf (stderr, "%lu", i);
	  }
	++len;
      }
    else
      {
	if (len > 1)
	  fprintf (stderr, ":%lu", len);
	len = 0;
      }
  if (len > 1)
    fprintf (stderr, ":%lu", len);
}

#else

#include "../posix/affinity.c"

#endif
