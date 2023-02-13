/* Copyright (C) 2006-2023 Free Software Foundation, Inc.
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
#include <limits.h>

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

/* Find the index of the last level cache.  We assume the index
   of the last level cache is the same for all logical CPUs.
   Also, if there are multiple caches with the same highest level,
   assume they have the same shared_cpu_list and pick the last one
   from them (highest index number).  */

static int
gomp_affinity_find_last_cache_level (char *name, size_t prefix_len,
				     unsigned long cpu)
{
  int ret = -1;
  unsigned long maxval = 0;
  char *line = NULL;
  size_t linelen = 0;
  FILE *f;

  for (int l = 0; l < 128; l++)
    {
      sprintf (name + prefix_len, "%lu/cache/index%u/level", cpu, l);
      f = fopen (name, "r");
      if (f == NULL)
	break;
      if (getline (&line, &linelen, f) > 0)
	{
	  unsigned long val;
	  char *p;
	  errno = 0;
	  val = strtoul (line, &p, 10);
	  if (!errno && p > line && val >= maxval)
	    {
	      ret = l;
	      maxval = val;
	    }
	}
      fclose (f);
    }
  free (line);
  return ret;
}

static void
gomp_affinity_init_level_1 (int level, int this_level, unsigned long count,
			    cpu_set_t *copy, char *name, bool quiet)
{
  size_t prefix_len = sizeof ("/sys/devices/system/cpu/cpu") - 1;
  FILE *f;
  char *line = NULL;
  size_t linelen = 0;
  unsigned long i, max = 8 * gomp_cpuset_size;
  int init = -1;

  for (i = 0; i < max && gomp_places_list_len < count; i++)
    if (CPU_ISSET_S (i, gomp_cpuset_size, copy))
      {
	if (level == 4)
	  {
	    if (init == -1)
	      {
		init = gomp_affinity_find_last_cache_level (name, prefix_len,
							    i);
		if (init == -1)
		  {
		    CPU_CLR_S (i, gomp_cpuset_size, copy);
		    continue;
		  }
		sprintf (name + prefix_len,
			 "%lu/cache/index%u/shared_cpu_list", i, init);
	      }
	  }
	else
	  sprintf (name + prefix_len, "%lu/topology/%s_siblings_list",
		   i, this_level == 3 ? "core" : "thread");
	f = fopen (name, "r");
	if (f == NULL)
	  {
	    CPU_CLR_S (i, gomp_cpuset_size, copy);
	    continue;
	  }
	if (getline (&line, &linelen, f) > 0)
	  {
	    char *p = line, *end;
	    void *pl = gomp_places_list[gomp_places_list_len];
	    if (level == this_level)
	      gomp_affinity_init_place (pl);
	    while (*p && *p != '\n')
	      {
		unsigned long first, last;
		errno = 0;
		first = strtoul (p, &end, 10);
		if (errno || end == p)
		  break;
		p = end;
		last = first;
		if (*p == '-')
		  {
		    errno = 0;
		    last = strtoul (p + 1, &end, 10);
		    if (errno || end == p + 1 || last < first)
		      break;
		    p = end;
		  }
		for (; first <= last; first++)
		  if (!CPU_ISSET_S (first, gomp_cpuset_size, copy))
		    continue;
		  else if (this_level == 3 && level < this_level)
		    gomp_affinity_init_level_1 (level, 2, count, copy,
						name, quiet);
		  else
		    {
		      if (level == 1)
			{
			  pl = gomp_places_list[gomp_places_list_len];
			  gomp_affinity_init_place (pl);
			}
		      if (gomp_affinity_add_cpus (pl, first, 1, 0, true))
			{
			  CPU_CLR_S (first, gomp_cpuset_size, copy);
			  if (level == 1
			      && ++gomp_places_list_len >= count)
			    {
			      fclose (f);
			      free (line);
			      return;
			    }
			}
		    }
		if (*p == ',')
		  ++p;
	      }
	    if (level == this_level
		&& !CPU_ISSET_S (i, gomp_cpuset_size, copy))
	      gomp_places_list_len++;
	    CPU_CLR_S (i, gomp_cpuset_size, copy);
	  }
	fclose (f);
      }
  free (line);
}

static void
gomp_affinity_init_numa_domains (unsigned long count, cpu_set_t *copy,
				 char *name)
{
  FILE *f;
  char *nline = NULL, *line = NULL;
  size_t nlinelen = 0, linelen = 0;
  char *q;
  size_t prefix_len = sizeof ("/sys/devices/system/node/") - 1;

  strcpy (name, "/sys/devices/system/node/online");
  f = fopen (name, "r");
  if (f == NULL || getline (&nline, &nlinelen, f) <= 0)
    {
      if (f)
	fclose (f);
      return;
    }
  fclose (f);
  q = nline;
  while (*q && *q != '\n' && gomp_places_list_len < count)
    {
      unsigned long nfirst, nlast;
      char *end;

      errno = 0;
      nfirst = strtoul (q, &end, 10);
      if (errno || end == q)
	break;
      q = end;
      nlast = nfirst;
      if (*q == '-')
	{
	  errno = 0;
	  nlast = strtoul (q + 1, &end, 10);
	  if (errno || end == q + 1 || nlast < nfirst)
	    break;
	  q = end;
	}
      for (; nfirst <= nlast && gomp_places_list_len < count; nfirst++)
	{
	  sprintf (name + prefix_len, "node%lu/cpulist", nfirst);
	  f = fopen (name, "r");
	  if (f == NULL)
	    continue;
	  if (getline (&line, &linelen, f) > 0)
	    {
	      char *p = line;
	      void *pl = NULL;
	      bool seen = false;

	      while (*p && *p != '\n')
		{
		  unsigned long first, last;

		  errno = 0;
		  first = strtoul (p, &end, 10);
		  if (errno || end == p)
		    break;
		  p = end;
		  last = first;
		  if (*p == '-')
		    {
		      errno = 0;
		      last = strtoul (p + 1, &end, 10);
		      if (errno || end == p + 1 || last < first)
			break;
		      p = end;
		    }
		  for (; first <= last; first++)
		    {
		      if (!CPU_ISSET_S (first, gomp_cpuset_size, copy))
			continue;
		      if (pl == NULL)
			{
			  pl = gomp_places_list[gomp_places_list_len];
			  gomp_affinity_init_place (pl);
			}
		      if (gomp_affinity_add_cpus (pl, first, 1, 0, true))
			{
			  CPU_CLR_S (first, gomp_cpuset_size, copy);
			  if (!seen)
			    {
			      gomp_places_list_len++;
			      seen = true;
			    }
			}
		    }
		  if (*p == ',')
		    ++p;
		}
	    }
	  fclose (f);
	}
      if (*q == ',')
	++q;
    }
  free (line);
  free (nline);
}

bool
gomp_affinity_init_level (int level, unsigned long count, bool quiet)
{
  char name[sizeof ("/sys/devices/system/cpu/cpu/topology/"
		    "thread_siblings_list") + 6 * sizeof (unsigned long)];
  cpu_set_t *copy;

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

  copy = gomp_alloca (gomp_cpuset_size);
  strcpy (name, "/sys/devices/system/cpu/cpu");
  memcpy (copy, gomp_cpusetp, gomp_cpuset_size);
  if (level == 5)
    gomp_affinity_init_numa_domains (count, copy, name);
  else
    gomp_affinity_init_level_1 (level, level > 3 ? level : 3, count, copy,
				name, quiet);
  if (gomp_places_list_len == 0)
    {
      if (!quiet)
	gomp_error ("Error reading core/socket topology");
      free (gomp_places_list);
      gomp_places_list = NULL;
      return false;
    }
  return true;
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

int
omp_get_place_num_procs (int place_num)
{
  if (place_num < 0 || place_num >= gomp_places_list_len)
    return 0;

  cpu_set_t *cpusetp = (cpu_set_t *) gomp_places_list[place_num];
  return gomp_cpuset_popcount (gomp_cpuset_size, cpusetp);
}

void
omp_get_place_proc_ids (int place_num, int *ids)
{
  if (place_num < 0 || place_num >= gomp_places_list_len)
    return;

  cpu_set_t *cpusetp = (cpu_set_t *) gomp_places_list[place_num];
  unsigned long i, max = 8 * gomp_cpuset_size;
  for (i = 0; i < max; i++)
    if (CPU_ISSET_S (i, gomp_cpuset_size, cpusetp))
      *ids++ = i;
}

void
gomp_get_place_proc_ids_8 (int place_num, int64_t *ids)
{
  if (place_num < 0 || place_num >= gomp_places_list_len)
    return;

  cpu_set_t *cpusetp = (cpu_set_t *) gomp_places_list[place_num];
  unsigned long i, max = 8 * gomp_cpuset_size;
  for (i = 0; i < max; i++)
    if (CPU_ISSET_S (i, gomp_cpuset_size, cpusetp))
      *ids++ = i;
}

void
gomp_display_affinity_place (char *buffer, size_t size, size_t *ret,
			     int place)
{
  cpu_set_t *cpusetp;
  char buf[sizeof (long) * 3 + 4];
  if (place >= 0 && place < gomp_places_list_len)
    cpusetp = (cpu_set_t *) gomp_places_list[place];
  else if (gomp_cpusetp)
    cpusetp = gomp_cpusetp;
  else
    {
      if (gomp_available_cpus > 1)
	sprintf (buf, "0-%lu", gomp_available_cpus - 1);
      else
	strcpy (buf, "0");
      gomp_display_string (buffer, size, ret, buf, strlen (buf));
      return;
    }

  unsigned long i, max = 8 * gomp_cpuset_size, start;
  bool prev_set = false;
  start = max;
  for (i = 0; i <= max; i++)
    {
      bool this_set;
      if (i == max)
	this_set = false;
      else
	this_set = CPU_ISSET_S (i, gomp_cpuset_size, cpusetp);
      if (this_set != prev_set)
	{
	  prev_set = this_set;
	  if (this_set)
	    {
	      char *p = buf;
	      if (start != max)
		*p++ = ',';
	      sprintf (p, "%lu", i);
	      start = i;
	    }
	  else if (i == start + 1)
	    continue;
	  else
	    sprintf (buf, "-%lu", i - 1);
	  gomp_display_string (buffer, size, ret, buf, strlen (buf));
	}
    }
}

ialias(omp_get_place_num_procs)
ialias(omp_get_place_proc_ids)

#else

#include "../../affinity.c"

#endif
