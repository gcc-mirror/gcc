/* Affinity tests.
   Copyright (C) 2013-2024 Free Software Foundation, Inc.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* { dg-do run } */
/* { dg-set-target-env-var OMP_PROC_BIND "false" } */
/* { dg-additional-options "-Wno-deprecated-declarations" } */
/* { dg-additional-options "-DINTERPOSE_GETAFFINITY -DDO_FORK -ldl -Wno-deprecated-declarations" { target *-*-linux* } } */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include "config.h"
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef DO_FORK
#include <signal.h>
#include <sys/wait.h>
#endif
#ifdef HAVE_PTHREAD_AFFINITY_NP
#include <sched.h>
#include <pthread.h>
#ifdef INTERPOSE_GETAFFINITY
#include <dlfcn.h>
#endif
#endif

struct place
{
  int start, len;
};
struct places
{
  const char *name;
  int count;
  struct place places[8];
} places_array[] = {
  { "", 1, { { -1, -1 } } },
  { "{0}:8", 8,
    { { 0, 1 }, { 1, 1 }, { 2, 1 }, { 3, 1 },
      { 4, 1 }, { 5, 1 }, { 6, 1 }, { 7, 1 } } },
  { "{7,6}:2:-3", 2, { { 6, 2 }, { 3, 2 } } },
  { "{6,7}:4:-2,!{2,3}", 3, { { 6, 2 }, { 4, 2 }, { 0, 2 } } },
  { "{1}:7:1", 7,
    { { 1, 1 }, { 2, 1 }, { 3, 1 },
      { 4, 1 }, { 5, 1 }, { 6, 1 }, { 7, 1 } } },
  { "{0,1},{3,2,4},{6,5,!6},{6},{7:2:-1,!6}", 5,
    { { 0, 2 }, { 2, 3 }, { 5, 1 }, { 6, 1 }, { 7, 1 } } },
  { "1,2,{2,3,!2},3,3,!3,!{5:3:-1,!4,!5},{4},5,!4,!5,"
    "1:2,!{1},!2,7:3:-2,!{5},!7,!3", 3,
    { { 1, 1 }, { 2, 1 }, { 3, 1 } } }
};

unsigned long contig_cpucount;
unsigned long min_cpusetsize;

#if defined (HAVE_PTHREAD_AFFINITY_NP) && defined (_SC_NPROCESSORS_CONF) \
    && defined (CPU_ALLOC_SIZE)

#if defined (RTLD_NEXT) && defined (INTERPOSE_GETAFFINITY)
int (*orig_getaffinity_np) (pthread_t, size_t, cpu_set_t *);

int
pthread_getaffinity_np (pthread_t thread, size_t cpusetsize, cpu_set_t *cpuset)
{
  int ret;
  unsigned long i, max;
  if (orig_getaffinity_np == NULL)
    {
      orig_getaffinity_np = (int (*) (pthread_t, size_t, cpu_set_t *))
			    dlsym (RTLD_NEXT, "pthread_getaffinity_np");
      if (orig_getaffinity_np == NULL)
	exit (0);
    }
  ret = orig_getaffinity_np (thread, cpusetsize, cpuset);
  if (ret != 0)
    return ret;
  if (contig_cpucount == 0)
    {
      max = 8 * cpusetsize;
      for (i = 0; i < max; i++)
	if (!CPU_ISSET_S (i, cpusetsize, cpuset))
	  break;
      contig_cpucount = i;
      min_cpusetsize = cpusetsize;
    }
  return ret;
}
#endif

void
print_affinity (struct place p)
{
  static unsigned long size;
  if (size == 0)
    {
      if (min_cpusetsize)
	size = min_cpusetsize;
      else
	{
	  size = sysconf (_SC_NPROCESSORS_CONF);
	  size = CPU_ALLOC_SIZE (size);
	  if (size < sizeof (cpu_set_t))
	    size = sizeof (cpu_set_t);
	}
    }
  cpu_set_t *cpusetp = (cpu_set_t *) __builtin_alloca (size);
  if (pthread_getaffinity_np (pthread_self (), size, cpusetp) == 0)
    {
      unsigned long i, len, max = 8 * size;
      int notfirst = 0, unexpected = 1;

      printf (" bound to {");
      for (i = 0, len = 0; i < max; i++)
	if (CPU_ISSET_S (i, size, cpusetp))
	  {
	    if (len == 0)
	      {
		if (notfirst)
		  {
		    unexpected = 1;
		    printf (",");
		  }
		else if (i == (unsigned long) p.start)
		  unexpected = 0;
		notfirst = 1;
		printf ("%lu", i);
	      }
	    ++len;
	  }
	else
	  {
	    if (len && len != (unsigned long) p.len)
	      unexpected = 1;
	    if (len > 1)
	      printf (":%lu", len);
	    len = 0;
	  }
      if (len && len != (unsigned long) p.len)
	unexpected = 1;
      if (len > 1)
	printf (":%lu", len);
      printf ("}");
      if (p.start != -1 && unexpected)
	{
	  printf (", expected {%d", p.start);
	  if (p.len != 1)
	    printf (":%d", p.len);
	  printf ("} instead");
	}
      else if (p.start != -1)
	printf (", verified");
    }
}
#else
void
print_affinity (struct place p)
{
  (void) p.start;
  (void) p.len;
}
#endif


int
main ()
{
  char *env_proc_bind = getenv ("OMP_PROC_BIND");
  int test_false = env_proc_bind && strcmp (env_proc_bind, "false") == 0;
  int test_true = env_proc_bind && strcmp (env_proc_bind, "true") == 0;
  int test_spread_master_close
    = (env_proc_bind
       && (strcmp (env_proc_bind, "spread,master,close") == 0
	   || strcmp (env_proc_bind, "spread,primary,close") == 0));
  char *env_places = getenv ("OMP_PLACES");
  int test_places = 0;

  if (omp_proc_bind_master != omp_proc_bind_primary)
    abort ();

#ifdef DO_FORK
  if (env_places == NULL && contig_cpucount >= 8 && test_false
      && getenv ("GOMP_AFFINITY") == NULL)
    {
      int i, j, status;
      pid_t pid;
      for (j = 0; j < 3; j++)
	{
	  if (setenv ("OMP_PROC_BIND",
		      j > 1 ? "spread,primary,close"
			    : (j ? "spread,master,close" : "true"), 1) < 0)
	    break;
	  for (i = sizeof (places_array) / sizeof (places_array[0]) - 1;
	       i; --i)
	    {
	      if (setenv ("OMP_PLACES", places_array[i].name, 1) < 0)
		break;
	      pid = fork ();
	      if (pid == -1)
		break;
	      if (pid == 0)
		{
		  execl ("/proc/self/exe", "affinity-1.exe", NULL);
		  _exit (1);
		}
	      if (waitpid (pid, &status, 0) < 0)
		break;
	      if (WIFSIGNALED (status) && WTERMSIG (status) == SIGABRT)
		abort ();
	      else if (!WIFEXITED (status) || WEXITSTATUS (status) != 0)
		break;
	    }
	  if (i)
	    break;
	}
    }
#endif

  int first = 1;
  if (env_proc_bind)
    {
      printf ("OMP_PROC_BIND='%s'", env_proc_bind);
      first = 0;
    }
  if (env_places)
    printf ("%sOMP_PLACES='%s'", first ? "" : " ", env_places);
  printf ("\n");

  if (env_places && contig_cpucount >= 8
      && (test_true || test_spread_master_close))
    {
      for (test_places = sizeof (places_array) / sizeof (places_array[0]) - 1;
	   test_places; --test_places)
	if (strcmp (env_places, places_array[test_places].name) == 0)
	  break;
    }

#define verify(if_true, if_s_m_c) \
  if (test_false && omp_get_proc_bind () != omp_proc_bind_false)	\
    abort ();								\
  if (test_true && omp_get_proc_bind () != if_true)			\
    abort ();								\
  if (test_spread_master_close && omp_get_proc_bind () != if_s_m_c)	\
    abort ();

  verify (omp_proc_bind_true, omp_proc_bind_spread);

  printf ("Initial thread");
  print_affinity (places_array[test_places].places[0]);
  printf ("\n");
  omp_set_nested (1);
  omp_set_dynamic (0);

  #pragma omp parallel if (0)
  {
    verify (omp_proc_bind_true, omp_proc_bind_master);
    #pragma omp parallel if (0)
    {
      verify (omp_proc_bind_true, omp_proc_bind_close);
      #pragma omp parallel if (0)
      {
	verify (omp_proc_bind_true, omp_proc_bind_close);
      }
      #pragma omp parallel if (0) proc_bind (spread)
      {
	verify (omp_proc_bind_spread, omp_proc_bind_spread);
      }
    }
    #pragma omp parallel if (0) proc_bind (master)
    {
      verify (omp_proc_bind_master, omp_proc_bind_close);
      #pragma omp parallel if (0)
      {
	verify (omp_proc_bind_master, omp_proc_bind_close);
      }
      #pragma omp parallel if (0) proc_bind (spread)
      {
	verify (omp_proc_bind_spread, omp_proc_bind_spread);
      }
    }
  }

  /* True/spread */
  #pragma omp parallel num_threads (4)
  {
    verify (omp_proc_bind_true, omp_proc_bind_master);
    #pragma omp critical
    {
      struct place p = places_array[0].places[0];
      int thr = omp_get_thread_num ();
      printf ("#1 thread %d", thr);
      if (omp_get_num_threads () == 4 && test_spread_master_close)
	switch (places_array[test_places].count)
	  {
	  case 8:
	    /* T = 4, P = 8, each subpartition has 2 places.  */
	  case 7:
	    /* T = 4, P = 7, each subpartition has 2 places, but
	       last partition, which has just one place.  */
	    p = places_array[test_places].places[2 * thr];
	    break;
	  case 5:
	    /* T = 4, P = 5, first subpartition has 2 places, the
	       rest just one.  */
	    p = places_array[test_places].places[thr ? 1 + thr : 0];
	    break;
	  case 3:
	    /* T = 4, P = 3, unit sized subpartitions, first gets
	       thr0 and thr3, second thr1, third thr2.  */
	    p = places_array[test_places].places[thr == 3 ? 0 : thr];
	    break;
	  case 2:
	    /* T = 4, P = 2, unit sized subpartitions, each with
	       2 threads.  */
	    p = places_array[test_places].places[thr / 2];
	    break;
	  }
      print_affinity (p);
      printf ("\n");
    }
    #pragma omp barrier
    if (omp_get_thread_num () == 3)
      {
	/* True/spread, true/master.  */
	#pragma omp parallel num_threads (3)
	{
	  verify (omp_proc_bind_true, omp_proc_bind_close);
	  #pragma omp critical
	  {
	    struct place p = places_array[0].places[0];
	    int thr = omp_get_thread_num ();
	    printf ("#1,#1 thread 3,%d", thr);
	    if (omp_get_num_threads () == 3 && test_spread_master_close)
	      /* Outer is spread, inner master, so just bind to the
		 place or the master thread, which is thr 3 above.  */
	      switch (places_array[test_places].count)
		{
		case 8:
		case 7:
		  p = places_array[test_places].places[6];
		  break;
		case 5:
		  p = places_array[test_places].places[4];
		  break;
		case 3:
		  p = places_array[test_places].places[0];
		  break;
		case 2:
		  p = places_array[test_places].places[1];
		  break;
		}
	    print_affinity (p);
	    printf ("\n");
	  }
	}
	/* True/spread, spread.  */
	#pragma omp parallel num_threads (5) proc_bind (spread)
	{
	  verify (omp_proc_bind_spread, omp_proc_bind_close);
	  #pragma omp critical
	  {
	    struct place p = places_array[0].places[0];
	    int thr = omp_get_thread_num ();
	    printf ("#1,#2 thread 3,%d", thr);
	    if (omp_get_num_threads () == 5 && test_spread_master_close)
	      /* Outer is spread, inner spread.  */
	      switch (places_array[test_places].count)
		{
		case 8:
		  /* T = 5, P = 2, unit sized subpartitions.  */
		  p = places_array[test_places].places[thr == 4 ? 6
						       : 6 + thr / 2];
		  break;
		/* The rest are T = 5, P = 1.  */
		case 7:
		  p = places_array[test_places].places[6];
		  break;
		case 5:
		  p = places_array[test_places].places[4];
		  break;
		case 3:
		  p = places_array[test_places].places[0];
		  break;
		case 2:
		  p = places_array[test_places].places[1];
		  break;
		}
	    print_affinity (p);
	    printf ("\n");
	  }
	  #pragma omp barrier
	  if (omp_get_thread_num () == 3)
	    {
	      /* True/spread, spread, close.  */
	      #pragma omp parallel num_threads (5) proc_bind (close)
	      {
		verify (omp_proc_bind_close, omp_proc_bind_close);
		#pragma omp critical
		{
		  struct place p = places_array[0].places[0];
		  int thr = omp_get_thread_num ();
		  printf ("#1,#2,#1 thread 3,3,%d", thr);
		  if (omp_get_num_threads () == 5 && test_spread_master_close)
		    /* Outer is spread, inner spread, innermost close.  */
		    switch (places_array[test_places].count)
		      {
		      /* All are T = 5, P = 1.  */
		      case 8:
			p = places_array[test_places].places[7];
			break;
		      case 7:
			p = places_array[test_places].places[6];
			break;
		      case 5:
			p = places_array[test_places].places[4];
			break;
		      case 3:
			p = places_array[test_places].places[0];
			break;
		      case 2:
			p = places_array[test_places].places[1];
			break;
		      }
		  print_affinity (p);
		  printf ("\n");
		}
	      }
	    }
	}
	/* True/spread, master.  */
	#pragma omp parallel num_threads (4) proc_bind(master)
	{
	  verify (omp_proc_bind_master, omp_proc_bind_close);
	  #pragma omp critical
	  {
	    struct place p = places_array[0].places[0];
	    int thr = omp_get_thread_num ();
	    printf ("#1,#3 thread 3,%d", thr);
	    if (omp_get_num_threads () == 4 && test_spread_master_close)
	      /* Outer is spread, inner master, so just bind to the
		 place or the master thread, which is thr 3 above.  */
	      switch (places_array[test_places].count)
		{
		case 8:
		case 7:
		  p = places_array[test_places].places[6];
		  break;
		case 5:
		  p = places_array[test_places].places[4];
		  break;
		case 3:
		  p = places_array[test_places].places[0];
		  break;
		case 2:
		  p = places_array[test_places].places[1];
		  break;
		}
	    print_affinity (p);
	    printf ("\n");
	  }
	}
	/* True/spread, close.  */
	#pragma omp parallel num_threads (6) proc_bind (close)
	{
	  verify (omp_proc_bind_close, omp_proc_bind_close);
	  #pragma omp critical
	  {
	    struct place p = places_array[0].places[0];
	    int thr = omp_get_thread_num ();
	    printf ("#1,#4 thread 3,%d", thr);
	    if (omp_get_num_threads () == 6 && test_spread_master_close)
	      /* Outer is spread, inner close.  */
	      switch (places_array[test_places].count)
		{
		case 8:
		  /* T = 6, P = 2, unit sized subpartitions.  */
		  p = places_array[test_places].places[6 + thr / 3];
		  break;
		/* The rest are T = 6, P = 1.  */
		case 7:
		  p = places_array[test_places].places[6];
		  break;
		case 5:
		  p = places_array[test_places].places[4];
		  break;
		case 3:
		  p = places_array[test_places].places[0];
		  break;
		case 2:
		  p = places_array[test_places].places[1];
		  break;
		}
	    print_affinity (p);
	    printf ("\n");
	  }
	}
      }
  }

  /* Spread.  */
  #pragma omp parallel num_threads (5) proc_bind(spread)
  {
    verify (omp_proc_bind_spread, omp_proc_bind_master);
    #pragma omp critical
    {
      struct place p = places_array[0].places[0];
      int thr = omp_get_thread_num ();
      printf ("#2 thread %d", thr);
      if (omp_get_num_threads () == 5
	  && (test_spread_master_close || test_true))
	switch (places_array[test_places].count)
	  {
	  case 8:
	    /* T = 5, P = 8, first 3 subpartitions have 2 places, last
	       2 one place.  */
	    p = places_array[test_places].places[thr < 3 ? 2 * thr : 3 + thr];
	    break;
	  case 7:
	    /* T = 5, P = 7, first 2 subpartitions have 2 places, last
	       3 one place.  */
	    p = places_array[test_places].places[thr < 2 ? 2 * thr : 2 + thr];
	    break;
	  case 5:
	    /* T = 5, P = 5, unit sized subpartitions, each one with one
	       thread.  */
	    p = places_array[test_places].places[thr];
	    break;
	  case 3:
	    /* T = 5, P = 3, unit sized subpartitions, first gets
	       thr0 and thr3, second thr1 and thr4, third thr2.  */
	    p = places_array[test_places].places[thr >= 3 ? thr - 3 : thr];
	    break;
	  case 2:
	    /* T = 5, P = 2, unit sized subpartitions, first with
	       thr{0,1,4} and second with thr{2,3}.  */
	    p = places_array[test_places].places[thr == 4 ? 0 : thr / 2];
	    break;
	  }
      print_affinity (p);
      printf ("\n");
    }
    #pragma omp barrier
    if (omp_get_thread_num () == 3)
      {
	int pp = 0;
	switch (places_array[test_places].count)
	  {
	  case 8: pp = 6; break;
	  case 7: pp = 5; break;
	  case 5: pp = 3; break;
	  case 2: pp = 1; break;
	  }
	/* Spread, spread/master.  */
	#pragma omp parallel num_threads (3) firstprivate (pp)
	{
	  verify (omp_proc_bind_spread, omp_proc_bind_close);
	  #pragma omp critical
	  {
	    struct place p = places_array[0].places[0];
	    int thr = omp_get_thread_num ();
	    printf ("#2,#1 thread 3,%d", thr);
	    if (test_spread_master_close || test_true)
	      /* Outer is spread, inner spread resp. master, bit we have
		 just unit sized partitions.  */
	      p = places_array[test_places].places[pp];
	    print_affinity (p);
	    printf ("\n");
	  }
	}
	/* Spread, spread.  */
	#pragma omp parallel num_threads (5) proc_bind (spread) \
			     firstprivate (pp)
	{
	  verify (omp_proc_bind_spread, omp_proc_bind_close);
	  #pragma omp critical
	  {
	    struct place p = places_array[0].places[0];
	    int thr = omp_get_thread_num ();
	    printf ("#2,#2 thread 3,%d", thr);
	    if (test_spread_master_close || test_true)
	      /* Outer is spread, inner spread, bit we have
		 just unit sized partitions.  */
	      p = places_array[test_places].places[pp];
	    print_affinity (p);
	    printf ("\n");
	  }
	}
	/* Spread, master.  */
	#pragma omp parallel num_threads (4) proc_bind(master) \
			     firstprivate(pp)
	{
	  verify (omp_proc_bind_master, omp_proc_bind_close);
	  #pragma omp critical
	  {
	    struct place p = places_array[0].places[0];
	    int thr = omp_get_thread_num ();
	    printf ("#2,#3 thread 3,%d", thr);
	    if (test_spread_master_close || test_true)
	      /* Outer is spread, inner master, bit we have
		 just unit sized partitions.  */
	      p = places_array[test_places].places[pp];
	    print_affinity (p);
	    printf ("\n");
	  }
	}
	/* Spread, close.  */
	#pragma omp parallel num_threads (6) proc_bind (close) \
			     firstprivate (pp)
	{
	  verify (omp_proc_bind_close, omp_proc_bind_close);
	  #pragma omp critical
	  {
	    struct place p = places_array[0].places[0];
	    int thr = omp_get_thread_num ();
	    printf ("#2,#4 thread 3,%d", thr);
	    if (test_spread_master_close || test_true)
	      /* Outer is spread, inner close, bit we have
		 just unit sized partitions.  */
	      p = places_array[test_places].places[pp];
	    print_affinity (p);
	    printf ("\n");
	  }
	}
      }
  }

  /* Master.  */
  #pragma omp parallel num_threads (3) proc_bind(master)
  {
    verify (omp_proc_bind_master, omp_proc_bind_master);
    #pragma omp critical
    {
      struct place p = places_array[0].places[0];
      int thr = omp_get_thread_num ();
      printf ("#3 thread %d", thr);
      if (test_spread_master_close || test_true)
	p = places_array[test_places].places[0];
      print_affinity (p);
      printf ("\n");
    }
    #pragma omp barrier
    if (omp_get_thread_num () == 2)
      {
	/* Master, master.  */
	#pragma omp parallel num_threads (4)
	{
	  verify (omp_proc_bind_master, omp_proc_bind_close);
	  #pragma omp critical
	  {
	    struct place p = places_array[0].places[0];
	    int thr = omp_get_thread_num ();
	    printf ("#3,#1 thread 2,%d", thr);
	    if (test_spread_master_close || test_true)
	      /* Outer is master, inner is master.  */
	      p = places_array[test_places].places[0];
	    print_affinity (p);
	    printf ("\n");
	  }
	}
	/* Master, spread.  */
	#pragma omp parallel num_threads (4) proc_bind (spread)
	{
	  verify (omp_proc_bind_spread, omp_proc_bind_close);
	  #pragma omp critical
	  {
	    struct place p = places_array[0].places[0];
	    int thr = omp_get_thread_num ();
	    printf ("#3,#2 thread 2,%d", thr);
	    if (omp_get_num_threads () == 4
		&& (test_spread_master_close || test_true))
	      /* Outer is master, inner is spread.  */
	      switch (places_array[test_places].count)
		{
		case 8:
		  /* T = 4, P = 8, each subpartition has 2 places.  */
		case 7:
		  /* T = 4, P = 7, each subpartition has 2 places, but
		     last partition, which has just one place.  */
		  p = places_array[test_places].places[2 * thr];
		  break;
		case 5:
		  /* T = 4, P = 5, first subpartition has 2 places, the
		     rest just one.  */
		  p = places_array[test_places].places[thr ? 1 + thr : 0];
		  break;
		case 3:
		  /* T = 4, P = 3, unit sized subpartitions, first gets
		     thr0 and thr3, second thr1, third thr2.  */
		  p = places_array[test_places].places[thr == 3 ? 0 : thr];
		  break;
		case 2:
		  /* T = 4, P = 2, unit sized subpartitions, each with
		     2 threads.  */
		  p = places_array[test_places].places[thr / 2];
		  break;
		}
	    print_affinity (p);
	    printf ("\n");
	  }
	  #pragma omp barrier
	  if (omp_get_thread_num () == 0)
	    {
	      /* Master, spread, close.  */
	      #pragma omp parallel num_threads (5) proc_bind (close)
	      {
		verify (omp_proc_bind_close, omp_proc_bind_close);
		#pragma omp critical
		{
		  struct place p = places_array[0].places[0];
		  int thr = omp_get_thread_num ();
		  printf ("#3,#2,#1 thread 2,0,%d", thr);
		  if (omp_get_num_threads () == 5
		      && (test_spread_master_close || test_true))
		    /* Outer is master, inner spread, innermost close.  */
		    switch (places_array[test_places].count)
		      {
		      /* First 3 are T = 5, P = 2.  */
		      case 8:
		      case 7:
		      case 5:
			p = places_array[test_places].places[(thr & 2) / 2];
			break;
		      /* All the rest are T = 5, P = 1.  */
		      case 3:
		      case 2:
			p = places_array[test_places].places[0];
			break;
		      }
		  print_affinity (p);
		  printf ("\n");
		}
	      }
	    }
	  #pragma omp barrier
	  if (omp_get_thread_num () == 3)
	    {
	      /* Master, spread, close.  */
	      #pragma omp parallel num_threads (5) proc_bind (close)
	      {
		verify (omp_proc_bind_close, omp_proc_bind_close);
		#pragma omp critical
		{
		  struct place p = places_array[0].places[0];
		  int thr = omp_get_thread_num ();
		  printf ("#3,#2,#2 thread 2,3,%d", thr);
		  if (omp_get_num_threads () == 5
		      && (test_spread_master_close || test_true))
		    /* Outer is master, inner spread, innermost close.  */
		    switch (places_array[test_places].count)
		      {
		      case 8:
			/* T = 5, P = 2.  */
			p = places_array[test_places].places[6
							     + (thr & 2) / 2];
			break;
		      /* All the rest are T = 5, P = 1.  */
		      case 7:
			p = places_array[test_places].places[6];
			break;
		      case 5:
			p = places_array[test_places].places[4];
			break;
		      case 3:
			p = places_array[test_places].places[0];
			break;
		      case 2:
			p = places_array[test_places].places[1];
			break;
		      }
		  print_affinity (p);
		  printf ("\n");
		}
	      }
	    }
	}
	/* Master, master.  */
	#pragma omp parallel num_threads (4) proc_bind(master)
	{
	  verify (omp_proc_bind_master, omp_proc_bind_close);
	  #pragma omp critical
	  {
	    struct place p = places_array[0].places[0];
	    int thr = omp_get_thread_num ();
	    printf ("#3,#3 thread 2,%d", thr);
	    if (test_spread_master_close || test_true)
	      /* Outer is master, inner master.  */
	      p = places_array[test_places].places[0];
	    print_affinity (p);
	    printf ("\n");
	  }
	}
	/* Master, close.  */
	#pragma omp parallel num_threads (6) proc_bind (close)
	{
	  verify (omp_proc_bind_close, omp_proc_bind_close);
	  #pragma omp critical
	  {
	    struct place p = places_array[0].places[0];
	    int thr = omp_get_thread_num ();
	    printf ("#3,#4 thread 2,%d", thr);
	    if (omp_get_num_threads () == 6
		&& (test_spread_master_close || test_true))
	      switch (places_array[test_places].count)
		{
		case 8:
		  /* T = 6, P = 8.  */
		case 7:
		  /* T = 6, P = 7.  */
		  p = places_array[test_places].places[thr];
		  break;
		case 5:
		  /* T = 6, P = 5.  thr{0,5} go into the first place.  */
		  p = places_array[test_places].places[thr == 5 ? 0 : thr];
		  break;
		case 3:
		  /* T = 6, P = 3, two threads into each place.  */
		  p = places_array[test_places].places[thr / 2];
		  break;
		case 2:
		  /* T = 6, P = 2, 3 threads into each place.  */
		  p = places_array[test_places].places[thr / 3];
		  break;
		}
	    print_affinity (p);
	    printf ("\n");
	  }
	}
      }
  }

  #pragma omp parallel num_threads (5) proc_bind(close)
  {
    verify (omp_proc_bind_close, omp_proc_bind_master);
    #pragma omp critical
    {
      struct place p = places_array[0].places[0];
      int thr = omp_get_thread_num ();
      printf ("#4 thread %d", thr);
      if (omp_get_num_threads () == 5
	  && (test_spread_master_close || test_true))
	switch (places_array[test_places].count)
	  {
	  case 8:
	    /* T = 5, P = 8.  */
	  case 7:
	    /* T = 5, P = 7.  */
	  case 5:
	    /* T = 5, P = 5.  */
	    p = places_array[test_places].places[thr];
	    break;
	  case 3:
	    /* T = 5, P = 3, thr{0,3} in first place, thr{1,4} in second,
	       thr2 in third.  */
	    p = places_array[test_places].places[thr >= 3 ? thr - 3 : thr];
	    break;
	  case 2:
	    /* T = 5, P = 2, thr{0,1,4} in first place, thr{2,3} in second.  */
	    p = places_array[test_places].places[thr == 4 ? 0 : thr / 2];
	    break;
	  }
      print_affinity (p);
      printf ("\n");
    }
    #pragma omp barrier
    if (omp_get_thread_num () == 2)
      {
	int pp = 0;
	switch (places_array[test_places].count)
	  {
	  case 8:
	  case 7:
	  case 5:
	  case 3:
	    pp = 2;
	    break;
	  case 2:
	    pp = 1;
	    break;
	  }
	/* Close, close/master.  */
	#pragma omp parallel num_threads (4) firstprivate (pp)
	{
	  verify (omp_proc_bind_close, omp_proc_bind_close);
	  #pragma omp critical
	  {
	    struct place p = places_array[0].places[0];
	    int thr = omp_get_thread_num ();
	    printf ("#4,#1 thread 2,%d", thr);
	    if (test_spread_master_close)
	      /* Outer is close, inner is master.  */
	      p = places_array[test_places].places[pp];
	    else if (omp_get_num_threads () == 4 && test_true)
	      /* Outer is close, inner is close.  */
	      switch (places_array[test_places].count)
		{
		case 8:
		  /* T = 4, P = 8.  */
		case 7:
		  /* T = 4, P = 7.  */
		  p = places_array[test_places].places[2 + thr];
		  break;
		case 5:
		  /* T = 4, P = 5.  There is wrap-around for thr3.  */
		  p = places_array[test_places].places[thr == 3 ? 0 : 2 + thr];
		  break;
		case 3:
		  /* T = 4, P = 3, thr{0,3} go into p2, thr1 into p0, thr2
		     into p1.  */
		  p = places_array[test_places].places[(2 + thr) % 3];
		  break;
		case 2:
		  /* T = 4, P = 2, 2 threads into each place.  */
		  p = places_array[test_places].places[1 - thr / 2];
		  break;
		}

	    print_affinity (p);
	    printf ("\n");
	  }
	}
	/* Close, spread.  */
	#pragma omp parallel num_threads (4) proc_bind (spread)
	{
	  verify (omp_proc_bind_spread, omp_proc_bind_close);
	  #pragma omp critical
	  {
	    struct place p = places_array[0].places[0];
	    int thr = omp_get_thread_num ();
	    printf ("#4,#2 thread 2,%d", thr);
	    if (omp_get_num_threads () == 4
		&& (test_spread_master_close || test_true))
	      /* Outer is close, inner is spread.  */
	      switch (places_array[test_places].count)
		{
		case 8:
		  /* T = 4, P = 8, each subpartition has 2 places.  */
		case 7:
		  /* T = 4, P = 7, each subpartition has 2 places, but
		     last partition, which has just one place.  */
		  p = places_array[test_places].places[thr == 3 ? 0
						       : 2 + 2 * thr];
		  break;
		case 5:
		  /* T = 4, P = 5, first subpartition has 2 places, the
		     rest just one.  */
		  p = places_array[test_places].places[thr == 3 ? 0
						       : 2 + thr];
		  break;
		case 3:
		  /* T = 4, P = 3, unit sized subpartitions, third gets
		     thr0 and thr3, first thr1, second thr2.  */
		  p = places_array[test_places].places[thr == 0 ? 2 : thr - 1];
		  break;
		case 2:
		  /* T = 4, P = 2, unit sized subpartitions, each with
		     2 threads.  */
		  p = places_array[test_places].places[1 - thr / 2];
		  break;
		}
	    print_affinity (p);
	    printf ("\n");
	  }
	  #pragma omp barrier
	  if (omp_get_thread_num () == 0)
	    {
	      /* Close, spread, close.  */
	      #pragma omp parallel num_threads (5) proc_bind (close)
	      {
		verify (omp_proc_bind_close, omp_proc_bind_close);
		#pragma omp critical
		{
		  struct place p = places_array[0].places[0];
		  int thr = omp_get_thread_num ();
		  printf ("#4,#2,#1 thread 2,0,%d", thr);
		  if (omp_get_num_threads () == 5
		      && (test_spread_master_close || test_true))
		    /* Outer is close, inner spread, innermost close.  */
		    switch (places_array[test_places].count)
		      {
		      case 8:
		      case 7:
			/* T = 5, P = 2.  */
			p = places_array[test_places].places[2
							     + (thr & 2) / 2];
			break;
		      /* All the rest are T = 5, P = 1.  */
		      case 5:
		      case 3:
			p = places_array[test_places].places[2];
			break;
		      case 2:
			p = places_array[test_places].places[1];
			break;
		      }
		  print_affinity (p);
		  printf ("\n");
		}
	      }
	    }
	  #pragma omp barrier
	  if (omp_get_thread_num () == 2)
	    {
	      /* Close, spread, close.  */
	      #pragma omp parallel num_threads (5) proc_bind (close)
	      {
		verify (omp_proc_bind_close, omp_proc_bind_close);
		#pragma omp critical
		{
		  struct place p = places_array[0].places[0];
		  int thr = omp_get_thread_num ();
		  printf ("#4,#2,#2 thread 2,2,%d", thr);
		  if (omp_get_num_threads () == 5
		      && (test_spread_master_close || test_true))
		    /* Outer is close, inner spread, innermost close.  */
		    switch (places_array[test_places].count)
		      {
		      case 8:
			/* T = 5, P = 2.  */
			p = places_array[test_places].places[6
							     + (thr & 2) / 2];
			break;
		      /* All the rest are T = 5, P = 1.  */
		      case 7:
			p = places_array[test_places].places[6];
			break;
		      case 5:
			p = places_array[test_places].places[4];
			break;
		      case 3:
			p = places_array[test_places].places[1];
			break;
		      case 2:
			p = places_array[test_places].places[0];
			break;
		      }
		  print_affinity (p);
		  printf ("\n");
		}
	      }
	    }
	  #pragma omp barrier
	  if (omp_get_thread_num () == 3)
	    {
	      /* Close, spread, close.  */
	      #pragma omp parallel num_threads (5) proc_bind (close)
	      {
		verify (omp_proc_bind_close, omp_proc_bind_close);
		#pragma omp critical
		{
		  struct place p = places_array[0].places[0];
		  int thr = omp_get_thread_num ();
		  printf ("#4,#2,#3 thread 2,3,%d", thr);
		  if (omp_get_num_threads () == 5
		      && (test_spread_master_close || test_true))
		    /* Outer is close, inner spread, innermost close.  */
		    switch (places_array[test_places].count)
		      {
		      case 8:
		      case 7:
		      case 5:
			/* T = 5, P = 2.  */
			p = places_array[test_places].places[(thr & 2) / 2];
			break;
		      /* All the rest are T = 5, P = 1.  */
		      case 3:
			p = places_array[test_places].places[2];
			break;
		      case 2:
			p = places_array[test_places].places[0];
			break;
		      }
		  print_affinity (p);
		  printf ("\n");
		}
	      }
	    }
	}
	/* Close, master.  */
	#pragma omp parallel num_threads (4) proc_bind(master) \
			     firstprivate (pp)
	{
	  verify (omp_proc_bind_master, omp_proc_bind_close);
	  #pragma omp critical
	  {
	    struct place p = places_array[0].places[0];
	    int thr = omp_get_thread_num ();
	    printf ("#4,#3 thread 2,%d", thr);
	    if (test_spread_master_close || test_true)
	      /* Outer is close, inner master.  */
	      p = places_array[test_places].places[pp];
	    print_affinity (p);
	    printf ("\n");
	  }
	}
	/* Close, close.  */
	#pragma omp parallel num_threads (6) proc_bind (close)
	{
	  verify (omp_proc_bind_close, omp_proc_bind_close);
	  #pragma omp critical
	  {
	    struct place p = places_array[0].places[0];
	    int thr = omp_get_thread_num ();
	    printf ("#4,#4 thread 2,%d", thr);
	    if (omp_get_num_threads () == 6
		&& (test_spread_master_close || test_true))
	      switch (places_array[test_places].count)
		{
		case 8:
		  /* T = 6, P = 8.  */
		  p = places_array[test_places].places[2 + thr];
		  break;
		case 7:
		  /* T = 6, P = 7.  */
		  p = places_array[test_places].places[thr == 5 ? 0 : 2 + thr];
		  break;
		case 5:
		  /* T = 6, P = 5.  thr{0,5} go into the third place.  */
		  p = places_array[test_places].places[thr >= 3 ? thr - 3
						       : 2 + thr];
		  break;
		case 3:
		  /* T = 6, P = 3, two threads into each place.  */
		  p = places_array[test_places].places[thr < 2 ? 2
						       : thr / 2 - 1];
		  break;
		case 2:
		  /* T = 6, P = 2, 3 threads into each place.  */
		  p = places_array[test_places].places[1 - thr / 3];
		  break;
		}
	    print_affinity (p);
	    printf ("\n");
	  }
	}
      }
  }

  return 0;
}
