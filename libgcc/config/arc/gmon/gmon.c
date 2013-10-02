/*-
 * Copyright (c) 1983, 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 * Copyright (C) 2007-2013 Free Software Foundation, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
#if 0
#include <sys/param.h>
#include <sys/time.h>
#endif
#include <sys/gmon.h>
#include <sys/gmon_out.h>

#include <stddef.h>
#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#if 0
#include <libc-internal.h>
#include <not-cancel.h>

#ifdef USE_IN_LIBIO
# include <wchar.h>
#endif
#endif
#define internal_function
#define weak_alias(fun,aliasid) extern __typeof(fun) aliasid __attribute__ ((weak, alias (#fun)));
#define __libc_enable_secure 0

/*  Head of basic-block list or NULL. */
struct __bb *__bb_head attribute_hidden;

struct gmonparam _gmonparam attribute_hidden = { GMON_PROF_OFF };

/*
 * See profil(2) where this is described:
 */
static int	s_scale;
#define		SCALE_1_TO_1	0x10000L

#define ERR(s) write (STDERR_FILENO, s, sizeof (s) - 1)

void moncontrol (int mode);
void __moncontrol (int mode);
static void write_hist (int fd) internal_function;
static void write_call_graph (int fd) internal_function;
static void write_bb_counts (int fd) internal_function;

/*
 * Control profiling
 *	profiling is what mcount checks to see if
 *	all the data structures are ready.
 */
void
__moncontrol (int mode)
{
  struct gmonparam *p = &_gmonparam;

  /* Don't change the state if we ran into an error.  */
  if (p->state == GMON_PROF_ERROR)
    return;

  if (mode)
    {
      /* start */
      __profil((void *) p->kcount, p->kcountsize, p->lowpc, s_scale);
      p->state = GMON_PROF_ON;
    }
  else
    {
      /* stop */
      __profil(NULL, 0, 0, 0);
      p->state = GMON_PROF_OFF;
    }
}
weak_alias (__moncontrol, moncontrol)


void
__monstartup (u_long lowpc, u_long highpc)
{
  register int o;
  char *cp;
  struct gmonparam *p = &_gmonparam;
  int linesz;

  /*
   * round lowpc and highpc to multiples of the density we're using
   * so the rest of the scaling (here and in gprof) stays in ints.
   */
  p->lowpc = ROUNDDOWN(lowpc, HISTFRACTION * sizeof(HISTCOUNTER));
  if (sizeof *p->froms % sizeof(HISTCOUNTER) != 0)
    {
      p->highpc = ROUNDUP(highpc, HISTFRACTION * sizeof(HISTCOUNTER));
      p->textsize = p->highpc - p->lowpc;
      p->kcountsize = ROUNDUP((p->textsize + HISTFRACTION - 1) / HISTFRACTION,
			      sizeof (*p->froms));
    }
  else
    {
      /* Avoid odd scales by rounding up highpc to get kcountsize rounded.  */
      p->textsize = ROUNDUP (highpc - p->lowpc,
			     HISTFRACTION * sizeof (*p->froms));
      p->highpc = p->lowpc + p->textsize;
      p->kcountsize = p->textsize / HISTFRACTION;
    }
  p->hashfraction = HASHFRACTION;
  p->log_hashfraction = -1;
  /* The following test must be kept in sync with the corresponding
     test in mcount.c.  */
  if ((HASHFRACTION & (HASHFRACTION - 1)) == 0) {
      /* if HASHFRACTION is a power of two, mcount can use shifting
	 instead of integer division.  Precompute shift amount. */
      p->log_hashfraction = ffs(p->hashfraction * sizeof(*p->froms)) - 1;
  }
  p->tolimit = p->textsize * ARCDENSITY / 100;
  if (p->tolimit < MINARCS)
    p->tolimit = MINARCS;
  else if (p->tolimit > MAXARCS)
    p->tolimit = MAXARCS;
  p->tossize = p->tolimit * sizeof(struct tostruct);

  /* p->kcount must not share cache lines with the adjacent data, because
     we use uncached accesses while profiling.  */
  linesz = __dcache_linesz ();
  cp = calloc (ROUNDUP (p->kcountsize, linesz) + p->tossize
	       + (linesz - 1), 1);
  if (! cp)
    {
      ERR("monstartup: out of memory\n");
      p->tos = NULL;
      p->state = GMON_PROF_ERROR;
      /* In case we loose the error state due to a race,
	 prevent invalid writes also by clearing tolimit.  */
      p->tolimit = 0;
      return;
    }
  p->tos = (struct tostruct *)cp;
  cp += p->tossize;
  cp = (char *) ROUNDUP ((ptrdiff_t) cp, linesz);
  p->kcount = (HISTCOUNTER *)cp;
  cp += ROUNDUP (p->kcountsize, linesz);

  p->tos[0].link = 0;

  o = p->highpc - p->lowpc;
  if (p->kcountsize < (u_long) o)
    {
#ifndef hp300
      s_scale = ((float)p->kcountsize / o ) * SCALE_1_TO_1;
#else
      /* avoid floating point operations */
      int quot = o / p->kcountsize;

      if (quot >= 0x10000)
	s_scale = 1;
      else if (quot >= 0x100)
	s_scale = 0x10000 / quot;
      else if (o >= 0x800000)
	s_scale = 0x1000000 / (o / (p->kcountsize >> 8));
      else
	s_scale = 0x1000000 / ((o << 8) / p->kcountsize);
#endif
    } else
      s_scale = SCALE_1_TO_1;

  __moncontrol(1);
}
weak_alias (__monstartup, monstartup)


static void
internal_function
write_hist (int fd)
{
  u_char tag = GMON_TAG_TIME_HIST;
  struct arc_gmon_hist_hdr thdr __attribute__ ((aligned (__alignof__ (char *))));
  int r;

  if (_gmonparam.kcountsize > 0)
    {
      *(char **) thdr.low_pc = (char *) _gmonparam.lowpc;
      *(char **) thdr.high_pc = (char *) _gmonparam.highpc;
      *(int32_t *) thdr.hist_size = (_gmonparam.kcountsize
				     / sizeof (HISTCOUNTER));
      *(int32_t *) thdr.prof_rate = __profile_frequency ();
      strncpy (thdr.dimen, "seconds", sizeof (thdr.dimen));
      thdr.dimen_abbrev = 's';

      r = write (fd, &tag, sizeof tag);
      if (r != sizeof tag)
	return;
      r = write (fd, &thdr, sizeof thdr);
      if (r != sizeof thdr)
	return;
      r = write (fd,_gmonparam.kcount, _gmonparam.kcountsize);
      if ((unsigned) r != _gmonparam.kcountsize)
	return;
    }
}


static void
internal_function
write_call_graph (int fd)
{
#define NARCS_PER_WRITE	64
#define BYTES_PER_ARC (1 + sizeof (struct gmon_cg_arc_record))
#define BYTES_PER_WRITE (BYTES_PER_ARC * NARCS_PER_WRITE)
  ARCINDEX to_index;
  u_long frompc, selfpc, count;
  char buffer[BYTES_PER_WRITE], *p;
  u_long *prof_desc = __arc_profile_desc_secstart;
  u_long *prof_count = __arc_profile_counters_secstart;
  u_long *prof_desc_end = __arc_profile_desc_secend;
  u_long *prof_forward = __arc_profile_forward_secstart;

  for (p = buffer; p < buffer + BYTES_PER_WRITE; p += BYTES_PER_ARC)
    *p = GMON_TAG_CG_ARC;
  p = buffer;
  frompc = *prof_desc++ & -2;
  while (prof_desc < prof_desc_end)
    {
      selfpc = *prof_desc++;
      if (selfpc & 1)
	{
	  frompc = selfpc & -2;
	  selfpc = *prof_desc++;
	}
      count = *prof_count++;
      if (selfpc)
	{
	  struct arc
	    {
	      char *frompc;
	      char *selfpc;
	      int32_t count;
	    }
	  arc;

	  if (!count)
	    continue;
	  arc.frompc = (char *) frompc;
	  arc.selfpc = (char *) selfpc;
	  arc.count  = count;
	  memcpy (p + 1, &arc, sizeof arc);
	  p += 1 + sizeof arc;

	  if (p == buffer + BYTES_PER_WRITE)
	    {
	      write (fd, buffer, BYTES_PER_WRITE);
	      p = buffer;
	    }
	}
      else
	{
	  for (to_index = count;
	       to_index != 0;
	       to_index = _gmonparam.tos[to_index].link)
	    {
	      struct arc
		{
		  char *frompc;
		  char *selfpc;
		  int32_t count;
		}
	      arc;

	      arc.frompc = (char *) frompc;
	      arc.selfpc = (char *) _gmonparam.tos[to_index].selfpc;
	      arc.count  = _gmonparam.tos[to_index].count;
	      memcpy (p + 1, &arc, sizeof arc);
	      p += 1 + sizeof arc;

	      if (p == buffer + BYTES_PER_WRITE)
		{
		  write (fd, buffer, BYTES_PER_WRITE);
		  p = buffer;
		}
	    }
	}
    }
  while (prof_forward < __arc_profile_forward_secend)
    {
      /* ??? The 'call count' is actually supposed to be a fixed point
	 factor, with 16 bits each before and after the point.
	 It would be much nicer if we figured out the actual number
	 of calls to the caller, and multiplied that with the fixed point
	 factor to arrive at the estimated calls for the callee.  */
      memcpy (p + 1, prof_forward, 3 * sizeof *prof_forward);
      prof_forward += 3;
      p += 1 + 3 * sizeof *prof_forward;
      if (p == buffer + BYTES_PER_WRITE)
	{
	  write (fd, buffer, BYTES_PER_WRITE);
	  p = buffer;
	}
    }
  if (p != buffer)
    write (fd, buffer, p - buffer);
}


static void
internal_function
write_bb_counts (int fd)
{
  struct __bb *grp;
  u_char tag = GMON_TAG_BB_COUNT;
  size_t ncounts;
  size_t i;

  struct { unsigned long address; long count; } bbbody[8];
  size_t nfilled;

  /* Write each group of basic-block info (all basic-blocks in a
     compilation unit form a single group). */

  for (grp = __bb_head; grp; grp = grp->next)
    {
      ncounts = grp->ncounts;
      write (fd, &tag, 1);
      write (fd, &ncounts, sizeof ncounts);
      for (nfilled = i = 0; i < ncounts; ++i)
	{
	  if (nfilled == sizeof (bbbody) / sizeof (bbbody[0]))
	    {
	      write (fd, bbbody, sizeof bbbody);
	      nfilled = 0;
	    }

	  bbbody[nfilled].address = grp->addresses[i];
	  bbbody[nfilled++].count = grp->counts[i];
	}
      if (nfilled > 0)
	write (fd, bbbody, nfilled * sizeof bbbody[0]);
    }
}


static void
write_gmon (void)
{
    struct gmon_hdr ghdr __attribute__ ((aligned (__alignof__ (int))));
    int fd = -1;
    char *env;

#ifndef O_NOFOLLOW
# define O_NOFOLLOW	0
#endif

    env = getenv ("GMON_OUT_PREFIX");
    if (env != NULL && !__libc_enable_secure)
      {
	size_t len = strlen (env);
	char buf[len + 20];
	snprintf (buf, sizeof (buf), "%s.%u", env, getpid ());
	fd = open (buf, O_CREAT|O_TRUNC|O_WRONLY|O_NOFOLLOW, 0666);
      }

    if (fd == -1)
      {
	fd = open ("gmon.out", O_CREAT|O_TRUNC|O_WRONLY|O_NOFOLLOW,
			      0666);
	if (fd < 0)
	  {
	    perror ("_mcleanup: gmon.out");
	    return;
	  }
      }

    /* write gmon.out header: */
    memset (&ghdr, '\0', sizeof (struct gmon_hdr));
    memcpy (&ghdr.cookie[0], GMON_MAGIC, sizeof (ghdr.cookie));
    *(int32_t *) ghdr.version = GMON_VERSION;
    write (fd, &ghdr, sizeof (struct gmon_hdr));

    /* write PC histogram: */
    write_hist (fd);

    /* write call-graph: */
    write_call_graph (fd);

    /* write basic-block execution counts: */
    write_bb_counts (fd);

    close (fd);
}


void
__write_profiling (void)
{
  int save = _gmonparam.state;
  _gmonparam.state = GMON_PROF_OFF;
  if (save == GMON_PROF_ON)
    write_gmon ();
  _gmonparam.state = save;
}
#ifndef SHARED
/* This symbol isn't used anywhere in the DSO and it is not exported.
   This would normally mean it should be removed to get the same API
   in static libraries.  But since profiling is special in static libs
   anyway we keep it.  But not when building the DSO since some
   quality assurance tests will otherwise trigger.  */
weak_alias (__write_profiling, write_profiling)
#endif


void
_mcleanup (void)
{
  __moncontrol (0);

  if (_gmonparam.state != GMON_PROF_ERROR)
    write_gmon ();

  /* free the memory. */
  if (_gmonparam.tos != NULL)
    free (_gmonparam.tos);
}
