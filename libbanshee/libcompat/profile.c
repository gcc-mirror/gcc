/*
 * Copyright (c) 1999-2001
 *      The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
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
 *
 */

#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <signal.h>
#undef REGION_PROFILE
#include "regions.h"
#include "profile.h"

typedef struct Alloc_info
{
  struct Alloc_info *next;
  char *file;
  int line;
  unsigned long size;
  unsigned long calls;
} *ainfo;

static ainfo ainfos = NULL;
static region profile_region = NULL;

/* perror(s) then exit */
void pfail(const char *s)
{
  perror(s);
  exit(EXIT_FAILURE);
}

/**************************************************************************
 *                                                                        *
 * Log information about an allocation -- generic                         *
 *                                                                        *
 **************************************************************************/

static int registered_exit = 0;

static ainfo find_ainfo(char *file, int line)
{
  ainfo ai;

  for (ai = ainfos; ai; ai = ai->next)
    if (line == ai->line && !strcmp(file, ai->file))
      return ai;

  if (!registered_exit)
    {
      if (atexit(profile))
	fprintf(stderr, "Registration of profile at exit failed\n");
      registered_exit = 1;
    }

  if (!profile_region)
    profile_region = newregion();
  ai = ralloc(profile_region, struct Alloc_info);
  ai->file = file;
  ai->line = line;
  ai->size = 0;
  ai->calls = 0;
  ai->next = ainfos;
  ainfos = ai;
  return ai;
}

/**************************************************************************
 *                                                                        *
 * Log information about an allocation -- GCC                             *
 *                                                                        *
 * WARNING:  This code uses __builtin_return_address, a non-portable      *
 * feature of gcc, to trace the call chain back.   You'll also get ugly   *
 * output unless the addr2line (in GNU binutils) is installed.            *
 *                                                                        *
 * ANOTHER WARNING:  The depths hard-coded in find_cinfo are only correct *
 * if find_cinfo is inlined.  Ack!                                        *
 *                                                                        *
 **************************************************************************/

#define REGION_PROFILE_DEPTH 2
#undef TRACE_STACK
#if defined(__GNUC__) && defined(__OPTIMIZE__) && REGION_PROFILE_DEPTH > 1
#define TRACE_STACK
#endif

#ifdef TRACE_STACK

#if REGION_PROFILE_DEPTH > 6
#error "REGION_PROFILE_DEPTH must be less than 6.  See find_cinfo()."
#endif

typedef struct Call_info
{
  struct Call_info *next;
  void **stack;         /* Array holding the call chain */
  unsigned long size;
  unsigned long calls;
} *cinfo;

static cinfo cinfos = NULL;

/* Find the current call chain and return a pointer to our status for
   it, or allocate a new entry if there is none. */
static cinfo find_cinfo(void)
{
  void *calls[REGION_PROFILE_DEPTH];
  int i;
  cinfo ci;

  /* Compute the call chain.  This is an awful hack. */
  i = 0;
  if (i < REGION_PROFILE_DEPTH)
    calls[i++] = __builtin_return_address(1);
  if (i < REGION_PROFILE_DEPTH)
    calls[i++] = __builtin_return_address(2);
  if (i < REGION_PROFILE_DEPTH)
    calls[i++] = __builtin_return_address(3);
  if (i < REGION_PROFILE_DEPTH)
    calls[i++] = __builtin_return_address(4);
  if (i < REGION_PROFILE_DEPTH)
    calls[i++] = __builtin_return_address(5);
  if (i < REGION_PROFILE_DEPTH)
    calls[i++] = __builtin_return_address(6);
  /* Add more if you want a higher call-depth (why would you?) */

  /* Find it */
  for (ci = cinfos; ci; ci = ci->next)
    if (!memcmp(calls, ci->stack, REGION_PROFILE_DEPTH*sizeof(void *)))
      return ci;

  if (!profile_region)
    profile_region = newregion();
  ci = ralloc(profile_region, struct Call_info);
  ci->stack = rarrayalloc(profile_region, REGION_PROFILE_DEPTH, void *);
  memcpy(ci->stack, calls, REGION_PROFILE_DEPTH*sizeof(void *));
  ci->size = 0;
  ci->calls = 0;
  ci->next = cinfos;
  cinfos = ci;
  return ci;
  
}
#endif

static void add_alloc(char *file, int line, int size)
{
  ainfo ai = find_ainfo(file, line);
  ai->calls++;
  ai->size += size;
#ifdef TRACE_STACK
  {
    cinfo ci;

    ci = find_cinfo();
    ci->calls++;
    ci->size += size;
  }
#endif
}

/**************************************************************************
 *                                                                        *
 * Intercept and log calls to region library                              *
 *                                                                        *
 **************************************************************************/

void *profile_typed_ralloc(region r, size_t size, type_t type, char *file,
			   int line)
{
  add_alloc(file, line, size);
  return typed_ralloc(r, size, type);
}

void *profile_typed_rarrayalloc(region r, size_t n, size_t size, type_t type,
				char *file, int line)
{
  add_alloc(file, line, n*size);
  return typed_rarrayalloc(r, n, size, type);
}

void *profile_typed_rarrayextend(region r, void *old, size_t n, size_t size,
				 type_t type, char *file, int line)
{
  add_alloc(file, line, n*size); /* XXX: Fix */
  return typed_rarrayextend(r, old, n, size, type);
}

char *profile_rstralloc(region r, size_t size, char *file, int line)
{
  add_alloc(file, line, size);
  return rstralloc(r, size);
}

char *profile_rstralloc0(region r, size_t size, char *file, int line)
{
  add_alloc(file, line, size);
  return rstralloc0(r, size);
}

char *profile_rstrdup(region r, const char *s, char *file, int line)
{
  add_alloc(file, line, strlen(s));
  return rstrdup(r, s);
}

char *profile_rstrextend(region r, const char *old, size_t newsize,
			 char *file, int line)
{
  add_alloc(file, line, newsize); /* XXX: Fix */
  return rstrextend(r, old, newsize);
}

char *profile_rstrextend0(region r, const char *old, size_t newsize,
			  char *file, int line)
{
  add_alloc(file, line, newsize); /* XXX: Fix */
  return rstrextend0(r, old, newsize);
}

/**************************************************************************
 *                                                                        *
 * Display results -- generic                                             *
 *                                                                        *
 **************************************************************************/

static FILE *out = NULL;

/* Generic list -- used for generic sorting.  Note that next field is
   at the top. */
typedef struct List
{
  struct List *next;
} *list;

/* Sort a list.  cmp should sort in reverse order. */
static list sort_list(list l, int (*cmp)(const void *, const void *))
{
  list cur, result;
  list *sorted;
  int i, length;
  region temp_region;

  /* Compute length of list */
  for (cur = l, length = 0; cur; cur = cur->next, length++);

  temp_region = newregion();
  sorted = rarrayalloc(temp_region, length, list *);
  for (cur = l, i = 0; cur; cur = cur->next)
    sorted[i++] = cur;
  qsort(sorted, length, sizeof(list *), cmp);

  result = NULL;
  for (i = 0; i < length; i++)
    {
      cur = result;
      result = sorted[i];
      result->next = cur;
    }
  deleteregion(temp_region);
  return result;
}


typedef struct File_info
{
  struct File_info *next;
  char *file;
  unsigned long size;
  unsigned long calls;
  unsigned long sites;
} *finfo;

static finfo finfos = NULL;

static int finfo_cmp(const void *a, const void *b)
{
  finfo *afi = (finfo *) a;
  finfo *bfi = (finfo *) b;
  return (*afi)->size - (*bfi)->size;  /* Reverse order */
}

static void print_finfos(void)
{
  finfo fi;
  unsigned long size, sites, calls;

  finfos = (finfo) sort_list((list) finfos, finfo_cmp);
  size = sites = calls = 0;
  fprintf(out, "        Bytes | Sites |    Calls | File\n");
  fprintf(out, "  ------------+-------+----------+---------------------\n");
  for (fi = finfos; fi; fi = fi->next)
    {
      size += fi->size;
      sites += fi->sites;
      calls += fi->calls;
      fprintf(out, " %12lu | %5lu | %8lu | %s\n",
	      fi->size, fi->sites, fi->calls, fi->file);
    }
  fprintf(out, "  ------------+-------+----------+---------------------\n");
    fprintf(out, " %12lu | %5lu | %8lu | Total\n",
	    size, sites, calls);

}

static int ainfo_cmp(const void *a, const void *b)
{
  ainfo *afi = (ainfo *) a;
  ainfo *bfi = (ainfo *) b;
  return (*afi)->size - (*bfi)->size;  /* Reverse order */
}

static void print_ainfos(void)
{
  ainfo ai;

  unsigned long size, calls;

  ainfos = (ainfo) sort_list((list) ainfos, ainfo_cmp);
  size = calls = 0;
  fprintf(out, "        Bytes |    Calls | Site\n");
  fprintf(out, "  ------------+----------+---------------------\n");
  for (ai = ainfos; ai; ai = ai->next)
    {
      size += ai->size;
      calls += ai->calls;
      fprintf(out, " %12lu | %8lu | %s:%d\n",
	      ai->size, ai->calls, ai->file, ai->line);
    }
  fprintf(out, "  ------------+----------+---------------------\n");
    fprintf(out, " %12lu | %8lu | Total\n",
	    size, calls);
}

static finfo find_finfo(char *file)
{
  finfo fi;

  for (fi = finfos; fi; fi = fi->next)
    if (!strcmp(file, fi->file))
      return fi;

  fi = ralloc(profile_region, struct File_info);
  fi->file = file;
  fi->size = 0;
  fi->calls = 0;
  fi->sites = 0;
  fi->next = finfos;
  finfos = fi;
  return fi;
}

static void gather_finfo(void)
{
  ainfo ai;

  for (ai = ainfos; ai; ai = ai->next)
    {
      finfo fi = find_finfo(ai->file);
      fi->size += ai->size;
      fi->calls += ai->calls;
      fi->sites++;
    }
}

/**************************************************************************
 *                                                                        *
 * Display results -- GCC                                                 *
 *                                                                        *
 **************************************************************************/

#ifdef TRACE_STACK

pid_t child_pid = 0;
int child_in[2], child_out[2]; /* pipes to child process */

static void start_prettiness(void)
{
  if (pipe(child_in) || pipe(child_out))
    pfail("Unable to open pipe to child process");
  if (!(child_pid = fork()))
    {
      /* Child process */
      pid_t parent_pid;
      char filename[64];

      if (dup2(child_in[0], STDIN_FILENO) == -1)
	pfail("Unable to open pipe from parent");
      close(child_in[0]);
      close(child_in[1]);
      if (dup2(child_out[1], STDOUT_FILENO) == -1)
	pfail("Unable to open pipe to parent");
      close(child_out[0]);
      close(child_out[1]);

      parent_pid = getppid();
      snprintf(filename, 64, "/proc/%d/exe", parent_pid);
      filename[63] = '\0';
      execlp("addr2line", "addr2line", "-s", "-e", filename, 0);
      fprintf(stderr, "Unable to fork addr2line\n");
      exit(EXIT_FAILURE);
    }
  else
    {
      close(child_in[0]);
      close(child_out[1]);
    }
}

/* Turn p into a file:line string */
static char *prettify(void *p)
{
#define BUFSIZE 1024
  static char buf[BUFSIZE];
  int size;

  /*printf("To child: %p\n", p);*/
  size = snprintf(buf, BUFSIZE, "%p\n", p);
  write(child_in[1], buf, size);
  size = read(child_out[0], buf, BUFSIZE - 1);
  if (!size)
    pfail("Unable to read from child process");
  buf[size-1] = '\0'; /* Kill \n */
  /*printf("Read: [%s]\n", buf);*/
  return buf;
}

static void end_prettiness(void)
{
  if (child_pid)
    kill(child_pid, SIGHUP);
}

static int cinfo_cmp(const void *a, const void *b)
{
  cinfo *aci = (cinfo *) a;
  cinfo *bci = (cinfo *) b;
  return (*aci)->size - (*bci)->size;  /* Reverse order */
}

/* Print the call chain information out to a file. */
static void print_cinfos(void)
{
  cinfo ci;
  unsigned long size, calls;
  int i;

  cinfos = (cinfo) sort_list((list) cinfos, cinfo_cmp);
  size = calls = 0;
  start_prettiness();
  fprintf(out, "        Bytes |    Calls | Call Stack\n");
  fprintf(out, "  ------------+----------+---------------------\n");
  for (ci = cinfos; ci; ci = ci->next)
    {
      size += ci->size;
      calls += ci->calls;
      fprintf(out, " %12lu | %8lu | ", ci->size, ci->calls);
      for (i = 0; i < REGION_PROFILE_DEPTH; i++)
	fprintf(out, "%s ", prettify(ci->stack[i]));
      fprintf(out, "\n");
    }
  fprintf(out, "  ------------+----------+---------------------\n");
    fprintf(out, " %12lu | %8lu | Total\n",
	    size, calls);
    end_prettiness();
}
#endif


void profile(void)
{
  if (profile_region == NULL)
    return;

  gather_finfo();

  if (!(out = fopen("profile.out", "w")))
    pfail("Unable to open profile.out");

  fprintf(out, "---------------------------\n");
  fprintf(out, "Region Library Memory Usage\n");
  fprintf(out, "---------------------------\n\n");

  print_finfos();
  fprintf(out, "\n");
  print_ainfos();
#ifdef TRACE_STACK
  fprintf(out, "\n");
  print_cinfos();
#endif

  fclose(out);
}
