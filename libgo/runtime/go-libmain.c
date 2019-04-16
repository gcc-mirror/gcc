/* go-libmain.c -- the startup function for a Go library.

   Copyright 2015 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include "config.h"

#include <errno.h>
#include <pthread.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "runtime.h"
#include "array.h"
#include "arch.h"

#if defined(__sun) && defined(__SVR4)

/* Read a file into memory on Solaris, returning an malloc'ed buffer
   and setting *SIZE to its size.  */

static char *
read_file (const char *fn, size_t *size)
{
  struct stat st;
  char *buf;
  int o;
  ssize_t got;

  if (stat (fn, &st) < 0)
    return NULL;
  buf = malloc ((size_t) st.st_size);
  if (buf == NULL)
    return NULL;
  o = open (fn, O_RDONLY);
  if (o < 0)
    {
      free (buf);
      return NULL;
    }
  got = read (o, buf, st.st_size);
  close (o);
  if (got != st.st_size)
    {
      free (buf);
      return NULL;
    }

  *size = (size_t) got;
  return buf;
}

/* On Solaris we don't get passed argc/argv, but we can fetch it from
   /proc/PID/cmdline.  */

static void
read_cmdline (int *argc, char ***argv)
{
  pid_t pid;
  char fn[50];
  char *argbuf;
  size_t argsize;
  char *envbuf;
  size_t envsize;
  char *p;
  int i;
  int ac;

  *argc = 0;
  *argv = NULL;

  pid = getpid ();
  snprintf (fn, sizeof fn, "/proc/%ld/cmdline", (long) pid);
  argbuf = read_file (fn, &argsize);
  if (argbuf == NULL)
    return;

  snprintf (fn, sizeof fn, "/proc/%ld/environ", (long) pid);
  envbuf = read_file (fn, &envsize);
  if (envbuf == NULL)
    {
      free (argbuf);
      return;
    }

  i = 0;
  for (p = argbuf; p < argbuf + argsize; p++)
    if (*p == '\0')
      ++i;
  ac = i;
  ++i; // For trailing NULL.
  for (p = envbuf; p < envbuf + envsize; p++)
    if (*p == '\0')
      ++i;
  ++i; // For trailing NULL.

  *argv = (char **) malloc (i * sizeof (char *));
  if (*argv == NULL)
    {
      free (argbuf);
      free (envbuf);
      return;
    }

  *argc = ac;
  (*argv)[0] = argbuf;
  i = 0;
  for (p = argbuf; p < argbuf + argsize; p++)
    {
      if (*p == '\0')
	{
	  ++i;
	  (*argv)[i] = p + 1;
	}
    }
  (*argv)[i] = NULL;
  ++i;
  (*argv)[i] = envbuf;
  for (p = envbuf; p < envbuf + envsize; p++)
    {
      if (*p == '\0')
	{
	  ++i;
	  (*argv)[i] = p + 1;
	}
    }
  (*argv)[i] = NULL;
}

#endif /* defined(__sun) && defined(__SVR4) */

/* This is used when building a standalone Go library using the Go
   command's -buildmode=c-archive or -buildmode=c-shared option.  It
   starts up the Go code as a global constructor but does not take any
   other action.  The main program is written in some other language
   and calls exported Go functions as needed.  */

static void die (const char *, int);
/* .init_array section does not exist in AIX XCOFF.
   -Wl,-binitfini:__go_init option will be required to build go
   libraries and make sure __go_init is called when the library is
   loaded. This requires __go_init to be exported.  */

void __go_init (int, char **, char **);
static void *gostart (void *);

/* Used to pass arguments to the thread that runs the Go startup.  */

struct args {
  int argc;
  char **argv;
};

#ifndef _AIX
/* We use .init_array so that we can get the command line arguments.
   This obviously assumes .init_array support; different systems may
   require other approaches.  */

typedef void (*initarrayfn) (int, char **, char **);

static initarrayfn initarray[1]
__attribute__ ((section (".init_array"), used)) =
  { __go_init };
#endif

/* This function is called at program startup time.  It starts a new
   thread to do the actual Go startup, so that program startup is not
   paused waiting for the Go initialization functions.  Exported cgo
   functions will wait for initialization to complete if
   necessary.  */

void
__go_init (int argc, char **argv, char** env __attribute__ ((unused)))
{
  int err;
  pthread_attr_t attr;
  struct args *a;
  pthread_t tid;

#if defined(__sun) && defined(__SVR4)
  read_cmdline (&argc, &argv);
#endif

  runtime_isarchive = true;

  setIsCgo ();
  runtime_cpuinit ();
  runtime_initsig(true);

  a = (struct args *) malloc (sizeof *a);
  if (a == NULL)
    die ("malloc", errno);
  a->argc = argc;
  a->argv = argv;

  err = pthread_attr_init (&attr);
  if (err != 0)
    die ("pthread_attr_init", err);
  err = pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED);
  if (err != 0)
    die ("pthread_attr_setdetachstate", err);

  err = pthread_create (&tid, &attr, gostart, (void *) a);
  if (err != 0)
    die ("pthread_create", err);

  err = pthread_attr_destroy (&attr);
  if (err != 0)
    die ("pthread_attr_destroy", err);
}

/* Start up the Go runtime.  */

static void *
gostart (void *arg)
{
  struct args *a = (struct args *) arg;

  if (runtime_isstarted)
    return NULL;
  runtime_isstarted = true;

  runtime_check ();
  runtime_args (a->argc, (byte **) a->argv);
  setncpu (getproccount ());
  setpagesize (getpagesize ());
  runtime_sched = runtime_getsched();
  runtime_schedinit ();
  __go_go ((uintptr)(runtime_main), NULL);
  runtime_mstart (runtime_m ());
  abort ();
}

/* If something goes wrong during program startup, crash.  There is no
   way to report failure and nobody to whom to report it.  */

static void
die (const char *fn, int err)
{
  fprintf (stderr, "%s: %d\n", fn, err);
  exit (EXIT_FAILURE);
}
