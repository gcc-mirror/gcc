/* MPW-Unix compatibility library.
   Copyright (C) 1993, 1994, 1995, 1996 Free Software Foundation, Inc.

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This should only be compiled and linked under MPW. */

#include "mpw.h"

#include <stdlib.h>

#ifndef USE_MW_HEADERS
#include <sys/time.h>
#include <sys/resource.h>
#endif

#include <Types.h>
#include <Files.h>

#include <Timer.h>

/* Initialize to 0 at first, then set to errno_max() later.  */

int sys_nerr = 0;

/* Debug flag for pathname hacking.  Set this to one and rebuild. */

int DebugPI = -1;

void
mpwify_filename(char *unixname, char *macname)
{
  int i, j;

  /* (should truncate 255 chars from end of name, not beginning) */
  if (strlen (unixname) > 255)
    {
      fprintf (stderr, "Pathname \"%s\" is too long for Macs, truncating\n",
	       unixname);
    }
  j = 0;
  /* If you're going to end up with one or more colons in the middle of a
     path after an all-Unix relative path is translated, you must add a
     colon on the front, so that the first component is not thought to be
     a disk name.  */
  if (unixname[0] != '/' && ! strchr (unixname, ':') && strchr (unixname, '/'))
    {
      macname[j++] = ':';
    }
  for (i = 0; unixname[i] != '\0' && i < 255; ++i)
    {
      if (i == 0 && unixname[i] == '/')
	{
	  if (strncmp (unixname, "/tmp/", 5) == 0)
	    {
	      /* A temporary name, make a more Mac-flavored tmpname. */
	      /* A better choice would be {Boot}Trash:foo, but
		 that would require being able to identify the
		 boot disk's and trashcan's name.  Another option
		 would be to have an env var, so user can point it
		 at a ramdisk. */
	      macname[j++] = ':';
	      macname[j++] = 't';
	      macname[j++] = 'm';
	      macname[j++] = 'p';
	      macname[j++] = '_';
	      i += 4;
	    }
	  else
	    {
	      /* Don't copy the leading slash. */
	    }
	}
      else if (unixname[i] == ':' && unixname[i+1] == '/')
	{
	  macname[j++] = ':';
	  i += 1;
	}
      else if (unixname[i] == '.' && unixname[i+1] == '/')
	{
	  macname[j++] = ':';
	  i += 1;
	}
      else if (unixname[i] == '.' && unixname[i+1] == '.' && unixname[i+2] == '/')
	{
	  macname[j++] = ':';
	  macname[j++] = ':';
	  i += 2;
	}
      else if (unixname[i] == '/')
	{
	  macname[j++] = ':';
	}
      else
	{
	  macname[j++] = unixname[i];
	}
    }
  macname[j] = '\0';
  /* Allow for getting the debug flag from an env var; quite useful. */
  if (DebugPI < 0)
    DebugPI = (*(getenv ("DEBUG_PATHNAMES")) == '1' ? 1 : 0);
  if (DebugPI)
    {
      fprintf (stderr, "# Made \"%s\"\n", unixname);
      fprintf (stderr, "# into \"%s\"\n", macname);
    }
}

/* MPW-flavored basename finder. */

char *
mpw_basename (name)
  char *name;
{
  char *base = name;

  while (*name)
    {
      if (*name++ == ':')
	{
	  base = name;
	}
    }
  return base;
}

/* Mixed MPW/Unix basename finder.  This can be led astray by
   filenames with slashes in them and come up with a basename that
   either corresponds to no file or (worse) to some other file, so
   should only be tried if other methods of finding a file via a
   basename have failed.  */

char *
mpw_mixed_basename (name)
  char *name;
{
  char *base = name;

  while (*name)
    {
      if (*name == '/' || *name == ':')
	{
	  base = name + 1;
	}
      ++name;
    }
  return base;
}

/* This function is fopen() modified to create files that are type TEXT
   or 'BIN ', and always of type 'MPS '.  */

FILE *
mpw_fopen (char *name, char *mode)
{
#undef fopen
  int errnum;
  FILE *fp;
  char tmpname[256];

  mpwify_filename (name, tmpname);
  PROGRESS (1);
  fp = fopen (tmpname, mode);
  errnum = errno;

  /* If writing, need to set type and creator usefully. */
  if (strchr (mode, 'w'))
    {
      char *pname = (char *) malloc (strlen (tmpname) + 2);
      OSErr e;
      struct FInfo fi;

      pname[0] = strlen (tmpname);
      strcpy (pname+1, tmpname);
	
      e = GetFInfo ((ConstStr255Param) pname, 0, &fi);
      /* should do spiffier error handling */
      if (e != 0)
	fprintf(stderr, "GetFInfo returns %d\n", e);
      if (strchr (mode, 'b'))
	{
	  fi.fdType = (OSType) 'BIN ';
	}
      else
	{
	  fi.fdType = (OSType) 'TEXT';
	}
      fi.fdCreator = (OSType) 'MPS ';
      e = SetFInfo ((ConstStr255Param) pname, 0, &fi);
      if (e != 0)
	fprintf(stderr, "SetFInfo returns %d\n", e);
      free (pname);
    }
  if (fp == NULL)
    errno = errnum;
  return fp;
}

/* This is a version of fseek() modified to fill the file with zeros
   if seeking past the end of it.  */

#define ZEROBLKSIZE 4096

char zeros[ZEROBLKSIZE];

int
mpw_fseek (FILE *fp, int offset, int whence)
{
#undef fseek
  int cursize, numleft;

  PROGRESS (1);
  if (whence == SEEK_SET)
    {
      fseek (fp, 0, SEEK_END);
      cursize = ftell (fp);
      if (offset > cursize)
	{
	  numleft = offset - cursize;
	  while (numleft > ZEROBLKSIZE)
	    {
	      /* This might fail, should check for that. */
	      PROGRESS (1);
	      fwrite (zeros, 1, ZEROBLKSIZE, fp);
	      numleft -= ZEROBLKSIZE;
	    }
	  PROGRESS (1);
	  fwrite (zeros, 1, numleft, fp);
	  fflush (fp);
	}
    }
  return fseek (fp, offset, whence);
}

int
mpw_fread (char *ptr, int size, int nitems, FILE *stream)
{
#undef fread
  int rslt;

  PROGRESS (1);
  rslt = fread (ptr, size, nitems, stream);
  PROGRESS (1);
  return rslt;
}

int
mpw_fwrite (char *ptr, int size, int nitems, FILE *stream)
{
#undef fwrite
  int rslt;

  PROGRESS (1);
  rslt = fwrite (ptr, size, nitems, stream);
  PROGRESS (1);
  return rslt;
}

int
link ()
{
  fprintf (stderr, "link not available!\n");
  mpw_abort ();
}

int
fork ()
{
  fprintf (stderr, "fork not available!\n");
  mpw_abort ();
}

int
vfork ()
{
  fprintf (stderr, "vfork not available!\n");
  mpw_abort ();
  return (-1);
}

int
pipe (int *fd)
{
  fprintf (stderr, "pipe not available!\n");
  mpw_abort ();
  return (-1);
}

#ifndef USE_MW_HEADERS
int
execvp (char *file, char **argv)
{
  fprintf (stderr, "execvp not available!\n");
  mpw_abort ();
  return (-1);
}

int
execv (char *path, char **argv)
{
  fprintf (stderr, "execv not available!\n");
  mpw_abort ();
  return (-1);
}
#endif

int
kill (int pid, int sig)
{
  fprintf (stderr, "kill not available!\n");
  mpw_abort ();
  return (-1);
}

int
wait (int *status)
{
  *status = 0;
  return 0;
}

#ifndef USE_MW_HEADERS
int
sleep (int seconds)
{
  unsigned long start_time, now;

  time (&start_time);

  while (1)
    {
      PROGRESS (1);
      time (&now);
      if (now > start_time + seconds)
	return 0;
    }
}
#endif

void
putenv (char *str)
{
  /* The GCC driver calls this to do things for collect2, but we
     don't care about collect2. */
}

int
chmod (char *path, int mode)
{
  /* Pretend it was all OK. */
  return 0;
}

#ifndef USE_MW_HEADERS
int
getuid ()
{
  /* One value is as good as another... */
  return 0;
}

int
getgid ()
{
  /* One value is as good as another... */
  return 0;
}
#endif

/* Instead of coredumping, which is not a normal Mac facility, we
   drop into Macsbug.  If we then "g" from Macsbug, the program will
   exit cleanly. */

void
mpw_abort ()
{
  /* Make sure no output still buffered up, then zap into MacsBug. */
  fflush(stdout);
  fflush(stderr);
  printf("## Abort! ##\n");
#ifdef MPW_SADE
  SysError(8005);
#else 
  Debugger();
#endif
  /* "g" in MacsBug will then cause a regular error exit. */
  exit (1);
}

/* Imitation getrusage based on the ANSI clock() function. */

int
getrusage (int who, struct rusage *rusage)
{
  int clk = clock ();

#if 0
  rusage->ru_utime.tv_sec = clk / CLOCKS_PER_SEC;
  rusage->ru_utime.tv_usec = ((clk * 1000) / CLOCKS_PER_SEC) * 1000;
  rusage->ru_stime.tv_sec = 0;
  rusage->ru_stime.tv_usec = 0;
#endif
}

int
sbrk ()
{
  return 0;
}

#ifndef USE_MW_HEADERS
int
isatty (int fd)
{
  return 0;
}

/* This is inherited from Timothy Murray's Posix library. */

#include "utime.h"

int
utime (char *filename, struct utimbuf *times)
{
  CInfoPBRec cipbr;
  HFileInfo *fpb = (HFileInfo *) &cipbr;
  DirInfo *dpb = (DirInfo *) &cipbr;
  unsigned char pname[256];
  short err;
  
  strcpy ((char *) pname, filename);
  c2pstr (pname);

  dpb->ioDrDirID = 0L;
  fpb->ioNamePtr = pname;
  fpb->ioVRefNum = 0;
  fpb->ioFDirIndex = 0;
  fpb->ioFVersNum = 0;
  err = PBGetCatInfo (&cipbr, 0);
  if (err != noErr) {
    errno = ENOENT;
    return -1;
  }
  dpb->ioDrDirID = 0L;
  fpb->ioFlMdDat = times->modtime;
  fpb->ioFlCrDat = times->actime;
  err = PBSetCatInfo (&cipbr, 0);
  if (err != noErr) {
    errno = EACCES;
    return -1;
  }
  return 0;
}

int
mkdir (char *path, int mode)
{
  errno = ENOSYS;
  return -1;
}

int
rmdir ()
{
  errno = ENOSYS;
  return -1;
}
#endif

chown ()
{
  errno = ENOSYS;
  return -1;
}

char *myenviron[] = {NULL};

char **environ = myenviron;

#ifndef USE_MW_HEADERS

/* Minimal 'stat' emulation: tells directories from files and
   gives length and mtime.

   Derived from code written by Guido van Rossum, CWI, Amsterdam
   and placed by him in the public domain.  */

extern int __uid, __gid;

int __uid = 0;
int __gid = 0;

/* Bits in ioFlAttrib: */
#define LOCKBIT	(1<<0)		/* File locked */
#define DIRBIT	(1<<4)		/* It's a directory */

/* Macified "stat" in which filename is given relative to a directory,
   specified by long DirID.  */

static int
_stat (char *name, long dirid, struct stat *buf)
{
  CInfoPBRec cipbr;
  HFileInfo *fpb = (HFileInfo*) &cipbr;
  DirInfo *dpb = (DirInfo*) &cipbr;
  Str255 pname;
  short err;

  /* Make a temp copy of the name and pascalize. */
  strcpy ((char *) pname, name);
  c2pstr (pname);
  
  cipbr.dirInfo.ioDrDirID = dirid;
  cipbr.hFileInfo.ioNamePtr = pname;
  cipbr.hFileInfo.ioVRefNum = 0;
  cipbr.hFileInfo.ioFDirIndex = 0;
  cipbr.hFileInfo.ioFVersNum = 0;
  err = PBGetCatInfo (&cipbr, 0);
  if (err != noErr)
    {
      errno = ENOENT;
      return -1;
    }
  /* Mac files are readable if they can be accessed at all. */
  buf->st_mode = 0444;
  /* Mark unlocked files as writeable. */
  if (!(fpb->ioFlAttrib & LOCKBIT))
    buf->st_mode |= 0222;
  if (fpb->ioFlAttrib & DIRBIT)
    {
      /* Mark directories as "executable". */
      buf->st_mode |= 0111 | S_IFDIR;
      buf->st_size = dpb->ioDrNmFls;
      buf->st_rsize = 0;
    }
  else
    {
      buf->st_mode |= S_IFREG;
      /* Mark apps as "executable". */
      if (fpb->ioFlFndrInfo.fdType == 'APPL')
	buf->st_mode |= 0111;
      /* Fill in the sizes of data and resource forks. */
      buf->st_size = fpb->ioFlLgLen;
      buf->st_rsize = fpb->ioFlRLgLen;
    }
  /* Fill in various times. */
  buf->st_atime = fpb->ioFlCrDat;
  buf->st_mtime = fpb->ioFlMdDat;
  buf->st_ctime = fpb->ioFlCrDat;
  /* Set up an imitation inode number. */
  buf->st_ino = (unsigned short) fpb->ioDirID;
  /* Set up an imitation device. */
  GetVRefNum (buf->st_ino, &buf->st_dev);
  buf->st_uid = __uid;
  buf->st_gid = __gid;
/*  buf->st_FlFndrInfo = fpb->ioFlFndrInfo;  */
  return 0;
}

/* stat() sets up an empty dirid. */

int
stat (char *path, struct stat *buf)
{
  long rslt, errnum;
  char tmpname[256];

  mpwify_filename (path, tmpname);
  if (DebugPI)
    fprintf (stderr, "# stat (%s, %x)", tmpname, buf);
  PROGRESS (1);
  rslt = _stat (tmpname, 0L, buf);
  errnum = errno;
  if (DebugPI)
    {
      fprintf (stderr, " -> %d", rslt);
      if (rslt != 0)
	fprintf (stderr, " (errno is %d)", errnum);
      fprintf (stderr, "\n");
      fflush (stderr);
    }
  if (rslt != 0)
    errno = errnum;
  return rslt;
}

int
fstat (int fd, struct stat *buf)
{
  FCBPBRec fcb;
  FILE *fp;
  Str255 pathname;
  long dirid = 0L, temp;
  long rslt, errnum;
  short err;

  if (DebugPI < 0)
    DebugPI = (*(getenv ("DEBUG_PATHNAMES")) == '1' ? 1 : 0);
  if (DebugPI)
    fprintf (stderr, "# fstat (%d, %x)", fd, buf);
  PROGRESS (1);
  pathname[0] = 0;
#ifdef FIOFNAME
  /* Use an MPW-specific ioctl to get the pathname associated with
     the file descriptor.  */
  ioctl (fd, FIOFNAME, (long *) pathname); 
#else
  you lose
#endif
  if (DebugPI)
    fprintf (stderr, " (name is %s)", pathname);
  dirid = 0L /* fcb.ioFCBParID */ ;
  rslt = _stat ((char *) pathname, dirid, buf);
  errnum = errno;
  if (DebugPI)
    {
      fprintf (stderr, " -> %d", rslt);
      if (rslt != 0)
	fprintf (stderr, " (errno is %d)", errnum);
      fprintf (stderr, "\n");
      fflush (stderr);
    }
  if (rslt != 0)
    errno = errnum;
  return rslt;
}

#endif /* n USE_MW_HEADERS */

chdir ()
{
  errno = ENOSYS;
  return (-1);
}

char *
getcwd (char *buf, int size)
{
  if (buf == NULL)
    buf = (char *) malloc (size);
  strcpy(buf, ":");
  return buf;
}

/* This should probably be more elaborate for MPW. */

char *
getpwd ()
{
  return ":";
}

int
mpw_open (char *filename, int arg2, int arg3)
{
#undef open
  int fd, errnum = 0;
  char tmpname[256];

  mpwify_filename (filename, tmpname);
  fd = open (tmpname, arg2);
  errnum = errno;

  if (DebugPI)
    {
      fprintf (stderr, "# open (%s, %d, %d)", tmpname, arg2, arg3);
      fprintf (stderr, " -> %d", fd);
      if (fd == -1)
	fprintf (stderr, " (errno is %d)", errnum);
      fprintf (stderr, "\n");
    }
  if (fd == -1)
    errno = errnum;
  return fd;
}

int
mpw_access (char *filename, unsigned int cmd)
{
#undef access

  int rslt, errnum = 0;
  struct stat st;
  char tmpname[256];

  mpwify_filename (filename, tmpname);
  if (cmd & R_OK || cmd & X_OK)
    {
      rslt = stat (tmpname, &st);
      errnum = errno;
      if (rslt >= 0)
	{
	  if ((((st.st_mode & 004) == 0) && (cmd & R_OK))
	      || (((st.st_mode & 002) == 0) && (cmd & W_OK))
	      || (((st.st_mode & 001) == 0) && (cmd & X_OK)))
	    {
	      rslt = -1;
	      errnum = EACCES;
	    }
	}
    }
  if (DebugPI)
    {
      fprintf (stderr, "# mpw_access (%s, %d)", tmpname, cmd);
      fprintf (stderr, " -> %d", rslt);
      if (rslt != 0)
	fprintf (stderr, " (errno is %d)", errnum);
      fprintf (stderr, "\n");
    }
  if (rslt != 0)
    errno = errnum;
  return rslt;
}

/* The MPW library creat() has no mode argument. */

int
mpw_creat (char *path, /* mode_t */ int mode)
{
#undef creat

#ifdef USE_MW_HEADERS
  return creat (path, mode);
#else
  return creat (path);
#endif
}

/* This is a hack to get control in an MPW tool before it crashes the
   machine.  */

mpw_special_init (name)
     char *name;
{
  if (strstr (name, "DEBUG"))
    DebugStr("\pat beginning of program");
}

static int current_umask;

int
umask(int mask)
{
  int oldmask = current_umask;

  current_umask = mask;
  return oldmask;
}

/* Cursor-spinning stuff that includes metering of spin rate and delays.  */

/* Nonzero when cursor spinning has been set up properly.  */

int cursor_inited;

/* Nonzero if spin should be measured and excessive delays reported.  */

int measure_spin;

/* Nonzero if spin histogram and rate data should be written out.  */

int dump_spin_data;

long warning_threshold = 400000;

long bucket_size = 1024;

long bucket_power = 10;

long numbuckets = 300;

int *delay_counts;

int overflow_count;

char *current_progress;

static UnsignedWide last_microseconds;

static char *last_spin_file = "";

static int last_spin_line;

void
warn_if_spin_delay (char *file, int line)
{
  long diff, ix;
  UnsignedWide now;

  Microseconds(&now);

  diff = now.lo - last_microseconds.lo;

  if (diff > warning_threshold)
    fprintf (stderr, "# %s: %ld.%06ld sec delay getting from %s:%d to %s:%d\n",
	     (current_progress ? current_progress : ""),
	     diff / 1000000, diff % 1000000,
	     last_spin_file, last_spin_line, file, line);
  if (dump_spin_data)
    {
      if (diff >= 0)
	{
	  ix = diff >> bucket_power;
	  if (ix >= 0 && ix < numbuckets && delay_counts != NULL)
	    ++delay_counts[ix];
	  else
	    ++overflow_count;
	}
      else
	fprintf (stderr, "raw diff is %ld (?)\n", diff);
    }
}

void
record_for_spin_delay (char *file, int line)
{
  Microseconds (&last_microseconds);
  last_spin_file = file;
  last_spin_line = line;
}

void
mpw_start_progress (char *str, int n, char *file, int line)
{
  int i;
  char *measure, *threshold;

  if (!cursor_inited)
    {
      InitCursorCtl (nil);
      cursor_inited = 1;
      record_for_spin_delay (file, line);
      measure = getenv ("MEASURE_SPIN");
      if (measure != NULL && measure[0] != '\0')
	{
	  measure_spin = 1;
	  if (strcmp (measure, "all") == 0)
	    dump_spin_data = 1;
	}
      threshold = getenv ("SPIN_WARN_THRESHOLD");
      if (threshold != NULL && threshold[0] != '\0')
	warning_threshold = atol (threshold);
      if (dump_spin_data)
	{
	  if (delay_counts == NULL)
	    delay_counts = (int *) malloc (numbuckets * sizeof (int));
	  for (i = 0; i < numbuckets; ++i)
	    delay_counts[i] = 0;
	  overflow_count = 0;
	}
    }
  current_progress = str;

  sys_nerr = errno_max ();

  mpw_special_init (str);
}

void
mpw_progress (int n)
{
  SpinCursor (32);
}

void
mpw_progress_measured (int n, char *file, int line)
{
  if (measure_spin)
    warn_if_spin_delay (file, line);
  SpinCursor (32);
  if (measure_spin)
    record_for_spin_delay (file, line);
}

void
mpw_end_progress (char *str, char *file, int line)
{
  long i, delay, count = 0, sum = 0, avgdelay, spinrate;
  long curpower = 0, curgroup = 0;

  /* Warn if it's been a while since the last spin.  */
  if (measure_spin)
    warn_if_spin_delay (file, line);

  /* Dump all the nonzero delay counts and an approximation of the delay.  */
  if (dump_spin_data && delay_counts != NULL)
    {
      for (i = 0; i < numbuckets; ++i)
	{
	  delay = (i + 1) * bucket_size;
	  sum += delay_counts[i] * (i + 1);
	  count += delay_counts[i];
	  if (delay <= (1 << curpower))
	    {
	      curgroup += delay_counts[i];
	    }
	  else
	    {
	      if (curgroup > 0)
		fprintf (stderr,
			 "# %s: %d delays between %ld.%06ld and %ld.%06ld sec\n",
			 (str ? str : ""),
			 curgroup,
			 (1 << curpower) / 1000000,
			 (1 << curpower) % 1000000,
			 (1 << (curpower + 1)) / 1000000,
			 (1 << (curpower + 1)) % 1000000);
	      ++curpower;
	      curgroup = 0;
	    }
	}
      if (count > 0)
	{
	  avgdelay = (sum * bucket_size) / count;
	  spinrate = 1000000 / avgdelay;
	  fprintf (stderr, "# %s: Average spin rate is %d times/sec\n",
		   (str ? str : ""), spinrate);
	}
    }
}

#ifdef PROGRESS_TEST

/* Test program.  */

main ()
{
  int i, j;
  double x = 1.0, y = 2.4;
  long start = Microseconds (), tm;  FIXME

  START_PROGRESS ("hi", 0);

  for (i = 0; i < 1000; ++i)
    {
      PROGRESS (1);

      for (j = 0; j < (i * 100); ++j)
	{
	  x += (x * y) / j;
	}
    }
  
  END_PROGRESS ("hi");
  
  tm = Microseconds () - start;

  printf ("Total time is %d.%d secs\n", tm / 1000000, tm % 1000000);
}

#endif

#ifdef USE_MW_HEADERS
/* Empty definitions for Metrowerks' SIOUX console library. */

#ifndef __CONSOLE__
#include <console.h>
#endif

short
InstallConsole(short fd)
{
#pragma unused (fd)
	return 0;
}

void
RemoveConsole(void)
{
}

long
WriteCharsToConsole(char *buf, long n)
{
#pragma unused (buf, n)
	return 0;
}

long ReadCharsFromConsole(char *buf, long n)
{
#pragma unused (buf, n)
	return 0;
}

extern char *
__ttyname(long fd)
{
	static char *__devicename = "null device";

	if (fd >= 0 && fd <= 2)
	  return (__devicename);
	return NULL;
}

#endif
