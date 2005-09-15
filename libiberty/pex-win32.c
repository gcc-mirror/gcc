/* Utilities to execute a program in a subprocess (possibly linked by pipes
   with other subprocesses), and wait for it.  Generic Win32 specialization.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2003, 2004, 2005
   Free Software Foundation, Inc.

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
License along with libiberty; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "pex-common.h"

#include <windows.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include <process.h>
#include <io.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/stat.h>

/* mingw32 headers may not define the following.  */

#ifndef _P_WAIT
#  define _P_WAIT	0
#  define _P_NOWAIT	1
#  define _P_OVERLAY	2
#  define _P_NOWAITO	3
#  define _P_DETACH	4

#  define WAIT_CHILD		0
#  define WAIT_GRANDCHILD	1
#endif

#define MINGW_NAME "Minimalist GNU for Windows"
#define MINGW_NAME_LEN (sizeof(MINGW_NAME) - 1)

/* Ensure that the executable pathname uses Win32 backslashes. This
   is not necessary on NT, but on W9x, forward slashes causes
   failure of spawn* and exec* functions (and probably any function
   that calls CreateProcess) *iff* the executable pathname (argv[0])
   is a quoted string.  And quoting is necessary in case a pathname
   contains embedded white space.  You can't win.  */
static void
backslashify (char *s)
{
  while ((s = strchr (s, '/')) != NULL)
    *s = '\\';
  return;
}

/* This is a kludge to get around the Microsoft C spawn functions' propensity
   to remove the outermost set of double quotes from all arguments.  */

static const char * const *
fix_argv (char * const *argvec)
{
  char **argv;
  int i;
  char *command0;

  /* See whether we need to change anything.  */
  for (command0 = argvec[0]; *command0 != '\0'; command0++)
    if (*command0 == '/')
      break;
  if (*command0 == '\0')
    {
      for (i = 1; argvec[i] != NULL; i++)
	if (strpbrk (argvec[i], "\" \t") != NULL)
	  break;

      if (argvec[i] == NULL)
	return (const char * const *) argvec;
    }

  for (i = 0; argvec[i] != NULL; i++)
    ;
  argv = XNEWVEC (char *, i + 2);

  argv++;	/* Leave space at the beginning of argv
		   for potential #! handling */

  for (i = 0; argvec[i] != NULL; i++)
    argv[i] = xstrdup (argvec[i]);
  argv[i] = NULL;

  backslashify (argv[0]);

  for (i = 1; argv[i] != 0; i++)
    {
      int len, j;
      char *temp, *newtemp;

      temp = argv[i];
      len = strlen (temp);
      for (j = 0; j < len; j++)
        {
          if (temp[j] == '"')
            {
              newtemp = XNEWVEC (char, len + 2);
              strncpy (newtemp, temp, j);
              newtemp [j] = '\\';
              strncpy (&newtemp [j+1], &temp [j], len-j);
              newtemp [len+1] = 0;
              temp = newtemp;
              len++;
              j++;
            }
        }

      if (argv[i] != temp)
	{
	  free (argv[i]);
	  argv[i] = temp;
	}
    }

  for (i = 0; argv[i] != 0; i++)
    {
      if (strpbrk (argv[i], " \t"))
        {
	  int len, trailing_backslash;
	  char *temp;

	  len = strlen (argv[i]);
	  trailing_backslash = 0;

	  /* There is an added complication when an arg with embedded white
	     space ends in a backslash (such as in the case of -iprefix arg
	     passed to cpp). The resulting quoted strings gets misinterpreted
	     by the command interpreter -- it thinks that the ending quote
	     is escaped by the trailing backslash and things get confused.
	     We handle this case by escaping the trailing backslash, provided
	     it was not escaped in the first place.  */
	  if (len > 1
	      && argv[i][len-1] == '\\'
	      && argv[i][len-2] != '\\')
	    {
	      trailing_backslash = 1;
	      ++len;			/* to escape the final backslash. */
	    }

	  len += 2;			/* and for the enclosing quotes. */

	  temp = XNEWVEC (char, len + 1);
	  temp[0] = '"';
	  strcpy (temp + 1, argv[i]);
	  if (trailing_backslash)
	    temp[len - 2] = '\\';
	  temp[len - 1] = '"';
	  temp[len] = '\0';

	  free (argv[i]);
	  argv[i] = temp;
	}
    }

  return (const char * const *) argv;
}

static int pex_win32_open_read (struct pex_obj *, const char *, int);
static int pex_win32_open_write (struct pex_obj *, const char *, int);
static long pex_win32_exec_child (struct pex_obj *, int, const char *,
				  char * const *, int, int, int,
				  const char **, int *);
static int pex_win32_close (struct pex_obj *, int);
static int pex_win32_wait (struct pex_obj *, long, int *,
			   struct pex_time *, int, const char **, int *);
static int pex_win32_pipe (struct pex_obj *, int *, int);
static FILE *pex_win32_fdopenr (struct pex_obj *, int, int);

/* The list of functions we pass to the common routines.  */

const struct pex_funcs funcs =
{
  pex_win32_open_read,
  pex_win32_open_write,
  pex_win32_exec_child,
  pex_win32_close,
  pex_win32_wait,
  pex_win32_pipe,
  pex_win32_fdopenr,
  NULL /* cleanup */
};

/* Return a newly initialized pex_obj structure.  */

struct pex_obj *
pex_init (int flags, const char *pname, const char *tempbase)
{
  return pex_init_common (flags, pname, tempbase, &funcs);
}

/* Open a file for reading.  */

static int
pex_win32_open_read (struct pex_obj *obj ATTRIBUTE_UNUSED, const char *name,
		     int binary)
{
  return _open (name, _O_RDONLY | (binary ? _O_BINARY : _O_TEXT));
}

/* Open a file for writing.  */

static int
pex_win32_open_write (struct pex_obj *obj ATTRIBUTE_UNUSED, const char *name,
		      int binary)
{
  /* Note that we can't use O_EXCL here because gcc may have already
     created the temporary file via make_temp_file.  */
  return _open (name,
		(_O_WRONLY | _O_CREAT | _O_TRUNC
		 | (binary ? _O_BINARY : _O_TEXT)),
		_S_IREAD | _S_IWRITE);
}

/* Close a file.  */

static int
pex_win32_close (struct pex_obj *obj ATTRIBUTE_UNUSED, int fd)
{
  return _close (fd);
}

#ifdef USE_MINGW_MSYS
static const char *mingw_keys[] = {"SOFTWARE", "Microsoft", "Windows", "CurrentVersion", "Uninstall", NULL};

/* Tack the executable on the end of a (possibly slash terminated) buffer
   and convert everything to \. */
static const char *
tack_on_executable (char *buf, const char *executable)
{
  char *p = strchr (buf, '\0');
  if (p > buf && (p[-1] == '\\' || p[-1] == '/'))
    p[-1] = '\0';
  backslashify (strcat (buf, executable));
  return buf;
}

/* Walk down a registry hierarchy until the end.  Return the key. */
static HKEY
openkey (HKEY hStart, const char *keys[])
{
  HKEY hKey, hTmp;
  for (hKey = hStart; *keys; keys++)
    {
      LONG res;
      hTmp = hKey;
      res = RegOpenKey (hTmp, *keys, &hKey);

      if (hTmp != HKEY_LOCAL_MACHINE)
	RegCloseKey (hTmp);

      if (res != ERROR_SUCCESS)
	return NULL;
    }
  return hKey;
}

/* Return the "mingw root" as derived from the mingw uninstall information. */
static const char *
mingw_rootify (const char *executable)
{
  HKEY hKey, hTmp;
  DWORD maxlen;
  char *namebuf, *foundbuf;
  DWORD i;
  LONG res;

  /* Open the uninstall "directory". */
  hKey = openkey (HKEY_LOCAL_MACHINE, mingw_keys);

  /* Not found. */
  if (!hKey)
    return executable;

  /* Need to enumerate all of the keys here looking for one the most recent
     one for MinGW. */
  if (RegQueryInfoKey (hKey, NULL, NULL, NULL, NULL, &maxlen, NULL, NULL,
		       NULL, NULL, NULL, NULL) != ERROR_SUCCESS)
    {
      RegCloseKey (hKey);
      return executable;
    }
  namebuf = XNEWVEC (char, ++maxlen);
  foundbuf = XNEWVEC (char, maxlen);
  foundbuf[0] = '\0';
  if (!namebuf || !foundbuf)
    {
      RegCloseKey (hKey);
      if (namebuf)
	free (namebuf);
      if (foundbuf)
	free (foundbuf);
      return executable;
    }

  /* Look through all of the keys for one that begins with Minimal GNU...
     Try to get the latest version by doing a string compare although that
     string never really works with version number sorting. */
  for (i = 0; RegEnumKey (hKey, i, namebuf, maxlen) == ERROR_SUCCESS; i++)
    {
      int match = strcasecmp (namebuf, MINGW_NAME);
      if (match < 0)
	continue;
      if (match > 0 && strncasecmp (namebuf, MINGW_NAME, MINGW_NAME_LEN) > 0)
	continue;
      if (strcasecmp (namebuf, foundbuf) > 0)
	strcpy (foundbuf, namebuf);
    }
  free (namebuf);

  /* If foundbuf is empty, we didn't find anything.  Punt. */
  if (!foundbuf[0])
    {
      free (foundbuf);
      RegCloseKey (hKey);
      return executable;
    }

  /* Open the key that we wanted */
  res = RegOpenKey (hKey, foundbuf, &hTmp);
  RegCloseKey (hKey);
  free (foundbuf);

  /* Don't know why this would fail, but you gotta check */
  if (res != ERROR_SUCCESS)
    return executable;

  maxlen = 0;
  /* Get the length of the value pointed to by InstallLocation */
  if (RegQueryValueEx (hTmp, "InstallLocation", 0, NULL, NULL,
		       &maxlen) != ERROR_SUCCESS || maxlen == 0)
    {
      RegCloseKey (hTmp);
      return executable;
    }

  /* Allocate space for the install location */
  foundbuf = XNEWVEC (char, maxlen + strlen (executable));
  if (!foundbuf)
    {
      free (foundbuf);
      RegCloseKey (hTmp);
    }

  /* Read the install location into the buffer */
  res = RegQueryValueEx (hTmp, "InstallLocation", 0, NULL, (LPBYTE) foundbuf,
			 &maxlen);
  RegCloseKey (hTmp);
  if (res != ERROR_SUCCESS)
    {
      free (foundbuf);
      return executable;
    }

  /* Concatenate the install location and the executable, turn all slashes
     to backslashes, and return that. */
  return tack_on_executable (foundbuf, executable);
}

/* Read the install location of msys from it's installation file and
   rootify the executable based on that. */
static const char *
msys_rootify (const char *executable)
{
  size_t bufsize = 64;
  size_t execlen = strlen (executable) + 1;
  char *buf;
  DWORD res = 0;
  for (;;)
    {
      buf = XNEWVEC (char, bufsize + execlen);
      if (!buf)
	break;
      res = GetPrivateProfileString ("InstallSettings", "InstallPath", NULL,
				     buf, bufsize, "msys.ini");
      if (!res)
	break;
      if (strlen (buf) < bufsize)
	break;
      res = 0;
      free (buf);
      bufsize *= 2;
      if (bufsize > 65536)
	{
	  buf = NULL;
	  break;
	}
    }

  if (res)
    return tack_on_executable (buf, executable);

  /* failed */
  if (buf)
    free (buf);
  return executable;
}
#endif

static long
spawn_script (const char *executable, const char * const * argv)
{
  int pid = -1;
  int save_errno = errno;
  int fd = _open (executable, _O_RDONLY);

  if (fd >= 0)
    {
      char buf[MAX_PATH + 5];
      int len = _read (fd, buf, sizeof (buf) - 1);
      _close (fd);
      if (len > 3)
	{
	  char *eol;
	  buf[len] = '\0';
	  eol = strchr (buf, '\n');
	  if (eol && strncmp (buf, "#!", 2) == 0)
	    {
	      char *executable1;
	      const char ** avhere = (const char **) --argv;
	      do
		*eol = '\0';
	      while (*--eol == '\r' || *eol == ' ' || *eol == '\t');
	      for (executable1 = buf + 2; *executable1 == ' ' || *executable1 == '\t'; executable1++)
		continue;

	      backslashify (executable1);
	      *avhere = executable1;
#ifndef USE_MINGW_MSYS
	      executable = strrchr (executable1, '\\') + 1;
	      if (!executable)
		executable = executable1;
	      pid = _spawnvp (_P_NOWAIT, executable, argv);
#else
	      if (strchr (executable1, '\\') == NULL)
		pid = _spawnvp (_P_NOWAIT, executable1, argv);
	      else if (executable1[0] != '\\')
		pid = _spawnv (_P_NOWAIT, executable1, argv);
	      else
		{
		  const char *newex = mingw_rootify (executable1);
		  *avhere = newex;
		  pid = _spawnv (_P_NOWAIT, newex, argv);
		  if (executable1 != newex)
		    free ((char *) newex);
		  if (pid < 0)
		    {
		      newex = msys_rootify (executable1);
		      if (newex != executable1)
			{
			  *avhere = newex;
			  pid = _spawnv (_P_NOWAIT, newex, argv);
			  free ((char *) newex);
			}
		    }
		}
#endif
	    }
	}
    }
  if (pid < 0)
    errno = save_errno;
  return pid;
}

/* Execute a child.  */

static long
pex_win32_exec_child (struct pex_obj *obj ATTRIBUTE_UNUSED, int flags,
		      const char *executable, char * const * argv,
		      int in, int out, int errdes, const char **errmsg,
		      int *err)
{
  int org_in, org_out, org_errdes;
  long pid;
  const char * const * newargv;

  org_in = -1;
  org_out = -1;
  org_errdes = -1;

  if (in != STDIN_FILE_NO)
    {
      org_in = _dup (STDIN_FILE_NO);
      if (org_in < 0)
	{
	  *err = errno;
	  *errmsg = "_dup";
	  return -1;
	}
      if (_dup2 (in, STDIN_FILE_NO) < 0)
	{
	  *err = errno;
	  *errmsg = "_dup2";
	  return -1;
	}
      if (_close (in) < 0)
	{
	  *err = errno;
	  *errmsg = "_close";
	  return -1;
	}
    }

  if (out != STDOUT_FILE_NO)
    {
      org_out = _dup (STDOUT_FILE_NO);
      if (org_out < 0)
	{
	  *err = errno;
	  *errmsg = "_dup";
	  return -1;
	}
      if (_dup2 (out, STDOUT_FILE_NO) < 0)
	{
	  *err = errno;
	  *errmsg = "_dup2";
	  return -1;
	}
      if (_close (out) < 0)
	{
	  *err = errno;
	  *errmsg = "_close";
	  return -1;
	}
    }

  if (errdes != STDERR_FILE_NO
      || (flags & PEX_STDERR_TO_STDOUT) != 0)
    {
      org_errdes = _dup (STDERR_FILE_NO);
      if (org_errdes < 0)
	{
	  *err = errno;
	  *errmsg = "_dup";
	  return -1;
	}
      if (_dup2 ((flags & PEX_STDERR_TO_STDOUT) != 0 ? STDOUT_FILE_NO : errdes,
		 STDERR_FILE_NO) < 0)
	{
	  *err = errno;
	  *errmsg = "_dup2";
	  return -1;
	}
      if (errdes != STDERR_FILE_NO)
	{
	  if (_close (errdes) < 0)
	    {
	      *err = errno;
	      *errmsg = "_close";
	      return -1;
	    }
	}
    }

  newargv = fix_argv (argv);
  pid = (((flags & PEX_SEARCH) != 0 ? _spawnvp : _spawnv)
	 (_P_NOWAIT, executable, newargv));

  if (pid == -1)
    pid = spawn_script (executable, newargv);

  if (pid == -1)
    {
      *err = errno;
      *errmsg = ((flags & PEX_SEARCH) != 0) ? "_spawnvp" : "_spawnv";
    }

  if (in != STDIN_FILE_NO)
    {
      if (_dup2 (org_in, STDIN_FILE_NO) < 0)
	{
	  *err = errno;
	  *errmsg = "_dup2";
	  return -1;
	}
      if (_close (org_in) < 0)
	{
	  *err = errno;
	  *errmsg = "_close";
	  return -1;
	}
    }

  if (out != STDOUT_FILE_NO)
    {
      if (_dup2 (org_out, STDOUT_FILE_NO) < 0)
	{
	  *err = errno;
	  *errmsg = "_dup2";
	  return -1;
	}
      if (_close (org_out) < 0)
	{
	  *err = errno;
	  *errmsg = "_close";
	  return -1;
	}
    }

  if (errdes != STDERR_FILE_NO
      || (flags & PEX_STDERR_TO_STDOUT) != 0)
    {
      if (_dup2 (org_errdes, STDERR_FILE_NO) < 0)
	{
	  *err = errno;
	  *errmsg = "_dup2";
	  return -1;
	}
      if (_close (org_errdes) < 0)
	{
	  *err = errno;
	  *errmsg = "_close";
	  return -1;
	}
    }

  return pid;
}

/* Wait for a child process to complete.  MS CRTDLL doesn't return
   enough information in status to decide if the child exited due to a
   signal or not, rather it simply returns an integer with the exit
   code of the child; eg., if the child exited with an abort() call
   and didn't have a handler for SIGABRT, it simply returns with
   status == 3.  We fix the status code to conform to the usual WIF*
   macros.  Note that WIFSIGNALED will never be true under CRTDLL. */

static int
pex_win32_wait (struct pex_obj *obj ATTRIBUTE_UNUSED, long pid,
		int *status, struct pex_time *time, int done ATTRIBUTE_UNUSED,
		const char **errmsg, int *err)
{
  int termstat;

  if (time != NULL)
    memset (time, 0, sizeof *time);

  /* FIXME: If done is non-zero, we should probably try to kill the
     process.  */

  if (_cwait (&termstat, pid, WAIT_CHILD) < 0)
    {
      *err = errno;
      *errmsg = "_cwait";
      return -1;
    }

  /* cwait returns the child process exit code in termstat.  A value
     of 3 indicates that the child caught a signal, but not which one.
     Since only SIGABRT, SIGFPE and SIGINT do anything, we report
     SIGABRT.  */

  if (termstat == 3)
    *status = SIGABRT;
  else
    *status = ((termstat & 0xff) << 8);

  return 0;
}

/* Create a pipe.  */

static int
pex_win32_pipe (struct pex_obj *obj ATTRIBUTE_UNUSED, int *p,
		int binary)
{
  return _pipe (p, 256, binary ? _O_BINARY : _O_TEXT);
}

/* Get a FILE pointer to read from a file descriptor.  */

static FILE *
pex_win32_fdopenr (struct pex_obj *obj ATTRIBUTE_UNUSED, int fd,
		   int binary)
{
  return fdopen (fd, binary ? "rb" : "r");
}

#ifdef MAIN
#include <stdio.h>

int
main (int argc ATTRIBUTE_UNUSED, char **argv)
{
  char const *errmsg;
  int err;
  argv++;
  printf ("%ld\n", pex_win32_exec_child (NULL, PEX_SEARCH, argv[0], argv, 0, 1, 2, &errmsg, &err));
  exit (0);
}
#endif
