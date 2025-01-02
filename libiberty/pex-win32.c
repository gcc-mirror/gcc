/* Utilities to execute a program in a subprocess (possibly linked by pipes
   with other subprocesses), and wait for it.  Generic Win32 specialization.
   Copyright (C) 1996-2025 Free Software Foundation, Inc.

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

#define WIN32_LEAN_AND_MEAN
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

#include <assert.h>
#include <process.h>
#include <io.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/stat.h>
#include <errno.h>
#include <ctype.h>

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

extern char *stpcpy (char *dst, const char *src);

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

static int pex_win32_open_read (struct pex_obj *, const char *, int);
static int pex_win32_open_write (struct pex_obj *, const char *, int, int);
static pid_t pex_win32_exec_child (struct pex_obj *, int, const char *,
				  char * const *, char * const *,
                                  int, int, int, int,
				  const char **, int *);
static int pex_win32_close (struct pex_obj *, int);
static pid_t pex_win32_wait (struct pex_obj *, pid_t, int *,
			   struct pex_time *, int, const char **, int *);
static int pex_win32_pipe (struct pex_obj *, int *, int);
static FILE *pex_win32_fdopenr (struct pex_obj *, int, int);
static FILE *pex_win32_fdopenw (struct pex_obj *, int, int);

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
  pex_win32_fdopenw,
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
		      int binary, int append)
{
  /* Note that we can't use O_EXCL here because gcc may have already
     created the temporary file via make_temp_file.  */
  if (append)
    return -1;
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
      free (namebuf);
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
  free (buf);
  return executable;
}
#endif

/* Return the number of arguments in an argv array, not including the null
   terminating argument. */

static int
argv_to_argc (char *const *argv)
{
  char *const *i = argv;
  while (*i)
    i++;
  return i - argv;
}

/* Return a Windows command-line from ARGV.  It is the caller's
   responsibility to free the string returned.  */

static char *
argv_to_cmdline (char *const *argv)
{
  char *cmdline;
  char *p;
  size_t cmdline_len;
  int i, j, k;
  int needs_quotes;

  cmdline_len = 0;
  for (i = 0; argv[i]; i++)
    {
      /* We only quote arguments that contain spaces, \t or " characters to
	 prevent wasting 2 chars per argument of the CreateProcess 32k char
	 limit.  We need only escape embedded double-quotes and immediately
	 preceeding backslash characters.  A sequence of backslach characters
	 that is not followed by a double quote character will not be
	 escaped.  */
      needs_quotes = 0;
      for (j = 0; argv[i][j]; j++)
	{
	  if (argv[i][j] == ' ' || argv[i][j] == '\t' || argv[i][j] == '"')
	    {
	      needs_quotes = 1;
	    }

	  if (argv[i][j] == '"')
	    {
	      /* Escape preceeding backslashes.  */
	      for (k = j - 1; k >= 0 && argv[i][k] == '\\'; k--)
		cmdline_len++;
	      /* Escape the quote character.  */
	      cmdline_len++;
	    }
	}
      if (j == 0)
	needs_quotes = 1;
      /* Trailing backslashes also need to be escaped because they will be
         followed by the terminating quote.  */
      if (needs_quotes)
        {
          for (k = j - 1; k >= 0 && argv[i][k] == '\\'; k--)
            cmdline_len++;
        }
      cmdline_len += j;
      /* for leading and trailing quotes and space */
      cmdline_len += needs_quotes * 2 + 1;
    }
  cmdline = XNEWVEC (char, cmdline_len);
  p = cmdline;
  for (i = 0; argv[i]; i++)
    {
      needs_quotes = 0;
      for (j = 0; argv[i][j]; j++)
        {
          if (argv[i][j] == ' ' || argv[i][j] == '\t' || argv[i][j] == '"')
            {
              needs_quotes = 1;
              break;
            }
        }
      if (j == 0)
	needs_quotes = 1;

      if (needs_quotes)
        {
          *p++ = '"';
        }
      for (j = 0; argv[i][j]; j++)
	{
	  if (argv[i][j] == '"')
	    {
	      for (k = j - 1; k >= 0 && argv[i][k] == '\\'; k--)
		*p++ = '\\';
	      *p++ = '\\';
	    }
	  *p++ = argv[i][j];
	}
      if (needs_quotes)
        {
          for (k = j - 1; k >= 0 && argv[i][k] == '\\'; k--)
            *p++ = '\\';
          *p++ = '"';
        }
      *p++ = ' ';
    }
  p[-1] = '\0';
  return cmdline;
}

/* We'll try the passed filename with all the known standard
   extensions, and then without extension.  We try no extension
   last so that we don't try to run some random extension-less
   file that might be hanging around.  We try both extension
   and no extension so that we don't need any fancy logic
   to determine if a file has extension.  */
static const char *const
std_suffixes[] = {
  ".com",
  ".exe",
  ".bat",
  ".cmd",
  "",
  0
};

/* Returns the full path to PROGRAM.  If SEARCH is true, look for
   PROGRAM in each directory in PATH.  */

static char *
find_executable (const char *program, BOOL search)
{
  char *full_executable;
  char *e;
  size_t fe_len;
  const char *path = 0;
  const char *const *ext;
  const char *p, *q;
  size_t proglen = strlen (program);
  int has_slash = (strchr (program, '/') || strchr (program, '\\'));
  HANDLE h;

  if (has_slash)
    search = FALSE;

  if (search)
    path = getenv ("PATH");
  if (!path)
    path = "";

  fe_len = 0;
  for (p = path; *p; p = q)
    {
      q = p;
      while (*q != ';' && *q != '\0')
	q++;
      if ((size_t)(q - p) > fe_len)
	fe_len = q - p;
      if (*q == ';')
	q++;
    }
  fe_len = fe_len + 1 + proglen + 5 /* space for extension */;
  full_executable = XNEWVEC (char, fe_len);

  p = path;
  do
    {
      q = p;
      while (*q != ';' && *q != '\0')
	q++;

      e = full_executable;
      memcpy (e, p, q - p);
      e += (q - p);
      if (q - p)
	*e++ = '\\';
      strcpy (e, program);

      if (*q == ';')
	q++;

      for (e = full_executable; *e; e++)
	if (*e == '/')
	  *e = '\\';

      /* At this point, e points to the terminating NUL character for
         full_executable.  */
      for (ext = std_suffixes; *ext; ext++)
	{
	  /* Remove any current extension.  */
	  *e = '\0';
	  /* Add the new one.  */
	  strcat (full_executable, *ext);

	  /* Attempt to open this file.  */
	  h = CreateFile (full_executable, GENERIC_READ,
			  FILE_SHARE_READ | FILE_SHARE_WRITE,
			  0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
	  if (h != INVALID_HANDLE_VALUE)
	    goto found;
	}
      p = q;
    }
  while (*p);
  free (full_executable);
  return 0;

 found:
  CloseHandle (h);
  return full_executable;
}

/* Low-level process creation function and helper.  */

static int
env_compare (const void *a_ptr, const void *b_ptr)
{
  const char *a;
  const char *b;
  unsigned char c1;
  unsigned char c2;

  a = *(const char **) a_ptr;
  b = *(const char **) b_ptr;

  /* a and b will be of the form: VAR=VALUE
     We compare only the variable name part here using a case-insensitive
     comparison algorithm.  It might appear that in fact strcasecmp () can
     take the place of this whole function, and indeed it could, save for
     the fact that it would fail in cases such as comparing A1=foo and
     A=bar (because 1 is less than = in the ASCII character set).
     (Environment variables containing no numbers would work in such a
     scenario.)  */

  do
    {
      c1 = (unsigned char) tolower (*a++);
      c2 = (unsigned char) tolower (*b++);

      if (c1 == '=')
        c1 = '\0';

      if (c2 == '=')
        c2 = '\0';
    }
  while (c1 == c2 && c1 != '\0');

  return c1 - c2;
}

/* Execute a Windows executable as a child process.  This will fail if the
 * target is not actually an executable, such as if it is a shell script. */

static pid_t
win32_spawn (struct pex_obj *obj,
         const char *executable,
	     BOOL search,
	     char *const *argv,
             char *const *env, /* array of strings of the form: VAR=VALUE */
	     DWORD dwCreationFlags,
	     LPSTARTUPINFO si,
	     LPPROCESS_INFORMATION pi)
{
  char *full_executable = NULL;
  char *cmdline = NULL;
  pid_t pid = (pid_t) -1;
  char **env_copy;
  char *env_block = NULL;

  if (env)
    {
      int env_size;

      /* Count the number of environment bindings supplied.  */
      for (env_size = 0; env[env_size]; env_size++)
        continue;
    
      /* Assemble an environment block, if required.  This consists of
         VAR=VALUE strings juxtaposed (with one null character between each
         pair) and an additional null at the end.  */
      if (env_size > 0)
        {
          int var;
          int total_size = 1; /* 1 is for the final null.  */
          char *bufptr;
    
          /* Windows needs the members of the block to be sorted by variable
             name.  */
          env_copy = (char **) alloca (sizeof (char *) * env_size);
          memcpy (env_copy, env, sizeof (char *) * env_size);
          qsort (env_copy, env_size, sizeof (char *), env_compare);
    
          for (var = 0; var < env_size; var++)
            total_size += strlen (env[var]) + 1;
    
          env_block = XNEWVEC (char, total_size);
          bufptr = env_block;
          for (var = 0; var < env_size; var++)
            bufptr = stpcpy (bufptr, env_copy[var]) + 1;
    
          *bufptr = '\0';
        }
    }

  full_executable = find_executable (executable, search);
  if (!full_executable)
    goto exit;
  cmdline = argv_to_cmdline (argv);
  if (!cmdline)
    goto exit;
  /* If cmdline is too large, CreateProcess will fail with a bad
     'No such file or directory' error. Try passing it through a
     temporary response file instead.  */
  if (strlen (cmdline) > 32767)
    {
      char *response_file = make_temp_file ("");
      /* Register the file for deletion by pex_free.  */
      ++obj->remove_count;
      obj->remove = XRESIZEVEC (char *, obj->remove, obj->remove_count);
      obj->remove[obj->remove_count - 1] = response_file;
      int fd = pex_win32_open_write (obj, response_file, 0, 0);
      if (fd == -1)
        goto exit;
      FILE *f = pex_win32_fdopenw (obj, fd, 0);
      /* Don't write argv[0] (program name) to the response file.  */
      if (writeargv (&argv[1], f))
        {
          fclose (f);
          goto exit;
        }
      fclose (f); /* Also closes fd and the underlying OS handle.  */
      char *response_arg = concat ("@", response_file, NULL);
      char *response_argv[3] = {argv[0], response_arg, NULL};
      free (cmdline);
      cmdline = argv_to_cmdline (response_argv);
      free (response_arg);
      if (!cmdline)
        goto exit;
    }
  
  /* Create the child process.  */
  if (CreateProcess (full_executable, cmdline,
		      /*lpProcessAttributes=*/NULL,
		      /*lpThreadAttributes=*/NULL,
		      /*bInheritHandles=*/TRUE,
		      dwCreationFlags,
		      (LPVOID) env_block,
		      /*lpCurrentDirectory=*/NULL,
		      si,
		      pi))
    {
      CloseHandle (pi->hThread);
      pid = (pid_t) pi->hProcess;
    }

 exit:
  /* Clean up.  */
  free (env_block);
  free (cmdline);
  free (full_executable);
  
  return pid;
}

/* Spawn a script.  This simulates the Unix script execution mechanism.
   This function is called as a fallback if win32_spawn fails. */

static pid_t
spawn_script (struct pex_obj *obj,
              const char *executable, char *const *argv,
              char* const *env,
	      DWORD dwCreationFlags,
	      LPSTARTUPINFO si,
	      LPPROCESS_INFORMATION pi)
{
  pid_t pid = (pid_t) -1;
  int save_errno = errno;
  int fd = _open (executable, _O_RDONLY);

  /* Try to open script, check header format, extract interpreter path,
     and spawn script using that interpretter. */
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
            
	      /* Header format is OK. */
	      char *executable1;
              int new_argc;
              const char **avhere;

	      /* Extract interpreter path. */
	      do
		*eol = '\0';
	      while (*--eol == '\r' || *eol == ' ' || *eol == '\t');
	      for (executable1 = buf + 2; *executable1 == ' ' || *executable1 == '\t'; executable1++)
		continue;
	      backslashify (executable1);

	      /* Duplicate argv, prepending the interpreter path. */
	      new_argc = argv_to_argc (argv) + 1;
	      avhere = XNEWVEC (const char *, new_argc + 1);
	      *avhere = executable1;
	      memcpy (avhere + 1, argv, new_argc * sizeof(*argv));
	      argv = (char *const *)avhere;

	      /* Spawn the child. */
#ifndef USE_MINGW_MSYS
	      executable = strrchr (executable1, '\\') + 1;
	      if (!executable)
		executable = executable1;
	      pid = win32_spawn (obj, executable, TRUE, argv, env,
				 dwCreationFlags, si, pi);
#else
	      if (strchr (executable1, '\\') == NULL)
		pid = win32_spawn (obj, executable1, TRUE, argv, env,
				   dwCreationFlags, si, pi);
	      else if (executable1[0] != '\\')
		pid = win32_spawn (obj, executable1, FALSE, argv, env,
				   dwCreationFlags, si, pi);
	      else
		{
		  const char *newex = mingw_rootify (executable1);
		  *avhere = newex;
		  pid = win32_spawn (obj, newex, FALSE, argv, env,
				     dwCreationFlags, si, pi);
		  if (executable1 != newex)
		    free ((char *) newex);
		  if (pid == (pid_t) -1)
		    {
		      newex = msys_rootify (executable1);
		      if (newex != executable1)
			{
			  *avhere = newex;
			  pid = win32_spawn (obj, newex, FALSE, argv, env,
					     dwCreationFlags, si, pi);
			  free ((char *) newex);
			}
		    }
		}
#endif
	      free (avhere);
	    }
	}
    }
  if (pid == (pid_t) -1)
    errno = save_errno;
  return pid;
}

/* Execute a child.  */

static pid_t
pex_win32_exec_child (struct pex_obj *obj, int flags,
		      const char *executable, char * const * argv,
                      char* const* env,
		      int in, int out, int errdes,
		      int toclose ATTRIBUTE_UNUSED,
		      const char **errmsg,
		      int *err)
{
  pid_t pid;
  HANDLE stdin_handle;
  HANDLE stdout_handle;
  HANDLE stderr_handle;
  DWORD dwCreationFlags;
  OSVERSIONINFO version_info;
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  int orig_out, orig_in, orig_err = 0;
  BOOL separate_stderr = !(flags & PEX_STDERR_TO_STDOUT);

  /* Ensure we have inheritable descriptors to pass to the child.  */
  orig_in = in;
  in = _dup (orig_in);
  
  orig_out = out;
  out = _dup (orig_out);
  
  if (separate_stderr)
    {
      orig_err = errdes;
      errdes = _dup (orig_err);
    }

  stdin_handle = INVALID_HANDLE_VALUE;
  stdout_handle = INVALID_HANDLE_VALUE;
  stderr_handle = INVALID_HANDLE_VALUE;

  stdin_handle = (HANDLE) _get_osfhandle (in);
  stdout_handle = (HANDLE) _get_osfhandle (out);
  if (separate_stderr)
    stderr_handle = (HANDLE) _get_osfhandle (errdes);
  else
    stderr_handle = stdout_handle;

  /* Determine the version of Windows we are running on.  */
  version_info.dwOSVersionInfoSize = sizeof (version_info); 
  GetVersionEx (&version_info);
  if (version_info.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS)
    /* On Windows 95/98/ME the CREATE_NO_WINDOW flag is not
       supported, so we cannot avoid creating a console window.  */
    dwCreationFlags = 0;
  else
    {
      HANDLE conout_handle;

      /* Determine whether or not we have an associated console.  */
      conout_handle = CreateFile("CONOUT$", 
				 GENERIC_WRITE,
				 FILE_SHARE_WRITE,
				 /*lpSecurityAttributes=*/NULL,
				 OPEN_EXISTING,
				 FILE_ATTRIBUTE_NORMAL,
				 /*hTemplateFile=*/NULL);
      if (conout_handle == INVALID_HANDLE_VALUE)
	/* There is no console associated with this process.  Since
	   the child is a console process, the OS would normally
	   create a new console Window for the child.  Since we'll be
	   redirecting the child's standard streams, we do not need
	   the console window.  */ 
	dwCreationFlags = CREATE_NO_WINDOW;
      else 
	{
	  /* There is a console associated with the process, so the OS
	     will not create a new console.  And, if we use
	     CREATE_NO_WINDOW in this situation, the child will have
	     no associated console.  Therefore, if the child's
	     standard streams are connected to the console, the output
	     will be discarded.  */
	  CloseHandle(conout_handle);
	  dwCreationFlags = 0;
	}
    }

  /* Since the child will be a console process, it will, by default,
     connect standard input/output to its console.  However, we want
     the child to use the handles specifically designated above.  In
     addition, if there is no console (such as when we are running in
     a Cygwin X window), then we must redirect the child's
     input/output, as there is no console for the child to use.  */
  memset (&si, 0, sizeof (si));
  si.cb = sizeof (si);
  si.dwFlags = STARTF_USESTDHANDLES;
  si.hStdInput = stdin_handle;
  si.hStdOutput = stdout_handle;
  si.hStdError = stderr_handle;

  /* Create the child process.  */  
  pid = win32_spawn (obj, executable, (flags & PEX_SEARCH) != 0,
		     argv, env, dwCreationFlags, &si, &pi);
  if (pid == (pid_t) -1)
    pid = spawn_script (obj, executable, argv, env, dwCreationFlags,
                        &si, &pi);
  if (pid == (pid_t) -1)
    {
      *err = ENOENT;
      *errmsg = "CreateProcess";
    }

  /* If the child was created successfully, close the original file
     descriptors.  If the process creation fails, these are closed by
     pex_run_in_environment instead.  We must not close them twice as
     that seems to cause a Windows exception.  */
     
  if (pid != (pid_t) -1)
    {
      if (orig_in != STDIN_FILENO)
	_close (orig_in);
      if (orig_out != STDOUT_FILENO)
	_close (orig_out);
      if (separate_stderr
	  && orig_err != STDERR_FILENO)
	_close (orig_err);
    }

  /* Close the standard input, standard output and standard error handles
     in the parent.  */ 

  _close (in);
  _close (out);
  if (separate_stderr)
    _close (errdes);

  return pid;
}

/* Wait for a child process to complete.  MS CRTDLL doesn't return
   enough information in status to decide if the child exited due to a
   signal or not, rather it simply returns an integer with the exit
   code of the child; eg., if the child exited with an abort() call
   and didn't have a handler for SIGABRT, it simply returns with
   status == 3.  We fix the status code to conform to the usual WIF*
   macros.  Note that WIFSIGNALED will never be true under CRTDLL. */

static pid_t
pex_win32_wait (struct pex_obj *obj ATTRIBUTE_UNUSED, pid_t pid,
		int *status, struct pex_time *time, int done ATTRIBUTE_UNUSED,
		const char **errmsg, int *err)
{
  DWORD termstat;
  HANDLE h;

  if (time != NULL)
    memset (time, 0, sizeof *time);

  h = (HANDLE) pid;

  /* FIXME: If done is non-zero, we should probably try to kill the
     process.  */
  if (WaitForSingleObject (h, INFINITE) != WAIT_OBJECT_0)
    {
      CloseHandle (h);
      *err = ECHILD;
      *errmsg = "WaitForSingleObject";
      return -1;
    }

  GetExitCodeProcess (h, &termstat);
  CloseHandle (h);
 
  /* A value of 3 indicates that the child caught a signal, but not
     which one.  Since only SIGABRT, SIGFPE and SIGINT do anything, we
     report SIGABRT.  */
  if (termstat == 3)
    *status = SIGABRT;
  else
    *status = (termstat & 0xff) << 8;

  return 0;
}

/* Create a pipe.  */

static int
pex_win32_pipe (struct pex_obj *obj ATTRIBUTE_UNUSED, int *p,
		int binary)
{
  return _pipe (p, 256, (binary ? _O_BINARY : _O_TEXT) | _O_NOINHERIT);
}

/* Get a FILE pointer to read from a file descriptor.  */

static FILE *
pex_win32_fdopenr (struct pex_obj *obj ATTRIBUTE_UNUSED, int fd,
		   int binary)
{
  HANDLE h = (HANDLE) _get_osfhandle (fd);
  if (h == INVALID_HANDLE_VALUE)
    return NULL;
  if (! SetHandleInformation (h, HANDLE_FLAG_INHERIT, 0))
    return NULL;
  return fdopen (fd, binary ? "rb" : "r");
}

static FILE *
pex_win32_fdopenw (struct pex_obj *obj ATTRIBUTE_UNUSED, int fd,
		   int binary)
{
  HANDLE h = (HANDLE) _get_osfhandle (fd);
  if (h == INVALID_HANDLE_VALUE)
    return NULL;
  if (! SetHandleInformation (h, HANDLE_FLAG_INHERIT, 0))
    return NULL;
  return fdopen (fd, binary ? "wb" : "w");
}

#ifdef MAIN
#include <stdio.h>

int
main (int argc ATTRIBUTE_UNUSED, char **argv)
{
  char const *errmsg;
  int err;
  argv++;
  printf ("%ld\n", (long) pex_win32_exec_child (NULL, PEX_SEARCH, argv[0], argv, NULL, 0, 0, 1, 2, &errmsg, &err));
  exit (0);
}
#endif
