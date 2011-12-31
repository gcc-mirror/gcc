/* gupc.c: the UPC compiler driver program
   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
   2010, 2011
   Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#define USED_FOR_TARGET 1	/* disable inclusion of code gen defs. */
#include "tm.h"

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>

#include <sys/stat.h>
#include <sys/param.h>
#include <sys/types.h>

/* The UPC driver program invokes the GCC compiler, passing along
   the switches on the command line. If the user does not supply
   switches (such -c, -S, or -E) that would disable linking,
   then add the additional link switches that are required to
   link a UPC program. */

#ifndef COMPILER
#error "-DCOMPILER must be supplied when compiling upc.c"
#endif

#define MULTI_DIR_SWITCH "-print-multi-directory"
#define FIND_LIBGUPC_SWITCH "-print-file-name=libgupc.a"

#define GCC_SWITCH_TAKES_ARG(CHAR) \
  ((CHAR) == 'D' || (CHAR) == 'U' || (CHAR) == 'o' \
   || (CHAR) == 'e' || (CHAR) == 'T' || (CHAR) == 'u' \
   || (CHAR) == 'I' || (CHAR) == 'm' || (CHAR) == 'x' \
   || (CHAR) == 'L' || (CHAR) == 'A' || (CHAR) == 'V' \
   || (CHAR) == 'B' || (CHAR) == 'b')


/* This defines which multi-letter switches take arguments.  */
#define GCC_WORD_SWITCH_TAKES_ARG(STR)		\
  (  !strcmp (STR, "aux-info") \
  || !strcmp (STR, "dumpbase") \
  || !strcmp (STR, "idirafter") \
  || !strcmp (STR, "imacros") \
  || !strcmp (STR, "include") \
  || !strcmp (STR, "iprefix") \
  || !strcmp (STR, "isystem") \
  || !strcmp (STR, "iwithprefix") \
  || !strcmp (STR, "iwithprefixbefore") \
  || !strcmp (STR, "param") \
  || !strcmp (STR, "specs") \
  || !strcmp (STR, "Tbss") \
  || !strcmp (STR, "Tdata") \
  || !strcmp (STR, "Ttext") \
  || !strcmp (STR, "Xlinker"))

#define NO_LINK_SWITCHES(STR) \
  (!strcmp (STR, "-fsyntax-only") || !strcmp (STR, "-c") \
  || !strcmp (STR, "-M") || !strcmp (STR, "-MM") \
  || !strcmp (STR, "-E") || !strcmp (STR, "-S"))

#define GCC_INFO_ONLY_SWITCHES(STR) \
  (!strcmp (STR, "-v") || !strcmp(STR, "--verbose") \
  || !strcmp (STR, "--version") \
  || !strcmp (STR, "--help") \
  || !strncmp(STR, "-print-", 7) || !strncmp(STR, "--print-", 8))

#ifndef ARG_MAX
#define ARG_MAX 4096
#endif

#ifndef GET_ENV_PATH_LIST
#define GET_ENV_PATH_LIST(VAR,NAME) \
	do { (VAR) = getenv (NAME); } while (0)
#endif

#define BINSUFFIX "/bin/"
#define GCCLIBSUFFIX "/lib/gcc/"

static const char *const standard_bindir_prefix = STANDARD_BINDIR_PREFIX;
static const char *const standard_exec_prefix = STANDARD_EXEC_PREFIX;

static char *progname;

static int debug;

static char *substr PARAMS ((const char *s, int len));
static int match_suffix PARAMS ((const char *s, const char *suffix));

#define END_ARGS ((char *) 0)

/* Concatenate a sequence of strings, returning the result.

   This function is based on the one in libiberty.  */

char *
concat (const char *first, ...)
{
  int length;
  char *newstr;
  char *end;
  const char *arg;
  va_list args;

  /* First compute the size of the result and get sufficient memory.  */

  va_start (args, first);
  arg = first;
  length = 0;

  while (arg != END_ARGS)
    {
      length += strlen (arg);
      arg = va_arg (args, const char *);
    }

  newstr = (char *) xmalloc (length + 1);
  va_end (args);

  /* Now copy the individual pieces to the result string.  */

  va_start (args, first);
  end = newstr;
  arg = first;
  while (arg != END_ARGS)
    {
      while (*arg)
	*end++ = *arg++;
      arg = va_arg (args, const char *);
    }
  *end = '\000';
  va_end (args);

  return newstr;
}

static char *
substr (const char *s, int len)
{
  char *sub = (char *) xmalloc (len + 1);
  strncpy (sub, s, len);
  sub[len] = '\0';
  return sub;
}

static int
match_suffix (const char *s, const char *suffix)
{
  int slen = strlen (s);
  int xlen = strlen (suffix);
  const char *start = (xlen <= slen) ? s + slen - xlen : 0;
  return start && !strncmp (start, suffix, xlen);
}

/* Escape characters that might be harmful to the shell.  */

static char *
shell_escape (const char *s)
{
  const char *meta = "&;`'\\\"|*?~<>^()[]{}$\n\r\f\t ";
  int needs_escape, needs_quote;
  char *r;
  const char *ps;
  char *result = xstrdup (s);
  for (needs_quote = 0, needs_escape = 0, ps = s; *ps; ++ps)
    {
      if (strchr (meta, (int) *ps))
	needs_quote = 1;
      needs_escape += (*ps == '\'');
    }
  if (needs_quote)
    {
      result = (char *) xmalloc (strlen (s) + 1 + 2 + needs_escape * 5);
      for (ps = s, r = result, *r++ = '\''; *ps; ++ps)
	if (*ps == '\'')
	  {
	    memcpy (r, "'\"'\"'", 5);
	    r += 5;
	  }
	else
	  *r++ = *ps;
      *r++ = '\'';
      *r = '\0';
    }
  return result;
}

/* Using the argument list we've built up so far, tack on
   the PRINT_CMD argument, and return the result.  */

static const char *
get_print_cmd (const char *exec_args[], int n_args, const char *print_cmd)
{
  int i, len;
  char *cmd;
  char *s;
  const char *result = NULL;
  const char * const err_null = "2>/dev/null";
  FILE *pipe;
  for (i = 0, len = strlen (print_cmd) + 1 + strlen (err_null) + 1;
       i < n_args; ++i)
    len += strlen (shell_escape (exec_args[i])) + 1;
  cmd = (char *) xmalloc (len + 1);
  for (i = 0, s = cmd; i < n_args; ++i)
    {
      char *p = shell_escape (exec_args[i]);
      if (i != 0)
        *s++ = ' ';
      strcpy (s, p);
      s += strlen (p);
    }
  *s++ = ' ';
  strcpy (s, print_cmd);
  s += strlen (print_cmd);
  *s++ = ' ';
  strcpy (s, err_null);
  s += strlen (err_null);
  if (debug)
    fprintf (stderr, "get_print_cmd: cmd='%s'\n", cmd);
  pipe = popen (cmd, "r");
  if (pipe)
    {
      int exit_status;
      char buf[4096];
      int slen;
      (void) fgets (buf, sizeof (buf), pipe);
      slen = strlen (buf);
      if (buf[slen - 1] == '\n')
	buf[slen - 1] = '\0';
      exit_status = pclose (pipe);
      if (!exit_status)
        {
          result = (const char *) xstrdup (buf);
	  if (debug)
	    fprintf (stderr, "get_print_cmd: result='%s'\n", result);
        }
    }
  return result;
}

/* Return the path of the library directory,
   where libgupc can be found.  LIB_PATH will be defined
   when the development version of the 'upc' command is
   being built; use that path, and add the multilib
   suffix if required.  Otherwise, for the installed
   'upc' command, use -print-file-name to find the "libgupc.a"
   library file, and return the containing directory.  */

static const char *
get_libgupc_path (const char *exec_args[], int n_args)
{
  const char *libgupc_path = NULL;
#ifdef LIB_PATH
  {
    const char *lib_suffix;
    libgupc_path = LIB_PATH;
    lib_suffix = get_print_cmd (exec_args, n_args, MULTI_DIR_SWITCH);
    if (debug)
      fprintf (stderr, "lib suffix = %s\n",
	       lib_suffix ? lib_suffix : "<none>");
    if (lib_suffix && *lib_suffix && (strcmp (lib_suffix, ".") != 0))
      libgupc_path = concat (libgupc_path, "/", lib_suffix, END_ARGS);
    libgupc_path = concat (libgupc_path, "/libgupc", END_ARGS);
  }
#else
  {
    const char *libgupc_archive;
    libgupc_archive = get_print_cmd (exec_args, n_args, FIND_LIBGUPC_SWITCH);
    if (debug)
      fprintf (stderr, "libgupc.a path = %s\n",
	       libgupc_archive ? libgupc_archive : "<none>");
    if (libgupc_archive && libgupc_archive[0] == '/')
      {
	const char *s, *last_slash;
	char *path;
	size_t slen;
	for (s = libgupc_archive; *s; ++s)
	  if (*s == '/')
	    last_slash = s;
	slen = (last_slash - libgupc_archive);
	path = (char *) xmalloc (slen + 1);
	memcpy (path, libgupc_archive, slen);
	path[slen] = '\0';
	libgupc_path = (const char *) path;
      }
  }
#endif
  if (debug)
    fprintf (stderr, "lib path = %s\n",
             libgupc_path ? libgupc_path : "<none>");
  return libgupc_path;
}

int
main (int argc, char *argv[])
{
  int i, nargs;
  int info_only = 1;
  int invoke_linker = 1;
  int no_default_libs = 0;
  int no_std_inc = 0;
  int no_upc_pre_inc = 0;
  int is_x_upc_in_effect = 0;
  const int is_dev_compiler = !strcmp (COMPILER, "xgcc");
  const char *cp;
  const char *compiler = 0;
  const char *compiler_dir = 0;
  const char *bin_dir = 0;
  const char *lib_dir = 0;
  const char *inc_dir = 0;
  const char *upc_exec_prefix = 0;
  const char *exec_args[ARG_MAX];
  char *exec_arg_list[ARG_MAX];

#ifdef DEBUG
  debug = 1;
#endif

  /* Parse command line early for instances of -debug.  This allows
     the debug flag to be set before functions like find_a_file()
     are called.  */
  for (i = 1; i < argc; ++i)
    if (!strcmp (argv[i], "-debug"))
      debug = 1;

  /* extract the program's name from the command line. */
  for (cp = argv[0] + strlen (argv[0]) - 1;
       cp != argv[0] && *cp != '/'; --cp) /* loop */ ;
  progname = (char *) xmalloc (strlen (cp + 1) + 1);
  strcpy (progname, cp + 1);


#ifdef COMPILER_DIR
  compiler_dir = concat (COMPILER_DIR, END_ARGS);
#endif
#ifdef BIN_PATH
  bin_dir = BIN_PATH;
#endif
#ifdef INC_PATH
  inc_dir = INC_PATH;
#endif

  /* Check to see if any switches are asserted that inhibit linking
     and record the presence of other switches that may require
     special handling. */
  for (i = 1; i < argc; ++i)
    {
      const char *const arg = argv[i];
      if (arg[0] == '-')
	{
	  /* skip upc's '-debug' switch */
	  if (!strcmp (arg, "-debug"))
	    continue;
	  else if (!strcmp (arg, "-nodefaultlibs"))
	    {
	      no_default_libs = 1;
	    }
	  else if (!strcmp (arg, "-nostdinc"))
	    {
	      no_std_inc = 1;
	    }
	  else if (!strcmp (arg, "-nostdlib"))
	    {
	      no_default_libs = 1;
	    }
	  else if (!strcmp (arg, "-fno-upc-pre-include"))
	    {
	      no_upc_pre_inc = 1;
	    }
	  invoke_linker = invoke_linker && !NO_LINK_SWITCHES (arg);
	  info_only = info_only && GCC_INFO_ONLY_SWITCHES (arg);
	  if (((arg[2] == '\0') && GCC_SWITCH_TAKES_ARG (arg[1]))
	      || GCC_WORD_SWITCH_TAKES_ARG (&arg[1])
	      || ((arg[1] == '-') && GCC_WORD_SWITCH_TAKES_ARG (&arg[2])))
            {
	      if ((i + 1) == argc)
	        {
		  fprintf (stderr, "'%s' option must have argument\n", arg);
		  exit (1);
		}
	      /* skip the following argument */
	      ++i;
	    }
	}
      else
	/* an argument that is not a switch implies that we'll do something. */
	info_only = FALSE;
    }
  invoke_linker = invoke_linker && !info_only;
  nargs = 0;
  /* The COMPILER preprocessor variable is passed on the command
     line to the C compiler when 'upc' is built. It usually has
     the form "xgcc" for builds in the development directory,
     and "gcc" for installed upc command. The COMPILER_DIR
     directory gives the location of where the gcc (or xgcc)
     binary lives, usually with a "/" appended to the end, so
     that the result can be passed directly to the "gcc" command,
     yield an invocation of the form:
     <full_pathname_of_gcc_or_xgcc> -B<compiler_dir>/

     If the UPC_EXEC_PREFIX environment variable is set, this value overrides
     the compiled-in COMPILER_DIR setting. */

  GET_ENV_PATH_LIST (upc_exec_prefix, "UPC_EXEC_PREFIX");
  if (!(compiler_dir || upc_exec_prefix))
    {
      upc_exec_prefix =
	make_relative_prefix (argv[0], standard_bindir_prefix,
			      standard_exec_prefix);
    }
  if (upc_exec_prefix && strcmp (upc_exec_prefix, standard_exec_prefix) != 0)
    {
      int len = strlen (upc_exec_prefix);

      if (debug)
	{
	  fprintf (stderr, "using UPC_EXEC_PREFIX=%s\n", upc_exec_prefix);
	}
      if (match_suffix (upc_exec_prefix, GCCLIBSUFFIX))
	{
	  bin_dir = concat (substr (upc_exec_prefix,
				    len - (sizeof (GCCLIBSUFFIX) - 1)),
			    "/bin", END_ARGS);
	  compiler_dir = upc_exec_prefix;
	}
      else if (match_suffix (upc_exec_prefix, BINSUFFIX))
	{
	  bin_dir = substr (upc_exec_prefix, len - 1);
	  compiler_dir = concat (substr (upc_exec_prefix,
					 len - (sizeof (BINSUFFIX) - 1)),
				 GCCLIBSUFFIX, END_ARGS);
	}
      else
	{
	  bin_dir = concat (upc_exec_prefix, "bin", END_ARGS);
	  compiler_dir = concat (upc_exec_prefix, "lib/gcc-lib/", END_ARGS);
	}

      inc_dir = concat (compiler_dir,
			DEFAULT_TARGET_MACHINE, "/",
			DEFAULT_TARGET_VERSION, "/include", END_ARGS);
    }

  compiler = concat (bin_dir, "/", COMPILER, END_ARGS);
  exec_args[nargs++] = compiler;

  if (compiler_dir)
    {
      exec_args[nargs++] = xstrdup ("-B");
      exec_args[nargs++] = compiler_dir;
    }

  if (!info_only)
    {
      if (inc_dir && !no_std_inc && !no_upc_pre_inc)
	{
	  /* Copy in the -isystem <path> argument */
	  exec_args[nargs++] = xstrdup ("-isystem");
	  exec_args[nargs++] = inc_dir;
	}
    }

  /* Copy in the arguments as passed to 'upc' */
  for (i = 1, is_x_upc_in_effect = 0; i < argc; ++i)
    {
      const char *const arg = argv[i];
      const int is_c_file = match_suffix (arg, ".c")
	|| match_suffix (arg, ".h");
      const int is_upc_file = match_suffix (arg, ".upc")
	|| match_suffix (arg, ".uph");
      int num;
      /* skip upc's '-debug' switch */
      if (!strcmp (arg, "-debug"))
	continue;
      if (!strcmp (arg, "-n") && ((i + 1) < argc))
	{
	  /* rewrite "-n <num>" into "-fupc-threads-<num>" */
	  exec_args[nargs++] = concat ("-fupc-threads-", argv[++i], END_ARGS);
	}
      else if (!strncmp (arg, "-n", 2) && (sscanf (arg + 2, "%d", &num) == 1))
	{
	  /* rewrite "-n<num>" into "-fupc-threads-<num>" */
	  exec_args[nargs++] = concat ("-fupc-threads-", arg + 2, END_ARGS);
	}
      else if (!strcmp (arg, "-inst") || !strcmp (arg, "--inst"))
	{
	  /* rewrite "-inst" or "--inst" into "-fupc-instrument" */
	  exec_args[nargs++] = "-fupc-instrument";
	}
      else if (!strcmp (arg, "-inst-functions")
	       || !strcmp (arg, "--inst-functions"))
	{
	  /* rewrite "-inst-functions" or "--inst-functions"
	     into "-fupc-instrument-functions" */
	  exec_args[nargs++] = "-fupc-instrument-functions";
	}
      else if (((arg[2] == '\0') && GCC_SWITCH_TAKES_ARG (arg[1]))
	       || GCC_WORD_SWITCH_TAKES_ARG (&arg[1])
	       || ((arg[1] == '-') && GCC_WORD_SWITCH_TAKES_ARG (&arg[2])))
	{
	  /* Copy the switch and the following argument.  */
	  exec_args[nargs++] = arg;
	  exec_args[nargs++] = argv[++i];
	}
      else if (arg[1] == '-')
	/* Copy the switch and continue.  */
	exec_args[nargs++] = arg;
      else
	{
	  if (is_c_file && !is_x_upc_in_effect)
	    {
	      /* Assume that .c files are in fact UPC source files */
	      is_x_upc_in_effect = 1;
	      exec_args[nargs++] = "-x";
	      exec_args[nargs++] = "upc";
	    }
	  else if (!(is_c_file || is_upc_file) && is_x_upc_in_effect)
	    {
	      is_x_upc_in_effect = 0;
	      exec_args[nargs++] = "-x";
	      exec_args[nargs++] = "none";
	    }
	  exec_args[nargs++] = arg;
	}
    }

  if (!info_only)
    {
      lib_dir = get_libgupc_path (exec_args, nargs);
      if (lib_dir)
	{
	  if (!no_std_inc && !no_upc_pre_inc)
	    {
	      /* Place libdir first so that we can find gcc-upc-lib.h. */
	      exec_args[nargs++] = xstrdup ("-isystem");
	      exec_args[nargs++] = lib_dir;
	    }
	  /* add -B <lib_dir>/ so that we can find libgupc.spec.  */
	  exec_args[nargs++] = concat ("-B", lib_dir, "/", END_ARGS);
        }
    }

  if (invoke_linker)
    {
      /* The -fupc-link switch triggers per-target libgupc compiler specs
         via %:include(libgupc.spec). */
      exec_args[nargs++] = "-fupc-link";
      if (!no_default_libs && lib_dir)
	{
	  const char *link_lib_dir = lib_dir;
	  /* If we're building the development version of the UPC
	     driver ("xgupc"), then we need to add the .libs suffix
	     because that's where libtool hides libgupc.a */
	  if (is_dev_compiler)
	    link_lib_dir = concat (link_lib_dir, "/.libs", END_ARGS);
	  /* Add the link library path where libgupc.a is located.  */
	  exec_args[nargs++] =
	    concat (xstrdup ("-L"), link_lib_dir, END_ARGS);
	}
    }

  if (debug)
    {
      fprintf (stderr, "upc exec args: ");
      for (i = 0; i < nargs; ++i)
	{
	  if (i != 0)
	    fprintf (stderr, " ");
	  fprintf (stderr, "%s", exec_args[i]);
	}
      fprintf (stderr, "\n");
    }
  exec_args[nargs++] = 0;

  /* The 'execv' prototype indicates that the strings
     pointed to by the argument pointers are not 'const'
     qualified.  Make a copy to be on the safe side.  */
  for (i = 0; i < nargs; ++i)
    {
      char *arg_copy = NULL;
      if (exec_args[i] != NULL)
	arg_copy = xstrdup (exec_args[i]);
      exec_arg_list[i] = arg_copy;
    }

  if (execv (exec_args[0], exec_arg_list) < 0)
    {
      perror (exec_args[0]);
      exit (255);
    }
  /* no return */
  exit (255);
}
