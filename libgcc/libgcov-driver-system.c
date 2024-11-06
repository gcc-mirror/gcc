/* Routines required for instrumenting a program.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1989-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#if !IN_GCOV_TOOL
/* Configured via the GCOV_ERROR_FILE environment variable;
   it will either be stderr, or a file of the user's choosing.
   Non-static to prevent multiple gcov-aware shared objects from
   instantiating their own copies. */
FILE *__gcov_error_file = NULL;
#endif

/* A utility function to populate the __gcov_error_file pointer.
   This should NOT be called outside of the gcov system driver code. */

static FILE *
get_gcov_error_file (void)
{
#if IN_GCOV_TOOL
  return stderr;
#else
  if (!__gcov_error_file)
    {
      const char *gcov_error_filename = getenv ("GCOV_ERROR_FILE");

      if (gcov_error_filename)
	__gcov_error_file = fopen (gcov_error_filename, "a");
      if (!__gcov_error_file)
	__gcov_error_file = stderr;
    }
  return __gcov_error_file;
#endif
}

/* A utility function for outputting errors.  */

static int __attribute__((format(printf, 1, 2)))
gcov_error (const char *fmt, ...)
{
  int ret;
  va_list argp;

  va_start (argp, fmt);
  FILE *f = get_gcov_error_file ();
  ret = vfprintf (f, fmt, argp);
  va_end (argp);

  if (getenv ("GCOV_EXIT_AT_ERROR"))
    {
      fprintf (f, "profiling:exiting after an error\n");
      exit (1);
    }

  return ret;
}

#if !IN_GCOV_TOOL
static void
gcov_error_exit (void)
{
  if (__gcov_error_file && __gcov_error_file != stderr)
    {
      fclose (__gcov_error_file);
      __gcov_error_file = NULL;
    }
}
#endif

/* Make sure path component of the given FILENAME exists, create
   missing directories. FILENAME must be writable.
   Returns zero on success, or -1 if an error occurred.  */

static int
create_file_directory (char *filename)
{
#if !defined(TARGET_POSIX_IO) && !defined(_WIN32)
  (void) filename;
  return -1;
#else
  char *s;

  s = filename;

  if (HAS_DRIVE_SPEC(s))
    s += 2;
  if (IS_DIR_SEPARATOR(*s))
    ++s;
  for (; *s != '\0'; s++)
    if (IS_DIR_SEPARATOR(*s))
      {
        char sep = *s;
        *s  = '\0';

        /* Try to make directory if it doesn't already exist.  */
        if (access (filename, F_OK) == -1
#ifdef TARGET_POSIX_IO
	    && mkdir (filename, 0777) == -1
#else
#ifdef mkdir
#undef mkdir
#endif
            && mkdir (filename) == -1
#endif
            /* The directory might have been made by another process.  */
            && errno != EEXIST)
          {
            gcov_error ("profiling:%s:Cannot create directory\n", filename);
            *s = sep;
            return -1;
          };

        *s = sep;
      };
  return 0;
#endif
}

/* Replace filename variables in FILENAME.  We currently support expansion:

   %p - process ID
   %q{ENV} - value of environment variable ENV
   */

static char *
replace_filename_variables (char *filename)
{
  char buffer[16];
  char empty[] = "";
  for (char *p = filename; *p != '\0'; p++)
    {
      unsigned length = strlen (filename);
      if (*p == '%' && *(p + 1) != '\0')
	{
	  unsigned start = p - filename;
	  p++;
	  char *replacement = NULL;
	  switch (*p)
	    {
	    case 'p':
	      sprintf (buffer, "%d", getpid ());
	      replacement = buffer;
	      p++;
	      break;
	    case 'q':
	      if (*(p + 1) == '{')
		{
		  p += 2;
		  char *e = strchr (p, '}');
		  if (e)
		    {
		      *e = '\0';
		      replacement = getenv (p);
		      if (replacement == NULL)
			replacement = empty;
		      p = e + 1;
		    }
		  else
		    return filename;
		}
	      break;
	    default:
	      return filename;
	    }

	  /* Concat beginning of the path, replacement and
	     ending of the path.  */
	  unsigned end = length - (p - filename);
	  unsigned repl_length = replacement != NULL ? strlen (replacement) : 0;

	  char *buffer = (char *)xmalloc (start + end + repl_length + 1);
	  char *buffer_ptr = buffer;
	  buffer_ptr = (char *)memcpy (buffer_ptr, filename, start);
	  buffer_ptr += start;
	  if (replacement != NULL)
	    buffer_ptr = (char *)memcpy (buffer_ptr, replacement, repl_length);
	  buffer_ptr += repl_length;
	  buffer_ptr = (char *)memcpy (buffer_ptr, p, end);
	  buffer_ptr += end;
	  *buffer_ptr = '\0';

	  free (filename);
	  filename = buffer;
	  p = buffer + start + repl_length;
	}
    }

  return filename;
}

static void
allocate_filename_struct (struct gcov_filename *gf)
{
  const char *gcov_prefix;
  size_t prefix_length;
  int strip = 0;
  gf->filename = NULL;

  {
    /* Check if the level of dirs to strip off specified. */
    char *tmp = getenv("GCOV_PREFIX_STRIP");
    if (tmp)
      {
        strip = atoi (tmp);
        /* Do not consider negative values. */
        if (strip < 0)
          strip = 0;
      }
  }
  gf->strip = strip;

  /* Get file name relocation prefix.  Non-absolute values are ignored. */
  gcov_prefix = getenv("GCOV_PREFIX");
  prefix_length = gcov_prefix ? strlen (gcov_prefix) : 0;

  /* Remove an unnecessary trailing '/' */
  if (prefix_length && IS_DIR_SEPARATOR (gcov_prefix[prefix_length - 1]))
    prefix_length--;

  /* If no prefix was specified and a prefix stip, then we assume
     relative.  */
  if (!prefix_length && gf->strip)
    {
      gcov_prefix = ".";
      prefix_length = 1;
    }

  /* Allocate and initialize the filename scratch space.  */
  if (prefix_length)
    {
      gf->prefix = (char *) xmalloc (prefix_length + 1);
      char *p = (char *) memcpy (gf->prefix, gcov_prefix, prefix_length);
      *(p + prefix_length) = '\0';
    }
  else
    gf->prefix = NULL;
}

/* Open a gcda file specified by GI_FILENAME.
   Return -1 on error.  Return 0 on success.  */

static int
gcov_exit_open_gcda_file (struct gcov_info *gi_ptr,
			  struct gcov_filename *gf,
			  int mode)
{
  int append_slash = 0;
  const char *fname = gi_ptr->filename;

  /* Build relocated filename, stripping off leading
     directories from the initial filename if requested. */
  if (gf->strip > 0)
    {
      const char *probe = fname;
      int level;

      /* Remove a leading separator, without counting it.  */
      if (IS_DIR_SEPARATOR (*probe))
	probe++;

      /* Skip selected directory levels.  If we fall off the end, we
	 keep the final part.  */
      for (level = gf->strip; *probe && level; probe++)
        if (IS_DIR_SEPARATOR (*probe))
          {
            fname = probe;
            level--;
          }
    }

  /* Update complete filename with stripped original. */
  if (gf->prefix)
    {
      /* Avoid to add multiple drive letters into combined path.  */
      if (HAS_DRIVE_SPEC(fname))
	fname += 2;

      if (!IS_DIR_SEPARATOR (*fname))
	append_slash = 1;
    }

  size_t prefix_length = gf->prefix ? strlen (gf->prefix) : 0;
  gf->filename = (char *) xmalloc (prefix_length + strlen (fname) + 2);
  *gf->filename = '\0';
  if (prefix_length)
    strcat (gf->filename, gf->prefix);
  if (append_slash)
    *gf->filename++ = '/';
  strcat (gf->filename, fname);

  gf->filename = replace_filename_variables (gf->filename);

  if (!gcov_open (gf->filename, mode))
    {
      /* Open failed likely due to missed directory.
         Create directory and retry to open file. */
      if (create_file_directory (gf->filename))
        {
          fprintf (stderr, "profiling:%s:Skip\n", gf->filename);
          return -1;
        }
      if (!gcov_open (gf->filename, mode))
        {
          fprintf (stderr, "profiling:%s:Cannot open\n", gf->filename);
          return -1;
        }
    }

  return 0;
}
