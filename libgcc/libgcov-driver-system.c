/* Routines required for instrumenting a program.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1989-2014 Free Software Foundation, Inc.

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

/* A utility function for outputing errors.  */

static int __attribute__((format(printf, 1, 2)))
gcov_error (const char *fmt, ...)
{
  int ret;
  va_list argp;
  va_start (argp, fmt);
  ret = vfprintf (stderr, fmt, argp);
  va_end (argp);
  return ret;
}

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
            && mkdir (filename, 0755) == -1
#else
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

static void
allocate_filename_struct (struct gcov_filename_aux *gf)
{
  const char *gcov_prefix;
  int gcov_prefix_strip = 0;
  size_t prefix_length;
  char *gi_filename_up;

  gcc_assert (gf);
  {
    /* Check if the level of dirs to strip off specified. */
    char *tmp = getenv("GCOV_PREFIX_STRIP");
    if (tmp)
      {
        gcov_prefix_strip = atoi (tmp);
        /* Do not consider negative values. */
        if (gcov_prefix_strip < 0)
          gcov_prefix_strip = 0;
      }
  }

  /* Get file name relocation prefix.  Non-absolute values are ignored. */
  gcov_prefix = getenv("GCOV_PREFIX");
  if (gcov_prefix)
    {
      prefix_length = strlen(gcov_prefix);

      /* Remove an unnecessary trailing '/' */
      if (IS_DIR_SEPARATOR (gcov_prefix[prefix_length - 1]))
        prefix_length--;
    }
  else
    prefix_length = 0;

  /* If no prefix was specified and a prefix stip, then we assume
     relative.  */
  if (gcov_prefix_strip != 0 && prefix_length == 0)
    {
      gcov_prefix = ".";
      prefix_length = 1;
    }
  /* Allocate and initialize the filename scratch space plus one.  */
  gi_filename = (char *) malloc (prefix_length + gcov_max_filename + 2);
  if (prefix_length)
    memcpy (gi_filename, gcov_prefix, prefix_length);
  gi_filename_up = gi_filename + prefix_length;

  gf->gi_filename_up = gi_filename_up;
  gf->prefix_length = prefix_length;
  gf->gcov_prefix_strip = gcov_prefix_strip;
}

/* Open a gcda file specified by GI_FILENAME.
   Return -1 on error.  Return 0 on success.  */

static int
gcov_exit_open_gcda_file (struct gcov_info *gi_ptr, struct gcov_filename_aux *gf)
{
  int gcov_prefix_strip;
  size_t prefix_length;
  char *gi_filename_up;
  const char *fname, *s;

  gcov_prefix_strip = gf->gcov_prefix_strip;
  gi_filename_up = gf->gi_filename_up;
  prefix_length = gf->prefix_length;
  fname = gi_ptr->filename;

  /* Avoid to add multiple drive letters into combined path.  */
  if (prefix_length != 0 && HAS_DRIVE_SPEC(fname))
    fname += 2;

  /* Build relocated filename, stripping off leading
     directories from the initial filename if requested. */
  if (gcov_prefix_strip > 0)
    {
      int level = 0;

      s = fname;
      if (IS_DIR_SEPARATOR(*s))
        ++s;

      /* Skip selected directory levels. */
      for (; (*s != '\0') && (level < gcov_prefix_strip); s++)
        if (IS_DIR_SEPARATOR(*s))
          {
            fname = s;
            level++;
          }
    }

  /* Update complete filename with stripped original. */
  if (prefix_length != 0 && !IS_DIR_SEPARATOR (*fname))
    {
      /* If prefix is given, add directory separator.  */
      strcpy (gi_filename_up, "/");
      strcpy (gi_filename_up + 1, fname);
    }
  else
    strcpy (gi_filename_up, fname);

  if (!gcov_open (gi_filename))
    {
      /* Open failed likely due to missed directory.
         Create directory and retry to open file. */
      if (create_file_directory (gi_filename))
        {
          fprintf (stderr, "profiling:%s:Skip\n", gi_filename);
          return -1;
        }
      if (!gcov_open (gi_filename))
        {
          fprintf (stderr, "profiling:%s:Cannot open\n", gi_filename);
          return -1;
        }
    }

  return 0;
}
