/* tilde.c -- Tilde expansion code (~/foo := $HOME/foo).
   $Id: tilde.c,v 1.1.1.3 1998/03/24 18:20:19 law Exp $

   This file is part of GNU Info, a program for reading online documentation
   stored in Info format.

   Copyright (C) 1988, 89, 90, 91, 92, 93, 96, 98
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

   Written by Brian Fox (bfox@ai.mit.edu). */

/* Indent #pragma so that older Cpp's don't try to parse it. */
#ifdef _AIX
 #pragma alloca
#endif /* _AIX */

/* Include config.h before doing alloca.  */
#include "info.h"

#ifdef __GNUC__
# undef alloca
# define alloca __builtin_alloca
#else
# ifdef HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifndef _AIX
char *alloca ();
#  endif
# endif
#endif

#if defined (TEST) || defined (STATIC_MALLOC)
static void *xmalloc (), *xrealloc ();
#endif /* TEST || STATIC_MALLOC */

/* The default value of tilde_additional_prefixes.  This is set to
   whitespace preceding a tilde so that simple programs which do not
   perform any word separation get desired behaviour. */
static char *default_prefixes[] =
  { " ~", "\t~", (char *)NULL };

/* The default value of tilde_additional_suffixes.  This is set to
   whitespace or newline so that simple programs which do not
   perform any word separation get desired behaviour. */
static char *default_suffixes[] =
  { " ", "\n", (char *)NULL };

/* If non-null, this contains the address of a function to call if the
   standard meaning for expanding a tilde fails.  The function is called
   with the text (sans tilde, as in "foo"), and returns a malloc()'ed string
   which is the expansion, or a NULL pointer if there is no expansion. */
CFunction *tilde_expansion_failure_hook = (CFunction *)NULL;

/* When non-null, this is a NULL terminated array of strings which
   are duplicates for a tilde prefix.  Bash uses this to expand
   `=~' and `:~'. */
char **tilde_additional_prefixes = default_prefixes;

/* When non-null, this is a NULL terminated array of strings which match
   the end of a username, instead of just "/".  Bash sets this to
   `:' and `=~'. */
char **tilde_additional_suffixes = default_suffixes;

/* Find the start of a tilde expansion in STRING, and return the index of
   the tilde which starts the expansion.  Place the length of the text
   which identified this tilde starter in LEN, excluding the tilde itself. */
static int
tilde_find_prefix (string, len)
     char *string;
     int *len;
{
  register int i, j, string_len;
  register char **prefixes = tilde_additional_prefixes;

  string_len = strlen (string);
  *len = 0;

  if (!*string || *string == '~')
    return (0);

  if (prefixes)
    {
      for (i = 0; i < string_len; i++)
        {
          for (j = 0; prefixes[j]; j++)
            {
              if (strncmp (string + i, prefixes[j], strlen (prefixes[j])) == 0)
                {
                  *len = strlen (prefixes[j]) - 1;
                  return (i + *len);
                }
            }
        }
    }
  return (string_len);
}

/* Find the end of a tilde expansion in STRING, and return the index of
   the character which ends the tilde definition.  */
static int
tilde_find_suffix (string)
     char *string;
{
  register int i, j, string_len;
  register char **suffixes = tilde_additional_suffixes;

  string_len = strlen (string);

  for (i = 0; i < string_len; i++)
    {
      if (string[i] == '/' || !string[i])
        break;

      for (j = 0; suffixes && suffixes[j]; j++)
        {
          if (strncmp (string + i, suffixes[j], strlen (suffixes[j])) == 0)
            return (i);
        }
    }
  return (i);
}

/* Return a new string which is the result of tilde expanding STRING. */
char *
tilde_expand (string)
     char *string;
{
  char *result, *tilde_expand_word ();
  int result_size, result_index;

  result_size = result_index = 0;
  result = (char *)NULL;

  /* Scan through STRING expanding tildes as we come to them. */
  while (1)
    {
      register int start, end;
      char *tilde_word, *expansion;
      int len;

      /* Make START point to the tilde which starts the expansion. */
      start = tilde_find_prefix (string, &len);

      /* Copy the skipped text into the result. */
      if ((result_index + start + 1) > result_size)
        result = (char *)xrealloc (result, 1 + (result_size += (start + 20)));

      strncpy (result + result_index, string, start);
      result_index += start;

      /* Advance STRING to the starting tilde. */
      string += start;

      /* Make END be the index of one after the last character of the
         username. */
      end = tilde_find_suffix (string);

      /* If both START and END are zero, we are all done. */
      if (!start && !end)
        break;

      /* Expand the entire tilde word, and copy it into RESULT. */
      tilde_word = (char *)xmalloc (1 + end);
      strncpy (tilde_word, string, end);
      tilde_word[end] = '\0';
      string += end;

      expansion = tilde_expand_word (tilde_word);
      free (tilde_word);

      len = strlen (expansion);
      if ((result_index + len + 1) > result_size)
        result = (char *)xrealloc (result, 1 + (result_size += (len + 20)));

      strcpy (result + result_index, expansion);
      result_index += len;
      free (expansion);
    }

  result[result_index] = '\0';

  return (result);
}

/* Do the work of tilde expansion on FILENAME.  FILENAME starts with a
   tilde.  If there is no expansion, call tilde_expansion_failure_hook. */
char *
tilde_expand_word (filename)
     char *filename;
{
  char *dirname;

  dirname = filename ? xstrdup (filename) : (char *)NULL;

  if (dirname && *dirname == '~')
    {
      char *temp_name;
      if (!dirname[1] || dirname[1] == '/')
        {
          /* Prepend $HOME to the rest of the string. */
          char *temp_home = getenv ("HOME");

          /* If there is no HOME variable, look up the directory in
             the password database. */
          if (!temp_home)
            {
              struct passwd *entry;

              entry = (struct passwd *) getpwuid (getuid ());
              if (entry)
                temp_home = entry->pw_dir;
            }

          temp_name = (char *)
            alloca (1 + strlen (&dirname[1])
                    + (temp_home ? strlen (temp_home) : 0));
          temp_name[0] = '\0';
          if (temp_home)
            strcpy (temp_name, temp_home);
          strcat (temp_name, &dirname[1]);
          free (dirname);
          dirname = xstrdup (temp_name);
        }
      else
        {
          struct passwd *user_entry;
          char *username = (char *)alloca (257);
          int i, c;

          for (i = 1; (c = dirname[i]); i++)
            {
              if (c == '/')
                break;
              else
                username[i - 1] = c;
            }
          username[i - 1] = '\0';

          if (!(user_entry = (struct passwd *) getpwnam (username)))
            {
              /* If the calling program has a special syntax for
                 expanding tildes, and we couldn't find a standard
                 expansion, then let them try. */
              if (tilde_expansion_failure_hook)
                {
                  char *expansion;

                  expansion = (*tilde_expansion_failure_hook) (username);

                  if (expansion)
                    {
                      temp_name = (char *)alloca
                        (1 + strlen (expansion) + strlen (&dirname[i]));
                      strcpy (temp_name, expansion);
                      strcat (temp_name, &dirname[i]);
                      free (expansion);
                      goto return_name;
                    }
                }
              /* We shouldn't report errors. */
            }
          else
            {
              temp_name = (char *)alloca
                (1 + strlen (user_entry->pw_dir) + strlen (&dirname[i]));
              strcpy (temp_name, user_entry->pw_dir);
              strcat (temp_name, &dirname[i]);
            return_name:
              free (dirname);
              dirname = xstrdup (temp_name);
            }
            endpwent ();
        }
    }
  return (dirname);
}


#if defined (TEST)
#undef NULL
#include <stdio.h>

main (argc, argv)
     int argc;
     char **argv;
{
  char *result, line[512];
  int done = 0;

  while (!done)
    {
      printf ("~expand: ");
      fflush (stdout);

      if (!gets (line))
        strcpy (line, "done");

      if ((strcmp (line, "done") == 0) ||
          (strcmp (line, "quit") == 0) ||
          (strcmp (line, "exit") == 0))
        {
          done = 1;
          break;
        }

      result = tilde_expand (line);
      printf ("  --> %s\n", result);
      free (result);
    }
  exit (0);
}

static void memory_error_and_abort ();

static void *
xmalloc (bytes)
     int bytes;
{
  void *temp = (void *)malloc (bytes);

  if (!temp)
    memory_error_and_abort ();
  return (temp);
}

static void *
xrealloc (pointer, bytes)
     void *pointer;
     int bytes;
{
  void *temp;

  if (!pointer)
    temp = (char *)malloc (bytes);
  else
    temp = (char *)realloc (pointer, bytes);

  if (!temp)
    memory_error_and_abort ();

  return (temp);
}

static void
memory_error_and_abort ()
{
  fprintf (stderr, _("readline: Out of virtual memory!\n"));
  abort ();
}
#endif /* TEST */

