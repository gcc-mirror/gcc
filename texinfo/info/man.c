/*  man.c: How to read and format man files. */

/* This file is part of GNU Info, a program for reading online documentation
   stored in Info format.

   Copyright (C) 1995 Free Software Foundation, Inc.

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

   Written by Brian Fox Thu May  4 09:17:52 1995 (bfox@ai.mit.edu). */

#include "info.h"
#include <sys/ioctl.h>
#include <sys/file.h>
#include "signals.h"
#if defined (HAVE_SYS_TIME_H)
#include <sys/time.h>
#endif
#if defined (HAVE_SYS_WAIT_H)
#include <sys/wait.h>
#endif
#include "tilde.h"

#include "man.h"

#if !defined (_POSIX_VERSION)
#define pid_t int
#endif

#if defined (FD_SET)
#  if defined (hpux)
#    define fd_set_cast(x) (int *)(x)
#  else
#    define fd_set_cast(x) (fd_set *)(x)
#  endif /* !hpux */
#endif /* FD_SET */

static char *read_from_fd ();
static void clean_manpage ();
static NODE *manpage_node_of_file_buffer ();
static char *get_manpage_contents ();

NODE *
make_manpage_node (pagename)
     char *pagename;
{
  return (info_get_node (MANPAGE_FILE_BUFFER_NAME, pagename));
}

NODE *
get_manpage_node (file_buffer, pagename)
     FILE_BUFFER *file_buffer;
     char *pagename;
{
  NODE *node;

  node = manpage_node_of_file_buffer (file_buffer, pagename);

  if (!node)
    {
      char *page;

      page = get_manpage_contents (pagename);

      if (page)
	{
	  char header[1024];
	  long oldsize, newsize;
	  int hlen, plen;

	  sprintf (header, "\n\n%c\n%s %s,  %s %s,  %s (dir)\n\n",
		   INFO_COOKIE,
		   INFO_FILE_LABEL, file_buffer->filename,
		   INFO_NODE_LABEL, pagename,
		   INFO_UP_LABEL);
	  oldsize = file_buffer->filesize;
	  hlen = strlen (header);
	  plen = strlen (page);
	  newsize = (oldsize + hlen + plen);
	  file_buffer->contents =
	    (char *)xrealloc (file_buffer->contents, 1 + newsize);
	  memcpy (file_buffer->contents + oldsize, header, hlen);
	  oldsize += hlen;
	  memcpy (file_buffer->contents + oldsize, page, plen);
	  file_buffer->contents[newsize] = '\0';
	  file_buffer->filesize = newsize;
	  file_buffer->finfo.st_size = newsize;
	  build_tags_and_nodes (file_buffer);
	  free (page);
	}

      node = manpage_node_of_file_buffer (file_buffer, pagename);
    }

  return (node);
}

FILE_BUFFER *
create_manpage_file_buffer ()
{
  FILE_BUFFER *file_buffer;
  struct stat *finfo;

  file_buffer = make_file_buffer ();
  file_buffer->filename = strdup (MANPAGE_FILE_BUFFER_NAME);
  file_buffer->fullpath = strdup (MANPAGE_FILE_BUFFER_NAME);
  file_buffer->finfo.st_size = 0;
  file_buffer->filesize = 0;
  file_buffer->contents = (char *)NULL;
  file_buffer->flags = (N_IsInternal | N_CannotGC | N_IsManPage);
  
  return (file_buffer);
}

/* Scan the list of directories in PATH looking for FILENAME.  If we find
   one that is an executable file, return it as a new string.  Otherwise,
   return a NULL pointer. */
static char *
executable_file_in_path (filename, path)
     char *filename, *path;
{
  struct stat finfo;
  char *temp_dirname;
  int statable, dirname_index;

  dirname_index = 0;

  while (temp_dirname = extract_colon_unit (path, &dirname_index))
    {
      register int i;
      char *temp;

      /* Expand a leading tilde if one is present. */
      if (*temp_dirname == '~')
	{
	  char *expanded_dirname;

	  expanded_dirname = tilde_expand_word (temp_dirname);
	  free (temp_dirname);
	  temp_dirname = expanded_dirname;
	}

      temp = (char *)xmalloc (30 + strlen (temp_dirname) + strlen (filename));
      strcpy (temp, temp_dirname);
      if (temp[(strlen (temp)) - 1] != '/')
	strcat (temp, "/");
      strcat (temp, filename);

      free (temp_dirname);

      statable = (stat (temp, &finfo) == 0);

      /* If we have found a regular executable file, then use it. */
      if ((statable) && (S_ISREG (finfo.st_mode)) &&
	  (access (temp, X_OK) == 0))
	return (temp);
      else
	free (temp);
    }
  return ((char *)NULL);
}

/* Return the full pathname of the system man page formatter. */
static char *
find_man_formatter ()
{
  return (executable_file_in_path ("man", (char *)getenv ("PATH")));
}

static char *manpage_pagename = (char *)NULL;
static char *manpage_section  = (char *)NULL;

static void
get_page_and_section (pagename)
     char *pagename;
{
  register int i;

  if (manpage_pagename)
    free (manpage_pagename);

  if (manpage_section)
    free (manpage_section);

  manpage_pagename = (char *)NULL;
  manpage_section  = (char *)NULL;

  for (i = 0; pagename[i] != '\0' && pagename[i] != '('; i++);

  manpage_pagename = (char *)xmalloc (1 + i);
  strncpy (manpage_pagename, pagename, i);
  manpage_pagename[i] = '\0';

  if (pagename[i] == '(')
    {
      int start;

      start = i + 1;

      for (i = start; pagename[i] != '\0' && pagename[i] != ')'; i++);

      manpage_section = (char *)xmalloc (1 + (i - start));
      strncpy (manpage_section, pagename + start, (i - start));
      manpage_section[i - start] = '\0';
    }
}

static void
reap_children (sig)
     int sig;
{
  unsigned int status;
  wait (&status);
}

static char *
get_manpage_contents (pagename)
     char *pagename;
{
  static char *formatter_args[4] = { (char *)NULL };
  int pipes[2];
  pid_t child;
  char *formatted_page = (char *)NULL;
  char *section = (char *)NULL;
  int arg_index = 1;

  if (formatter_args[0] == (char *)NULL)
    formatter_args[0] = find_man_formatter ();

  if (formatter_args[0] == (char *)NULL)
    return ((char *)NULL);

  get_page_and_section (pagename);

  if (manpage_section != (char *)NULL)
    formatter_args[arg_index++] = manpage_section;

  formatter_args[arg_index++] = manpage_pagename;
  formatter_args[arg_index] = (char *)NULL;

  /* Open a pipe to this program, read the output, and save it away
     in FORMATTED_PAGE.  The reader end of the pipe is pipes[0]; the
     writer end is pipes[1]. */
  pipe (pipes);

  signal (SIGCHLD, reap_children);

  child = fork ();

  if (child == -1)
    return ((char *)NULL);

  if (child != 0)
    {
      /* In the parent, close the writing end of the pipe, and read from
	 the exec'd child. */
      close (pipes[1]);
      formatted_page = read_from_fd (pipes[0]);
      close (pipes[0]);
    }
  else
    {
      /* In the child, close the read end of the pipe, make the write end
	 of the pipe be stdout, and execute the man page formatter. */
      close (pipes[0]);
      close (fileno (stderr));
      close (fileno (stdin));	/* Don't print errors. */
      dup2 (pipes[1], fileno (stdout));

      execv (formatter_args[0], formatter_args);

      /* If we get here, we couldn't exec, so close out the pipe and
	 exit. */
      close (pipes[1]);
      exit (0);
    }

  /* If we have the page, then clean it up. */
  if (formatted_page)
    clean_manpage (formatted_page);

  return (formatted_page);
}

static void
clean_manpage (manpage)
     char *manpage;
{
  register int i, j;
  int newline_count = 0;
  char *newpage;

  newpage = (char *)xmalloc (1 + strlen (manpage));

  for (i = 0, j = 0; newpage[j] = manpage[i]; i++, j++)
    {
      if (manpage[i] == '\n')
	newline_count++;
      else
	newline_count = 0;

      if (newline_count == 3)
	{
	  j--;
	  newline_count--;
	}

      if (manpage[i] == '\b' || manpage[i] == '\f')
	j -= 2;
    }

  newpage[j++] = '\0';

  strcpy (manpage, newpage);
  free (newpage);
}

static NODE *
manpage_node_of_file_buffer (file_buffer, pagename)
     FILE_BUFFER *file_buffer;
     char *pagename;
{
  NODE *node = (NODE *)NULL;
  TAG *tag = (TAG *)NULL;

  if (file_buffer->contents)
    {
      register int i;

      for (i = 0; tag = file_buffer->tags[i]; i++)
	{
	  if (strcasecmp (pagename, tag->nodename) == 0)
	    break;
	}
    }

  if (tag)
    {
      node = (NODE *)xmalloc (sizeof (NODE));
      node->filename = file_buffer->filename;
      node->nodename = tag->nodename;
      node->contents = file_buffer->contents + tag->nodestart;
      node->nodelen = tag->nodelen;
      node->flags    = 0;
      node->parent   = (char *)NULL;
      node->flags = (N_HasTagsTable | N_IsManPage);
      node->contents += skip_node_separator (node->contents);
    }

  return (node);
}

static char *
read_from_fd (fd)
     int fd;
{
  struct timeval timeout;
  char *buffer = (char *)NULL;
  int bsize = 0;
  int bindex = 0;
  int select_result;
#if defined (FD_SET)
  fd_set read_fds;

  timeout.tv_sec = 15;
  timeout.tv_usec = 0;

  FD_ZERO (&read_fds);
  FD_SET (fd, &read_fds);

  select_result = select (fd + 1, fd_set_cast (&read_fds), 0, 0, &timeout);
#else /* !FD_SET */
  select_result = 1;
#endif /* !FD_SET */

  switch (select_result)
    {
    case 0:
    case -1:
      break;

    default:
      {
        int amount_read;
        int done = 0;

        while (!done)
          {
            while ((bindex + 1024) > (bsize))
              buffer = (char *)xrealloc (buffer, (bsize += 1024));
            buffer[bindex] = '\0';

            amount_read = read (fd, buffer + bindex, 1023);

            if (amount_read < 0)
              {
                done = 1;
              }
            else
              {
                bindex += amount_read;
                buffer[bindex] = '\0';
                if (amount_read == 0)
                  done = 1;
              }
          }
      }
    }

  if ((buffer != (char *)NULL) && (*buffer == '\0'))
    {
      free (buffer);
      buffer = (char *)NULL;
    }

  return (buffer);
}

static char *reference_section_starters[] =
{
  "\nRELATED INFORMATION",
  "\nRELATED\tINFORMATION",
  "RELATED INFORMATION\n",
  "RELATED\tINFORMATION\n",
  "\nSEE ALSO",
  "\nSEE\tALSO",
  "SEE ALSO\n",
  "SEE\tALSO\n",
  (char *)NULL
};

static SEARCH_BINDING frs_binding;

static SEARCH_BINDING *
find_reference_section (node)
     NODE *node;
{
  register int i;
  long position = -1;

  frs_binding.buffer = node->contents;
  frs_binding.start = 0;
  frs_binding.end = node->nodelen;
  frs_binding.flags = S_SkipDest;

  for (i = 0; reference_section_starters[i] != (char *)NULL; i++)
    {
      position = search_forward (reference_section_starters[i], &frs_binding);
      if (position != -1)
	break;
    }

  if (position == -1)
    return ((SEARCH_BINDING *)NULL);

  /* We found the start of the reference section, and point is right after
     the string which starts it.  The text from here to the next header
     (or end of buffer) contains the only references in this manpage. */
  frs_binding.start = position;

  for (i = frs_binding.start; i < frs_binding.end - 2; i++)
    {
      if ((frs_binding.buffer[i] == '\n') &&
	  (!whitespace (frs_binding.buffer[i + 1])))
	{
	  frs_binding.end = i;
	  break;
	}
    }

  return (&frs_binding);
}

REFERENCE **
xrefs_of_manpage (node)
     NODE *node;
{
  SEARCH_BINDING *reference_section;
  REFERENCE **refs = (REFERENCE **)NULL;
  int refs_index = 0;
  int refs_slots = 0;
  long position;

  reference_section = find_reference_section (node);

  if (reference_section == (SEARCH_BINDING *)NULL)
    return ((REFERENCE **)NULL);

  /* Grovel the reference section building a list of references found there.
     A reference is alphabetic characters followed by non-whitespace text
     within parenthesis. */
  reference_section->flags = 0;

  while ((position = search_forward ("(", reference_section)) != -1)
    {
      register int start, end;

      for (start = position; start > reference_section->start; start--)
	if (whitespace (reference_section->buffer[start]))
	  break;

      start++;

      for (end = position; end < reference_section->end; end++)
	{
	  if (whitespace (reference_section->buffer[end]))
	    {
	      end = start;
	      break;
	    }

	  if (reference_section->buffer[end] == ')')
	    {
	      end++;
	      break;
	    }
	}

      if (end != start)
	{
	  REFERENCE *entry;
	  int len = end - start;

	  entry = (REFERENCE *)xmalloc (sizeof (REFERENCE));
	  entry->label = (char *)xmalloc (1 + len);
	  strncpy (entry->label, (reference_section->buffer) + start, len);
	  entry->label[len] = '\0';
	  entry->filename = strdup (node->filename);
	  entry->nodename = strdup (entry->label);
	  entry->start = start;
	  entry->end = end;

	  add_pointer_to_array
	    (entry, refs_index, refs, refs_slots, 10, REFERENCE *);
	}

      reference_section->start = position + 1;
    }

  return (refs);
}

long
locate_manpage_xref (node, start, dir)
     NODE *node;
     long start;
     int dir;
{
  register int i, count;
  REFERENCE **refs;
  long position = -1;

  refs = xrefs_of_manpage (node);

  if (refs)
    {
      register int i, count;
      REFERENCE *entry;

      for (i = 0; refs[i]; i++);
      count = i;

      if (dir > 0)
	{
	  for (i = 0; entry = refs[i]; i++)
	    if (entry->start > start)
	      {
		position = entry->start;
		break;
	      }
	}
      else
	{
	  for (i = count - 1; i > -1; i--)
	    {
	      entry = refs[i];

	      if (entry->start < start)
		{
		  position = entry->start;
		  break;
		}
	    }
	}

      info_free_references (refs);
    }
  return (position);
}

/* This one was a little tricky.  The binding buffer that is passed in has
   a START and END value of 0 -- strlen (window-line-containing-point).
   The BUFFER is a pointer to the start of that line. */
REFERENCE **
manpage_xrefs_in_binding (node, binding)
     NODE *node;
     SEARCH_BINDING *binding;
{
  register int i;
  REFERENCE **all_refs = xrefs_of_manpage (node);
  REFERENCE **brefs = (REFERENCE **)NULL;
  REFERENCE *entry;
  int brefs_index = 0;
  int brefs_slots = 0;
  int start, end;

  if (!all_refs)
    return ((REFERENCE **)NULL);

  start = binding->start + (binding->buffer - node->contents);
  end = binding->end + (binding->buffer - node->contents);

  for (i = 0; entry = all_refs[i]; i++)
    {
      if ((entry->start > start) && (entry->end < end))
	{
	  add_pointer_to_array
	    (entry, brefs_index, brefs, brefs_slots, 10, REFERENCE *);
	}
      else
	{
	  maybe_free (entry->label);
	  maybe_free (entry->filename);
	  maybe_free (entry->nodename);
	  free (entry);
	}
    }

  free (all_refs);
  return (brefs);
}
