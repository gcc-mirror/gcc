/* Implement runtime actions for CHILL.
   Copyright (C) 1992,1993 Free Software Foundation, Inc.
   Author: Wilfried Moser

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include "iomodes.h"

/* predefined associations, accesses, and text for stdin, stdout, stderr */
/* stdin */
#define STDIO_TEXT_LENGTH 1024
#define STDIN_TEXT_LENGTH STDIO_TEXT_LENGTH

static Access_Mode stdin_access;

#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif

static
Association_Mode stdin_association =
{
  IO_EXISTING | IO_READABLE | IO_SEQUENCIBLE | IO_ISASSOCIATED,
  NULL,
  &stdin_access,
  STDIN_FILENO,
  NULL,
  0,
  ReadOnly
};

static Access_Mode stdin_access = 
{
  IO_TEXTIO,
  STDIN_TEXT_LENGTH + 2,
  0,
  0,
  &stdin_association,
  0,
  NULL,
  VaryingChars
};

static
VARYING_STRING(STDIN_TEXT_LENGTH) stdin_text_record;

Text_Mode chill_stdin =
{
  IO_TEXTLOCATION,
  (VarString *)&stdin_text_record,
  &stdin_access,
  0
};

/* stdout */
#define STDOUT_TEXT_LENGTH STDIO_TEXT_LENGTH
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif

static Access_Mode stdout_access;

static
Association_Mode stdout_association =
{
  IO_EXISTING | IO_WRITEABLE | IO_SEQUENCIBLE | IO_ISASSOCIATED,
  NULL,
  &stdout_access,
  STDOUT_FILENO,
  NULL,
  0,
  WriteOnly
};

static Access_Mode stdout_access = 
{
  IO_TEXTIO,
  STDOUT_TEXT_LENGTH + 2,
  0,
  0,
  &stdout_association,
  0,
  NULL,
  VaryingChars
};

static
VARYING_STRING(STDOUT_TEXT_LENGTH) stdout_text_record;

Text_Mode chill_stdout =
{
  IO_TEXTLOCATION,
  (VarString *)&stdout_text_record,
  &stdout_access,
  0
};

/* stderr */
#define STDERR_TEXT_LENGTH STDIO_TEXT_LENGTH
#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif

static Access_Mode stderr_access;

static
Association_Mode stderr_association =
{
  IO_EXISTING | IO_WRITEABLE | IO_SEQUENCIBLE | IO_ISASSOCIATED,
  NULL,
  &stderr_access,
  STDERR_FILENO,
  NULL,
  0,
  WriteOnly
};

static Access_Mode stderr_access = 
{
  IO_TEXTIO,
  STDERR_TEXT_LENGTH + 2,
  0,
  0,
  &stderr_association,
  0,
  NULL,
  VaryingChars
};

static
VARYING_STRING(STDIN_TEXT_LENGTH) stderr_text_record;

Text_Mode chill_stderr =
{
  IO_TEXTLOCATION,
  (VarString *)&stderr_text_record,
  &stderr_access,
  0
};

/*
 * function __xmalloc_
 *
 * parameter:
 *   size		number of bytes to allocate
 *
 * returns:
 *  void*
 *
 * abstract:
 *  This is the general allocation routine for libchill
 *
 */

void *
__xmalloc_ (size)
int size;
{
  void	*tmp = malloc (size);
  
  if (!tmp)
    {
      fprintf (stderr, "ChillLib: Out of heap space.\n");
      fflush (stderr);
      exit (ENOMEM);
    }
  return (tmp);
} /* __xmalloc_ */

static char *
newstring (char *str)
{
  char *tmp = __xmalloc_ (strlen (str) + 1);
  strcpy (tmp, str);
  return tmp;
}

static void setup_stdinout (void) __attribute__((constructor));

static void
setup_stdinout ()
{
  /* allocate the names */
  stdin_association.pathname = newstring ("stdin");
  stdout_association.pathname = newstring ("stdout");
  stderr_association.pathname = newstring ("stderr");

  /* stdin needs a readbuffer */
  stdin_association.bufptr = __xmalloc_ (sizeof (readbuf_t));
  memset (stdin_association.bufptr, 0, sizeof (readbuf_t));
}
