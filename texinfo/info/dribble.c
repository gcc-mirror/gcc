/* dribble.c -- Dribble files for Info. */

/* This file is part of GNU Info, a program for reading online documentation
   stored in Info format.

   Copyright (C) 1993 Free Software Foundation, Inc.

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

#include <stdio.h>
#include "dribble.h"

/* When non-zero, it is a stream to write all input characters to for the
   duration of this info session. */
FILE *info_dribble_file = (FILE *)NULL;

/* Open a dribble file named NAME, perhaps closing an already open one.
   This sets the global variable INFO_DRIBBLE_FILE to the open stream. */
void
open_dribble_file (name)
     char *name;
{
  /* Perhaps close existing dribble file. */
  close_dribble_file ();

  info_dribble_file = fopen (name, "w");

#if defined (HAVE_SETVBUF)
  if (info_dribble_file)
#  if defined (SETVBUF_REVERSED)
    setvbuf (info_dribble_file, _IONBF, (char *)NULL, 1);
#  else
    setvbuf (info_dribble_file, (char *)NULL, _IONBF, 1);
#  endif /* !SETVBUF_REVERSED */
#endif /* HAVE_SETVBUF */
}

/* If there is a dribble file already open, close it. */
void
close_dribble_file ()
{
  if (info_dribble_file)
    {
      fflush (info_dribble_file);
      fclose (info_dribble_file);
      info_dribble_file = (FILE *)NULL;
    }
}

/* Write some output to our existing dribble file. */
void
dribble (byte)
     unsigned char byte;
{
  if (info_dribble_file)
    fwrite (&byte, sizeof (unsigned char), 1, info_dribble_file);
}
