/* gc.c -- Functions to remember and garbage collect unused node contents. */

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

#include "info.h"

/* Array of pointers to the contents of gc-able nodes.  A pointer on this
   list can be garbage collected when no info window contains a node whose
   contents member match the pointer. */
static char **gcable_pointers = (char **)NULL;
static int gcable_pointers_index = 0;
static int gcable_pointers_slots = 0;

/* Add POINTER to the list of garbage collectible pointers.  A pointer
   is not actually garbage collected until no info window contains a node
   whose contents member is equal to the pointer. */
void
add_gcable_pointer (pointer)
     char *pointer;
{
  gc_pointers ();
  add_pointer_to_array (pointer, gcable_pointers_index, gcable_pointers,
			gcable_pointers_slots, 10, char *);
}

/* Grovel the list of info windows and gc-able pointers finding those
   node->contents which are collectible, and free them. */
void
gc_pointers ()
{
  register int i, j, k;
  INFO_WINDOW *iw;
  char **new = (char **)NULL;
  int new_index = 0;
  int new_slots = 0;

  if (!info_windows || !gcable_pointers_index)
    return;

  for (i = 0; (iw = info_windows[i]); i++)
    {
      for (j = 0; j < iw->nodes_index; j++)
	{
	  NODE *node = iw->nodes[j];

	  /* If this node->contents appears in our list of gcable_pointers,
	     it is not gc-able, so save it. */
	  for (k = 0; k < gcable_pointers_index; k++)
	    if (gcable_pointers[k] == node->contents)
	      {
		add_pointer_to_array
		  (node->contents, new_index, new, new_slots, 10, char *);
		break;
	      }
	}
    }

  /* We have gathered all of the pointers which need to be saved.  Free any
     of the original pointers which do not appear in the new list. */
  for (i = 0; i < gcable_pointers_index; i++)
    {
      for (j = 0; j < new_index; j++)
	if (gcable_pointers[i] == new[j])
	  break;

      /* If we got all the way through the new list, then the old pointer
	 can be garbage collected. */
      if (new && !new[j])
	free (gcable_pointers[i]);
    }

  free (gcable_pointers);
  gcable_pointers = new;
  gcable_pointers_slots = new_slots;
  gcable_pointers_index = new_index;
}
