/* footnotes.c -- Some functions for manipulating footnotes. */

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

/* Non-zero means attempt to show footnotes when displaying a new window. */
int auto_footnotes_p = 1;

static char *footnote_nodename = "*Footnotes*";

#define FOOTNOTE_HEADER_FORMAT \
   "*** Footnotes appearing in the node \"%s\" ***\n"

/* Find the window currently showing footnotes. */
static WINDOW *
find_footnotes_window ()
{
  WINDOW *win;

  /* Try to find an existing window first. */
  for (win = windows; win; win = win->next)
    if (internal_info_node_p (win->node) &&
	(strcmp (win->node->nodename, footnote_nodename) == 0))
      break;

  return (win);
}

/* Manufacture a node containing the footnotes of this node, and
   return the manufactured node.  If NODE has no footnotes, return a 
   NULL pointer. */
NODE *
make_footnotes_node (node)
     NODE *node;
{
  NODE *fn_node, *result = (NODE *)NULL;
  long fn_start;

  /* Make the initial assumption that the footnotes appear as simple
     text within this windows node. */
  fn_node = node;

  /* See if this node contains the magic footnote label. */
  fn_start =
    info_search_in_node (FOOTNOTE_LABEL, node, 0, (WINDOW *)NULL, 1);

  /* If it doesn't, check to see if it has an associated footnotes node. */
  if (fn_start == -1)
    {
      REFERENCE **refs;

      refs = info_xrefs_of_node (node);

      if (refs)
	{
	  register int i;
	  char *refname;

	  refname = (char *)xmalloc
	    (1 + strlen ("-Footnotes") + strlen (node->nodename));

	  strcpy (refname, node->nodename);
	  strcat (refname, "-Footnotes");

	  for (i = 0; refs[i]; i++)
	    if ((refs[i]->nodename != (char *)NULL) &&
		(strcmp (refs[i]->nodename, refname) == 0))
	      {
		char *filename;

		filename = node->parent;
		if (!filename)
		  filename = node->filename;

		fn_node = info_get_node (filename, refname);

		if (fn_node)
		  fn_start = 0;

		break;
	      }

	  free (refname);
	  info_free_references (refs);
	}
    }

  /* If we never found the start of a footnotes area, quit now. */
  if (fn_start == -1)
    return ((NODE *)NULL);

  /* Make the new node. */
  result = (NODE *)xmalloc (sizeof (NODE));
  result->flags = 0;

  /* Get the size of the footnotes appearing within this node. */
  {
    char *header;
    long text_start = fn_start;

    header = (char *)xmalloc
      (1 + strlen (node->nodename) + strlen (FOOTNOTE_HEADER_FORMAT));
    sprintf (header, FOOTNOTE_HEADER_FORMAT, node->nodename);

    /* Move the start of the displayed text to right after the first line.
       This effectively skips either "---- footno...", or "File: foo...". */
    while (text_start < fn_node->nodelen)
      if (fn_node->contents[text_start++] == '\n')
	break;
  
    result->nodelen = strlen (header) + fn_node->nodelen - text_start;

    /* Set the contents of this node. */
    result->contents = (char *)xmalloc (1 + result->nodelen);
    sprintf (result->contents, "%s", header);
    memcpy (result->contents + strlen (header),
	    fn_node->contents + text_start, fn_node->nodelen - text_start);

    name_internal_node (result, footnote_nodename);
    free (header);
  }

#if defined (NOTDEF)
  /* If the footnotes were gleaned from the node that we were called with,
     shorten the calling node's display length. */
  if (fn_node == node)
    narrow_node (node, 0, fn_start);
#endif /* NOTDEF */

  return (result);
}

/* Create or delete the footnotes window depending on whether footnotes
   exist in WINDOW's node or not.  Returns FN_FOUND if footnotes were found
   and displayed.  Returns FN_UNFOUND if there were no footnotes found
   in WINDOW's node.  Returns FN_UNABLE if there were footnotes, but the
   window to show them couldn't be made. */
int
info_get_or_remove_footnotes (window)
     WINDOW *window;
{
  WINDOW *fn_win;
  NODE *new_footnotes;

  fn_win = find_footnotes_window ();

  /* If we are in the footnotes window, change nothing. */
  if (fn_win == window)
    return (FN_FOUND);

  /* Try to find footnotes for this window's node. */
  new_footnotes = make_footnotes_node (window->node);

  /* If there was a window showing footnotes, and there are no footnotes
     for the current window, delete the old footnote window. */
  if (fn_win && !new_footnotes)
    {
      if (windows->next)
	info_delete_window_internal (fn_win);
    }

  /* If there are footnotes for this window's node, but no window around
     showing footnotes, try to make a new window. */
  if (new_footnotes && !fn_win)
    {
      WINDOW *old_active;
      WINDOW *last, *win;

      /* Always make this window be the last one appearing in the list.  Find
	 the last window in the chain. */
      for (win = windows, last = windows; win; last = win, win = win->next);

      /* Try to split this window, and make the split window the one to
	 contain the footnotes. */
      old_active = active_window;
      active_window = last;
      fn_win = window_make_window (new_footnotes);
      active_window = old_active;

      if (!fn_win)
	{
	  free (new_footnotes->contents);
	  free (new_footnotes);

	  /* If we are hacking automatic footnotes, and there are footnotes
	     but we couldn't display them, print a message to that effect. */
	  if (auto_footnotes_p)
	    inform_in_echo_area ("Footnotes could not be displayed");
	  return (FN_UNABLE);
	}
    }

  /* If there are footnotes, and there is a window to display them,
     make that window be the number of lines appearing in the footnotes. */
  if (new_footnotes && fn_win)
    {
      window_set_node_of_window (fn_win, new_footnotes);

      window_change_window_height
	(fn_win, fn_win->line_count - fn_win->height);

      remember_window_and_node (fn_win, new_footnotes);
      add_gcable_pointer (new_footnotes->contents);
    }

  if (!new_footnotes)
    return (FN_UNFOUND);
  else
    return (FN_FOUND);
}

/* Show the footnotes associated with this node in another window. */
DECLARE_INFO_COMMAND (info_show_footnotes,
   "Show the footnotes associated with this node in another window")
{
  int result;

  /* A negative argument means just make the window go away. */
  if (count < 0)
    {
      WINDOW *fn_win = find_footnotes_window ();

      /* If there is an old footnotes window, and it isn't the only window
	 on the screen, delete it. */
      if (fn_win && windows->next)
	info_delete_window_internal (fn_win);
    }
  else
    {
      int result;

      result = info_get_or_remove_footnotes (window);

      switch (result)
	{
	case FN_UNFOUND:
	  info_error (NO_FOOT_NODE);
	  break;

	case FN_UNABLE:
	  info_error (WIN_TOO_SMALL);
	  break;
	}
    }
}
