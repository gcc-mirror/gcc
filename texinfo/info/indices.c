/* indices.c -- Commands for dealing with an Info file Index. */

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
#include "indices.h"

/* User-visible variable controls the output of info-index-next. */
int show_index_match = 1;

/* In the Info sense, an index is a menu.  This variable holds the last
   parsed index. */
static REFERENCE **index_index = (REFERENCE **)NULL;

/* The offset of the most recently selected index element. */
static int index_offset = 0;

/* Variable which holds the last string searched for. */
static char *index_search = (char *)NULL;

/* A couple of "globals" describing where the initial index was found. */
static char *initial_index_filename = (char *)NULL;
static char *initial_index_nodename = (char *)NULL;

/* A structure associating index names with index offset ranges. */
typedef struct {
  char *name;			/* The nodename of this index. */
  int first;			/* The index in our list of the first entry. */
  int last;			/* The index in our list of the last entry. */
} INDEX_NAME_ASSOC;

/* An array associating index nodenames with index offset ranges. */
static INDEX_NAME_ASSOC **index_nodenames = (INDEX_NAME_ASSOC **)NULL;
static int index_nodenames_index = 0;
static int index_nodenames_slots = 0;

/* Add the name of NODE, and the range of the associated index elements
   (passed in ARRAY) to index_nodenames. */
static void
add_index_to_index_nodenames (array, node)
     REFERENCE **array;
     NODE *node;
{
  register int i, last;
  INDEX_NAME_ASSOC *assoc;

  for (last = 0; array[last]; last++);
  assoc = (INDEX_NAME_ASSOC *)xmalloc (sizeof (INDEX_NAME_ASSOC));
  assoc->name = strdup (node->nodename);

  if (!index_nodenames_index)
    {
      assoc->first = 0;
      assoc->last = last;
    }
  else
    {
      for (i = 0; index_nodenames[i + 1]; i++);
      assoc->first = 1 + index_nodenames[i]->last;
      assoc->last = assoc->first + last;
    }
  add_pointer_to_array
    (assoc, index_nodenames_index, index_nodenames, index_nodenames_slots,
     10, INDEX_NAME_ASSOC *);
}

/* Find and return the indices of WINDOW's file.  The indices are defined
   as the first node in the file containing the word "Index" and any
   immediately following nodes whose names also contain "Index".  All such
   indices are concatenated and the result returned.  If WINDOW's info file
   doesn't have any indices, a NULL pointer is returned. */
REFERENCE **
info_indices_of_window (window)
     WINDOW *window;
{
  FILE_BUFFER *fb;

  fb = file_buffer_of_window (window);

  return (info_indices_of_file_buffer (fb));
}

REFERENCE **
info_indices_of_file_buffer (file_buffer)
     FILE_BUFFER *file_buffer;
{
  register int i;
  REFERENCE **result = (REFERENCE **)NULL;

  /* No file buffer, no indices. */
  if (!file_buffer)
    return ((REFERENCE **)NULL);

  /* Reset globals describing where the index was found. */
  maybe_free (initial_index_filename);
  maybe_free (initial_index_nodename);
  initial_index_filename = (char *)NULL;
  initial_index_nodename = (char *)NULL;

  if (index_nodenames)
    {
      for (i = 0; index_nodenames[i]; i++)
	{
	  free (index_nodenames[i]->name);
	  free (index_nodenames[i]);
	}

      index_nodenames_index = 0;
      index_nodenames[0] = (INDEX_NAME_ASSOC *)NULL;
    }

  /* Grovel the names of the nodes found in this file. */
  if (file_buffer->tags)
    {
      TAG *tag;

      for (i = 0; tag = file_buffer->tags[i]; i++)
	{
	  if (string_in_line ("Index", tag->nodename) != -1)
	    {
	      NODE *node;
	      REFERENCE **menu;

	      /* Found one.  Get its menu. */
	      node = info_get_node (tag->filename, tag->nodename);
	      if (!node)
		continue;

	      /* Remember the filename and nodename of this index. */
	      initial_index_filename = strdup (file_buffer->filename);
	      initial_index_nodename = strdup (tag->nodename);

	      menu = info_menu_of_node (node);

	      /* If we have a menu, add this index's nodename and range
		 to our list of index_nodenames. */
	      if (menu)
		{
		  add_index_to_index_nodenames (menu, node);

		  /* Concatenate the references found so far. */
		  result = info_concatenate_references (result, menu);
		}
	      free (node);
	    }
	}
    }

  /* If there is a result, clean it up so that every entry has a filename. */
  for (i = 0; result && result[i]; i++)
    if (!result[i]->filename)
      result[i]->filename = strdup (file_buffer->filename);

  return (result);
}

DECLARE_INFO_COMMAND (info_index_search,
   "Look up a string in the index for this file")
{
  FILE_BUFFER *fb;
  char *line;

  /* Reset the index offset, since this is not the info-index-next command. */
  index_offset = 0;

  /* The user is selecting a new search string, so flush the old one. */
  maybe_free (index_search);
  index_search = (char *)NULL;

  /* If this window's file is not the same as the one that we last built an
     index for, build and remember an index now. */
  fb = file_buffer_of_window (window);
  if (!initial_index_filename ||
      (strcmp (initial_index_filename, fb->filename) != 0))
    {
      info_free_references (index_index);
      window_message_in_echo_area ("Finding index entries...");
      index_index = info_indices_of_file_buffer (fb);
    }

  /* If there is no index, quit now. */
  if (!index_index)
    {
      info_error ("No indices found.");
      return;
    }

  /* Okay, there is an index.  Let the user select one of the members of it. */
  line =
    info_read_maybe_completing (window, "Index entry: ", index_index);

  window = active_window;

  /* User aborted? */
  if (!line)
    {
      info_abort_key (active_window, 1, 0);
      return;
    }

  /* Empty line means move to the Index node. */
  if (!*line)
    {
      free (line);

      if (initial_index_filename && initial_index_nodename)
	{
	  NODE *node;

	  node =
	    info_get_node (initial_index_filename, initial_index_nodename);
	  set_remembered_pagetop_and_point (window);
	  window_set_node_of_window (window, node);
	  remember_window_and_node (window, node);
	  window_clear_echo_area ();
	  return;
	}
    }

  /* The user typed either a completed index label, or a partial string.
     Find an exact match, or, failing that, the first index entry containing
     the partial string.  So, we just call info_next_index_match () with minor
     manipulation of INDEX_OFFSET. */
  {
    int old_offset;

    /* Start the search right after/before this index. */
    if (count < 0)
      {
	register int i;
	for (i = 0; index_index[i]; i++);
	index_offset = i;
      }
    else
      index_offset = -1;

    old_offset = index_offset;

    /* The "last" string searched for is this one. */
    index_search = line;

    /* Find it, or error. */
    info_next_index_match (window, count, 0);

    /* If the search failed, return the index offset to where it belongs. */
    if (index_offset == old_offset)
      index_offset = 0;
  }
}

DECLARE_INFO_COMMAND (info_next_index_match,
 "Go to the next matching index item from the last `\\[index-search]' command")
{
  register int i;
  int partial, dir;
  NODE *node;

  /* If there is no previous search string, the user hasn't built an index
     yet. */
  if (!index_search)
    {
      info_error ("No previous index search string.");
      return;
    }

  /* If there is no index, that is an error. */
  if (!index_index)
    {
      info_error ("No index entries.");
      return;
    }

  /* The direction of this search is controlled by the value of the
     numeric argument. */
  if (count < 0)
    dir = -1;
  else
    dir = 1;

  /* Search for the next occurence of index_search.  First try to find
     an exact match. */
  partial = 0;

  for (i = index_offset + dir; (i > -1) && (index_index[i]); i += dir)
    if (strcmp (index_search, index_index[i]->label) == 0)
      break;

  /* If that failed, look for the next substring match. */
  if ((i < 0) || (!index_index[i]))
    {
      for (i = index_offset + dir; (i > -1) && (index_index[i]); i += dir)
	if (string_in_line (index_search, index_index[i]->label) != -1)
	  break;

      if ((i > -1) && (index_index[i]))
	partial = string_in_line (index_search, index_index[i]->label);
    }

  /* If that failed, print an error. */
  if ((i < 0) || (!index_index[i]))
    {
      info_error ("No %sindex entries containing \"%s\".",
		  index_offset > 0 ? "more " : "", index_search);
      return;
    }

  /* Okay, we found the next one.  Move the offset to the current entry. */
  index_offset = i;

  /* Report to the user on what we have found. */
  {
    register int j;
    char *name = "CAN'T SEE THIS";
    char *match;

    for (j = 0; index_nodenames[j]; j++)
      {
	if ((i >= index_nodenames[j]->first) &&
	    (i <= index_nodenames[j]->last))
	  {
	    name = index_nodenames[j]->name;
	    break;
	  }
      }

    /* If we had a partial match, indicate to the user which part of the
       string matched. */
    match = strdup (index_index[i]->label);

    if (partial && show_index_match)
      {
	int j, ls, start, upper;

	ls = strlen (index_search);
	start = partial - ls;
	upper = isupper (match[start]) ? 1 : 0;

	for (j = 0; j < ls; j++)
	  if (upper)
	    match[j + start] = info_tolower (match[j + start]);
	  else
	    match[j + start] = info_toupper (match[j + start]);
      }

    {
      char *format;

      format = replace_in_documentation
	("Found \"%s\" in %s. (`\\[next-index-match]' tries to find next.)");

      window_message_in_echo_area (format, match, name);
    }

    free (match);
  }

  /* Select the node corresponding to this index entry. */
  node = info_get_node (index_index[i]->filename, index_index[i]->nodename);

  if (!node)
    {
      info_error (CANT_FILE_NODE,
		  index_index[i]->filename, index_index[i]->nodename);
      return;
    }

  set_remembered_pagetop_and_point (window);
  window_set_node_of_window (window, node);
  remember_window_and_node (window, node);


  /* Try to find an occurence of LABEL in this node. */
  {
    long start, loc;

    start = window->line_starts[1] - window->node->contents;
    loc = info_target_search_node (node, index_index[i]->label, start);

    if (loc != -1)
      {
	window->point = loc;
	window_adjust_pagetop (window);
      }
  }
}

/* **************************************************************** */
/*								    */
/*		   Info APROPOS: Search every known index.	    */
/*								    */
/* **************************************************************** */

/* For every menu item in DIR, search the indices of that file for
   SEARCH_STRING. */
REFERENCE **
apropos_in_all_indices (search_string, inform)
     char *search_string;
     int inform;
{
  register int i, dir_index;
  REFERENCE **all_indices = (REFERENCE **)NULL;
  REFERENCE **dir_menu = (REFERENCE **)NULL;
  NODE *dir_node;
  int printed = 0;

  dir_node = info_get_node ("dir", "Top");
  if (dir_node)
    dir_menu = info_menu_of_node (dir_node);

  if (!dir_menu)
    return;

  /* For every menu item in DIR, get the associated node's file buffer and
     read the indices of that file buffer.  Gather all of the indices into
     one large one. */
  for (dir_index = 0; dir_menu[dir_index]; dir_index++)
    {
      REFERENCE **this_index, *this_item;
      NODE *this_node;
      FILE_BUFFER *this_fb;

      this_item = dir_menu[dir_index];

      if (!this_item->filename)
	{
	  if (dir_node->parent)
	    this_item->filename = strdup (dir_node->parent);
	  else
	    this_item->filename = strdup (dir_node->filename);
	}

      /* Find this node.  If we cannot find it, try using the label of the
	 entry as a file (i.e., "(LABEL)Top"). */
      this_node = info_get_node (this_item->filename, this_item->nodename);

      if (!this_node && this_item->nodename &&
	  (strcmp (this_item->label, this_item->nodename) == 0))
	this_node = info_get_node (this_item->label, "Top");

      if (!this_node)
	continue;

      /* Get the file buffer associated with this node. */
      {
	char *files_name;

	files_name = this_node->parent;
	if (!files_name)
	  files_name = this_node->filename;

	this_fb = info_find_file (files_name);

	if (this_fb && inform)
	  message_in_echo_area ("Scanning indices of \"%s\"...", files_name);

	this_index = info_indices_of_file_buffer (this_fb);
	free (this_node);

	if (this_fb && inform)
	  unmessage_in_echo_area ();
      }

      if (this_index)
	{
	  /* Remember the filename which contains this set of references. */
	  for (i = 0; this_index && this_index[i]; i++)
	    if (!this_index[i]->filename)
	      this_index[i]->filename = strdup (this_fb->filename);

	  /* Concatenate with the other indices.  */
	  all_indices = info_concatenate_references (all_indices, this_index);
	}
    }

  info_free_references (dir_menu);

  /* Build a list of the references which contain SEARCH_STRING. */
  if (all_indices)
    {
      REFERENCE *entry, **apropos_list = (REFERENCE **)NULL;
      int apropos_list_index = 0;
      int apropos_list_slots = 0;

      for (i = 0; (entry = all_indices[i]); i++)
	{
	  if (string_in_line (search_string, entry->label) != -1)
	    {
	      add_pointer_to_array
		(entry, apropos_list_index, apropos_list, apropos_list_slots,
		 100, REFERENCE *);
	    }
	  else
	    {
	      maybe_free (entry->label);
	      maybe_free (entry->filename);
	      maybe_free (entry->nodename);
	      free (entry);
	    }
	}

      free (all_indices);
      all_indices = apropos_list;
    }
  return (all_indices);
}

#define APROPOS_NONE \
   "No available info files reference \"%s\" in their indices."

void
info_apropos (string)
     char *string;
{
  REFERENCE **apropos_list;

  apropos_list = apropos_in_all_indices (string, 0);

  if (!apropos_list)
    {
      info_error (APROPOS_NONE, string);
    }
  else
    {
      register int i;
      REFERENCE *entry;

      for (i = 0; (entry = apropos_list[i]); i++)
	fprintf (stderr, "\"(%s)%s\" -- %s\n",
		 entry->filename, entry->nodename, entry->label);
    }
  info_free_references (apropos_list);
}

static char *apropos_list_nodename = "*Apropos*";

DECLARE_INFO_COMMAND (info_index_apropos,
   "Grovel all known info file's indices for a string and build a menu")
{
  char *line;

  line = info_read_in_echo_area (window, "Index apropos: ");

  window = active_window;

  /* User aborted? */
  if (!line)
    {
      info_abort_key (window, 1, 1);
      return;
    }

  /* User typed something? */
  if (*line)
    {
      REFERENCE **apropos_list;
      NODE *apropos_node;

      apropos_list = apropos_in_all_indices (line, 1);

      if (!apropos_list)
	{
	  info_error (APROPOS_NONE, line);
	}
      else
	{
	  register int i;
	  char *line_buffer;

	  initialize_message_buffer ();
	  printf_to_message_buffer
	    ("\n* Menu: Nodes whoses indices contain \"%s\":\n", line);
	  line_buffer = (char *)xmalloc (500);

	  for (i = 0; apropos_list[i]; i++)
	    {
	      int len;
	      sprintf (line_buffer, "* (%s)%s::",
		       apropos_list[i]->filename, apropos_list[i]->nodename);
	      len = pad_to (36, line_buffer);
	      sprintf (line_buffer + len, "%s", apropos_list[i]->label);
	      printf_to_message_buffer ("%s\n", line_buffer);
	    }
	  free (line_buffer);
	}

      apropos_node = message_buffer_to_node ();
      add_gcable_pointer (apropos_node->contents);
      name_internal_node (apropos_node, apropos_list_nodename);

      /* Even though this is an internal node, we don't want the window
	 system to treat it specially.  So we turn off the internalness
	 of it here. */
      apropos_node->flags &= ~N_IsInternal;

      /* Find/Create a window to contain this node. */
      {
	WINDOW *new;
	NODE *node;

	set_remembered_pagetop_and_point (window);

	/* If a window is visible and showing an apropos list already,
	   re-use it. */
	for (new = windows; new; new = new->next)
	  {
	    node = new->node;

	    if (internal_info_node_p (node) &&
		(strcmp (node->nodename, apropos_list_nodename) == 0))
	      break;
	  }

	/* If we couldn't find an existing window, try to use the next window
	   in the chain. */
	if (!new && window->next)
	  new = window->next;

	/* If we still don't have a window, make a new one to contain
	   the list. */
	if (!new)
	  {
	    WINDOW *old_active;

	    old_active = active_window;
	    active_window = window;
	    new = window_make_window ((NODE *)NULL);
	    active_window = old_active;
	  }

	/* If we couldn't make a new window, use this one. */
	if (!new)
	  new = window;

	/* Lines do not wrap in this window. */
	new->flags |= W_NoWrap;

	window_set_node_of_window (new, apropos_node);
	remember_window_and_node (new, apropos_node);
	active_window = new;
      }
      info_free_references (apropos_list);
    }
  free (line);

  if (!info_error_was_printed)
    window_clear_echo_area ();
}

