/* variables.c -- How to manipulate user visible variables in Info. */

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
#include "variables.h"

/* **************************************************************** */
/*								    */
/*		    User Visible Variables in Info		    */
/*								    */
/* **************************************************************** */

/* Choices used by the completer when reading a zero/non-zero value for
   a variable. */
static char *on_off_choices[] = { "Off", "On", (char *)NULL };

VARIABLE_ALIST info_variables[] = {
  { "automatic-footnotes",
      "When \"On\", footnotes appear and disappear automatically",
      &auto_footnotes_p, (char **)on_off_choices },

  { "automatic-tiling",
      "When \"On\", creating or deleting a window resizes other windows",
      &auto_tiling_p, (char **)on_off_choices },

  { "visible-bell",
      "When \"On\", flash the screen instead of ringing the bell",
      &terminal_use_visible_bell_p, (char **)on_off_choices },

  { "errors-ring-bell",
      "When \"On\", errors cause the bell to ring",
      &info_error_rings_bell_p, (char **)on_off_choices },

  { "gc-compressed-files",
      "When \"On\", Info garbage collects files which had to be uncompressed",
      &gc_compressed_files, (char **)on_off_choices },
  { "show-index-match",
      "When \"On\", the portion of the matched search string is highlighted",
      &show_index_match, (char **)on_off_choices },

  { "scroll-behaviour",
      "Controls what happens when scrolling is requested at the end of a node",
      &info_scroll_behaviour, (char **)info_scroll_choices },

  { "scroll-step",
      "The number lines to scroll when the cursor moves out of the window",
      &window_scroll_step, (char **)NULL },

  { "ISO-Latin",
      "When \"On\", Info accepts and displays ISO Latin characters",
      &ISO_Latin_p, (char **)on_off_choices },

  { (char *)NULL, (char *)NULL, (int *)NULL, (char **)NULL }
};

DECLARE_INFO_COMMAND (describe_variable, "Explain the use of a variable")
{
  VARIABLE_ALIST *var;
  char *description;

  /* Get the variable's name. */
  var = read_variable_name ("Describe variable: ", window);

  if (!var)
    return;

  description = (char *)xmalloc (20 + strlen (var->name) + strlen (var->doc));

  if (var->choices)
    sprintf (description, "%s (%s): %s.",
	     var->name, var->choices[*(var->value)], var->doc);
  else
    sprintf (description, "%s (%d): %s.", var->name, *(var->value), var->doc);

  window_message_in_echo_area ("%s", description);
  free (description);
}

DECLARE_INFO_COMMAND (set_variable, "Set the value of an Info variable")
{
  VARIABLE_ALIST *var;
  char *line;

  /* Get the variable's name and value. */
  var = read_variable_name ("Set variable: ", window);

  if (!var)
    return;

  /* Read a new value for this variable. */
  {
    char prompt[100];

    if (!var->choices)
      {
	int potential_value;

	if (info_explicit_arg || count != 1)
	  potential_value = count;
	else
	  potential_value = *(var->value);

	sprintf (prompt, "Set %s to value (%d): ",
		 var->name, potential_value);
	line = info_read_in_echo_area (active_window, prompt);

	/* If no error was printed, clear the echo area. */
	if (!info_error_was_printed)
	  window_clear_echo_area ();

	/* User aborted? */
	if (!line)
	  return;

	/* If the user specified a value, get that, otherwise, we are done. */
	canonicalize_whitespace (line);
	if (*line)
	  *(var->value) = atoi (line);
	else
	  *(var->value) = potential_value;

	free (line);
      }
    else
      {
	register int i;
	REFERENCE **array = (REFERENCE **)NULL;
	int array_index = 0;
	int array_slots = 0;

	for (i = 0; var->choices[i]; i++)
	  {
	    REFERENCE *entry;

	    entry = (REFERENCE *)xmalloc (sizeof (REFERENCE));
	    entry->label = strdup (var->choices[i]);
	    entry->nodename = (char *)NULL;
	    entry->filename = (char *)NULL;

	    add_pointer_to_array
	      (entry, array_index, array, array_slots, 10, REFERENCE *);
	  }

	sprintf (prompt, "Set %s to value (%s): ",
		 var->name, var->choices[*(var->value)]);

	/* Ask the completer to read a variable value for us. */
	line = info_read_completing_in_echo_area (window, prompt, array);

	info_free_references (array);

	if (!echo_area_is_active)
	  window_clear_echo_area ();

	/* User aborted? */
	if (!line)
	  {
	    info_abort_key (active_window, 0, 0);
	    return;
	  }

	/* User accepted default choice?  If so, no change. */
	if (!*line)
	  {
	    free (line);
	    return;
	  }

	/* Find the choice in our list of choices. */
	for (i = 0; var->choices[i]; i++)
	  if (strcmp (var->choices[i], line) == 0)
	    break;

	if (var->choices[i])
	  *(var->value) = i;
      }
  }
}

/* Read the name of an Info variable in the echo area and return the
   address of a VARIABLE_ALIST member.  A return value of NULL indicates
   that no variable could be read. */
VARIABLE_ALIST *
read_variable_name (prompt, window)
     char *prompt;
     WINDOW *window;
{
  register int i;
  char *line;
  REFERENCE **variables;

  /* Get the completion array of variable names. */
  variables = make_variable_completions_array ();

  /* Ask the completer to read a variable for us. */
  line =
    info_read_completing_in_echo_area (window, prompt, variables);

  info_free_references (variables);

  if (!echo_area_is_active)
    window_clear_echo_area ();

  /* User aborted? */
  if (!line)
    {
      info_abort_key (active_window, 0, 0);
      return ((VARIABLE_ALIST *)NULL);
    }

  /* User accepted "default"?  (There is none.) */
  if (!*line)
    {
      free (line);
      return ((VARIABLE_ALIST *)NULL);
    }

  /* Find the variable in our list of variables. */
  for (i = 0; info_variables[i].name; i++)
    if (strcmp (info_variables[i].name, line) == 0)
      break;

  if (!info_variables[i].name)
    return ((VARIABLE_ALIST *)NULL);
  else
    return (&(info_variables[i]));
}

/* Make an array of REFERENCE which actually contains the names of the
   variables available in Info. */
REFERENCE **
make_variable_completions_array ()
{
  register int i;
  REFERENCE **array = (REFERENCE **)NULL;
  int array_index = 0, array_slots = 0;

  for (i = 0; info_variables[i].name; i++)
    {
      REFERENCE *entry;

      entry = (REFERENCE *)xmalloc (sizeof (REFERENCE));
      entry->label = strdup (info_variables[i].name);
      entry->nodename = (char *)NULL;
      entry->filename = (char *)NULL;

      add_pointer_to_array
	(entry, array_index, array, array_slots, 200, REFERENCE *);
    }

  return (array);
}
