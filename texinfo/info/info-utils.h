/* info-utils.h -- Exported functions and variables from info-util.c.
   $Id: info-utils.h,v 1.1.1.2 1998/03/22 20:42:36 law Exp $   

   This file is part of GNU Info, a program for reading online documentation
   stored in Info format.

   Copyright (C) 1993, 96 Free Software Foundation, Inc.

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

#ifndef INFO_UTILS_H
#define INFO_UTILS_H

#if !defined (HAVE_STRCHR)
#  undef strchr
#  undef strrchr
#  define strchr index
#  define strrchr rindex
#endif /* !HAVE_STRCHR */

#include "nodes.h"
#include "window.h"
#include "search.h"

/* Structure which describes a node reference, such as a menu entry or
   cross reference.  Arrays of such references can be built by calling
   info_menus_of_node () or info_xrefs_of_node (). */
typedef struct {
  char *label;          /* User Label. */
  char *filename;       /* File where this node can be found. */
  char *nodename;       /* Name of the node. */
  int start, end;       /* Offsets within the containing node of LABEL. */
} REFERENCE;

/* When non-zero, various display and input functions handle ISO Latin
   character sets correctly. */
extern int ISO_Latin_p;

/* Variable which holds the most recent filename parsed as a result of
   calling info_parse_xxx (). */
extern char *info_parsed_filename;

/* Variable which holds the most recent nodename parsed as a result of
   calling info_parse_xxx (). */
extern char *info_parsed_nodename;

/* Parse the filename and nodename out of STRING.  If STRING doesn't
   contain a filename (i.e., it is NOT (FILENAME)NODENAME) then set
   INFO_PARSED_FILENAME to NULL.  If second argument NEWLINES_OKAY is
   non-zero, it says to allow the nodename specification to cross a
   newline boundary (i.e., only `,', `.', or `TAB' can end the spec). */
void info_parse_node ();

/* Return a NULL terminated array of REFERENCE * which represents the menu
   found in NODE.  If there is no menu in NODE, just return a NULL pointer. */
extern REFERENCE **info_menu_of_node ();

/* Return a NULL terminated array of REFERENCE * which represents the cross
   refrences found in NODE.  If there are no cross references in NODE, just
   return a NULL pointer. */
extern REFERENCE **info_xrefs_of_node ();

/* Glean cross references from BINDING->buffer + BINDING->start until
   BINDING->end.  Return an array of REFERENCE * that represents each
   cross reference in this range. */
extern REFERENCE **info_xrefs ();

/* Get the entry associated with LABEL in REFERENCES.  Return a pointer to
   the reference if found, or NULL. */
extern REFERENCE *info_get_labeled_reference ();

/* Glean menu entries from BINDING->buffer + BINDING->start until we
   have looked at the entire contents of BINDING.  Return an array
   of REFERENCE * that represents each menu item in this range. */
extern REFERENCE **info_menu_items ();

/* A utility function for concatenating REFERENCE **.  Returns a new
   REFERENCE ** which is the concatenation of REF1 and REF2.  The REF1
   and REF2 arrays are freed, but their contents are not. */
REFERENCE **info_concatenate_references ();

/* Free the data associated with REFERENCES. */
extern void info_free_references ();

/* Search for sequences of whitespace or newlines in STRING, replacing
   all such sequences with just a single space.  Remove whitespace from
   start and end of string. */
void canonicalize_whitespace ();

/* Return a pointer to a string which is the printed representation
   of CHARACTER if it were printed at HPOS. */
extern char *printed_representation ();

/* Return a pointer to the part of PATHNAME that simply defines the file. */
extern char *filename_non_directory ();

/* Return non-zero if NODE is one especially created by Info. */
extern int internal_info_node_p ();

/* Make NODE appear to be one especially created by Info, and give it NAME. */
extern void name_internal_node ();

/* Return the window displaying NAME, the name of an internally created
   Info window. */
extern WINDOW *get_internal_info_window ();

/* Return the node addressed by LABEL in NODE (usually one of "Prev:",
   "Next:", "Up:", "File:", or "Node:".  After a call to this function,
   the global INFO_PARSED_NODENAME and INFO_PARSED_FILENAME contain
   the information. */
extern void info_parse_label (/* label, node */);

#define info_label_was_found \
   (info_parsed_nodename != NULL || info_parsed_filename != NULL)

#define info_file_label_of_node(n) info_parse_label (INFO_FILE_LABEL, n)
#define info_next_label_of_node(n) info_parse_label (INFO_NEXT_LABEL, n)
#define info_up_label_of_node(n)   info_parse_label (INFO_UP_LABEL, n)
#define info_prev_label_of_node(n) \
  do { \
    info_parse_label (INFO_PREV_LABEL, n); \
    if (!info_label_was_found) \
      info_parse_label (INFO_ALTPREV_LABEL, n); \
  } while (0)

#endif /* not INFO_UTILS_H */
