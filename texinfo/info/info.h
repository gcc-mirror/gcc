/* info.h -- Header file which includes all of the other headers.
   $Id: info.h,v 1.1.1.3 1998/03/24 18:20:14 law Exp $

   This file is part of GNU Info, a program for reading online documentation
   stored in Info format.

   Copyright (C) 1993, 97, 98 Free Software Foundation, Inc.

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

#if !defined (INFO_H)
#define INFO_H

/* We always want these, so why clutter up the compile command?  */
#define HANDLE_MAN_PAGES
#define NAMED_FUNCTIONS

/* System dependencies.  */
#include "system.h"

/* Some of our other include files use these.  */
typedef int Function ();
typedef void VFunction ();
typedef char *CFunction ();


#include "filesys.h"
#include "display.h"
#include "session.h"
#include "echo-area.h"
#include "doc.h"
#include "footnotes.h"
#include "gc.h"

#define info_toupper(x) (islower (x) ? toupper (x) : x)
#define info_tolower(x) (isupper (x) ? tolower (x) : x)

#if !defined (whitespace)
#  define whitespace(c) ((c == ' ') || (c == '\t'))
#endif /* !whitespace */

#if !defined (whitespace_or_newline)
#  define whitespace_or_newline(c) (whitespace (c) || (c == '\n'))
#endif /* !whitespace_or_newline */

/* Add POINTER to the list of pointers found in ARRAY.  SLOTS is the number
   of slots that have already been allocated.  INDEX is the index into the
   array where POINTER should be added.  GROW is the number of slots to grow
   ARRAY by, in the case that it needs growing.  TYPE is a cast of the type
   of object stored in ARRAY (e.g., NODE_ENTRY *. */
#define add_pointer_to_array(pointer, idx, array, slots, grow, type) \
  do { \
    if (idx + 2 >= slots) \
      array = (type *)(xrealloc (array, (slots += grow) * sizeof (type))); \
    array[idx++] = (type)pointer; \
    array[idx] = (type)NULL; \
  } while (0)

#define maybe_free(x) do { if (x) free (x); } while (0)

#if !defined (zero_mem) && defined (HAVE_MEMSET)
#  define zero_mem(mem, length) memset (mem, 0, length)
#endif /* !zero_mem && HAVE_MEMSET */

#if !defined (zero_mem) && defined (HAVE_BZERO)
#  define zero_mem(mem, length) bzero (mem, length)
#endif /* !zero_mem && HAVE_BZERO */

#if !defined (zero_mem)
#  define zero_mem(mem, length) \
  do {                                  \
        register int zi;                \
        register unsigned char *place;  \
                                        \
        place = (unsigned char *)mem;   \
        for (zi = 0; zi < length; zi++) \
          place[zi] = 0;                \
      } while (0)
#endif /* !zero_mem */


/* A structure associating the nodes visited in a particular window. */
typedef struct {
  WINDOW *window;               /* The window that this list is attached to. */
  NODE **nodes;                 /* Array of nodes visited in this window. */
  int *pagetops;                /* For each node in NODES, the pagetop. */
  long *points;                 /* For each node in NODES, the point. */
  int current;                  /* Index in NODES of the current node. */
  int nodes_index;              /* Index where to add the next node. */
  int nodes_slots;              /* Number of slots allocated to NODES. */
} INFO_WINDOW;

/* Array of structures describing for each window which nodes have been
   visited in that window. */
extern INFO_WINDOW **info_windows;

/* For handling errors.  If you initialize the window system, you should
   also set info_windows_initialized_p to non-zero.  It is used by the
   info_error () function to determine how to format and output errors. */
extern int info_windows_initialized_p;

/* Non-zero if an error message has been printed. */
extern int info_error_was_printed;

/* Non-zero means ring terminal bell on errors. */
extern int info_error_rings_bell_p;

/* Print FORMAT with ARG1 and ARG2.  If the window system was initialized,
   then the message is printed in the echo area.  Otherwise, a message is
   output to stderr. */
extern void info_error ();

/* The version numbers of Info. */
extern int info_major_version, info_minor_version;

/* How to get the version string for this version of Info.  Returns
   something similar to "2.11". */
extern char *version_string ();

/* Error message defines. */
#define CANT_FIND_NODE  _("Cannot find the node \"%s\".")
#define CANT_FILE_NODE  _("Cannot find the node \"(%s)%s\".")
#define CANT_FIND_WIND  _("Cannot find a window!")
#define CANT_FIND_POINT _("Point doesn't appear within this window's node!")
#define CANT_KILL_LAST  _("Cannot delete the last window.")
#define NO_MENU_NODE    _("No menu in this node.")
#define NO_FOOT_NODE    _("No footnotes in this node.")
#define NO_XREF_NODE    _("No cross references in this node.")
#define NO_POINTER      _("No \"%s\" pointer for this node.")
#define UNKNOWN_COMMAND _("Unknown Info command `%c'.  `?' for help.")
#define TERM_TOO_DUMB   _("Terminal type \"%s\" is not smart enough to run Info.")
#define AT_NODE_BOTTOM  _("You are already at the last page of this node.")
#define AT_NODE_TOP     _("You are already at the first page of this node.")
#define ONE_WINDOW      _("Only one window.")
#define WIN_TOO_SMALL   _("Resulting window would be too small.")
#define CANT_MAKE_HELP  \
_("There isn't enough room to make a help window.  Please delete a window.")


/* Found in info-utils.c. */
extern char *filename_non_directory ();

#if !defined (BUILDING_LIBRARY)
/* Found in session.c */
extern int info_windows_initialized_p;

/* Found in window.c. */
extern void message_in_echo_area (), unmessage_in_echo_area ();
#endif /* !BUILDING_LIBRARY */

#endif /* !INFO_H */
