/* info.h -- Header file which includes all of the other headers. */

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

#if !defined (_INFO_H_)
#define _INFO_H_

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#if defined (HAVE_STRING_H)
#include <string.h>
#endif /* HAVE_STRING_H */
#include "filesys.h"
#include "display.h"
#include "session.h"
#include "echo_area.h"
#include "doc.h"
#include "footnotes.h"
#include "gc.h"

/* A structure associating the nodes visited in a particular window. */
typedef struct {
  WINDOW *window;		/* The window that this list is attached to. */
  NODE **nodes;			/* Array of nodes visited in this window. */
  int *pagetops;		/* For each node in NODES, the pagetop. */
  long *points;			/* For each node in NODES, the point. */
  int current;			/* Index in NODES of the current node. */
  int nodes_index;		/* Index where to add the next node. */
  int nodes_slots;		/* Number of slots allocated to NODES. */
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
extern int info_major_version, info_minor_version, info_patch_level;

/* How to get the version string for this version of Info.  Returns
   something similar to "2.11". */
extern char *version_string ();

/* Error message defines. */
#define CANT_FIND_NODE	"Cannot find the node \"%s\"."
#define CANT_FILE_NODE	"Cannot find the node \"(%s)%s\"."
#define CANT_FIND_WIND	"Cannot find a window!"
#define CANT_FIND_POINT	"Point doesn't appear within this window's node!"
#define CANT_KILL_LAST	"Cannot delete the last window."
#define NO_MENU_NODE	"No menu in this node."
#define NO_FOOT_NODE	"No footnotes in this node."
#define NO_XREF_NODE	"No cross references in this node."
#define NO_POINTER	"No \"%s\" pointer for this node."
#define UNKNOWN_COMMAND	"Unknown Info command `%c'.  `?' for help."
#define TERM_TOO_DUMB	"Terminal type \"%s\" is not smart enough to run Info."
#define AT_NODE_BOTTOM	"You are already at the last page of this node."
#define AT_NODE_TOP	"You are already at the first page of this node."
#define ONE_WINDOW	"Only one window."
#define WIN_TOO_SMALL	"Resulting window would be too small."
#define CANT_MAKE_HELP	\
"There isn't enough room to make a help window.  Please delete a window."

#endif /* !_INFO_H_ */

