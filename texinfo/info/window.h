/* window.h -- Structure and flags used in manipulating Info windows. */

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

#if !defined (_WINDOW_H_)
#define _WINDOW_H_

#include "nodes.h"
#include "infomap.h"

/* Smallest number of visible lines in a window.  The actual height is
   always one more than this number because each window has a modeline. */
#define WINDOW_MIN_HEIGHT 2

/* Smallest number of screen lines that can be used to fully present a
   window.  This number includes the modeline of the window. */
#define WINDOW_MIN_SIZE (WINDOW_MIN_HEIGHT + 1)

/* The exact same elements are used within the WINDOW_STATE structure and a
   subsection of the WINDOW structure.  We could define a structure which
   contains this elements, and include that structure in each of WINDOW_STATE
   and WINDOW.  But that would lead references in the code such as
   window->state->node which we would like to avoid.  Instead, we #define the
   elements here, and simply include the define in both data structures. Thus,
   if you need to change window state information, here is where you would
   do it.  NB> The last element does NOT end with a semi-colon. */
#define WINDOW_STATE_DECL \
   NODE *node;		/* The node displayed in this window. */ \
   int pagetop;		/* LINE_STARTS[PAGETOP] is first line in WINDOW. */ \
   long point		/* Offset within NODE of the cursor position. */

/* Structure which defines a window.  Windows are doubly linked, next
   and prev. The list of windows is kept on WINDOWS.  The structure member
   window->height is the total height of the window.  The position location
   (0, window->height + window->first_row) is the first character of this
   windows modeline.  The number of lines that can be displayed in a window
   is equal to window->height - 1. */
typedef struct __window__ {
  struct __window__ *next;	/* Next window in this chain. */
  struct __window__ *prev;	/* Previous window in this chain. */
  int width;		/* Width of this window. */
  int height;		/* Height of this window. */
  int first_row;	/* Offset of the first line in the_screen. */
  int goal_column;	/* The column we would like the cursor to appear in. */
  Keymap keymap;	/* Keymap used to read commands in this window. */
  WINDOW_STATE_DECL;	/* Node, pagetop and point. */
  char *modeline;	/* Calculated text of the modeline for this window. */
  char **line_starts;	/* Array of printed line starts for this node. */
  int line_count;	/* Number of lines appearing in LINE_STARTS. */
  int flags;		/* See below for details. */
} WINDOW;

typedef struct {
  WINDOW_STATE_DECL;		/* What gets saved. */
} WINDOW_STATE;

#define W_UpdateWindow	0x01	/* WINDOW needs updating. */
#define W_WindowIsPerm	0x02	/* This WINDOW is a permanent object. */
#define W_WindowVisible	0x04	/* This WINDOW is currently visible. */
#define W_InhibitMode	0x08	/* This WINDOW has no modeline. */
#define W_NoWrap	0x10	/* Lines do not wrap in this window. */
#define W_InputWindow	0x20	/* Window accepts input. */
#define W_TempWindow	0x40	/* Window is less important. */

extern WINDOW *windows;		/* List of visible Info windows. */
extern WINDOW *active_window;	/* The currently active window. */
extern WINDOW *the_screen;	/* The Info screen is just another window. */
extern WINDOW *the_echo_area;	/* THE_ECHO_AREA is a window in THE_SCREEN. */

/* Global variable control redisplay of scrolled windows.  If non-zero, it
   is the desired number of lines to scroll the window in order to make
   point visible.  A user might set this to 1 for smooth scrolling.  If
   set to zero, the line containing point is centered within the window. */
extern int window_scroll_step;

 /* Make the modeline member for WINDOW. */
extern void window_make_modeline ();

/* Initalize the window system by creating THE_SCREEN and THE_ECHO_AREA.
   Create the first window ever, and make it permanent.
   You pass WIDTH and HEIGHT; the dimensions of the total screen size. */
extern void window_initialize_windows ();

/* Make a new window showing NODE, and return that window structure.
   The new window is made to be the active window.  If NODE is passed
   as NULL, then show the node showing in the active window.  If the
   window could not be made return a NULL pointer.  The active window
   is not changed.*/
extern WINDOW *window_make_window ();

/* Delete WINDOW from the list of known windows.  If this window was the
   active window, make the next window in the chain be the active window,
   or the previous window in the chain if there is no next window. */
extern void window_delete_window ();

/* A function to call when the screen changes size, and some windows have
   to get deleted.  The function is called with the window to be deleted
   as an argument, and it can't do anything about the window getting deleted;
   it can only clean up dangling references to that window. */
extern VFunction *window_deletion_notifier;

/* Set WINDOW to display NODE. */
extern void window_set_node_of_window ();

/* Tell the window system that the size of the screen has changed.  This
   causes lots of interesting things to happen.  The permanent windows
   are resized, as well as every visible window.  You pass WIDTH and HEIGHT;
   the dimensions of the total screen size. */
extern void window_new_screen_size ();

/* Change the height of WINDOW by AMOUNT.  This also automagically adjusts
   the previous and next windows in the chain.  If there is only one user
   window, then no change takes place. */
extern void window_change_window_height ();

/* Adjust the pagetop of WINDOW such that the cursor point will be visible. */
extern void window_adjust_pagetop ();

/* Tile all of the windows currently displayed in the global variable
   WINDOWS.  If argument DO_INTERNALS is non-zero, tile windows displaying
   internal nodes as well. */
#define DONT_TILE_INTERNALS 0
#define TILE_INTERNALS      1
extern void window_tile_windows ();

/* Toggle the state of line wrapping in WINDOW.  This can do a bit of fancy
   redisplay. */
extern void window_toggle_wrap ();

/* For every window in CHAIN, set the flags member to have FLAG set. */
extern void window_mark_chain ();

/* For every window in CHAIN, clear the flags member of FLAG. */
extern void window_unmark_chain ();

/* Make WINDOW start displaying at PERCENT percentage of its node. */
extern void window_goto_percentage ();

/* Build a new node which has FORMAT printed with ARG1 and ARG2 as the
   contents. */
extern NODE *build_message_node ();

/* Useful functions can be called from outside of window.c. */
extern void initialize_message_buffer ();

/* Print FORMAT with ARG1,2 to the end of the current message buffer. */
extern void printf_to_message_buffer ();

/* Convert the contents of the message buffer to a node. */
extern NODE *message_buffer_to_node ();

/* Return the length of the most recently printed line in message buffer. */
extern int message_buffer_length_this_line ();

/* Pad STRING to COUNT characters by inserting blanks. */
extern int pad_to ();

/* Make a message appear in the echo area, built from FORMAT, ARG1 and ARG2.
   The arguments are treated similar to printf () arguments, but not all of
   printf () hair is present.  The message appears immediately.  If there was
   already a message appearing in the echo area, it is removed. */
extern void window_message_in_echo_area ();

/* Place a temporary message in the echo area built from FORMAT, ARG1
   and ARG2.  The message appears immediately, but does not destroy
   any existing message.  A future call to unmessage_in_echo_area ()
   restores the old contents. */
extern void message_in_echo_area ();
extern void unmessage_in_echo_area ();

/* Clear the echo area, removing any message that is already present.
   The echo area is cleared immediately. */
extern void window_clear_echo_area ();

/* Quickly guess the approximate number of lines to that NODE would
   take to display.  This really only counts carriage returns. */
extern int window_physical_lines ();

/* Calculate a list of line starts for the node belonging to WINDOW.  The line
   starts are pointers to the actual text within WINDOW->NODE. */
extern void calculate_line_starts ();

/* Given WINDOW, recalculate the line starts for the node it displays. */
extern void recalculate_line_starts ();

/* Return the number of characters it takes to display CHARACTER on the
   screen at HPOS. */
extern int character_width ();

/* Return the number of characters it takes to display STRING on the
   screen at HPOS. */
extern int string_width ();

/* Return the index of the line containing point. */
extern int window_line_of_point ();

/* Get and return the goal column for this window. */
extern int window_get_goal_column ();

/* Get and return the printed column offset of the cursor in this window. */
extern int window_get_cursor_column ();

/* Get and Set the node, pagetop, and point of WINDOW. */
extern void window_get_state (), window_set_state ();

/* Count the number of characters in LINE that precede the printed column
   offset of GOAL. */
extern int window_chars_to_goal ();

#endif /* !_WINDOW_H_ */
