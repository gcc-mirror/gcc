/* display.h -- How the display in Info is done.
   $Id: display.h,v 1.2 1997/07/15 18:37:29 karl Exp $

   This file is part of GNU Info, a program for reading online documentation
   stored in Info format.

   Copyright (C) 1993, 97 Free Software Foundation, Inc.

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

#ifndef INFO_DISPLAY_H
#define INFO_DISPLAY_H

#include "info-utils.h"
#include "terminal.h"

typedef struct {
  char *text;			/* Text of the line as it appears. */
  int textlen;			/* Printable Length of TEXT. */
  int inverse;			/* Non-zero means this line is inverse. */
} DISPLAY_LINE;

/* An array of display lines which tell us what is currently visible on
   the display.  */
extern DISPLAY_LINE **the_display;

/* Non-zero means do no output. */
extern int display_inhibited;

/* Non-zero if we didn't completely redisplay a window. */
extern int display_was_interrupted_p;

/* Initialize THE_DISPLAY to WIDTH and HEIGHT, with nothing in it. */
extern void display_initialize_display ();

/* Clear all of the lines in DISPLAY making the screen blank. */
extern void display_clear_display ();

/* Update the windows pointed to by WINDOWS in THE_DISPLAY.  This actually
   writes the text on the screen. */
extern void display_update_display ();

/* Display WIN on THE_DISPLAY.  Unlike display_update_display (), this
   function only does one window. */
extern void display_update_one_window ();

/* Move the screen cursor to directly over the current character in WINDOW. */
extern void display_cursor_at_point ();

/* Scroll the region of the_display starting at START, ending at END, and
   moving the lines AMOUNT lines.  If AMOUNT is less than zero, the lines
   are moved up in the screen, otherwise down.  Actually, it is possible
   for no scrolling to take place in the case that the terminal doesn't
   support it.  This doesn't matter to us. */
extern void display_scroll_display ();

/* Try to scroll lines in WINDOW.  OLD_PAGETOP is the pagetop of WINDOW before
   having had its line starts recalculated.  OLD_STARTS is the list of line
   starts that used to appear in this window.  OLD_COUNT is the number of lines
   that appear in the OLD_STARTS array. */
extern void display_scroll_line_starts ();

#endif /* not INFO_DISPLAY_H */
