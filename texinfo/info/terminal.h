/* terminal.h -- The external interface to terminal I/O. */

/* This file is part of GNU Info, a program for reading online documentation
   stored in Info format.

   Copyright (C) 1993, 96, 97 Free Software Foundation, Inc.

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

#if !defined (TERMINAL_H)
#define TERMINAL_H

#include "info.h"

/* For almost every function externally visible from terminal.c, there is
   a corresponding "hook" function which can be bound in order to replace
   the functionality of the one found in terminal.c.  This is how we go
   about implemented X window display. */

/* The width and height of the terminal. */
extern int screenwidth, screenheight;

/* Non-zero means this terminal can't really do anything. */
extern int terminal_is_dumb_p;

/* Non-zero means that this terminal has a meta key. */
extern int terminal_has_meta_p;

/* Non-zero means that this terminal can produce a visible bell. */
extern int terminal_has_visible_bell_p;

/* Non-zero means to use that visible bell if at all possible. */
extern int terminal_use_visible_bell_p;

/* Non-zero means that this terminal can scroll lines up and down. */
extern int terminal_can_scroll;

/* Initialize the terminal which is known as TERMINAL_NAME.  If this terminal
   doesn't have cursor addressability, TERMINAL_IS_DUMB_P becomes non-zero.
   The variables SCREENHEIGHT and SCREENWIDTH are set to the dimensions that
   this terminal actually has.  The variable TERMINAL_HAS_META_P becomes non-
   zero if this terminal supports a Meta key. */
extern void terminal_initialize_terminal ();
extern VFunction *terminal_initialize_terminal_hook;

/* Return the current screen width and height in the variables
   SCREENWIDTH and SCREENHEIGHT. */
extern void terminal_get_screen_size ();
extern VFunction *terminal_get_screen_size_hook;

/* Save and restore tty settings. */
extern void terminal_prep_terminal (), terminal_unprep_terminal ();
extern VFunction *terminal_prep_terminal_hook, *terminal_unprep_terminal_hook;

/* Re-initialize the terminal to TERMINAL_NAME. */
extern void terminal_new_terminal ();
extern VFunction *terminal_new_terminal_hook;

/* Move the cursor to the terminal location of X and Y. */
extern void terminal_goto_xy ();
extern VFunction *terminal_goto_xy_hook;

/* Print STRING to the terminal at the current position. */
extern void terminal_put_text ();
extern VFunction *terminal_put_text_hook;

/* Print NCHARS from STRING to the terminal at the current position. */
extern void terminal_write_chars ();
extern VFunction *terminal_write_chars_hook;

/* Clear from the current position of the cursor to the end of the line. */
extern void terminal_clear_to_eol ();
extern VFunction *terminal_clear_to_eol_hook;

/* Clear the entire terminal screen. */
extern void terminal_clear_screen ();
extern VFunction *terminal_clear_screen_hook;

/* Move the cursor up one line. */
extern void terminal_up_line ();
extern VFunction *terminal_up_line_hook;

/* Move the cursor down one line. */
extern void terminal_down_line ();
extern VFunction *terminal_down_line_hook;

/* Turn on reverse video if possible. */
extern void terminal_begin_inverse ();
extern VFunction *terminal_begin_inverse_hook;

/* Turn off reverse video if possible. */
extern void terminal_end_inverse ();
extern VFunction *terminal_end_inverse_hook;

/* Scroll an area of the terminal, starting with the region from START
   to END, AMOUNT lines.  If AMOUNT is negative, the lines are scrolled
   towards the top of the screen, else they are scrolled towards the
   bottom of the screen. */
extern void terminal_scroll_terminal ();
extern VFunction *terminal_scroll_terminal_hook;

/* Ring the terminal bell.  The bell is run visibly if it both has one and
   terminal_use_visible_bell_p is non-zero. */
extern void terminal_ring_bell ();
extern VFunction *terminal_ring_bell_hook;

/* The key sequences output by the arrow keys, if this terminal has any. */
extern char *term_ku, *term_kd, *term_kr, *term_kl;
extern char *term_kP, *term_kN;

#endif /* !TERMINAL_H */
