/* echo-area.h -- Functions used in reading information from the echo area.
   $Id: echo-area.h,v 1.1.1.2 1998/03/24 18:20:09 law Exp $

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

#ifndef INFO_ECHO_AREA_H
#define INFO_ECHO_AREA_H

#define EA_MAX_INPUT 256

extern int echo_area_is_active, info_aborted_echo_area;

/* Non-zero means that the last command executed while reading input
   killed some text. */
extern int echo_area_last_command_was_kill;

extern void inform_in_echo_area (), echo_area_inform_of_deleted_window ();
extern void echo_area_prep_read ();
extern VFunction *ea_last_executed_command;

/* Read a line of text in the echo area.  Return a malloc ()'ed string,
   or NULL if the user aborted out of this read.  WINDOW is the currently
   active window, so that we can restore it when we need to.  PROMPT, if
   non-null, is a prompt to print before reading the line. */
extern char *info_read_in_echo_area ();

/* Read a line in the echo area with completion over COMPLETIONS.
   Takes arguments of WINDOW, PROMPT, and COMPLETIONS, a REFERENCE **. */
char *info_read_completing_in_echo_area ();

/* Read a line in the echo area allowing completion over COMPLETIONS, but
   not requiring it.  Takes arguments of WINDOW, PROMPT, and COMPLETIONS,
   a REFERENCE **. */
extern char *info_read_maybe_completing ();

extern void ea_insert (), ea_quoted_insert ();
extern void ea_beg_of_line (), ea_backward (), ea_delete (), ea_end_of_line ();
extern void ea_forward (), ea_abort (), ea_rubout (), ea_complete ();
extern void ea_newline (), ea_kill_line (), ea_transpose_chars ();
extern void ea_yank (), ea_tab_insert (), ea_possible_completions ();
extern void ea_backward_word (), ea_kill_word (), ea_forward_word ();
extern void ea_yank_pop (), ea_backward_kill_word ();
extern void ea_scroll_completions_window ();

#endif /* not INFO_ECHO_AREA_H */
