/* session.h -- Functions found in session.c. */

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

#if !defined (_SESSION_H_)
#define _SESSION_H_

#include "general.h"
#include "dribble.h"

/* All commands that can be invoked from within info_session () receive
   arguments in the same way.  This simple define declares the header
   of a function named NAME, with associated documentation DOC.  The
   documentation string is groveled out of the source files by the
   utility program `makedoc', which is also responsible for making
   the documentation/function-pointer maps. */
#define DECLARE_INFO_COMMAND(name, doc) \
void name (window, count, key) WINDOW *window; int count; unsigned char key;

/* Variables found in session.h. */
extern VFunction *info_last_executed_command;

/* Variable controlling the garbage collection of files briefly visited
   during searches.  Such files are normally gc'ed, unless they were
   compressed to begin with.  If this variable is non-zero, it says
   to gc even those file buffer contents which had to be uncompressed. */
extern int gc_compressed_files;

/* When non-zero, tiling takes place automatically when info_split_window
   is called. */
extern int auto_tiling_p;

/* Variable controlling the behaviour of default scrolling when you are
   already at the bottom of a node. */
extern int info_scroll_behaviour;
extern char *info_scroll_choices[];

/* Values for info_scroll_behaviour. */
#define IS_Continuous 0	/* Try to get first menu item, or failing that, the
			   "Next:" pointer, or failing that, the "Up:" and
			   "Next:" of the up. */
#define IS_NextOnly   1 /* Try to get "Next:" menu item. */
#define IS_PageOnly   2	/* Simply give up at the bottom of a node. */

/* Utility functions found in session.c */
extern void info_dispatch_on_key ();
extern unsigned char info_get_input_char (), info_get_another_input_char ();
extern unsigned char info_input_pending_p ();
extern void remember_window_and_node (), set_remembered_pagetop_and_point ();
extern void set_window_pagetop (), info_set_node_of_window ();
extern char *pretty_keyseq ();
extern void initialize_keyseq (), add_char_to_keyseq ();
extern void info_gather_typeahead ();
extern FILE_BUFFER *file_buffer_of_window ();
extern long info_search_in_node (), info_target_search_node ();
extern void info_select_reference ();
extern int info_any_buffered_input_p ();
extern void print_node ();
extern void dump_node_to_file (), dump_nodes_to_file ();

/* Do the physical deletion of WINDOW, and forget this window and
   associated nodes. */
extern void info_delete_window_internal ();

/* Tell Info that input is coming from the file FILENAME. */
extern void info_set_input_from_file ();

#define return_if_control_g(val) \
  do { \
    info_gather_typeahead (); \
    if (info_input_pending_p () == Control ('g')) \
      return (val); \
  } while (0)

/* The names of the functions that run an info session. */

/* Starting an info session. */
extern void begin_multiple_window_info_session (), begin_info_session ();
extern void begin_info_session_with_error (), info_session ();
extern void info_read_and_dispatch ();

/* Moving the point within a node. */
extern void info_next_line (), info_prev_line ();
extern void info_end_of_line (), info_beginning_of_line ();
extern void info_forward_char (), info_backward_char ();
extern void info_forward_word (), info_backward_word ();
extern void info_beginning_of_node (), info_end_of_node ();
extern void info_move_to_prev_xref (), info_move_to_next_xref ();

/* Scrolling text within a window. */
extern void info_scroll_forward (), info_scroll_backward ();
extern void info_redraw_display (), info_toggle_wrap ();
extern void info_move_to_window_line ();

/* Manipulating multiple windows. */
extern void info_split_window (), info_delete_window ();
extern void info_keep_one_window (), info_grow_window ();
extern void info_scroll_other_window (), info_tile_windows ();
extern void info_next_window (), info_prev_window ();

/* Selecting nodes. */
extern void info_next_node (), info_prev_node (), info_up_node ();
extern void info_last_node (), info_first_node (), info_history_node ();
extern void info_goto_node (), info_top_node (), info_dir_node ();
extern void info_global_next_node (), info_global_prev_node ();
extern void info_kill_node (), info_view_file ();

/* Selecting cross references. */
extern void info_menu_digit (), info_menu_item (), info_xref_item ();
extern void info_find_menu (), info_select_reference_this_line ();

/* Hacking numeric arguments. */
extern int info_explicit_arg, info_numeric_arg, info_numeric_arg_sign;

extern void info_add_digit_to_numeric_arg (), info_universal_argument ();
extern void info_initialize_numeric_arg (), info_numeric_arg_digit_loop ();

/* Searching commands. */
extern void info_search (), isearch_forward (), isearch_backward ();

/* Dumping and printing nodes. */
extern void info_print_node ();

/* Miscellaneous commands. */
extern void info_abort_key (), info_quit (), info_do_lowercase_version ();

#endif /* _SESSION_H_ */
