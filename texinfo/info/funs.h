/* funs.h -- Generated declarations for Info commands. */

/* Functions declared in "./session.c". */
extern void info_next_line ();
extern void info_prev_line ();
extern void info_end_of_line ();
extern void info_beginning_of_line ();
extern void info_forward_char ();
extern void info_backward_char ();
extern void info_forward_word ();
extern void info_backward_word ();
extern void info_global_next_node ();
extern void info_global_prev_node ();
extern void info_scroll_forward ();
extern void info_scroll_backward ();
extern void info_beginning_of_node ();
extern void info_end_of_node ();
extern void info_next_window ();
extern void info_prev_window ();
extern void info_split_window ();
extern void info_delete_window ();
extern void info_keep_one_window ();
extern void info_scroll_other_window ();
extern void info_grow_window ();
extern void info_tile_windows ();
extern void info_toggle_wrap ();
extern void info_next_node ();
extern void info_prev_node ();
extern void info_up_node ();
extern void info_last_node ();
extern void info_first_node ();
extern void info_last_menu_item ();
extern void info_menu_digit ();
extern void info_menu_item ();
extern void info_xref_item ();
extern void info_find_menu ();
extern void info_visit_menu ();
extern void info_goto_node ();
extern void info_man ();
extern void info_top_node ();
extern void info_dir_node ();
extern void info_history_node ();
extern void info_kill_node ();
extern void info_view_file ();
extern void info_print_node ();
extern void info_search ();
extern void isearch_forward ();
extern void isearch_backward ();
extern void info_move_to_prev_xref ();
extern void info_move_to_next_xref ();
extern void info_select_reference_this_line ();
extern void info_abort_key ();
extern void info_move_to_window_line ();
extern void info_redraw_display ();
extern void info_quit ();
extern void info_do_lowercase_version ();
extern void info_add_digit_to_numeric_arg ();
extern void info_universal_argument ();
extern void info_numeric_arg_digit_loop ();

/* Functions declared in "./echo-area.c". */
extern void ea_forward ();
extern void ea_backward ();
extern void ea_beg_of_line ();
extern void ea_end_of_line ();
extern void ea_forward_word ();
extern void ea_backward_word ();
extern void ea_delete ();
extern void ea_rubout ();
extern void ea_abort ();
extern void ea_newline ();
extern void ea_quoted_insert ();
extern void ea_insert ();
extern void ea_tab_insert ();
extern void ea_transpose_chars ();
extern void ea_yank ();
extern void ea_yank_pop ();
extern void ea_kill_line ();
extern void ea_backward_kill_line ();
extern void ea_kill_word ();
extern void ea_backward_kill_word ();
extern void ea_possible_completions ();
extern void ea_complete ();
extern void ea_scroll_completions_window ();

/* Functions declared in "./infodoc.c". */
extern void info_get_help_window ();
extern void info_get_info_help_node ();
extern void describe_key ();
extern void info_where_is ();

/* Functions declared in "./m-x.c". */
extern void describe_command ();
extern void info_execute_command ();
extern void set_screen_height ();

/* Functions declared in "./indices.c". */
extern void info_index_search ();
extern void info_next_index_match ();
extern void info_index_apropos ();

/* Functions declared in "./nodemenu.c". */
extern void list_visited_nodes ();
extern void select_visited_node ();

/* Functions declared in "./footnotes.c". */
extern void info_show_footnotes ();

/* Functions declared in "./variables.c". */
extern void describe_variable ();
extern void set_variable ();
