/* 

    TREELANG Compiler definitions for interfacing to treetree.c
    (compiler back end interface).

    Copyright (C) 1986, 87, 89, 92-96, 1997, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

    This program is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the
    Free Software Foundation; either version 2, or (at your option) any
    later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, 59 Temple Place - Suite 330,
    Boston, MA 02111-1307, USA.

    In other words, you are welcome to use, share and improve this program.
    You are forbidden to forbid anyone else to use, share and improve
    what you give them.   Help stamp out software-hoarding!  

    ---------------------------------------------------------------------------

    Written by Tim Josling 1999, 2000, 2001, based in part on other
    parts of the GCC compiler.

 */

tree tree_code_init_parameters (void);
tree tree_code_add_parameter (tree list, tree proto_exp, tree exp);
tree tree_code_get_integer_value (unsigned char *chars, unsigned int length);
void tree_code_generate_return (tree type, tree exp);
void tree_ggc_storage_always_used  (void *m);
tree tree_code_get_expression (unsigned int exp_type, tree type, tree op1, tree op2, tree op3);
tree tree_code_get_numeric_type (unsigned int size1, unsigned int sign1);
void tree_code_create_function_initial (tree prev_saved,
                                       unsigned char* filename, int lineno,
                                       struct prod_token_parm_item* parms);
void tree_code_create_function_wrapup (unsigned char* filename, int lineno);
tree tree_code_create_function_prototype (unsigned char* chars,
                                         unsigned int storage_class,
                                         unsigned int ret_type,
                                         struct prod_token_parm_item* parms,                                 
                                         unsigned char* filename,
                                         int lineno);
tree tree_code_create_variable (unsigned int storage_class,
                               unsigned char* chars,
                               unsigned int length,
                               unsigned int expression_type,
                               tree init,
                               unsigned char* filename,
                               int lineno);
void tree_code_output_expression_statement (tree code, unsigned char* filename, int lineno);
tree get_type_for_numeric_type (unsigned int numeric_type);
void tree_code_if_start (tree exp, unsigned char* filename, int lineno);
void tree_code_if_else (unsigned char* filename, int lineno);
void tree_code_if_end (unsigned char* filename, int lineno);
tree tree_code_get_type (int type_num);
void treelang_init_decl_processing (void);
void treelang_finish (void);
const char * treelang_init (const char* filename);
int treelang_decode_option (int, char **);
void treelang_parse_file (int debug_flag);
void push_var_level (void);
void pop_var_level (void);



