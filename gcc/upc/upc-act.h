/* Declarations for upc-act.c.
   Copyright (C) 2001 Free Software Foundation, Inc.
   Original Implementation by Jesse M. Draper <jdraper@super.org>
   and William W. Carlson <wwc@super.org>.
   Ported to SGI Irix 6.5 and the gcc 2.95.2 baseline by
   Gary Funck <gary@intrepid.com> and Nenad Vukicevic <nenad@intrepid.com>.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef _UPC_ACT_H_
#define _UPC_ACT_H_


extern int upc_handle_option (size_t, const char *, int, int);
extern bool upc_lang_init (void);
extern void upc_finish (void);
extern void upc_finish_file (void);
extern void upc_write_init_func (void);
extern void upc_free_unshared_var_table (void);
extern void upc_check_decl (tree);
extern int upc_types_compatible_p (tree, tree);
extern int upc_inner_shared_ref_p (tree);

/* used by c-parser */
extern tree upc_build_sync_stmt (location_t, tree, tree);
extern tree upc_affinity_test (location_t, tree, tree);
extern struct c_expr upc_blocksizeof_expr (location_t, struct c_expr);
extern struct c_expr upc_blocksizeof_type (location_t, struct c_type_name *);
extern struct c_expr upc_elemsizeof_expr (location_t, struct c_expr);
extern struct c_expr upc_elemsizeof_type (location_t, struct c_type_name *);
extern struct c_expr upc_localsizeof_expr (location_t, struct c_expr);
extern struct c_expr upc_localsizeof_type (location_t, struct c_type_name *);

/* UPC-specific routines */
extern tree upc_num_threads (void);
extern int is_valid_pts_p (tree);

#endif /* !_UPC_ACT_H_ */
