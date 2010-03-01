/* Implement UPC PTS 'struct' semantics.
   Copyright (C) 2008 Free Software Foundation, Inc.
   Written by: Gary Funck <gary@intrepid.com>

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

#ifndef _UPC_PTS_H_
#define _UPC_PTS_H_ 1

extern tree upc_pts_build_value (location_t, tree, tree, tree, tree);
extern tree upc_pts_build_add_offset (location_t, tree, tree);
extern tree upc_pts_build_cond_expr (location_t, tree);
extern tree upc_pts_build_constant (location_t, tree);
extern tree upc_pts_build_cvt (location_t, tree);
extern tree upc_pts_build_diff (location_t, tree);
extern tree upc_pts_build_sum (location_t, tree);
extern tree upc_pts_build_threadof (location_t, tree);
extern void upc_pts_init_type (void);

#endif  /* !_UPC_PTS_H_ */
