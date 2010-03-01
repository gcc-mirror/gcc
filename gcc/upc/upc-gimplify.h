/* Declarations for upc-gimplify.c.
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

#ifndef _UPC_GIMPLIFY_H_
#define _UPC_GIMPLIFY_H_

extern int upc_gimplify_expr (tree *, gimple_seq *, gimple_seq *,
                              bool (*) (tree), int);
extern void upc_genericize (tree);

#endif /* !_UPC_GIMPLIFY_H_ */
