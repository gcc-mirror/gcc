/* upc-gimplify.h: declarations for upc-gimplify.c
   Copyright (C) 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef _UPC_GIMPLIFY_H_
#define _UPC_GIMPLIFY_H_

extern int upc_gimplify_expr (tree *, gimple_seq *, gimple_seq *,
                              bool (*) (tree), int);
extern void upc_genericize (tree);

#endif /* !_UPC_GIMPLIFY_H_ */
