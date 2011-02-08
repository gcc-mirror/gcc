/* upc-tree.h: UPC language specific tree node access functions
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.
   Based on original implementation
     by Jesse M. Draper <jdraper@super.org>
     and William W. Carlson <wwc@super.org>.

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

#ifndef UPC_TREE_H
#define UPC_CP_TREE_H

/* Used to represent a UPC synchronization statement. The first
   operand is the synchonization operation, UPC_SYNC_OP:
   UPC_SYNC_NOTIFY_OP	1	Notify operation
   UPC_SYNC_WAIT_OP	2	Wait operation
   UPC_SYNC_BARRIER_OP	3	Barrier operation

   The second operand, UPC_SYNC_ID is the (optional) expression
   whose value specifies the barrier identifier which is checked
   by the various synchronization operations. */

#define UPC_SYNC_OP(NODE)	TREE_OPERAND (UPC_SYNC_STMT_CHECK (NODE), 0)
#define UPC_SYNC_ID(NODE)	TREE_OPERAND (UPC_SYNC_STMT_CHECK (NODE), 1)

/* Values of the first operand in a UPC_SYNC_STMT */

#define UPC_SYNC_NOTIFY_OP	1	/* Notify operation */
#define UPC_SYNC_WAIT_OP	2	/* Wait operation */
#define UPC_SYNC_BARRIER_OP	3	/* Barrier operation */

#endif /* ! UPC_TREE_H */
