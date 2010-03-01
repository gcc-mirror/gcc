/* Definitions for UPC parsing and type checking.
   Copyright (C) 1987, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

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
