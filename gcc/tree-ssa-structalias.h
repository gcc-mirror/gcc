/* Tree based points-to analysis
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dberlin@dberlin.org>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#ifndef TREE_SSA_STRUCTALIAS_H
#define TREE_SSA_STRUCTALIAS_H

struct constraint;
typedef struct constraint *constraint_t;

extern void dump_constraint (FILE *, constraint_t);
extern void dump_constraints (FILE *);
extern void debug_constraint (constraint_t);
extern void debug_constraints (void);
extern void dump_solution_for_var (FILE *, unsigned int);
extern void debug_solution_for_var (unsigned int);
extern void dump_sa_points_to_info (FILE *);
extern void debug_sa_points_to_info (void);

#endif /* TREE_SSA_STRUCTALIAS_H  */
