/* Data structures and function declarations for the SSA value propagation
   engine.
   Copyright (C) 2001, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef _TREE_SSA_PROPAGATE_H
#define _TREE_SSA_PROPAGATE_H 1

/* Use the TREE_VISITED bitflag to mark statements and PHI nodes that
   have been deemed varying and should not be simulated again.  */
#define DONT_SIMULATE_AGAIN(T)	TREE_VISITED (T)

/* Lattice values used for propagation purposes.  Specific instances
   of a propagation engine must return these values from the statement
   and PHI visit functions to direct the engine.  */

enum ssa_prop_result {
    /* The statement produces nothing of interest.  No edges will be
       added to the work lists.  */
    SSA_PROP_NOT_INTERESTING,

    /* The statement produces an interesting value.  The set SSA_NAMEs
       returned by SSA_PROP_VISIT_STMT should be added to
       INTERESTING_SSA_EDGES.  If the statement being visited is a
       conditional jump, SSA_PROP_VISIT_STMT should indicate which edge
       out of the basic block should be marked executable.  */
    SSA_PROP_INTERESTING,

    /* The statement produces a varying (i.e., useless) value and
       should not be simulated again.  If the statement being visited
       is a conditional jump, all the edges coming out of the block
       will be considered executable.  */
    SSA_PROP_VARYING
};


/* Call-back functions used by the value propagation engine.  */
typedef enum ssa_prop_result (*ssa_prop_visit_stmt_fn) (tree, edge *, tree *);
typedef enum ssa_prop_result (*ssa_prop_visit_phi_fn) (tree);

void ssa_propagate (ssa_prop_visit_stmt_fn, ssa_prop_visit_phi_fn);
tree get_rhs (tree);
bool set_rhs (tree *, tree);

#endif /* _TREE_SSA_PROPAGATE_H  */
