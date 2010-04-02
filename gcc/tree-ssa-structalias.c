/* Tree based points-to analysis
   Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dberlin@dberlin.org>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "ggc.h"
#include "obstack.h"
#include "bitmap.h"
#include "flags.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "tree.h"
#include "tree-flow.h"
#include "tree-inline.h"
#include "varray.h"
#include "diagnostic.h"
#include "toplev.h"
#include "gimple.h"
#include "hashtab.h"
#include "function.h"
#include "cgraph.h"
#include "tree-pass.h"
#include "timevar.h"
#include "alloc-pool.h"
#include "splay-tree.h"
#include "params.h"
#include "cgraph.h"
#include "alias.h"
#include "pointer-set.h"

/* The idea behind this analyzer is to generate set constraints from the
   program, then solve the resulting constraints in order to generate the
   points-to sets.

   Set constraints are a way of modeling program analysis problems that
   involve sets.  They consist of an inclusion constraint language,
   describing the variables (each variable is a set) and operations that
   are involved on the variables, and a set of rules that derive facts
   from these operations.  To solve a system of set constraints, you derive
   all possible facts under the rules, which gives you the correct sets
   as a consequence.

   See  "Efficient Field-sensitive pointer analysis for C" by "David
   J. Pearce and Paul H. J. Kelly and Chris Hankin, at
   http://citeseer.ist.psu.edu/pearce04efficient.html

   Also see "Ultra-fast Aliasing Analysis using CLA: A Million Lines
   of C Code in a Second" by ""Nevin Heintze and Olivier Tardieu" at
   http://citeseer.ist.psu.edu/heintze01ultrafast.html

   There are three types of real constraint expressions, DEREF,
   ADDRESSOF, and SCALAR.  Each constraint expression consists
   of a constraint type, a variable, and an offset.

   SCALAR is a constraint expression type used to represent x, whether
   it appears on the LHS or the RHS of a statement.
   DEREF is a constraint expression type used to represent *x, whether
   it appears on the LHS or the RHS of a statement.
   ADDRESSOF is a constraint expression used to represent &x, whether
   it appears on the LHS or the RHS of a statement.

   Each pointer variable in the program is assigned an integer id, and
   each field of a structure variable is assigned an integer id as well.

   Structure variables are linked to their list of fields through a "next
   field" in each variable that points to the next field in offset
   order.
   Each variable for a structure field has

   1. "size", that tells the size in bits of that field.
   2. "fullsize, that tells the size in bits of the entire structure.
   3. "offset", that tells the offset in bits from the beginning of the
   structure to this field.

   Thus,
   struct f
   {
     int a;
     int b;
   } foo;
   int *bar;

   looks like

   foo.a -> id 1, size 32, offset 0, fullsize 64, next foo.b
   foo.b -> id 2, size 32, offset 32, fullsize 64, next NULL
   bar -> id 3, size 32, offset 0, fullsize 32, next NULL


  In order to solve the system of set constraints, the following is
  done:

  1. Each constraint variable x has a solution set associated with it,
  Sol(x).

  2. Constraints are separated into direct, copy, and complex.
  Direct constraints are ADDRESSOF constraints that require no extra
  processing, such as P = &Q
  Copy constraints are those of the form P = Q.
  Complex constraints are all the constraints involving dereferences
  and offsets (including offsetted copies).

  3. All direct constraints of the form P = &Q are processed, such
  that Q is added to Sol(P)

  4. All complex constraints for a given constraint variable are stored in a
  linked list attached to that variable's node.

  5. A directed graph is built out of the copy constraints. Each
  constraint variable is a node in the graph, and an edge from
  Q to P is added for each copy constraint of the form P = Q

  6. The graph is then walked, and solution sets are
  propagated along the copy edges, such that an edge from Q to P
  causes Sol(P) <- Sol(P) union Sol(Q).

  7.  As we visit each node, all complex constraints associated with
  that node are processed by adding appropriate copy edges to the graph, or the
  appropriate variables to the solution set.

  8. The process of walking the graph is iterated until no solution
  sets change.

  Prior to walking the graph in steps 6 and 7, We perform static
  cycle elimination on the constraint graph, as well
  as off-line variable substitution.

  TODO: Adding offsets to pointer-to-structures can be handled (IE not punted
  on and turned into anything), but isn't.  You can just see what offset
  inside the pointed-to struct it's going to access.

  TODO: Constant bounded arrays can be handled as if they were structs of the
  same number of elements.

  TODO: Modeling heap and incoming pointers becomes much better if we
  add fields to them as we discover them, which we could do.

  TODO: We could handle unions, but to be honest, it's probably not
  worth the pain or slowdown.  */

static GTY ((if_marked ("tree_map_marked_p"), param_is (struct tree_map)))
htab_t heapvar_for_stmt;

static bool use_field_sensitive = true;
static int in_ipa_mode = 0;

/* Used for predecessor bitmaps. */
static bitmap_obstack predbitmap_obstack;

/* Used for points-to sets.  */
static bitmap_obstack pta_obstack;

/* Used for oldsolution members of variables. */
static bitmap_obstack oldpta_obstack;

/* Used for per-solver-iteration bitmaps.  */
static bitmap_obstack iteration_obstack;

static unsigned int create_variable_info_for (tree, const char *);
typedef struct constraint_graph *constraint_graph_t;
static void unify_nodes (constraint_graph_t, unsigned int, unsigned int, bool);

struct constraint;
typedef struct constraint *constraint_t;

DEF_VEC_P(constraint_t);
DEF_VEC_ALLOC_P(constraint_t,heap);

#define EXECUTE_IF_IN_NONNULL_BITMAP(a, b, c, d)	\
  if (a)						\
    EXECUTE_IF_SET_IN_BITMAP (a, b, c, d)

static struct constraint_stats
{
  unsigned int total_vars;
  unsigned int nonpointer_vars;
  unsigned int unified_vars_static;
  unsigned int unified_vars_dynamic;
  unsigned int iterations;
  unsigned int num_edges;
  unsigned int num_implicit_edges;
  unsigned int points_to_sets_created;
} stats;

struct variable_info
{
  /* ID of this variable  */
  unsigned int id;

  /* True if this is a variable created by the constraint analysis, such as
     heap variables and constraints we had to break up.  */
  unsigned int is_artificial_var : 1;

  /* True if this is a special variable whose solution set should not be
     changed.  */
  unsigned int is_special_var : 1;

  /* True for variables whose size is not known or variable.  */
  unsigned int is_unknown_size_var : 1;

  /* True for (sub-)fields that represent a whole variable.  */
  unsigned int is_full_var : 1;

  /* True if this is a heap variable.  */
  unsigned int is_heap_var : 1;

  /* True if this is a variable tracking a restrict pointer source.  */
  unsigned int is_restrict_var : 1;

  /* True if this field may contain pointers.  */
  unsigned int may_have_pointers : 1;

  /* True if this represents a global variable.  */
  unsigned int is_global_var : 1;

  /* A link to the variable for the next field in this structure.  */
  struct variable_info *next;

  /* Offset of this variable, in bits, from the base variable  */
  unsigned HOST_WIDE_INT offset;

  /* Size of the variable, in bits.  */
  unsigned HOST_WIDE_INT size;

  /* Full size of the base variable, in bits.  */
  unsigned HOST_WIDE_INT fullsize;

  /* Name of this variable */
  const char *name;

  /* Tree that this variable is associated with.  */
  tree decl;

  /* Points-to set for this variable.  */
  bitmap solution;

  /* Old points-to set for this variable.  */
  bitmap oldsolution;
};
typedef struct variable_info *varinfo_t;

static varinfo_t first_vi_for_offset (varinfo_t, unsigned HOST_WIDE_INT);
static varinfo_t first_or_preceding_vi_for_offset (varinfo_t,
						   unsigned HOST_WIDE_INT);
static varinfo_t lookup_vi_for_tree (tree);

/* Pool of variable info structures.  */
static alloc_pool variable_info_pool;

DEF_VEC_P(varinfo_t);

DEF_VEC_ALLOC_P(varinfo_t, heap);

/* Table of variable info structures for constraint variables.
   Indexed directly by variable info id.  */
static VEC(varinfo_t,heap) *varmap;

/* Return the varmap element N */

static inline varinfo_t
get_varinfo (unsigned int n)
{
  return VEC_index (varinfo_t, varmap, n);
}

/* Static IDs for the special variables.  */
enum { nothing_id = 0, anything_id = 1, readonly_id = 2,
       escaped_id = 3, nonlocal_id = 4, callused_id = 5,
       storedanything_id = 6, integer_id = 7 };

struct GTY(()) heapvar_map {
  struct tree_map map;
  unsigned HOST_WIDE_INT offset;
};

static int
heapvar_map_eq (const void *p1, const void *p2)
{
  const struct heapvar_map *h1 = (const struct heapvar_map *)p1;
  const struct heapvar_map *h2 = (const struct heapvar_map *)p2;
  return (h1->map.base.from == h2->map.base.from
	  && h1->offset == h2->offset);
}

static unsigned int
heapvar_map_hash (struct heapvar_map *h)
{
  return iterative_hash_host_wide_int (h->offset,
				       htab_hash_pointer (h->map.base.from));
}

/* Lookup a heap var for FROM, and return it if we find one.  */

static tree
heapvar_lookup (tree from, unsigned HOST_WIDE_INT offset)
{
  struct heapvar_map *h, in;
  in.map.base.from = from;
  in.offset = offset;
  h = (struct heapvar_map *) htab_find_with_hash (heapvar_for_stmt, &in,
						  heapvar_map_hash (&in));
  if (h)
    return h->map.to;
  return NULL_TREE;
}

/* Insert a mapping FROM->TO in the heap var for statement
   hashtable.  */

static void
heapvar_insert (tree from, unsigned HOST_WIDE_INT offset, tree to)
{
  struct heapvar_map *h;
  void **loc;

  h = GGC_NEW (struct heapvar_map);
  h->map.base.from = from;
  h->offset = offset;
  h->map.hash = heapvar_map_hash (h);
  h->map.to = to;
  loc = htab_find_slot_with_hash (heapvar_for_stmt, h, h->map.hash, INSERT);
  gcc_assert (*loc == NULL);
  *(struct heapvar_map **) loc = h;
}

/* Return a new variable info structure consisting for a variable
   named NAME, and using constraint graph node NODE.  Append it
   to the vector of variable info structures.  */

static varinfo_t
new_var_info (tree t, const char *name)
{
  unsigned index = VEC_length (varinfo_t, varmap);
  varinfo_t ret = (varinfo_t) pool_alloc (variable_info_pool);

  ret->id = index;
  ret->name = name;
  ret->decl = t;
  /* Vars without decl are artificial and do not have sub-variables.  */
  ret->is_artificial_var = (t == NULL_TREE);
  ret->is_special_var = false;
  ret->is_unknown_size_var = false;
  ret->is_full_var = (t == NULL_TREE);
  ret->is_heap_var = false;
  ret->is_restrict_var = false;
  ret->may_have_pointers = true;
  ret->is_global_var = (t == NULL_TREE);
  if (t && DECL_P (t))
    ret->is_global_var = is_global_var (t);
  ret->solution = BITMAP_ALLOC (&pta_obstack);
  ret->oldsolution = BITMAP_ALLOC (&oldpta_obstack);
  ret->next = NULL;

  VEC_safe_push (varinfo_t, heap, varmap, ret);

  return ret;
}

typedef enum {SCALAR, DEREF, ADDRESSOF} constraint_expr_type;

/* An expression that appears in a constraint.  */

struct constraint_expr
{
  /* Constraint type.  */
  constraint_expr_type type;

  /* Variable we are referring to in the constraint.  */
  unsigned int var;

  /* Offset, in bits, of this constraint from the beginning of
     variables it ends up referring to.

     IOW, in a deref constraint, we would deref, get the result set,
     then add OFFSET to each member.   */
  HOST_WIDE_INT offset;
};

/* Use 0x8000... as special unknown offset.  */
#define UNKNOWN_OFFSET ((HOST_WIDE_INT)-1 << (HOST_BITS_PER_WIDE_INT-1))

typedef struct constraint_expr ce_s;
DEF_VEC_O(ce_s);
DEF_VEC_ALLOC_O(ce_s, heap);
static void get_constraint_for_1 (tree, VEC(ce_s, heap) **, bool);
static void get_constraint_for (tree, VEC(ce_s, heap) **);
static void do_deref (VEC (ce_s, heap) **);

/* Our set constraints are made up of two constraint expressions, one
   LHS, and one RHS.

   As described in the introduction, our set constraints each represent an
   operation between set valued variables.
*/
struct constraint
{
  struct constraint_expr lhs;
  struct constraint_expr rhs;
};

/* List of constraints that we use to build the constraint graph from.  */

static VEC(constraint_t,heap) *constraints;
static alloc_pool constraint_pool;

/* The constraint graph is represented as an array of bitmaps
   containing successor nodes.  */

struct constraint_graph
{
  /* Size of this graph, which may be different than the number of
     nodes in the variable map.  */
  unsigned int size;

  /* Explicit successors of each node. */
  bitmap *succs;

  /* Implicit predecessors of each node (Used for variable
     substitution). */
  bitmap *implicit_preds;

  /* Explicit predecessors of each node (Used for variable substitution).  */
  bitmap *preds;

  /* Indirect cycle representatives, or -1 if the node has no indirect
     cycles.  */
  int *indirect_cycles;

  /* Representative node for a node.  rep[a] == a unless the node has
     been unified. */
  unsigned int *rep;

  /* Equivalence class representative for a label.  This is used for
     variable substitution.  */
  int *eq_rep;

  /* Pointer equivalence label for a node.  All nodes with the same
     pointer equivalence label can be unified together at some point
     (either during constraint optimization or after the constraint
     graph is built).  */
  unsigned int *pe;

  /* Pointer equivalence representative for a label.  This is used to
     handle nodes that are pointer equivalent but not location
     equivalent.  We can unite these once the addressof constraints
     are transformed into initial points-to sets.  */
  int *pe_rep;

  /* Pointer equivalence label for each node, used during variable
     substitution.  */
  unsigned int *pointer_label;

  /* Location equivalence label for each node, used during location
     equivalence finding.  */
  unsigned int *loc_label;

  /* Pointed-by set for each node, used during location equivalence
     finding.  This is pointed-by rather than pointed-to, because it
     is constructed using the predecessor graph.  */
  bitmap *pointed_by;

  /* Points to sets for pointer equivalence.  This is *not* the actual
     points-to sets for nodes.  */
  bitmap *points_to;

  /* Bitmap of nodes where the bit is set if the node is a direct
     node.  Used for variable substitution.  */
  sbitmap direct_nodes;

  /* Bitmap of nodes where the bit is set if the node is address
     taken.  Used for variable substitution.  */
  bitmap address_taken;

  /* Vector of complex constraints for each graph node.  Complex
     constraints are those involving dereferences or offsets that are
     not 0.  */
  VEC(constraint_t,heap) **complex;
};

static constraint_graph_t graph;

/* During variable substitution and the offline version of indirect
   cycle finding, we create nodes to represent dereferences and
   address taken constraints.  These represent where these start and
   end.  */
#define FIRST_REF_NODE (VEC_length (varinfo_t, varmap))
#define LAST_REF_NODE (FIRST_REF_NODE + (FIRST_REF_NODE - 1))

/* Return the representative node for NODE, if NODE has been unioned
   with another NODE.
   This function performs path compression along the way to finding
   the representative.  */

static unsigned int
find (unsigned int node)
{
  gcc_assert (node < graph->size);
  if (graph->rep[node] != node)
    return graph->rep[node] = find (graph->rep[node]);
  return node;
}

/* Union the TO and FROM nodes to the TO nodes.
   Note that at some point in the future, we may want to do
   union-by-rank, in which case we are going to have to return the
   node we unified to.  */

static bool
unite (unsigned int to, unsigned int from)
{
  gcc_assert (to < graph->size && from < graph->size);
  if (to != from && graph->rep[from] != to)
    {
      graph->rep[from] = to;
      return true;
    }
  return false;
}

/* Create a new constraint consisting of LHS and RHS expressions.  */

static constraint_t
new_constraint (const struct constraint_expr lhs,
		const struct constraint_expr rhs)
{
  constraint_t ret = (constraint_t) pool_alloc (constraint_pool);
  ret->lhs = lhs;
  ret->rhs = rhs;
  return ret;
}

/* Print out constraint C to FILE.  */

static void
dump_constraint (FILE *file, constraint_t c)
{
  if (c->lhs.type == ADDRESSOF)
    fprintf (file, "&");
  else if (c->lhs.type == DEREF)
    fprintf (file, "*");
  fprintf (file, "%s", get_varinfo (c->lhs.var)->name);
  if (c->lhs.offset == UNKNOWN_OFFSET)
    fprintf (file, " + UNKNOWN");
  else if (c->lhs.offset != 0)
    fprintf (file, " + " HOST_WIDE_INT_PRINT_DEC, c->lhs.offset);
  fprintf (file, " = ");
  if (c->rhs.type == ADDRESSOF)
    fprintf (file, "&");
  else if (c->rhs.type == DEREF)
    fprintf (file, "*");
  fprintf (file, "%s", get_varinfo (c->rhs.var)->name);
  if (c->rhs.offset == UNKNOWN_OFFSET)
    fprintf (file, " + UNKNOWN");
  else if (c->rhs.offset != 0)
    fprintf (file, " + " HOST_WIDE_INT_PRINT_DEC, c->rhs.offset);
  fprintf (file, "\n");
}


void debug_constraint (constraint_t);
void debug_constraints (void);
void debug_constraint_graph (void);
void debug_solution_for_var (unsigned int);
void debug_sa_points_to_info (void);

/* Print out constraint C to stderr.  */

void
debug_constraint (constraint_t c)
{
  dump_constraint (stderr, c);
}

/* Print out all constraints to FILE */

static void
dump_constraints (FILE *file)
{
  int i;
  constraint_t c;
  for (i = 0; VEC_iterate (constraint_t, constraints, i, c); i++)
    dump_constraint (file, c);
}

/* Print out all constraints to stderr.  */

void
debug_constraints (void)
{
  dump_constraints (stderr);
}

/* Print out to FILE the edge in the constraint graph that is created by
   constraint c. The edge may have a label, depending on the type of
   constraint that it represents. If complex1, e.g: a = *b, then the label
   is "=*", if complex2, e.g: *a = b, then the label is "*=", if
   complex with an offset, e.g: a = b + 8, then the label is "+".
   Otherwise the edge has no label.  */

static void
dump_constraint_edge (FILE *file, constraint_t c)
{
  if (c->rhs.type != ADDRESSOF)
    {
      const char *src = get_varinfo (c->rhs.var)->name;
      const char *dst = get_varinfo (c->lhs.var)->name;
      fprintf (file, "  \"%s\" -> \"%s\" ", src, dst);
      /* Due to preprocessing of constraints, instructions like *a = *b are
         illegal; thus, we do not have to handle such cases.  */
      if (c->lhs.type == DEREF)
        fprintf (file, " [ label=\"*=\" ] ;\n");
      else if (c->rhs.type == DEREF)
        fprintf (file, " [ label=\"=*\" ] ;\n");
      else
        {
          /* We must check the case where the constraint is an offset.
             In this case, it is treated as a complex constraint.  */
          if (c->rhs.offset != c->lhs.offset)
            fprintf (file, " [ label=\"+\" ] ;\n");
          else
            fprintf (file, " ;\n");
        }
    }
}

/* Print the constraint graph in dot format.  */

static void
dump_constraint_graph (FILE *file)
{
  unsigned int i=0, size;
  constraint_t c;

  /* Only print the graph if it has already been initialized:  */
  if (!graph)
    return;

  /* Print the constraints used to produce the constraint graph. The
     constraints will be printed as comments in the dot file:  */
  fprintf (file, "\n\n/* Constraints used in the constraint graph:\n");
  dump_constraints (file);
  fprintf (file, "*/\n");

  /* Prints the header of the dot file:  */
  fprintf (file, "\n\n// The constraint graph in dot format:\n");
  fprintf (file, "strict digraph {\n");
  fprintf (file, "  node [\n    shape = box\n  ]\n");
  fprintf (file, "  edge [\n    fontsize = \"12\"\n  ]\n");
  fprintf (file, "\n  // List of nodes in the constraint graph:\n");

  /* The next lines print the nodes in the graph. In order to get the
     number of nodes in the graph, we must choose the minimum between the
     vector VEC (varinfo_t, varmap) and graph->size. If the graph has not
     yet been initialized, then graph->size == 0, otherwise we must only
     read nodes that have an entry in VEC (varinfo_t, varmap).  */
  size = VEC_length (varinfo_t, varmap);
  size = size < graph->size ? size : graph->size;
  for (i = 0; i < size; i++)
    {
      const char *name = get_varinfo (graph->rep[i])->name;
      fprintf (file, "  \"%s\" ;\n", name);
    }

  /* Go over the list of constraints printing the edges in the constraint
     graph.  */
  fprintf (file, "\n  // The constraint edges:\n");
  for (i = 0; VEC_iterate (constraint_t, constraints, i, c); i++)
    if (c)
      dump_constraint_edge (file, c);

  /* Prints the tail of the dot file. By now, only the closing bracket.  */
  fprintf (file, "}\n\n\n");
}

/* Print out the constraint graph to stderr.  */

void
debug_constraint_graph (void)
{
  dump_constraint_graph (stderr);
}

/* SOLVER FUNCTIONS

   The solver is a simple worklist solver, that works on the following
   algorithm:

   sbitmap changed_nodes = all zeroes;
   changed_count = 0;
   For each node that is not already collapsed:
       changed_count++;
       set bit in changed nodes

   while (changed_count > 0)
   {
     compute topological ordering for constraint graph

     find and collapse cycles in the constraint graph (updating
     changed if necessary)

     for each node (n) in the graph in topological order:
       changed_count--;

       Process each complex constraint associated with the node,
       updating changed if necessary.

       For each outgoing edge from n, propagate the solution from n to
       the destination of the edge, updating changed as necessary.

   }  */

/* Return true if two constraint expressions A and B are equal.  */

static bool
constraint_expr_equal (struct constraint_expr a, struct constraint_expr b)
{
  return a.type == b.type && a.var == b.var && a.offset == b.offset;
}

/* Return true if constraint expression A is less than constraint expression
   B.  This is just arbitrary, but consistent, in order to give them an
   ordering.  */

static bool
constraint_expr_less (struct constraint_expr a, struct constraint_expr b)
{
  if (a.type == b.type)
    {
      if (a.var == b.var)
	return a.offset < b.offset;
      else
	return a.var < b.var;
    }
  else
    return a.type < b.type;
}

/* Return true if constraint A is less than constraint B.  This is just
   arbitrary, but consistent, in order to give them an ordering.  */

static bool
constraint_less (const constraint_t a, const constraint_t b)
{
  if (constraint_expr_less (a->lhs, b->lhs))
    return true;
  else if (constraint_expr_less (b->lhs, a->lhs))
    return false;
  else
    return constraint_expr_less (a->rhs, b->rhs);
}

/* Return true if two constraints A and B are equal.  */

static bool
constraint_equal (struct constraint a, struct constraint b)
{
  return constraint_expr_equal (a.lhs, b.lhs)
    && constraint_expr_equal (a.rhs, b.rhs);
}


/* Find a constraint LOOKFOR in the sorted constraint vector VEC */

static constraint_t
constraint_vec_find (VEC(constraint_t,heap) *vec,
		     struct constraint lookfor)
{
  unsigned int place;
  constraint_t found;

  if (vec == NULL)
    return NULL;

  place = VEC_lower_bound (constraint_t, vec, &lookfor, constraint_less);
  if (place >= VEC_length (constraint_t, vec))
    return NULL;
  found = VEC_index (constraint_t, vec, place);
  if (!constraint_equal (*found, lookfor))
    return NULL;
  return found;
}

/* Union two constraint vectors, TO and FROM.  Put the result in TO.  */

static void
constraint_set_union (VEC(constraint_t,heap) **to,
		      VEC(constraint_t,heap) **from)
{
  int i;
  constraint_t c;

  for (i = 0; VEC_iterate (constraint_t, *from, i, c); i++)
    {
      if (constraint_vec_find (*to, *c) == NULL)
	{
	  unsigned int place = VEC_lower_bound (constraint_t, *to, c,
						constraint_less);
	  VEC_safe_insert (constraint_t, heap, *to, place, c);
	}
    }
}

/* Expands the solution in SET to all sub-fields of variables included.
   Union the expanded result into RESULT.  */

static void
solution_set_expand (bitmap result, bitmap set)
{
  bitmap_iterator bi;
  bitmap vars = NULL;
  unsigned j;

  /* In a first pass record all variables we need to add all
     sub-fields off.  This avoids quadratic behavior.  */
  EXECUTE_IF_SET_IN_BITMAP (set, 0, j, bi)
    {
      varinfo_t v = get_varinfo (j);
      if (v->is_artificial_var
	  || v->is_full_var)
	continue;
      v = lookup_vi_for_tree (v->decl);
      if (vars == NULL)
	vars = BITMAP_ALLOC (NULL);
      bitmap_set_bit (vars, v->id);
    }

  /* In the second pass now do the addition to the solution and
     to speed up solving add it to the delta as well.  */
  if (vars != NULL)
    {
      EXECUTE_IF_SET_IN_BITMAP (vars, 0, j, bi)
	{
	  varinfo_t v = get_varinfo (j);
	  for (; v != NULL; v = v->next)
	    bitmap_set_bit (result, v->id);
	}
      BITMAP_FREE (vars);
    }
}

/* Take a solution set SET, add OFFSET to each member of the set, and
   overwrite SET with the result when done.  */

static void
solution_set_add (bitmap set, HOST_WIDE_INT offset)
{
  bitmap result = BITMAP_ALLOC (&iteration_obstack);
  unsigned int i;
  bitmap_iterator bi;

  /* If the offset is unknown we have to expand the solution to
     all subfields.  */
  if (offset == UNKNOWN_OFFSET)
    {
      solution_set_expand (set, set);
      return;
    }

  EXECUTE_IF_SET_IN_BITMAP (set, 0, i, bi)
    {
      varinfo_t vi = get_varinfo (i);

      /* If this is a variable with just one field just set its bit
         in the result.  */
      if (vi->is_artificial_var
	  || vi->is_unknown_size_var
	  || vi->is_full_var)
	bitmap_set_bit (result, i);
      else
	{
	  unsigned HOST_WIDE_INT fieldoffset = vi->offset + offset;

	  /* If the offset makes the pointer point to before the
	     variable use offset zero for the field lookup.  */
	  if (offset < 0
	      && fieldoffset > vi->offset)
	    fieldoffset = 0;

	  if (offset != 0)
	    vi = first_or_preceding_vi_for_offset (vi, fieldoffset);

	  bitmap_set_bit (result, vi->id);
	  /* If the result is not exactly at fieldoffset include the next
	     field as well.  See get_constraint_for_ptr_offset for more
	     rationale.  */
	  if (vi->offset != fieldoffset
	      && vi->next != NULL)
	    bitmap_set_bit (result, vi->next->id);
	}
    }

  bitmap_copy (set, result);
  BITMAP_FREE (result);
}

/* Union solution sets TO and FROM, and add INC to each member of FROM in the
   process.  */

static bool
set_union_with_increment  (bitmap to, bitmap from, HOST_WIDE_INT inc)
{
  if (inc == 0)
    return bitmap_ior_into (to, from);
  else
    {
      bitmap tmp;
      bool res;

      tmp = BITMAP_ALLOC (&iteration_obstack);
      bitmap_copy (tmp, from);
      solution_set_add (tmp, inc);
      res = bitmap_ior_into (to, tmp);
      BITMAP_FREE (tmp);
      return res;
    }
}

/* Insert constraint C into the list of complex constraints for graph
   node VAR.  */

static void
insert_into_complex (constraint_graph_t graph,
		     unsigned int var, constraint_t c)
{
  VEC (constraint_t, heap) *complex = graph->complex[var];
  unsigned int place = VEC_lower_bound (constraint_t, complex, c,
					constraint_less);

  /* Only insert constraints that do not already exist.  */
  if (place >= VEC_length (constraint_t, complex)
      || !constraint_equal (*c, *VEC_index (constraint_t, complex, place)))
    VEC_safe_insert (constraint_t, heap, graph->complex[var], place, c);
}


/* Condense two variable nodes into a single variable node, by moving
   all associated info from SRC to TO.  */

static void
merge_node_constraints (constraint_graph_t graph, unsigned int to,
			unsigned int from)
{
  unsigned int i;
  constraint_t c;

  gcc_assert (find (from) == to);

  /* Move all complex constraints from src node into to node  */
  for (i = 0; VEC_iterate (constraint_t, graph->complex[from], i, c); i++)
    {
      /* In complex constraints for node src, we may have either
	 a = *src, and *src = a, or an offseted constraint which are
	 always added to the rhs node's constraints.  */

      if (c->rhs.type == DEREF)
	c->rhs.var = to;
      else if (c->lhs.type == DEREF)
	c->lhs.var = to;
      else
	c->rhs.var = to;
    }
  constraint_set_union (&graph->complex[to], &graph->complex[from]);
  VEC_free (constraint_t, heap, graph->complex[from]);
  graph->complex[from] = NULL;
}


/* Remove edges involving NODE from GRAPH.  */

static void
clear_edges_for_node (constraint_graph_t graph, unsigned int node)
{
  if (graph->succs[node])
    BITMAP_FREE (graph->succs[node]);
}

/* Merge GRAPH nodes FROM and TO into node TO.  */

static void
merge_graph_nodes (constraint_graph_t graph, unsigned int to,
		   unsigned int from)
{
  if (graph->indirect_cycles[from] != -1)
    {
      /* If we have indirect cycles with the from node, and we have
	 none on the to node, the to node has indirect cycles from the
	 from node now that they are unified.
	 If indirect cycles exist on both, unify the nodes that they
	 are in a cycle with, since we know they are in a cycle with
	 each other.  */
      if (graph->indirect_cycles[to] == -1)
	graph->indirect_cycles[to] = graph->indirect_cycles[from];
    }

  /* Merge all the successor edges.  */
  if (graph->succs[from])
    {
      if (!graph->succs[to])
	graph->succs[to] = BITMAP_ALLOC (&pta_obstack);
      bitmap_ior_into (graph->succs[to],
		       graph->succs[from]);
    }

  clear_edges_for_node (graph, from);
}


/* Add an indirect graph edge to GRAPH, going from TO to FROM if
   it doesn't exist in the graph already.  */

static void
add_implicit_graph_edge (constraint_graph_t graph, unsigned int to,
			 unsigned int from)
{
  if (to == from)
    return;

  if (!graph->implicit_preds[to])
    graph->implicit_preds[to] = BITMAP_ALLOC (&predbitmap_obstack);

  if (bitmap_set_bit (graph->implicit_preds[to], from))
    stats.num_implicit_edges++;
}

/* Add a predecessor graph edge to GRAPH, going from TO to FROM if
   it doesn't exist in the graph already.
   Return false if the edge already existed, true otherwise.  */

static void
add_pred_graph_edge (constraint_graph_t graph, unsigned int to,
		     unsigned int from)
{
  if (!graph->preds[to])
    graph->preds[to] = BITMAP_ALLOC (&predbitmap_obstack);
  bitmap_set_bit (graph->preds[to], from);
}

/* Add a graph edge to GRAPH, going from FROM to TO if
   it doesn't exist in the graph already.
   Return false if the edge already existed, true otherwise.  */

static bool
add_graph_edge (constraint_graph_t graph, unsigned int to,
		unsigned int from)
{
  if (to == from)
    {
      return false;
    }
  else
    {
      bool r = false;

      if (!graph->succs[from])
	graph->succs[from] = BITMAP_ALLOC (&pta_obstack);
      if (bitmap_set_bit (graph->succs[from], to))
	{
	  r = true;
	  if (to < FIRST_REF_NODE && from < FIRST_REF_NODE)
	    stats.num_edges++;
	}
      return r;
    }
}


/* Return true if {DEST.SRC} is an existing graph edge in GRAPH.  */

static bool
valid_graph_edge (constraint_graph_t graph, unsigned int src,
		  unsigned int dest)
{
  return (graph->succs[dest]
	  && bitmap_bit_p (graph->succs[dest], src));
}

/* Initialize the constraint graph structure to contain SIZE nodes.  */

static void
init_graph (unsigned int size)
{
  unsigned int j;

  graph = XCNEW (struct constraint_graph);
  graph->size = size;
  graph->succs = XCNEWVEC (bitmap, graph->size);
  graph->indirect_cycles = XNEWVEC (int, graph->size);
  graph->rep = XNEWVEC (unsigned int, graph->size);
  graph->complex = XCNEWVEC (VEC(constraint_t, heap) *, size);
  graph->pe = XCNEWVEC (unsigned int, graph->size);
  graph->pe_rep = XNEWVEC (int, graph->size);

  for (j = 0; j < graph->size; j++)
    {
      graph->rep[j] = j;
      graph->pe_rep[j] = -1;
      graph->indirect_cycles[j] = -1;
    }
}

/* Build the constraint graph, adding only predecessor edges right now.  */

static void
build_pred_graph (void)
{
  int i;
  constraint_t c;
  unsigned int j;

  graph->implicit_preds = XCNEWVEC (bitmap, graph->size);
  graph->preds = XCNEWVEC (bitmap, graph->size);
  graph->pointer_label = XCNEWVEC (unsigned int, graph->size);
  graph->loc_label = XCNEWVEC (unsigned int, graph->size);
  graph->pointed_by = XCNEWVEC (bitmap, graph->size);
  graph->points_to = XCNEWVEC (bitmap, graph->size);
  graph->eq_rep = XNEWVEC (int, graph->size);
  graph->direct_nodes = sbitmap_alloc (graph->size);
  graph->address_taken = BITMAP_ALLOC (&predbitmap_obstack);
  sbitmap_zero (graph->direct_nodes);

  for (j = 0; j < FIRST_REF_NODE; j++)
    {
      if (!get_varinfo (j)->is_special_var)
	SET_BIT (graph->direct_nodes, j);
    }

  for (j = 0; j < graph->size; j++)
    graph->eq_rep[j] = -1;

  for (j = 0; j < VEC_length (varinfo_t, varmap); j++)
    graph->indirect_cycles[j] = -1;

  for (i = 0; VEC_iterate (constraint_t, constraints, i, c); i++)
    {
      struct constraint_expr lhs = c->lhs;
      struct constraint_expr rhs = c->rhs;
      unsigned int lhsvar = lhs.var;
      unsigned int rhsvar = rhs.var;

      if (lhs.type == DEREF)
	{
	  /* *x = y.  */
	  if (rhs.offset == 0 && lhs.offset == 0 && rhs.type == SCALAR)
	    add_pred_graph_edge (graph, FIRST_REF_NODE + lhsvar, rhsvar);
	}
      else if (rhs.type == DEREF)
	{
	  /* x = *y */
	  if (rhs.offset == 0 && lhs.offset == 0 && lhs.type == SCALAR)
	    add_pred_graph_edge (graph, lhsvar, FIRST_REF_NODE + rhsvar);
	  else
	    RESET_BIT (graph->direct_nodes, lhsvar);
	}
      else if (rhs.type == ADDRESSOF)
	{
	  varinfo_t v;

	  /* x = &y */
	  if (graph->points_to[lhsvar] == NULL)
	    graph->points_to[lhsvar] = BITMAP_ALLOC (&predbitmap_obstack);
	  bitmap_set_bit (graph->points_to[lhsvar], rhsvar);

	  if (graph->pointed_by[rhsvar] == NULL)
	    graph->pointed_by[rhsvar] = BITMAP_ALLOC (&predbitmap_obstack);
	  bitmap_set_bit (graph->pointed_by[rhsvar], lhsvar);

	  /* Implicitly, *x = y */
	  add_implicit_graph_edge (graph, FIRST_REF_NODE + lhsvar, rhsvar);

	  /* All related variables are no longer direct nodes.  */
	  RESET_BIT (graph->direct_nodes, rhsvar);
          v = get_varinfo (rhsvar);
          if (!v->is_full_var)
            {
              v = lookup_vi_for_tree (v->decl);
              do
                {
                  RESET_BIT (graph->direct_nodes, v->id);
                  v = v->next;
                }
              while (v != NULL);
            }
	  bitmap_set_bit (graph->address_taken, rhsvar);
	}
      else if (lhsvar > anything_id
	       && lhsvar != rhsvar && lhs.offset == 0 && rhs.offset == 0)
	{
	  /* x = y */
	  add_pred_graph_edge (graph, lhsvar, rhsvar);
	  /* Implicitly, *x = *y */
	  add_implicit_graph_edge (graph, FIRST_REF_NODE + lhsvar,
				   FIRST_REF_NODE + rhsvar);
	}
      else if (lhs.offset != 0 || rhs.offset != 0)
	{
	  if (rhs.offset != 0)
	    RESET_BIT (graph->direct_nodes, lhs.var);
	  else if (lhs.offset != 0)
	    RESET_BIT (graph->direct_nodes, rhs.var);
	}
    }
}

/* Build the constraint graph, adding successor edges.  */

static void
build_succ_graph (void)
{
  unsigned i, t;
  constraint_t c;

  for (i = 0; VEC_iterate (constraint_t, constraints, i, c); i++)
    {
      struct constraint_expr lhs;
      struct constraint_expr rhs;
      unsigned int lhsvar;
      unsigned int rhsvar;

      if (!c)
	continue;

      lhs = c->lhs;
      rhs = c->rhs;
      lhsvar = find (lhs.var);
      rhsvar = find (rhs.var);

      if (lhs.type == DEREF)
	{
	  if (rhs.offset == 0 && lhs.offset == 0 && rhs.type == SCALAR)
	    add_graph_edge (graph, FIRST_REF_NODE + lhsvar, rhsvar);
	}
      else if (rhs.type == DEREF)
	{
	  if (rhs.offset == 0 && lhs.offset == 0 && lhs.type == SCALAR)
	    add_graph_edge (graph, lhsvar, FIRST_REF_NODE + rhsvar);
	}
      else if (rhs.type == ADDRESSOF)
	{
	  /* x = &y */
	  gcc_assert (find (rhs.var) == rhs.var);
	  bitmap_set_bit (get_varinfo (lhsvar)->solution, rhsvar);
	}
      else if (lhsvar > anything_id
	       && lhsvar != rhsvar && lhs.offset == 0 && rhs.offset == 0)
	{
	  add_graph_edge (graph, lhsvar, rhsvar);
	}
    }

  /* Add edges from STOREDANYTHING to all non-direct nodes that can
     receive pointers.  */
  t = find (storedanything_id);
  for (i = integer_id + 1; i < FIRST_REF_NODE; ++i)
    {
      if (!TEST_BIT (graph->direct_nodes, i)
	  && get_varinfo (i)->may_have_pointers)
	add_graph_edge (graph, find (i), t);
    }

  /* Everything stored to ANYTHING also potentially escapes.  */
  add_graph_edge (graph, find (escaped_id), t);
}


/* Changed variables on the last iteration.  */
static unsigned int changed_count;
static sbitmap changed;

/* Strongly Connected Component visitation info.  */

struct scc_info
{
  sbitmap visited;
  sbitmap deleted;
  unsigned int *dfs;
  unsigned int *node_mapping;
  int current_index;
  VEC(unsigned,heap) *scc_stack;
};


/* Recursive routine to find strongly connected components in GRAPH.
   SI is the SCC info to store the information in, and N is the id of current
   graph node we are processing.

   This is Tarjan's strongly connected component finding algorithm, as
   modified by Nuutila to keep only non-root nodes on the stack.
   The algorithm can be found in "On finding the strongly connected
   connected components in a directed graph" by Esko Nuutila and Eljas
   Soisalon-Soininen, in Information Processing Letters volume 49,
   number 1, pages 9-14.  */

static void
scc_visit (constraint_graph_t graph, struct scc_info *si, unsigned int n)
{
  unsigned int i;
  bitmap_iterator bi;
  unsigned int my_dfs;

  SET_BIT (si->visited, n);
  si->dfs[n] = si->current_index ++;
  my_dfs = si->dfs[n];

  /* Visit all the successors.  */
  EXECUTE_IF_IN_NONNULL_BITMAP (graph->succs[n], 0, i, bi)
    {
      unsigned int w;

      if (i > LAST_REF_NODE)
	break;

      w = find (i);
      if (TEST_BIT (si->deleted, w))
	continue;

      if (!TEST_BIT (si->visited, w))
	scc_visit (graph, si, w);
      {
	unsigned int t = find (w);
	unsigned int nnode = find (n);
	gcc_assert (nnode == n);

	if (si->dfs[t] < si->dfs[nnode])
	  si->dfs[n] = si->dfs[t];
      }
    }

  /* See if any components have been identified.  */
  if (si->dfs[n] == my_dfs)
    {
      if (VEC_length (unsigned, si->scc_stack) > 0
	  && si->dfs[VEC_last (unsigned, si->scc_stack)] >= my_dfs)
	{
	  bitmap scc = BITMAP_ALLOC (NULL);
	  unsigned int lowest_node;
	  bitmap_iterator bi;

	  bitmap_set_bit (scc, n);

	  while (VEC_length (unsigned, si->scc_stack) != 0
		 && si->dfs[VEC_last (unsigned, si->scc_stack)] >= my_dfs)
	    {
	      unsigned int w = VEC_pop (unsigned, si->scc_stack);

	      bitmap_set_bit (scc, w);
	    }

	  lowest_node = bitmap_first_set_bit (scc);
	  gcc_assert (lowest_node < FIRST_REF_NODE);

	  /* Collapse the SCC nodes into a single node, and mark the
	     indirect cycles.  */
	  EXECUTE_IF_SET_IN_BITMAP (scc, 0, i, bi)
	    {
	      if (i < FIRST_REF_NODE)
		{
		  if (unite (lowest_node, i))
		    unify_nodes (graph, lowest_node, i, false);
		}
	      else
		{
		  unite (lowest_node, i);
		  graph->indirect_cycles[i - FIRST_REF_NODE] = lowest_node;
		}
	    }
	}
      SET_BIT (si->deleted, n);
    }
  else
    VEC_safe_push (unsigned, heap, si->scc_stack, n);
}

/* Unify node FROM into node TO, updating the changed count if
   necessary when UPDATE_CHANGED is true.  */

static void
unify_nodes (constraint_graph_t graph, unsigned int to, unsigned int from,
	     bool update_changed)
{

  gcc_assert (to != from && find (to) == to);
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Unifying %s to %s\n",
	     get_varinfo (from)->name,
	     get_varinfo (to)->name);

  if (update_changed)
    stats.unified_vars_dynamic++;
  else
    stats.unified_vars_static++;

  merge_graph_nodes (graph, to, from);
  merge_node_constraints (graph, to, from);

  /* Mark TO as changed if FROM was changed. If TO was already marked
     as changed, decrease the changed count.  */

  if (update_changed && TEST_BIT (changed, from))
    {
      RESET_BIT (changed, from);
      if (!TEST_BIT (changed, to))
	SET_BIT (changed, to);
      else
	{
	  gcc_assert (changed_count > 0);
	  changed_count--;
	}
    }
  if (get_varinfo (from)->solution)
    {
      /* If the solution changes because of the merging, we need to mark
	 the variable as changed.  */
      if (bitmap_ior_into (get_varinfo (to)->solution,
			   get_varinfo (from)->solution))
	{
	  if (update_changed && !TEST_BIT (changed, to))
	    {
	      SET_BIT (changed, to);
	      changed_count++;
	    }
	}

      BITMAP_FREE (get_varinfo (from)->solution);
      BITMAP_FREE (get_varinfo (from)->oldsolution);

      if (stats.iterations > 0)
	{
	  BITMAP_FREE (get_varinfo (to)->oldsolution);
	  get_varinfo (to)->oldsolution = BITMAP_ALLOC (&oldpta_obstack);
	}
    }
  if (valid_graph_edge (graph, to, to))
    {
      if (graph->succs[to])
	bitmap_clear_bit (graph->succs[to], to);
    }
}

/* Information needed to compute the topological ordering of a graph.  */

struct topo_info
{
  /* sbitmap of visited nodes.  */
  sbitmap visited;
  /* Array that stores the topological order of the graph, *in
     reverse*.  */
  VEC(unsigned,heap) *topo_order;
};


/* Initialize and return a topological info structure.  */

static struct topo_info *
init_topo_info (void)
{
  size_t size = graph->size;
  struct topo_info *ti = XNEW (struct topo_info);
  ti->visited = sbitmap_alloc (size);
  sbitmap_zero (ti->visited);
  ti->topo_order = VEC_alloc (unsigned, heap, 1);
  return ti;
}


/* Free the topological sort info pointed to by TI.  */

static void
free_topo_info (struct topo_info *ti)
{
  sbitmap_free (ti->visited);
  VEC_free (unsigned, heap, ti->topo_order);
  free (ti);
}

/* Visit the graph in topological order, and store the order in the
   topo_info structure.  */

static void
topo_visit (constraint_graph_t graph, struct topo_info *ti,
	    unsigned int n)
{
  bitmap_iterator bi;
  unsigned int j;

  SET_BIT (ti->visited, n);

  if (graph->succs[n])
    EXECUTE_IF_SET_IN_BITMAP (graph->succs[n], 0, j, bi)
      {
	if (!TEST_BIT (ti->visited, j))
	  topo_visit (graph, ti, j);
      }

  VEC_safe_push (unsigned, heap, ti->topo_order, n);
}

/* Process a constraint C that represents x = *(y + off), using DELTA as the
   starting solution for y.  */

static void
do_sd_constraint (constraint_graph_t graph, constraint_t c,
		  bitmap delta)
{
  unsigned int lhs = c->lhs.var;
  bool flag = false;
  bitmap sol = get_varinfo (lhs)->solution;
  unsigned int j;
  bitmap_iterator bi;
  HOST_WIDE_INT roffset = c->rhs.offset;

  /* Our IL does not allow this.  */
  gcc_assert (c->lhs.offset == 0);

  /* If the solution of Y contains anything it is good enough to transfer
     this to the LHS.  */
  if (bitmap_bit_p (delta, anything_id))
    {
      flag |= bitmap_set_bit (sol, anything_id);
      goto done;
    }

  /* If we do not know at with offset the rhs is dereferenced compute
     the reachability set of DELTA, conservatively assuming it is
     dereferenced at all valid offsets.  */
  if (roffset == UNKNOWN_OFFSET)
    {
      solution_set_expand (delta, delta);
      /* No further offset processing is necessary.  */
      roffset = 0;
    }

  /* For each variable j in delta (Sol(y)), add
     an edge in the graph from j to x, and union Sol(j) into Sol(x).  */
  EXECUTE_IF_SET_IN_BITMAP (delta, 0, j, bi)
    {
      varinfo_t v = get_varinfo (j);
      HOST_WIDE_INT fieldoffset = v->offset + roffset;
      unsigned int t;

      if (v->is_full_var)
	fieldoffset = v->offset;
      else if (roffset != 0)
	v = first_vi_for_offset (v, fieldoffset);
      /* If the access is outside of the variable we can ignore it.  */
      if (!v)
	continue;

      do
	{
	  t = find (v->id);

	  /* Adding edges from the special vars is pointless.
	     They don't have sets that can change.  */
	  if (get_varinfo (t)->is_special_var)
	    flag |= bitmap_ior_into (sol, get_varinfo (t)->solution);
	  /* Merging the solution from ESCAPED needlessly increases
	     the set.  Use ESCAPED as representative instead.  */
	  else if (v->id == escaped_id)
	    flag |= bitmap_set_bit (sol, escaped_id);
	  else if (add_graph_edge (graph, lhs, t))
	    flag |= bitmap_ior_into (sol, get_varinfo (t)->solution);

	  /* If the variable is not exactly at the requested offset
	     we have to include the next one.  */
	  if (v->offset == (unsigned HOST_WIDE_INT)fieldoffset
	      || v->next == NULL)
	    break;

	  v = v->next;
	  fieldoffset = v->offset;
	}
      while (1);
    }

done:
  /* If the LHS solution changed, mark the var as changed.  */
  if (flag)
    {
      get_varinfo (lhs)->solution = sol;
      if (!TEST_BIT (changed, lhs))
	{
	  SET_BIT (changed, lhs);
	  changed_count++;
	}
    }
}

/* Process a constraint C that represents *(x + off) = y using DELTA
   as the starting solution for x.  */

static void
do_ds_constraint (constraint_t c, bitmap delta)
{
  unsigned int rhs = c->rhs.var;
  bitmap sol = get_varinfo (rhs)->solution;
  unsigned int j;
  bitmap_iterator bi;
  HOST_WIDE_INT loff = c->lhs.offset;

  /* Our IL does not allow this.  */
  gcc_assert (c->rhs.offset == 0);

  /* If the solution of y contains ANYTHING simply use the ANYTHING
     solution.  This avoids needlessly increasing the points-to sets.  */
  if (bitmap_bit_p (sol, anything_id))
    sol = get_varinfo (find (anything_id))->solution;

  /* If the solution for x contains ANYTHING we have to merge the
     solution of y into all pointer variables which we do via
     STOREDANYTHING.  */
  if (bitmap_bit_p (delta, anything_id))
    {
      unsigned t = find (storedanything_id);
      if (add_graph_edge (graph, t, rhs))
	{
	  if (bitmap_ior_into (get_varinfo (t)->solution, sol))
	    {
	      if (!TEST_BIT (changed, t))
		{
		  SET_BIT (changed, t);
		  changed_count++;
		}
	    }
	}
      return;
    }

  /* If we do not know at with offset the rhs is dereferenced compute
     the reachability set of DELTA, conservatively assuming it is
     dereferenced at all valid offsets.  */
  if (loff == UNKNOWN_OFFSET)
    {
      solution_set_expand (delta, delta);
      loff = 0;
    }

  /* For each member j of delta (Sol(x)), add an edge from y to j and
     union Sol(y) into Sol(j) */
  EXECUTE_IF_SET_IN_BITMAP (delta, 0, j, bi)
    {
      varinfo_t v = get_varinfo (j);
      unsigned int t;
      HOST_WIDE_INT fieldoffset = v->offset + loff;

      /* If v is a global variable then this is an escape point.  */
      if (v->is_global_var)
	{
	  t = find (escaped_id);
	  if (add_graph_edge (graph, t, rhs)
	      && bitmap_ior_into (get_varinfo (t)->solution, sol)
	      && !TEST_BIT (changed, t))
	    {
	      SET_BIT (changed, t);
	      changed_count++;
	    }
	}

      if (v->is_special_var)
	continue;

      if (v->is_full_var)
	fieldoffset = v->offset;
      else if (loff != 0)
	v = first_vi_for_offset (v, fieldoffset);
      /* If the access is outside of the variable we can ignore it.  */
      if (!v)
	continue;

      do
	{
	  if (v->may_have_pointers)
	    {
	      t = find (v->id);
	      if (add_graph_edge (graph, t, rhs)
		  && bitmap_ior_into (get_varinfo (t)->solution, sol)
		  && !TEST_BIT (changed, t))
		{
		  SET_BIT (changed, t);
		  changed_count++;
		}
	    }

	  /* If the variable is not exactly at the requested offset
	     we have to include the next one.  */
	  if (v->offset == (unsigned HOST_WIDE_INT)fieldoffset
	      || v->next == NULL)
	    break;

	  v = v->next;
	  fieldoffset = v->offset;
	}
      while (1);
    }
}

/* Handle a non-simple (simple meaning requires no iteration),
   constraint (IE *x = &y, x = *y, *x = y, and x = y with offsets involved).  */

static void
do_complex_constraint (constraint_graph_t graph, constraint_t c, bitmap delta)
{
  if (c->lhs.type == DEREF)
    {
      if (c->rhs.type == ADDRESSOF)
	{
	  gcc_unreachable();
	}
      else
	{
	  /* *x = y */
	  do_ds_constraint (c, delta);
	}
    }
  else if (c->rhs.type == DEREF)
    {
      /* x = *y */
      if (!(get_varinfo (c->lhs.var)->is_special_var))
	do_sd_constraint (graph, c, delta);
    }
  else
    {
      bitmap tmp;
      bitmap solution;
      bool flag = false;

      gcc_assert (c->rhs.type == SCALAR && c->lhs.type == SCALAR);
      solution = get_varinfo (c->rhs.var)->solution;
      tmp = get_varinfo (c->lhs.var)->solution;

      flag = set_union_with_increment (tmp, solution, c->rhs.offset);

      if (flag)
	{
	  get_varinfo (c->lhs.var)->solution = tmp;
	  if (!TEST_BIT (changed, c->lhs.var))
	    {
	      SET_BIT (changed, c->lhs.var);
	      changed_count++;
	    }
	}
    }
}

/* Initialize and return a new SCC info structure.  */

static struct scc_info *
init_scc_info (size_t size)
{
  struct scc_info *si = XNEW (struct scc_info);
  size_t i;

  si->current_index = 0;
  si->visited = sbitmap_alloc (size);
  sbitmap_zero (si->visited);
  si->deleted = sbitmap_alloc (size);
  sbitmap_zero (si->deleted);
  si->node_mapping = XNEWVEC (unsigned int, size);
  si->dfs = XCNEWVEC (unsigned int, size);

  for (i = 0; i < size; i++)
    si->node_mapping[i] = i;

  si->scc_stack = VEC_alloc (unsigned, heap, 1);
  return si;
}

/* Free an SCC info structure pointed to by SI */

static void
free_scc_info (struct scc_info *si)
{
  sbitmap_free (si->visited);
  sbitmap_free (si->deleted);
  free (si->node_mapping);
  free (si->dfs);
  VEC_free (unsigned, heap, si->scc_stack);
  free (si);
}


/* Find indirect cycles in GRAPH that occur, using strongly connected
   components, and note them in the indirect cycles map.

   This technique comes from Ben Hardekopf and Calvin Lin,
   "It Pays to be Lazy: Fast and Accurate Pointer Analysis for Millions of
   Lines of Code", submitted to PLDI 2007.  */

static void
find_indirect_cycles (constraint_graph_t graph)
{
  unsigned int i;
  unsigned int size = graph->size;
  struct scc_info *si = init_scc_info (size);

  for (i = 0; i < MIN (LAST_REF_NODE, size); i ++ )
    if (!TEST_BIT (si->visited, i) && find (i) == i)
      scc_visit (graph, si, i);

  free_scc_info (si);
}

/* Compute a topological ordering for GRAPH, and store the result in the
   topo_info structure TI.  */

static void
compute_topo_order (constraint_graph_t graph,
		    struct topo_info *ti)
{
  unsigned int i;
  unsigned int size = graph->size;

  for (i = 0; i != size; ++i)
    if (!TEST_BIT (ti->visited, i) && find (i) == i)
      topo_visit (graph, ti, i);
}

/* Structure used to for hash value numbering of pointer equivalence
   classes.  */

typedef struct equiv_class_label
{
  hashval_t hashcode;
  unsigned int equivalence_class;
  bitmap labels;
} *equiv_class_label_t;
typedef const struct equiv_class_label *const_equiv_class_label_t;

/* A hashtable for mapping a bitmap of labels->pointer equivalence
   classes.  */
static htab_t pointer_equiv_class_table;

/* A hashtable for mapping a bitmap of labels->location equivalence
   classes.  */
static htab_t location_equiv_class_table;

/* Hash function for a equiv_class_label_t */

static hashval_t
equiv_class_label_hash (const void *p)
{
  const_equiv_class_label_t const ecl = (const_equiv_class_label_t) p;
  return ecl->hashcode;
}

/* Equality function for two equiv_class_label_t's.  */

static int
equiv_class_label_eq (const void *p1, const void *p2)
{
  const_equiv_class_label_t const eql1 = (const_equiv_class_label_t) p1;
  const_equiv_class_label_t const eql2 = (const_equiv_class_label_t) p2;
  return (eql1->hashcode == eql2->hashcode
	  && bitmap_equal_p (eql1->labels, eql2->labels));
}

/* Lookup a equivalence class in TABLE by the bitmap of LABELS it
   contains.  */

static unsigned int
equiv_class_lookup (htab_t table, bitmap labels)
{
  void **slot;
  struct equiv_class_label ecl;

  ecl.labels = labels;
  ecl.hashcode = bitmap_hash (labels);

  slot = htab_find_slot_with_hash (table, &ecl,
				   ecl.hashcode, NO_INSERT);
  if (!slot)
    return 0;
  else
    return ((equiv_class_label_t) *slot)->equivalence_class;
}


/* Add an equivalence class named EQUIVALENCE_CLASS with labels LABELS
   to TABLE.  */

static void
equiv_class_add (htab_t table, unsigned int equivalence_class,
		 bitmap labels)
{
  void **slot;
  equiv_class_label_t ecl = XNEW (struct equiv_class_label);

  ecl->labels = labels;
  ecl->equivalence_class = equivalence_class;
  ecl->hashcode = bitmap_hash (labels);

  slot = htab_find_slot_with_hash (table, ecl,
				   ecl->hashcode, INSERT);
  gcc_assert (!*slot);
  *slot = (void *) ecl;
}

/* Perform offline variable substitution.

   This is a worst case quadratic time way of identifying variables
   that must have equivalent points-to sets, including those caused by
   static cycles, and single entry subgraphs, in the constraint graph.

   The technique is described in "Exploiting Pointer and Location
   Equivalence to Optimize Pointer Analysis. In the 14th International
   Static Analysis Symposium (SAS), August 2007."  It is known as the
   "HU" algorithm, and is equivalent to value numbering the collapsed
   constraint graph including evaluating unions.

   The general method of finding equivalence classes is as follows:
   Add fake nodes (REF nodes) and edges for *a = b and a = *b constraints.
   Initialize all non-REF nodes to be direct nodes.
   For each constraint a = a U {b}, we set pts(a) = pts(a) u {fresh
   variable}
   For each constraint containing the dereference, we also do the same
   thing.

   We then compute SCC's in the graph and unify nodes in the same SCC,
   including pts sets.

   For each non-collapsed node x:
    Visit all unvisited explicit incoming edges.
    Ignoring all non-pointers, set pts(x) = Union of pts(a) for y
    where y->x.
    Lookup the equivalence class for pts(x).
     If we found one, equivalence_class(x) = found class.
     Otherwise, equivalence_class(x) = new class, and new_class is
    added to the lookup table.

   All direct nodes with the same equivalence class can be replaced
   with a single representative node.
   All unlabeled nodes (label == 0) are not pointers and all edges
   involving them can be eliminated.
   We perform these optimizations during rewrite_constraints

   In addition to pointer equivalence class finding, we also perform
   location equivalence class finding.  This is the set of variables
   that always appear together in points-to sets.  We use this to
   compress the size of the points-to sets.  */

/* Current maximum pointer equivalence class id.  */
static int pointer_equiv_class;

/* Current maximum location equivalence class id.  */
static int location_equiv_class;

/* Recursive routine to find strongly connected components in GRAPH,
   and label it's nodes with DFS numbers.  */

static void
condense_visit (constraint_graph_t graph, struct scc_info *si, unsigned int n)
{
  unsigned int i;
  bitmap_iterator bi;
  unsigned int my_dfs;

  gcc_assert (si->node_mapping[n] == n);
  SET_BIT (si->visited, n);
  si->dfs[n] = si->current_index ++;
  my_dfs = si->dfs[n];

  /* Visit all the successors.  */
  EXECUTE_IF_IN_NONNULL_BITMAP (graph->preds[n], 0, i, bi)
    {
      unsigned int w = si->node_mapping[i];

      if (TEST_BIT (si->deleted, w))
	continue;

      if (!TEST_BIT (si->visited, w))
	condense_visit (graph, si, w);
      {
	unsigned int t = si->node_mapping[w];
	unsigned int nnode = si->node_mapping[n];
	gcc_assert (nnode == n);

	if (si->dfs[t] < si->dfs[nnode])
	  si->dfs[n] = si->dfs[t];
      }
    }

  /* Visit all the implicit predecessors.  */
  EXECUTE_IF_IN_NONNULL_BITMAP (graph->implicit_preds[n], 0, i, bi)
    {
      unsigned int w = si->node_mapping[i];

      if (TEST_BIT (si->deleted, w))
	continue;

      if (!TEST_BIT (si->visited, w))
	condense_visit (graph, si, w);
      {
	unsigned int t = si->node_mapping[w];
	unsigned int nnode = si->node_mapping[n];
	gcc_assert (nnode == n);

	if (si->dfs[t] < si->dfs[nnode])
	  si->dfs[n] = si->dfs[t];
      }
    }

  /* See if any components have been identified.  */
  if (si->dfs[n] == my_dfs)
    {
      while (VEC_length (unsigned, si->scc_stack) != 0
	     && si->dfs[VEC_last (unsigned, si->scc_stack)] >= my_dfs)
	{
	  unsigned int w = VEC_pop (unsigned, si->scc_stack);
	  si->node_mapping[w] = n;

	  if (!TEST_BIT (graph->direct_nodes, w))
	    RESET_BIT (graph->direct_nodes, n);

	  /* Unify our nodes.  */
	  if (graph->preds[w])
	    {
	      if (!graph->preds[n])
		graph->preds[n] = BITMAP_ALLOC (&predbitmap_obstack);
	      bitmap_ior_into (graph->preds[n], graph->preds[w]);
	    }
	  if (graph->implicit_preds[w])
	    {
	      if (!graph->implicit_preds[n])
		graph->implicit_preds[n] = BITMAP_ALLOC (&predbitmap_obstack);
	      bitmap_ior_into (graph->implicit_preds[n],
			       graph->implicit_preds[w]);
	    }
	  if (graph->points_to[w])
	    {
	      if (!graph->points_to[n])
		graph->points_to[n] = BITMAP_ALLOC (&predbitmap_obstack);
	      bitmap_ior_into (graph->points_to[n],
			       graph->points_to[w]);
	    }
	}
      SET_BIT (si->deleted, n);
    }
  else
    VEC_safe_push (unsigned, heap, si->scc_stack, n);
}

/* Label pointer equivalences.  */

static void
label_visit (constraint_graph_t graph, struct scc_info *si, unsigned int n)
{
  unsigned int i;
  bitmap_iterator bi;
  SET_BIT (si->visited, n);

  if (!graph->points_to[n])
    graph->points_to[n] = BITMAP_ALLOC (&predbitmap_obstack);

  /* Label and union our incoming edges's points to sets.  */
  EXECUTE_IF_IN_NONNULL_BITMAP (graph->preds[n], 0, i, bi)
    {
      unsigned int w = si->node_mapping[i];
      if (!TEST_BIT (si->visited, w))
	label_visit (graph, si, w);

      /* Skip unused edges  */
      if (w == n || graph->pointer_label[w] == 0)
	continue;

      if (graph->points_to[w])
	bitmap_ior_into(graph->points_to[n], graph->points_to[w]);
    }
  /* Indirect nodes get fresh variables.  */
  if (!TEST_BIT (graph->direct_nodes, n))
    bitmap_set_bit (graph->points_to[n], FIRST_REF_NODE + n);

  if (!bitmap_empty_p (graph->points_to[n]))
    {
      unsigned int label = equiv_class_lookup (pointer_equiv_class_table,
					       graph->points_to[n]);
      if (!label)
	{
	  label = pointer_equiv_class++;
	  equiv_class_add (pointer_equiv_class_table,
			   label, graph->points_to[n]);
	}
      graph->pointer_label[n] = label;
    }
}

/* Perform offline variable substitution, discovering equivalence
   classes, and eliminating non-pointer variables.  */

static struct scc_info *
perform_var_substitution (constraint_graph_t graph)
{
  unsigned int i;
  unsigned int size = graph->size;
  struct scc_info *si = init_scc_info (size);

  bitmap_obstack_initialize (&iteration_obstack);
  pointer_equiv_class_table = htab_create (511, equiv_class_label_hash,
					   equiv_class_label_eq, free);
  location_equiv_class_table = htab_create (511, equiv_class_label_hash,
					    equiv_class_label_eq, free);
  pointer_equiv_class = 1;
  location_equiv_class = 1;

  /* Condense the nodes, which means to find SCC's, count incoming
     predecessors, and unite nodes in SCC's.  */
  for (i = 0; i < FIRST_REF_NODE; i++)
    if (!TEST_BIT (si->visited, si->node_mapping[i]))
      condense_visit (graph, si, si->node_mapping[i]);

  sbitmap_zero (si->visited);
  /* Actually the label the nodes for pointer equivalences  */
  for (i = 0; i < FIRST_REF_NODE; i++)
    if (!TEST_BIT (si->visited, si->node_mapping[i]))
      label_visit (graph, si, si->node_mapping[i]);

  /* Calculate location equivalence labels.  */
  for (i = 0; i < FIRST_REF_NODE; i++)
    {
      bitmap pointed_by;
      bitmap_iterator bi;
      unsigned int j;
      unsigned int label;

      if (!graph->pointed_by[i])
	continue;
      pointed_by = BITMAP_ALLOC (&iteration_obstack);

      /* Translate the pointed-by mapping for pointer equivalence
	 labels.  */
      EXECUTE_IF_SET_IN_BITMAP (graph->pointed_by[i], 0, j, bi)
	{
	  bitmap_set_bit (pointed_by,
			  graph->pointer_label[si->node_mapping[j]]);
	}
      /* The original pointed_by is now dead.  */
      BITMAP_FREE (graph->pointed_by[i]);

      /* Look up the location equivalence label if one exists, or make
	 one otherwise.  */
      label = equiv_class_lookup (location_equiv_class_table,
				  pointed_by);
      if (label == 0)
	{
	  label = location_equiv_class++;
	  equiv_class_add (location_equiv_class_table,
			   label, pointed_by);
	}
      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Found location equivalence for node %s\n",
		     get_varinfo (i)->name);
	  BITMAP_FREE (pointed_by);
	}
      graph->loc_label[i] = label;

    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    for (i = 0; i < FIRST_REF_NODE; i++)
      {
	bool direct_node = TEST_BIT (graph->direct_nodes, i);
	fprintf (dump_file,
		 "Equivalence classes for %s node id %d:%s are pointer: %d"
		 ", location:%d\n",
		 direct_node ? "Direct node" : "Indirect node", i,
		 get_varinfo (i)->name,
		 graph->pointer_label[si->node_mapping[i]],
		 graph->loc_label[si->node_mapping[i]]);
      }

  /* Quickly eliminate our non-pointer variables.  */

  for (i = 0; i < FIRST_REF_NODE; i++)
    {
      unsigned int node = si->node_mapping[i];

      if (graph->pointer_label[node] == 0)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "%s is a non-pointer variable, eliminating edges.\n",
		     get_varinfo (node)->name);
	  stats.nonpointer_vars++;
	  clear_edges_for_node (graph, node);
	}
    }

  return si;
}

/* Free information that was only necessary for variable
   substitution.  */

static void
free_var_substitution_info (struct scc_info *si)
{
  free_scc_info (si);
  free (graph->pointer_label);
  free (graph->loc_label);
  free (graph->pointed_by);
  free (graph->points_to);
  free (graph->eq_rep);
  sbitmap_free (graph->direct_nodes);
  htab_delete (pointer_equiv_class_table);
  htab_delete (location_equiv_class_table);
  bitmap_obstack_release (&iteration_obstack);
}

/* Return an existing node that is equivalent to NODE, which has
   equivalence class LABEL, if one exists.  Return NODE otherwise.  */

static unsigned int
find_equivalent_node (constraint_graph_t graph,
		      unsigned int node, unsigned int label)
{
  /* If the address version of this variable is unused, we can
     substitute it for anything else with the same label.
     Otherwise, we know the pointers are equivalent, but not the
     locations, and we can unite them later.  */

  if (!bitmap_bit_p (graph->address_taken, node))
    {
      gcc_assert (label < graph->size);

      if (graph->eq_rep[label] != -1)
	{
	  /* Unify the two variables since we know they are equivalent.  */
	  if (unite (graph->eq_rep[label], node))
	    unify_nodes (graph, graph->eq_rep[label], node, false);
	  return graph->eq_rep[label];
	}
      else
	{
	  graph->eq_rep[label] = node;
	  graph->pe_rep[label] = node;
	}
    }
  else
    {
      gcc_assert (label < graph->size);
      graph->pe[node] = label;
      if (graph->pe_rep[label] == -1)
	graph->pe_rep[label] = node;
    }

  return node;
}

/* Unite pointer equivalent but not location equivalent nodes in
   GRAPH.  This may only be performed once variable substitution is
   finished.  */

static void
unite_pointer_equivalences (constraint_graph_t graph)
{
  unsigned int i;

  /* Go through the pointer equivalences and unite them to their
     representative, if they aren't already.  */
  for (i = 0; i < FIRST_REF_NODE; i++)
    {
      unsigned int label = graph->pe[i];
      if (label)
	{
	  int label_rep = graph->pe_rep[label];

	  if (label_rep == -1)
	    continue;

	  label_rep = find (label_rep);
	  if (label_rep >= 0 && unite (label_rep, find (i)))
	    unify_nodes (graph, label_rep, i, false);
	}
    }
}

/* Move complex constraints to the GRAPH nodes they belong to.  */

static void
move_complex_constraints (constraint_graph_t graph)
{
  int i;
  constraint_t c;

  for (i = 0; VEC_iterate (constraint_t, constraints, i, c); i++)
    {
      if (c)
	{
	  struct constraint_expr lhs = c->lhs;
	  struct constraint_expr rhs = c->rhs;

	  if (lhs.type == DEREF)
	    {
	      insert_into_complex (graph, lhs.var, c);
	    }
	  else if (rhs.type == DEREF)
	    {
	      if (!(get_varinfo (lhs.var)->is_special_var))
		insert_into_complex (graph, rhs.var, c);
	    }
	  else if (rhs.type != ADDRESSOF && lhs.var > anything_id
		   && (lhs.offset != 0 || rhs.offset != 0))
	    {
	      insert_into_complex (graph, rhs.var, c);
	    }
	}
    }
}


/* Optimize and rewrite complex constraints while performing
   collapsing of equivalent nodes.  SI is the SCC_INFO that is the
   result of perform_variable_substitution.  */

static void
rewrite_constraints (constraint_graph_t graph,
		     struct scc_info *si)
{
  int i;
  unsigned int j;
  constraint_t c;

  for (j = 0; j < graph->size; j++)
    gcc_assert (find (j) == j);

  for (i = 0; VEC_iterate (constraint_t, constraints, i, c); i++)
    {
      struct constraint_expr lhs = c->lhs;
      struct constraint_expr rhs = c->rhs;
      unsigned int lhsvar = find (lhs.var);
      unsigned int rhsvar = find (rhs.var);
      unsigned int lhsnode, rhsnode;
      unsigned int lhslabel, rhslabel;

      lhsnode = si->node_mapping[lhsvar];
      rhsnode = si->node_mapping[rhsvar];
      lhslabel = graph->pointer_label[lhsnode];
      rhslabel = graph->pointer_label[rhsnode];

      /* See if it is really a non-pointer variable, and if so, ignore
	 the constraint.  */
      if (lhslabel == 0)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {

	      fprintf (dump_file, "%s is a non-pointer variable,"
		       "ignoring constraint:",
		       get_varinfo (lhs.var)->name);
	      dump_constraint (dump_file, c);
	    }
	  VEC_replace (constraint_t, constraints, i, NULL);
	  continue;
	}

      if (rhslabel == 0)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {

	      fprintf (dump_file, "%s is a non-pointer variable,"
		       "ignoring constraint:",
		       get_varinfo (rhs.var)->name);
	      dump_constraint (dump_file, c);
	    }
	  VEC_replace (constraint_t, constraints, i, NULL);
	  continue;
	}

      lhsvar = find_equivalent_node (graph, lhsvar, lhslabel);
      rhsvar = find_equivalent_node (graph, rhsvar, rhslabel);
      c->lhs.var = lhsvar;
      c->rhs.var = rhsvar;

    }
}

/* Eliminate indirect cycles involving NODE.  Return true if NODE was
   part of an SCC, false otherwise.  */

static bool
eliminate_indirect_cycles (unsigned int node)
{
  if (graph->indirect_cycles[node] != -1
      && !bitmap_empty_p (get_varinfo (node)->solution))
    {
      unsigned int i;
      VEC(unsigned,heap) *queue = NULL;
      int queuepos;
      unsigned int to = find (graph->indirect_cycles[node]);
      bitmap_iterator bi;

      /* We can't touch the solution set and call unify_nodes
	 at the same time, because unify_nodes is going to do
	 bitmap unions into it. */

      EXECUTE_IF_SET_IN_BITMAP (get_varinfo (node)->solution, 0, i, bi)
	{
	  if (find (i) == i && i != to)
	    {
	      if (unite (to, i))
		VEC_safe_push (unsigned, heap, queue, i);
	    }
	}

      for (queuepos = 0;
	   VEC_iterate (unsigned, queue, queuepos, i);
	   queuepos++)
	{
	  unify_nodes (graph, to, i, true);
	}
      VEC_free (unsigned, heap, queue);
      return true;
    }
  return false;
}

/* Solve the constraint graph GRAPH using our worklist solver.
   This is based on the PW* family of solvers from the "Efficient Field
   Sensitive Pointer Analysis for C" paper.
   It works by iterating over all the graph nodes, processing the complex
   constraints and propagating the copy constraints, until everything stops
   changed.  This corresponds to steps 6-8 in the solving list given above.  */

static void
solve_graph (constraint_graph_t graph)
{
  unsigned int size = graph->size;
  unsigned int i;
  bitmap pts;

  changed_count = 0;
  changed = sbitmap_alloc (size);
  sbitmap_zero (changed);

  /* Mark all initial non-collapsed nodes as changed.  */
  for (i = 0; i < size; i++)
    {
      varinfo_t ivi = get_varinfo (i);
      if (find (i) == i && !bitmap_empty_p (ivi->solution)
	  && ((graph->succs[i] && !bitmap_empty_p (graph->succs[i]))
	      || VEC_length (constraint_t, graph->complex[i]) > 0))
	{
	  SET_BIT (changed, i);
	  changed_count++;
	}
    }

  /* Allocate a bitmap to be used to store the changed bits.  */
  pts = BITMAP_ALLOC (&pta_obstack);

  while (changed_count > 0)
    {
      unsigned int i;
      struct topo_info *ti = init_topo_info ();
      stats.iterations++;

      bitmap_obstack_initialize (&iteration_obstack);

      compute_topo_order (graph, ti);

      while (VEC_length (unsigned, ti->topo_order) != 0)
	{

	  i = VEC_pop (unsigned, ti->topo_order);

	  /* If this variable is not a representative, skip it.  */
	  if (find (i) != i)
	    continue;

	  /* In certain indirect cycle cases, we may merge this
	     variable to another.  */
	  if (eliminate_indirect_cycles (i) && find (i) != i)
	    continue;

	  /* If the node has changed, we need to process the
	     complex constraints and outgoing edges again.  */
	  if (TEST_BIT (changed, i))
	    {
	      unsigned int j;
	      constraint_t c;
	      bitmap solution;
	      VEC(constraint_t,heap) *complex = graph->complex[i];
	      bool solution_empty;

	      RESET_BIT (changed, i);
	      changed_count--;

	      /* Compute the changed set of solution bits.  */
	      bitmap_and_compl (pts, get_varinfo (i)->solution,
				get_varinfo (i)->oldsolution);

	      if (bitmap_empty_p (pts))
		continue;

	      bitmap_ior_into (get_varinfo (i)->oldsolution, pts);

	      solution = get_varinfo (i)->solution;
	      solution_empty = bitmap_empty_p (solution);

	      /* Process the complex constraints */
	      for (j = 0; VEC_iterate (constraint_t, complex, j, c); j++)
		{
		  /* XXX: This is going to unsort the constraints in
		     some cases, which will occasionally add duplicate
		     constraints during unification.  This does not
		     affect correctness.  */
		  c->lhs.var = find (c->lhs.var);
		  c->rhs.var = find (c->rhs.var);

		  /* The only complex constraint that can change our
		     solution to non-empty, given an empty solution,
		     is a constraint where the lhs side is receiving
		     some set from elsewhere.  */
		  if (!solution_empty || c->lhs.type != DEREF)
		    do_complex_constraint (graph, c, pts);
		}

	      solution_empty = bitmap_empty_p (solution);

	      if (!solution_empty)
		{
		  bitmap_iterator bi;
		  unsigned eff_escaped_id = find (escaped_id);

		  /* Propagate solution to all successors.  */
		  EXECUTE_IF_IN_NONNULL_BITMAP (graph->succs[i],
						0, j, bi)
		    {
		      bitmap tmp;
		      bool flag;

		      unsigned int to = find (j);
		      tmp = get_varinfo (to)->solution;
		      flag = false;

		      /* Don't try to propagate to ourselves.  */
		      if (to == i)
			continue;

		      /* If we propagate from ESCAPED use ESCAPED as
		         placeholder.  */
		      if (i == eff_escaped_id)
			flag = bitmap_set_bit (tmp, escaped_id);
		      else
			flag = set_union_with_increment (tmp, pts, 0);

		      if (flag)
			{
			  get_varinfo (to)->solution = tmp;
			  if (!TEST_BIT (changed, to))
			    {
			      SET_BIT (changed, to);
			      changed_count++;
			    }
			}
		    }
		}
	    }
	}
      free_topo_info (ti);
      bitmap_obstack_release (&iteration_obstack);
    }

  BITMAP_FREE (pts);
  sbitmap_free (changed);
  bitmap_obstack_release (&oldpta_obstack);
}

/* Map from trees to variable infos.  */
static struct pointer_map_t *vi_for_tree;


/* Insert ID as the variable id for tree T in the vi_for_tree map.  */

static void
insert_vi_for_tree (tree t, varinfo_t vi)
{
  void **slot = pointer_map_insert (vi_for_tree, t);
  gcc_assert (vi);
  gcc_assert (*slot == NULL);
  *slot = vi;
}

/* Find the variable info for tree T in VI_FOR_TREE.  If T does not
   exist in the map, return NULL, otherwise, return the varinfo we found.  */

static varinfo_t
lookup_vi_for_tree (tree t)
{
  void **slot = pointer_map_contains (vi_for_tree, t);
  if (slot == NULL)
    return NULL;

  return (varinfo_t) *slot;
}

/* Return a printable name for DECL  */

static const char *
alias_get_name (tree decl)
{
  const char *res = get_name (decl);
  char *temp;
  int num_printed = 0;

  if (res != NULL)
    return res;

  res = "NULL";
  if (!dump_file)
    return res;

  if (TREE_CODE (decl) == SSA_NAME)
    {
      num_printed = asprintf (&temp, "%s_%u",
			      alias_get_name (SSA_NAME_VAR (decl)),
			      SSA_NAME_VERSION (decl));
    }
  else if (DECL_P (decl))
    {
      num_printed = asprintf (&temp, "D.%u", DECL_UID (decl));
    }
  if (num_printed > 0)
    {
      res = ggc_strdup (temp);
      free (temp);
    }
  return res;
}

/* Find the variable id for tree T in the map.
   If T doesn't exist in the map, create an entry for it and return it.  */

static varinfo_t
get_vi_for_tree (tree t)
{
  void **slot = pointer_map_contains (vi_for_tree, t);
  if (slot == NULL)
    return get_varinfo (create_variable_info_for (t, alias_get_name (t)));

  return (varinfo_t) *slot;
}

/* Get a scalar constraint expression for a new temporary variable.  */

static struct constraint_expr
new_scalar_tmp_constraint_exp (const char *name)
{
  struct constraint_expr tmp;
  varinfo_t vi;

  vi = new_var_info (NULL_TREE, name);
  vi->offset = 0;
  vi->size = -1;
  vi->fullsize = -1;
  vi->is_full_var = 1;

  tmp.var = vi->id;
  tmp.type = SCALAR;
  tmp.offset = 0;

  return tmp;
}

/* Get a constraint expression vector from an SSA_VAR_P node.
   If address_p is true, the result will be taken its address of.  */

static void
get_constraint_for_ssa_var (tree t, VEC(ce_s, heap) **results, bool address_p)
{
  struct constraint_expr cexpr;
  varinfo_t vi;

  /* We allow FUNCTION_DECLs here even though it doesn't make much sense.  */
  gcc_assert (SSA_VAR_P (t) || DECL_P (t));

  /* For parameters, get at the points-to set for the actual parm
     decl.  */
  if (TREE_CODE (t) == SSA_NAME
      && TREE_CODE (SSA_NAME_VAR (t)) == PARM_DECL
      && SSA_NAME_IS_DEFAULT_DEF (t))
    {
      get_constraint_for_ssa_var (SSA_NAME_VAR (t), results, address_p);
      return;
    }

  vi = get_vi_for_tree (t);
  cexpr.var = vi->id;
  cexpr.type = SCALAR;
  cexpr.offset = 0;
  /* If we determine the result is "anything", and we know this is readonly,
     say it points to readonly memory instead.  */
  if (cexpr.var == anything_id && TREE_READONLY (t))
    {
      gcc_unreachable ();
      cexpr.type = ADDRESSOF;
      cexpr.var = readonly_id;
    }

  /* If we are not taking the address of the constraint expr, add all
     sub-fiels of the variable as well.  */
  if (!address_p
      && !vi->is_full_var)
    {
      for (; vi; vi = vi->next)
	{
	  cexpr.var = vi->id;
	  VEC_safe_push (ce_s, heap, *results, &cexpr);
	}
      return;
    }

  VEC_safe_push (ce_s, heap, *results, &cexpr);
}

/* Process constraint T, performing various simplifications and then
   adding it to our list of overall constraints.  */

static void
process_constraint (constraint_t t)
{
  struct constraint_expr rhs = t->rhs;
  struct constraint_expr lhs = t->lhs;

  gcc_assert (rhs.var < VEC_length (varinfo_t, varmap));
  gcc_assert (lhs.var < VEC_length (varinfo_t, varmap));

  /* If we didn't get any useful constraint from the lhs we get
     &ANYTHING as fallback from get_constraint_for.  Deal with
     it here by turning it into *ANYTHING.  */
  if (lhs.type == ADDRESSOF
      && lhs.var == anything_id)
    lhs.type = DEREF;

  /* ADDRESSOF on the lhs is invalid.  */
  gcc_assert (lhs.type != ADDRESSOF);

  /* This can happen in our IR with things like n->a = *p */
  if (rhs.type == DEREF && lhs.type == DEREF && rhs.var != anything_id)
    {
      /* Split into tmp = *rhs, *lhs = tmp */
      struct constraint_expr tmplhs;
      tmplhs = new_scalar_tmp_constraint_exp ("doubledereftmp");
      process_constraint (new_constraint (tmplhs, rhs));
      process_constraint (new_constraint (lhs, tmplhs));
    }
  else if (rhs.type == ADDRESSOF && lhs.type == DEREF)
    {
      /* Split into tmp = &rhs, *lhs = tmp */
      struct constraint_expr tmplhs;
      tmplhs = new_scalar_tmp_constraint_exp ("derefaddrtmp");
      process_constraint (new_constraint (tmplhs, rhs));
      process_constraint (new_constraint (lhs, tmplhs));
    }
  else
    {
      gcc_assert (rhs.type != ADDRESSOF || rhs.offset == 0);
      VEC_safe_push (constraint_t, heap, constraints, t);
    }
}

/* Return true if T is a type that could contain pointers.  */

static bool
type_could_have_pointers (tree type)
{
  if (POINTER_TYPE_P (type))
    return true;

  if (TREE_CODE (type) == ARRAY_TYPE)
    return type_could_have_pointers (TREE_TYPE (type));

  return AGGREGATE_TYPE_P (type);
}

/* Return true if T is a variable of a type that could contain
   pointers.  */

static bool
could_have_pointers (tree t)
{
  return type_could_have_pointers (TREE_TYPE (t));
}

/* Return the position, in bits, of FIELD_DECL from the beginning of its
   structure.  */

static HOST_WIDE_INT
bitpos_of_field (const tree fdecl)
{

  if (!host_integerp (DECL_FIELD_OFFSET (fdecl), 0)
      || !host_integerp (DECL_FIELD_BIT_OFFSET (fdecl), 0))
    return -1;

  return (TREE_INT_CST_LOW (DECL_FIELD_OFFSET (fdecl)) * 8
	  + TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (fdecl)));
}


/* Get constraint expressions for offsetting PTR by OFFSET.  Stores the
   resulting constraint expressions in *RESULTS.  */

static void
get_constraint_for_ptr_offset (tree ptr, tree offset,
			       VEC (ce_s, heap) **results)
{
  struct constraint_expr c;
  unsigned int j, n;
  HOST_WIDE_INT rhsunitoffset, rhsoffset;

  /* If we do not do field-sensitive PTA adding offsets to pointers
     does not change the points-to solution.  */
  if (!use_field_sensitive)
    {
      get_constraint_for (ptr, results);
      return;
    }

  /* If the offset is not a non-negative integer constant that fits
     in a HOST_WIDE_INT, we have to fall back to a conservative
     solution which includes all sub-fields of all pointed-to
     variables of ptr.  */
  if (offset == NULL_TREE
      || !host_integerp (offset, 0))
    rhsoffset = UNKNOWN_OFFSET;
  else
    {
      /* Make sure the bit-offset also fits.  */
      rhsunitoffset = TREE_INT_CST_LOW (offset);
      rhsoffset = rhsunitoffset * BITS_PER_UNIT;
      if (rhsunitoffset != rhsoffset / BITS_PER_UNIT)
	rhsoffset = UNKNOWN_OFFSET;
    }

  get_constraint_for (ptr, results);
  if (rhsoffset == 0)
    return;

  /* As we are eventually appending to the solution do not use
     VEC_iterate here.  */
  n = VEC_length (ce_s, *results);
  for (j = 0; j < n; j++)
    {
      varinfo_t curr;
      c = *VEC_index (ce_s, *results, j);
      curr = get_varinfo (c.var);

      if (c.type == ADDRESSOF
	  /* If this varinfo represents a full variable just use it.  */
	  && curr->is_full_var)
	c.offset = 0;
      else if (c.type == ADDRESSOF
	       /* If we do not know the offset add all subfields.  */
	       && rhsoffset == UNKNOWN_OFFSET)
	{
	  varinfo_t temp = lookup_vi_for_tree (curr->decl);
	  do
	    {
	      struct constraint_expr c2;
	      c2.var = temp->id;
	      c2.type = ADDRESSOF;
	      c2.offset = 0;
	      if (c2.var != c.var)
		VEC_safe_push (ce_s, heap, *results, &c2);
	      temp = temp->next;
	    }
	  while (temp);
	}
      else if (c.type == ADDRESSOF)
	{
	  varinfo_t temp;
	  unsigned HOST_WIDE_INT offset = curr->offset + rhsoffset;

	  /* Search the sub-field which overlaps with the
	     pointed-to offset.  If the result is outside of the variable
	     we have to provide a conservative result, as the variable is
	     still reachable from the resulting pointer (even though it
	     technically cannot point to anything).  The last and first
	     sub-fields are such conservative results.
	     ???  If we always had a sub-field for &object + 1 then
	     we could represent this in a more precise way.  */
	  if (rhsoffset < 0
	      && curr->offset < offset)
	    offset = 0;
	  temp = first_or_preceding_vi_for_offset (curr, offset);

	  /* If the found variable is not exactly at the pointed to
	     result, we have to include the next variable in the
	     solution as well.  Otherwise two increments by offset / 2
	     do not result in the same or a conservative superset
	     solution.  */
	  if (temp->offset != offset
	      && temp->next != NULL)
	    {
	      struct constraint_expr c2;
	      c2.var = temp->next->id;
	      c2.type = ADDRESSOF;
	      c2.offset = 0;
	      VEC_safe_push (ce_s, heap, *results, &c2);
	    }
	  c.var = temp->id;
	  c.offset = 0;
	}
      else
	c.offset = rhsoffset;

      VEC_replace (ce_s, *results, j, &c);
    }
}


/* Given a COMPONENT_REF T, return the constraint_expr vector for it.
   If address_p is true the result will be taken its address of.  */

static void
get_constraint_for_component_ref (tree t, VEC(ce_s, heap) **results,
				  bool address_p)
{
  tree orig_t = t;
  HOST_WIDE_INT bitsize = -1;
  HOST_WIDE_INT bitmaxsize = -1;
  HOST_WIDE_INT bitpos;
  tree forzero;
  struct constraint_expr *result;

  /* Some people like to do cute things like take the address of
     &0->a.b */
  forzero = t;
  while (!SSA_VAR_P (forzero) && !CONSTANT_CLASS_P (forzero))
    forzero = TREE_OPERAND (forzero, 0);

  if (CONSTANT_CLASS_P (forzero) && integer_zerop (forzero))
    {
      struct constraint_expr temp;

      temp.offset = 0;
      temp.var = integer_id;
      temp.type = SCALAR;
      VEC_safe_push (ce_s, heap, *results, &temp);
      return;
    }

  t = get_ref_base_and_extent (t, &bitpos, &bitsize, &bitmaxsize);

  /* Pretend to take the address of the base, we'll take care of
     adding the required subset of sub-fields below.  */
  get_constraint_for_1 (t, results, true);
  gcc_assert (VEC_length (ce_s, *results) == 1);
  result = VEC_last (ce_s, *results);

  if (result->type == SCALAR
      && get_varinfo (result->var)->is_full_var)
    /* For single-field vars do not bother about the offset.  */
    result->offset = 0;
  else if (result->type == SCALAR)
    {
      /* In languages like C, you can access one past the end of an
	 array.  You aren't allowed to dereference it, so we can
	 ignore this constraint. When we handle pointer subtraction,
	 we may have to do something cute here.  */

      if ((unsigned HOST_WIDE_INT)bitpos < get_varinfo (result->var)->fullsize
	  && bitmaxsize != 0)
	{
	  /* It's also not true that the constraint will actually start at the
	     right offset, it may start in some padding.  We only care about
	     setting the constraint to the first actual field it touches, so
	     walk to find it.  */
	  struct constraint_expr cexpr = *result;
	  varinfo_t curr;
	  VEC_pop (ce_s, *results);
	  cexpr.offset = 0;
	  for (curr = get_varinfo (cexpr.var); curr; curr = curr->next)
	    {
	      if (ranges_overlap_p (curr->offset, curr->size,
				    bitpos, bitmaxsize))
		{
		  cexpr.var = curr->id;
		  VEC_safe_push (ce_s, heap, *results, &cexpr);
		  if (address_p)
		    break;
		}
	    }
	  /* If we are going to take the address of this field then
	     to be able to compute reachability correctly add at least
	     the last field of the variable.  */
	  if (address_p
	      && VEC_length (ce_s, *results) == 0)
	    {
	      curr = get_varinfo (cexpr.var);
	      while (curr->next != NULL)
		curr = curr->next;
	      cexpr.var = curr->id;
	      VEC_safe_push (ce_s, heap, *results, &cexpr);
	    }
	  else
	    /* Assert that we found *some* field there. The user couldn't be
	       accessing *only* padding.  */
	    /* Still the user could access one past the end of an array
	       embedded in a struct resulting in accessing *only* padding.  */
	    gcc_assert (VEC_length (ce_s, *results) >= 1
			|| ref_contains_array_ref (orig_t));
	}
      else if (bitmaxsize == 0)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Access to zero-sized part of variable,"
		     "ignoring\n");
	}
      else
	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, "Access to past the end of variable, ignoring\n");
    }
  else if (result->type == DEREF)
    {
      /* If we do not know exactly where the access goes say so.  Note
	 that only for non-structure accesses we know that we access
	 at most one subfiled of any variable.  */
      if (bitpos == -1
	  || bitsize != bitmaxsize
	  || AGGREGATE_TYPE_P (TREE_TYPE (orig_t)))
	result->offset = UNKNOWN_OFFSET;
      else
	result->offset = bitpos;
    }
  else if (result->type == ADDRESSOF)
    {
      /* We can end up here for component references on a
         VIEW_CONVERT_EXPR <>(&foobar).  */
      result->type = SCALAR;
      result->var = anything_id;
      result->offset = 0;
    }
  else
    gcc_unreachable ();
}


/* Dereference the constraint expression CONS, and return the result.
   DEREF (ADDRESSOF) = SCALAR
   DEREF (SCALAR) = DEREF
   DEREF (DEREF) = (temp = DEREF1; result = DEREF(temp))
   This is needed so that we can handle dereferencing DEREF constraints.  */

static void
do_deref (VEC (ce_s, heap) **constraints)
{
  struct constraint_expr *c;
  unsigned int i = 0;

  for (i = 0; VEC_iterate (ce_s, *constraints, i, c); i++)
    {
      if (c->type == SCALAR)
	c->type = DEREF;
      else if (c->type == ADDRESSOF)
	c->type = SCALAR;
      else if (c->type == DEREF)
	{
	  struct constraint_expr tmplhs;
	  tmplhs = new_scalar_tmp_constraint_exp ("dereftmp");
	  process_constraint (new_constraint (tmplhs, *c));
	  c->var = tmplhs.var;
	}
      else
	gcc_unreachable ();
    }
}

static void get_constraint_for_1 (tree, VEC (ce_s, heap) **, bool);

/* Given a tree T, return the constraint expression for taking the
   address of it.  */

static void
get_constraint_for_address_of (tree t, VEC (ce_s, heap) **results)
{
  struct constraint_expr *c;
  unsigned int i;

  get_constraint_for_1 (t, results, true);

  for (i = 0; VEC_iterate (ce_s, *results, i, c); i++)
    {
      if (c->type == DEREF)
	c->type = SCALAR;
      else
	c->type = ADDRESSOF;
    }
}

/* Given a tree T, return the constraint expression for it.  */

static void
get_constraint_for_1 (tree t, VEC (ce_s, heap) **results, bool address_p)
{
  struct constraint_expr temp;

  /* x = integer is all glommed to a single variable, which doesn't
     point to anything by itself.  That is, of course, unless it is an
     integer constant being treated as a pointer, in which case, we
     will return that this is really the addressof anything.  This
     happens below, since it will fall into the default case. The only
     case we know something about an integer treated like a pointer is
     when it is the NULL pointer, and then we just say it points to
     NULL.

     Do not do that if -fno-delete-null-pointer-checks though, because
     in that case *NULL does not fail, so it _should_ alias *anything.
     It is not worth adding a new option or renaming the existing one,
     since this case is relatively obscure.  */
  if (flag_delete_null_pointer_checks
      && ((TREE_CODE (t) == INTEGER_CST
	   && integer_zerop (t))
	  /* The only valid CONSTRUCTORs in gimple with pointer typed
	     elements are zero-initializer.  */
	  || TREE_CODE (t) == CONSTRUCTOR))
    {
      temp.var = nothing_id;
      temp.type = ADDRESSOF;
      temp.offset = 0;
      VEC_safe_push (ce_s, heap, *results, &temp);
      return;
    }

  /* String constants are read-only.  */
  if (TREE_CODE (t) == STRING_CST)
    {
      temp.var = readonly_id;
      temp.type = SCALAR;
      temp.offset = 0;
      VEC_safe_push (ce_s, heap, *results, &temp);
      return;
    }

  switch (TREE_CODE_CLASS (TREE_CODE (t)))
    {
    case tcc_expression:
      {
	switch (TREE_CODE (t))
	  {
	  case ADDR_EXPR:
	    get_constraint_for_address_of (TREE_OPERAND (t, 0), results);
	    return;
	  default:;
	  }
	break;
      }
    case tcc_reference:
      {
	switch (TREE_CODE (t))
	  {
	  case INDIRECT_REF:
	    {
	      get_constraint_for_1 (TREE_OPERAND (t, 0), results, address_p);
	      do_deref (results);
	      return;
	    }
	  case ARRAY_REF:
	  case ARRAY_RANGE_REF:
	  case COMPONENT_REF:
	    get_constraint_for_component_ref (t, results, address_p);
	    return;
	  case VIEW_CONVERT_EXPR:
	    get_constraint_for_1 (TREE_OPERAND (t, 0), results, address_p);
	    return;
	  /* We are missing handling for TARGET_MEM_REF here.  */
	  default:;
	  }
	break;
      }
    case tcc_exceptional:
      {
	switch (TREE_CODE (t))
	  {
	  case SSA_NAME:
	    {
	      get_constraint_for_ssa_var (t, results, address_p);
	      return;
	    }
	  default:;
	  }
	break;
      }
    case tcc_declaration:
      {
	get_constraint_for_ssa_var (t, results, address_p);
	return;
      }
    default:;
    }

  /* The default fallback is a constraint from anything.  */
  temp.type = ADDRESSOF;
  temp.var = anything_id;
  temp.offset = 0;
  VEC_safe_push (ce_s, heap, *results, &temp);
}

/* Given a gimple tree T, return the constraint expression vector for it.  */

static void
get_constraint_for (tree t, VEC (ce_s, heap) **results)
{
  gcc_assert (VEC_length (ce_s, *results) == 0);

  get_constraint_for_1 (t, results, false);
}


/* Efficiently generates constraints from all entries in *RHSC to all
   entries in *LHSC.  */

static void
process_all_all_constraints (VEC (ce_s, heap) *lhsc, VEC (ce_s, heap) *rhsc)
{
  struct constraint_expr *lhsp, *rhsp;
  unsigned i, j;

  if (VEC_length (ce_s, lhsc) <= 1
      || VEC_length (ce_s, rhsc) <= 1)
    {
      for (i = 0; VEC_iterate (ce_s, lhsc, i, lhsp); ++i)
	for (j = 0; VEC_iterate (ce_s, rhsc, j, rhsp); ++j)
	  process_constraint (new_constraint (*lhsp, *rhsp));
    }
  else
    {
      struct constraint_expr tmp;
      tmp = new_scalar_tmp_constraint_exp ("allalltmp");
      for (i = 0; VEC_iterate (ce_s, rhsc, i, rhsp); ++i)
	process_constraint (new_constraint (tmp, *rhsp));
      for (i = 0; VEC_iterate (ce_s, lhsc, i, lhsp); ++i)
	process_constraint (new_constraint (*lhsp, tmp));
    }
}

/* Handle aggregate copies by expanding into copies of the respective
   fields of the structures.  */

static void
do_structure_copy (tree lhsop, tree rhsop)
{
  struct constraint_expr *lhsp, *rhsp;
  VEC (ce_s, heap) *lhsc = NULL, *rhsc = NULL;
  unsigned j;

  get_constraint_for (lhsop, &lhsc);
  get_constraint_for (rhsop, &rhsc);
  lhsp = VEC_index (ce_s, lhsc, 0);
  rhsp = VEC_index (ce_s, rhsc, 0);
  if (lhsp->type == DEREF
      || (lhsp->type == ADDRESSOF && lhsp->var == anything_id)
      || rhsp->type == DEREF)
    process_all_all_constraints (lhsc, rhsc);
  else if (lhsp->type == SCALAR
	   && (rhsp->type == SCALAR
	       || rhsp->type == ADDRESSOF))
    {
      HOST_WIDE_INT lhssize, lhsmaxsize, lhsoffset;
      HOST_WIDE_INT rhssize, rhsmaxsize, rhsoffset;
      unsigned k = 0;
      get_ref_base_and_extent (lhsop, &lhsoffset, &lhssize, &lhsmaxsize);
      get_ref_base_and_extent (rhsop, &rhsoffset, &rhssize, &rhsmaxsize);
      for (j = 0; VEC_iterate (ce_s, lhsc, j, lhsp);)
	{
	  varinfo_t lhsv, rhsv;
	  rhsp = VEC_index (ce_s, rhsc, k);
	  lhsv = get_varinfo (lhsp->var);
	  rhsv = get_varinfo (rhsp->var);
	  if (lhsv->may_have_pointers
	      && ranges_overlap_p (lhsv->offset + rhsoffset, lhsv->size,
				   rhsv->offset + lhsoffset, rhsv->size))
	    process_constraint (new_constraint (*lhsp, *rhsp));
	  if (lhsv->offset + rhsoffset + lhsv->size
	      > rhsv->offset + lhsoffset + rhsv->size)
	    {
	      ++k;
	      if (k >= VEC_length (ce_s, rhsc))
		break;
	    }
	  else
	    ++j;
	}
    }
  else
    gcc_unreachable ();

  VEC_free (ce_s, heap, lhsc);
  VEC_free (ce_s, heap, rhsc);
}

/* Create a constraint ID = OP.  */

static void
make_constraint_to (unsigned id, tree op)
{
  VEC(ce_s, heap) *rhsc = NULL;
  struct constraint_expr *c;
  struct constraint_expr includes;
  unsigned int j;

  includes.var = id;
  includes.offset = 0;
  includes.type = SCALAR;

  get_constraint_for (op, &rhsc);
  for (j = 0; VEC_iterate (ce_s, rhsc, j, c); j++)
    process_constraint (new_constraint (includes, *c));
  VEC_free (ce_s, heap, rhsc);
}

/* Create a constraint ID = &FROM.  */

static void
make_constraint_from (varinfo_t vi, int from)
{
  struct constraint_expr lhs, rhs;

  lhs.var = vi->id;
  lhs.offset = 0;
  lhs.type = SCALAR;

  rhs.var = from;
  rhs.offset = 0;
  rhs.type = ADDRESSOF;
  process_constraint (new_constraint (lhs, rhs));
}

/* Create a constraint ID = FROM.  */

static void
make_copy_constraint (varinfo_t vi, int from)
{
  struct constraint_expr lhs, rhs;

  lhs.var = vi->id;
  lhs.offset = 0;
  lhs.type = SCALAR;

  rhs.var = from;
  rhs.offset = 0;
  rhs.type = SCALAR;
  process_constraint (new_constraint (lhs, rhs));
}

/* Make constraints necessary to make OP escape.  */

static void
make_escape_constraint (tree op)
{
  make_constraint_to (escaped_id, op);
}

/* Create a new artificial heap variable with NAME and make a
   constraint from it to LHS.  Return the created variable.  */

static varinfo_t
make_constraint_from_heapvar (varinfo_t lhs, const char *name)
{
  varinfo_t vi;
  tree heapvar = heapvar_lookup (lhs->decl, lhs->offset);

  if (heapvar == NULL_TREE)
    {
      var_ann_t ann;
      heapvar = create_tmp_var_raw (ptr_type_node, name);
      DECL_EXTERNAL (heapvar) = 1;

      heapvar_insert (lhs->decl, lhs->offset, heapvar);

      ann = get_var_ann (heapvar);
      ann->is_heapvar = 1;
    }

  /* For global vars we need to add a heapvar to the list of referenced
     vars of a different function than it was created for originally.  */
  if (gimple_referenced_vars (cfun))
    add_referenced_var (heapvar);

  vi = new_var_info (heapvar, name);
  vi->is_artificial_var = true;
  vi->is_heap_var = true;
  vi->is_unknown_size_var = true;
  vi->offset = 0;
  vi->fullsize = ~0;
  vi->size = ~0;
  vi->is_full_var = true;
  insert_vi_for_tree (heapvar, vi);

  make_constraint_from (lhs, vi->id);

  return vi;
}

/* Create a new artificial heap variable with NAME and make a
   constraint from it to LHS.  Set flags according to a tag used
   for tracking restrict pointers.  */

static void
make_constraint_from_restrict (varinfo_t lhs, const char *name)
{
  varinfo_t vi;
  vi = make_constraint_from_heapvar (lhs, name);
  vi->is_restrict_var = 1;
  vi->is_global_var = 0;
  vi->is_special_var = 1;
  vi->may_have_pointers = 0;
}

/* For non-IPA mode, generate constraints necessary for a call on the
   RHS.  */

static void
handle_rhs_call (gimple stmt, VEC(ce_s, heap) **results)
{
  struct constraint_expr rhsc;
  unsigned i;

  for (i = 0; i < gimple_call_num_args (stmt); ++i)
    {
      tree arg = gimple_call_arg (stmt, i);

      /* Find those pointers being passed, and make sure they end up
	 pointing to anything.  */
      if (could_have_pointers (arg))
	make_escape_constraint (arg);
    }

  /* The static chain escapes as well.  */
  if (gimple_call_chain (stmt))
    make_escape_constraint (gimple_call_chain (stmt));

  /* And if we applied NRV the address of the return slot escapes as well.  */
  if (gimple_call_return_slot_opt_p (stmt)
      && gimple_call_lhs (stmt) != NULL_TREE
      && TREE_ADDRESSABLE (TREE_TYPE (gimple_call_lhs (stmt))))
    {
      VEC(ce_s, heap) *tmpc = NULL;
      struct constraint_expr lhsc, *c;
      get_constraint_for_address_of (gimple_call_lhs (stmt), &tmpc);
      lhsc.var = escaped_id;
      lhsc.offset = 0;
      lhsc.type = SCALAR;
      for (i = 0; VEC_iterate (ce_s, tmpc, i, c); ++i)
	process_constraint (new_constraint (lhsc, *c));
      VEC_free(ce_s, heap, tmpc);
    }

  /* Regular functions return nonlocal memory.  */
  rhsc.var = nonlocal_id;
  rhsc.offset = 0;
  rhsc.type = SCALAR;
  VEC_safe_push (ce_s, heap, *results, &rhsc);
}

/* For non-IPA mode, generate constraints necessary for a call
   that returns a pointer and assigns it to LHS.  This simply makes
   the LHS point to global and escaped variables.  */

static void
handle_lhs_call (tree lhs, int flags, VEC(ce_s, heap) *rhsc, tree fndecl)
{
  VEC(ce_s, heap) *lhsc = NULL;

  get_constraint_for (lhs, &lhsc);

  if (flags & ECF_MALLOC)
    {
      varinfo_t vi;
      vi = make_constraint_from_heapvar (get_vi_for_tree (lhs), "HEAP");
      /* We delay marking allocated storage global until we know if
         it escapes.  */
      DECL_EXTERNAL (vi->decl) = 0;
      vi->is_global_var = 0;
      /* If this is not a real malloc call assume the memory was
         initialized and thus may point to global memory.  All
	 builtin functions with the malloc attribute behave in a sane way.  */
      if (!fndecl
	  || DECL_BUILT_IN_CLASS (fndecl) != BUILT_IN_NORMAL)
	make_constraint_from (vi, nonlocal_id);
    }
  else if (VEC_length (ce_s, rhsc) > 0)
    {
      /* If the store is to a global decl make sure to
	 add proper escape constraints.  */
      lhs = get_base_address (lhs);
      if (lhs
	  && DECL_P (lhs)
	  && is_global_var (lhs))
	{
	  struct constraint_expr tmpc;
	  tmpc.var = escaped_id;
	  tmpc.offset = 0;
	  tmpc.type = SCALAR;
	  VEC_safe_push (ce_s, heap, lhsc, &tmpc);
	}
      process_all_all_constraints (lhsc, rhsc);
    }
  VEC_free (ce_s, heap, lhsc);
}

/* For non-IPA mode, generate constraints necessary for a call of a
   const function that returns a pointer in the statement STMT.  */

static void
handle_const_call (gimple stmt, VEC(ce_s, heap) **results)
{
  struct constraint_expr rhsc;
  unsigned int k;

  /* Treat nested const functions the same as pure functions as far
     as the static chain is concerned.  */
  if (gimple_call_chain (stmt))
    {
      make_constraint_to (callused_id, gimple_call_chain (stmt));
      rhsc.var = callused_id;
      rhsc.offset = 0;
      rhsc.type = SCALAR;
      VEC_safe_push (ce_s, heap, *results, &rhsc);
    }

  /* May return arguments.  */
  for (k = 0; k < gimple_call_num_args (stmt); ++k)
    {
      tree arg = gimple_call_arg (stmt, k);

      if (could_have_pointers (arg))
	{
	  VEC(ce_s, heap) *argc = NULL;
	  unsigned i;
	  struct constraint_expr *argp;
	  get_constraint_for (arg, &argc);
	  for (i = 0; VEC_iterate (ce_s, argc, i, argp); ++i)
	    VEC_safe_push (ce_s, heap, *results, argp);
	  VEC_free(ce_s, heap, argc);
	}
    }

  /* May return addresses of globals.  */
  rhsc.var = nonlocal_id;
  rhsc.offset = 0;
  rhsc.type = ADDRESSOF;
  VEC_safe_push (ce_s, heap, *results, &rhsc);
}

/* For non-IPA mode, generate constraints necessary for a call to a
   pure function in statement STMT.  */

static void
handle_pure_call (gimple stmt, VEC(ce_s, heap) **results)
{
  struct constraint_expr rhsc;
  unsigned i;
  bool need_callused = false;

  /* Memory reached from pointer arguments is call-used.  */
  for (i = 0; i < gimple_call_num_args (stmt); ++i)
    {
      tree arg = gimple_call_arg (stmt, i);

      if (could_have_pointers (arg))
	{
	  make_constraint_to (callused_id, arg);
	  need_callused = true;
	}
    }

  /* The static chain is used as well.  */
  if (gimple_call_chain (stmt))
    {
      make_constraint_to (callused_id, gimple_call_chain (stmt));
      need_callused = true;
    }

  /* Pure functions may return callused and nonlocal memory.  */
  if (need_callused)
    {
      rhsc.var = callused_id;
      rhsc.offset = 0;
      rhsc.type = SCALAR;
      VEC_safe_push (ce_s, heap, *results, &rhsc);
    }
  rhsc.var = nonlocal_id;
  rhsc.offset = 0;
  rhsc.type = SCALAR;
  VEC_safe_push (ce_s, heap, *results, &rhsc);
}

/* Walk statement T setting up aliasing constraints according to the
   references found in T.  This function is the main part of the
   constraint builder.  AI points to auxiliary alias information used
   when building alias sets and computing alias grouping heuristics.  */

static void
find_func_aliases (gimple origt)
{
  gimple t = origt;
  VEC(ce_s, heap) *lhsc = NULL;
  VEC(ce_s, heap) *rhsc = NULL;
  struct constraint_expr *c;

  /* Now build constraints expressions.  */
  if (gimple_code (t) == GIMPLE_PHI)
    {
      gcc_assert (!AGGREGATE_TYPE_P (TREE_TYPE (gimple_phi_result (t))));

      /* Only care about pointers and structures containing
	 pointers.  */
      if (could_have_pointers (gimple_phi_result (t)))
	{
	  size_t i;
	  unsigned int j;

	  /* For a phi node, assign all the arguments to
	     the result.  */
	  get_constraint_for (gimple_phi_result (t), &lhsc);
	  for (i = 0; i < gimple_phi_num_args (t); i++)
	    {
	      tree strippedrhs = PHI_ARG_DEF (t, i);

	      STRIP_NOPS (strippedrhs);
	      get_constraint_for (gimple_phi_arg_def (t, i), &rhsc);

	      for (j = 0; VEC_iterate (ce_s, lhsc, j, c); j++)
		{
		  struct constraint_expr *c2;
		  while (VEC_length (ce_s, rhsc) > 0)
		    {
		      c2 = VEC_last (ce_s, rhsc);
		      process_constraint (new_constraint (*c, *c2));
		      VEC_pop (ce_s, rhsc);
		    }
		}
	    }
	}
    }
  /* In IPA mode, we need to generate constraints to pass call
     arguments through their calls.   There are two cases,
     either a GIMPLE_CALL returning a value, or just a plain
     GIMPLE_CALL when we are not.

     In non-ipa mode, we need to generate constraints for each
     pointer passed by address.  */
  else if (is_gimple_call (t))
    {
      tree fndecl = gimple_call_fndecl (t);
      if (fndecl != NULL_TREE
	  && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
	/* ???  All builtins that are handled here need to be handled
	   in the alias-oracle query functions explicitly!  */
	switch (DECL_FUNCTION_CODE (fndecl))
	  {
	  /* All the following functions return a pointer to the same object
	     as their first argument points to.  The functions do not add
	     to the ESCAPED solution.  The functions make the first argument
	     pointed to memory point to what the second argument pointed to
	     memory points to.  */
	  case BUILT_IN_STRCPY:
	  case BUILT_IN_STRNCPY:
	  case BUILT_IN_BCOPY:
	  case BUILT_IN_MEMCPY:
	  case BUILT_IN_MEMMOVE:
	  case BUILT_IN_MEMPCPY:
	  case BUILT_IN_STPCPY:
	  case BUILT_IN_STPNCPY:
	  case BUILT_IN_STRCAT:
	  case BUILT_IN_STRNCAT:
	    {
	      tree res = gimple_call_lhs (t);
	      tree dest = gimple_call_arg (t, (DECL_FUNCTION_CODE (fndecl)
					       == BUILT_IN_BCOPY ? 1 : 0));
	      tree src = gimple_call_arg (t, (DECL_FUNCTION_CODE (fndecl)
					      == BUILT_IN_BCOPY ? 0 : 1));
	      if (res != NULL_TREE)
		{
		  get_constraint_for (res, &lhsc);
		  if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_MEMPCPY
		      || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_STPCPY
		      || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_STPNCPY)
		    get_constraint_for_ptr_offset (dest, NULL_TREE, &rhsc);
		  else
		    get_constraint_for (dest, &rhsc);
		  process_all_all_constraints (lhsc, rhsc);
		  VEC_free (ce_s, heap, lhsc);
		  VEC_free (ce_s, heap, rhsc);
		}
	      get_constraint_for_ptr_offset (dest, NULL_TREE, &lhsc);
	      get_constraint_for_ptr_offset (src, NULL_TREE, &rhsc);
	      do_deref (&lhsc);
	      do_deref (&rhsc);
	      process_all_all_constraints (lhsc, rhsc);
	      VEC_free (ce_s, heap, lhsc);
	      VEC_free (ce_s, heap, rhsc);
	      return;
	    }
	  case BUILT_IN_MEMSET:
	    {
	      tree res = gimple_call_lhs (t);
	      tree dest = gimple_call_arg (t, 0);
	      unsigned i;
	      ce_s *lhsp;
	      struct constraint_expr ac;
	      if (res != NULL_TREE)
		{
		  get_constraint_for (res, &lhsc);
		  get_constraint_for (dest, &rhsc);
		  process_all_all_constraints (lhsc, rhsc);
		  VEC_free (ce_s, heap, lhsc);
		  VEC_free (ce_s, heap, rhsc);
		}
	      get_constraint_for_ptr_offset (dest, NULL_TREE, &lhsc);
	      do_deref (&lhsc);
	      if (flag_delete_null_pointer_checks
		  && integer_zerop (gimple_call_arg (t, 1)))
		{
		  ac.type = ADDRESSOF;
		  ac.var = nothing_id;
		}
	      else
		{
		  ac.type = SCALAR;
		  ac.var = integer_id;
		}
	      ac.offset = 0;
	      for (i = 0; VEC_iterate (ce_s, lhsc, i, lhsp); ++i)
		process_constraint (new_constraint (*lhsp, ac));
	      VEC_free (ce_s, heap, lhsc);
	      return;
	    }
	  /* All the following functions do not return pointers, do not
	     modify the points-to sets of memory reachable from their
	     arguments and do not add to the ESCAPED solution.  */
	  case BUILT_IN_SINCOS:
	  case BUILT_IN_SINCOSF:
	  case BUILT_IN_SINCOSL:
	  case BUILT_IN_FREXP:
	  case BUILT_IN_FREXPF:
	  case BUILT_IN_FREXPL:
	  case BUILT_IN_GAMMA_R:
	  case BUILT_IN_GAMMAF_R:
	  case BUILT_IN_GAMMAL_R:
	  case BUILT_IN_LGAMMA_R:
	  case BUILT_IN_LGAMMAF_R:
	  case BUILT_IN_LGAMMAL_R:
	  case BUILT_IN_MODF:
	  case BUILT_IN_MODFF:
	  case BUILT_IN_MODFL:
	  case BUILT_IN_REMQUO:
	  case BUILT_IN_REMQUOF:
	  case BUILT_IN_REMQUOL:
	  case BUILT_IN_FREE:
	    return;
	  /* printf-style functions may have hooks to set pointers to
	     point to somewhere into the generated string.  Leave them
	     for a later excercise...  */
	  default:
	    /* Fallthru to general call handling.  */;
	  }
      if (!in_ipa_mode
	  || (fndecl
	      && !lookup_vi_for_tree (fndecl)))
	{
	  VEC(ce_s, heap) *rhsc = NULL;
	  int flags = gimple_call_flags (t);

	  /* Const functions can return their arguments and addresses
	     of global memory but not of escaped memory.  */
	  if (flags & (ECF_CONST|ECF_NOVOPS))
	    {
	      if (gimple_call_lhs (t)
		  && could_have_pointers (gimple_call_lhs (t)))
		handle_const_call (t, &rhsc);
	    }
	  /* Pure functions can return addresses in and of memory
	     reachable from their arguments, but they are not an escape
	     point for reachable memory of their arguments.  */
	  else if (flags & (ECF_PURE|ECF_LOOPING_CONST_OR_PURE))
	    handle_pure_call (t, &rhsc);
	  else
	    handle_rhs_call (t, &rhsc);
	  if (gimple_call_lhs (t)
	      && could_have_pointers (gimple_call_lhs (t)))
	    handle_lhs_call (gimple_call_lhs (t), flags, rhsc, fndecl);
	  VEC_free (ce_s, heap, rhsc);
	}
      else
	{
	  tree lhsop;
	  varinfo_t fi;
	  int i = 1;
	  size_t j;
	  tree decl;

	  lhsop = gimple_call_lhs (t);
	  decl = gimple_call_fndecl (t);

	  /* If we can directly resolve the function being called, do so.
	     Otherwise, it must be some sort of indirect expression that
	     we should still be able to handle.  */
	  if (decl)
	    fi = get_vi_for_tree (decl);
	  else
	    {
	      decl = gimple_call_fn (t);
	      fi = get_vi_for_tree (decl);
	    }

	  /* Assign all the passed arguments to the appropriate incoming
	     parameters of the function.  */
	  for (j = 0; j < gimple_call_num_args (t); j++)
	    {
	      struct constraint_expr lhs ;
	      struct constraint_expr *rhsp;
	      tree arg = gimple_call_arg (t, j);

	      get_constraint_for (arg, &rhsc);
	      if (TREE_CODE (decl) != FUNCTION_DECL)
		{
		  lhs.type = DEREF;
		  lhs.var = fi->id;
		  lhs.offset = i;
		}
	      else
		{
		  lhs.type = SCALAR;
		  lhs.var = first_vi_for_offset (fi, i)->id;
		  lhs.offset = 0;
		}
	      while (VEC_length (ce_s, rhsc) != 0)
		{
		  rhsp = VEC_last (ce_s, rhsc);
		  process_constraint (new_constraint (lhs, *rhsp));
		  VEC_pop (ce_s, rhsc);
		}
	      i++;
	    }

	  /* If we are returning a value, assign it to the result.  */
	  if (lhsop)
	    {
	      struct constraint_expr rhs;
	      struct constraint_expr *lhsp;
	      unsigned int j = 0;

	      get_constraint_for (lhsop, &lhsc);
	      if (TREE_CODE (decl) != FUNCTION_DECL)
		{
		  rhs.type = DEREF;
		  rhs.var = fi->id;
		  rhs.offset = i;
		}
	      else
		{
		  rhs.type = SCALAR;
		  rhs.var = first_vi_for_offset (fi, i)->id;
		  rhs.offset = 0;
		}
	      for (j = 0; VEC_iterate (ce_s, lhsc, j, lhsp); j++)
		process_constraint (new_constraint (*lhsp, rhs));
	    }
	}
    }
  /* Otherwise, just a regular assignment statement.  Only care about
     operations with pointer result, others are dealt with as escape
     points if they have pointer operands.  */
  else if (is_gimple_assign (t)
	   && could_have_pointers (gimple_assign_lhs (t)))
    {
      /* Otherwise, just a regular assignment statement.  */
      tree lhsop = gimple_assign_lhs (t);
      tree rhsop = (gimple_num_ops (t) == 2) ? gimple_assign_rhs1 (t) : NULL;

      if (rhsop && AGGREGATE_TYPE_P (TREE_TYPE (lhsop)))
	do_structure_copy (lhsop, rhsop);
      else
	{
	  struct constraint_expr temp;
	  get_constraint_for (lhsop, &lhsc);

	  if (gimple_assign_rhs_code (t) == POINTER_PLUS_EXPR)
	    get_constraint_for_ptr_offset (gimple_assign_rhs1 (t),
					   gimple_assign_rhs2 (t), &rhsc);
	  else if ((CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (t))
		    && !(POINTER_TYPE_P (gimple_expr_type (t))
			 && !POINTER_TYPE_P (TREE_TYPE (rhsop))))
		   || gimple_assign_single_p (t))
	    get_constraint_for (rhsop, &rhsc);
	  else
	    {
	      temp.type = ADDRESSOF;
	      temp.var = anything_id;
	      temp.offset = 0;
	      VEC_safe_push (ce_s, heap, rhsc, &temp);
	    }
	  process_all_all_constraints (lhsc, rhsc);
	}
      /* If there is a store to a global variable the rhs escapes.  */
      if ((lhsop = get_base_address (lhsop)) != NULL_TREE
	  && DECL_P (lhsop)
	  && is_global_var (lhsop))
	make_escape_constraint (rhsop);
      /* If this is a conversion of a non-restrict pointer to a
	 restrict pointer track it with a new heapvar.  */
      else if (gimple_assign_cast_p (t)
	       && POINTER_TYPE_P (TREE_TYPE (rhsop))
	       && POINTER_TYPE_P (TREE_TYPE (lhsop))
	       && !TYPE_RESTRICT (TREE_TYPE (rhsop))
	       && TYPE_RESTRICT (TREE_TYPE (lhsop)))
	make_constraint_from_restrict (get_vi_for_tree (lhsop),
				       "CAST_RESTRICT");
    }
  /* For conversions of pointers to non-pointers the pointer escapes.  */
  else if (gimple_assign_cast_p (t)
	   && POINTER_TYPE_P (TREE_TYPE (gimple_assign_rhs1 (t)))
	   && !POINTER_TYPE_P (TREE_TYPE (gimple_assign_lhs (t))))
    {
      make_escape_constraint (gimple_assign_rhs1 (t));
    }
  /* Handle escapes through return.  */
  else if (gimple_code (t) == GIMPLE_RETURN
	   && gimple_return_retval (t) != NULL_TREE
	   && could_have_pointers (gimple_return_retval (t)))
    {
      make_escape_constraint (gimple_return_retval (t));
    }
  /* Handle asms conservatively by adding escape constraints to everything.  */
  else if (gimple_code (t) == GIMPLE_ASM)
    {
      unsigned i, noutputs;
      const char **oconstraints;
      const char *constraint;
      bool allows_mem, allows_reg, is_inout;

      noutputs = gimple_asm_noutputs (t);
      oconstraints = XALLOCAVEC (const char *, noutputs);

      for (i = 0; i < noutputs; ++i)
	{
	  tree link = gimple_asm_output_op (t, i);
	  tree op = TREE_VALUE (link);

	  constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));
	  oconstraints[i] = constraint;
	  parse_output_constraint (&constraint, i, 0, 0, &allows_mem,
				   &allows_reg, &is_inout);

	  /* A memory constraint makes the address of the operand escape.  */
	  if (!allows_reg && allows_mem)
	    make_escape_constraint (build_fold_addr_expr (op));

	  /* The asm may read global memory, so outputs may point to
	     any global memory.  */
	  if (op && could_have_pointers (op))
	    {
	      VEC(ce_s, heap) *lhsc = NULL;
	      struct constraint_expr rhsc, *lhsp;
	      unsigned j;
	      get_constraint_for (op, &lhsc);
	      rhsc.var = nonlocal_id;
	      rhsc.offset = 0;
	      rhsc.type = SCALAR;
	      for (j = 0; VEC_iterate (ce_s, lhsc, j, lhsp); j++)
		process_constraint (new_constraint (*lhsp, rhsc));
	      VEC_free (ce_s, heap, lhsc);
	    }
	}
      for (i = 0; i < gimple_asm_ninputs (t); ++i)
	{
	  tree link = gimple_asm_input_op (t, i);
	  tree op = TREE_VALUE (link);

	  constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));

	  parse_input_constraint (&constraint, 0, 0, noutputs, 0, oconstraints,
				  &allows_mem, &allows_reg);

	  /* A memory constraint makes the address of the operand escape.  */
	  if (!allows_reg && allows_mem)
	    make_escape_constraint (build_fold_addr_expr (op));
	  /* Strictly we'd only need the constraint to ESCAPED if
	     the asm clobbers memory, otherwise using CALLUSED
	     would be enough.  */
	  else if (op && could_have_pointers (op))
	    make_escape_constraint (op);
	}
    }

  VEC_free (ce_s, heap, rhsc);
  VEC_free (ce_s, heap, lhsc);
}


/* Find the first varinfo in the same variable as START that overlaps with
   OFFSET.  Return NULL if we can't find one.  */

static varinfo_t
first_vi_for_offset (varinfo_t start, unsigned HOST_WIDE_INT offset)
{
  /* If the offset is outside of the variable, bail out.  */
  if (offset >= start->fullsize)
    return NULL;

  /* If we cannot reach offset from start, lookup the first field
     and start from there.  */
  if (start->offset > offset)
    start = lookup_vi_for_tree (start->decl);

  while (start)
    {
      /* We may not find a variable in the field list with the actual
	 offset when when we have glommed a structure to a variable.
	 In that case, however, offset should still be within the size
	 of the variable. */
      if (offset >= start->offset
	  && (offset - start->offset) < start->size)
	return start;

      start= start->next;
    }

  return NULL;
}

/* Find the first varinfo in the same variable as START that overlaps with
   OFFSET.  If there is no such varinfo the varinfo directly preceding
   OFFSET is returned.  */

static varinfo_t
first_or_preceding_vi_for_offset (varinfo_t start,
				  unsigned HOST_WIDE_INT offset)
{
  /* If we cannot reach offset from start, lookup the first field
     and start from there.  */
  if (start->offset > offset)
    start = lookup_vi_for_tree (start->decl);

  /* We may not find a variable in the field list with the actual
     offset when when we have glommed a structure to a variable.
     In that case, however, offset should still be within the size
     of the variable.
     If we got beyond the offset we look for return the field
     directly preceding offset which may be the last field.  */
  while (start->next
	 && offset >= start->offset
	 && !((offset - start->offset) < start->size))
    start = start->next;

  return start;
}


/* Insert the varinfo FIELD into the field list for BASE, at the front
   of the list.  */

static void
insert_into_field_list (varinfo_t base, varinfo_t field)
{
  varinfo_t prev = base;
  varinfo_t curr = base->next;

  field->next = curr;
  prev->next = field;
}

/* Insert the varinfo FIELD into the field list for BASE, ordered by
   offset.  */

static void
insert_into_field_list_sorted (varinfo_t base, varinfo_t field)
{
  varinfo_t prev = base;
  varinfo_t curr = base->next;

  if (curr == NULL)
    {
      prev->next = field;
      field->next = NULL;
    }
  else
    {
      while (curr)
	{
	  if (field->offset <= curr->offset)
	    break;
	  prev = curr;
	  curr = curr->next;
	}
      field->next = prev->next;
      prev->next = field;
    }
}

/* This structure is used during pushing fields onto the fieldstack
   to track the offset of the field, since bitpos_of_field gives it
   relative to its immediate containing type, and we want it relative
   to the ultimate containing object.  */

struct fieldoff
{
  /* Offset from the base of the base containing object to this field.  */
  HOST_WIDE_INT offset;

  /* Size, in bits, of the field.  */
  unsigned HOST_WIDE_INT size;

  unsigned has_unknown_size : 1;

  unsigned may_have_pointers : 1;

  unsigned only_restrict_pointers : 1;
};
typedef struct fieldoff fieldoff_s;

DEF_VEC_O(fieldoff_s);
DEF_VEC_ALLOC_O(fieldoff_s,heap);

/* qsort comparison function for two fieldoff's PA and PB */

static int
fieldoff_compare (const void *pa, const void *pb)
{
  const fieldoff_s *foa = (const fieldoff_s *)pa;
  const fieldoff_s *fob = (const fieldoff_s *)pb;
  unsigned HOST_WIDE_INT foasize, fobsize;

  if (foa->offset < fob->offset)
    return -1;
  else if (foa->offset > fob->offset)
    return 1;

  foasize = foa->size;
  fobsize = fob->size;
  if (foasize < fobsize)
    return -1;
  else if (foasize > fobsize)
    return 1;
  return 0;
}

/* Sort a fieldstack according to the field offset and sizes.  */
static void
sort_fieldstack (VEC(fieldoff_s,heap) *fieldstack)
{
  qsort (VEC_address (fieldoff_s, fieldstack),
	 VEC_length (fieldoff_s, fieldstack),
	 sizeof (fieldoff_s),
	 fieldoff_compare);
}

/* Return true if V is a tree that we can have subvars for.
   Normally, this is any aggregate type.  Also complex
   types which are not gimple registers can have subvars.  */

static inline bool
var_can_have_subvars (const_tree v)
{
  /* Volatile variables should never have subvars.  */
  if (TREE_THIS_VOLATILE (v))
    return false;

  /* Non decls or memory tags can never have subvars.  */
  if (!DECL_P (v))
    return false;

  /* Aggregates without overlapping fields can have subvars.  */
  if (TREE_CODE (TREE_TYPE (v)) == RECORD_TYPE)
    return true;

  return false;
}

/* Given a TYPE, and a vector of field offsets FIELDSTACK, push all
   the fields of TYPE onto fieldstack, recording their offsets along
   the way.

   OFFSET is used to keep track of the offset in this entire
   structure, rather than just the immediately containing structure.
   Returns the number of fields pushed.  */

static int
push_fields_onto_fieldstack (tree type, VEC(fieldoff_s,heap) **fieldstack,
			     HOST_WIDE_INT offset)
{
  tree field;
  int count = 0;

  if (TREE_CODE (type) != RECORD_TYPE)
    return 0;

  /* If the vector of fields is growing too big, bail out early.
     Callers check for VEC_length <= MAX_FIELDS_FOR_FIELD_SENSITIVE, make
     sure this fails.  */
  if (VEC_length (fieldoff_s, *fieldstack) > MAX_FIELDS_FOR_FIELD_SENSITIVE)
    return 0;

  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    if (TREE_CODE (field) == FIELD_DECL)
      {
	bool push = false;
	int pushed = 0;
	HOST_WIDE_INT foff = bitpos_of_field (field);

	if (!var_can_have_subvars (field)
	    || TREE_CODE (TREE_TYPE (field)) == QUAL_UNION_TYPE
	    || TREE_CODE (TREE_TYPE (field)) == UNION_TYPE)
	  push = true;
	else if (!(pushed = push_fields_onto_fieldstack
		   (TREE_TYPE (field), fieldstack, offset + foff))
		 && (DECL_SIZE (field)
		     && !integer_zerop (DECL_SIZE (field))))
	  /* Empty structures may have actual size, like in C++.  So
	     see if we didn't push any subfields and the size is
	     nonzero, push the field onto the stack.  */
	  push = true;

	if (push)
	  {
	    fieldoff_s *pair = NULL;
	    bool has_unknown_size = false;

	    if (!VEC_empty (fieldoff_s, *fieldstack))
	      pair = VEC_last (fieldoff_s, *fieldstack);

	    if (!DECL_SIZE (field)
		|| !host_integerp (DECL_SIZE (field), 1))
	      has_unknown_size = true;

	    /* If adjacent fields do not contain pointers merge them.  */
	    if (pair
		&& !pair->may_have_pointers
		&& !could_have_pointers (field)
		&& !pair->has_unknown_size
		&& !has_unknown_size
		&& pair->offset + (HOST_WIDE_INT)pair->size == offset + foff)
	      {
		pair = VEC_last (fieldoff_s, *fieldstack);
		pair->size += TREE_INT_CST_LOW (DECL_SIZE (field));
	      }
	    else
	      {
		pair = VEC_safe_push (fieldoff_s, heap, *fieldstack, NULL);
		pair->offset = offset + foff;
		pair->has_unknown_size = has_unknown_size;
		if (!has_unknown_size)
		  pair->size = TREE_INT_CST_LOW (DECL_SIZE (field));
		else
		  pair->size = -1;
		pair->may_have_pointers = could_have_pointers (field);
		pair->only_restrict_pointers
		  = (!has_unknown_size
		     && POINTER_TYPE_P (TREE_TYPE (field))
		     && TYPE_RESTRICT (TREE_TYPE (field)));
		count++;
	      }
	  }
	else
	  count += pushed;
      }

  return count;
}

/* Count the number of arguments DECL has, and set IS_VARARGS to true
   if it is a varargs function.  */

static unsigned int
count_num_arguments (tree decl, bool *is_varargs)
{
  unsigned int num = 0;
  tree t;

  /* Capture named arguments for K&R functions.  They do not
     have a prototype and thus no TYPE_ARG_TYPES.  */
  for (t = DECL_ARGUMENTS (decl); t; t = TREE_CHAIN (t))
    ++num;

  /* Check if the function has variadic arguments.  */
  for (t = TYPE_ARG_TYPES (TREE_TYPE (decl)); t; t = TREE_CHAIN (t))
    if (TREE_VALUE (t) == void_type_node)
      break;
  if (!t)
    *is_varargs = true;

  return num;
}

/* Creation function node for DECL, using NAME, and return the index
   of the variable we've created for the function.  */

static unsigned int
create_function_info_for (tree decl, const char *name)
{
  varinfo_t vi;
  tree arg;
  unsigned int i;
  bool is_varargs = false;

  /* Create the variable info.  */

  vi = new_var_info (decl, name);
  vi->offset = 0;
  vi->size = 1;
  vi->fullsize = count_num_arguments (decl, &is_varargs) + 1;
  insert_vi_for_tree (vi->decl, vi);

  stats.total_vars++;

  /* If it's varargs, we don't know how many arguments it has, so we
     can't do much.  */
  if (is_varargs)
    {
      vi->fullsize = ~0;
      vi->size = ~0;
      vi->is_unknown_size_var = true;
      return vi->id;
    }

  arg = DECL_ARGUMENTS (decl);

  /* Set up variables for each argument.  */
  for (i = 1; i < vi->fullsize; i++)
    {
      varinfo_t argvi;
      const char *newname;
      char *tempname;
      tree argdecl = decl;

      if (arg)
	argdecl = arg;

      asprintf (&tempname, "%s.arg%d", name, i-1);
      newname = ggc_strdup (tempname);
      free (tempname);

      argvi = new_var_info (argdecl, newname);
      argvi->offset = i;
      argvi->size = 1;
      argvi->is_full_var = true;
      argvi->fullsize = vi->fullsize;
      insert_into_field_list_sorted (vi, argvi);
      stats.total_vars ++;
      if (arg)
	{
	  insert_vi_for_tree (arg, argvi);
	  arg = TREE_CHAIN (arg);
	}
    }

  /* Create a variable for the return var.  */
  if (DECL_RESULT (decl) != NULL
      || !VOID_TYPE_P (TREE_TYPE (TREE_TYPE (decl))))
    {
      varinfo_t resultvi;
      const char *newname;
      char *tempname;
      tree resultdecl = decl;

      vi->fullsize ++;

      if (DECL_RESULT (decl))
	resultdecl = DECL_RESULT (decl);

      asprintf (&tempname, "%s.result", name);
      newname = ggc_strdup (tempname);
      free (tempname);

      resultvi = new_var_info (resultdecl, newname);
      resultvi->offset = i;
      resultvi->size = 1;
      resultvi->fullsize = vi->fullsize;
      resultvi->is_full_var = true;
      insert_into_field_list_sorted (vi, resultvi);
      stats.total_vars ++;
      if (DECL_RESULT (decl))
	insert_vi_for_tree (DECL_RESULT (decl), resultvi);
    }

  return vi->id;
}


/* Return true if FIELDSTACK contains fields that overlap.
   FIELDSTACK is assumed to be sorted by offset.  */

static bool
check_for_overlaps (VEC (fieldoff_s,heap) *fieldstack)
{
  fieldoff_s *fo = NULL;
  unsigned int i;
  HOST_WIDE_INT lastoffset = -1;

  for (i = 0; VEC_iterate (fieldoff_s, fieldstack, i, fo); i++)
    {
      if (fo->offset == lastoffset)
	return true;
      lastoffset = fo->offset;
    }
  return false;
}

/* Create a varinfo structure for NAME and DECL, and add it to VARMAP.
   This will also create any varinfo structures necessary for fields
   of DECL.  */

static unsigned int
create_variable_info_for (tree decl, const char *name)
{
  varinfo_t vi;
  tree decl_type = TREE_TYPE (decl);
  tree declsize = DECL_P (decl) ? DECL_SIZE (decl) : TYPE_SIZE (decl_type);
  VEC (fieldoff_s,heap) *fieldstack = NULL;

  if (var_can_have_subvars (decl) && use_field_sensitive)
    push_fields_onto_fieldstack (decl_type, &fieldstack, 0);

  /* If the variable doesn't have subvars, we may end up needing to
     sort the field list and create fake variables for all the
     fields.  */
  vi = new_var_info (decl, name);
  vi->offset = 0;
  vi->may_have_pointers = could_have_pointers (decl);
  if (!declsize
      || !host_integerp (declsize, 1))
    {
      vi->is_unknown_size_var = true;
      vi->fullsize = ~0;
      vi->size = ~0;
    }
  else
    {
      vi->fullsize = TREE_INT_CST_LOW (declsize);
      vi->size = vi->fullsize;
    }

  insert_vi_for_tree (vi->decl, vi);
  if (vi->is_global_var
      && (!flag_whole_program || !in_ipa_mode)
      && vi->may_have_pointers)
    {
      if (POINTER_TYPE_P (TREE_TYPE (decl))
	  && TYPE_RESTRICT (TREE_TYPE (decl)))
	make_constraint_from_restrict (vi, "GLOBAL_RESTRICT");
      make_copy_constraint (vi, nonlocal_id);
    }

  stats.total_vars++;
  if (use_field_sensitive
      && !vi->is_unknown_size_var
      && var_can_have_subvars (decl)
      && VEC_length (fieldoff_s, fieldstack) > 1
      && VEC_length (fieldoff_s, fieldstack) <= MAX_FIELDS_FOR_FIELD_SENSITIVE)
    {
      fieldoff_s *fo = NULL;
      bool notokay = false;
      unsigned int i;

      for (i = 0; !notokay && VEC_iterate (fieldoff_s, fieldstack, i, fo); i++)
	{
	  if (fo->has_unknown_size
	      || fo->offset < 0)
	    {
	      notokay = true;
	      break;
	    }
	}

      /* We can't sort them if we have a field with a variable sized type,
	 which will make notokay = true.  In that case, we are going to return
	 without creating varinfos for the fields anyway, so sorting them is a
	 waste to boot.  */
      if (!notokay)
	{
	  sort_fieldstack (fieldstack);
	  /* Due to some C++ FE issues, like PR 22488, we might end up
	     what appear to be overlapping fields even though they,
	     in reality, do not overlap.  Until the C++ FE is fixed,
	     we will simply disable field-sensitivity for these cases.  */
	  notokay = check_for_overlaps (fieldstack);
	}


      if (VEC_length (fieldoff_s, fieldstack) != 0)
	fo = VEC_index (fieldoff_s, fieldstack, 0);

      if (fo == NULL || notokay)
	{
	  vi->is_unknown_size_var = 1;
	  vi->fullsize = ~0;
	  vi->size = ~0;
	  vi->is_full_var = true;
	  VEC_free (fieldoff_s, heap, fieldstack);
	  return vi->id;
	}

      vi->size = fo->size;
      vi->offset = fo->offset;
      vi->may_have_pointers = fo->may_have_pointers;
      if (vi->is_global_var
	  && (!flag_whole_program || !in_ipa_mode)
	  && vi->may_have_pointers)
	{
	  if (fo->only_restrict_pointers)
	    make_constraint_from_restrict (vi, "GLOBAL_RESTRICT");
	}
      for (i = VEC_length (fieldoff_s, fieldstack) - 1;
	   i >= 1 && VEC_iterate (fieldoff_s, fieldstack, i, fo);
	   i--)
	{
	  varinfo_t newvi;
	  const char *newname = "NULL";
	  char *tempname;

	  if (dump_file)
	    {
	      asprintf (&tempname, "%s." HOST_WIDE_INT_PRINT_DEC
			"+" HOST_WIDE_INT_PRINT_DEC,
			vi->name, fo->offset, fo->size);
	      newname = ggc_strdup (tempname);
	      free (tempname);
	    }
	  newvi = new_var_info (decl, newname);
	  newvi->offset = fo->offset;
	  newvi->size = fo->size;
	  newvi->fullsize = vi->fullsize;
	  newvi->may_have_pointers = fo->may_have_pointers;
	  insert_into_field_list (vi, newvi);
	  if ((newvi->is_global_var || TREE_CODE (decl) == PARM_DECL)
	      && newvi->may_have_pointers)
	    {
	       if (fo->only_restrict_pointers)
		 make_constraint_from_restrict (newvi, "GLOBAL_RESTRICT");
	       if (newvi->is_global_var && !in_ipa_mode)
		 make_copy_constraint (newvi, nonlocal_id);
	    }

	  stats.total_vars++;
	}
    }
  else
    vi->is_full_var = true;

  VEC_free (fieldoff_s, heap, fieldstack);

  return vi->id;
}

/* Print out the points-to solution for VAR to FILE.  */

static void
dump_solution_for_var (FILE *file, unsigned int var)
{
  varinfo_t vi = get_varinfo (var);
  unsigned int i;
  bitmap_iterator bi;

  if (find (var) != var)
    {
      varinfo_t vipt = get_varinfo (find (var));
      fprintf (file, "%s = same as %s\n", vi->name, vipt->name);
    }
  else
    {
      fprintf (file, "%s = { ", vi->name);
      EXECUTE_IF_SET_IN_BITMAP (vi->solution, 0, i, bi)
	{
	  fprintf (file, "%s ", get_varinfo (i)->name);
	}
      fprintf (file, "}\n");
    }
}

/* Print the points-to solution for VAR to stdout.  */

void
debug_solution_for_var (unsigned int var)
{
  dump_solution_for_var (stdout, var);
}

/* Create varinfo structures for all of the variables in the
   function for intraprocedural mode.  */

static void
intra_create_variable_infos (void)
{
  tree t;

  /* For each incoming pointer argument arg, create the constraint ARG
     = NONLOCAL or a dummy variable if flag_argument_noalias is set.  */
  for (t = DECL_ARGUMENTS (current_function_decl); t; t = TREE_CHAIN (t))
    {
      varinfo_t p;

      if (!could_have_pointers (t))
	continue;

      /* For restrict qualified pointers to objects passed by
         reference build a real representative for the pointed-to object.  */
      if (DECL_BY_REFERENCE (t)
	  && POINTER_TYPE_P (TREE_TYPE (t))
	  && TYPE_RESTRICT (TREE_TYPE (t)))
	{
	  struct constraint_expr lhsc, rhsc;
	  varinfo_t vi;
	  tree heapvar = heapvar_lookup (t, 0);
	  if (heapvar == NULL_TREE)
	    {
	      var_ann_t ann;
	      heapvar = create_tmp_var_raw (TREE_TYPE (TREE_TYPE (t)),
					    "PARM_NOALIAS");
	      DECL_EXTERNAL (heapvar) = 1;
	      heapvar_insert (t, 0, heapvar);
	      ann = get_var_ann (heapvar);
	      ann->is_heapvar = 1;
	    }
	  if (gimple_referenced_vars (cfun))
	    add_referenced_var (heapvar);
	  lhsc.var = get_vi_for_tree (t)->id;
	  lhsc.type = SCALAR;
	  lhsc.offset = 0;
	  rhsc.var = (vi = get_vi_for_tree (heapvar))->id;
	  rhsc.type = ADDRESSOF;
	  rhsc.offset = 0;
	  process_constraint (new_constraint (lhsc, rhsc));
	  vi->is_restrict_var = 1;
	  continue;
	}

      for (p = get_vi_for_tree (t); p; p = p->next)
	if (p->may_have_pointers)
	  make_constraint_from (p, nonlocal_id);
      if (POINTER_TYPE_P (TREE_TYPE (t))
	  && TYPE_RESTRICT (TREE_TYPE (t)))
	make_constraint_from_restrict (get_vi_for_tree (t), "PARM_RESTRICT");
    }

  /* Add a constraint for a result decl that is passed by reference.  */
  if (DECL_RESULT (cfun->decl)
      && DECL_BY_REFERENCE (DECL_RESULT (cfun->decl)))
    {
      varinfo_t p, result_vi = get_vi_for_tree (DECL_RESULT (cfun->decl));

      for (p = result_vi; p; p = p->next)
	make_constraint_from (p, nonlocal_id);
    }

  /* Add a constraint for the incoming static chain parameter.  */
  if (cfun->static_chain_decl != NULL_TREE)
    {
      varinfo_t p, chain_vi = get_vi_for_tree (cfun->static_chain_decl);

      for (p = chain_vi; p; p = p->next)
	make_constraint_from (p, nonlocal_id);
    }
}

/* Structure used to put solution bitmaps in a hashtable so they can
   be shared among variables with the same points-to set.  */

typedef struct shared_bitmap_info
{
  bitmap pt_vars;
  hashval_t hashcode;
} *shared_bitmap_info_t;
typedef const struct shared_bitmap_info *const_shared_bitmap_info_t;

static htab_t shared_bitmap_table;

/* Hash function for a shared_bitmap_info_t */

static hashval_t
shared_bitmap_hash (const void *p)
{
  const_shared_bitmap_info_t const bi = (const_shared_bitmap_info_t) p;
  return bi->hashcode;
}

/* Equality function for two shared_bitmap_info_t's. */

static int
shared_bitmap_eq (const void *p1, const void *p2)
{
  const_shared_bitmap_info_t const sbi1 = (const_shared_bitmap_info_t) p1;
  const_shared_bitmap_info_t const sbi2 = (const_shared_bitmap_info_t) p2;
  return bitmap_equal_p (sbi1->pt_vars, sbi2->pt_vars);
}

/* Lookup a bitmap in the shared bitmap hashtable, and return an already
   existing instance if there is one, NULL otherwise.  */

static bitmap
shared_bitmap_lookup (bitmap pt_vars)
{
  void **slot;
  struct shared_bitmap_info sbi;

  sbi.pt_vars = pt_vars;
  sbi.hashcode = bitmap_hash (pt_vars);

  slot = htab_find_slot_with_hash (shared_bitmap_table, &sbi,
				   sbi.hashcode, NO_INSERT);
  if (!slot)
    return NULL;
  else
    return ((shared_bitmap_info_t) *slot)->pt_vars;
}


/* Add a bitmap to the shared bitmap hashtable.  */

static void
shared_bitmap_add (bitmap pt_vars)
{
  void **slot;
  shared_bitmap_info_t sbi = XNEW (struct shared_bitmap_info);

  sbi->pt_vars = pt_vars;
  sbi->hashcode = bitmap_hash (pt_vars);

  slot = htab_find_slot_with_hash (shared_bitmap_table, sbi,
				   sbi->hashcode, INSERT);
  gcc_assert (!*slot);
  *slot = (void *) sbi;
}


/* Set bits in INTO corresponding to the variable uids in solution set FROM.  */

static void
set_uids_in_ptset (bitmap into, bitmap from, struct pt_solution *pt)
{
  unsigned int i;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (from, 0, i, bi)
    {
      varinfo_t vi = get_varinfo (i);

      /* The only artificial variables that are allowed in a may-alias
	 set are heap variables.  */
      if (vi->is_artificial_var && !vi->is_heap_var)
	continue;

      if (TREE_CODE (vi->decl) == VAR_DECL
	  || TREE_CODE (vi->decl) == PARM_DECL
	  || TREE_CODE (vi->decl) == RESULT_DECL)
	{
	  /* Add the decl to the points-to set.  Note that the points-to
	     set contains global variables.  */
	  bitmap_set_bit (into, DECL_UID (vi->decl));
	  if (vi->is_global_var)
	    pt->vars_contains_global = true;
	}
    }
}


/* Compute the points-to solution *PT for the variable VI.  */

static void
find_what_var_points_to (varinfo_t orig_vi, struct pt_solution *pt)
{
  unsigned int i;
  bitmap_iterator bi;
  bitmap finished_solution;
  bitmap result;
  varinfo_t vi;

  memset (pt, 0, sizeof (struct pt_solution));

  /* This variable may have been collapsed, let's get the real
     variable.  */
  vi = get_varinfo (find (orig_vi->id));

  /* Translate artificial variables into SSA_NAME_PTR_INFO
     attributes.  */
  EXECUTE_IF_SET_IN_BITMAP (vi->solution, 0, i, bi)
    {
      varinfo_t vi = get_varinfo (i);

      if (vi->is_artificial_var)
	{
	  if (vi->id == nothing_id)
	    pt->null = 1;
	  else if (vi->id == escaped_id)
	    pt->escaped = 1;
	  else if (vi->id == callused_id)
	    gcc_unreachable ();
	  else if (vi->id == nonlocal_id)
	    pt->nonlocal = 1;
	  else if (vi->is_heap_var)
	    /* We represent heapvars in the points-to set properly.  */
	    ;
	  else if (vi->id == readonly_id)
	    /* Nobody cares.  */
	    ;
	  else if (vi->id == anything_id
		   || vi->id == integer_id)
	    pt->anything = 1;
	}
      if (vi->is_restrict_var)
	pt->vars_contains_restrict = true;
    }

  /* Instead of doing extra work, simply do not create
     elaborate points-to information for pt_anything pointers.  */
  if (pt->anything
      && (orig_vi->is_artificial_var
	  || !pt->vars_contains_restrict))
    return;

  /* Share the final set of variables when possible.  */
  finished_solution = BITMAP_GGC_ALLOC ();
  stats.points_to_sets_created++;

  set_uids_in_ptset (finished_solution, vi->solution, pt);
  result = shared_bitmap_lookup (finished_solution);
  if (!result)
    {
      shared_bitmap_add (finished_solution);
      pt->vars = finished_solution;
    }
  else
    {
      pt->vars = result;
      bitmap_clear (finished_solution);
    }
}

/* Given a pointer variable P, fill in its points-to set.  */

static void
find_what_p_points_to (tree p)
{
  struct ptr_info_def *pi;
  tree lookup_p = p;
  varinfo_t vi;

  /* For parameters, get at the points-to set for the actual parm
     decl.  */
  if (TREE_CODE (p) == SSA_NAME
      && TREE_CODE (SSA_NAME_VAR (p)) == PARM_DECL
      && SSA_NAME_IS_DEFAULT_DEF (p))
    lookup_p = SSA_NAME_VAR (p);

  vi = lookup_vi_for_tree (lookup_p);
  if (!vi)
    return;

  pi = get_ptr_info (p);
  find_what_var_points_to (vi, &pi->pt);
}


/* Query statistics for points-to solutions.  */

static struct {
  unsigned HOST_WIDE_INT pt_solution_includes_may_alias;
  unsigned HOST_WIDE_INT pt_solution_includes_no_alias;
  unsigned HOST_WIDE_INT pt_solutions_intersect_may_alias;
  unsigned HOST_WIDE_INT pt_solutions_intersect_no_alias;
} pta_stats;

void
dump_pta_stats (FILE *s)
{
  fprintf (s, "\nPTA query stats:\n");
  fprintf (s, "  pt_solution_includes: "
	   HOST_WIDE_INT_PRINT_DEC" disambiguations, "
	   HOST_WIDE_INT_PRINT_DEC" queries\n",
	   pta_stats.pt_solution_includes_no_alias,
	   pta_stats.pt_solution_includes_no_alias
	   + pta_stats.pt_solution_includes_may_alias);
  fprintf (s, "  pt_solutions_intersect: "
	   HOST_WIDE_INT_PRINT_DEC" disambiguations, "
	   HOST_WIDE_INT_PRINT_DEC" queries\n",
	   pta_stats.pt_solutions_intersect_no_alias,
	   pta_stats.pt_solutions_intersect_no_alias
	   + pta_stats.pt_solutions_intersect_may_alias);
}


/* Reset the points-to solution *PT to a conservative default
   (point to anything).  */

void
pt_solution_reset (struct pt_solution *pt)
{
  memset (pt, 0, sizeof (struct pt_solution));
  pt->anything = true;
}

/* Set the points-to solution *PT to point only to the variables
   in VARS.  */

void
pt_solution_set (struct pt_solution *pt, bitmap vars)
{
  bitmap_iterator bi;
  unsigned i;

  memset (pt, 0, sizeof (struct pt_solution));
  pt->vars = vars;
  EXECUTE_IF_SET_IN_BITMAP (vars, 0, i, bi)
    {
      tree var = referenced_var_lookup (i);
      if (is_global_var (var))
	{
	  pt->vars_contains_global = true;
	  break;
	}
    }
}

/* Return true if the points-to solution *PT is empty.  */

static bool
pt_solution_empty_p (struct pt_solution *pt)
{
  if (pt->anything
      || pt->nonlocal)
    return false;

  if (pt->vars
      && !bitmap_empty_p (pt->vars))
    return false;

  /* If the solution includes ESCAPED, check if that is empty.  */
  if (pt->escaped
      && !pt_solution_empty_p (&cfun->gimple_df->escaped))
    return false;

  return true;
}

/* Return true if the points-to solution *PT includes global memory.  */

bool
pt_solution_includes_global (struct pt_solution *pt)
{
  if (pt->anything
      || pt->nonlocal
      || pt->vars_contains_global)
    return true;

  if (pt->escaped)
    return pt_solution_includes_global (&cfun->gimple_df->escaped);

  return false;
}

/* Return true if the points-to solution *PT includes the variable
   declaration DECL.  */

static bool
pt_solution_includes_1 (struct pt_solution *pt, const_tree decl)
{
  if (pt->anything)
    return true;

  if (pt->nonlocal
      && is_global_var (decl))
    return true;

  if (pt->vars
      && bitmap_bit_p (pt->vars, DECL_UID (decl)))
    return true;

  /* If the solution includes ESCAPED, check it.  */
  if (pt->escaped
      && pt_solution_includes_1 (&cfun->gimple_df->escaped, decl))
    return true;

  return false;
}

bool
pt_solution_includes (struct pt_solution *pt, const_tree decl)
{
  bool res = pt_solution_includes_1 (pt, decl);
  if (res)
    ++pta_stats.pt_solution_includes_may_alias;
  else
    ++pta_stats.pt_solution_includes_no_alias;
  return res;
}

/* Return true if both points-to solutions PT1 and PT2 have a non-empty
   intersection.  */

static bool
pt_solutions_intersect_1 (struct pt_solution *pt1, struct pt_solution *pt2)
{
  if (pt1->anything || pt2->anything)
    return true;

  /* If either points to unknown global memory and the other points to
     any global memory they alias.  */
  if ((pt1->nonlocal
       && (pt2->nonlocal
	   || pt2->vars_contains_global))
      || (pt2->nonlocal
	  && pt1->vars_contains_global))
    return true;

  /* Check the escaped solution if required.  */
  if ((pt1->escaped || pt2->escaped)
      && !pt_solution_empty_p (&cfun->gimple_df->escaped))
    {
      /* If both point to escaped memory and that solution
	 is not empty they alias.  */
      if (pt1->escaped && pt2->escaped)
	return true;

      /* If either points to escaped memory see if the escaped solution
	 intersects with the other.  */
      if ((pt1->escaped
	   && pt_solutions_intersect_1 (&cfun->gimple_df->escaped, pt2))
	  || (pt2->escaped
	      && pt_solutions_intersect_1 (&cfun->gimple_df->escaped, pt1)))
	return true;
    }

  /* Now both pointers alias if their points-to solution intersects.  */
  return (pt1->vars
	  && pt2->vars
	  && bitmap_intersect_p (pt1->vars, pt2->vars));
}

bool
pt_solutions_intersect (struct pt_solution *pt1, struct pt_solution *pt2)
{
  bool res = pt_solutions_intersect_1 (pt1, pt2);
  if (res)
    ++pta_stats.pt_solutions_intersect_may_alias;
  else
    ++pta_stats.pt_solutions_intersect_no_alias;
  return res;
}

/* Return true if both points-to solutions PT1 and PT2 for two restrict
   qualified pointers are possibly based on the same pointer.  */

bool
pt_solutions_same_restrict_base (struct pt_solution *pt1,
				 struct pt_solution *pt2)
{
  /* If we deal with points-to solutions of two restrict qualified
     pointers solely rely on the pointed-to variable bitmap intersection.
     For two pointers that are based on each other the bitmaps will
     intersect.  */
  if (pt1->vars_contains_restrict
      && pt2->vars_contains_restrict)
    {
      gcc_assert (pt1->vars && pt2->vars);
      return bitmap_intersect_p (pt1->vars, pt2->vars);
    }

  return true;
}


/* Dump points-to information to OUTFILE.  */

static void
dump_sa_points_to_info (FILE *outfile)
{
  unsigned int i;

  fprintf (outfile, "\nPoints-to sets\n\n");

  if (dump_flags & TDF_STATS)
    {
      fprintf (outfile, "Stats:\n");
      fprintf (outfile, "Total vars:               %d\n", stats.total_vars);
      fprintf (outfile, "Non-pointer vars:          %d\n",
	       stats.nonpointer_vars);
      fprintf (outfile, "Statically unified vars:  %d\n",
	       stats.unified_vars_static);
      fprintf (outfile, "Dynamically unified vars: %d\n",
	       stats.unified_vars_dynamic);
      fprintf (outfile, "Iterations:               %d\n", stats.iterations);
      fprintf (outfile, "Number of edges:          %d\n", stats.num_edges);
      fprintf (outfile, "Number of implicit edges: %d\n",
	       stats.num_implicit_edges);
    }

  for (i = 0; i < VEC_length (varinfo_t, varmap); i++)
    dump_solution_for_var (outfile, i);
}


/* Debug points-to information to stderr.  */

void
debug_sa_points_to_info (void)
{
  dump_sa_points_to_info (stderr);
}


/* Initialize the always-existing constraint variables for NULL
   ANYTHING, READONLY, and INTEGER */

static void
init_base_vars (void)
{
  struct constraint_expr lhs, rhs;
  varinfo_t var_anything;
  varinfo_t var_nothing;
  varinfo_t var_readonly;
  varinfo_t var_escaped;
  varinfo_t var_nonlocal;
  varinfo_t var_callused;
  varinfo_t var_storedanything;
  varinfo_t var_integer;

  /* Create the NULL variable, used to represent that a variable points
     to NULL.  */
  var_nothing = new_var_info (NULL_TREE, "NULL");
  gcc_assert (var_nothing->id == nothing_id);
  var_nothing->is_artificial_var = 1;
  var_nothing->offset = 0;
  var_nothing->size = ~0;
  var_nothing->fullsize = ~0;
  var_nothing->is_special_var = 1;

  /* Create the ANYTHING variable, used to represent that a variable
     points to some unknown piece of memory.  */
  var_anything = new_var_info (NULL_TREE, "ANYTHING");
  gcc_assert (var_anything->id == anything_id);
  var_anything->is_artificial_var = 1;
  var_anything->size = ~0;
  var_anything->offset = 0;
  var_anything->next = NULL;
  var_anything->fullsize = ~0;
  var_anything->is_special_var = 1;

  /* Anything points to anything.  This makes deref constraints just
     work in the presence of linked list and other p = *p type loops,
     by saying that *ANYTHING = ANYTHING. */
  lhs.type = SCALAR;
  lhs.var = anything_id;
  lhs.offset = 0;
  rhs.type = ADDRESSOF;
  rhs.var = anything_id;
  rhs.offset = 0;

  /* This specifically does not use process_constraint because
     process_constraint ignores all anything = anything constraints, since all
     but this one are redundant.  */
  VEC_safe_push (constraint_t, heap, constraints, new_constraint (lhs, rhs));

  /* Create the READONLY variable, used to represent that a variable
     points to readonly memory.  */
  var_readonly = new_var_info (NULL_TREE, "READONLY");
  gcc_assert (var_readonly->id == readonly_id);
  var_readonly->is_artificial_var = 1;
  var_readonly->offset = 0;
  var_readonly->size = ~0;
  var_readonly->fullsize = ~0;
  var_readonly->next = NULL;
  var_readonly->is_special_var = 1;

  /* readonly memory points to anything, in order to make deref
     easier.  In reality, it points to anything the particular
     readonly variable can point to, but we don't track this
     separately. */
  lhs.type = SCALAR;
  lhs.var = readonly_id;
  lhs.offset = 0;
  rhs.type = ADDRESSOF;
  rhs.var = readonly_id;  /* FIXME */
  rhs.offset = 0;
  process_constraint (new_constraint (lhs, rhs));

  /* Create the ESCAPED variable, used to represent the set of escaped
     memory.  */
  var_escaped = new_var_info (NULL_TREE, "ESCAPED");
  gcc_assert (var_escaped->id == escaped_id);
  var_escaped->is_artificial_var = 1;
  var_escaped->offset = 0;
  var_escaped->size = ~0;
  var_escaped->fullsize = ~0;
  var_escaped->is_special_var = 0;

  /* Create the NONLOCAL variable, used to represent the set of nonlocal
     memory.  */
  var_nonlocal = new_var_info (NULL_TREE, "NONLOCAL");
  gcc_assert (var_nonlocal->id == nonlocal_id);
  var_nonlocal->is_artificial_var = 1;
  var_nonlocal->offset = 0;
  var_nonlocal->size = ~0;
  var_nonlocal->fullsize = ~0;
  var_nonlocal->is_special_var = 1;

  /* ESCAPED = *ESCAPED, because escaped is may-deref'd at calls, etc.  */
  lhs.type = SCALAR;
  lhs.var = escaped_id;
  lhs.offset = 0;
  rhs.type = DEREF;
  rhs.var = escaped_id;
  rhs.offset = 0;
  process_constraint (new_constraint (lhs, rhs));

  /* ESCAPED = ESCAPED + UNKNOWN_OFFSET, because if a sub-field escapes the
     whole variable escapes.  */
  lhs.type = SCALAR;
  lhs.var = escaped_id;
  lhs.offset = 0;
  rhs.type = SCALAR;
  rhs.var = escaped_id;
  rhs.offset = UNKNOWN_OFFSET;
  process_constraint (new_constraint (lhs, rhs));

  /* *ESCAPED = NONLOCAL.  This is true because we have to assume
     everything pointed to by escaped points to what global memory can
     point to.  */
  lhs.type = DEREF;
  lhs.var = escaped_id;
  lhs.offset = 0;
  rhs.type = SCALAR;
  rhs.var = nonlocal_id;
  rhs.offset = 0;
  process_constraint (new_constraint (lhs, rhs));

  /* NONLOCAL = &NONLOCAL, NONLOCAL = &ESCAPED.  This is true because
     global memory may point to global memory and escaped memory.  */
  lhs.type = SCALAR;
  lhs.var = nonlocal_id;
  lhs.offset = 0;
  rhs.type = ADDRESSOF;
  rhs.var = nonlocal_id;
  rhs.offset = 0;
  process_constraint (new_constraint (lhs, rhs));
  rhs.type = ADDRESSOF;
  rhs.var = escaped_id;
  rhs.offset = 0;
  process_constraint (new_constraint (lhs, rhs));

  /* Create the CALLUSED variable, used to represent the set of call-used
     memory.  */
  var_callused = new_var_info (NULL_TREE, "CALLUSED");
  gcc_assert (var_callused->id == callused_id);
  var_callused->is_artificial_var = 1;
  var_callused->offset = 0;
  var_callused->size = ~0;
  var_callused->fullsize = ~0;
  var_callused->is_special_var = 0;

  /* CALLUSED = *CALLUSED, because call-used is may-deref'd at calls, etc.  */
  lhs.type = SCALAR;
  lhs.var = callused_id;
  lhs.offset = 0;
  rhs.type = DEREF;
  rhs.var = callused_id;
  rhs.offset = 0;
  process_constraint (new_constraint (lhs, rhs));

  /* CALLUSED = CALLUSED + UNKNOWN, because if a sub-field is call-used the
     whole variable is call-used.  */
  lhs.type = SCALAR;
  lhs.var = callused_id;
  lhs.offset = 0;
  rhs.type = SCALAR;
  rhs.var = callused_id;
  rhs.offset = UNKNOWN_OFFSET;
  process_constraint (new_constraint (lhs, rhs));

  /* Create the STOREDANYTHING variable, used to represent the set of
     variables stored to *ANYTHING.  */
  var_storedanything = new_var_info (NULL_TREE, "STOREDANYTHING");
  gcc_assert (var_storedanything->id == storedanything_id);
  var_storedanything->is_artificial_var = 1;
  var_storedanything->offset = 0;
  var_storedanything->size = ~0;
  var_storedanything->fullsize = ~0;
  var_storedanything->is_special_var = 0;

  /* Create the INTEGER variable, used to represent that a variable points
     to what an INTEGER "points to".  */
  var_integer = new_var_info (NULL_TREE, "INTEGER");
  gcc_assert (var_integer->id == integer_id);
  var_integer->is_artificial_var = 1;
  var_integer->size = ~0;
  var_integer->fullsize = ~0;
  var_integer->offset = 0;
  var_integer->next = NULL;
  var_integer->is_special_var = 1;

  /* INTEGER = ANYTHING, because we don't know where a dereference of
     a random integer will point to.  */
  lhs.type = SCALAR;
  lhs.var = integer_id;
  lhs.offset = 0;
  rhs.type = ADDRESSOF;
  rhs.var = anything_id;
  rhs.offset = 0;
  process_constraint (new_constraint (lhs, rhs));
}

/* Initialize things necessary to perform PTA */

static void
init_alias_vars (void)
{
  use_field_sensitive = (MAX_FIELDS_FOR_FIELD_SENSITIVE > 1);

  bitmap_obstack_initialize (&pta_obstack);
  bitmap_obstack_initialize (&oldpta_obstack);
  bitmap_obstack_initialize (&predbitmap_obstack);

  constraint_pool = create_alloc_pool ("Constraint pool",
				       sizeof (struct constraint), 30);
  variable_info_pool = create_alloc_pool ("Variable info pool",
					  sizeof (struct variable_info), 30);
  constraints = VEC_alloc (constraint_t, heap, 8);
  varmap = VEC_alloc (varinfo_t, heap, 8);
  vi_for_tree = pointer_map_create ();

  memset (&stats, 0, sizeof (stats));
  shared_bitmap_table = htab_create (511, shared_bitmap_hash,
				     shared_bitmap_eq, free);
  init_base_vars ();
}

/* Remove the REF and ADDRESS edges from GRAPH, as well as all the
   predecessor edges.  */

static void
remove_preds_and_fake_succs (constraint_graph_t graph)
{
  unsigned int i;

  /* Clear the implicit ref and address nodes from the successor
     lists.  */
  for (i = 0; i < FIRST_REF_NODE; i++)
    {
      if (graph->succs[i])
	bitmap_clear_range (graph->succs[i], FIRST_REF_NODE,
			    FIRST_REF_NODE * 2);
    }

  /* Free the successor list for the non-ref nodes.  */
  for (i = FIRST_REF_NODE; i < graph->size; i++)
    {
      if (graph->succs[i])
	BITMAP_FREE (graph->succs[i]);
    }

  /* Now reallocate the size of the successor list as, and blow away
     the predecessor bitmaps.  */
  graph->size = VEC_length (varinfo_t, varmap);
  graph->succs = XRESIZEVEC (bitmap, graph->succs, graph->size);

  free (graph->implicit_preds);
  graph->implicit_preds = NULL;
  free (graph->preds);
  graph->preds = NULL;
  bitmap_obstack_release (&predbitmap_obstack);
}

/* Initialize the heapvar for statement mapping.  */

static void
init_alias_heapvars (void)
{
  if (!heapvar_for_stmt)
    heapvar_for_stmt = htab_create_ggc (11, tree_map_hash, heapvar_map_eq,
					NULL);
}

/* Delete the heapvar for statement mapping.  */

void
delete_alias_heapvars (void)
{
  if (heapvar_for_stmt)
    htab_delete (heapvar_for_stmt);
  heapvar_for_stmt = NULL;
}

/* Solve the constraint set.  */

static void
solve_constraints (void)
{
  struct scc_info *si;

  if (dump_file)
    {
      fprintf (dump_file, "Points-to analysis\n\nConstraints:\n\n");
      dump_constraints (dump_file);
    }

  if (dump_file)
    fprintf (dump_file,
	     "\nCollapsing static cycles and doing variable "
	     "substitution\n");

  init_graph (VEC_length (varinfo_t, varmap) * 2);

  if (dump_file)
    fprintf (dump_file, "Building predecessor graph\n");
  build_pred_graph ();

  if (dump_file)
    fprintf (dump_file, "Detecting pointer and location "
	     "equivalences\n");
  si = perform_var_substitution (graph);

  if (dump_file)
    fprintf (dump_file, "Rewriting constraints and unifying "
	     "variables\n");
  rewrite_constraints (graph, si);

  build_succ_graph ();
  free_var_substitution_info (si);

  if (dump_file && (dump_flags & TDF_GRAPH))
    dump_constraint_graph (dump_file);

  move_complex_constraints (graph);

  if (dump_file)
    fprintf (dump_file, "Uniting pointer but not location equivalent "
	     "variables\n");
  unite_pointer_equivalences (graph);

  if (dump_file)
    fprintf (dump_file, "Finding indirect cycles\n");
  find_indirect_cycles (graph);

  /* Implicit nodes and predecessors are no longer necessary at this
     point. */
  remove_preds_and_fake_succs (graph);

  if (dump_file)
    fprintf (dump_file, "Solving graph\n");

  solve_graph (graph);

  if (dump_file)
    dump_sa_points_to_info (dump_file);
}

/* Create points-to sets for the current function.  See the comments
   at the start of the file for an algorithmic overview.  */

static void
compute_points_to_sets (void)
{
  basic_block bb;
  unsigned i;
  varinfo_t vi;

  timevar_push (TV_TREE_PTA);

  init_alias_vars ();
  init_alias_heapvars ();

  intra_create_variable_infos ();

  /* Now walk all statements and derive aliases.  */
  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator gsi;

      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple phi = gsi_stmt (gsi);

	  if (is_gimple_reg (gimple_phi_result (phi)))
	    find_func_aliases (phi);
	}

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);

	  find_func_aliases (stmt);
	}
    }

  /* From the constraints compute the points-to sets.  */
  solve_constraints ();

  /* Compute the points-to sets for ESCAPED and CALLUSED used for
     call-clobber analysis.  */
  find_what_var_points_to (get_varinfo (escaped_id),
			   &cfun->gimple_df->escaped);
  find_what_var_points_to (get_varinfo (callused_id),
			   &cfun->gimple_df->callused);

  /* Make sure the ESCAPED solution (which is used as placeholder in
     other solutions) does not reference itself.  This simplifies
     points-to solution queries.  */
  cfun->gimple_df->escaped.escaped = 0;

  /* Mark escaped HEAP variables as global.  */
  for (i = 0; VEC_iterate (varinfo_t, varmap, i, vi); ++i)
    if (vi->is_heap_var
	&& !vi->is_restrict_var
	&& !vi->is_global_var)
      DECL_EXTERNAL (vi->decl) = vi->is_global_var
	= pt_solution_includes (&cfun->gimple_df->escaped, vi->decl);

  /* Compute the points-to sets for pointer SSA_NAMEs.  */
  for (i = 0; i < num_ssa_names; ++i)
    {
      tree ptr = ssa_name (i);
      if (ptr
	  && POINTER_TYPE_P (TREE_TYPE (ptr)))
	find_what_p_points_to (ptr);
    }

  timevar_pop (TV_TREE_PTA);
}


/* Delete created points-to sets.  */

static void
delete_points_to_sets (void)
{
  unsigned int i;

  htab_delete (shared_bitmap_table);
  if (dump_file && (dump_flags & TDF_STATS))
    fprintf (dump_file, "Points to sets created:%d\n",
	     stats.points_to_sets_created);

  pointer_map_destroy (vi_for_tree);
  bitmap_obstack_release (&pta_obstack);
  VEC_free (constraint_t, heap, constraints);

  for (i = 0; i < graph->size; i++)
    VEC_free (constraint_t, heap, graph->complex[i]);
  free (graph->complex);

  free (graph->rep);
  free (graph->succs);
  free (graph->pe);
  free (graph->pe_rep);
  free (graph->indirect_cycles);
  free (graph);

  VEC_free (varinfo_t, heap, varmap);
  free_alloc_pool (variable_info_pool);
  free_alloc_pool (constraint_pool);
}


/* Compute points-to information for every SSA_NAME pointer in the
   current function and compute the transitive closure of escaped
   variables to re-initialize the call-clobber states of local variables.  */

unsigned int
compute_may_aliases (void)
{
  /* For each pointer P_i, determine the sets of variables that P_i may
     point-to.  Compute the reachability set of escaped and call-used
     variables.  */
  compute_points_to_sets ();

  /* Debugging dumps.  */
  if (dump_file)
    {
      dump_alias_info (dump_file);

      if (dump_flags & TDF_DETAILS)
	dump_referenced_vars (dump_file);
    }

  /* Deallocate memory used by aliasing data structures and the internal
     points-to solution.  */
  delete_points_to_sets ();

  gcc_assert (!need_ssa_update_p (cfun));

  return 0;
}

static bool
gate_tree_pta (void)
{
  return flag_tree_pta;
}

/* A dummy pass to cause points-to information to be computed via
   TODO_rebuild_alias.  */

struct gimple_opt_pass pass_build_alias =
{
 {
  GIMPLE_PASS,
  "alias",		    /* name */
  gate_tree_pta,	    /* gate */
  NULL,                     /* execute */
  NULL,                     /* sub */
  NULL,                     /* next */
  0,                        /* static_pass_number */
  TV_NONE,                  /* tv_id */
  PROP_cfg | PROP_ssa,      /* properties_required */
  0,			    /* properties_provided */
  0,                        /* properties_destroyed */
  0,                        /* todo_flags_start */
  TODO_rebuild_alias | TODO_dump_func  /* todo_flags_finish */
 }
};

/* A dummy pass to cause points-to information to be computed via
   TODO_rebuild_alias.  */

struct gimple_opt_pass pass_build_ealias =
{
 {
  GIMPLE_PASS,
  "ealias",		    /* name */
  gate_tree_pta,	    /* gate */
  NULL,                     /* execute */
  NULL,                     /* sub */
  NULL,                     /* next */
  0,                        /* static_pass_number */
  TV_NONE,                  /* tv_id */
  PROP_cfg | PROP_ssa,      /* properties_required */
  0,			    /* properties_provided */
  0,                        /* properties_destroyed */
  0,                        /* todo_flags_start */
  TODO_rebuild_alias | TODO_dump_func  /* todo_flags_finish */
 }
};


/* Return true if we should execute IPA PTA.  */
static bool
gate_ipa_pta (void)
{
  return (optimize
	  && flag_ipa_pta
	  /* Don't bother doing anything if the program has errors.  */
	  && !(errorcount || sorrycount));
}

/* Execute the driver for IPA PTA.  */
static unsigned int
ipa_pta_execute (void)
{
  struct cgraph_node *node;

  in_ipa_mode = 1;

  init_alias_heapvars ();
  init_alias_vars ();

  /* Build the constraints.  */
  for (node = cgraph_nodes; node; node = node->next)
    {
      /* Nodes without a body are not interesting.  Especially do not
         visit clones at this point for now - we get duplicate decls
	 there for inline clones at least.  */
      if (!gimple_has_body_p (node->decl)
	  || node->clone_of)
	continue;

      /* It does not make sense to have graph edges into or out of
         externally visible functions.  There is no extra information
	 we can gather from them.  */
      if (node->local.externally_visible)
	continue;

      create_function_info_for (node->decl,
				cgraph_node_name (node));
    }

  for (node = cgraph_nodes; node; node = node->next)
    {
      struct function *func;
      basic_block bb;
      tree old_func_decl;

      /* Nodes without a body are not interesting.  */
      if (!gimple_has_body_p (node->decl)
	  || node->clone_of)
	continue;

      if (dump_file)
	fprintf (dump_file,
		 "Generating constraints for %s\n",
		 cgraph_node_name (node));

      func = DECL_STRUCT_FUNCTION (node->decl);
      old_func_decl = current_function_decl;
      push_cfun (func);
      current_function_decl = node->decl;

      /* For externally visible functions use local constraints for
	 their arguments.  For local functions we see all callers
	 and thus do not need initial constraints for parameters.  */
      if (node->local.externally_visible)
	intra_create_variable_infos ();

      /* Build constriants for the function body.  */
      FOR_EACH_BB_FN (bb, func)
	{
	  gimple_stmt_iterator gsi;

	  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	       gsi_next (&gsi))
	    {
	      gimple phi = gsi_stmt (gsi);

	      if (is_gimple_reg (gimple_phi_result (phi)))
		find_func_aliases (phi);
	    }

	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple stmt = gsi_stmt (gsi);

	      find_func_aliases (stmt);
	    }
	}

      current_function_decl = old_func_decl;
      pop_cfun ();
    }

  /* From the constraints compute the points-to sets.  */
  solve_constraints ();

  delete_points_to_sets ();

  in_ipa_mode = 0;

  return 0;
}

struct simple_ipa_opt_pass pass_ipa_pta =
{
 {
  SIMPLE_IPA_PASS,
  "pta",		                /* name */
  gate_ipa_pta,			/* gate */
  ipa_pta_execute,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_IPA_PTA,		        /* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_update_ssa                       /* todo_flags_finish */
 }
};


#include "gt-tree-ssa-structalias.h"
