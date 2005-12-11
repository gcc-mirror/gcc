/* Tree based points-to analysis
   Copyright (C) 2005 Free Software Foundation, Inc.
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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

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
#include "errors.h"
#include "diagnostic.h"
#include "tree.h"
#include "c-common.h"
#include "tree-flow.h"
#include "tree-inline.h"
#include "varray.h"
#include "c-tree.h"
#include "tree-gimple.h"
#include "hashtab.h"
#include "function.h"
#include "cgraph.h"
#include "tree-pass.h"
#include "timevar.h"
#include "alloc-pool.h"
#include "splay-tree.h"
#include "tree-ssa-structalias.h"
#include "cgraph.h"

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

   There are three types of constraint expressions, DEREF, ADDRESSOF, and
   SCALAR.  Each constraint expression consists of a constraint type,
   a variable, and an offset.  
   
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
  Complex constraints are all the constraints involving dereferences.
  
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
static bitmap_obstack predbitmap_obstack;
static bitmap_obstack ptabitmap_obstack;
static bitmap_obstack iteration_obstack;

static unsigned int create_variable_info_for (tree, const char *);
static void build_constraint_graph (void);

DEF_VEC_P(constraint_t);
DEF_VEC_ALLOC_P(constraint_t,heap);

#define EXECUTE_IF_IN_NONNULL_BITMAP(a, b, c, d)	\
  if (a)						\
    EXECUTE_IF_SET_IN_BITMAP (a, b, c, d)

static struct constraint_stats
{
  unsigned int total_vars;
  unsigned int collapsed_vars;
  unsigned int unified_vars_static;
  unsigned int unified_vars_dynamic;
  unsigned int iterations;
  unsigned int num_edges;
} stats;

struct variable_info
{
  /* ID of this variable  */
  unsigned int id;

  /* Name of this variable */
  const char *name;

  /* Tree that this variable is associated with.  */
  tree decl;

  /* Offset of this variable, in bits, from the base variable  */
  unsigned HOST_WIDE_INT offset;  

  /* Size of the variable, in bits.  */
  unsigned HOST_WIDE_INT size;

  /* Full size of the base variable, in bits.  */
  unsigned HOST_WIDE_INT fullsize;

  /* A link to the variable for the next field in this structure.  */
  struct variable_info *next;

  /* Node in the graph that represents the constraints and points-to
     solution for the variable.  */
  unsigned int node;

  /* True if the address of this variable is taken.  Needed for
     variable substitution.  */
  unsigned int address_taken:1;

  /* True if this variable is the target of a dereference.  Needed for
     variable substitution.  */
  unsigned int indirect_target:1;

  /* True if this is a variable created by the constraint analysis, such as
     heap variables and constraints we had to break up.  */
  unsigned int is_artificial_var:1;
  
  /* True if this is a special variable whose solution set should not be
     changed.  */
  unsigned int is_special_var:1;

  /* True for variables whose size is not known or variable.  */
  unsigned int is_unknown_size_var:1;  

  /* True for variables that have unions somewhere in them.  */
  unsigned int has_union:1;

  /* True if this is a heap variable.  */
  unsigned int is_heap_var:1;

  /* Points-to set for this variable.  */
  bitmap solution;

  /* Variable ids represented by this node.  */
  bitmap variables;

  /* Vector of complex constraints for this node.  Complex
     constraints are those involving dereferences.  */
  VEC(constraint_t,heap) *complex;
  
  /* Variable id this was collapsed to due to type unsafety.
     This should be unused completely after build_constraint_graph, or
     something is broken.  */
  struct variable_info *collapsed_to;
};
typedef struct variable_info *varinfo_t;

static varinfo_t first_vi_for_offset (varinfo_t, unsigned HOST_WIDE_INT);

/* Pool of variable info structures.  */
static alloc_pool variable_info_pool;

DEF_VEC_P(varinfo_t);

DEF_VEC_ALLOC_P(varinfo_t, heap);

/* Table of variable info structures for constraint variables.  Indexed directly
   by variable info id.  */
static VEC(varinfo_t,heap) *varmap;

/* Return the varmap element N */

static inline varinfo_t
get_varinfo (unsigned int n)
{
  return VEC_index(varinfo_t, varmap, n);
}

/* Return the varmap element N, following the collapsed_to link.  */

static inline varinfo_t
get_varinfo_fc (unsigned int n)
{
  varinfo_t v = VEC_index(varinfo_t, varmap, n);

  if (v->collapsed_to)
    return v->collapsed_to;
  return v;
}

/* Variable that represents the unknown pointer.  */
static varinfo_t var_anything;
static tree anything_tree;
static unsigned int anything_id;

/* Variable that represents the NULL pointer.  */
static varinfo_t var_nothing;
static tree nothing_tree;
static unsigned int nothing_id;

/* Variable that represents read only memory.  */
static varinfo_t var_readonly;
static tree readonly_tree;
static unsigned int readonly_id;

/* Variable that represents integers.  This is used for when people do things
   like &0->a.b.  */
static varinfo_t var_integer;
static tree integer_tree;
static unsigned int integer_id;


/* Lookup a heap var for FROM, and return it if we find one.  */

static tree 
heapvar_lookup (tree from)
{
  struct tree_map *h, in;
  in.from = from;

  h = htab_find_with_hash (heapvar_for_stmt, &in, htab_hash_pointer (from));
  if (h)
    return h->to;
  return NULL_TREE;
}

/* Insert a mapping FROM->TO in the heap var for statement
   hashtable.  */

static void
heapvar_insert (tree from, tree to)
{
  struct tree_map *h;
  void **loc;

  h = ggc_alloc (sizeof (struct tree_map));
  h->hash = htab_hash_pointer (from);
  h->from = from;
  h->to = to;
  loc = htab_find_slot_with_hash (heapvar_for_stmt, h, h->hash, INSERT);
  *(struct tree_map **) loc = h;
}  

/* Return a new variable info structure consisting for a variable
   named NAME, and using constraint graph node NODE.  */

static varinfo_t
new_var_info (tree t, unsigned int id, const char *name, unsigned int node)
{
  varinfo_t ret = pool_alloc (variable_info_pool);

  ret->id = id;
  ret->name = name;
  ret->decl = t;
  ret->node = node;
  ret->address_taken = false;
  ret->indirect_target = false;
  ret->is_artificial_var = false;
  ret->is_heap_var = false;
  ret->is_special_var = false;
  ret->is_unknown_size_var = false;
  ret->has_union = false;
  ret->solution = BITMAP_ALLOC (&ptabitmap_obstack);
  ret->variables = BITMAP_ALLOC (&ptabitmap_obstack);
  ret->complex = NULL;
  ret->next = NULL;
  ret->collapsed_to = NULL;
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
  unsigned HOST_WIDE_INT offset;
};

typedef struct constraint_expr ce_s;
DEF_VEC_O(ce_s);
DEF_VEC_ALLOC_O(ce_s, heap);
static void get_constraint_for (tree, VEC(ce_s, heap) **, bool *);
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

/* An edge in the weighted constraint graph.   The edges are weighted,
   with a bit set in weights meaning their is an edge with that
   weight. 
   We don't keep the src in the edge, because we always know what it
   is. */

struct constraint_edge
{
  unsigned int dest;
  bitmap weights;
};

typedef struct constraint_edge *constraint_edge_t;
static alloc_pool constraint_edge_pool;

/* Return a new constraint edge from SRC to DEST.  */

static constraint_edge_t
new_constraint_edge (unsigned int dest)
{
  constraint_edge_t ret = pool_alloc (constraint_edge_pool);
  ret->dest = dest;
  ret->weights = NULL;
  return ret;
}

DEF_VEC_P(constraint_edge_t);
DEF_VEC_ALLOC_P(constraint_edge_t,heap);


/* The constraint graph is represented internally in two different
   ways.  The overwhelming majority of edges in the constraint graph
   are zero weigh edges, and thus, using a vector of contrainst_edge_t
   is a waste of time and memory, since they have no weights.  We
   simply use a bitmap to store the preds and succs for each node.
   The weighted edges are stored as a set of adjacency vectors, one
   per variable. succs[x] is the vector of successors for variable x,
   and preds[x] is the vector of predecessors for variable x.  IOW,
   all edges are "forward" edges, which is not like our CFG.  So
   remember that preds[x]->src == x, and succs[x]->src == x.  */

struct constraint_graph
{
  bitmap *zero_weight_succs;
  bitmap *zero_weight_preds;
  VEC(constraint_edge_t,heap) **succs;
  VEC(constraint_edge_t,heap) **preds;
};

typedef struct constraint_graph *constraint_graph_t;

static constraint_graph_t graph;

/* Create a new constraint consisting of LHS and RHS expressions.  */

static constraint_t 
new_constraint (const struct constraint_expr lhs,
		const struct constraint_expr rhs)
{
  constraint_t ret = pool_alloc (constraint_pool);
  ret->lhs = lhs;
  ret->rhs = rhs;
  return ret;
}

/* Print out constraint C to FILE.  */

void
dump_constraint (FILE *file, constraint_t c)
{
  if (c->lhs.type == ADDRESSOF)
    fprintf (file, "&");
  else if (c->lhs.type == DEREF)
    fprintf (file, "*");  
  fprintf (file, "%s", get_varinfo_fc (c->lhs.var)->name);
  if (c->lhs.offset != 0)
    fprintf (file, " + " HOST_WIDE_INT_PRINT_DEC, c->lhs.offset);
  fprintf (file, " = ");
  if (c->rhs.type == ADDRESSOF)
    fprintf (file, "&");
  else if (c->rhs.type == DEREF)
    fprintf (file, "*");
  fprintf (file, "%s", get_varinfo_fc (c->rhs.var)->name);
  if (c->rhs.offset != 0)
    fprintf (file, " + " HOST_WIDE_INT_PRINT_DEC, c->rhs.offset);
  fprintf (file, "\n");
}

/* Print out constraint C to stderr.  */

void
debug_constraint (constraint_t c)
{
  dump_constraint (stderr, c);
}

/* Print out all constraints to FILE */

void
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

/* SOLVER FUNCTIONS 

   The solver is a simple worklist solver, that works on the following
   algorithm:
   
   sbitmap changed_nodes = all ones;
   changed_count = number of nodes;
   For each node that was already collapsed:
       changed_count--;

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

/* Take a solution set SET, add OFFSET to each member of the set, and
   overwrite SET with the result when done.  */

static void
solution_set_add (bitmap set, unsigned HOST_WIDE_INT offset)
{
  bitmap result = BITMAP_ALLOC (&iteration_obstack);
  unsigned int i;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (set, 0, i, bi)
    {
      /* If this is a properly sized variable, only add offset if it's
	 less than end.  Otherwise, it is globbed to a single
	 variable.  */
      
      if ((get_varinfo (i)->offset + offset) < get_varinfo (i)->fullsize)
	{
	  unsigned HOST_WIDE_INT fieldoffset = get_varinfo (i)->offset + offset;
	  varinfo_t v = first_vi_for_offset (get_varinfo (i), fieldoffset);
	  if (!v)
	    continue;
	  bitmap_set_bit (result, v->id);
	}
      else if (get_varinfo (i)->is_artificial_var 
	       || get_varinfo (i)->has_union
	       || get_varinfo (i)->is_unknown_size_var)
	{
	  bitmap_set_bit (result, i);
	}
    }
  
  bitmap_copy (set, result);  
  BITMAP_FREE (result);
}

/* Union solution sets TO and FROM, and add INC to each member of FROM in the
   process.  */

static bool
set_union_with_increment  (bitmap to, bitmap from, unsigned HOST_WIDE_INT inc)
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

/* Insert constraint C into the list of complex constraints for VAR.  */

static void
insert_into_complex (unsigned int var, constraint_t c)
{
  varinfo_t vi = get_varinfo (var);
  unsigned int place = VEC_lower_bound (constraint_t, vi->complex, c,
					constraint_less);
  VEC_safe_insert (constraint_t, heap, vi->complex, place, c);
}


/* Compare two constraint edges A and B, return true if they are equal.  */

static bool
constraint_edge_equal (struct constraint_edge a, struct constraint_edge b)
{
  return a.dest == b.dest;
}

/* Compare two constraint edges, return true if A is less than B */

static bool
constraint_edge_less (const constraint_edge_t a, const constraint_edge_t b)
{
  if (a->dest < b->dest)
    return true;
  return false;
}

/* Find the constraint edge that matches LOOKFOR, in VEC.
   Return the edge, if found, NULL otherwise.  */

static constraint_edge_t 
constraint_edge_vec_find (VEC(constraint_edge_t,heap) *vec, 
			  struct constraint_edge lookfor)
{
  unsigned int place;  
  constraint_edge_t edge = NULL;

  place = VEC_lower_bound (constraint_edge_t, vec, &lookfor, 
			   constraint_edge_less);
  if (place >= VEC_length (constraint_edge_t, vec))
    return NULL;
  edge = VEC_index (constraint_edge_t, vec, place);
  if (!constraint_edge_equal (*edge, lookfor))
    return NULL;
  return edge;
}

/* Condense two variable nodes into a single variable node, by moving
   all associated info from SRC to TO.  */

static void 
condense_varmap_nodes (unsigned int to, unsigned int src)
{
  varinfo_t tovi = get_varinfo (to);
  varinfo_t srcvi = get_varinfo (src);
  unsigned int i;
  constraint_t c;
  bitmap_iterator bi;
  
  /* the src node, and all its variables, are now the to node.  */
  srcvi->node = to;
  EXECUTE_IF_SET_IN_BITMAP (srcvi->variables, 0, i, bi)
    get_varinfo (i)->node = to;
  
  /* Merge the src node variables and the to node variables.  */
  bitmap_set_bit (tovi->variables, src);
  bitmap_ior_into (tovi->variables, srcvi->variables);
  bitmap_clear (srcvi->variables);
  
  /* Move all complex constraints from src node into to node  */
  for (i = 0; VEC_iterate (constraint_t, srcvi->complex, i, c); i++)
    {
      /* In complex constraints for node src, we may have either
	 a = *src, and *src = a.  */
      
      if (c->rhs.type == DEREF)
	c->rhs.var = to;
      else
	c->lhs.var = to;
    }
  constraint_set_union (&tovi->complex, &srcvi->complex);
  VEC_free (constraint_t, heap, srcvi->complex);
  srcvi->complex = NULL;
}

/* Erase an edge from SRC to SRC from GRAPH.  This routine only
   handles self-edges (e.g. an edge from a to a).  */

static void
erase_graph_self_edge (constraint_graph_t graph, unsigned int src)
{
  VEC(constraint_edge_t,heap) *predvec = graph->preds[src];
  VEC(constraint_edge_t,heap) *succvec = graph->succs[src];
  struct constraint_edge edge;
  unsigned int place;

  edge.dest = src;

  /* Remove from the successors.  */
  place = VEC_lower_bound (constraint_edge_t, succvec, &edge, 
			   constraint_edge_less);
  
  /* Make sure we found the edge.  */
#ifdef ENABLE_CHECKING
  {
    constraint_edge_t tmp = VEC_index (constraint_edge_t, succvec, place);
    gcc_assert (constraint_edge_equal (*tmp, edge));
  }
#endif
  VEC_ordered_remove (constraint_edge_t, succvec, place);

  /* Remove from the predecessors.  */
  place = VEC_lower_bound (constraint_edge_t, predvec, &edge,
			   constraint_edge_less);

  /* Make sure we found the edge.  */
#ifdef ENABLE_CHECKING
  {
    constraint_edge_t tmp = VEC_index (constraint_edge_t, predvec, place);
    gcc_assert (constraint_edge_equal (*tmp, edge));
  }
#endif
  VEC_ordered_remove (constraint_edge_t, predvec, place);
}

/* Remove edges involving NODE from GRAPH.  */

static void
clear_edges_for_node (constraint_graph_t graph, unsigned int node)
{
  VEC(constraint_edge_t,heap) *succvec = graph->succs[node];
  VEC(constraint_edge_t,heap) *predvec = graph->preds[node];
  bitmap_iterator bi;
  unsigned int j;
  constraint_edge_t c = NULL;
  int i;

  /* Walk the successors, erase the associated preds.  */
  
  EXECUTE_IF_IN_NONNULL_BITMAP (graph->zero_weight_succs[node], 0, j, bi)
    if (j != node)
      bitmap_clear_bit (graph->zero_weight_preds[j], node);
  
  for (i = 0; VEC_iterate (constraint_edge_t, succvec, i, c); i++)
    if (c->dest != node)
      {
	unsigned int place;
	struct constraint_edge lookfor;
	constraint_edge_t result;

	lookfor.dest = node;
	place = VEC_lower_bound (constraint_edge_t, graph->preds[c->dest], 
				 &lookfor, constraint_edge_less);
	result = VEC_ordered_remove (constraint_edge_t, 
				     graph->preds[c->dest], place);
	pool_free (constraint_edge_pool, result);
      }

  /* Walk the preds, erase the associated succs.  */

  EXECUTE_IF_IN_NONNULL_BITMAP (graph->zero_weight_preds[node], 0, j, bi)
    if (j != node)
      bitmap_clear_bit (graph->zero_weight_succs[j], node);
  
  for (i =0; VEC_iterate (constraint_edge_t, predvec, i, c); i++)
    if (c->dest != node)
      {
	unsigned int place;
	struct constraint_edge lookfor;
	constraint_edge_t result;

	lookfor.dest = node;
	place = VEC_lower_bound (constraint_edge_t, graph->succs[c->dest],
				 &lookfor, constraint_edge_less);
	result = VEC_ordered_remove (constraint_edge_t, 
				     graph->succs[c->dest], place);
	pool_free (constraint_edge_pool, result);

      }    

  if (graph->zero_weight_preds[node])
    {
      BITMAP_FREE (graph->zero_weight_preds[node]);
      graph->zero_weight_preds[node] = NULL;
    } 

  if (graph->zero_weight_succs[node])
    {
      BITMAP_FREE (graph->zero_weight_succs[node]);
      graph->zero_weight_succs[node] = NULL;
    } 

  VEC_free (constraint_edge_t, heap, graph->preds[node]);
  VEC_free (constraint_edge_t, heap, graph->succs[node]);
  graph->preds[node] = NULL;
  graph->succs[node] = NULL;
}

static bool edge_added = false;
  
/* Add edge (src, dest) to the graph.  */

static bool
add_graph_edge (constraint_graph_t graph, unsigned int src, unsigned int dest)
{
  unsigned int place;
  VEC(constraint_edge_t,heap) *vec;
  struct constraint_edge newe;
  newe.dest = dest;

  vec = graph->preds[src];
  place = VEC_lower_bound (constraint_edge_t, vec, &newe, 
			   constraint_edge_less);
  if (place == VEC_length (constraint_edge_t, vec)
      || VEC_index (constraint_edge_t, vec, place)->dest != dest)
    {
      constraint_edge_t edge = new_constraint_edge (dest);

      VEC_safe_insert (constraint_edge_t, heap, graph->preds[src], 
		       place, edge);
      edge = new_constraint_edge (src);

      place = VEC_lower_bound (constraint_edge_t, graph->succs[dest],
			       edge, constraint_edge_less);
      VEC_safe_insert (constraint_edge_t, heap, graph->succs[dest], 
		       place, edge);
      edge_added = true;
      stats.num_edges++;
      return true;
    }
  else
    return false;
}


/* Return the bitmap representing the weights of edge (SRC, DEST).  */

static bitmap *
get_graph_weights (constraint_graph_t graph, unsigned int src,
		   unsigned int dest)
{
  constraint_edge_t edge;
  VEC(constraint_edge_t,heap) *vec;
  struct constraint_edge lookfor;

  lookfor.dest = dest;

  vec = graph->preds[src];
  edge = constraint_edge_vec_find (vec, lookfor);
  gcc_assert (edge != NULL);
  return &edge->weights;
}

/* Allocate graph weight bitmap for the edges associated with SRC and
   DEST in GRAPH.  Both the pred and the succ edges share a single
   bitmap, so we need to set both edges to that bitmap.  */

static bitmap
allocate_graph_weights (constraint_graph_t graph, unsigned int src, 
			unsigned int dest)
{
  bitmap result;
  constraint_edge_t edge;
  VEC(constraint_edge_t,heap) *vec;
  struct constraint_edge lookfor;
  
  result = BITMAP_ALLOC (&ptabitmap_obstack);

  /* Set the pred weight.  */
  lookfor.dest = dest;
  vec = graph->preds[src];
  edge = constraint_edge_vec_find (vec, lookfor);
  gcc_assert (edge != NULL);
  edge->weights = result;

  /* Set the succ weight.  */  
  lookfor.dest = src;
  vec = graph->succs[dest];
  edge = constraint_edge_vec_find (vec, lookfor);
  gcc_assert (edge != NULL);
  edge->weights = result;
  
  return result;  
}


/* Merge GRAPH nodes FROM and TO into node TO.  */

static void
merge_graph_nodes (constraint_graph_t graph, unsigned int to, 
		   unsigned int from)
{
  VEC(constraint_edge_t,heap) *succvec = graph->succs[from];
  VEC(constraint_edge_t,heap) *predvec = graph->preds[from];
  int i;
  constraint_edge_t c;
  unsigned int j;
  bitmap_iterator bi;

  /* Merge all the zero weighted predecessor edges.  */
  if (graph->zero_weight_preds[from])
    {
      if (!graph->zero_weight_preds[to])
	graph->zero_weight_preds[to] = BITMAP_ALLOC (&predbitmap_obstack);
      
      EXECUTE_IF_SET_IN_BITMAP (graph->zero_weight_preds[from], 0, j, bi)
	{
	  if (j != to)
	    {
	      bitmap_clear_bit (graph->zero_weight_succs[j], from);
	      bitmap_set_bit (graph->zero_weight_succs[j], to);
	    }
	}
      bitmap_ior_into (graph->zero_weight_preds[to], 
		       graph->zero_weight_preds[from]);
    }

  /* Merge all the zero weighted successor edges.  */
  if (graph->zero_weight_succs[from])
    {
      if (!graph->zero_weight_succs[to])
	graph->zero_weight_succs[to] = BITMAP_ALLOC (&ptabitmap_obstack);
      EXECUTE_IF_SET_IN_BITMAP (graph->zero_weight_succs[from], 0, j, bi)
	{
	  bitmap_clear_bit (graph->zero_weight_preds[j], from);
	  bitmap_set_bit (graph->zero_weight_preds[j], to);
	}
      bitmap_ior_into (graph->zero_weight_succs[to], 
		       graph->zero_weight_succs[from]);
    }

  /* Merge all the non-zero weighted predecessor edges.  */
  for (i = 0; VEC_iterate (constraint_edge_t, predvec, i, c); i++)
    {
      unsigned int d = c->dest;
      bitmap temp;
      bitmap *weights;

      if (c->dest == from)
	d = to;

      add_graph_edge (graph, to, d);

      temp = *(get_graph_weights (graph, from, c->dest));      
      if (temp)
	{
	  weights = get_graph_weights (graph, to, d);
	  if (!*weights)
	    *weights = allocate_graph_weights (graph, to, d);
	  
	  bitmap_ior_into (*weights, temp);
	}
      
    }
  
  /* Merge all the non-zero weighted successor edges.  */
  for (i = 0; VEC_iterate (constraint_edge_t, succvec, i, c); i++)
    {
      unsigned int d = c->dest;
      bitmap temp;
      bitmap *weights;

      if (c->dest == from)
	d = to;

      add_graph_edge (graph, d, to);

      temp = *(get_graph_weights (graph, c->dest, from));
      if (temp)
	{
	  weights = get_graph_weights (graph, d, to);
	  if (!*weights)
	    *weights = allocate_graph_weights (graph, d, to);
	  bitmap_ior_into (*weights, temp);
	}
    }
  clear_edges_for_node (graph, from);
}

/* Add a graph edge to GRAPH, going from TO to FROM, with WEIGHT, if
   it doesn't exist in the graph already.
   Return false if the edge already existed, true otherwise.  */

static bool
int_add_graph_edge (constraint_graph_t graph, unsigned int to, 
		    unsigned int from, unsigned HOST_WIDE_INT weight)
{
  if (to == from && weight == 0)
    {
      return false;
    }
  else
    {
      bool r = false;

      if (weight == 0)
	{
          if (!graph->zero_weight_preds[to])
	    graph->zero_weight_preds[to] = BITMAP_ALLOC (&predbitmap_obstack);
          if (!graph->zero_weight_succs[from])
	    graph->zero_weight_succs[from] = BITMAP_ALLOC (&ptabitmap_obstack);
	  if (!bitmap_bit_p (graph->zero_weight_succs[from], to))
	    {
	      edge_added = true;
	      r = true;
	      stats.num_edges++;
	      bitmap_set_bit (graph->zero_weight_preds[to], from);
	      bitmap_set_bit (graph->zero_weight_succs[from], to);
	    }
	}
      else
	{
	  bitmap *weights;

	  r = add_graph_edge (graph, to, from);
	  weights = get_graph_weights (graph, to, from);

	  if (!*weights)
	    {
	      r = true;
	      *weights = allocate_graph_weights (graph, to, from);
	      bitmap_set_bit (*weights, weight);
	    }
	  else
	    {
	      r |= !bitmap_bit_p (*weights, weight);
	      bitmap_set_bit (*weights, weight);
	    }
	}
      
      return r;
    }
}


/* Return true if {DEST.SRC} is an existing graph edge in GRAPH.  */

static bool
valid_graph_edge (constraint_graph_t graph, unsigned int src, 
		  unsigned int dest)
{
  struct constraint_edge lookfor;
  lookfor.dest = src;
  
  return (graph->zero_weight_succs[dest] 
      && bitmap_bit_p (graph->zero_weight_succs[dest], src)) 
    || constraint_edge_vec_find (graph->succs[dest], lookfor) != NULL;
}

/* Return true if {DEST, SRC} is an existing weighted graph edge (IE has
   a weight other than 0) in GRAPH.  */
static bool
valid_weighted_graph_edge (constraint_graph_t graph, unsigned int src, 
			   unsigned int dest)
{
  struct constraint_edge lookfor;
  lookfor.dest = src;
  
  return graph->preds[src] 
    && constraint_edge_vec_find (graph->succs[dest], lookfor) != NULL;
}


/* Build the constraint graph.  */

static void
build_constraint_graph (void)
{
  int i = 0;
  constraint_t c;

  graph = xmalloc (sizeof (struct constraint_graph));
  graph->succs = xcalloc (VEC_length (varinfo_t, varmap) + 1,
			  sizeof (*graph->succs));
  graph->preds = xcalloc (VEC_length (varinfo_t, varmap) + 1,
			  sizeof (*graph->preds));
  graph->zero_weight_succs = xcalloc (VEC_length (varinfo_t, varmap) + 1,
			  sizeof (*graph->zero_weight_succs));
  graph->zero_weight_preds = xcalloc (VEC_length (varinfo_t, varmap) + 1,
			  sizeof (*graph->zero_weight_preds));

  for (i = 0; VEC_iterate (constraint_t, constraints, i, c); i++)
    {
      struct constraint_expr lhs = c->lhs;
      struct constraint_expr rhs = c->rhs;
      unsigned int lhsvar = get_varinfo_fc (lhs.var)->id;
      unsigned int rhsvar = get_varinfo_fc (rhs.var)->id;

      if (lhs.type == DEREF)
	{
	  /* *x = y or *x = &y (complex) */
	  if (rhs.type == ADDRESSOF || rhsvar > anything_id)
	    insert_into_complex (lhsvar, c);
	}
      else if (rhs.type == DEREF)
	{
	  /* !special var= *y */
	  if (!(get_varinfo (lhsvar)->is_special_var))
	    insert_into_complex (rhsvar, c);
	}
      else if (rhs.type == ADDRESSOF)
	{
	  /* x = &y */
	  bitmap_set_bit (get_varinfo (lhsvar)->solution, rhsvar);
	}
      else if (lhsvar > anything_id)
	{
	  /* Ignore 0 weighted self edges, as they can't possibly contribute
	     anything */
	  if (lhsvar != rhsvar || rhs.offset != 0 || lhs.offset != 0)
	    {
	      /* x = y (simple) */
	      int_add_graph_edge (graph, lhs.var, rhs.var, rhs.offset);
	    }
	  
	}
    }
}


/* Changed variables on the last iteration.  */
static unsigned int changed_count;
static sbitmap changed;

DEF_VEC_I(unsigned);
DEF_VEC_ALLOC_I(unsigned,heap);


/* Strongly Connected Component visitation info.  */

struct scc_info
{
  sbitmap visited;
  sbitmap in_component;
  int current_index;
  unsigned int *visited_index;
  VEC(unsigned,heap) *scc_stack;
  VEC(unsigned,heap) *unification_queue;
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

  gcc_assert (get_varinfo (n)->node == n);
  SET_BIT (si->visited, n);
  RESET_BIT (si->in_component, n);
  si->visited_index[n] = si->current_index ++;
  
  /* Visit all the successors.  */
  EXECUTE_IF_IN_NONNULL_BITMAP (graph->zero_weight_succs[n], 0, i, bi)
    {
      unsigned int w = i;
      if (!TEST_BIT (si->visited, w))
	scc_visit (graph, si, w);
      if (!TEST_BIT (si->in_component, w))
	{
	  unsigned int t = get_varinfo (w)->node;
	  unsigned int nnode = get_varinfo (n)->node;
	  if (si->visited_index[t] < si->visited_index[nnode])
	    get_varinfo (n)->node = t;
	}
    }
  
  /* See if any components have been identified.  */
  if (get_varinfo (n)->node == n)
    {
      unsigned int t = si->visited_index[n];
      SET_BIT (si->in_component, n);
      while (VEC_length (unsigned, si->scc_stack) != 0 
	     && t < si->visited_index[VEC_last (unsigned, si->scc_stack)])
	{
	  unsigned int w = VEC_pop (unsigned, si->scc_stack);
	  get_varinfo (w)->node = n;
	  SET_BIT (si->in_component, w);
	  /* Mark this node for collapsing.  */
	  VEC_safe_push (unsigned, heap, si->unification_queue, w);
	} 
    }
  else
    VEC_safe_push (unsigned, heap, si->scc_stack, n);
}


/* Collapse two variables into one variable.  */

static void
collapse_nodes (constraint_graph_t graph, unsigned int to, unsigned int from)
{
  bitmap tosol, fromsol;

  condense_varmap_nodes (to, from);
  tosol = get_varinfo (to)->solution;
  fromsol = get_varinfo (from)->solution;
  bitmap_ior_into (tosol, fromsol);
  merge_graph_nodes (graph, to, from);

  if (valid_graph_edge (graph, to, to))
    {
      if (graph->zero_weight_preds[to])
	{
	  bitmap_clear_bit (graph->zero_weight_preds[to], to);
	  bitmap_clear_bit (graph->zero_weight_succs[to], to);
	}
      if (valid_weighted_graph_edge (graph, to, to))
	{
	  bitmap weights = *(get_graph_weights (graph, to, to));
	  if (!weights || bitmap_empty_p (weights))
	    erase_graph_self_edge (graph, to);
	}
    }
  BITMAP_FREE (fromsol);
  get_varinfo (to)->address_taken |= get_varinfo (from)->address_taken;
  get_varinfo (to)->indirect_target |= get_varinfo (from)->indirect_target;
}


/* Unify nodes in GRAPH that we have found to be part of a cycle.
   SI is the Strongly Connected Components information structure that tells us
   what components to unify.
   UPDATE_CHANGED should be set to true if the changed sbitmap and changed
   count should be updated to reflect the unification.  */

static void
process_unification_queue (constraint_graph_t graph, struct scc_info *si,
			   bool update_changed)
{
  size_t i = 0;
  bitmap tmp = BITMAP_ALLOC (update_changed ? &iteration_obstack : NULL);
  bitmap_clear (tmp);

  /* We proceed as follows:

     For each component in the queue (components are delineated by
     when current_queue_element->node != next_queue_element->node):

        rep = representative node for component

        For each node (tounify) to be unified in the component,
           merge the solution for tounify into tmp bitmap

           clear solution for tounify

           merge edges from tounify into rep

	   merge complex constraints from tounify into rep

	   update changed count to note that tounify will never change
	   again

	Merge tmp into solution for rep, marking rep changed if this
	changed rep's solution.
	
	Delete any 0 weighted self-edges we now have for rep.  */
  while (i != VEC_length (unsigned, si->unification_queue))
    {
      unsigned int tounify = VEC_index (unsigned, si->unification_queue, i);
      unsigned int n = get_varinfo (tounify)->node;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Unifying %s to %s\n", 
		 get_varinfo (tounify)->name,
		 get_varinfo (n)->name);
      if (update_changed)
	stats.unified_vars_dynamic++;
      else
	stats.unified_vars_static++;
      bitmap_ior_into (tmp, get_varinfo (tounify)->solution);
      merge_graph_nodes (graph, n, tounify);
      condense_varmap_nodes (n, tounify);
      
      if (update_changed && TEST_BIT (changed, tounify))
	{
	  RESET_BIT (changed, tounify);
	  if (!TEST_BIT (changed, n))
	    SET_BIT (changed, n);
	  else
	    {
	      gcc_assert (changed_count > 0);
	      changed_count--;
	    }
	}

      bitmap_clear (get_varinfo (tounify)->solution);
      ++i;

      /* If we've either finished processing the entire queue, or
	 finished processing all nodes for component n, update the solution for
	 n.  */
      if (i == VEC_length (unsigned, si->unification_queue)
	  || get_varinfo (VEC_index (unsigned, si->unification_queue, i))->node != n)
	{
	  /* If the solution changes because of the merging, we need to mark
	     the variable as changed.  */
	  if (bitmap_ior_into (get_varinfo (n)->solution, tmp))
	    {
	      if (update_changed && !TEST_BIT (changed, n))
		{
		  SET_BIT (changed, n);
		  changed_count++;
		}
	    }
	  bitmap_clear (tmp);

	  if (valid_graph_edge (graph, n, n))
	    {
	      if (graph->zero_weight_succs[n])
		{
		  if (graph->zero_weight_preds[n])
		    bitmap_clear_bit (graph->zero_weight_preds[n], n);
		  bitmap_clear_bit (graph->zero_weight_succs[n], n);
		}
	      if (valid_weighted_graph_edge (graph, n, n))
		{
		  bitmap weights = *(get_graph_weights (graph, n, n));
		  if (!weights || bitmap_empty_p (weights))
		    erase_graph_self_edge (graph, n);
		}
	    }
	}
    }
  BITMAP_FREE (tmp);
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
  size_t size = VEC_length (varinfo_t, varmap);
  struct topo_info *ti = xmalloc (sizeof (struct topo_info));
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
  VEC(constraint_edge_t,heap) *succs = graph->succs[n];
  bitmap temp;
  bitmap_iterator bi;
  constraint_edge_t c;
  int i;
  unsigned int j;

  SET_BIT (ti->visited, n);
  if (VEC_length (constraint_edge_t, succs) != 0)
    {
      temp = BITMAP_ALLOC (&iteration_obstack);
      if (graph->zero_weight_succs[n])
	bitmap_ior_into (temp, graph->zero_weight_succs[n]);
      for (i = 0; VEC_iterate (constraint_edge_t, succs, i, c); i++)
	bitmap_set_bit (temp, c->dest);
    }
  else 
    temp = graph->zero_weight_succs[n];

  if (temp) 
    EXECUTE_IF_SET_IN_BITMAP (temp, 0, j, bi)
      {
	if (!TEST_BIT (ti->visited, j))
	  topo_visit (graph, ti, j);
      }
  VEC_safe_push (unsigned, heap, ti->topo_order, n);
}

/* Return true if variable N + OFFSET is a legal field of N.  */

static bool 
type_safe (unsigned int n, unsigned HOST_WIDE_INT *offset)
{
  varinfo_t ninfo = get_varinfo (n);

  /* For things we've globbed to single variables, any offset into the
     variable acts like the entire variable, so that it becomes offset
     0.  */
  if (ninfo->is_special_var
      || ninfo->is_artificial_var
      || ninfo->is_unknown_size_var)
    {
      *offset = 0;
      return true;
    }
  return (get_varinfo (n)->offset + *offset) < get_varinfo (n)->fullsize;
}

#define DONT_PROPAGATE_WITH_ANYTHING 0

/* Process a constraint C that represents *x = &y.  */

static void
do_da_constraint (constraint_graph_t graph ATTRIBUTE_UNUSED,
		  constraint_t c, bitmap delta)
{
  unsigned int rhs = c->rhs.var;
  unsigned int j;
  bitmap_iterator bi;

  /* For each member j of Delta (Sol(x)), add x to Sol(j)  */
  EXECUTE_IF_SET_IN_BITMAP (delta, 0, j, bi)
    {
      unsigned HOST_WIDE_INT offset = c->lhs.offset;
      if (type_safe (j, &offset) && !(get_varinfo (j)->is_special_var))
	{
	/* *x != NULL && *x != ANYTHING*/
	  varinfo_t v;
	  unsigned int t;
	  bitmap sol;
	  unsigned HOST_WIDE_INT fieldoffset = get_varinfo (j)->offset + offset;

	  v = first_vi_for_offset (get_varinfo (j), fieldoffset);
	  if (!v)
	    continue;
	  t = v->node;
	  sol = get_varinfo (t)->solution;
	  if (!bitmap_bit_p (sol, rhs))
	    {		  
	      bitmap_set_bit (sol, rhs);
	      if (!TEST_BIT (changed, t))
		{
		  SET_BIT (changed, t);
		  changed_count++;
		}
	    }
	}
      else if (0 && dump_file && !(get_varinfo (j)->is_special_var))
	fprintf (dump_file, "Untypesafe usage in do_da_constraint.\n");
      
    }
}

/* Process a constraint C that represents x = *y, using DELTA as the
   starting solution.  */

static void
do_sd_constraint (constraint_graph_t graph, constraint_t c,
		  bitmap delta)
{
  unsigned int lhs = get_varinfo (c->lhs.var)->node;
  bool flag = false;
  bitmap sol = get_varinfo (lhs)->solution;
  unsigned int j;
  bitmap_iterator bi;

#if DONT_PROPAGATE_WITH_ANYTHING 
 if (bitmap_bit_p (delta, anything_id))
   {
     flag = !bitmap_bit_p (sol, anything_id);
     if (flag)
       bitmap_set_bit (sol, anything_id);
     goto done;
   }
#endif
  /* For each variable j in delta (Sol(y)), add    
     an edge in the graph from j to x, and union Sol(j) into Sol(x).  */
  EXECUTE_IF_SET_IN_BITMAP (delta, 0, j, bi)
    {
      unsigned HOST_WIDE_INT roffset = c->rhs.offset;
      if (type_safe (j, &roffset))
	{
	  varinfo_t v;
	  unsigned HOST_WIDE_INT fieldoffset = get_varinfo (j)->offset + roffset;
	  unsigned int t;

	  v = first_vi_for_offset (get_varinfo (j), fieldoffset);
	  if (!v)
	    continue;
	  t = v->node;

	  /* Adding edges from the special vars is pointless.
	     They don't have sets that can change.  */
	  if (get_varinfo (t) ->is_special_var)
	    flag |= bitmap_ior_into (sol, get_varinfo (t)->solution);
	  else if (int_add_graph_edge (graph, lhs, t, 0))
	    flag |= bitmap_ior_into (sol, get_varinfo (t)->solution);
	}
      else if (0 && dump_file && !(get_varinfo (j)->is_special_var))
	fprintf (dump_file, "Untypesafe usage in do_sd_constraint\n");
      
    }
#if DONT_PROPAGATE_WITH_ANYTHING
done:
#endif
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

/* Process a constraint C that represents *x = y.  */

static void
do_ds_constraint (constraint_graph_t graph, constraint_t c, bitmap delta)
{
  unsigned int rhs = get_varinfo (c->rhs.var)->node;
  unsigned HOST_WIDE_INT roff = c->rhs.offset;
  bitmap sol = get_varinfo (rhs)->solution;
  unsigned int j;
  bitmap_iterator bi;

#if DONT_PROPAGATE_WITH_ANYTHING 
 if (bitmap_bit_p (sol, anything_id))
   {
     EXECUTE_IF_SET_IN_BITMAP (delta, 0, j, bi)
       {
	 varinfo_t jvi = get_varinfo (j);
	 unsigned int t;
	 unsigned int loff = c->lhs.offset;
	 unsigned HOST_WIDE_INT fieldoffset = jvi->offset + loff;
	 varinfo_t v;

	 v = first_vi_for_offset (get_varinfo (j), fieldoffset);
	 if (!v)
	   continue;
	 t = v->node;
	 
	 if (!bitmap_bit_p (get_varinfo (t)->solution, anything_id))
	   {
	     bitmap_set_bit (get_varinfo (t)->solution, anything_id);
	     if (!TEST_BIT (changed, t))
	       {
		 SET_BIT (changed, t);
		 changed_count++;
	       }
	   }
       }
     return;
   }
#endif

  /* For each member j of delta (Sol(x)), add an edge from y to j and
     union Sol(y) into Sol(j) */
  EXECUTE_IF_SET_IN_BITMAP (delta, 0, j, bi)
    {
      unsigned HOST_WIDE_INT loff = c->lhs.offset;
      if (type_safe (j, &loff) && !(get_varinfo(j)->is_special_var))
	{
	  varinfo_t v;
	  unsigned int t;
	  unsigned HOST_WIDE_INT fieldoffset = get_varinfo (j)->offset + loff;

	  v = first_vi_for_offset (get_varinfo (j), fieldoffset);
	  if (!v)
	    continue;
	  t = v->node;
	  if (int_add_graph_edge (graph, t, rhs, roff))
	    {
	      bitmap tmp = get_varinfo (t)->solution;
	      if (set_union_with_increment (tmp, sol, roff))
		{
		  get_varinfo (t)->solution = tmp;
		  if (t == rhs)
		    sol = get_varinfo (rhs)->solution;
		  if (!TEST_BIT (changed, t))
		    {
		      SET_BIT (changed, t);
		      changed_count++;
		    }
		}
	    }
	}    
      else if (0 && dump_file && !(get_varinfo (j)->is_special_var))
	fprintf (dump_file, "Untypesafe usage in do_ds_constraint\n");
    }
}

/* Handle a non-simple (simple meaning requires no iteration), non-copy
   constraint (IE *x = &y, x = *y, and *x = y).  */
   
static void
do_complex_constraint (constraint_graph_t graph, constraint_t c, bitmap delta)
{
  if (c->lhs.type == DEREF)
    {
      if (c->rhs.type == ADDRESSOF)
	{
	  /* *x = &y */
	  do_da_constraint (graph, c, delta);
	}
      else
	{
	  /* *x = y */
	  do_ds_constraint (graph, c, delta);
	}
    }
  else
    {
      /* x = *y */
      if (!(get_varinfo (c->lhs.var)->is_special_var))
	do_sd_constraint (graph, c, delta);
    }
}

/* Initialize and return a new SCC info structure.  */

static struct scc_info *
init_scc_info (void)
{
  struct scc_info *si = xmalloc (sizeof (struct scc_info));
  size_t size = VEC_length (varinfo_t, varmap);

  si->current_index = 0;
  si->visited = sbitmap_alloc (size);
  sbitmap_zero (si->visited);
  si->in_component = sbitmap_alloc (size);
  sbitmap_ones (si->in_component);
  si->visited_index = xcalloc (sizeof (unsigned int), size + 1);
  si->scc_stack = VEC_alloc (unsigned, heap, 1);
  si->unification_queue = VEC_alloc (unsigned, heap, 1);
  return si;
}

/* Free an SCC info structure pointed to by SI */

static void
free_scc_info (struct scc_info *si)
{  
  sbitmap_free (si->visited);
  sbitmap_free (si->in_component);
  free (si->visited_index);
  VEC_free (unsigned, heap, si->scc_stack);
  VEC_free (unsigned, heap, si->unification_queue);
  free(si); 
}


/* Find cycles in GRAPH that occur, using strongly connected components, and
   collapse the cycles into a single representative node.  if UPDATE_CHANGED
   is true, then update the changed sbitmap to note those nodes whose
   solutions have changed as a result of collapsing.  */

static void
find_and_collapse_graph_cycles (constraint_graph_t graph, bool update_changed)
{
  unsigned int i;
  unsigned int size = VEC_length (varinfo_t, varmap);
  struct scc_info *si = init_scc_info ();

  for (i = 0; i != size; ++i)
    if (!TEST_BIT (si->visited, i) && get_varinfo (i)->node == i)
      scc_visit (graph, si, i);
  
  process_unification_queue (graph, si, update_changed);
  free_scc_info (si);
}

/* Compute a topological ordering for GRAPH, and store the result in the
   topo_info structure TI.  */

static void 
compute_topo_order (constraint_graph_t graph,
		    struct topo_info *ti)
{
  unsigned int i;
  unsigned int size = VEC_length (varinfo_t, varmap);
  
  for (i = 0; i != size; ++i)
    if (!TEST_BIT (ti->visited, i) && get_varinfo (i)->node == i)
      topo_visit (graph, ti, i);
}

/* Return true if bitmap B is empty, or a bitmap other than bit 0 is set. */

static bool
bitmap_other_than_zero_bit_set (bitmap b)
{
  unsigned int i;
  bitmap_iterator bi;

  if (bitmap_empty_p (b))
    return false;
  EXECUTE_IF_SET_IN_BITMAP (b, 1, i, bi)
    return true;
  return false;
}

/* Perform offline variable substitution.
   
   This is a linear time way of identifying variables that must have
   equivalent points-to sets, including those caused by static cycles,
   and single entry subgraphs, in the constraint graph.

   The technique is described in "Off-line variable substitution for
   scaling points-to analysis" by Atanas Rountev and Satish Chandra,
   in "ACM SIGPLAN Notices" volume 35, number 5, pages 47-56.  */

static void
perform_var_substitution (constraint_graph_t graph)
{
  struct topo_info *ti = init_topo_info ();
 
  bitmap_obstack_initialize (&iteration_obstack);
  /* Compute the topological ordering of the graph, then visit each
     node in topological order.  */
  compute_topo_order (graph, ti);
 
  while (VEC_length (unsigned, ti->topo_order) != 0)
    {
      unsigned int i = VEC_pop (unsigned, ti->topo_order);
      unsigned int pred;
      varinfo_t vi = get_varinfo (i);
      bool okay_to_elim = false;
      unsigned int root = VEC_length (varinfo_t, varmap);
      VEC(constraint_edge_t,heap) *predvec = graph->preds[i];
      constraint_edge_t ce = NULL;
      bitmap tmp;
      unsigned int k;
      bitmap_iterator bi;

      /* We can't eliminate things whose address is taken, or which is
	 the target of a dereference.  */
      if (vi->address_taken || vi->indirect_target)
	continue;

      /* See if all predecessors of I are ripe for elimination */
      EXECUTE_IF_IN_NONNULL_BITMAP (graph->zero_weight_preds[i], 0, k, bi)
	  {
	    unsigned int w;
	    w = get_varinfo (k)->node;

	    /* We can't eliminate the node if one of the predecessors is
	       part of a different strongly connected component.  */
	    if (!okay_to_elim)
	      {
		root = w;
		okay_to_elim = true;
	      }
	    else if (w != root)
	      {
		okay_to_elim = false;
		break;
	      }

	    /* Theorem 4 in Rountev and Chandra: If i is a direct node,
	       then Solution(i) is a subset of Solution (w), where w is a
	       predecessor in the graph.  
	       Corollary: If all predecessors of i have the same
	       points-to set, then i has that same points-to set as
	       those predecessors.  */
	    tmp = BITMAP_ALLOC (NULL);
	    bitmap_and_compl (tmp, get_varinfo (i)->solution,
			      get_varinfo (w)->solution);
	    if (!bitmap_empty_p (tmp))
	      {
		okay_to_elim = false;
		BITMAP_FREE (tmp);
		break;
	      }
	    BITMAP_FREE (tmp);
	  }

      if (okay_to_elim)
	for (pred = 0; 
	     VEC_iterate (constraint_edge_t, predvec, pred, ce); 
	     pred++)
	  {
	    bitmap weight;
	    unsigned int w;
	    weight = *(get_graph_weights (graph, i, ce->dest));

	    /* We can't eliminate variables that have nonzero weighted
	       edges between them.  */
	    if (weight && bitmap_other_than_zero_bit_set (weight))
	      {
		okay_to_elim = false;
		break;
	      }
	    w = get_varinfo (ce->dest)->node;

	    /* We can't eliminate the node if one of the predecessors is
	       part of a different strongly connected component.  */
	    if (!okay_to_elim)
	      {
		root = w;
		okay_to_elim = true;
	      }
	    else if (w != root)
	      {
		okay_to_elim = false;
		break;
	      }

	    /* Theorem 4 in Rountev and Chandra: If i is a direct node,
	       then Solution(i) is a subset of Solution (w), where w is a
	       predecessor in the graph.  
	       Corollary: If all predecessors of i have the same
	       points-to set, then i has that same points-to set as
	       those predecessors.  */
	    tmp = BITMAP_ALLOC (NULL);
	    bitmap_and_compl (tmp, get_varinfo (i)->solution,
			      get_varinfo (w)->solution);
	    if (!bitmap_empty_p (tmp))
	      {
		okay_to_elim = false;
		BITMAP_FREE (tmp);
		break;
	      }
	    BITMAP_FREE (tmp);
	  }

      /* See if the root is different than the original node. 
	 If so, we've found an equivalence.  */
      if (root != get_varinfo (i)->node && okay_to_elim)
	{
	  /* Found an equivalence */
	  get_varinfo (i)->node = root;
	  collapse_nodes (graph, root, i);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Collapsing %s into %s\n",
		     get_varinfo (i)->name,
		     get_varinfo (root)->name);
	  stats.collapsed_vars++;
	}
    }

  bitmap_obstack_release (&iteration_obstack);
  free_topo_info (ti);
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
  unsigned int size = VEC_length (varinfo_t, varmap);
  unsigned int i;

  changed_count = size;
  changed = sbitmap_alloc (size);
  sbitmap_ones (changed);
  
  /* The already collapsed/unreachable nodes will never change, so we
     need to  account for them in changed_count.  */
  for (i = 0; i < size; i++)
    if (get_varinfo (i)->node != i)
      changed_count--;
  
  while (changed_count > 0)
    {
      unsigned int i;
      struct topo_info *ti = init_topo_info ();
      stats.iterations++;

      bitmap_obstack_initialize (&iteration_obstack);
      
      if (edge_added)
	{
	  /* We already did cycle elimination once, when we did
	     variable substitution, so we don't need it again for the
	     first iteration.  */
	  if (stats.iterations > 1)
	    find_and_collapse_graph_cycles (graph, true);

	  edge_added = false;
	}

      compute_topo_order (graph, ti);

      while (VEC_length (unsigned, ti->topo_order) != 0)
	{
	  i = VEC_pop (unsigned, ti->topo_order);
	  gcc_assert (get_varinfo (i)->node == i);

	  /* If the node has changed, we need to process the
	     complex constraints and outgoing edges again.  */
	  if (TEST_BIT (changed, i))
	    {
	      unsigned int j;
	      constraint_t c;
	      constraint_edge_t e = NULL;
	      bitmap solution;
	      bitmap_iterator bi;
	      VEC(constraint_t,heap) *complex = get_varinfo (i)->complex;
	      VEC(constraint_edge_t,heap) *succs;

	      RESET_BIT (changed, i);
	      changed_count--;

	      /* Process the complex constraints */
	      solution = get_varinfo (i)->solution;
	      for (j = 0; VEC_iterate (constraint_t, complex, j, c); j++)
		do_complex_constraint (graph, c, solution);

	      /* Propagate solution to all successors.  */
	      succs = graph->succs[i];
	      
	      EXECUTE_IF_IN_NONNULL_BITMAP (graph->zero_weight_succs[i], 0, j, bi)
		{
		  bitmap tmp = get_varinfo (j)->solution;
		  bool flag = false;
		  
		  flag = set_union_with_increment (tmp, solution, 0);
		  
		  if (flag)
		    {
		      get_varinfo (j)->solution = tmp;
		      if (!TEST_BIT (changed, j))
			{
			  SET_BIT (changed, j);
			  changed_count++;
			}
		    }
		}
	      for (j = 0; VEC_iterate (constraint_edge_t, succs, j, e); j++)
		{
		  bitmap tmp = get_varinfo (e->dest)->solution;
		  bool flag = false;
		  unsigned int k;
		  bitmap weights = e->weights;
		  bitmap_iterator bi;

		  gcc_assert (weights && !bitmap_empty_p (weights));
		  EXECUTE_IF_SET_IN_BITMAP (weights, 0, k, bi)
		    flag |= set_union_with_increment (tmp, solution, k);

		  if (flag)
		    {
		      get_varinfo (e->dest)->solution = tmp;
		      if (!TEST_BIT (changed, e->dest))
			{
			  SET_BIT (changed, e->dest);
			  changed_count++;
			}
		    }
		}
	    }
	}
      free_topo_info (ti);
      bitmap_obstack_release (&iteration_obstack);
    }

  sbitmap_free (changed);
}


/* CONSTRAINT AND VARIABLE GENERATION FUNCTIONS */

/* Map from trees to variable ids.  */    
static htab_t id_for_tree;

typedef struct tree_id
{
  tree t;
  unsigned int id;
} *tree_id_t;

/* Hash a tree id structure.  */

static hashval_t 
tree_id_hash (const void *p)
{
  const tree_id_t ta = (tree_id_t) p;
  return htab_hash_pointer (ta->t);
}

/* Return true if the tree in P1 and the tree in P2 are the same.  */

static int
tree_id_eq (const void *p1, const void *p2)
{
  const tree_id_t ta1 = (tree_id_t) p1;
  const tree_id_t ta2 = (tree_id_t) p2;
  return ta1->t == ta2->t;
}

/* Insert ID as the variable id for tree T in the hashtable.  */

static void 
insert_id_for_tree (tree t, int id)
{
  void **slot;
  struct tree_id finder;
  tree_id_t new_pair;
  
  finder.t = t;
  slot = htab_find_slot (id_for_tree, &finder, INSERT);
  gcc_assert (*slot == NULL);
  new_pair = xmalloc (sizeof (struct tree_id));
  new_pair->t = t;
  new_pair->id = id;
  *slot = (void *)new_pair;
}

/* Find the variable id for tree T in ID_FOR_TREE.  If T does not
   exist in the hash table, return false, otherwise, return true and
   set *ID to the id we found.  */

static bool
lookup_id_for_tree (tree t, unsigned int *id)
{
  tree_id_t pair;
  struct tree_id finder;

  finder.t = t;
  pair = htab_find (id_for_tree,  &finder);
  if (pair == NULL)
    return false;
  *id = pair->id;
  return true;
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

/* Find the variable id for tree T in the hashtable.
   If T doesn't exist in the hash table, create an entry for it.  */

static unsigned int
get_id_for_tree (tree t)
{
  tree_id_t pair;
  struct tree_id finder;

  finder.t = t;
  pair = htab_find (id_for_tree,  &finder);
  if (pair == NULL)
    return create_variable_info_for (t, alias_get_name (t));
  
  return pair->id;
}

/* Get a constraint expression from an SSA_VAR_P node.  */

static struct constraint_expr
get_constraint_exp_from_ssa_var (tree t)
{
  struct constraint_expr cexpr;

  gcc_assert (SSA_VAR_P (t) || DECL_P (t));

  /* For parameters, get at the points-to set for the actual parm
     decl.  */
  if (TREE_CODE (t) == SSA_NAME 
      && TREE_CODE (SSA_NAME_VAR (t)) == PARM_DECL 
      && default_def (SSA_NAME_VAR (t)) == t)
    return get_constraint_exp_from_ssa_var (SSA_NAME_VAR (t));

  cexpr.type = SCALAR;
  
  cexpr.var = get_id_for_tree (t);
  /* If we determine the result is "anything", and we know this is readonly,
     say it points to readonly memory instead.  */
  if (cexpr.var == anything_id && TREE_READONLY (t))
    {
      cexpr.type = ADDRESSOF;
      cexpr.var = readonly_id;
    }
    
  cexpr.offset = 0;
  return cexpr;
}

/* Process a completed constraint T, and add it to the constraint
   list.  */

static void
process_constraint (constraint_t t)
{
  struct constraint_expr rhs = t->rhs;
  struct constraint_expr lhs = t->lhs;
  
  gcc_assert (rhs.var < VEC_length (varinfo_t, varmap));
  gcc_assert (lhs.var < VEC_length (varinfo_t, varmap));

  /* ANYTHING == ANYTHING is pointless.  */
  if (lhs.var == anything_id && rhs.var == anything_id)
    return;

  /* If we have &ANYTHING = something, convert to SOMETHING = &ANYTHING) */
  else if (lhs.var == anything_id && lhs.type == ADDRESSOF)
    {
      rhs = t->lhs;
      t->lhs = t->rhs;
      t->rhs = rhs;
      process_constraint (t);
    }   
  /* This can happen in our IR with things like n->a = *p */
  else if (rhs.type == DEREF && lhs.type == DEREF && rhs.var != anything_id)
    {
      /* Split into tmp = *rhs, *lhs = tmp */
      tree rhsdecl = get_varinfo (rhs.var)->decl;
      tree pointertype = TREE_TYPE (rhsdecl);
      tree pointedtotype = TREE_TYPE (pointertype);
      tree tmpvar = create_tmp_var_raw (pointedtotype, "doubledereftmp");
      struct constraint_expr tmplhs = get_constraint_exp_from_ssa_var (tmpvar);
      
      /* If this is an aggregate of known size, we should have passed
	 this off to do_structure_copy, and it should have broken it
	 up.  */
      gcc_assert (!AGGREGATE_TYPE_P (pointedtotype) 
		  || get_varinfo (rhs.var)->is_unknown_size_var);
      
      process_constraint (new_constraint (tmplhs, rhs));
      process_constraint (new_constraint (lhs, tmplhs));
    }
  else if (rhs.type == ADDRESSOF)
    {
      varinfo_t vi;
      gcc_assert (rhs.offset == 0);
      
      for (vi = get_varinfo (rhs.var); vi != NULL; vi = vi->next)
	vi->address_taken = true;

      VEC_safe_push (constraint_t, heap, constraints, t);
    }
  else
    {
      if (lhs.type != DEREF && rhs.type == DEREF)
	get_varinfo (lhs.var)->indirect_target = true;
      VEC_safe_push (constraint_t, heap, constraints, t);
    }
}


/* Return the position, in bits, of FIELD_DECL from the beginning of its
   structure.  */

static unsigned HOST_WIDE_INT
bitpos_of_field (const tree fdecl)
{

  if (TREE_CODE (DECL_FIELD_OFFSET (fdecl)) != INTEGER_CST
      || TREE_CODE (DECL_FIELD_BIT_OFFSET (fdecl)) != INTEGER_CST)
    return -1;
  
  return (tree_low_cst (DECL_FIELD_OFFSET (fdecl), 1) * 8) 
         + tree_low_cst (DECL_FIELD_BIT_OFFSET (fdecl), 1);
}


/* Return true if an access to [ACCESSPOS, ACCESSSIZE]
   overlaps with a field at [FIELDPOS, FIELDSIZE] */

static bool
offset_overlaps_with_access (const unsigned HOST_WIDE_INT fieldpos,
			     const unsigned HOST_WIDE_INT fieldsize,
			     const unsigned HOST_WIDE_INT accesspos,
			     const unsigned HOST_WIDE_INT accesssize)
{
  if (fieldpos == accesspos && fieldsize == accesssize)
    return true;
  if (accesspos >= fieldpos && accesspos < (fieldpos + fieldsize))
    return true;
  if (accesspos < fieldpos && (accesspos + accesssize > fieldpos))
    return true;
  
  return false;
}

/* Given a COMPONENT_REF T, return the constraint_expr for it.  */

static void
get_constraint_for_component_ref (tree t, VEC(ce_s, heap) **results, 
				  bool *anyoffset)
{
  tree orig_t = t;
  HOST_WIDE_INT bitsize = -1;
  HOST_WIDE_INT bitpos;
  tree offset = NULL_TREE;
  enum machine_mode mode;
  int unsignedp;
  int volatilep;
  tree forzero;
  struct constraint_expr *result;
  unsigned int beforelength = VEC_length (ce_s, *results);

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
 
  t = get_inner_reference (t, &bitsize, &bitpos, &offset, &mode,
			   &unsignedp, &volatilep, false);
  get_constraint_for (t, results, anyoffset);
  result = VEC_last (ce_s, *results);

  gcc_assert (beforelength + 1 == VEC_length (ce_s, *results));

  /* This can also happen due to weird offsetof type macros.  */
  if (TREE_CODE (t) != ADDR_EXPR && result->type == ADDRESSOF)
    result->type = SCALAR;
  
  /* If we know where this goes, then yay. Otherwise, booo. */

  if (offset == NULL && bitsize != -1)
    {
      result->offset = bitpos;
    }	
  /* FIXME: Handle the DEREF case.  */
  else if (anyoffset && result->type != DEREF)
    {
      result->offset = 0;
      *anyoffset = true;
    }
  else
    {
      result->var = anything_id;
      result->offset = 0;      
    }

  if (result->type == SCALAR)
    {
      /* In languages like C, you can access one past the end of an
	 array.  You aren't allowed to dereference it, so we can
	 ignore this constraint. When we handle pointer subtraction,
	 we may have to do something cute here.  */
      
      if (result->offset < get_varinfo (result->var)->fullsize)
	{
	  /* It's also not true that the constraint will actually start at the
	     right offset, it may start in some padding.  We only care about
	     setting the constraint to the first actual field it touches, so
	     walk to find it.  */ 
	  varinfo_t curr;
	  for (curr = get_varinfo (result->var); curr; curr = curr->next)
	    {
	      if (offset_overlaps_with_access (curr->offset, curr->size,
					       result->offset, bitsize))
		{
		  result->var = curr->id;
		  break;
		}
	    }
	  /* assert that we found *some* field there. The user couldn't be
	     accessing *only* padding.  */
	  /* Still the user could access one past the end of an array
	     embedded in a struct resulting in accessing *only* padding.  */
	  gcc_assert (curr || ref_contains_array_ref (orig_t));
	}
      else
	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, "Access to past the end of variable, ignoring\n");

      result->offset = 0;
    }
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
	  tree tmpvar = create_tmp_var_raw (ptr_type_node, "dereftmp");
	  struct constraint_expr tmplhs = get_constraint_exp_from_ssa_var (tmpvar);
	  process_constraint (new_constraint (tmplhs, *c));
	  c->var = tmplhs.var;
	}
      else
	gcc_unreachable ();
    }
}


/* Given a tree T, return the constraint expression for it.  */

static void
get_constraint_for (tree t, VEC (ce_s, heap) **results, bool *anyoffset)
{
  struct constraint_expr temp;

  /* x = integer is all glommed to a single variable, which doesn't
     point to anything by itself.  That is, of course, unless it is an
     integer constant being treated as a pointer, in which case, we
     will return that this is really the addressof anything.  This
     happens below, since it will fall into the default case. The only
     case we know something about an integer treated like a pointer is
     when it is the NULL pointer, and then we just say it points to
     NULL.  */
  if (TREE_CODE (t) == INTEGER_CST
      && !POINTER_TYPE_P (TREE_TYPE (t)))
    {
      temp.var = integer_id;
      temp.type = SCALAR;
      temp.offset = 0;
      VEC_safe_push (ce_s, heap, *results, &temp);
      return;
    }
  else if (TREE_CODE (t) == INTEGER_CST
	   && integer_zerop (t))
    {
      temp.var = nothing_id;
      temp.type = ADDRESSOF;
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
	    {
	      struct constraint_expr *c;
	      unsigned int i;

	      get_constraint_for (TREE_OPERAND (t, 0), results, anyoffset);
	      for (i = 0; VEC_iterate (ce_s, *results, i, c); i++)
		{
		  if (c->type == DEREF)
		    c->type = SCALAR;
		  else 
		    c->type = ADDRESSOF;
		}
	      return;
	    }
	    break;
	  case CALL_EXPR:
	    
	    /* XXX: In interprocedural mode, if we didn't have the
	       body, we would need to do *each pointer argument =
	       &ANYTHING added.  */
	    if (call_expr_flags (t) & (ECF_MALLOC | ECF_MAY_BE_ALLOCA))
	      {
		varinfo_t vi;
		tree heapvar = heapvar_lookup (t);
		
		if (heapvar == NULL)
		  {		    
		    heapvar = create_tmp_var_raw (ptr_type_node, "HEAP");
		    DECL_EXTERNAL (heapvar) = 1;
		    add_referenced_tmp_var (heapvar);
		    heapvar_insert (t, heapvar);
		  }

		temp.var = create_variable_info_for (heapvar,
						     alias_get_name (heapvar));
		
		vi = get_varinfo (temp.var);
		vi->is_artificial_var = 1;
		vi->is_heap_var = 1;
		temp.type = ADDRESSOF;
		temp.offset = 0;
		VEC_safe_push (ce_s, heap, *results, &temp);
		return;
	      }
	    /* FALLTHRU */
	  default:
	    {
	      temp.type = ADDRESSOF;
	      temp.var = anything_id;
	      temp.offset = 0;
	      VEC_safe_push (ce_s, heap, *results, &temp);
	      return;
	    }
	  }
      }
    case tcc_reference:
      {
	switch (TREE_CODE (t))
	  {
	  case INDIRECT_REF:
	    {
	      get_constraint_for (TREE_OPERAND (t, 0), results, anyoffset);
	      do_deref (results);
	      return;
	    }
	  case ARRAY_REF:
	  case ARRAY_RANGE_REF:
	  case COMPONENT_REF:
	    get_constraint_for_component_ref (t, results, anyoffset);
	    return;
	  default:
	    {
	      temp.type = ADDRESSOF;
	      temp.var = anything_id;
	      temp.offset = 0;
	      VEC_safe_push (ce_s, heap, *results, &temp);
	      return;
	    }
	  }
      }
    case tcc_unary:
      {
	switch (TREE_CODE (t))
	  {
	  case NOP_EXPR:
	  case CONVERT_EXPR:
	  case NON_LVALUE_EXPR:
	    {
	      tree op = TREE_OPERAND (t, 0);
	      
	      /* Cast from non-pointer to pointers are bad news for us.
		 Anything else, we see through */
	      if (!(POINTER_TYPE_P (TREE_TYPE (t))
		    && ! POINTER_TYPE_P (TREE_TYPE (op))))
		{
		  get_constraint_for (op, results, anyoffset);
		  return;
		}

	      /* FALLTHRU  */
	    }
	  default:
	    {
	      temp.type = ADDRESSOF;
	      temp.var = anything_id;
	      temp.offset = 0;
	      VEC_safe_push (ce_s, heap, *results, &temp);
	      return;
	    }
	  }
      }
    case tcc_exceptional:
      {
	switch (TREE_CODE (t))
	  {
	  case PHI_NODE:	   
	    {
	      get_constraint_for (PHI_RESULT (t), results, anyoffset);
	      return;
	    }
	    break;
	  case SSA_NAME:
	    {
	      struct constraint_expr temp;
	      temp = get_constraint_exp_from_ssa_var (t);
	      VEC_safe_push (ce_s, heap, *results, &temp);
	      return;
	    }
	    break;
	  default:
	    {
	      temp.type = ADDRESSOF;
	      temp.var = anything_id;
	      temp.offset = 0;
	      VEC_safe_push (ce_s, heap, *results, &temp);
	      return;
	    }
	  }
      }
    case tcc_declaration:
      {
	struct constraint_expr temp;
	temp = get_constraint_exp_from_ssa_var (t);
	VEC_safe_push (ce_s, heap, *results, &temp);
	return;
      }
    default:
      {
	temp.type = ADDRESSOF;
	temp.var = anything_id;
	temp.offset = 0;
	VEC_safe_push (ce_s, heap, *results, &temp);
	return;
      }
    }
}


/* Handle the structure copy case where we have a simple structure copy
   between LHS and RHS that is of SIZE (in bits) 
  
   For each field of the lhs variable (lhsfield)
     For each field of the rhs variable at lhsfield.offset (rhsfield)
       add the constraint lhsfield = rhsfield

   If we fail due to some kind of type unsafety or other thing we
   can't handle, return false.  We expect the caller to collapse the
   variable in that case.  */

static bool
do_simple_structure_copy (const struct constraint_expr lhs,
			  const struct constraint_expr rhs,
			  const unsigned HOST_WIDE_INT size)
{
  varinfo_t p = get_varinfo (lhs.var);
  unsigned HOST_WIDE_INT pstart, last;
  pstart = p->offset;
  last = p->offset + size;
  for (; p && p->offset < last; p = p->next)
    {
      varinfo_t q;
      struct constraint_expr templhs = lhs;
      struct constraint_expr temprhs = rhs;
      unsigned HOST_WIDE_INT fieldoffset;

      templhs.var = p->id;            
      q = get_varinfo (temprhs.var);
      fieldoffset = p->offset - pstart;
      q = first_vi_for_offset (q, q->offset + fieldoffset);
      if (!q)
	return false;
      temprhs.var = q->id;
      process_constraint (new_constraint (templhs, temprhs));
    }
  return true;
}


/* Handle the structure copy case where we have a  structure copy between a
   aggregate on the LHS and a dereference of a pointer on the RHS
   that is of SIZE (in bits) 
  
   For each field of the lhs variable (lhsfield)
       rhs.offset = lhsfield->offset
       add the constraint lhsfield = rhs
*/

static void
do_rhs_deref_structure_copy (const struct constraint_expr lhs,
			     const struct constraint_expr rhs,
			     const unsigned HOST_WIDE_INT size)
{
  varinfo_t p = get_varinfo (lhs.var);
  unsigned HOST_WIDE_INT pstart,last;
  pstart = p->offset;
  last = p->offset + size;

  for (; p && p->offset < last; p = p->next)
    {
      varinfo_t q;
      struct constraint_expr templhs = lhs;
      struct constraint_expr temprhs = rhs;
      unsigned HOST_WIDE_INT fieldoffset;


      if (templhs.type == SCALAR)
	templhs.var = p->id;      
      else
	templhs.offset = p->offset;
      
      q = get_varinfo (temprhs.var);
      fieldoffset = p->offset - pstart;      
      temprhs.offset += fieldoffset;
      process_constraint (new_constraint (templhs, temprhs));
    }
}

/* Handle the structure copy case where we have a structure copy
   between a aggregate on the RHS and a dereference of a pointer on
   the LHS that is of SIZE (in bits) 

   For each field of the rhs variable (rhsfield)
       lhs.offset = rhsfield->offset
       add the constraint lhs = rhsfield
*/

static void
do_lhs_deref_structure_copy (const struct constraint_expr lhs,
			     const struct constraint_expr rhs,
			     const unsigned HOST_WIDE_INT size)
{
  varinfo_t p = get_varinfo (rhs.var);
  unsigned HOST_WIDE_INT pstart,last;
  pstart = p->offset;
  last = p->offset + size;

  for (; p && p->offset < last; p = p->next)
    {
      varinfo_t q;
      struct constraint_expr templhs = lhs;
      struct constraint_expr temprhs = rhs;
      unsigned HOST_WIDE_INT fieldoffset;


      if (temprhs.type == SCALAR)
	temprhs.var = p->id;      
      else
	temprhs.offset = p->offset;
      
      q = get_varinfo (templhs.var);
      fieldoffset = p->offset - pstart;      
      templhs.offset += fieldoffset;
      process_constraint (new_constraint (templhs, temprhs));
    }
}

/* Sometimes, frontends like to give us bad type information.  This
   function will collapse all the fields from VAR to the end of VAR,
   into VAR, so that we treat those fields as a single variable. 
   We return the variable they were collapsed into.  */

static unsigned int
collapse_rest_of_var (unsigned int var)
{
  varinfo_t currvar = get_varinfo (var);
  varinfo_t field;

  for (field = currvar->next; field; field = field->next)
    {
      if (dump_file)
	fprintf (dump_file, "Type safety: Collapsing var %s into %s\n", 
		 field->name, currvar->name);
      
      gcc_assert (!field->collapsed_to);
      field->collapsed_to = currvar;
    }

  currvar->next = NULL;
  currvar->size = currvar->fullsize - currvar->offset;
  
  return currvar->id;
}

/* Handle aggregate copies by expanding into copies of the respective
   fields of the structures.  */

static void
do_structure_copy (tree lhsop, tree rhsop)
{
  struct constraint_expr lhs, rhs, tmp;
  VEC (ce_s, heap) *lhsc = NULL, *rhsc = NULL;
  varinfo_t p;
  unsigned HOST_WIDE_INT lhssize;
  unsigned HOST_WIDE_INT rhssize;

  get_constraint_for (lhsop, &lhsc, NULL);
  get_constraint_for (rhsop, &rhsc, NULL);
  gcc_assert (VEC_length (ce_s, lhsc) == 1);
  gcc_assert (VEC_length (ce_s, rhsc) == 1);
  lhs = *(VEC_last (ce_s, lhsc));
  rhs = *(VEC_last (ce_s, rhsc));
  
  VEC_free (ce_s, heap, lhsc);
  VEC_free (ce_s, heap, rhsc);

  /* If we have special var = x, swap it around.  */
  if (lhs.var <= integer_id && !(get_varinfo (rhs.var)->is_special_var))
    {
      tmp = lhs;
      lhs = rhs;
      rhs = tmp;
    }
  
  /*  This is fairly conservative for the RHS == ADDRESSOF case, in that it's
      possible it's something we could handle.  However, most cases falling
      into this are dealing with transparent unions, which are slightly
      weird. */
  if (rhs.type == ADDRESSOF && !(get_varinfo (rhs.var)->is_special_var))
    {
      rhs.type = ADDRESSOF;
      rhs.var = anything_id;
    }

  /* If the RHS is a special var, or an addressof, set all the LHS fields to
     that special var.  */
  if (rhs.var <= integer_id)
    {
      for (p = get_varinfo (lhs.var); p; p = p->next)
	{
	  struct constraint_expr templhs = lhs;
	  struct constraint_expr temprhs = rhs;

	  if (templhs.type == SCALAR )
	    templhs.var = p->id;
	  else
	    templhs.offset += p->offset;
	  process_constraint (new_constraint (templhs, temprhs));
	}
    }
  else
    {
      tree rhstype = TREE_TYPE (rhsop);
      tree lhstype = TREE_TYPE (lhsop);
      tree rhstypesize;
      tree lhstypesize;

      lhstypesize = DECL_P (lhsop) ? DECL_SIZE (lhsop) : TYPE_SIZE (lhstype);
      rhstypesize = DECL_P (rhsop) ? DECL_SIZE (rhsop) : TYPE_SIZE (rhstype);

      /* If we have a variably sized types on the rhs or lhs, and a deref
	 constraint, add the constraint, lhsconstraint = &ANYTHING.
	 This is conservatively correct because either the lhs is an unknown
	 sized var (if the constraint is SCALAR), or the lhs is a DEREF
	 constraint, and every variable it can point to must be unknown sized
	 anyway, so we don't need to worry about fields at all.  */
      if ((rhs.type == DEREF && TREE_CODE (rhstypesize) != INTEGER_CST)
	  || (lhs.type == DEREF && TREE_CODE (lhstypesize) != INTEGER_CST))
	{
	  rhs.var = anything_id;
	  rhs.type = ADDRESSOF;
	  rhs.offset = 0;
	  process_constraint (new_constraint (lhs, rhs));
	  return;
	}

      /* The size only really matters insofar as we don't set more or less of
	 the variable.  If we hit an unknown size var, the size should be the
	 whole darn thing.  */
      if (get_varinfo (rhs.var)->is_unknown_size_var)
	rhssize = ~0;
      else
	rhssize = TREE_INT_CST_LOW (rhstypesize);

      if (get_varinfo (lhs.var)->is_unknown_size_var)
	lhssize = ~0;
      else
	lhssize = TREE_INT_CST_LOW (lhstypesize);

  
      if (rhs.type == SCALAR && lhs.type == SCALAR)  
	{
	  if (!do_simple_structure_copy (lhs, rhs, MIN (lhssize, rhssize)))
	    {	      
	      lhs.var = collapse_rest_of_var (lhs.var);
	      rhs.var = collapse_rest_of_var (rhs.var);
	      lhs.offset = 0;
	      rhs.offset = 0;
	      lhs.type = SCALAR;
	      rhs.type = SCALAR;
	      process_constraint (new_constraint (lhs, rhs));
	    }
	}
      else if (lhs.type != DEREF && rhs.type == DEREF)
	do_rhs_deref_structure_copy (lhs, rhs, MIN (lhssize, rhssize));
      else if (lhs.type == DEREF && rhs.type != DEREF)
	do_lhs_deref_structure_copy (lhs, rhs, MIN (lhssize, rhssize));
      else
	{
	  tree pointedtotype = lhstype;
	  tree tmpvar;  

	  gcc_assert (rhs.type == DEREF && lhs.type == DEREF);
	  tmpvar = create_tmp_var_raw (pointedtotype, "structcopydereftmp");
	  do_structure_copy (tmpvar, rhsop);
	  do_structure_copy (lhsop, tmpvar);
	}
    }
}

/* Update related alias information kept in AI.  This is used when
   building name tags, alias sets and deciding grouping heuristics.
   STMT is the statement to process.  This function also updates
   ADDRESSABLE_VARS.  */

static void
update_alias_info (tree stmt, struct alias_info *ai)
{
  bitmap addr_taken;
  use_operand_p use_p;
  ssa_op_iter iter;
  bool stmt_escapes_p = is_escape_site (stmt, ai);
  tree op;

  /* Mark all the variables whose address are taken by the statement.  */
  addr_taken = addresses_taken (stmt);
  if (addr_taken)
    {
      bitmap_ior_into (addressable_vars, addr_taken);

      /* If STMT is an escape point, all the addresses taken by it are
	 call-clobbered.  */
      if (stmt_escapes_p)
	{
	  bitmap_iterator bi;
	  unsigned i;

	  EXECUTE_IF_SET_IN_BITMAP (addr_taken, 0, i, bi)
	    mark_call_clobbered (referenced_var (i));
	}
    }

  /* Process each operand use.  If an operand may be aliased, keep
     track of how many times it's being used.  For pointers, determine
     whether they are dereferenced by the statement, or whether their
     value escapes, etc.  */
  FOR_EACH_PHI_OR_STMT_USE (use_p, stmt, iter, SSA_OP_USE)
    {
      tree op, var;
      var_ann_t v_ann;
      struct ptr_info_def *pi;
      bool is_store, is_potential_deref;
      unsigned num_uses, num_derefs;

      op = USE_FROM_PTR (use_p);

      /* If STMT is a PHI node, OP may be an ADDR_EXPR.  If so, add it
	 to the set of addressable variables.  */
      if (TREE_CODE (op) == ADDR_EXPR)
	{
	  gcc_assert (TREE_CODE (stmt) == PHI_NODE);

	  /* PHI nodes don't have annotations for pinning the set
	     of addresses taken, so we collect them here.

	     FIXME, should we allow PHI nodes to have annotations
	     so that they can be treated like regular statements?
	     Currently, they are treated as second-class
	     statements.  */
	  add_to_addressable_set (TREE_OPERAND (op, 0), &addressable_vars);
	  continue;
	}

      /* Ignore constants.  */
      if (TREE_CODE (op) != SSA_NAME)
	continue;

      var = SSA_NAME_VAR (op);
      v_ann = var_ann (var);

      /* If the operand's variable may be aliased, keep track of how
	 many times we've referenced it.  This is used for alias
	 grouping in compute_flow_insensitive_aliasing.  */
      if (may_be_aliased (var))
	NUM_REFERENCES_INC (v_ann);

      /* We are only interested in pointers.  */
      if (!POINTER_TYPE_P (TREE_TYPE (op)))
	continue;

      pi = get_ptr_info (op);

      /* Add OP to AI->PROCESSED_PTRS, if it's not there already.  */
      if (!TEST_BIT (ai->ssa_names_visited, SSA_NAME_VERSION (op)))
	{
	  SET_BIT (ai->ssa_names_visited, SSA_NAME_VERSION (op));
	  VARRAY_PUSH_TREE (ai->processed_ptrs, op);
	}

      /* If STMT is a PHI node, then it will not have pointer
	 dereferences and it will not be an escape point.  */
      if (TREE_CODE (stmt) == PHI_NODE)
	continue;

      /* Determine whether OP is a dereferenced pointer, and if STMT
	 is an escape point, whether OP escapes.  */
      count_uses_and_derefs (op, stmt, &num_uses, &num_derefs, &is_store);

      /* Handle a corner case involving address expressions of the
	 form '&PTR->FLD'.  The problem with these expressions is that
	 they do not represent a dereference of PTR.  However, if some
	 other transformation propagates them into an INDIRECT_REF
	 expression, we end up with '*(&PTR->FLD)' which is folded
	 into 'PTR->FLD'.

	 So, if the original code had no other dereferences of PTR,
	 the aliaser will not create memory tags for it, and when
	 &PTR->FLD gets propagated to INDIRECT_REF expressions, the
	 memory operations will receive no V_MAY_DEF/VUSE operands.

	 One solution would be to have count_uses_and_derefs consider
	 &PTR->FLD a dereference of PTR.  But that is wrong, since it
	 is not really a dereference but an offset calculation.

	 What we do here is to recognize these special ADDR_EXPR
	 nodes.  Since these expressions are never GIMPLE values (they
	 are not GIMPLE invariants), they can only appear on the RHS
	 of an assignment and their base address is always an
	 INDIRECT_REF expression.  */
      is_potential_deref = false;
      if (TREE_CODE (stmt) == MODIFY_EXPR
	  && TREE_CODE (TREE_OPERAND (stmt, 1)) == ADDR_EXPR
	  && !is_gimple_val (TREE_OPERAND (stmt, 1)))
	{
	  /* If the RHS if of the form &PTR->FLD and PTR == OP, then
	     this represents a potential dereference of PTR.  */
	  tree rhs = TREE_OPERAND (stmt, 1);
	  tree base = get_base_address (TREE_OPERAND (rhs, 0));
	  if (TREE_CODE (base) == INDIRECT_REF
	      && TREE_OPERAND (base, 0) == op)
	    is_potential_deref = true;
	}

      if (num_derefs > 0 || is_potential_deref)
	{
	  /* Mark OP as dereferenced.  In a subsequent pass,
	     dereferenced pointers that point to a set of
	     variables will be assigned a name tag to alias
	     all the variables OP points to.  */
	  pi->is_dereferenced = 1;

	  /* Keep track of how many time we've dereferenced each
	     pointer.  */
	  NUM_REFERENCES_INC (v_ann);

	  /* If this is a store operation, mark OP as being
	     dereferenced to store, otherwise mark it as being
	     dereferenced to load.  */
	  if (is_store)
	    bitmap_set_bit (ai->dereferenced_ptrs_store, DECL_UID (var));
	  else
	    bitmap_set_bit (ai->dereferenced_ptrs_load, DECL_UID (var));
	}

      if (stmt_escapes_p && num_derefs < num_uses)
	{
	  /* If STMT is an escape point and STMT contains at
	     least one direct use of OP, then the value of OP
	     escapes and so the pointed-to variables need to
	     be marked call-clobbered.  */
	  pi->value_escapes_p = 1;

	  /* If the statement makes a function call, assume
	     that pointer OP will be dereferenced in a store
	     operation inside the called function.  */
	  if (get_call_expr_in (stmt))
	    {
	      bitmap_set_bit (ai->dereferenced_ptrs_store, DECL_UID (var));
	      pi->is_dereferenced = 1;
	    }
	}
    }

  if (TREE_CODE (stmt) == PHI_NODE)
    return;

  /* Update reference counter for definitions to any
     potentially aliased variable.  This is used in the alias
     grouping heuristics.  */
  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_DEF)
    {
      tree var = SSA_NAME_VAR (op);
      var_ann_t ann = var_ann (var);
      bitmap_set_bit (ai->written_vars, DECL_UID (var));
      if (may_be_aliased (var))
	NUM_REFERENCES_INC (ann);
      
    }
  
  /* Mark variables in V_MAY_DEF operands as being written to.  */
  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_VIRTUAL_DEFS)
    {
      tree var = DECL_P (op) ? op : SSA_NAME_VAR (op);
      bitmap_set_bit (ai->written_vars, DECL_UID (var));
    }
}


/* Handle pointer arithmetic EXPR when creating aliasing constraints.
   Expressions of the type PTR + CST can be handled in two ways:

   1- If the constraint for PTR is ADDRESSOF for a non-structure
      variable, then we can use it directly because adding or
      subtracting a constant may not alter the original ADDRESSOF
      constraint (i.e., pointer arithmetic may not legally go outside
      an object's boundaries).

   2- If the constraint for PTR is ADDRESSOF for a structure variable,
      then if CST is a compile-time constant that can be used as an
      offset, we can determine which sub-variable will be pointed-to
      by the expression.

   Return true if the expression is handled.  For any other kind of
   expression, return false so that each operand can be added as a
   separate constraint by the caller.  */

static bool
handle_ptr_arith (VEC (ce_s, heap) *lhsc, tree expr)
{
  tree op0, op1;
  struct constraint_expr *c, *c2;
  unsigned int i = 0;
  unsigned int j = 0;
  VEC (ce_s, heap) *temp = NULL;
  unsigned int rhsoffset = 0;

  if (TREE_CODE (expr) != PLUS_EXPR)
    return false;

  op0 = TREE_OPERAND (expr, 0);
  op1 = TREE_OPERAND (expr, 1);

  get_constraint_for (op0, &temp, NULL);
  if (POINTER_TYPE_P (TREE_TYPE (op0))
      && TREE_CODE (TREE_TYPE (TREE_TYPE (op0))) == RECORD_TYPE
      && TREE_CODE (op1) == INTEGER_CST)
    {
      rhsoffset = TREE_INT_CST_LOW (op1) * BITS_PER_UNIT;
    }
  

  for (i = 0; VEC_iterate (ce_s, lhsc, i, c); i++)
    for (j = 0; VEC_iterate (ce_s, temp, j, c2); j++)
      {
	if (c2->type == ADDRESSOF && rhsoffset != 0)
	  {
	    varinfo_t temp = get_varinfo (c2->var);
	    
	    gcc_assert (first_vi_for_offset (temp, rhsoffset) != NULL);
	    c2->var = first_vi_for_offset (temp, rhsoffset)->id;
	    c2->offset = 0;
	  }
	else
	  c2->offset = rhsoffset;
	process_constraint (new_constraint (*c, *c2));
      }

  VEC_free (ce_s, heap, temp);

  return true;
}


/* Walk statement T setting up aliasing constraints according to the
   references found in T.  This function is the main part of the
   constraint builder.  AI points to auxiliary alias information used
   when building alias sets and computing alias grouping heuristics.  */

static void
find_func_aliases (tree origt)
{
  tree t = origt;
  VEC(ce_s, heap) *lhsc = NULL;
  VEC(ce_s, heap) *rhsc = NULL;
  struct constraint_expr *c;

  if (TREE_CODE (t) == RETURN_EXPR && TREE_OPERAND (t, 0))
    t = TREE_OPERAND (t, 0);

  /* Now build constraints expressions.  */
  if (TREE_CODE (t) == PHI_NODE)
    {
      /* Only care about pointers and structures containing
	 pointers.  */
      if (POINTER_TYPE_P (TREE_TYPE (PHI_RESULT (t)))
	  || AGGREGATE_TYPE_P (TREE_TYPE (PHI_RESULT (t))))
	{
	  int i;
	  unsigned int j;
	  
	  /* For a phi node, assign all the arguments to
	     the result.  */
	  get_constraint_for (PHI_RESULT (t), &lhsc, NULL);
	  for (i = 0; i < PHI_NUM_ARGS (t); i++)
	    { 
	      get_constraint_for (PHI_ARG_DEF (t, i), &rhsc, NULL);
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
     arguments through their calls.   There are two case, either a
     modify_expr when we are returning a value, or just a plain
     call_expr when we are not.   */
  else if (in_ipa_mode
	   && ((TREE_CODE (t) == MODIFY_EXPR 
		&& TREE_CODE (TREE_OPERAND (t, 1)) == CALL_EXPR
	       && !(call_expr_flags (TREE_OPERAND (t, 1)) 
		    & (ECF_MALLOC | ECF_MAY_BE_ALLOCA)))
	       || (TREE_CODE (t) == CALL_EXPR 
		   && !(call_expr_flags (t) 
			& (ECF_MALLOC | ECF_MAY_BE_ALLOCA)))))
    {
      tree lhsop;
      tree rhsop;
      unsigned int varid;
      bool found = false;
      tree arglist;
      varinfo_t fi;
      int i = 1;
      tree decl;
      if (TREE_CODE (t) == MODIFY_EXPR)
	{
	  lhsop = TREE_OPERAND (t, 0);
	  rhsop = TREE_OPERAND (t, 1);
	}
      else
	{
	  lhsop = NULL;
	  rhsop = t;
	}
      decl = get_callee_fndecl (rhsop);

      /* If we can directly resolve the function being called, do so.
	 Otherwise, it must be some sort of indirect expression that
	 we should still be able to handle.  */
      if (decl)
	{
	  found = lookup_id_for_tree (decl, &varid);
	  gcc_assert (found);
	}
      else
	{
	  decl = TREE_OPERAND (rhsop, 0);
	  found = lookup_id_for_tree (decl, &varid);
	  gcc_assert (found);
	}

      /* Assign all the passed arguments to the approriate incoming
	 parameters of the function.  */
      fi = get_varinfo (varid);
      arglist = TREE_OPERAND (rhsop, 1);
	
      for (;arglist; arglist = TREE_CHAIN (arglist))
	{
	  tree arg = TREE_VALUE (arglist);
	  struct constraint_expr lhs ;
	  struct constraint_expr *rhsp;

	  get_constraint_for (arg, &rhsc, NULL);
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
	  
	  get_constraint_for (lhsop, &lhsc, NULL);
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
  /* Otherwise, just a regular assignment statement.  */
  else if (TREE_CODE (t) == MODIFY_EXPR)
    {
      tree lhsop = TREE_OPERAND (t, 0);
      tree rhsop = TREE_OPERAND (t, 1);
      int i;

      if (AGGREGATE_TYPE_P (TREE_TYPE (lhsop)) 
	  && AGGREGATE_TYPE_P (TREE_TYPE (rhsop)))
	{
	  do_structure_copy (lhsop, rhsop);
	}
      else
	{
	  /* Only care about operations with pointers, structures
	     containing pointers, dereferences, and call expressions.  */
	  if (POINTER_TYPE_P (TREE_TYPE (lhsop))
	      || AGGREGATE_TYPE_P (TREE_TYPE (lhsop))
	      || TREE_CODE (rhsop) == CALL_EXPR)
	    {
	      get_constraint_for (lhsop, &lhsc, NULL);
	      switch (TREE_CODE_CLASS (TREE_CODE (rhsop)))
		{
		  /* RHS that consist of unary operations,
		     exceptional types, or bare decls/constants, get
		     handled directly by get_constraint_for.  */ 
		  case tcc_reference:
		  case tcc_declaration:
		  case tcc_constant:
		  case tcc_exceptional:
		  case tcc_expression:
		  case tcc_unary:
		      {
			unsigned int j;
			bool need_anyoffset = false;
			tree strippedrhs = rhsop;
			tree rhstype;

			/* XXX: Push this back into the ADDR_EXPR
			   case, and remove anyoffset handling.  */
			STRIP_NOPS (strippedrhs);
			rhstype = TREE_TYPE (strippedrhs);
			
			get_constraint_for (rhsop, &rhsc, &need_anyoffset);
			if (TREE_CODE (strippedrhs) == ADDR_EXPR
			    && AGGREGATE_TYPE_P (TREE_TYPE (rhstype)))
			  {
			    struct constraint_expr *origrhs;
			    varinfo_t origvar;
			    struct constraint_expr tmp;

			    gcc_assert (VEC_length (ce_s, rhsc) == 1);
			    origrhs = VEC_last (ce_s, rhsc);
			    tmp = *origrhs;
			    VEC_pop (ce_s, rhsc);
			    origvar = get_varinfo (origrhs->var);
			    for (; origvar; origvar = origvar->next)
			      {
				tmp.var = origvar->id;
				VEC_safe_push (ce_s, heap, rhsc, &tmp);
			      }
			  }

			for (j = 0; VEC_iterate (ce_s, lhsc, j, c); j++)
			  {
			    struct constraint_expr *c2;
			    unsigned int k;
			    
			    for (k = 0; VEC_iterate (ce_s, rhsc, k, c2); k++)
			      process_constraint (new_constraint (*c, *c2));
			  }

		      }
		    break;

		  case tcc_binary:
		      {
			/* For pointer arithmetic of the form
			   PTR + CST, we can simply use PTR's
			   constraint because pointer arithmetic is
			   not allowed to go out of bounds.  */
			if (handle_ptr_arith (lhsc, rhsop))
			  break;
		      }
		    /* FALLTHRU  */

		  /* Otherwise, walk each operand.  Notice that we
		     can't use the operand interface because we need
		     to process expressions other than simple operands
		     (e.g. INDIRECT_REF, ADDR_EXPR, CALL_EXPR).  */
		  default:
		    for (i = 0; i < TREE_CODE_LENGTH (TREE_CODE (rhsop)); i++)
		      {
			tree op = TREE_OPERAND (rhsop, i);
			unsigned int j;

			gcc_assert (VEC_length (ce_s, rhsc) == 0);
			get_constraint_for (op, &rhsc, NULL);
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
	}
    }

  /* After promoting variables and computing aliasing we will
     need to re-scan most statements.  FIXME: Try to minimize the
     number of statements re-scanned.  It's not really necessary to
     re-scan *all* statements.  */  
  mark_stmt_modified (origt);
  VEC_free (ce_s, heap, rhsc);
  VEC_free (ce_s, heap, lhsc);
}


/* Find the first varinfo in the same variable as START that overlaps with
   OFFSET.
   Effectively, walk the chain of fields for the variable START to find the
   first field that overlaps with OFFSET.
   Return NULL if we can't find one.  */

static varinfo_t 
first_vi_for_offset (varinfo_t start, unsigned HOST_WIDE_INT offset)
{
  varinfo_t curr = start;
  while (curr)
    {
      /* We may not find a variable in the field list with the actual
	 offset when when we have glommed a structure to a variable.
	 In that case, however, offset should still be within the size
	 of the variable. */
      if (offset >= curr->offset && offset < (curr->offset +  curr->size))
	return curr;
      curr = curr->next;
    }
  return NULL;
}


/* Insert the varinfo FIELD into the field list for BASE, ordered by
   offset.  */

static void
insert_into_field_list (varinfo_t base, varinfo_t field)
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

/* qsort comparison function for two fieldoff's PA and PB */

static int 
fieldoff_compare (const void *pa, const void *pb)
{
  const fieldoff_s *foa = (const fieldoff_s *)pa;
  const fieldoff_s *fob = (const fieldoff_s *)pb;
  HOST_WIDE_INT foasize, fobsize;
  
  if (foa->offset != fob->offset)
    return foa->offset - fob->offset;

  foasize = TREE_INT_CST_LOW (DECL_SIZE (foa->field));
  fobsize = TREE_INT_CST_LOW (DECL_SIZE (fob->field));
  return foasize - fobsize;
}

/* Sort a fieldstack according to the field offset and sizes.  */
void sort_fieldstack (VEC(fieldoff_s,heap) *fieldstack)
{
  qsort (VEC_address (fieldoff_s, fieldstack), 
	 VEC_length (fieldoff_s, fieldstack), 
	 sizeof (fieldoff_s),
	 fieldoff_compare);
}

/* Given a TYPE, and a vector of field offsets FIELDSTACK, push all the fields
   of TYPE onto fieldstack, recording their offsets along the way.
   OFFSET is used to keep track of the offset in this entire structure, rather
   than just the immediately containing structure.  Returns the number
   of fields pushed.
   HAS_UNION is set to true if we find a union type as a field of
   TYPE.  */

int
push_fields_onto_fieldstack (tree type, VEC(fieldoff_s,heap) **fieldstack, 
			     HOST_WIDE_INT offset, bool *has_union)
{
  tree field;
  int count = 0;

  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    if (TREE_CODE (field) == FIELD_DECL)
      {
	bool push = false;
	int pushed = 0;
	
	if (has_union 
	    && (TREE_CODE (TREE_TYPE (field)) == QUAL_UNION_TYPE
		|| TREE_CODE (TREE_TYPE (field)) == UNION_TYPE))
	  *has_union = true;
	
	if (!var_can_have_subvars (field))
	  push = true;
	else if (!(pushed = push_fields_onto_fieldstack
		   (TREE_TYPE (field), fieldstack,
		    offset + bitpos_of_field (field), has_union))
		 && DECL_SIZE (field)
		 && !integer_zerop (DECL_SIZE (field)))
	  /* Empty structures may have actual size, like in C++. So
	     see if we didn't push any subfields and the size is
	     nonzero, push the field onto the stack */
	  push = true;
	
	if (push)
	  {
	    fieldoff_s *pair;

	    pair = VEC_safe_push (fieldoff_s, heap, *fieldstack, NULL);
	    pair->field = field;
	    pair->offset = offset + bitpos_of_field (field);
	    count++;
	  }
	else
	  count += pushed;
      }

  return count;
}

static void
make_constraint_to_anything (varinfo_t vi)
{
  struct constraint_expr lhs, rhs;
  
  lhs.var = vi->id;
  lhs.offset = 0;
  lhs.type = SCALAR;
  
  rhs.var = anything_id;
  rhs.offset =0 ;
  rhs.type = ADDRESSOF;
  process_constraint (new_constraint (lhs, rhs));
}

/* Count the number of arguments DECL has, and set IS_VARARGS to true
   if it is a varargs function.  */

static unsigned int
count_num_arguments (tree decl, bool *is_varargs)
{
  unsigned int i = 0;
  tree t;

  for (t = TYPE_ARG_TYPES (TREE_TYPE (decl)); 
       t;
       t = TREE_CHAIN (t))
    {	
      if (TREE_VALUE (t) == void_type_node)
	break;
      i++;
    }
  
  if (!t)
    *is_varargs = true;
  return i;
}

/* Creation function node for DECL, using NAME, and return the index
   of the variable we've created for the function.  */

static unsigned int
create_function_info_for (tree decl, const char *name)
{
  unsigned int index = VEC_length (varinfo_t, varmap);
  varinfo_t vi;
  tree arg; 
  unsigned int i;
  bool is_varargs = false;

  /* Create the variable info.  */

  vi = new_var_info (decl, index, name, index);
  vi->decl = decl;
  vi->offset = 0;
  vi->has_union = 0;
  vi->size = 1;
  vi->fullsize = count_num_arguments (decl, &is_varargs) + 1;
  insert_id_for_tree (vi->decl, index);  
  VEC_safe_push (varinfo_t, heap, varmap, vi);

  stats.total_vars++;

  /* If it's varargs, we don't know how many arguments it has, so we
     can't do much.
  */
  if (is_varargs)
    {
      vi->fullsize = ~0;
      vi->size = ~0;
      vi->is_unknown_size_var = true;
      return index;
    }

  
  arg = DECL_ARGUMENTS (decl);

  /* Set up varirables for each argument.  */
  for (i = 1; i < vi->fullsize; i++)
    {      
      varinfo_t argvi;
      const char *newname;
      char *tempname;
      unsigned int newindex;
      tree argdecl = decl;

      if (arg)
	argdecl = arg;
      
      newindex = VEC_length (varinfo_t, varmap);
      asprintf (&tempname, "%s.arg%d", name, i-1);
      newname = ggc_strdup (tempname);
      free (tempname);

      argvi = new_var_info (argdecl, newindex,newname, newindex);
      argvi->decl = argdecl;
      VEC_safe_push (varinfo_t, heap, varmap, argvi);
      argvi->offset = i;
      argvi->size = 1;
      argvi->fullsize = vi->fullsize;
      argvi->has_union = false;
      insert_into_field_list (vi, argvi);
      stats.total_vars ++;
      if (arg)
	{
	  insert_id_for_tree (arg, newindex);
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
      unsigned int newindex;
      tree resultdecl = decl;

      vi->fullsize ++;


      if (DECL_RESULT (decl))
	resultdecl = DECL_RESULT (decl);
      
      newindex = VEC_length (varinfo_t, varmap);
      asprintf (&tempname, "%s.result", name);
      newname = ggc_strdup (tempname);
      free (tempname);

      resultvi = new_var_info (resultdecl, newindex, newname, newindex);
      resultvi->decl = resultdecl;
      VEC_safe_push (varinfo_t, heap, varmap, resultvi);
      resultvi->offset = i;
      resultvi->size = 1;
      resultvi->fullsize = vi->fullsize;
      resultvi->has_union = false;
      insert_into_field_list (vi, resultvi);
      stats.total_vars ++;
      if (DECL_RESULT (decl))
	insert_id_for_tree (DECL_RESULT (decl), newindex);
    }
  return index;
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
  unsigned int index = VEC_length (varinfo_t, varmap);
  varinfo_t vi;
  tree decltype = TREE_TYPE (decl);
  tree declsize = DECL_P (decl) ? DECL_SIZE (decl) : TYPE_SIZE (decltype);
  bool notokay = false;
  bool hasunion;
  bool is_global = DECL_P (decl) ? is_global_var (decl) : false;
  VEC (fieldoff_s,heap) *fieldstack = NULL;
  
  if (TREE_CODE (decl) == FUNCTION_DECL && in_ipa_mode)
    return create_function_info_for (decl, name);

  hasunion = TREE_CODE (decltype) == UNION_TYPE
             || TREE_CODE (decltype) == QUAL_UNION_TYPE;
  if (var_can_have_subvars (decl) && use_field_sensitive && !hasunion)
    {
      push_fields_onto_fieldstack (decltype, &fieldstack, 0, &hasunion);
      if (hasunion)
	{
	  VEC_free (fieldoff_s, heap, fieldstack);
	  notokay = true;
	}
    }
  

  /* If the variable doesn't have subvars, we may end up needing to
     sort the field list and create fake variables for all the
     fields.  */
  vi = new_var_info (decl, index, name, index);
  vi->decl = decl;
  vi->offset = 0;
  vi->has_union = hasunion;
  if (!declsize
      || TREE_CODE (declsize) != INTEGER_CST
      || TREE_CODE (decltype) == UNION_TYPE
      || TREE_CODE (decltype) == QUAL_UNION_TYPE)
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
  
  insert_id_for_tree (vi->decl, index);  
  VEC_safe_push (varinfo_t, heap, varmap, vi);
  if (is_global && (!flag_whole_program || !in_ipa_mode))
    make_constraint_to_anything (vi);

  stats.total_vars++;
  if (use_field_sensitive 
      && !notokay 
      && !vi->is_unknown_size_var 
      && var_can_have_subvars (decl))
    {
      unsigned int newindex = VEC_length (varinfo_t, varmap);
      fieldoff_s *fo = NULL;
      unsigned int i;
      tree field;

      for (i = 0; !notokay && VEC_iterate (fieldoff_s, fieldstack, i, fo); i++)
	{
	  if (!DECL_SIZE (fo->field) 
	      || TREE_CODE (DECL_SIZE (fo->field)) != INTEGER_CST
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
	  VEC_free (fieldoff_s, heap, fieldstack);
	  return index;
	}
      
      field = fo->field;
      vi->size = TREE_INT_CST_LOW (DECL_SIZE (field));
      vi->offset = fo->offset;
      for (i = 1; VEC_iterate (fieldoff_s, fieldstack, i, fo); i++)
	{
	  varinfo_t newvi;
	  const char *newname;
	  char *tempname;

	  field = fo->field;
	  newindex = VEC_length (varinfo_t, varmap);
	  asprintf (&tempname, "%s.%s", vi->name, alias_get_name (field));
	  newname = ggc_strdup (tempname);
	  free (tempname);
	  newvi = new_var_info (decl, newindex, newname, newindex);
	  newvi->offset = fo->offset;
	  newvi->size = TREE_INT_CST_LOW (DECL_SIZE (field));
	  newvi->fullsize = vi->fullsize;
	  insert_into_field_list (vi, newvi);
	  VEC_safe_push (varinfo_t, heap, varmap, newvi);
	  if (is_global && (!flag_whole_program || !in_ipa_mode))
	    make_constraint_to_anything (newvi);

	  stats.total_vars++;
	}
      VEC_free (fieldoff_s, heap, fieldstack);
    }
  return index;
}

/* Print out the points-to solution for VAR to FILE.  */

void
dump_solution_for_var (FILE *file, unsigned int var)
{
  varinfo_t vi = get_varinfo (var);
  unsigned int i;
  bitmap_iterator bi; 
  
  fprintf (file, "%s = { ", vi->name);
  EXECUTE_IF_SET_IN_BITMAP (get_varinfo (vi->node)->solution, 0, i, bi)
    {
      fprintf (file, "%s ", get_varinfo (i)->name);
    }
  fprintf (file, "}\n");
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

  /* For each incoming argument arg, ARG = &ANYTHING */
  for (t = DECL_ARGUMENTS (current_function_decl); t; t = TREE_CHAIN (t))
    {
      struct constraint_expr lhs;
      varinfo_t p;
      
      lhs.offset = 0;
      lhs.type = SCALAR;
      lhs.var  = create_variable_info_for (t, alias_get_name (t));
      
      for (p = get_varinfo (lhs.var); p; p = p->next)
	make_constraint_to_anything (p);
    }	

}

/* Set bits in INTO corresponding to the variable uids in solution set
   FROM  */

static void
set_uids_in_ptset (bitmap into, bitmap from)
{
  unsigned int i;
  bitmap_iterator bi;
  subvar_t sv;

  EXECUTE_IF_SET_IN_BITMAP (from, 0, i, bi)
    {
      varinfo_t vi = get_varinfo (i);

      /* The only artificial variables that are allowed in a may-alias
	 set are heap variables.  */
      if (vi->is_artificial_var && !vi->is_heap_var)
	continue;
      
      if (vi->has_union && get_subvars_for_var (vi->decl) != NULL)
	{
	  /* Variables containing unions may need to be converted to
	     their SFT's, because SFT's can have unions and we cannot.  */
	  for (sv = get_subvars_for_var (vi->decl); sv; sv = sv->next)
	    bitmap_set_bit (into, DECL_UID (sv->var));
	}
      else if (TREE_CODE (vi->decl) == VAR_DECL 
	       || TREE_CODE (vi->decl) == PARM_DECL)
	{
	  if (var_can_have_subvars (vi->decl)
		   && get_subvars_for_var (vi->decl))
	    {
	      /* If VI->DECL is an aggregate for which we created
		 SFTs, add the SFT corresponding to VI->OFFSET.  */
	      tree sft = get_subvar_at (vi->decl, vi->offset);
	      if (sft)
		bitmap_set_bit (into, DECL_UID (sft));
	    }
	  else
	    {
	      /* Otherwise, just add VI->DECL to the alias set.  */
	      bitmap_set_bit (into, DECL_UID (vi->decl));
	    }
	}
    }
}


static bool have_alias_info = false;

/* Given a pointer variable P, fill in its points-to set, or return
   false if we can't.  */

bool
find_what_p_points_to (tree p)
{
  unsigned int id = 0;

  if (!have_alias_info)
    return false;

  if (lookup_id_for_tree (p, &id))
    {
      varinfo_t vi = get_varinfo (id);
      
      if (vi->is_artificial_var)
	return false;

      /* See if this is a field or a structure.  */
      if (vi->size != vi->fullsize)
	{
	  /* Nothing currently asks about structure fields directly,
	     but when they do, we need code here to hand back the
	     points-to set.  */
	  if (!var_can_have_subvars (vi->decl)
	      || get_subvars_for_var (vi->decl) == NULL)
	    return false;
	} 
      else
	{
	  struct ptr_info_def *pi = get_ptr_info (p);
	  unsigned int i;
	  bitmap_iterator bi;

	  /* This variable may have been collapsed, let's get the real
	     variable.  */
	  vi = get_varinfo (vi->node);
	  
	  /* Translate artificial variables into SSA_NAME_PTR_INFO
	     attributes.  */
	  EXECUTE_IF_SET_IN_BITMAP (vi->solution, 0, i, bi)
	    {
	      varinfo_t vi = get_varinfo (i);

	      if (vi->is_artificial_var)
		{
		  /* FIXME.  READONLY should be handled better so that
		     flow insensitive aliasing can disregard writable
		     aliases.  */
		  if (vi->id == nothing_id)
		    pi->pt_null = 1;
		  else if (vi->id == anything_id)
		    pi->pt_anything = 1;
		  else if (vi->id == readonly_id)
		    pi->pt_anything = 1;
		  else if (vi->id == integer_id)
		    pi->pt_anything = 1;
		  else if (vi->is_heap_var)
		    pi->pt_global_mem = 1;
		}
	    }

	  if (pi->pt_anything)
	    return false;

	  if (!pi->pt_vars)
	    pi->pt_vars = BITMAP_GGC_ALLOC ();

	  set_uids_in_ptset (pi->pt_vars, vi->solution);

	  if (bitmap_empty_p (pi->pt_vars))
	    pi->pt_vars = NULL;

	  return true;
	}
    }

  return false;
}



/* Dump points-to information to OUTFILE.  */

void
dump_sa_points_to_info (FILE *outfile)
{
  unsigned int i;

  fprintf (outfile, "\nPoints-to sets\n\n");

  if (dump_flags & TDF_STATS)
    {
      fprintf (outfile, "Stats:\n");
      fprintf (outfile, "Total vars:               %d\n", stats.total_vars);
      fprintf (outfile, "Statically unified vars:  %d\n",
	       stats.unified_vars_static);
      fprintf (outfile, "Collapsed vars:           %d\n", stats.collapsed_vars);
      fprintf (outfile, "Dynamically unified vars: %d\n",
	       stats.unified_vars_dynamic);
      fprintf (outfile, "Iterations:               %d\n", stats.iterations);
      fprintf (outfile, "Number of edges:          %d\n", stats.num_edges);
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

  /* Create the NULL variable, used to represent that a variable points
     to NULL.  */
  nothing_tree = create_tmp_var_raw (void_type_node, "NULL");
  var_nothing = new_var_info (nothing_tree, 0, "NULL", 0);
  insert_id_for_tree (nothing_tree, 0);
  var_nothing->is_artificial_var = 1;
  var_nothing->offset = 0;
  var_nothing->size = ~0;
  var_nothing->fullsize = ~0;
  var_nothing->is_special_var = 1;
  nothing_id = 0;
  VEC_safe_push (varinfo_t, heap, varmap, var_nothing);

  /* Create the ANYTHING variable, used to represent that a variable
     points to some unknown piece of memory.  */
  anything_tree = create_tmp_var_raw (void_type_node, "ANYTHING");
  var_anything = new_var_info (anything_tree, 1, "ANYTHING", 1); 
  insert_id_for_tree (anything_tree, 1);
  var_anything->is_artificial_var = 1;
  var_anything->size = ~0;
  var_anything->offset = 0;
  var_anything->next = NULL;
  var_anything->fullsize = ~0;
  var_anything->is_special_var = 1;
  anything_id = 1;

  /* Anything points to anything.  This makes deref constraints just
     work in the presence of linked list and other p = *p type loops, 
     by saying that *ANYTHING = ANYTHING. */
  VEC_safe_push (varinfo_t, heap, varmap, var_anything);
  lhs.type = SCALAR;
  lhs.var = anything_id;
  lhs.offset = 0;
  rhs.type = ADDRESSOF;
  rhs.var = anything_id;
  rhs.offset = 0;
  var_anything->address_taken = true;

  /* This specifically does not use process_constraint because
     process_constraint ignores all anything = anything constraints, since all
     but this one are redundant.  */
  VEC_safe_push (constraint_t, heap, constraints, new_constraint (lhs, rhs));
  
  /* Create the READONLY variable, used to represent that a variable
     points to readonly memory.  */
  readonly_tree = create_tmp_var_raw (void_type_node, "READONLY");
  var_readonly = new_var_info (readonly_tree, 2, "READONLY", 2);
  var_readonly->is_artificial_var = 1;
  var_readonly->offset = 0;
  var_readonly->size = ~0;
  var_readonly->fullsize = ~0;
  var_readonly->next = NULL;
  var_readonly->is_special_var = 1;
  insert_id_for_tree (readonly_tree, 2);
  readonly_id = 2;
  VEC_safe_push (varinfo_t, heap, varmap, var_readonly);

  /* readonly memory points to anything, in order to make deref
     easier.  In reality, it points to anything the particular
     readonly variable can point to, but we don't track this
     separately. */
  lhs.type = SCALAR;
  lhs.var = readonly_id;
  lhs.offset = 0;
  rhs.type = ADDRESSOF;
  rhs.var = anything_id;
  rhs.offset = 0;
  
  process_constraint (new_constraint (lhs, rhs));
  
  /* Create the INTEGER variable, used to represent that a variable points
     to an INTEGER.  */
  integer_tree = create_tmp_var_raw (void_type_node, "INTEGER");
  var_integer = new_var_info (integer_tree, 3, "INTEGER", 3);
  insert_id_for_tree (integer_tree, 3);
  var_integer->is_artificial_var = 1;
  var_integer->size = ~0;
  var_integer->fullsize = ~0;
  var_integer->offset = 0;
  var_integer->next = NULL;
  var_integer->is_special_var = 1;
  integer_id = 3;
  VEC_safe_push (varinfo_t, heap, varmap, var_integer);

  /* *INTEGER = ANYTHING, because we don't know where a dereference of a random
     integer will point to.  */
  lhs.type = SCALAR;
  lhs.var = integer_id;
  lhs.offset = 0;
  rhs.type = ADDRESSOF;
  rhs.var = anything_id;
  rhs.offset = 0;
  process_constraint (new_constraint (lhs, rhs));
}  

/* Return true if we actually need to solve the constraint graph in order to
   get our points-to sets.  This is false when, for example, no addresses are
   taken other than special vars, or all points-to sets with members already
   contain the anything variable and there are no predecessors for other
   sets.  */

static bool
need_to_solve (void)
{
  int i;
  varinfo_t v;
  bool found_address_taken = false;
  bool found_non_anything = false;

  for (i = 0; VEC_iterate (varinfo_t, varmap, i, v); i++)
    {
      if (v->is_special_var)
	continue;

      if (v->address_taken)
	found_address_taken = true;

      if (v->solution 
	  && !bitmap_empty_p (v->solution) 
	  && !bitmap_bit_p (v->solution, anything_id))
	found_non_anything = true;
      else if (bitmap_empty_p (v->solution)
	       && (VEC_length (constraint_edge_t, graph->preds[v->id]) != 0
		 || (graph->zero_weight_preds[v->id] && !bitmap_empty_p (graph->zero_weight_preds[v->id]))))
	found_non_anything = true;

      if (found_address_taken && found_non_anything)
	return true;
    }

  return false;
}

/* Initialize things necessary to perform PTA */

static void
init_alias_vars (void)
{
  bitmap_obstack_initialize (&ptabitmap_obstack);
  bitmap_obstack_initialize (&predbitmap_obstack);

  constraint_pool = create_alloc_pool ("Constraint pool", 
				       sizeof (struct constraint), 30);
  variable_info_pool = create_alloc_pool ("Variable info pool",
					  sizeof (struct variable_info), 30);
  constraint_edge_pool = create_alloc_pool ("Constraint edges",
					    sizeof (struct constraint_edge), 30);
  
  constraints = VEC_alloc (constraint_t, heap, 8);
  varmap = VEC_alloc (varinfo_t, heap, 8);
  id_for_tree = htab_create (10, tree_id_hash, tree_id_eq, free);
  memset (&stats, 0, sizeof (stats));

  init_base_vars ();
}


/* Create points-to sets for the current function.  See the comments
   at the start of the file for an algorithmic overview.  */

void
compute_points_to_sets (struct alias_info *ai)
{
  basic_block bb;

  timevar_push (TV_TREE_PTA);

  init_alias_vars ();

  intra_create_variable_infos ();

  /* Now walk all statements and derive aliases.  */
  FOR_EACH_BB (bb)
    {
      block_stmt_iterator bsi; 
      tree phi;

      for (phi = phi_nodes (bb); phi; phi = TREE_CHAIN (phi))
	{
	  if (is_gimple_reg (PHI_RESULT (phi)))
	    {
	      find_func_aliases (phi);
	      /* Update various related attributes like escaped
		 addresses, pointer dereferences for loads and stores.
		 This is used when creating name tags and alias
		 sets.  */
	      update_alias_info (phi, ai);
	    }
	}

      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	{
	  tree stmt = bsi_stmt (bsi);
	  find_func_aliases (stmt);
	      /* Update various related attributes like escaped
		 addresses, pointer dereferences for loads and stores.
		 This is used when creating name tags and alias
		 sets.  */
	  update_alias_info (stmt, ai);
	}
    }

  build_constraint_graph ();

  if (dump_file)
    {
      fprintf (dump_file, "Points-to analysis\n\nConstraints:\n\n");
      dump_constraints (dump_file);
    }
  
  if (need_to_solve ())
    {
      if (dump_file)
	fprintf (dump_file,
		 "\nCollapsing static cycles and doing variable "
		 "substitution:\n");
      
      find_and_collapse_graph_cycles (graph, false);
      perform_var_substitution (graph);
      
      if (dump_file)
	fprintf (dump_file, "\nSolving graph:\n");
      
      solve_graph (graph);
    }
  
  if (dump_file)
    dump_sa_points_to_info (dump_file);
  
  have_alias_info = true;

  timevar_pop (TV_TREE_PTA);
}


/* Delete created points-to sets.  */

void
delete_points_to_sets (void)
{
  varinfo_t v;
  int i;

  htab_delete (id_for_tree);
  bitmap_obstack_release (&ptabitmap_obstack);
  bitmap_obstack_release (&predbitmap_obstack);
  VEC_free (constraint_t, heap, constraints);
  
  for (i = 0; VEC_iterate (varinfo_t, varmap, i, v); i++)
    {
      VEC_free (constraint_edge_t, heap, graph->succs[i]);
      VEC_free (constraint_edge_t, heap, graph->preds[i]);
      VEC_free (constraint_t, heap, v->complex);
    }
  free (graph->zero_weight_preds);
  free (graph->zero_weight_succs);
  free (graph->succs);
  free (graph->preds);
  free (graph);

  VEC_free (varinfo_t, heap, varmap);
  free_alloc_pool (variable_info_pool);
  free_alloc_pool (constraint_pool); 
  free_alloc_pool (constraint_edge_pool);

  have_alias_info = false;
}

/* Return true if we should execute IPA PTA.  */
static bool
gate_ipa_pta (void)
{
  return (flag_unit_at_a_time != 0
	  /* Don't bother doing anything if the program has errors.  */
	  && !(errorcount || sorrycount));
}

/* Execute the driver for IPA PTA.  */
static void
ipa_pta_execute (void)
{
  struct cgraph_node *node;
  in_ipa_mode = 1;

  init_alias_vars ();
  
  for (node = cgraph_nodes; node; node = node->next)
    {
      if (!node->analyzed || cgraph_is_master_clone (node))
	{
	  unsigned int varid;
	  
	  varid = create_function_info_for (node->decl, 
					    cgraph_node_name (node));
	  if (node->local.externally_visible)
	    {
	      varinfo_t fi = get_varinfo (varid);
	      for (; fi; fi = fi->next)
		make_constraint_to_anything (fi);
	    }
	}
    }
  for (node = cgraph_nodes; node; node = node->next)
    {
      if (node->analyzed && cgraph_is_master_clone (node))
	{
	  struct function *cfun = DECL_STRUCT_FUNCTION (node->decl);
	  basic_block bb;
	  tree old_func_decl = current_function_decl;
	  if (dump_file)
	    fprintf (dump_file, 
		     "Generating constraints for %s\n", 
		     cgraph_node_name (node)); 
	  push_cfun (cfun);
	  current_function_decl = node->decl;

	  FOR_EACH_BB_FN (bb, cfun)
	    {
	      block_stmt_iterator bsi; 
	      tree phi;
	      
	      for (phi = phi_nodes (bb); phi; phi = TREE_CHAIN (phi))
		{
		  if (is_gimple_reg (PHI_RESULT (phi)))
		    {
		      find_func_aliases (phi);
		    }
		}
	      
	      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
		{
		  tree stmt = bsi_stmt (bsi);
		  find_func_aliases (stmt);
		}
	    }	
	  current_function_decl = old_func_decl;
	  pop_cfun ();	  
	}
      else
	{
	  /* Make point to anything.  */
	}
    }

  build_constraint_graph ();

  if (dump_file)
    {
      fprintf (dump_file, "Points-to analysis\n\nConstraints:\n\n");
      dump_constraints (dump_file);
    }
  
  if (need_to_solve ())
    {
      if (dump_file)
	fprintf (dump_file, 
		 "\nCollapsing static cycles and doing variable "
		 "substitution:\n");
      
      find_and_collapse_graph_cycles (graph, false);
      perform_var_substitution (graph);
      
      if (dump_file)
	fprintf (dump_file, "\nSolving graph:\n");
      
      solve_graph (graph);
    }
  
  if (dump_file)
    dump_sa_points_to_info (dump_file);
  in_ipa_mode = 0;
}
  
struct tree_opt_pass pass_ipa_pta =
{
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
  0,                                    /* todo_flags_finish */
  0					/* letter */
};

/* Initialize the heapvar for statement mapping.  */
void 
init_alias_heapvars (void)
{
  heapvar_for_stmt = htab_create_ggc (11, tree_map_hash, tree_map_eq, NULL);
}

void
delete_alias_heapvars (void)
{
  htab_delete (heapvar_for_stmt);  
}

  
#include "gt-tree-ssa-structalias.h"
