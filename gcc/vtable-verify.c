/* Copyright (C) 2013-2014 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Virtual Table Pointer Security Pass - Detect corruption of vtable pointers
   before using them for virtual method dispatches.  */

/* This file is part of the vtable security feature implementation.
   The vtable security feature is designed to detect when a virtual
   call is about to be made through an invalid vtable pointer
   (possibly due to data corruption or malicious attacks). The
   compiler finds every virtual call, and inserts a verification call
   before the virtual call.  The verification call takes the actual
   vtable pointer value in the object through which the virtual call
   is being made, and compares the vtable pointer against a set of all
   valid vtable pointers that the object could contain (this set is
   based on the declared type of the object).  If the pointer is in
   the valid set, execution is allowed to continue; otherwise the
   program is halted.

  There are several pieces needed in order to make this work: 1. For
  every virtual class in the program (i.e. a class that contains
  virtual methods), we need to build the set of all possible valid
  vtables that an object of that class could point to.  This includes
  vtables for any class(es) that inherit from the class under
  consideration.  2. For every such data set we build up, we need a
  way to find and reference the data set.  This is complicated by the
  fact that the real vtable addresses are not known until runtime,
  when the program is loaded into memory, but we need to reference the
  sets at compile time when we are inserting verification calls into
  the program.  3.  We need to find every virtual call in the program,
  and insert the verification call (with the appropriate arguments)
  before the virtual call.  4. We need some runtime library pieces:
  the code to build up the data sets at runtime; the code to actually
  perform the verification using the data sets; and some code to set
  protections on the data sets, so they themselves do not become
  hacker targets.

  To find and reference the set of valid vtable pointers for any given
  virtual class, we create a special global variable for each virtual
  class.  We refer to this as the "vtable map variable" for that
  class.  The vtable map variable has the type "void *", and is
  initialized by the compiler to NULL.  At runtime when the set of
  valid vtable pointers for a virtual class, e.g. class Foo, is built,
  the vtable map variable for class Foo is made to point to the set.
  During compile time, when the compiler is inserting verification
  calls into the program, it passes the vtable map variable for the
  appropriate class to the verification call, so that at runtime the
  verification call can find the appropriate data set.

  The actual set of valid vtable pointers for a virtual class,
  e.g. class Foo, cannot be built until runtime, when the vtables get
  loaded into memory and their addresses are known.  But the knowledge
  about which vtables belong in which class' hierarchy is only known
  at compile time.  Therefore at compile time we collect class
  hierarchy and vtable information about every virtual class, and we
  generate calls to build up the data sets at runtime.  To build the
  data sets, we call one of the functions we add to the runtime
  library, __VLTRegisterPair.  __VLTRegisterPair takes two arguments,
  a vtable map variable and the address of a vtable.  If the vtable
  map variable is currently NULL, it creates a new data set (hash
  table), makes the vtable map variable point to the new data set, and
  inserts the vtable address into the data set.  If the vtable map
  variable is not NULL, it just inserts the vtable address into the
  data set.  In order to make sure that our data sets are built before
  any verification calls happen, we create a special constructor
  initialization function for each compilation unit, give it a very
  high initialization priority, and insert all of our calls to
  __VLTRegisterPair into our special constructor initialization
  function.

  The vtable verification feature is controlled by the flag
  '-fvtable-verify='.  There are three flavors of this:
  '-fvtable-verify=std', '-fvtable-verify=preinit', and
  '-fvtable-verify=none'.  If the option '-fvtable-verfy=preinit' is
  used, then our constructor initialization function gets put into the
  preinit array.  This is necessary if there are data sets that need
  to be built very early in execution.  If the constructor
  initialization function gets put into the preinit array, the we also
  add calls to __VLTChangePermission at the beginning and end of the
  function.  The call at the beginning sets the permissions on the
  data sets and vtable map variables to read/write, and the one at the
  end makes them read-only.  If the '-fvtable-verify=std' option is
  used, the constructor initialization functions are executed at their
  normal time, and the __VLTChangePermission calls are handled
  differently (see the comments in libstdc++-v3/libsupc++/vtv_rts.cc).
  The option '-fvtable-verify=none' turns off vtable verification.

  This file contains code for the tree pass that goes through all the
  statements in each basic block, looking for virtual calls, and
  inserting a call to __VLTVerifyVtablePointer (with appropriate
  arguments) before each one.  It also contains the hash table
  functions for the data structures used for collecting the class
  hierarchy data and building/maintaining the vtable map variable data
  are defined in gcc/vtable-verify.h.  These data structures are
  shared with the code in the C++ front end that collects the class
  hierarchy & vtable information and generates the vtable map
  variables (see cp/vtable-class-hierarchy.c).  This tree pass should
  run just before the gimple is converted to RTL.

  Some implementation details for this pass:

  To find all of the virtual calls, we iterate through all the
  gimple statements in each basic block, looking for any call
  statement with the code "OBJ_TYPE_REF".  Once we have found the
  virtual call, we need to find the vtable pointer through which the
  call is being made, and the type of the object containing the
  pointer (to find the appropriate vtable map variable).  We then use
  these to build a call to __VLTVerifyVtablePointer, passing the
  vtable map variable, and the vtable pointer.  We insert the
  verification call just after the gimple statement that gets the
  vtable pointer out of the object, and we update the next
  statement to depend on the result returned from
  __VLTVerifyVtablePointer (the vtable pointer value), to ensure
  subsequent compiler phases don't remove or reorder the call (it's no
  good to have the verification occur after the virtual call, for
  example).  To find the vtable pointer being used (and the type of
  the object) we search backwards through the def_stmts chain from the
  virtual call (see verify_bb_vtables for more details).  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "tree-pass.h"
#include "cfgloop.h"

#include "vtable-verify.h"

unsigned num_vtable_map_nodes = 0;
int total_num_virtual_calls = 0;
int total_num_verified_vcalls = 0;

extern GTY(()) tree verify_vtbl_ptr_fndecl;
tree verify_vtbl_ptr_fndecl = NULL_TREE;

/* Keep track of whether or not any virtual call were verified.  */
static bool any_verification_calls_generated = false;

unsigned int vtable_verify_main (void);


/* The following few functions are for the vtbl pointer hash table
   in the 'registered' field of the struct vtable_map_node.  The hash
   table keeps track of which vtable pointers have been used in
   calls to __VLTRegisterPair with that particular vtable map variable.  */

/* This function checks to see if a particular VTABLE_DECL and OFFSET are
   already in the 'registered' hash table for NODE.  */

bool
vtbl_map_node_registration_find (struct vtbl_map_node *node,
                                 tree vtable_decl,
                                 unsigned offset)
{
  struct vtable_registration key;
  struct vtable_registration **slot;

  gcc_assert (node && node->registered.is_created ());

  key.vtable_decl = vtable_decl;
  slot = (struct vtable_registration **) node->registered.find_slot (&key,
                                                                     NO_INSERT);

  if (slot && (*slot))
    {
      unsigned i;
      for (i = 0; i < ((*slot)->offsets).length (); ++i)
        if ((*slot)->offsets[i] == offset)
          return true;
    }

  return false;
}

/* This function inserts VTABLE_DECL and OFFSET into the 'registered'
   hash table for NODE.  It returns a boolean indicating whether or not
   it actually inserted anything.  */

bool
vtbl_map_node_registration_insert (struct vtbl_map_node *node,
                                   tree vtable_decl,
                                   unsigned offset)
{
  struct vtable_registration key;
  struct vtable_registration **slot;
  bool inserted_something = false;

  if (!node || !node->registered.is_created ())
    return false;

  key.vtable_decl = vtable_decl;
  slot = (struct vtable_registration **) node->registered.find_slot (&key,
                                                                     INSERT);

  if (! *slot)
    {
      struct vtable_registration *node;
      node = XNEW (struct vtable_registration);
      node->vtable_decl = vtable_decl;

      (node->offsets).create (10);
      (node->offsets).safe_push (offset);
      *slot = node;
      inserted_something = true;
    }
  else
    {
      /* We found the vtable_decl slot; we need to see if it already
         contains the offset.  If not, we need to add the offset.  */
      unsigned i;
      bool found = false;
      for (i = 0; i < ((*slot)->offsets).length () && !found; ++i)
        if ((*slot)->offsets[i] == offset)
          found = true;

      if (!found)
        {
          ((*slot)->offsets).safe_push (offset);
          inserted_something = true;
        }
     }
  return inserted_something;
}

/* Hashtable functions for vtable_registration hashtables.  */

inline hashval_t
registration_hasher::hash (const value_type *p)
{
  const struct vtable_registration *n = (const struct vtable_registration *) p;
  return (hashval_t) (DECL_UID (n->vtable_decl));
}

inline bool
registration_hasher::equal (const value_type *p1, const compare_type *p2)
{
  const struct vtable_registration *n1 =
                                    (const struct vtable_registration *) p1;
  const struct vtable_registration *n2 =
                                    (const struct vtable_registration *) p2;
  return (DECL_UID (n1->vtable_decl) == DECL_UID (n2->vtable_decl));
}

/* End of hashtable functions for "registered" hashtables.  */



/* Hashtable definition and functions for vtbl_map_hash.  */

struct vtbl_map_hasher : typed_noop_remove <struct vtbl_map_node>
{
  typedef struct vtbl_map_node value_type;
  typedef struct vtbl_map_node compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
};

/* Returns a hash code for P.  */

inline hashval_t
vtbl_map_hasher::hash (const value_type *p)
{
  const struct vtbl_map_node n = *((const struct vtbl_map_node *) p);
  return (hashval_t) IDENTIFIER_HASH_VALUE (n.class_name);
}

/* Returns nonzero if P1 and P2 are equal.  */

inline bool
vtbl_map_hasher::equal (const value_type *p1, const compare_type *p2)
{
  const struct vtbl_map_node n1 = *((const struct vtbl_map_node *) p1);
  const struct vtbl_map_node n2 = *((const struct vtbl_map_node *) p2);
  return (IDENTIFIER_HASH_VALUE (n1.class_name) ==
          IDENTIFIER_HASH_VALUE (n2.class_name));
}

/* Here are the two structures into which we insert vtable map nodes.
   We use two data structures because of the vastly different ways we need
   to find the nodes for various tasks (see comments in vtable-verify.h
   for more details.  */

typedef hash_table <vtbl_map_hasher> vtbl_map_table_type;
typedef vtbl_map_table_type::iterator vtbl_map_iterator_type;

/* Vtable map variable nodes stored in a hash table.  */
static vtbl_map_table_type vtbl_map_hash;

/* Vtable map variable nodes stored in a vector.  */
vec<struct vtbl_map_node *> vtbl_map_nodes_vec;

/* Return vtbl_map node for CLASS_NAME  without creating a new one.  */

struct vtbl_map_node *
vtbl_map_get_node (tree class_type)
{
  struct vtbl_map_node key;
  struct vtbl_map_node **slot;

  tree class_type_decl;
  tree class_name;
  unsigned int type_quals;

  if (!vtbl_map_hash.is_created ())
    return NULL;

  gcc_assert (TREE_CODE (class_type) == RECORD_TYPE);


  /* Find the TYPE_DECL for the class.  */
  class_type_decl = TYPE_NAME (class_type);

  /* Verify that there aren't any qualifiers on the type.  */
  type_quals = TYPE_QUALS (TREE_TYPE (class_type_decl));
  gcc_assert (type_quals == TYPE_UNQUALIFIED);

  /* Get the mangled name for the unqualified type.  */
  gcc_assert (HAS_DECL_ASSEMBLER_NAME_P (class_type_decl));
  class_name = DECL_ASSEMBLER_NAME (class_type_decl);

  key.class_name = class_name;
  slot = (struct vtbl_map_node **) vtbl_map_hash.find_slot (&key,
                                                            NO_INSERT);
  if (!slot)
    return NULL;
  return *slot;
}

/* Return vtbl_map node assigned to BASE_CLASS_TYPE.  Create new one
   when needed.  */

struct vtbl_map_node *
find_or_create_vtbl_map_node (tree base_class_type)
{
  struct vtbl_map_node key;
  struct vtbl_map_node *node;
  struct vtbl_map_node **slot;
  tree class_type_decl;
  unsigned int type_quals;

  if (!vtbl_map_hash.is_created ())
    vtbl_map_hash.create (10);

  /* Find the TYPE_DECL for the class.  */
  class_type_decl = TYPE_NAME (base_class_type);

  /* Verify that there aren't any type qualifiers on type.  */
  type_quals = TYPE_QUALS (TREE_TYPE (class_type_decl));
  gcc_assert (type_quals == TYPE_UNQUALIFIED);

  gcc_assert (HAS_DECL_ASSEMBLER_NAME_P (class_type_decl));
  key.class_name = DECL_ASSEMBLER_NAME (class_type_decl);
  slot = (struct vtbl_map_node **) vtbl_map_hash.find_slot (&key,
                                                            INSERT);

  if (*slot)
    return *slot;

  node = XNEW (struct vtbl_map_node);
  node->vtbl_map_decl = NULL_TREE;
  node->class_name = key.class_name;
  node->uid = num_vtable_map_nodes++;

  node->class_info = XNEW (struct vtv_graph_node);
  node->class_info->class_type = base_class_type;
  node->class_info->class_uid = node->uid;
  node->class_info->num_processed_children = 0;

  (node->class_info->parents).create (4);
  (node->class_info->children).create (4);

  node->registered.create (16);

  node->is_used = false;

  vtbl_map_nodes_vec.safe_push (node);
  gcc_assert (vtbl_map_nodes_vec[node->uid] == node);

  *slot = node;
  return node;
}

/* End of hashtable functions for vtable_map variables hash table.   */

/* Given a gimple STMT, this function checks to see if the statement
   is an assignment, the rhs of which is getting the vtable pointer
   value out of an object.  (i.e. it's the value we need to verify
   because its the vtable pointer that will be used for a virtual
   call).  */

static bool
is_vtable_assignment_stmt (gimple stmt)
{

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return false;
  else
    {
      tree lhs = gimple_assign_lhs (stmt);
      tree rhs = gimple_assign_rhs1 (stmt);

      if (TREE_CODE (lhs) != SSA_NAME)
        return false;

      if (TREE_CODE (rhs) != COMPONENT_REF)
        return false;

      if (! (TREE_OPERAND (rhs, 1))
          || (TREE_CODE (TREE_OPERAND (rhs, 1)) != FIELD_DECL))
        return false;

      if (! DECL_VIRTUAL_P (TREE_OPERAND (rhs, 1)))
        return false;
    }

    return true;
}

/* This function attempts to recover the declared class of an object
   that is used in making a virtual call.  We try to get the type from
   the type cast in the gimple assignment statement that extracts the
   vtable pointer from the object (DEF_STMT).  The gimple statement
   usually looks something like this:

   D.2201_4 = MEM[(struct Event *)this_1(D)]._vptr.Event    */

static tree
extract_object_class_type (tree rhs)
{
  tree result = NULL_TREE;

  /* Try to find and extract the type cast from that stmt.  */
  if (TREE_CODE (rhs) == COMPONENT_REF)
    {
      tree op0 = TREE_OPERAND (rhs, 0);
      tree op1 = TREE_OPERAND (rhs, 1);

      if (TREE_CODE (op1) == FIELD_DECL
          && DECL_VIRTUAL_P (op1))
        {
          if (TREE_CODE (op0) == COMPONENT_REF
              && TREE_CODE (TREE_OPERAND (op0, 0)) == MEM_REF
              && TREE_CODE (TREE_TYPE (TREE_OPERAND (op0, 0)))== RECORD_TYPE)
            result = TREE_TYPE (TREE_OPERAND (op0, 0));
          else
            result = TREE_TYPE (op0);
        }
      else if (TREE_CODE (op0) == COMPONENT_REF)
        {
          result = extract_object_class_type (op0);
          if (result == NULL_TREE
              && TREE_CODE (op1) == COMPONENT_REF)
            result = extract_object_class_type (op1);
        }
    }

  return result;
}

/* This function traces forward through the def-use chain of an SSA
   variable to see if it ever gets used in a virtual function call.  It
   returns a boolean indicating whether or not it found a virtual call in
   the use chain.  */

static bool
var_is_used_for_virtual_call_p (tree lhs, int *mem_ref_depth)
{
  imm_use_iterator imm_iter;
  bool found_vcall = false;
  use_operand_p use_p;

  if (TREE_CODE (lhs) != SSA_NAME)
    return false;

  if (*mem_ref_depth > 2)
    return false;

  /* Iterate through the immediate uses of the current variable.  If
     it's a virtual function call, we're done.  Otherwise, if there's
     an LHS for the use stmt, add the ssa var to the work list
     (assuming it's not already in the list and is not a variable
     we've already examined.  */

  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, lhs)
    {
      gimple stmt2 = USE_STMT (use_p);

      if (is_gimple_call (stmt2))
        {
          tree fncall = gimple_call_fn (stmt2);
          if (fncall && TREE_CODE (fncall) == OBJ_TYPE_REF)
            found_vcall = true;
	  else
	    return false;
        }
      else if (gimple_code (stmt2) == GIMPLE_PHI)
        {
          found_vcall = var_is_used_for_virtual_call_p
	                                            (gimple_phi_result (stmt2),
	                                             mem_ref_depth);
        }
      else if (is_gimple_assign (stmt2))
        {
	  tree rhs = gimple_assign_rhs1 (stmt2);
	  if (TREE_CODE (rhs) == ADDR_EXPR
	      || TREE_CODE (rhs) == MEM_REF)
	    *mem_ref_depth = *mem_ref_depth + 1;
	  
	  if (TREE_CODE (rhs) == COMPONENT_REF)
	    {
	      while (TREE_CODE (TREE_OPERAND (rhs, 0)) == COMPONENT_REF)
		rhs = TREE_OPERAND (rhs, 0);

	      if (TREE_CODE (TREE_OPERAND (rhs, 0)) == ADDR_EXPR
		  || TREE_CODE (TREE_OPERAND (rhs, 0)) == MEM_REF)
		*mem_ref_depth = *mem_ref_depth + 1;
	    }

	  if (*mem_ref_depth < 3)
	    found_vcall = var_is_used_for_virtual_call_p
	                                            (gimple_assign_lhs (stmt2),
						     mem_ref_depth);
        }

      else
        break;

      if (found_vcall)
        return true;
    }

  return false;
}

/* Search through all the statements in a basic block (BB), searching
   for virtual method calls.  For each virtual method dispatch, find
   the vptr value used, and the statically declared type of the
   object; retrieve the vtable map variable for the type of the
   object; generate a call to __VLTVerifyVtablePointer; and insert the
   generated call into the basic block, after the point where the vptr
   value is gotten out of the object and before the virtual method
   dispatch. Make the virtual method dispatch depend on the return
   value from the verification call, so that subsequent optimizations
   cannot reorder the two calls.  */

static void
verify_bb_vtables (basic_block bb)
{
  gimple_seq stmts;
  gimple stmt = NULL;
  gimple_stmt_iterator gsi_vtbl_assign;
  gimple_stmt_iterator gsi_virtual_call;

  stmts = bb_seq (bb);
  gsi_virtual_call = gsi_start (stmts);
  for (; !gsi_end_p (gsi_virtual_call); gsi_next (&gsi_virtual_call))
    {
      stmt = gsi_stmt (gsi_virtual_call);

      /* Count virtual calls.  */
      if (is_gimple_call (stmt))
        {
          tree fncall = gimple_call_fn (stmt);
          if (fncall && TREE_CODE (fncall) == OBJ_TYPE_REF)
            total_num_virtual_calls++;
        }

      if (is_vtable_assignment_stmt (stmt))
        {
          tree lhs = gimple_assign_lhs (stmt);
          tree vtbl_var_decl = NULL_TREE;
          struct vtbl_map_node *vtable_map_node;
          tree vtbl_decl = NULL_TREE;
          gimple call_stmt;
          const char *vtable_name = "<unknown>";
          tree tmp0;
          bool found;
	  int mem_ref_depth = 0;

          /* Make sure this vptr field access is for a virtual call.  */
          if (!var_is_used_for_virtual_call_p (lhs, &mem_ref_depth))
            continue;

          /* Now we have found the virtual method dispatch and
             the preceding access of the _vptr.* field... Next
             we need to find the statically declared type of
             the object, so we can find and use the right
             vtable map variable in the verification call.  */
          tree class_type = extract_object_class_type
                                                   (gimple_assign_rhs1 (stmt));

          gsi_vtbl_assign = gsi_for_stmt (stmt);

          if (class_type
              && (TREE_CODE (class_type) == RECORD_TYPE)
              && TYPE_BINFO (class_type))
            {
              /* Get the vtable VAR_DECL for the type.  */
              vtbl_var_decl = BINFO_VTABLE (TYPE_BINFO (class_type));

              if (TREE_CODE (vtbl_var_decl) == POINTER_PLUS_EXPR)
                vtbl_var_decl = TREE_OPERAND (TREE_OPERAND (vtbl_var_decl, 0),
                                              0);

              gcc_assert (vtbl_var_decl);

              vtbl_decl = vtbl_var_decl;
              vtable_map_node = vtbl_map_get_node
                                               (TYPE_MAIN_VARIANT (class_type));

              gcc_assert (verify_vtbl_ptr_fndecl);

              /* Given the vtable pointer for the base class of the
                 object, build the call to __VLTVerifyVtablePointer to
                 verify that the object's vtable pointer (contained in
                 lhs) is in the set of valid vtable pointers for the
                 base class.  */

              if (vtable_map_node && vtable_map_node->vtbl_map_decl)
                {
                  vtable_map_node->is_used = true;
                  vtbl_var_decl = vtable_map_node->vtbl_map_decl;

                  if (TREE_CODE (vtbl_decl) == VAR_DECL)
                    vtable_name = IDENTIFIER_POINTER (DECL_NAME (vtbl_decl));

                  /* Call different routines if we are interested in
                     trace information to debug problems.  */
                  if (flag_vtv_debug)
                    {
                      int len1 = IDENTIFIER_LENGTH
                                                 (DECL_NAME (vtbl_var_decl));
                      int len2 = strlen (vtable_name);

                      call_stmt = gimple_build_call
                                     (verify_vtbl_ptr_fndecl, 4,
                                      build1 (ADDR_EXPR,
                                                TYPE_POINTER_TO
                                                  (TREE_TYPE (vtbl_var_decl)),
                                              vtbl_var_decl),
                                      lhs,
                                      build_string_literal
                                                  (len1 + 1,
                                                   IDENTIFIER_POINTER
                                                       (DECL_NAME
                                                            (vtbl_var_decl))),
                                      build_string_literal (len2 + 1,
                                                            vtable_name));
                    }
                  else
                    call_stmt = gimple_build_call
                                     (verify_vtbl_ptr_fndecl, 2,
                                      build1 (ADDR_EXPR,
                                                TYPE_POINTER_TO
                                                  (TREE_TYPE (vtbl_var_decl)),
                                                 vtbl_var_decl),
                                      lhs);


                  /* Create a new SSA_NAME var to hold the call's
                     return value, and make the call_stmt use the
                     variable for that purpose.  */
                  tmp0 = make_temp_ssa_name (TREE_TYPE (lhs), NULL, "VTV");
                  gimple_call_set_lhs (call_stmt, tmp0);
                  update_stmt (call_stmt);

                  /* Replace all uses of lhs with tmp0. */
                  found = false;
                  imm_use_iterator iterator;
                  gimple use_stmt;
                  FOR_EACH_IMM_USE_STMT (use_stmt, iterator, lhs)
                    {
                      use_operand_p use_p;
                      if (use_stmt == call_stmt)
                        continue;
                      FOR_EACH_IMM_USE_ON_STMT (use_p, iterator)
                        SET_USE (use_p, tmp0);
                      update_stmt (use_stmt);
                      found = true;
                    }

                  gcc_assert (found);

                  /* Insert the new verification call just after the
                     statement that gets the vtable pointer out of the
                     object.  */
                  gcc_assert (gsi_stmt (gsi_vtbl_assign) == stmt);
                  gsi_insert_after (&gsi_vtbl_assign, call_stmt,
                                    GSI_NEW_STMT);

                  any_verification_calls_generated = true;
                  total_num_verified_vcalls++;
                }
            }
        }
    }
}

/* Definition of this optimization pass.  */

namespace {

const pass_data pass_data_vtable_verify =
{
  GIMPLE_PASS, /* type */
  "vtable-verify", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_execute */
  TV_VTABLE_VERIFICATION, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_vtable_verify : public gimple_opt_pass
{
public:
  pass_vtable_verify (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_vtable_verify, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return (flag_vtable_verify); }
  virtual unsigned int execute (function *);

}; // class pass_vtable_verify

/* Loop through all the basic blocks in the current function, passing them to
   verify_bb_vtables, which searches for virtual calls, and inserts
   calls to __VLTVerifyVtablePointer.  */

unsigned int
pass_vtable_verify::execute (function *fun)
{
  unsigned int ret = 1;
  basic_block bb;

  FOR_ALL_BB_FN (bb, fun)
      verify_bb_vtables (bb);

  return ret;
}

} // anon namespace

gimple_opt_pass *
make_pass_vtable_verify (gcc::context *ctxt)
{
  return new pass_vtable_verify (ctxt);
}

#include "gt-vtable-verify.h"
