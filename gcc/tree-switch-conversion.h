/* Tree switch conversion for GNU compiler.
   Copyright (C) 2017 Free Software Foundation, Inc.

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

#ifndef TREE_SWITCH_CONVERSION_H
#define TREE_SWITCH_CONVERSION_H

namespace tree_switch_conversion {

/*
     Switch initialization conversion

The following pass changes simple initializations of scalars in a switch
statement into initializations from a static array.  Obviously, the values
must be constant and known at compile time and a default branch must be
provided.  For example, the following code:

	int a,b;

	switch (argc)
	{
	 case 1:
	 case 2:
		a_1 = 8;
		b_1 = 6;
		break;
	 case 3:
		a_2 = 9;
		b_2 = 5;
		break;
	 case 12:
		a_3 = 10;
		b_3 = 4;
		break;
	 default:
		a_4 = 16;
		b_4 = 1;
		break;
	}
	a_5 = PHI <a_1, a_2, a_3, a_4>
	b_5 = PHI <b_1, b_2, b_3, b_4>


is changed into:

	static const int = CSWTCH01[] = {6, 6, 5, 1, 1, 1, 1, 1, 1, 1, 1, 4};
	static const int = CSWTCH02[] = {8, 8, 9, 16, 16, 16, 16, 16, 16, 16,
				 16, 16, 10};

	if (((unsigned) argc) - 1 < 11)
	  {
	    a_6 = CSWTCH02[argc - 1];
	    b_6 = CSWTCH01[argc - 1];
	  }
	else
	  {
	    a_7 = 16;
	    b_7 = 1;
	  }
	a_5 = PHI <a_6, a_7>
	b_b = PHI <b_6, b_7>

There are further constraints.  Specifically, the range of values across all
case labels must not be bigger than SWITCH_CONVERSION_BRANCH_RATIO (default
eight) times the number of the actual switch branches.

This transformation was contributed by Martin Jambor, see this e-mail:
   http://gcc.gnu.org/ml/gcc-patches/2008-07/msg00011.html  */

/* The main structure of the pass.  */
struct switch_conversion
{
  /* Constructor.  */
  switch_conversion ();

  /* Destructor.  */
  ~switch_conversion ();

  /* The following function is invoked on every switch statement (the current
     one is given in SWTCH) and runs the individual phases of switch
     conversion on it one after another until one fails or the conversion
     is completed.  On success, NULL is in m_reason, otherwise points
     to a string with the reason why the conversion failed.  */
  void expand (gswitch *swtch);

  /* Collection information about SWTCH statement.  */
  void collect (gswitch *swtch);

  /* Checks whether the range given by individual case statements of the switch
     switch statement isn't too big and whether the number of branches actually
     satisfies the size of the new array.  */
  bool check_range ();

  /* Checks whether all but the final BB basic blocks are empty.  */
  bool check_all_empty_except_final ();

  /* This function checks whether all required values in phi nodes in final_bb
     are constants.  Required values are those that correspond to a basic block
     which is a part of the examined switch statement.  It returns true if the
     phi nodes are OK, otherwise false.  */
  bool check_final_bb ();

  /* The following function allocates default_values, target_{in,out}_names and
     constructors arrays.  The last one is also populated with pointers to
     vectors that will become constructors of new arrays.  */
  void create_temp_arrays ();

  /* Populate the array of default values in the order of phi nodes.
     DEFAULT_CASE is the CASE_LABEL_EXPR for the default switch branch
     if the range is non-contiguous or the default case has standard
     structure, otherwise it is the first non-default case instead.  */
  void gather_default_values (tree default_case);

  /* The following function populates the vectors in the constructors array with
     future contents of the static arrays.  The vectors are populated in the
     order of phi nodes.  */
  void build_constructors ();

  /* If all values in the constructor vector are the same, return the value.
     Otherwise return NULL_TREE.  Not supposed to be called for empty
     vectors.  */
  tree contains_same_values_p (vec<constructor_elt, va_gc> *vec);

  /* Return type which should be used for array elements, either TYPE's
     main variant or, for integral types, some smaller integral type
     that can still hold all the constants.  */
  tree array_value_type (tree type, int num);

  /* Create an appropriate array type and declaration and assemble a static
     array variable.  Also create a load statement that initializes
     the variable in question with a value from the static array.  SWTCH is
     the switch statement being converted, NUM is the index to
     arrays of constructors, default values and target SSA names
     for this particular array.  ARR_INDEX_TYPE is the type of the index
     of the new array, PHI is the phi node of the final BB that corresponds
     to the value that will be loaded from the created array.  TIDX
     is an ssa name of a temporary variable holding the index for loads from the
     new array.  */
  void build_one_array (int num, tree arr_index_type,
			gphi *phi, tree tidx);

  /* Builds and initializes static arrays initialized with values gathered from
     the switch statement.  Also creates statements that load values from
     them.  */
  void build_arrays ();

  /* Generates and appropriately inserts loads of default values at the position
     given by GSI.  Returns the last inserted statement.  */
  gassign *gen_def_assigns (gimple_stmt_iterator *gsi);

  /* Deletes the unused bbs and edges that now contain the switch statement and
     its empty branch bbs.  BBD is the now dead BB containing
     the original switch statement, FINAL is the last BB of the converted
     switch statement (in terms of succession).  */
  void prune_bbs (basic_block bbd, basic_block final, basic_block default_bb);

  /* Add values to phi nodes in final_bb for the two new edges.  E1F is the edge
     from the basic block loading values from an array and E2F from the basic
     block loading default values.  BBF is the last switch basic block (see the
     bbf description in the comment below).  */
  void fix_phi_nodes (edge e1f, edge e2f, basic_block bbf);

  /* Creates a check whether the switch expression value actually falls into the
     range given by all the cases.  If it does not, the temporaries are loaded
     with default values instead.  */
  void gen_inbound_check ();

  /* Switch statement for which switch conversion takes place.  */
  gswitch *m_switch;

  /* The expression used to decide the switch branch.  */
  tree m_index_expr;

  /* The following integer constants store the minimum and maximum value
     covered by the case labels.  */
  tree m_range_min;
  tree m_range_max;

  /* The difference between the above two numbers.  Stored here because it
     is used in all the conversion heuristics, as well as for some of the
     transformation, and it is expensive to re-compute it all the time.  */
  tree m_range_size;

  /* Basic block that contains the actual GIMPLE_SWITCH.  */
  basic_block m_switch_bb;

  /* Basic block that is the target of the default case.  */
  basic_block m_default_bb;

  /* The single successor block of all branches out of the GIMPLE_SWITCH,
     if such a block exists.  Otherwise NULL.  */
  basic_block m_final_bb;

  /* The probability of the default edge in the replaced switch.  */
  profile_probability m_default_prob;

  /* The count of the default edge in the replaced switch.  */
  profile_count m_default_count;

  /* Combined count of all other (non-default) edges in the replaced switch.  */
  profile_count m_other_count;

  /* Number of phi nodes in the final bb (that we'll be replacing).  */
  int m_phi_count;

  /* Constructors of new static arrays.  */
  vec<constructor_elt, va_gc> **m_constructors;

  /* Array of default values, in the same order as phi nodes.  */
  tree *m_default_values;

  /* Array of ssa names that are initialized with a value from a new static
     array.  */
  tree *m_target_inbound_names;

  /* Array of ssa names that are initialized with the default value if the
     switch expression is out of range.  */
  tree *m_target_outbound_names;

  /* VOP SSA_NAME.  */
  tree m_target_vop;

  /* The first load statement that loads a temporary from a new static array.
   */
  gimple *m_arr_ref_first;

  /* The last load statement that loads a temporary from a new static array.  */
  gimple *m_arr_ref_last;

  /* String reason why the case wasn't a good candidate that is written to the
     dump file, if there is one.  */
  const char *m_reason;

  /* True if default case is not used for any value between range_min and
     range_max inclusive.  */
  bool m_contiguous_range;

  /* True if default case does not have the required shape for other case
     labels.  */
  bool m_default_case_nonstandard;

  /* Count is number of non-default edges.  */
  unsigned int m_count;

  /* True if CFG has been changed.  */
  bool m_cfg_altered;
};

} // tree_switch_conversion namespace

#endif // TREE_SWITCH_CONVERSION_H
