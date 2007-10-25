// -*- C++ -*-

// Copyright (C) 2007 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA.

// As a special exception, you may use this file as part of a free
// software library without restriction.  Specifically, if other files
// instantiate templates or use macros or inline functions from this
// file, or you compile this file and link it with other files to
// produce an executable, this file does not by itself cause the
// resulting executable to be covered by the GNU General Public
// License.  This exception does not however invalidate any other
// reasons why the executable file might be covered by the GNU General
// Public License.

/** @file parallel/tree.h
 *  @brief Parallel red-black tree operations.
 *
 *  This implementation is described in
 *
 *  Leonor Frias, Johannes Singler.
 *  Parallelization of Bulk Operations for STL Dictionaries.
 *  Workshop on Highly Parallel Processing on a Chip (HPPC) 2007.
 *
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Leonor Frias Moya, Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_TREE_H
#define _GLIBCXX_PARALLEL_TREE_H 1

#include <parallel/parallel.h>
#include <functional>
#include <cmath>
#include <algorithm>
#include <iterator>
#include <functional>
#include <iostream>
//#include <ext/malloc_allocator.h>
#include <bits/stl_tree.h>

#include <parallel/list_partition.h>

namespace std
{
  // XXX Declaration should go to stl_tree.h.
  void
  _Rb_tree_rotate_left(_Rb_tree_node_base* const __x,
                       _Rb_tree_node_base*& __root);

  void
  _Rb_tree_rotate_right(_Rb_tree_node_base* const __x,
                        _Rb_tree_node_base*& __root);
}


namespace __gnu_parallel
{
  // XXX move into parallel/type_traits.h if <type_traits> doesn't work.
  /** @brief Helper class: remove the const modifier from the first
      component, if present. Set kind component.
   *  @param T Simple type, nothing to unconst */
  template<typename T>
  struct unconst_first_component
  {
    /** @brief New type after removing the const */
    typedef T type;
  };

  /** @brief Helper class: remove the const modifier from the first
      component, if present. Map kind component
   *  @param Key First component, from which to remove the const modifier
   *  @param Load Second component
   *  @sa unconst_first_component */
  template<typename Key, typename Load>
  struct unconst_first_component<std::pair<const Key, Load> >
  {
    /** @brief New type after removing the const */
    typedef std::pair<Key, Load> type;
  };

  /** @brief Helper class: set the appropriate comparator to deal with
   * repetitions. Comparator for unique dictionaries.
   *
   *  StrictlyLess and LessEqual are part of a mechanism to deal with
   *  repetitions transparently whatever the actual policy is.
   *  @param _Key Keys to compare
   *  @param _Compare Comparator equal to conceptual < */
  template<typename _Key, typename _Compare>
  struct StrictlyLess : public std::binary_function<_Key, _Key, bool>
  {
    /** @brief Comparator equal to conceptual < */
    _Compare c;

    /** @brief Constructor given a Comparator */
    StrictlyLess(const _Compare& _c) : c(_c) { }

    /** @brief Copy constructor */
    StrictlyLess(const StrictlyLess<_Key, _Compare>& strictly_less)
    : c(strictly_less.c) { }

    /** @brief Operator() */
    bool operator()(const _Key& k1, const _Key& k2) const
    {
      return c(k1, k2);
    }
  };

  /** @brief Helper class: set the appropriate comparator to deal with
   * repetitions. Comparator for non-unique dictionaries.
   *
   *  StrictlyLess and LessEqual are part of a mechanism to deal with
   *  repetitions transparently whatever the actual policy is.
   *  @param _Key Keys to compare
   *  @param _Compare Comparator equal to conceptual <= */
  template<typename _Key, typename _Compare>
  struct LessEqual : public std::binary_function<_Key, _Key, bool>
  {
    /** @brief Comparator equal to conceptual < */
    _Compare c;

    /** @brief Constructor given a Comparator */
    LessEqual(const _Compare& _c) : c(_c) { }

    /** @brief Copy constructor */
    LessEqual(const LessEqual<_Key, _Compare>& less_equal)
    : c(less_equal.c) { }

    /** @brief Operator() */
    bool operator()(const _Key& k1, const _Key& k2) const
    { return !c(k2, k1); }
  };


  /** @brief Parallel red-black tree.
   *
   *  Extension of the sequential red-black tree. Specifically,
   *  parallel bulk insertion operations are provided.
   *  @param _Key Keys to compare
   *  @param _Val Elements to store in the tree
   *  @param _KeyOfValue Obtains the key from an element <
   *  @param _Compare Comparator equal to conceptual <
   *  @param _Alloc Allocator for the elements */
  template<typename _Key, typename _Val, typename _KeyOfValue,
	   typename _Compare, typename _Alloc = std::allocator<_Val> >
  class _Rb_tree 
  : public std::_Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>
  {
  private:
    /** @brief Sequential tree */
    typedef std::_Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc> base_type;

    /** @brief Renaming of base node type */
    typedef typename std::_Rb_tree_node<_Val> _Rb_tree_node;

    /** @brief Renaming of libstdc++ node type */
    typedef typename std::_Rb_tree_node_base _Rb_tree_node_base;

    /** @brief Renaming of base key_type */
    typedef typename base_type::key_type key_type;

    /** @brief Renaming of base value_type */
    typedef typename base_type::value_type value_type;

    /** @brief Helper class to unconst the first component of
     * value_type if exists.
     *
     * This helper class is needed for map, but may discard qualifiers
     * for set; however, a set with a const element type is not useful
     * and should fail in some other place anyway.
     */
    typedef typename unconst_first_component<value_type>::type nc_value_type;

    /** @brief Pointer to a node */
    typedef _Rb_tree_node* _Rb_tree_node_ptr;

    /** @brief Wrapper comparator class to deal with repetitions
	transparently according to dictionary type with key _Key and
	comparator _Compare. Unique dictionaries object
    */
    StrictlyLess<_Key, _Compare> strictly_less;

    /** @brief Wrapper comparator class to deal with repetitions
	transparently according to dictionary type with key _Key and
	comparator _Compare. Non-unique dictionaries object
    */
    LessEqual<_Key, _Compare> less_equal;

  public:
    /** @brief Renaming of base size_type */
    typedef typename base_type::size_type size_type;

    /** @brief Constructor with a given comparator and allocator.
     *
     * Delegates the basic initialization to the sequential class and
     * initializes the helper comparators of the parallel class
     * @param c Comparator object with which to initialize the class
     * comparator and the helper comparators
     * @param a Allocator object with which to initialize the class comparator
     */
    _Rb_tree(const _Compare& c, const _Alloc& a)
    : base_type(c, a), strictly_less(base_type::_M_impl._M_key_compare), 
      less_equal(base_type::_M_impl._M_key_compare)
    { }

    /** @brief Copy constructor.
     *
     * Delegates the basic initialization to the sequential class and
     * initializes the helper comparators of the parallel class
     * @param __x Parallel red-black instance to copy
     */
    _Rb_tree(const _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>& __x)
    : base_type(__x), strictly_less(base_type::_M_impl._M_key_compare), 
      less_equal(base_type::_M_impl._M_key_compare)
    { }

    /** @brief Parallel replacement of the sequential
     * std::_Rb_tree::_M_insert_unique()
     *
     * Parallel bulk insertion and construction. If the container is
     * empty, bulk construction is performed. Otherwise, bulk
     * insertion is performed
     * @param __first First element of the input
     * @param __last Last element of the input
     */
    template<typename _InputIterator>
    void
    _M_insert_unique(_InputIterator __first, _InputIterator __last)
    {
      if (__first==__last) return;
      if (_GLIBCXX_PARALLEL_CONDITION(true))
	if (base_type::_M_impl._M_node_count == 0)
	  {
	    _M_bulk_insertion_construction(__first, __last, true, 
					   strictly_less);
	    _GLIBCXX_PARALLEL_ASSERT(rb_verify());
	  }
	else
	  {
	    _M_bulk_insertion_construction(__first, __last, false, 
					   strictly_less);
	    _GLIBCXX_PARALLEL_ASSERT(rb_verify());
	  }
      else
	{
	  base_type::_M_insert_unique(__first, __last);
	}
    }

    /** @brief Parallel replacement of the sequential
     * std::_Rb_tree::_M_insert_equal()
     *
     * Parallel bulk insertion and construction. If the container is
     * empty, bulk construction is performed. Otherwise, bulk
     * insertion is performed
     * @param __first First element of the input
     * @param __last Last element of the input 	*/
    template<typename _InputIterator>
    void
    _M_insert_equal(_InputIterator __first, _InputIterator __last)
    {
      if (__first==__last) return;
      if (_GLIBCXX_PARALLEL_CONDITION(true))
	if (base_type::_M_impl._M_node_count == 0)
	  _M_bulk_insertion_construction(__first, __last, true, less_equal);
	else
	  _M_bulk_insertion_construction(__first, __last, false, less_equal);
      else
	base_type::_M_insert_equal(__first, __last);
      _GLIBCXX_PARALLEL_ASSERT(rb_verify());
    }

  private:

    /** @brief Helper class of _Rb_tree: node linking.
     *
     * Nodes linking forming an almost complete tree. The last level
     * is coloured red, the rest are black
     * @param ranker Calculates the position of a node in an array of nodes
     */
    template<typename ranker>
    class nodes_initializer
    {
      /** @brief Renaming of tree size_type */
      
      typedef _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc> tree_type;
      typedef typename tree_type::size_type size_type;
    public:

      /** @brief mask[%i]= 0..01..1, where the number of 1s is %i+1 */
      size_type mask[sizeof(size_type)*8];

      /** @brief Array of nodes (initial address)	 */
      const _Rb_tree_node_ptr* r_init;

      /** @brief Total number of (used) nodes */
      size_type n;

      /** @brief Rank of the last tree node that can be calculated
	  taking into account a complete tree
      */
      size_type splitting_point;

      /** @brief Rank of the tree root */
      size_type rank_root;

      /** @brief Height of the tree */
      int height;

      /** @brief Number of threads into which divide the work */
      const thread_index_t num_threads;

      /** @brief Helper object to mind potential gaps in r_init */
      const ranker& rank;

      /** @brief Constructor
       * @param r Array of nodes
       * @param _n Total number of (used) nodes
       * @param _num_threads Number of threads into which divide the work
       * @param _rank Helper object to mind potential gaps in @c r_init */
      nodes_initializer(const _Rb_tree_node_ptr* r, const size_type _n, 
			const thread_index_t _num_threads, const ranker& _rank):
	r_init(r),
	n(_n),
	num_threads(_num_threads),
	rank(_rank)
      {
	height = log2(n);
	splitting_point = 2 * (n - ((1 << height) - 1)) -1;

	// Rank root.
	size_type max = 1 << (height + 1);
	rank_root= (max-2) >> 1;
	if (rank_root > splitting_point)
	  rank_root = complete_to_original(rank_root);

	mask[0] = 0x1;
	for (unsigned int i = 1; i < sizeof(size_type)*8; ++i)
	  {
	    mask[i] = (mask[i-1] << 1) + 1;
	  }
      }

      /** @brief Query for tree height
       * @return Tree height */
      int 
      get_height() const
      { return height; }

      /** @brief Query for the splitting point
       * @return Splitting point */
      size_type 
      get_shifted_splitting_point() const
      { return rank.get_shifted_rank(splitting_point, 0); }

      /** @brief Query for the tree root node
       * @return Tree root node */
      _Rb_tree_node_ptr 
      get_root() const
      { return  r_init[rank.get_shifted_rank(rank_root,num_threads/2)]; }

      /** @brief Calculation of the parent position in the array of nodes
       * @hideinitializer */
#define CALCULATE_PARENT						\
      if (p_s> splitting_point)						\
	p_s = complete_to_original(p_s);				\
	    int s_r = rank.get_shifted_rank(p_s,iam);			\
	    r->_M_parent = r_init[s_r];					\
									\
      /** @brief Link a node with its parent and children taking into
	  account that its rank (without gaps) is different to that in
	  a complete tree
       * @param r Pointer to the node
       * @param iam Partition of the array in which the node is, where
       * iam is in [0..num_threads)
       * @sa link_complete */
      void 
      link_incomplete(const _Rb_tree_node_ptr& r, const int iam) const
      {
	size_type real_pos = rank.get_real_rank(&r-r_init, iam);
	size_type l_s, r_s, p_s;
	int mod_pos= original_to_complete(real_pos);
	int zero= first_0_right(mod_pos);

	// 1. Convert n to n', where n' will be its rank if the tree
	//    was complete
	// 2. Calculate neighbours for n'
	// 3. Convert the neighbors n1', n2' and n3' to their
	//    appropriate values n1, n2, n3. Note that it must be
	//    checked that these neighbors actually exist.
	calculate_shifts_pos_level(mod_pos, zero, l_s, r_s, p_s);
	if (l_s > splitting_point)
	  {
	    _GLIBCXX_PARALLEL_ASSERT(r_s > splitting_point);
	    if (zero == 1)
	      {
		r->_M_left = 0;
		r->_M_right = 0;
	      }
	    else
	      {
		r->_M_left= r_init[rank.get_shifted_rank(complete_to_original(l_s),iam)];
		r->_M_right= r_init[rank.get_shifted_rank(complete_to_original(r_s),iam)];
	      }

	  }
	else{
	  r->_M_left= r_init[rank.get_shifted_rank(l_s,iam)];
	  if (zero != 1)
	    {
	      r->_M_right= r_init[rank.get_shifted_rank(complete_to_original(r_s),iam)];
	    }
	  else
	    {
	      r->_M_right = 0;
	    }
	}
	r->_M_color = std::_S_black;
	CALCULATE_PARENT;
      }

      /** @brief Link a node with its parent and children taking into
	  account that its rank (without gaps) is the same as that in
	  a complete tree
       * @param r Pointer to the node
       * @param iam Partition of the array in which the node is, where
       * iam is in [0..@c num_threads)
       * @sa link_incomplete
       */
      void 
      link_complete(const _Rb_tree_node_ptr& r, const int iam) const
      {
	size_type real_pos = rank.get_real_rank(&r-r_init, iam);
	size_type p_s;

	// Test if it is a leaf on the last not necessarily full level
	if ((real_pos & mask[0]) == 0)
	  {
	    if ((real_pos & 0x2) == 0)
	      p_s = real_pos + 1;
	    else
	      p_s = real_pos - 1;
	    r->_M_color = std::_S_red;
	    r->_M_left = 0;
	    r->_M_right = 0;
	  }
	else
	  {
	    size_type l_s, r_s;
	    int zero = first_0_right(real_pos);
	    calculate_shifts_pos_level(real_pos, zero, l_s, r_s, p_s);
	    r->_M_color = std::_S_black;

	    r->_M_left = r_init[rank.get_shifted_rank(l_s,iam)];
	    if (r_s > splitting_point)
	      r_s = complete_to_original(r_s);
	    r->_M_right = r_init[rank.get_shifted_rank(r_s,iam)];
	  }
	CALCULATE_PARENT;
      }

#undef CALCULATE_PARENT

    private:
      /** @brief Change of "base": Convert the rank in the actual tree
	  into the corresponding rank if the tree was complete
       * @param pos Rank in the actual incomplete tree
       * @return Rank in the corresponding complete tree
       * @sa complete_to_original  */
      int 
      original_to_complete(const int pos) const
      { return (pos << 1) - splitting_point; }

      /** @brief Change of "base": Convert the rank if the tree was
	  complete into the corresponding rank in the actual tree
       * @param pos Rank in the complete tree
       * @return Rank in the actual incomplete tree
       * @sa original_to_complete */
      int 
      complete_to_original(const int pos) const
      { return (pos + splitting_point) >> 1; }


      /** @brief Calculate the rank in the complete tree of the parent
	  and children of a node
       * @param pos Rank in the complete tree of the node whose parent
       * and children rank must be calculated
       * @param level Tree level in which the node at pos is in
       * (starting to count at leaves). @pre @c level > 1
       * @param left_shift Rank in the complete tree of the left child
       * of pos (out parameter)
       * @param right_shift Rank in the complete tree of the right
       * child of pos (out parameter)
       * @param parent_shift Rank in the complete tree of the parent
       * of pos (out parameter)
       */
      void 
      calculate_shifts_pos_level(const size_type pos, const int level, 
				 size_type& left_shift, size_type& right_shift,
				 size_type& parent_shift) const
      {
	int stride =  1 << (level -1);
	left_shift = pos - stride;
	right_shift = pos + stride;
	if (((pos >> (level + 1)) & 0x1) == 0)
	  parent_shift = pos + 2*stride;
	else
	  parent_shift = pos - 2*stride;
      }

      /** @brief Search for the first 0 bit (growing the weight)
       * @param x Binary number (corresponding to a rank in the tree)
       * whose first 0 bit must be calculated
       * @return Position of the first 0 bit in @c x (starting to
       * count with 1)
       */
      int 
      first_0_right(const size_type x) const
      {
	if ((x & 0x2) == 0)
	  return 1;
	else
	  return first_0_right_bs(x);
      }

      /** @brief Search for the first 0 bit (growing the weight) using
       * binary search
       *
       * Binary search can be used instead of a naive loop using the
       * masks in mask array
       * @param x Binary number (corresponding to a rank in the tree)
       * whose first 0 bit must be calculated
       * @param k_beg Position in which to start searching. By default is 2.
       * @return Position of the first 0 bit in x (starting to count with 1) */
      int 
      first_0_right_bs(const size_type x, int k_beg=2) const
      {
	int k_end = sizeof(size_type)*8;
	size_type not_x = x ^ mask[k_end-1];
	while ((k_end-k_beg) > 1)
	  {
	    int k = k_beg + (k_end-k_beg)/2;
	    if ((not_x & mask[k-1]) != 0)
	      k_end = k;
	    else
	      k_beg = k;
	  }
	return k_beg;
      }
    };

    /***** Dealing with repetitions (EFFICIENCY ISSUE) *****/
    /** @brief Helper class of nodes_initializer: mind the gaps of an
	array of nodes.
     *
     * Get absolute positions in an array of nodes taking into account
     * the gaps in it @sa ranker_no_gaps
     */
    class ranker_gaps
    {
      /** @brief Renaming of tree's size_type */
      typedef _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc> tree_type;
      typedef typename tree_type::size_type size_type;

      /** @brief Array containing the beginning ranks of all the
	  num_threads partitions just considering the valid nodes, not
	  the gaps */
      size_type* beg_partition;

      /** @brief Array containing the beginning ranks of all the
	  num_threads partitions considering the valid nodes and the
	  gaps */
      const size_type* beg_shift_partition;

      /** @brief Array containing the number of accumulated gaps at
	  the beginning of each partition */
      const size_type* rank_shift;

      /** @brief Number of partitions (and threads that work on it) */
      const thread_index_t num_threads;

    public:
      /** @brief Constructor
       * @param size_p Pointer to the array containing the beginning
       * ranks of all the @c _num_threads partitions considering the
       * valid nodes and the gaps
       * @param shift_r Array containing the number of accumulated
       * gaps at the beginning of each partition
       * @param _num_threads Number of partitions (and threads that
       * work on it) */
      ranker_gaps(const size_type* size_p, const size_type* shift_r, 
		  const thread_index_t _num_threads) :
	beg_shift_partition(size_p),
	rank_shift(shift_r),
	num_threads(_num_threads)
      {
	beg_partition = new size_type[num_threads+1];
	beg_partition[0] = 0;
	for (int i = 1; i <= num_threads; ++i)
	  {
	    beg_partition[i] = beg_partition[i-1] + (beg_shift_partition[i] - beg_shift_partition[i-1]) - (rank_shift[i] - rank_shift[i-1]);

	  }

	// Ghost element, strictly larger than any index requested.
	++beg_partition[num_threads];
      }

      /** @brief Destructor
       * Needs to be defined to deallocate the dynamic memory that has
       * been allocated for beg_partition array
       */
      ~ranker_gaps()
      { delete[] beg_partition; }

      /** @brief Convert a rank in the array of nodes considering
	  valid nodes and gaps, to the corresponding considering only
	  the valid nodes
       * @param pos Rank in the array of nodes considering valid nodes and gaps
       * @param index Partition which the rank belongs to
       * @return Rank in the array of nodes considering only the valid nodes
       * @sa get_shifted_rank
       */
      size_type 
      get_real_rank(const size_type pos, const int index) const
      { return pos - rank_shift[index]; }

      /** @brief Inverse of get_real_rank: Convert a rank in the array
	  of nodes considering only valid nodes, to the corresponding
	  considering valid nodes and gaps
       * @param pos Rank in the array of nodes considering only valid nodes
       * @param index Partition which the rank is most likely to
       * belong to (i. e. the corresponding if there were no gaps)
       * @pre 0 <= @c pos <= number_of_distinct_elements
       * @return Rank in the array of nodes considering valid nodes and gaps
       * @post 0 <= @c return <= number_of_elements
       * @sa get_real_rank()
       */
      size_type 
      get_shifted_rank(const size_type pos, const int index) const
      {
	// Heuristic.
	if (beg_partition[index] <= pos and pos < beg_partition[index+1])
	  return pos + rank_shift[index];
	else
	  // Called rarely, do not hinder inlining.
	  return get_shifted_rank_loop(pos,index);
      }

      /** @brief Helper method of get_shifted_rank: in case the given
	  index in get_shifted_rank is not correct, look for it and
	  then calculate the rank
       * @param pos Rank in the array of nodes considering only valid nodes
       * @param index Partition which the rank should have belong to
       * if there were no gaps
       * @return Rank in the array of nodes considering valid nodes and gaps
       */
      size_type 
      get_shifted_rank_loop(const size_type pos, int index) const
      {
	while (pos >= beg_partition[index+1])
	  ++index;
	while (pos < beg_partition[index])
	  --index;
	_GLIBCXX_PARALLEL_ASSERT(0 <= index && index < num_threads);
	return pos + rank_shift[index];
      }
    };

    /** @brief Helper class of nodes_initializer: access an array of
     * nodes with no gaps
     *
     * Get absolute positions in an array of nodes taking into account
     * that there are no gaps in it.  @sa ranker_gaps */
    class ranker_no_gaps
    {
      /** @brief Renaming of tree's size_type */
      typedef _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc> tree_type;
      typedef typename tree_type::size_type size_type;

    public:
      /** @brief Convert a rank in the array of nodes considering
       * valid nodes and gaps, to the corresponding considering only
       * the valid nodes
       *
       * As there are no gaps in this case, get_shifted_rank() and
       * get_real_rank() are synonyms and make no change on pos
       * @param pos Rank in the array of nodes considering valid nodes and gaps
       * @param index Partition which the rank belongs to, unused here
       * @return Rank in the array of nodes considering only the valid nodes */
      size_type 
      get_real_rank(const size_type pos, const int index) const
      { return pos; }

      /** @brief Inverse of get_real_rank: Convert a rank in the array
       * of nodes considering only valid nodes, to the corresponding
       * considering valid nodes and gaps
       *
       * As there are no gaps in this case, get_shifted_rank() and
       * get_real_rank() are synonyms and make no change on pos
       * @param pos Rank in the array of nodes considering only valid nodes
       * @param index Partition which the rank belongs to, unused here
       * @return Rank in the array of nodes considering valid nodes and gaps
       */
      size_type 
      get_shifted_rank(const size_type pos, const int index) const
      { return pos; }
    };


    /** @brief Helper comparator class: Invert a binary comparator
     * @param _Comp Comparator to invert
     * @param _Iterator Iterator to the elements to compare */
    template<typename _Comp, typename _Iterator>
    class gr_or_eq
    {
      /** @brief Renaming value_type of _Iterator */
      typedef typename std::iterator_traits<_Iterator>::value_type value_type;

      /** @brief Comparator to be inverted */
      const _Comp comp;

    public:
      /** @brief Constructor
       * @param c Comparator */
      gr_or_eq(const _Comp& c) : comp(c) { }

      /** @brief Operator()
       * @param a First value to compare
       * @param b Second value to compare */
      bool operator()(const value_type& a, const value_type& b) const
      {
	if (not (comp(_KeyOfValue()(a), _KeyOfValue()(b))))
	  return true;
	return false;
      }
    };

    /** @brief Helper comparator class: Passed as a parameter of
	list_partition to check that a sequence is sorted
     * @param _InputIterator Iterator to the elements to compare
     * @param _CompIsSorted  Comparator to check for sortednesss */
    template<typename _InputIterator, typename _CompIsSorted>
    class is_sorted_functor
    {
      /** @brief Element to compare with (first parameter of comp) */
      _InputIterator prev;

      /** @brief Comparator to check for sortednesss */
      const _CompIsSorted comp;

      /** @brief Sum up the history of the operator() of this
       * comparator class Its value is true if all calls to comp from
       * this class have returned true. It is false otherwise */
      bool sorted;

    public:
      /** @brief Constructor
       *
       * Sorted is set to true
       * @param first Element to compare with the first time the
       * operator() is called
       * @param c  Comparator to check for sortedness */
      is_sorted_functor(const _InputIterator first, const _CompIsSorted c)
      : prev(first), comp(c), sorted(true) { }

      /** @brief Operator() with only one explicit parameter. Updates
	  the class member @c prev and sorted.
       * @param it Iterator to the element which must be compared to
       * the element pointed by the the class member @c prev */
      void operator()(const _InputIterator it)
      {
	if (sorted and it != prev and comp(_KeyOfValue()(*it),
					   _KeyOfValue()(*prev)))
	  sorted = false;
	prev = it;
      }

      /** @brief Query method for sorted
       * @return Current value of sorted */
      bool is_sorted() const
      {
	return sorted;
      }
    };

    /** @brief Helper functor: sort the input based upon elements
	instead of keys
     * @param KeyComparator Comparator for the key of values */
    template<typename KeyComparator>
    class ValueCompare
    : public std::binary_function<value_type, value_type, bool>
    {
      /** @brief Comparator for the key of values */
      const KeyComparator comp;

    public:
      /** @brief Constructor
       * @param c Comparator for the key of values */
      ValueCompare(const KeyComparator& c): comp(c)  { }

      /** @brief Operator(): Analogous to comp but for values and not keys
       * @param v1 First value to compare
       * @param v2 Second value to compare
       * @return Result of the comparison */
      bool operator()(const value_type& v1, const value_type& v2) const
      { return comp(_KeyOfValue()(v1),_KeyOfValue()(v2)); }
    };

    /** @brief Helper comparator: compare a key with the key in a node
     * @param _Comparator Comparator for keys */
    template<typename _Comparator>
    struct compare_node_key
    {
      /** @brief Comparator for keys */
      const _Comparator& c;

      /** @brief Constructor
       * @param _c Comparator for keys */
      compare_node_key(const _Comparator& _c) : c(_c) { }

      /** @brief Operator() with the first parameter being a node
       * @param r Node whose key is to be compared
       * @param k Key to be compared
       * @return Result of the comparison */
      bool operator()(const _Rb_tree_node_ptr r, const key_type& k) const
      { return c(base_type::_S_key(r),k); }

      /** @brief Operator() with the second parameter being a node
       * @param k Key to be compared
       * @param r Node whose key is to be compared
       * @return Result of the comparison */
      bool operator()(const key_type& k, const _Rb_tree_node_ptr r) const
      { return c(k, base_type::_S_key(r)); }
    };

    /** @brief Helper comparator: compare a key with the key of a
	value pointed by an iterator
     * @param _Comparator Comparator for keys 
     */
    template<typename _Iterator, typename _Comparator>
    struct compare_value_key
    {
      /** @brief Comparator for keys */
      const _Comparator& c;

      /** @brief Constructor
       * @param _c Comparator for keys */
      compare_value_key(const _Comparator& _c) : c(_c){ }

      /** @brief Operator() with the first parameter being an iterator
       * @param v Iterator to the value whose key is to be compared
       * @param k Key to be compared
       * @return Result of the comparison */
      bool operator()(const _Iterator& v, const key_type& k) const
      { return c(_KeyOfValue()(*v),k); }

      /** @brief Operator() with the second parameter being an iterator
       * @param k Key to be compared
       * @param v Iterator to the value whose key is to be compared
       * @return Result of the comparison */
      bool operator()(const key_type& k, const _Iterator& v) const
      { return c(k, _KeyOfValue()(*v)); }
    };

    /** @brief Helper class of _Rb_tree to avoid some symmetric code
	in tree operations */
    struct LeftRight
    {
      /** @brief Obtain the conceptual left child of a node
       * @param parent Node whose child must be obtained
       * @return Reference to the child node */
      static _Rb_tree_node_base*& left(_Rb_tree_node_base* parent)
      { return parent->_M_left; }

      /** @brief Obtain the conceptual right child of a node
       * @param parent Node whose child must be obtained
       * @return Reference to the child node */
      static _Rb_tree_node_base*& right(_Rb_tree_node_base* parent)
      { return parent->_M_right; }
    };

    /** @brief Helper class of _Rb_tree to avoid some symmetric code
	in tree operations: inverse the symmetry
     * @param S Symmetry to inverse
     * @sa LeftRight */
    template<typename S>
    struct Opposite
    {
      /** @brief Obtain the conceptual left child of a node, inverting
	  the symmetry
       * @param parent Node whose child must be obtained
       * @return Reference to the child node */
      static _Rb_tree_node_base*& left(_Rb_tree_node_base* parent)
      { return S::right(parent);}

      /** @brief Obtain the conceptual right child of a node,
	  inverting the symmetry
       * @param parent Node whose child must be obtained
       * @return Reference to the child node */
      static _Rb_tree_node_base*& right(_Rb_tree_node_base* parent)
      { return S::left(parent);}
    };

    /** @brief Inverse symmetry of LeftRight */
    typedef Opposite<LeftRight> RightLeft;

    /** @brief Helper comparator to compare value pointers, so that
	the value is taken
     * @param Comparator Comparator for values
     * @param _ValuePtr Pointer to values */
    template<typename Comparator, typename _ValuePtr>
    class PtrComparator 
    : public std::binary_function<_ValuePtr, _ValuePtr, bool>
    {
      /** @brief Comparator for values */
      Comparator comp;

    public:
      /** @brief Constructor
       * @param comp Comparator for values */
      PtrComparator(Comparator comp) : comp(comp)  { }

      /** @brief Operator(): compare the values instead of the pointers
       * @param v1 Pointer to the first element to compare
       * @param v2 Pointer to the second element to compare */
      bool operator()(const _ValuePtr& v1, const _ValuePtr& v2) const
      { return comp(*v1,*v2); }
    };

    /** @brief Iterator whose elements are pointers
     * @param value_type Type pointed by the pointers */
    template<typename _ValueTp>
    class PtrIterator
    {
    public:
      /** @brief The iterator category is random access iterator */
      typedef typename std::random_access_iterator_tag iterator_category;
      typedef _ValueTp  value_type;
      typedef size_t difference_type;
      typedef value_type* ValuePtr;
      typedef ValuePtr& reference;
      typedef value_type** pointer;

      /** @brief Element accessed by the iterator */
      value_type** ptr;

      /** @brief Trivial constructor */
      PtrIterator() { }

      /** @brief Constructor from an element */
      PtrIterator(const ValuePtr& __i) : ptr(&__i) { }

      /** @brief Constructor from a pointer */
      PtrIterator(const pointer& __i) : ptr(__i) { }

      /** @brief Copy constructor */
      PtrIterator(const PtrIterator<value_type>& __i) : ptr(__i.ptr) { }

      reference
      operator*() const
      { return **ptr; }

      ValuePtr
      operator->() const
      { return *ptr; }

      /** @brief Bidirectional iterator requirement */
      PtrIterator&
      operator++()
      {
	++ptr;
	return *this;
      }

      /** @brief Bidirectional iterator requirement */
      PtrIterator
      operator++(int)
      { return PtrIterator(ptr++); }

      /** @brief Bidirectional iterator requirement */
      PtrIterator&
      operator--()
      {
	--ptr;
	return *this;
      }

      /** @brief Bidirectional iterator requirement */
      PtrIterator
      operator--(int)
      { return PtrIterator(ptr--); }

      /** @brief Random access iterator requirement */
      reference
      operator[](const difference_type& __n) const
      { return *ptr[__n]; }

      /** @brief Random access iterator requirement */
      PtrIterator&
      operator+=(const difference_type& __n)
      {
	ptr += __n;
	return *this;
      }

      /** @brief Random access iterator requirement */
      PtrIterator
      operator+(const difference_type& __n) const
      { return PtrIterator(ptr + __n); }

      /** @brief Random access iterator requirement */
      PtrIterator&
      operator-=(const difference_type& __n)
      {
	ptr -= __n;
	return *this;
      }

      /** @brief Random access iterator requirement */
      PtrIterator
      operator-(const difference_type& __n) const
      { return PtrIterator(ptr - __n); }

      /** @brief Random access iterator requirement */
      difference_type
      operator-(const PtrIterator<value_type>& iter) const
      { return ptr - iter.ptr; }

      /** @brief Random access iterator requirement */
      difference_type
      operator+(const PtrIterator<value_type>& iter) const
      { return ptr + iter.ptr; }

      /** @brief Allow assignment of an element ValuePtr to the iterator */
      PtrIterator<value_type>& operator=(const ValuePtr sptr)
      {
	ptr = &sptr;
	return *this;
      }

      PtrIterator<value_type>& operator=(const PtrIterator<value_type>& piter)
      {
	ptr = piter.ptr;
	return *this;
      }

      bool operator==(const PtrIterator<value_type>& piter)
      { return ptr == piter.ptr; }

      bool operator!=(const PtrIterator<value_type>& piter)
      { return ptr != piter.ptr; }

    };


    /** @brief Bulk insertion helper: synchronization and construction
	of the tree bottom up */
    struct concat_problem
    {
      /** @brief Root of a tree.
       *
       * Input: Middle node to concatenate two subtrees. Out: Root of
       * the resulting concatenated tree. */
      _Rb_tree_node_ptr t;

      /** @brief Black height of @c t */
      int black_h;

      /** @brief Synchronization variable.
       *
       * \li READY_YES: the root of the tree can be concatenated with
       * the result of the children concatenation problems (both of
       * them have finished).
       * \li READY_NOT: at least one of the children
       * concatenation_problem have not finished */
      int is_ready;

      /** @brief Parent concatenation problem to solve when @c
	  is_ready = READY_YES */
      concat_problem* par_problem;

      /** @brief Left concatenation problem */
      concat_problem* left_problem;

      /** @brief Right concatenation problem */
      concat_problem* right_problem;

      /** @brief Value NO for the synchronization variable. */
      static const int READY_NO = 0;

      /** @brief Value YES for the synchronization variable. */
      static const int READY_YES = 1;

      /** @brief Trivial constructor.
       *
       * Initialize the synchronization variable to not ready. */
      concat_problem(): is_ready(READY_NO) { }

      /** @brief Constructor.
       *
       * Initialize the synchronization variable to not ready.
       * @param _t Root of a tree.
       * @param _black_h Black height of @c _t
       * @param _par_problem Parent concatenation problem to solve
       * when @c is_ready = READY_YES
       */
      concat_problem(const _Rb_tree_node_ptr _t, const int _black_h, 
		     concat_problem* _par_problem)
      : t(_t), black_h(_black_h), is_ready(READY_NO), par_problem(_par_problem)
      {
	// The root of an insertion problem must be black.
	if (t != NULL and t->_M_color == std::_S_red)
	  {
	    t->_M_color = std::_S_black;
	    ++black_h;
	  }
      }
    };


    /** @brief Bulk insertion helper: insertion of a sequence of
	elements in a subtree
	@invariant t, pos_beg and pos_end will not change after initialization
    */
    struct insertion_problem
    {
      /** @brief Renaming of _Rb_tree @c size_type */
      typedef _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc> tree_type;
      typedef typename tree_type::size_type size_type;

      /** @brief Root of the tree where the elements are to be inserted */
      _Rb_tree_node_ptr t;

      /** @brief Position of the first node in the array of nodes to
	  be inserted into @c t */
      size_type pos_beg;

      /** @brief Position of the first node in the array of nodes
	  that won't be inserted into @c t */
      size_type pos_end;

      /** @brief Partition in the array of nodes of @c pos_beg and @c
	  pos_end (must be the same for both, and so gaps are
	  avoided) */
      int array_partition;

      /** @brief Concatenation problem to solve once the insertion
	  problem is finished */
      concat_problem* conc;

      /** @brief Trivial constructor. */
      insertion_problem()
      { }

      /** @brief Constructor.
       * @param b Position of the first node in the array of nodes to
       * be inserted into @c _conc->t
       * @param e Position of the first node in the array of nodes
       * that won't be inserted into @c _conc->t
       * @param array_p Partition in the array of nodes of @c b and @c e
       * @param _conc Concatenation problem to solve once the
       * insertion problem is finished
       */
      insertion_problem(const size_type b, const size_type e, 
			const int array_p, concat_problem* _conc)
      : t(_conc->t), pos_beg(b), pos_end(e), array_partition(array_p), 
	conc(_conc)
      {
	_GLIBCXX_PARALLEL_ASSERT(pos_beg <= pos_end);

	//The root of an insertion problem must be black!!
	_GLIBCXX_PARALLEL_ASSERT(t == NULL or t->_M_color != std::_S_red);
      }
    };


    /** @brief Main bulk construction and insertion helper method
     * @param __first First element in a sequence to be added into the tree
     * @param __last End of the sequence of elements to be added into the tree
     * @param is_construction If true, the tree was empty and so, this
     * is constructed. Otherwise, the elements are added to an
     * existing tree.
     * @param strictly_less_or_less_equal Comparator to deal
     * transparently with repetitions with respect to the uniqueness
     * of the wrapping container
     * The input sequence is preprocessed so that the bulk
     * construction or insertion can be performed
     * efficiently. Essentially, the sequence is checked for
     * sortednesss and iterators to the middle of the structure are
     * saved so that afterwards the sequence can be processed
     * effectively in parallel. */
    template<typename _InputIterator, typename StrictlyLessOrLessEqual>
    void
    _M_bulk_insertion_construction(const _InputIterator __first, const _InputIterator __last, const bool is_construction, StrictlyLessOrLessEqual strictly_less_or_less_equal)
    {
      thread_index_t num_threads = get_max_threads();
      size_type n;
      size_type beg_partition[num_threads+1];
      _InputIterator access[num_threads+1];
      beg_partition[0] = 0;
      bool is_sorted= is_sorted_distance_accessors(__first, __last, access, beg_partition,n, num_threads, std::__iterator_category(__first));

      if (not is_sorted)
	{
	  _M_not_sorted_bulk_insertion_construction(access, beg_partition, n, num_threads, is_construction, strictly_less_or_less_equal);
	}
      else
	{
	  // The vector must be moved... all ranges must have at least
	  // one element, or make just sequential???
	  if (static_cast<size_type>(num_threads) > n)
	    {
	      int j = 1;
	      for (int i = 1; i <= num_threads; ++i)
		{
		  if (beg_partition[j-1] != beg_partition[i])
		    {
		      beg_partition[j] = beg_partition[i];
		      access[j] = access[i];
		      ++j;
		    }
		}
	      num_threads = static_cast<thread_index_t>(n);
	    }

	  if (is_construction)
	    _M_sorted_bulk_construction(access, beg_partition, n, num_threads, 
					strictly_less_or_less_equal);
	  else
	    _M_sorted_bulk_insertion(access, beg_partition, n, num_threads, 
				     strictly_less_or_less_equal);
	}
    }

    /** @brief Bulk construction and insertion helper method on an
     * input sequence which is not sorted
     *
     * The elements are copied, according to the copy policy, in order
     * to be sorted. Then the
     * _M_not_sorted_bulk_insertion_construction() method is called
     * appropriately
     * @param access Array of iterators of size @c num_threads +
     * 1. Each position contains the first element in each subsequence
     * to be added into the tree.
     * @param beg_partition Array of positions of size @c num_threads
     * + 1. Each position contains the rank of the first element in
     * each subsequence to be added into the tree.
     * @param n Size of the sequence to be inserted
     * @param num_threads Number of threads and corresponding
     * subsequences in which the insertion work is going to be shared
     * @param is_construction If true, the tree was empty and so, this
     * is constructed. Otherwise, the elements are added to an
     * existing tree.
     * @param strictly_less_or_less_equal Comparator to deal
     * transparently with repetitions with respect to the uniqueness
     * of the wrapping container 
     */
    template<typename _InputIterator, typename StrictlyLessOrLessEqual>
    void
    _M_not_sorted_bulk_insertion_construction(_InputIterator* access,
					      size_type* beg_partition,
					      const size_type n,
					      const thread_index_t num_threads,
					      const bool is_construction,
			   StrictlyLessOrLessEqual strictly_less_or_less_equal)
    {
      // Copy entire elements. In the case of a map, we would be
      // copying the pair. Therefore, the copy should be reconsidered
      // when objects are big. Essentially two cases:
      // - The key is small: make that the pair, is a pointer to data
      //   instead of a copy to it
      // - The key is big: we simply have a pointer to the iterator
#if _GLIBCXX_TREE_FULL_COPY
      nc_value_type* v = static_cast<nc_value_type*> (::operator new(sizeof(nc_value_type)*(n+1)));

      uninitialized_copy_from_accessors(access, beg_partition, v, num_threads);

      _M_not_sorted_bulk_insertion_construction<nc_value_type, nc_value_type*, ValueCompare<_Compare> >
	(beg_partition, v, ValueCompare<_Compare>(base_type::_M_impl._M_key_compare), n, num_threads, is_construction, strictly_less_or_less_equal);
#else
      // For sorting, we cannot use the new PtrIterator because we
      // want the pointers to be exchanged and not the elements.
      typedef PtrComparator<ValueCompare<_Compare>, nc_value_type*>  this_ptr_comparator;
      nc_value_type** v = static_cast<nc_value_type**> (::operator new(sizeof(nc_value_type*)*(n+1)));

      uninitialized_ptr_copy_from_accessors(access, beg_partition, v, num_threads);

      _M_not_sorted_bulk_insertion_construction<nc_value_type*, PtrIterator<nc_value_type>, this_ptr_comparator>
	(beg_partition, v, this_ptr_comparator(ValueCompare<_Compare>(base_type::_M_impl._M_key_compare)), n, num_threads, is_construction, strictly_less_or_less_equal);
#endif
    }

    /** @brief Bulk construction and insertion helper method on an
     * input sequence which is not sorted
     *
     * The elements are sorted and its accessors calculated. Then,
     * _M_sorted_bulk_construction() or _M_sorted_bulk_insertion() is
     * called.
     * @param beg_partition Array of positions of size @c num_threads
     * + 1. Each position contains the rank of the first element in
     * each subsequence to be added into the tree.
     * @param v Array of elements to be sorted (copy of the original sequence).
     * @param comp Comparator to be used for sorting the elements
     * @param n Size of the sequence to be inserted
     * @param num_threads Number of threads and corresponding
     * subsequences in which the insertion work is going to be shared
     * @param is_construction If true, _M_sorted_bulk_construction()
     * is called. Otherwise, _M_sorted_bulk_insertion() is called.
     * @param strictly_less_or_less_equal Comparator to deal
     * transparently with repetitions with respect to the uniqueness
     * of the wrapping container
     */
    template<typename ElementsToSort, typename IteratorSortedElements, typename Comparator, typename StrictlyLessOrLessEqual>
    void
    _M_not_sorted_bulk_insertion_construction(size_type* beg_partition, ElementsToSort* v, Comparator comp, const size_type n, thread_index_t num_threads, const bool is_construction, StrictlyLessOrLessEqual strictly_less_or_less_equal)
    {
      // The accessors have been calculated for the non sorted.
      num_threads = static_cast<thread_index_t>(std::min<size_type>(num_threads, n));

      std::stable_sort(v, v+n, comp);

      IteratorSortedElements sorted_access[num_threads+1];
      range_accessors(IteratorSortedElements(v), IteratorSortedElements(v+n), sorted_access, beg_partition, n, num_threads, std::__iterator_category(v));

      // Partial template specialization not available.
      if (is_construction)
	_M_sorted_bulk_construction(sorted_access, beg_partition, n, num_threads, strictly_less_or_less_equal);
      else
	_M_sorted_bulk_insertion(sorted_access, beg_partition, n, num_threads, strictly_less_or_less_equal);
      delete v;
    }

    /** @brief Construct a tree sequentially using the parallel routine
     * @param r_array Array of nodes from which to take the nodes to
     * build the tree
     * @param pos_beg Position of the first node in the array of nodes
     * to be part of the tree
     * @param pos_end Position of the first node in the array of nodes
     * that will not be part of the tree
     * @param black_h Black height of the resulting tree (out)
     */
    static _Rb_tree_node_ptr
    simple_tree_construct(_Rb_tree_node_ptr* r_array, const size_type pos_beg, 
			  const size_type pos_end, int& black_h)
    {
      if (pos_beg == pos_end)
	{
	  black_h = 0;
	  return NULL;
	}
      if (pos_beg+1 == pos_end)
	{
	  // It is needed, not only for efficiency but because the
	  // last level in our tree construction is red.
	  make_leaf(r_array[pos_beg], black_h);
	  return r_array[pos_beg];
	}

      // Dummy b_p
      size_type b_p[2];
      b_p[0] = 0;
      b_p[1] = pos_end - pos_beg;
      _Rb_tree_node_ptr* r= r_array + pos_beg;
      size_type length = pos_end - pos_beg;

      ranker_no_gaps rank;
      nodes_initializer<ranker_no_gaps> nodes_init(r, length, 1, rank);

      black_h = nodes_init.get_height();

      size_type split = nodes_init.get_shifted_splitting_point();
      for (size_type i = 0; i < split; ++i)
	nodes_init.link_complete(r[i],0);

      for (size_type i = split; i < length; ++i)
	nodes_init.link_incomplete(r[i],0);

      _Rb_tree_node_ptr t = nodes_init.get_root();
      _GLIBCXX_PARALLEL_ASSERT(rb_verify_tree(t));
      _GLIBCXX_PARALLEL_ASSERT(t->_M_color == std::_S_black);
      return t;
    }


    /** @brief Allocation of an array of nodes and initialization of
	their value fields from an input sequence. Done in parallel.
     * @param access Array of iterators of size @c num_threads +
     * 1. Each position contains the first value in the subsequence to
     * be copied into the corresponding tree node.
     * @param beg_partition Array of positions of size @c num_threads
     * + 1. Each position contains the rank of the first element in
     * the subsequence from which to copy the data to initialize the
     * nodes.
     * @param n Size of the sequence and the array of nodes to be allocated.
     * @param num_threads Number of threads and corresponding
     * subsequences in which the allocation and initialization work is
     * going to be shared
     */
    template<typename _Iterator>
    _Rb_tree_node_ptr* 
    _M_unsorted_bulk_allocation_and_initialization(const _Iterator* access, const size_type* beg_partition, const size_type n, const thread_index_t num_threads)
    {
      _Rb_tree_node_ptr* r = static_cast<_Rb_tree_node_ptr*> (::operator new (sizeof(_Rb_tree_node_ptr)*(n+1)));

      // Allocate and initialize the nodes (don't check for uniqueness
      // because the sequence is not necessarily sorted.
#pragma omp parallel num_threads(num_threads)
      {
#if USE_PAPI
	PAPI_register_thread();
#endif

	int iam = omp_get_thread_num();
	_Iterator it = access[iam];
	size_type i = beg_partition[iam];
	while (it!= access[iam+1])
	  {
	    r[i] = base_type::_M_create_node(*it);
	    ++i;
	    ++it;
	  }
      }
      return r;
    }


    /** @brief Allocation of an array of nodes and initialization of
     * their value fields from an input sequence. Done in
     * parallel. Besides, the sequence is checked for uniqueness while
     * copying the elements, and if there are repetitions, gaps within
     * the partitions are created.
     *
     * An extra ghost node pointer is reserved in the array to ease
     * comparisons later while linking the nodes
     * @pre The sequence is sorted.
     * @param access Array of iterators of size @c num_threads +
     * 1. Each position contains the first value in the subsequence to
     * be copied into the corresponding tree node.
     * @param beg_partition Array of positions of size @c num_threads
     * + 1. Each position contains the rank of the first element in
     * the subsequence from which to copy the data to initialize the
     * nodes.
     * @param rank_shift Array of size @c num_threads + 1 containing
     * the number of accumulated gaps at the beginning of each
     * partition
     * @param n Size of the sequence and the array of nodes (-1) to be
     * allocated.
     * @param num_threads Number of threads and corresponding
     * subsequences in which the allocation and initialization work is
     * going to be shared
     * @param strictly_less_or_less_equal Comparator to deal
     * transparently with repetitions with respect to the uniqueness
     * of the wrapping container
     */
    template<typename _Iterator, typename StrictlyLessOrLessEqual>
    _Rb_tree_node_ptr* 
    _M_sorted_bulk_allocation_and_initialization(_Iterator* access, size_type*  beg_partition, size_type* rank_shift, const size_type n, thread_index_t& num_threads, StrictlyLessOrLessEqual strictly_less_or_less_equal)
    {
      // Ghost node at the end to avoid extra comparisons in nodes_initializer.
      _Rb_tree_node_ptr* r = static_cast<_Rb_tree_node_ptr*> (::operator new (sizeof(_Rb_tree_node_ptr)*(n+1)));
      r[n] = NULL;

      // Dealing with repetitions (EFFICIENCY ISSUE).
      _Iterator access_copy[num_threads+1];
      for (int i = 0; i <= num_threads; ++i)
	access_copy[i] = access[i];
      // Allocate and initialize the nodes
#pragma omp parallel num_threads(num_threads)
      {
#if USE_PAPI
	PAPI_register_thread();
#endif
	thread_index_t iam = omp_get_thread_num();
	_Iterator prev = access[iam];
	size_type i = beg_partition[iam];
	_Iterator it = prev;
	if (iam != 0)
	  {
	    --prev;
	    // Dealing with repetitions (CORRECTNESS ISSUE).
	    while (it!= access_copy[iam+1] and not strictly_less_or_less_equal(_KeyOfValue()(*prev), _KeyOfValue()(*it)))
	      {
		_GLIBCXX_PARALLEL_ASSERT(not base_type::_M_impl._M_key_compare(_KeyOfValue()(*it),_KeyOfValue()(*prev)));
		++it;
	      }
	    access[iam] = it;
	    if (it != access_copy[iam+1]){
	      r[i] = base_type::_M_create_node(*it);
	      ++i;
	      prev=it;
	      ++it;
	    }
	    //}
	  }
	else
	  {
	    r[i] = base_type::_M_create_node(*prev);
	    ++i;
	    ++it;
	  }
	while (it!= access_copy[iam+1])
	  {
	    /*****	Dealing with repetitions (CORRECTNESS ISSUE) *****/
	    if (strictly_less_or_less_equal(_KeyOfValue()(*prev),_KeyOfValue()(*it)))
	      {
		r[i] = base_type::_M_create_node(*it);
		++i;
		prev=it;
	      }
	    else{
	      _GLIBCXX_PARALLEL_ASSERT(not base_type::_M_impl._M_key_compare(_KeyOfValue()(*it),_KeyOfValue()(*prev)));
	    }
	    ++it;
	  }
	/*****	Dealing with repetitions (EFFICIENCY ISSUE) *****/
	rank_shift[iam+1] =  beg_partition[iam+1] - i;
      }
      /*****	Dealing with repetitions (EFFICIENCY ISSUE) *****/
      rank_shift[0] = 0;
      /* Guarantee that there are no empty intervals.
      - If an empty interval is found, is joined with the previous one
	 (the rank_shift of the previous is augmented with all the new
	 repetitions)
      */
      thread_index_t i = 1;
      while (i <= num_threads and rank_shift[i] != (beg_partition[i] - beg_partition[i-1]))
	{
	  rank_shift[i] += rank_shift[i-1];
	  ++i;
	}
      if (i <= num_threads)
	{
	  thread_index_t j = i - 1;
	  while (true)
	    {
	      do
		{
		  rank_shift[j] += rank_shift[i];
		  ++i;
		} while (i <= num_threads and rank_shift[i] == (beg_partition[i] - beg_partition[i-1]));

	      beg_partition[j] = beg_partition[i-1];
	      access[j] = access[i-1];
	      if (i > num_threads) break;
	      ++j;

	      // Initialize with the previous.
	      rank_shift[j] = rank_shift[j-1];
	    }
	  num_threads = j;
	}
      return r;

    }

    /** @brief Allocation of an array of nodes and initialization of
     * their value fields from an input sequence.
     *
     * The allocation and initialization is done in parallel. Besides,
     * the sequence is checked for uniqueness while copying the
     * elements. However, in contrast to
     * _M_sorted_bulk_allocation_and_initialization(), if there are
     * repetitions, no gaps within the partitions are created. To do
     * so efficiently, some extra memory is needed to compute a prefix
     * sum.
     * @pre The sequence is sorted.
     * @param access Array of iterators of size @c num_threads +
     * 1. Each position contains the first value in the subsequence to
     * be copied into the corresponding tree node.
     * @param beg_partition Array of positions of size @c num_threads
     * + 1. Each position contains the rank of the first element in
     * the subsequence from which to copy the data to initialize the
     * nodes.
     * @param n Size of the sequence and the array of nodes (-1) to be
     * allocated.
     * @param num_threads Number of threads and corresponding
     * subsequences in which the allocation and initialization work is
     * going to be shared
     * @param strictly_less_or_less_equal Comparator to deal
     * transparently with repetitions with respect to the uniqueness
     * of the wrapping container
     */
    template<typename _Iterator, typename StrictlyLessOrLessEqual>
    _Rb_tree_node_ptr* 
    _M_sorted_no_gapped_bulk_allocation_and_initialization(_Iterator* access, size_type* beg_partition, size_type& n, const thread_index_t num_threads, StrictlyLessOrLessEqual strictly_less_or_less_equal)
    {
      size_type* sums = static_cast<size_type*> (::operator new (sizeof(size_type)*n));
      // Allocate and initialize the nodes
      /*		try
	{*/
#pragma omp parallel num_threads(num_threads)
      {
#if USE_PAPI
	PAPI_register_thread();
#endif
	int iam = omp_get_thread_num();
	_Iterator prev = access[iam];
	size_type i = beg_partition[iam];
	_Iterator it = prev;
	if (iam !=0)
	  {
	    --prev;

	    // First iteration here, to update accessor in case was
	    // equal to the last element of the previous range

	    // Dealing with repetitions (CORRECTNESS ISSUE).
	    if (strictly_less_or_less_equal(_KeyOfValue()(*prev),_KeyOfValue()(*it)))
	      {
		sums[i] = 0;
		prev=it;
	      }
	    else
	      {
		sums[i] = 1;
	      }
	    ++i;
	    ++it;
	  }
	else
	  {
	    sums[i] = 0;
	    ++i;
	    ++it;
	  }
	while (it!= access[iam+1])
	  {
	    // Dealing with repetitions (CORRECTNESS ISSUE).
	    if (strictly_less_or_less_equal(_KeyOfValue()(*prev),_KeyOfValue()(*it)))
	      {
		sums[i] = 0;
		prev=it;
	      }
	    else
	      sums[i] = 1;
	    ++i;
	    ++it;
	  }
      }
      // Should be done in parallel.
      partial_sum(sums,sums + n, sums);

      n -= sums[n-1];
      _Rb_tree_node_ptr* r = static_cast<_Rb_tree_node_ptr*> (::operator new (sizeof(_Rb_tree_node_ptr)*(n+1)));
      r[n]=0;

#pragma omp parallel num_threads(num_threads)
      {
#if USE_PAPI
	PAPI_register_thread();
#endif
	int iam = omp_get_thread_num();
	_Iterator it = access[iam];
	size_type i = beg_partition[iam];
	size_type j = i;
	size_type before = 0;
	if (iam > 0)
	  {
	    before = sums[i-1];
	    j -= sums[i-1];
	  }
	beg_partition[iam] = j;
	while (it!= access[iam+1])
	  {
	    while (it!= access[iam+1] and sums[i]!=before)
	      {
		before = sums[i];
		++i;
		++it;
	      }
	    if (it!= access[iam+1])
	      {
		r[j] = base_type::_M_create_node(*it);
		++j;
		++i;
		++it;
	      }
	  }

      }
      beg_partition[num_threads] = n;

      // Update beginning of partitions.
      ::operator delete(sums);
      return r;
    }

    /** @brief Main bulk construction method: perform the actual
	initialization, allocation and finally node linking once the
	input sequence has already been preprocessed.
     * @param access Array of iterators of size @c num_threads +
     * 1. Each position contains the first value in the subsequence to
     * be copied into the corresponding tree node.
     * @param beg_partition Array of positions of size @c num_threads
     * + 1. Each position contains the rank of the first element in
     * the subsequence from which to copy the data to initialize the
     * nodes.
     * @param n Size of the sequence and the array of nodes (-1) to be
     * allocated.
     * @param num_threads Number of threads and corresponding
     * subsequences in which the work is going to be shared
     * @param strictly_less_or_less_equal Comparator to deal
     * transparently with repetitions with respect to the uniqueness
     * of the wrapping container
     */
    template<typename _Iterator, typename StrictlyLessOrLessEqual>
    void
    _M_sorted_bulk_construction(_Iterator* access, size_type* beg_partition, const size_type n, thread_index_t num_threads, StrictlyLessOrLessEqual strictly_less_or_less_equal)
    {
      // Dealing with repetitions (EFFICIENCY ISSUE).
      size_type rank_shift[num_threads+1];

      _Rb_tree_node_ptr* r = _M_sorted_bulk_allocation_and_initialization(access, beg_partition, rank_shift, n, num_threads, strictly_less_or_less_equal);

      // Link the tree appropriately.
      // Dealing with repetitions (EFFICIENCY ISSUE).
      ranker_gaps rank(beg_partition, rank_shift, num_threads);
      nodes_initializer<ranker_gaps> nodes_init(r, n - rank_shift[num_threads], num_threads, rank);
      size_type split = nodes_init.get_shifted_splitting_point();

#pragma omp parallel num_threads(num_threads)
      {
#if USE_PAPI
	PAPI_register_thread();
#endif
	int iam = omp_get_thread_num();
	size_type beg = beg_partition[iam];
	// Dealing with repetitions (EFFICIENCY ISSUE).
	size_type end = beg_partition[iam+1] - (rank_shift[iam+1] - rank_shift[iam]);
	if (split >= end)
	  {
	    for (size_type i = beg; i < end; ++i)
	      {
		nodes_init.link_complete(r[i],iam);
	      }
	  }
	else
	  {
	    if (split <= beg)
	      {
		for (size_type i = beg; i < end; ++i)
		  nodes_init.link_incomplete(r[i],iam);
	      }
	    else
	      {
		for (size_type i = beg; i < split; ++i)
		  nodes_init.link_complete(r[i],iam);
		for (size_type i = split; i < end; ++i)
		  nodes_init.link_incomplete(r[i],iam);
	      }
	  }
      }
      // If the execution reaches this point, there has been no
      // exception, and so the structure can be initialized.

      // Join the tree laid on the array of ptrs with the header node.
      // Dealing with repetitions (EFFICIENCY ISSUE).
      base_type::_M_impl._M_node_count = n - rank_shift[num_threads];
      base_type::_M_impl._M_header._M_left = r[0];
      thread_index_t with_element =  num_threads;
      while ((beg_partition[with_element] - beg_partition[with_element-1]) == (rank_shift[with_element] - rank_shift[with_element-1]))
	{
	  --with_element;
	}
      base_type::_M_impl._M_header._M_right = r[beg_partition[with_element] - (rank_shift[with_element] - rank_shift[with_element-1]) - 1];
      base_type::_M_impl._M_header._M_parent = nodes_init.get_root();
      nodes_init.get_root()->_M_parent= &base_type::_M_impl._M_header;

      ::operator delete(r);
    }


    /** @brief Main bulk insertion method: perform the actual
	initialization, allocation and finally insertion once the
	input sequence has already been preprocessed.
     * @param access Array of iterators of size @c num_threads +
     * 1. Each position contains the first value in the subsequence to
     * be copied into the corresponding tree node.
     * @param beg_partition Array of positions of size @c num_threads
     * + 1. Each position contains the rank of the first element in
     * the subsequence from which to copy the data to initialize the
     * nodes.
     * @param k Size of the sequence to be inserted (including the
     * possible repeated elements among the sequence itself and
     * against those elements already in the tree)
     * @param num_threads Number of threads and corresponding
     * subsequences in which the work is going to be shared
     * @param strictly_less_or_less_equal Comparator to deal
     * transparently with repetitions with respect to the uniqueness
     * of the wrapping container
     */
    template<typename _Iterator, typename StrictlyLessOrLessEqual>
    void
    _M_sorted_bulk_insertion(_Iterator* access, size_type* beg_partition, size_type k, thread_index_t num_threads, StrictlyLessOrLessEqual strictly_less_or_less_equal)
    {
      _GLIBCXX_PARALLEL_ASSERT((size_type)num_threads <= k);
      // num_thr-1 problems in the upper part of the tree
      // num_thr problems to further parallelize
      std::vector<size_type> existing(num_threads,0);
#if _GLIBCXX_TREE_INITIAL_SPLITTING
      /*****	Dealing with repetitions (EFFICIENCY ISSUE) *****/
      size_type rank_shift[num_threads+1];

      // Need to create them dynamically because they are so erased
      concat_problem* conc[2*num_threads-1];
#endif
      _Rb_tree_node_ptr* r;
      /*****	Dealing with repetitions (EFFICIENCY ISSUE) *****/
      if (not strictly_less_or_less_equal(base_type::_S_key(base_type::_M_root()),base_type::_S_key(base_type::_M_root()) ))
	{
	  // Unique container
	  // Set 1 and 2 could be done in parallel ...
	  // 1. Construct the nodes with their corresponding data
#if _GLIBCXX_TREE_INITIAL_SPLITTING
	  r = _M_sorted_bulk_allocation_and_initialization(access, beg_partition, rank_shift, k, num_threads, strictly_less_or_less_equal);
#else
	  r = _M_sorted_no_gapped_bulk_allocation_and_initialization(access, beg_partition, k, num_threads, strictly_less_or_less_equal);
#endif
	}
      else
	{
	  // Not unique container.
	  r = _M_unsorted_bulk_allocation_and_initialization(access, beg_partition, k, num_threads);
#if _GLIBCXX_TREE_INITIAL_SPLITTING
	  // Trivial initialization of rank_shift.
	  for (int i=0; i <= num_threads; ++i)
	    rank_shift[i] = 0;
#endif
	}
#if _GLIBCXX_TREE_INITIAL_SPLITTING
      // Calculate position of last element to be inserted: must be
      // done now, or otherwise becomes messy.

  /***** Dealing with
      repetitions (EFFICIENCY ISSUE) *****/
      size_type last = beg_partition[num_threads] - (rank_shift[num_threads] - rank_shift[num_threads - 1]);

      //2. Split the tree according to access in num_threads parts
      //Initialize upper concat_problems
      //Allocate them dynamically because they are afterwards so erased
      for (int i=0; i < (2*num_threads-1); ++i)
	{
	  conc[i] = new concat_problem ();
	}
      concat_problem* root_problem = _M_bulk_insertion_initialize_upper_problems(conc, 0, num_threads, NULL);

      // The first position of access and the last are ignored, so we
      // have exactly num_threads subtrees.
      bool before = omp_get_nested();
      omp_set_nested(true);
      _M_bulk_insertion_split_tree_by_pivot(static_cast<_Rb_tree_node_ptr>(base_type::_M_root()), r, access, beg_partition, rank_shift, 0, num_threads-1, conc, num_threads, strictly_less_or_less_equal);
      omp_set_nested(before);

      // Construct upper tree with the first elements of ranges if
      // they are NULL We cannot do this by default because they could
      // be repeated and would not be checked.
      size_type r_s = 0;
      for (int pos = 1; pos < num_threads; ++pos)
	{
	  _GLIBCXX_PARALLEL_ASSERT(conc[(pos-1)*2]->t == NULL or conc[pos*2-1]->t == NULL or strictly_less_or_less_equal(base_type::_S_key(base_type::_S_maximum(conc[(pos-1)*2]->t)), base_type::_S_key(conc[pos*2-1]->t)));
	  _GLIBCXX_PARALLEL_ASSERT(conc[pos*2]->t == NULL  or conc[pos*2-1]->t == NULL or strictly_less_or_less_equal( base_type::_S_key(conc[pos*2-1]->t), base_type::_S_key(base_type::_S_minimum(conc[pos*2]->t))));
	  /*****	Dealing with repetitions (CORRECTNESS ISSUE) *****/

	  // The first element of the range is the root.
	  if (conc[pos*2-1]->t == NULL or (not(strictly_less_or_less_equal(base_type::_S_key(static_cast<_Rb_tree_node_ptr>(conc[pos*2-1]->t)), _KeyOfValue()(*access[pos])))))
	    {
	      // There was not a candidate element
	      // or
	      // Exists an initialized position in the array which
	      // corresponds to conc[pos*2-1]->t */
	      if (conc[pos*2-1]->t == NULL)
		{
		  size_t np = beg_partition[pos];
		  _GLIBCXX_PARALLEL_ASSERT(conc[(pos-1)*2]->t == NULL or strictly_less_or_less_equal(base_type::_S_key(base_type::_S_maximum(conc[(pos-1)*2]->t)), base_type::_S_key(r[np])));
		  _GLIBCXX_PARALLEL_ASSERT(conc[pos*2]->t == NULL  or strictly_less_or_less_equal( base_type::_S_key(r[np]), base_type::_S_key(base_type::_S_minimum(conc[pos*2]->t))));
		  conc[pos*2-1]->t = r[np];
		  r[np]->_M_color = std::_S_black;
		  ++base_type::_M_impl._M_node_count;
		}
	      else
		{
		  base_type::_M_destroy_node(r[beg_partition[pos]]);
		}
	      ++(access[pos]);
	      ++(beg_partition[pos]);
	      ++r_s;
	    }
	  _GLIBCXX_PARALLEL_ASSERT(conc[(pos-1)*2]->t == NULL or conc[(pos-1)*2]->t->_M_color == std::_S_black);
	  /*****	Dealing with repetitions (EFFICIENCY ISSUE) *****/
	  rank_shift[pos] += r_s;
	}
      /*****	Dealing with repetitions (EFFICIENCY ISSUE) *****/
      rank_shift[num_threads] += r_s;
#else
      concat_problem root_problem_on_stack(static_cast<_Rb_tree_node_ptr>(base_type::_M_root()), black_height(static_cast<_Rb_tree_node_ptr>(base_type::_M_root())), NULL);
      concat_problem * root_problem = &root_problem_on_stack;
      size_type last = k;
#endif

      // 3. Split the range according to tree and create
      // 3. insertion/concatenation problems to be solved in parallel
#if _GLIBCXX_TREE_DYNAMIC_BALANCING
      size_type min_problem = (k/num_threads) / (log2(k/num_threads + 1)+1);
#else
      size_type min_problem = base_type::size() + k;
#endif

      RestrictedBoundedConcurrentQueue<insertion_problem>* ins_problems[num_threads];

#pragma omp parallel num_threads(num_threads)
      {
	int num_thread = omp_get_thread_num();
	ins_problems[num_thread] = new RestrictedBoundedConcurrentQueue<insertion_problem>(2*(log2(base_type::size())+1));
#if _GLIBCXX_TREE_INITIAL_SPLITTING
	/*****	Dealing with repetitions (EFFICIENCY ISSUE) *****/
	size_type end_k_thread = beg_partition[num_thread+1]  - (rank_shift[num_thread+1] - rank_shift[num_thread]);
	ins_problems[num_thread]->push_front(insertion_problem(beg_partition[num_thread], end_k_thread, num_thread, conc[num_thread*2]));
#else
	// size_type end_k_thread = beg_partition[num_thread+1];
#endif
	insertion_problem ip_to_solve;
	bool change;

#if _GLIBCXX_TREE_INITIAL_SPLITTING
#pragma omp barrier
#else
#pragma omp single
	ins_problems[num_thread]->push_front(insertion_problem(0, k, num_thread, root_problem));
#endif

	do
	  {
	    // First do own work.
	    while (ins_problems[num_thread]->pop_front(ip_to_solve))
	      {
		_GLIBCXX_PARALLEL_ASSERT(ip_to_solve.pos_beg <= ip_to_solve.pos_end);
		_M_bulk_insertion_split_sequence(r, ins_problems[num_thread], ip_to_solve, existing[num_thread], min_problem, strictly_less_or_less_equal);

	      }
	    yield();
	    change = false;

	    //Then, try to steal from others (and become own).
	    for (int i=1; i<num_threads; ++i)
	      {
		if (ins_problems[(num_thread+i)%num_threads]->pop_back(ip_to_solve))
		  {
		    change = true;
		    _M_bulk_insertion_split_sequence(r, ins_problems[num_thread], ip_to_solve, existing[num_thread], min_problem, strictly_less_or_less_equal);
		    break;
		  }
	      }
	  } while (change);
      }

      // Update root and sizes.
      base_type::_M_root() = root_problem->t;
      root_problem->t->_M_parent = &(base_type::_M_impl._M_header);
      /*****	Dealing with repetitions (EFFICIENCY ISSUE) *****/

      // Add the k elements that wanted to be inserted, minus the ones
      // that were repeated.
#if _GLIBCXX_TREE_INITIAL_SPLITTING
      base_type::_M_impl._M_node_count += (k - (rank_shift[num_threads]));
#else
      base_type::_M_impl._M_node_count += k;
#endif
      // Also then, take out the ones that were already existing in the tree.
      for (int i = 0; i< num_threads; ++i)
	{
	  base_type::_M_impl._M_node_count -= existing[i];
	}
      // Update leftmost and rightmost.
      /*****	Dealing with repetitions (EFFICIENCY ISSUE) *****/
      if (not strictly_less_or_less_equal(base_type::_S_key(base_type::_M_root()), base_type::_S_key(base_type::_M_root()))){
	// Unique container.
	if (base_type::_M_impl._M_key_compare(_KeyOfValue()(*(access[0])), base_type::_S_key(base_type::_M_leftmost())))
	  base_type::_M_leftmost() = r[0];
	if (base_type::_M_impl._M_key_compare(base_type::_S_key(base_type::_M_rightmost()), _KeyOfValue()(*(--access[num_threads]))))
	  base_type::_M_rightmost() = r[last - 1];
      }
      else{
	if (strictly_less_or_less_equal(_KeyOfValue()(*(access[0])), base_type::_S_key(base_type::_M_leftmost())))
	  base_type::_M_leftmost() = base_type::_S_minimum(base_type::_M_root());
	if (strictly_less_or_less_equal(base_type::_S_key(base_type::_M_rightmost()), _KeyOfValue()(*(--access[num_threads]))))
	  base_type::_M_rightmost() =  base_type::_S_maximum(base_type::_M_root());
      }




#if _GLIBCXX_TREE_INITIAL_SPLITTING
      // Delete root problem
      delete root_problem;
#endif

      // Delete queues
      for (int pos = 0; pos < num_threads; ++pos)
	{
	  delete ins_problems[pos];
	}

      // Delete array of pointers
      ::operator delete(r);
    }


    /** @brief Divide a tree according to the splitter elements of a
     * given sequence.
     *
     * The tree of the initial recursive call is divided in exactly
     * num_threads partitions, some of which may be empty. Besides,
     * some nodes may be extracted from it to afterwards concatenate
     * the subtrees resulting from inserting the elements into it.
     * This is done sequentially. It could be done in parallel but the
     * performance is much worse.
     * @param t Root of the tree to be split
     * @param r Array of nodes to be inserted into the tree (here only
     * used to look up its elements)
     * @param access Array of iterators of size @c num_threads +
     * 1. Each position contains the first value in the subsequence
     * that has been copied into the corresponding tree node.
     * @param beg_partition Array of positions of size @c num_threads
     * + 1. Each position contains the rank of the first element in
     * the array of nodes to be inserted.
     * @param rank_shift Array of size @c num_threads + 1 containing
     * the number of accumulated gaps at the beginning of each
     * partition
     * @param pos_beg First position in the access array to be
     * considered to split @c t
     * @param pos_end Last position (included) in the access array to
     * be considered to split @c t
     * @param conc Array of concatenation problems to be initialized
     * @param num_threads Number of threads and corresponding
     * subsequences in which the original sequence has been
     * partitioned
     * @param strictly_less_or_less_equal Comparator to deal
     * transparently with repetitions with respect to the uniqueness
     * of the wrapping container
     */
    template<typename _Iterator, typename StrictlyLessOrLessEqual>
    void
    _M_bulk_insertion_split_tree_by_pivot(_Rb_tree_node_ptr t, _Rb_tree_node_ptr* r, _Iterator* access,  size_type* beg_partition, size_type* rank_shift, const size_type pos_beg, const size_type pos_end, concat_problem** conc, const thread_index_t num_threads, StrictlyLessOrLessEqual strictly_less_or_less_equal)
    {
      if (pos_beg == pos_end)
	{
	  //Elements are in [pos_beg, pos_end]
	  conc[pos_beg*2]->t = t;
	  conc[pos_beg*2]->black_h = black_height(t);
	  force_black_root (conc[pos_beg*2]->t, conc[pos_beg*2]->black_h);
	  return;
	}
      if (t == 0)
	{
	  for (size_type i = pos_beg; i < pos_end; ++i)
	    {
	      conc[i*2]->t = NULL;
	      conc[i*2]->black_h = 0;
	      conc[i*2+1]->t = NULL;
	    }
	  conc[pos_end*2]->t = NULL;
	  conc[pos_end*2]->black_h = 0;
	  return;
	}

      // Return the last pos, in which key >= (pos-1).
      // Search in the range [pos_beg, pos_end]
      size_type pos = std::upper_bound(access + pos_beg, access + pos_end + 1, base_type::_S_key(t), compare_value_key<_Iterator, _Compare>(base_type::_M_impl._M_key_compare)) - access;
      if (pos != pos_beg)
	{
	  --pos;
	}
      _GLIBCXX_PARALLEL_ASSERT(pos == 0 or not base_type::_M_impl._M_key_compare(base_type::_S_key(t), _KeyOfValue()(*access[pos])));


      _Rb_tree_node_ptr ll, lr;
      int black_h_ll, black_h_lr;
      _Rb_tree_node_ptr rl, rr;
      int black_h_rl, black_h_rr;

      if (pos != pos_beg)
	{
	  _Rb_tree_node_ptr prev = r[beg_partition[pos] - 1 - (rank_shift[pos] - rank_shift[pos - 1])];

	  _GLIBCXX_PARALLEL_ASSERT(strictly_less_or_less_equal(base_type::_S_key(prev), _KeyOfValue()(*access[pos])));

	  split(static_cast<_Rb_tree_node_ptr>(t->_M_left),
		static_cast<const key_type&>(_KeyOfValue()(*access[pos])),
		static_cast<const key_type&>(base_type::_S_key(prev)),
		conc[pos*2-1]->t, ll, lr, black_h_ll, black_h_lr,
		strictly_less_or_less_equal);

	  _M_bulk_insertion_split_tree_by_pivot(ll, r, access, beg_partition, rank_shift, pos_beg, pos-1, conc,num_threads, strictly_less_or_less_equal);
	}
      else
	{
	  lr = static_cast<_Rb_tree_node_ptr>(t->_M_left);
	  black_h_lr = black_height (lr);
	  force_black_root (lr, black_h_lr);
	}

      if (pos != pos_end)
	{
	  _Rb_tree_node_ptr prev = r[beg_partition[pos+1] - 1 - (rank_shift[pos+1] - rank_shift[pos])];

	  _GLIBCXX_PARALLEL_ASSERT(not base_type::_M_impl._M_key_compare(_KeyOfValue()(*access[pos+1]), base_type::_S_key(prev)));
	  _GLIBCXX_PARALLEL_ASSERT(strictly_less_or_less_equal(base_type::_S_key(prev), _KeyOfValue()(*access[pos+1])));

	  split(static_cast<_Rb_tree_node_ptr>(t->_M_right),
		static_cast<const key_type&>(_KeyOfValue()(*access[pos+1])),
		static_cast<const key_type&>(base_type::_S_key(prev)),
		conc[pos*2+1]->t, rl, rr, black_h_rl, black_h_rr,
		strictly_less_or_less_equal);

	  _M_bulk_insertion_split_tree_by_pivot(rr, r, access, beg_partition, rank_shift, pos+1, pos_end, conc,num_threads, strictly_less_or_less_equal);
	}
      else
	{
	  rl = static_cast<_Rb_tree_node_ptr>(t->_M_right);
	  black_h_rl = black_height (rl);
	  force_black_root (rl, black_h_rl);
	}

      // When key(t) is equal to key(access[pos]) and no other key in
      // the left tree satisfies the criteria to be conc[pos*2-1]->t,
      // key(t) must be assigned to it to avoid repetitions.
      // Therefore, we do not have a root parameter for the
      // concatenate function and a new concatenate function must be
      // provided.
      if (pos != pos_beg and conc[pos*2-1]->t == NULL and not strictly_less_or_less_equal(_KeyOfValue()(*access[pos]), base_type::_S_key(t)))
	{
	  conc[pos*2-1]->t = t;
	  t = NULL;
	}
      concatenate(t, lr, rl, black_h_lr, black_h_rl, conc[pos*2]->t, conc[pos*2]->black_h);
    }

    /** @brief Divide the insertion problem until a leaf is reached or
     * the problem is small.
     *
     *  During the recursion, the right subproblem is queued, so that
     *  it can be handled by any thread.  The left subproblem is
     *  divided recursively, and finally, solved right away
     *  sequentially.
     * @param r Array of nodes containing the nodes to added into the tree
     * @param ins_problems Pointer to a queue of insertion
     * problems. The calling thread owns this queue, i. e. it is the
     * only one to push elements, but other threads could pop elements
     * from it in other methods.
     * @param ip Current insertion problem to be solved
     * @param existing Number of existing elements found when solving
     * the insertion problem (out)
     * @param min_problem Threshold size on the size of the insertion
     * problem in which to stop recursion
     * @param strictly_less_or_less_equal Comparator to deal
     * transparently with repetitions with respect to the uniqueness
     * of the wrapping container
     */
    template<typename StrictlyLessOrLessEqual>
    void
    _M_bulk_insertion_split_sequence(_Rb_tree_node_ptr* r, RestrictedBoundedConcurrentQueue<insertion_problem>* ins_problems,  insertion_problem& ip, size_type& existing, const size_type min_problem, StrictlyLessOrLessEqual strictly_less_or_less_equal)
    {
      _GLIBCXX_PARALLEL_ASSERT(ip.t == ip.conc->t);
      if (ip.t == NULL or (ip.pos_end- ip.pos_beg) <= min_problem)
	{
	  // SOLVE PROBLEM SEQUENTIALLY
	  // Start solving the problem.
	  _GLIBCXX_PARALLEL_ASSERT(ip.pos_beg <= ip.pos_end);
	  _M_bulk_insertion_merge_concatenate(r, ip, existing, strictly_less_or_less_equal);
	  return;
	}

      size_type pos_beg_right;
      size_type pos_end_left = divide(r, ip.pos_beg, ip.pos_end, base_type::_S_key(ip.t), pos_beg_right, existing, strictly_less_or_less_equal);

      int black_h_l, black_h_r;
      if (ip.t->_M_color == std::_S_black)
	{
	  black_h_l = black_h_r = ip.conc->black_h - 1;
	}
      else
	{
	  black_h_l = black_h_r = ip.conc->black_h;
	}

      // Right problem into the queue.
      ip.conc->right_problem = new concat_problem(static_cast<_Rb_tree_node_ptr>(ip.t->_M_right), black_h_r, ip.conc);
      ip.conc->left_problem = new concat_problem(static_cast<_Rb_tree_node_ptr>(ip.t->_M_left), black_h_l, ip.conc);

      ins_problems->push_front(insertion_problem(pos_beg_right, ip.pos_end, ip.array_partition, ip.conc->right_problem));

      // Solve left problem.
      insertion_problem ip_left(ip.pos_beg, pos_end_left, ip.array_partition, ip.conc->left_problem);
      _M_bulk_insertion_split_sequence(r, ins_problems, ip_left, existing, min_problem, strictly_less_or_less_equal);
    }


    /** @brief Insert a sequence of elements into a tree using a
     * divide-and-conquer scheme.
     *
     * The problem is solved recursively and sequentially dividing the
     * sequence to be inserted according to the root of the tree. This
     * is done until a leaf is reached or the proportion of elements
     * to be inserted is small. Finally, the two resulting trees are
     * concatenated.
     *  @param r_array Array of nodes containing the nodes to be added
     *  into the tree (among others)
     *  @param t Root of the tree
     *  @param pos_beg Position of the first node in the array of
     *  nodes to be inserted into the tree
     *  @param pos_end Position of the first node in the array of
     *  nodes that will not be inserted into the tree
     *  @param existing Number of existing elements found while
     *  inserting the range [@c pos_beg, @c pos_end) (out)
     *  @param black_h Height of the tree @c t and of the resulting
     *  tree after the recursive calls (in and out)
     *  @param strictly_less_or_less_equal Comparator to deal
     *  transparently with repetitions with respect to the uniqueness
     *  of the wrapping container
     *  @return Resulting tree after the elements have been inserted
     */
    template<typename StrictlyLessOrLessEqual>
    _Rb_tree_node_ptr 
    _M_bulk_insertion_merge(_Rb_tree_node_ptr* r_array, _Rb_tree_node_ptr t, const size_type pos_beg, const size_type pos_end,  size_type& existing, int& black_h, StrictlyLessOrLessEqual strictly_less_or_less_equal)
    {
#ifndef NDEBUG
      int count;
#endif
      _GLIBCXX_PARALLEL_ASSERT(pos_beg<=pos_end);

      // Leaf: a tree with the range must be constructed. Returns its
      // height in black nodes and its root (in ip.t) If there is
      // nothing to insert, we still need the height for balancing.
      if (t == NULL)
	{
	  if (pos_end == pos_beg) return NULL;
	  t = simple_tree_construct(r_array,pos_beg, pos_end, black_h);
	  _GLIBCXX_PARALLEL_ASSERT(rb_verify_tree(t,count));
	  return t;
	}
      if (pos_end == pos_beg)
	return t;
      if ((pos_end - pos_beg) <= (size_type)(black_h))
	{
	  // Exponential size tree with respect the number of elements
	  // to be inserted.
	  for (size_type p = pos_beg; p < pos_end; ++p)
	    {
	      t = _M_insert_local(t, r_array[p], existing, black_h, strictly_less_or_less_equal);
	    }
	  _GLIBCXX_PARALLEL_ASSERT(rb_verify_tree(t,count));
	  return t;
	}

      size_type pos_beg_right;
      size_type pos_end_left = divide(r_array, pos_beg, pos_end, base_type::_S_key(t), pos_beg_right, existing, strictly_less_or_less_equal);


      int black_h_l, black_h_r;
      if (t->_M_color == std::_S_black)
	{
	  black_h_l = black_h_r = black_h - 1;
	}
      else
	{
	  black_h_l = black_h_r = black_h;
	}
      force_black_root(t->_M_left, black_h_l);
      _Rb_tree_node_ptr l = _M_bulk_insertion_merge(r_array, static_cast<_Rb_tree_node_ptr>(t->_M_left), pos_beg, pos_end_left, existing, black_h_l, strictly_less_or_less_equal);
      force_black_root(t->_M_right, black_h_r);
      _Rb_tree_node_ptr r = _M_bulk_insertion_merge(r_array, static_cast<_Rb_tree_node_ptr>(t->_M_right), pos_beg_right, pos_end, existing, black_h_r, strictly_less_or_less_equal);

      concatenate(t, l, r, black_h_l,  black_h_r, t, black_h);

      return t;
    }

    /** @brief Solve a given insertion problem and all the parent
     * concatenation problem that are ready to be solved.
     *
     *  First, solve an insertion problem.

     *  Then, check if it is possible to solve the parent
     *  concatenation problem. If this is the case, solve it and go
     *  up recursively, as far as possible. Quit otherwise.
     *
     *  @param r Array of nodes containing the nodes to be added into
     *  the tree (among others)
     *  @param ip Insertion problem to solve initially.
     *  @param existing Number of existing elements found while
     *  inserting the range defined by the insertion problem (out)
     *  @param strictly_less_or_less_equal Comparator to deal
     *  transparently with repetitions with respect to the uniqueness
     *  of the wrapping container
     */
    template<typename StrictlyLessOrLessEqual>
    void 
    _M_bulk_insertion_merge_concatenate(_Rb_tree_node_ptr* r, insertion_problem& ip, size_type& existing, StrictlyLessOrLessEqual strictly_less_or_less_equal)
    {
      concat_problem* conc = ip.conc;
      _GLIBCXX_PARALLEL_ASSERT(ip.pos_beg <= ip.pos_end);

      conc->t = _M_bulk_insertion_merge(r, ip.t, ip.pos_beg, ip.pos_end, existing, conc->black_h, strictly_less_or_less_equal);
      _GLIBCXX_PARALLEL_ASSERT(conc->t == NULL or conc->t->_M_color == std::_S_black);

      bool is_ready = true;
      while (conc->par_problem != NULL and is_ready)
	{
	  // Pre: exists left and right problem, so there is not a deadlock
	  if (compare_and_swap(&conc->par_problem->is_ready, concat_problem::READY_NO,  concat_problem::READY_YES))
	    is_ready = false;

	  if (is_ready)
	    {
	      conc = conc->par_problem;
	      _GLIBCXX_PARALLEL_ASSERT(conc->left_problem!=NULL and conc->right_problem!=NULL);
	      _GLIBCXX_PARALLEL_ASSERT (conc->left_problem->black_h >=0 and conc->right_problem->black_h>=0);
	      // Finished working with the problems.
	      concatenate(conc->t, conc->left_problem->t, conc->right_problem->t, conc->left_problem->black_h,  conc->right_problem->black_h, conc->t, conc->black_h);

	      delete conc->left_problem;
	      delete conc->right_problem;
	    }
	}
    }

    // Begin of sorting, searching and related comparison-based helper methods.

    /** @brief Check whether a random-access sequence is sorted, and
     * calculate its size.
     *
     *  @param __first Begin iterator of sequence.
     *  @param __last End iterator of sequence.
     *  @param dist Size of the sequence (out)
     *  @return sequence is sorted. */
    template<typename _RandomAccessIterator>
    bool
    is_sorted_distance(const _RandomAccessIterator __first, const _RandomAccessIterator __last, size_type& dist, std::random_access_iterator_tag) const
    {
      gr_or_eq<_Compare, _RandomAccessIterator> geq(base_type::_M_impl._M_key_compare);
      dist = __last - __first;

      // In parallel.
      return equal(__first + 1, __last, __first, geq);
    }

    /** @brief Check whether an input sequence is sorted, and
     * calculate its size.
     *
     *  The list partitioning tool is used so that all the work is
     *  done in only one traversal.
     *  @param __first Begin iterator of sequence.
     *  @param __last End iterator of sequence.
     *  @param dist Size of the sequence (out)
     *  @return sequence is sorted. */
    template<typename _InputIterator>
    bool
    is_sorted_distance(const _InputIterator __first, const _InputIterator __last, size_type& dist, std::input_iterator_tag) const
    {
      dist = 1;
      bool is_sorted = true;
      _InputIterator it = __first;
      _InputIterator prev = it++;
      while (it != __last)
	{
	  ++dist;
	  if (base_type::_M_impl._M_key_compare(_KeyOfValue()(*it),_KeyOfValue()(*prev)))
	    {
	      is_sorted = false;
	      ++it;
	      break;
	    }
	  prev = it;
	  ++it;
	}
      while (it != __last)
	{
	  ++dist;
	  ++it;
	}
      return is_sorted;
    }

    /** @brief Check whether a random-access sequence is sorted,
     * calculate its size, and obtain intermediate accessors to the
     * sequence to ease parallelization.
     *
     *  @param __first Begin iterator of sequence.
     *  @param __last End iterator of sequence.
     *  @param access Array of size @c num_pieces + 1 that defines @c
     *  num_pieces subsequences of the original sequence (out). Each
     *  position @c i will contain an iterator to the first element in
     *  the subsequence @c i.
     *  @param beg_partition Array of size @c num_pieces + 1 that
     *  defines @c num_pieces subsequences of the original sequence
     *  (out). Each position @c i will contain the rank of the first
     *  element in the subsequence @c i.
     *  @param dist Size of the sequence (out)
     *  @param num_pieces Number of pieces to generate.
     *  @return Sequence is sorted. */
    template<typename _RandomAccessIterator>
    bool
    is_sorted_distance_accessors(const _RandomAccessIterator __first, const _RandomAccessIterator __last,  _RandomAccessIterator* access, size_type* beg_partition, size_type& dist, thread_index_t& num_pieces, std::random_access_iterator_tag) const
    {
      bool is_sorted = is_sorted_distance(__first, __last, dist,std::__iterator_category(__first));
      if (dist < (unsigned int) num_pieces)
	num_pieces = dist;

      // Do it opposite way to use accessors in equal function???
      range_accessors(__first,__last, access, beg_partition, dist, num_pieces, std::__iterator_category(__first));
      return is_sorted;
    }

    /** @brief Check whether an input sequence is sorted, calculate
     * its size, and obtain intermediate accessors to the sequence to
     * ease parallelization.
     *
     *  The list partitioning tool is used so that all the work is
     *  done in only one traversal.
     *  @param __first Begin iterator of sequence.
     *  @param __last End iterator of sequence.
     *  @param access Array of size @c num_pieces + 1 that defines @c
     *  num_pieces subsequences of the original sequence (out). Each
     *  position @c i will contain an iterator to the first element in
     *  the subsequence @c i.
     *  @param beg_partition Array of size @c num_pieces + 1 that
     *  defines @c num_pieces subsequences of the original sequence
     *  (out). Each position @c i will contain the rank of the first
     *  element in the subsequence @c i.
     *  @param dist Size of the sequence (out)
     *  @param num_pieces Number of pieces to generate.
     *  @return Sequence is sorted. */
    template<typename _InputIterator>
    bool
    is_sorted_distance_accessors(const _InputIterator __first, const _InputIterator __last, _InputIterator* access,  size_type* beg_partition, size_type& dist, thread_index_t& num_pieces, std::input_iterator_tag) const
    {
      is_sorted_functor<_InputIterator, _Compare> sorted(__first, base_type::_M_impl._M_key_compare);
      dist = list_partition(__first, __last, access,  (beg_partition+1),  num_pieces, sorted,  0);

      // Calculate the rank of the beginning each partition from the
      // sequence sizes (what is stored at this point in beg_partition
      // array).
      beg_partition[0] = 0;
      for (int i = 0; i < num_pieces; ++i)
	{
	  beg_partition[i+1] += beg_partition[i];
	}

      return sorted.is_sorted();
    }

    /** @brief Make a full copy of the elements of a sequence
     *
     *  The uninitialized_copy method from the STL is called in parallel
     *  using the access array to point to the beginning of each
     *  partition
     *  @param access Array of size @c num_threads + 1 that defines @c
     *  num_threads subsequences. Each position @c i contains an
     *  iterator to the first element in the subsequence @c i.
     *  @param beg_partition Array of size @c num_threads + 1 that
     *  defines @c num_threads subsequences. Each position @c i
     *  contains the rank of the first element in the subsequence @c
     *  i.
     *  @param out Begin iterator of output sequence.
     *  @param num_threads Number of threads to use. */
    template<typename _InputIterator, typename _OutputIterator>
    static void
    uninitialized_copy_from_accessors(_InputIterator* access, size_type* beg_partition, _OutputIterator out, const thread_index_t num_threads)
    {
#pragma omp parallel num_threads(num_threads)
      {
	int iam = omp_get_thread_num();
	uninitialized_copy(access[iam], access[iam+1], out+beg_partition[iam]);
      }
    }

    /** @brief Make a copy of the pointers of the elements of a sequence
     *  @param access Array of size @c num_threads + 1 that defines @c
     *  num_threads subsequences. Each position @c i contains an
     *  iterator to the first element in the subsequence @c i.
     *  @param beg_partition Array of size @c num_threads + 1 that
     *  defines @c num_threads subsequences. Each position @c i
     *  contains the rank of the first element in the subsequence @c
     *  i.
     *  @param out Begin iterator of output sequence.
     *  @param num_threads Number of threads to use. */
    template<typename _InputIterator, typename _OutputIterator>
    static void
    uninitialized_ptr_copy_from_accessors(_InputIterator* access, size_type* beg_partition, _OutputIterator out, const thread_index_t num_threads)
    {
#pragma omp parallel num_threads(num_threads)
      {
	int iam = omp_get_thread_num();
	_OutputIterator itout = out + beg_partition[iam];
	for (_InputIterator it = access[iam]; it != access[iam+1]; ++it)
	  {
	    *itout = &(*it);
	    ++itout;
	  }
      }
    }

    /** @brief Split a sorted node array in two parts according to a key.
     *
     *  For unique containers, if the splitting key is in the array of
     *  nodes, the corresponding node is erased.
     *  @param r Array of nodes containing the nodes to split (among others)
     *  @param pos_beg Position of the first node in the array of
     *  nodes to be considered
     *  @param pos_end Position of the first node in the array of
     *  nodes to be not considered
     *  @param key Splitting key
     *  @param pos_beg_right Position of the first node in the
     *  resulting right partition (out)
     *  @param existing Number of existing elements before dividing
     *  (in) and after (out). Specifically, the counter is
     *  incremented by one for unique containers if the splitting key
     *  was already in the array of nodes.
     *  @param strictly_less_or_less_equal Comparator to deal
     *  transparently with repetitions with respect to the uniqueness
     *  of the wrapping container
     *  @return Position of the last node (not included) in the
     *  resulting left partition (out)
     */
    template<typename StrictlyLessOrLessEqual>
    size_type
    divide(_Rb_tree_node_ptr* r, const size_type pos_beg, const size_type pos_end, const key_type& key, size_type& pos_beg_right, size_type& existing, StrictlyLessOrLessEqual strictly_less_or_less_equal)
    {
      pos_beg_right = std::lower_bound(r + pos_beg, r + pos_end, key, compare_node_key<_Compare>(base_type::_M_impl._M_key_compare)) - r;

      //Check if the element exists.
      size_type pos_end_left = pos_beg_right;

      // If r[pos_beg_right] is equal to key, must be erased
      /*****	Dealing with repetitions (CORRECTNESS ISSUE) *****/
      _GLIBCXX_PARALLEL_ASSERT((pos_beg_right == pos_end) or not base_type::_M_impl._M_key_compare(base_type::_S_key(r[pos_beg_right]),key));
      _GLIBCXX_PARALLEL_ASSERT((pos_beg_right + 1 >= pos_end) or strictly_less_or_less_equal(key, base_type::_S_key(r[pos_beg_right + 1])));
      if (pos_beg_right != pos_end and not strictly_less_or_less_equal(key, base_type::_S_key(r[pos_beg_right])))
	{
	  _M_destroy_node(r[pos_beg_right]);
	  r[pos_beg_right] = NULL;
	  ++pos_beg_right;
	  ++existing;
	}
      _GLIBCXX_PARALLEL_ASSERT(pos_end_left <= pos_beg_right and pos_beg_right <= pos_end and pos_end_left >= pos_beg);
      return pos_end_left;
    }


    /** @brief Parallelization helper method: Given a random-access
	sequence of known size, divide it into pieces of almost the
	same size.
     *  @param __first Begin iterator of sequence.
     *  @param __last End iterator of sequence.
     *  @param access Array of size @c num_pieces + 1 that defines @c
     *  num_pieces subsequences. Each position @c i contains an
     *  iterator to the first element in the subsequence @c i.
     *  @param beg_partition Array of size @c num_pieces + 1 that
     *  defines @c num_pieces subsequences. Each position @c i
     *  contains the rank of the first element in the subsequence @c
     *  i.
     *  @param n Sequence size
     *  @param num_pieces Number of pieces. */
    template<typename _RandomAccessIterator>
    static void
    range_accessors(const _RandomAccessIterator __first, const _RandomAccessIterator __last,  _RandomAccessIterator* access, size_type* beg_partition, const size_type n, const thread_index_t num_pieces, std::random_access_iterator_tag)
    {
      access[0] = __first;
      for (int i=1; i< num_pieces; ++i)
	{
	  access[i] = access[i-1] + (__last-__first)/num_pieces;
	  beg_partition[i]= beg_partition[i-1]+ (__last-__first)/num_pieces;
	}
      beg_partition[num_pieces] = __last - access[num_pieces-1] +  beg_partition[num_pieces-1];
      access[num_pieces]= __last;
    }

    /** @brief Parallelization helper method: Given an input-access
	sequence of known size, divide it into pieces of almost the
	same size.
     *  @param __first Begin iterator of sequence.
     *  @param __last End iterator of sequence.
     *  @param access Array of size @c num_pieces + 1 that defines @c
     *  num_pieces subsequences. Each position @c i contains an
     *  iterator to the first element in the subsequence @c i.
     *  @param beg_partition Array of size @c num_pieces + 1 that
     *  defines @c num_pieces subsequences. Each position @c i
     *  contains the rank of the first element in the subsequence @c
     *  i.
     *  @param n Sequence size
     *  @param num_pieces Number of pieces. */
    template<typename _InputIterator>
    static void
    range_accessors(const _InputIterator __first, const _InputIterator __last, _InputIterator* access,  size_type* beg_partition, const size_type n, const thread_index_t num_pieces, std::input_iterator_tag)
    {
      access[0] = __first;
      _InputIterator it= __first;
      for (int i=1; i< num_pieces; ++i)
	{
	  for (int j=0; j< n/num_pieces; ++j)
	    ++it;
	  access[i] = it;
	  beg_partition[i]= n/num_pieces + beg_partition[i-1];
	}
      access[num_pieces] = __last;
      beg_partition[num_pieces] = n - (num_pieces-1)*(n/num_pieces) + beg_partition[num_pieces-1];
    }

    /** @brief Initialize an array of concatenation problems for bulk
	insertion. They are linked as a tree with (end - beg) leaves.
     *  @param conc Array of concatenation problems pointers to initialize.
     *  @param beg Rank of the first leave to initialize
     *  @param end Rank of the last (not included) leave to initialize
     *  @param parent Pointer to the parent concatenation problem.
     */
    static concat_problem*
    _M_bulk_insertion_initialize_upper_problems(concat_problem** conc, const int beg, const int end, concat_problem* parent)
    {
      if (beg + 1 == end)
	{
	  conc[2*beg]->par_problem = parent;
	  return conc[2*beg];
	}

      int size = end - beg;
      int mid = beg + size/2;
      conc[2*mid-1]->par_problem = parent;
      conc[2*mid-1]->left_problem = _M_bulk_insertion_initialize_upper_problems(conc, beg, mid, conc[2*mid-1]);
      conc[2*mid-1]->right_problem = _M_bulk_insertion_initialize_upper_problems(conc, mid, end, conc[2*mid-1]);
      return conc[2*mid-1];
    }


    /** @brief Determine black height of a node recursively.
     *  @param t Node.
     *  @return Black height of the node. */
    static int
    black_height(const _Rb_tree_node_ptr t)
    {
      if (t == NULL) 
	return 0;
      int bh = black_height (static_cast<const _Rb_tree_node_ptr> (t->_M_left));
      if (t->_M_color == std::_S_black)
	++bh;
      return bh;
    }

    /** @brief Color a leaf black
     *  @param t Leaf pointer.
     *  @param black_h Black height of @c t (out) */
    static void
    make_black_leaf(const _Rb_tree_node_ptr t, int& black_h)
    {
      black_h = 0;
      if (t != NULL)
	{
	  _GLIBCXX_PARALLEL_ASSERT(t->_M_left == NULL and t->_M_right == NULL);
	  black_h = 1;
	  t->_M_color = std::_S_black;
	}
    }

    /** @brief Color a node black.
     *  @param t Node to color black.
     *  @param black_h Black height of @c t (out) */
    static void
    make_leaf(const _Rb_tree_node_ptr t, int& black_h)
    {
      _GLIBCXX_PARALLEL_ASSERT(t != NULL);
      black_h = 1;
      t->_M_color = std::_S_black;
      t->_M_left = NULL;
      t->_M_right = NULL;
    }

    /** @brief Construct a tree from a root, a left subtree and a
	right subtree.
     *  @param root Root of constructed tree.
     *  @param l Root of left subtree.
     *  @param r Root of right subtree.
     *  @pre @c l, @c r are black.
     */
    template<typename S>
    static _Rb_tree_node_ptr
    plant(const _Rb_tree_node_ptr root, const _Rb_tree_node_ptr l, 
	  const _Rb_tree_node_ptr r)
    {
      S::left(root) = l;
      S::right(root) = r;
      if (l != NULL)
	l->_M_parent = root;
      if (r != NULL)
	r->_M_parent = root;
      root->_M_color = std::_S_red;
      return root;
    }

    /** @brief Concatenate two red-black subtrees using and an
	intermediate node, which might be NULL
     *  @param root Intermediate node.
     *  @param l Left subtree.
     *  @param r Right subtree.
     *  @param black_h_l Black height of left subtree.
     *  @param black_h_r Black height of right subtree.
     *  @param t Tree resulting of the concatenation
     *  @param black_h Black height of the resulting tree
     *  @pre Left tree is higher than left tree
     *  @post @c t is correct red-black tree with height @c black_h.
     */
    void
    concatenate(_Rb_tree_node_ptr root, _Rb_tree_node_ptr l, 
		_Rb_tree_node_ptr r,  int black_h_l, int black_h_r, 
		_Rb_tree_node_ptr& t, int& black_h) const
    {
#ifndef NDEBUG
      int count = 0, count1 = 0, count2 = 0;
#endif
      _GLIBCXX_PARALLEL_ASSERT(rb_verify_tree(l, count1));
      _GLIBCXX_PARALLEL_ASSERT(rb_verify_tree(r, count2));

      _GLIBCXX_PARALLEL_ASSERT(l != NULL ? l->_M_color != std::_S_red and black_h_l > 0 : black_h_l == 0);
      _GLIBCXX_PARALLEL_ASSERT(r != NULL ? r->_M_color != std::_S_red and black_h_r > 0 : black_h_r == 0);

      if (black_h_l > black_h_r)
	if (root != NULL)
	  concatenate<LeftRight>(root, l, r, black_h_l, black_h_r, t, black_h);
	else
	  {
	    if (r == NULL)
	      {
		t = l;
		black_h = black_h_l;
	      }
	    else
	      {
		// XXX SHOULD BE the same as extract_min but slower.
		/*
		   root = static_cast<_Rb_tree_node_ptr>(_Rb_tree_node_base::_S_minimum(r));
		   split(r, _S_key(_Rb_tree_increment(root)), _S_key(root), root, t, r, black_h, black_h_r);
		*/
		extract_min(r, root, r, black_h_r);
		_GLIBCXX_PARALLEL_ASSERT(root != NULL);
		concatenate<LeftRight>(root, l, r, black_h_l, black_h_r, t, black_h);
	      }
	  }
      else
	if (root != NULL)
	  concatenate<RightLeft>(root, r, l, black_h_r, black_h_l, t, black_h);
	else
	  {
	    if (l == NULL)
	      {
		t = r;
		black_h = black_h_r;
	      }
	    else
	      {
		// XXX SHOULD BE the same as extract_max but slower
		/*
		   root = static_cast<_Rb_tree_node_ptr>(_Rb_tree_node_base::_S_maximum(l));
		   split(l, _S_key(root), _S_key(_Rb_tree_decrement(root)), root, l, t, black_h_l, black_h);
		*/
		extract_max(l, root, l, black_h_l);
		_GLIBCXX_PARALLEL_ASSERT(root != NULL);
		concatenate<RightLeft>(root, r, l, black_h_r, black_h_l, t, black_h);
	      }
	  }
#ifndef NDEBUG
      if (root!=NULL) ++count1;
      _GLIBCXX_PARALLEL_ASSERT(t == NULL or t->_M_color == std::_S_black);
      bool b = rb_verify_tree(t, count);
      if (not b){
	_GLIBCXX_PARALLEL_ASSERT(false);
      }
      _GLIBCXX_PARALLEL_ASSERT(count1+count2 == count);
#endif
    }

    /** @brief Concatenate two red-black subtrees using and a not NULL
     * intermediate node.
     *
     *  @c S is the symmetry parameter.
     *  @param rt Intermediate node.
     *  @param l Left subtree.
     *  @param r Right subtree.
     *  @param black_h_l Black height of left subtree.
     *  @param black_h_r Black height of right subtree.
     *  @param t Tree resulting of the concatenation
     *  @param black_h Black height of the resulting tree
     *  @pre Left tree is higher than right tree. @c rt != NULL
     *  @post @c t is correct red-black tree with height @c black_h.
     */
    template<typename S>
    static void
    concatenate(const _Rb_tree_node_ptr rt, _Rb_tree_node_ptr l, 
		_Rb_tree_node_ptr r, int black_h_l, int black_h_r, 
		_Rb_tree_node_ptr& t, int& black_h)
    {
      _Rb_tree_node_base* root = l;
      _Rb_tree_node_ptr parent = NULL;
      black_h = black_h_l;
      _GLIBCXX_PARALLEL_ASSERT(black_h_l >= black_h_r);
      while (black_h_l != black_h_r)
	{
	  if (l->_M_color == std::_S_black)
	    --black_h_l;
	  parent = l;
	  l = static_cast<_Rb_tree_node_ptr>(S::right(l));
	  _GLIBCXX_PARALLEL_ASSERT((black_h_l == 0 and (l == NULL or l->_M_color == std::_S_red)) or (black_h_l != 0 and l != NULL));
	  _GLIBCXX_PARALLEL_ASSERT((black_h_r == 0 and (r == NULL or r->_M_color == std::_S_red)) or (black_h_r != 0 and r != NULL));
	}
      if (l != NULL and l->_M_color == std::_S_red)
	{
	  //the root needs to be black
	  parent = l;
	  l = static_cast<_Rb_tree_node_ptr>(S::right(l));
	}
      _GLIBCXX_PARALLEL_ASSERT(l != NULL ? l->_M_color == std::_S_black : true);
      _GLIBCXX_PARALLEL_ASSERT(r != NULL ? r->_M_color == std::_S_black : true);
      t = plant<S>(rt, l, r);
      t->_M_parent = parent;
      if (parent != NULL)
	{
	  S::right(parent) = t;
	  black_h += _Rb_tree_rebalance(t, root);
	  t = static_cast<_Rb_tree_node_ptr> (root);
	}
      else
	{
	  ++black_h;
	  t->_M_color = std::_S_black;
	}
      _GLIBCXX_PARALLEL_ASSERT(t->_M_color == std::_S_black);
    }

    /** @brief Split a tree according to key in three parts: a left
     *  child, a right child and an intermediate node.
     *
     *  Trees are concatenated once the recursive call returns. That
     *  is, from bottom to top (i. e. smaller to larger), so the cost
     *  bounds for split hold.
     *  @param t Root of the tree to split.
     *  @param key Key to split according to.
     *  @param prev_k Key to split the intermediate node
     *  @param root Out parameter. If a node exists whose key is
     *  smaller or equal than @c key, but strictly larger than @c
     *  prev_k, this is returned. Otherwise, it is null.
     *  @param l Root of left subtree returned, nodes less than @c key.
     *  @param r Root of right subtree returned, nodes greater or
     *  equal than @c key.
     *  @param black_h_l Black height of the left subtree.
     *  @param black_h_r Black height of the right subtree.
     *  @param strictly_less_or_less_equal Comparator to deal
     *  transparently with repetitions with respect to the uniqueness
     *  of the wrapping container
     *  @return Black height of t */
    template<typename StrictlyLessOrEqual>
    int
    split(_Rb_tree_node_ptr t, const key_type& key, const key_type& prev_k, 
	  _Rb_tree_node_ptr& root, _Rb_tree_node_ptr& l, _Rb_tree_node_ptr& r, 
	  int& black_h_l, int& black_h_r, 
	  StrictlyLessOrEqual strictly_less_or_less_equal) const
    {
      if (t != NULL)
	{
	  // Must be initialized, in case we never go left!!!
	  root = NULL;
	  int h = split_not_null(t, key, prev_k, root, l, r, black_h_l, black_h_r, strictly_less_or_less_equal);
#ifndef NDEBUG
	  _GLIBCXX_PARALLEL_ASSERT(l == NULL or base_type::_M_impl._M_key_compare(base_type::_S_key(base_type::_S_maximum(l)),key));
	  _GLIBCXX_PARALLEL_ASSERT(r == NULL or not base_type::_M_impl._M_key_compare(base_type::_S_key(base_type::_S_minimum(r)),key));
	  int count1, count2;
	  _GLIBCXX_PARALLEL_ASSERT(rb_verify_tree(l, count1));
	  _GLIBCXX_PARALLEL_ASSERT(rb_verify_tree(r, count2));
	  _GLIBCXX_PARALLEL_ASSERT(root == NULL or base_type::_M_impl._M_key_compare(prev_k, base_type::_S_key(root)) and not base_type::_M_impl._M_key_compare(key, base_type::_S_key(root)));
	  _GLIBCXX_PARALLEL_ASSERT(root != NULL or l==NULL or  not base_type::_M_impl._M_key_compare(prev_k, base_type::_S_key(base_type::_S_maximum(l))));
#endif
	  return h;
	}

      r = NULL;
      root = NULL;
      l = NULL;
      black_h_r = 0;
      black_h_l = 0;
      return 0;
    }

    /** @brief Split a tree according to key in three parts: a left
     * child, a right child and an intermediate node.
     *
     *  @param t Root of the tree to split.
     *  @param key Key to split according to.
     *  @param prev_k Key to split the intermediate node
     *  @param root Out parameter. If a node exists whose key is
     *  smaller or equal than @c key, but strictly larger than @c
     *  prev_k, this is returned. Otherwise, it is null.
     *  @param l Root of left subtree returned, nodes less than @c key.
     *  @param r Root of right subtree returned, nodes greater or
     *  equal than @c key.
     *  @param black_h_l Black height of the left subtree.
     *  @param black_h_r Black height of the right subtree.
     *  @param strictly_less_or_equal Comparator to deal transparently
     *  with repetitions with respect to the uniqueness of the
     *  wrapping container
     *  @pre t != NULL
     *  @return Black height of t */
    template<typename StrictlyLessOrEqual>
    int
    split_not_null(const _Rb_tree_node_ptr t, const key_type& key, 
		   const key_type& prev_k, _Rb_tree_node_ptr& root, 
		   _Rb_tree_node_ptr& l, _Rb_tree_node_ptr& r, int& black_h_l, 
		   int& black_h_r, 
		   StrictlyLessOrEqual strictly_less_or_equal) const
    {
      _GLIBCXX_PARALLEL_ASSERT (t != NULL);
      int black_h, b_h;
      int black_node = 0;
      if (t->_M_color == std::_S_black)
	++black_node;
      if (strictly_less_or_equal(key, base_type::_S_key(t)))
	{
	  if (t->_M_left != NULL )
	    {
	      // t->M_right is at most one node
	      // go to the left
	      b_h = black_h = split_not_null( static_cast<_Rb_tree_node_ptr>(t->_M_left), key, prev_k, root, l, r, black_h_l, black_h_r, strictly_less_or_equal);
	      // Moin root and right subtree to already existing right
	      // half, leave left subtree.
	      force_black_root(t->_M_right, b_h);
	      concatenate(t, r, static_cast<_Rb_tree_node_ptr>(t->_M_right), black_h_r, b_h, r, black_h_r);
	    }
	  else
	    {
	      // t->M_right is at most one node
	      r = t;
	      black_h_r = black_node;
	      force_black_root(r, black_h_r);

	      black_h = 0;
	      l = NULL;
	      black_h_l = 0;
	    }
	  _GLIBCXX_PARALLEL_ASSERT(l == NULL or base_type::_M_impl._M_key_compare(base_type::_S_key(base_type::_S_maximum(l)),key));
	  _GLIBCXX_PARALLEL_ASSERT(r == NULL or not base_type::_M_impl._M_key_compare(base_type::_S_key(base_type::_S_minimum(r)),key));
	}
      else
	{
	  if (t->_M_right != NULL )
	    {
	      // Go to the right.
	      if (strictly_less_or_equal(prev_k, base_type::_S_key(t)))
		root = t;
	      b_h = black_h = split_not_null(static_cast<_Rb_tree_node_ptr>(t->_M_right), key, prev_k, root, l, r, black_h_l, black_h_r, strictly_less_or_equal);
	      // Join root and left subtree to already existing left
	      // half, leave right subtree.
	      force_black_root(t->_M_left, b_h);
	      if (root != t)
		{
		  // There was another point where we went right.
		  concatenate(t, static_cast<_Rb_tree_node_ptr>(t->_M_left), l, b_h, black_h_l, l, black_h_l);
		}
	      else
		{
		  l = static_cast<_Rb_tree_node_ptr>(t->_M_left);
		  black_h_l = b_h;
		}
	      _GLIBCXX_PARALLEL_ASSERT(l == NULL or base_type::_M_impl._M_key_compare(base_type::_S_key(base_type::_S_maximum(l)),key));
	      _GLIBCXX_PARALLEL_ASSERT(r == NULL or not base_type::_M_impl._M_key_compare(base_type::_S_key(base_type::_S_minimum(r)),key));
	    }
	  else
	    {
	      if (strictly_less_or_equal(prev_k, base_type::_S_key(t)))
		{
		  root = t;
		  l= static_cast<_Rb_tree_node_ptr>(t->_M_left);
		  make_black_leaf(l, black_h_l);
		  _GLIBCXX_PARALLEL_ASSERT(l == NULL or base_type::_M_impl._M_key_compare(base_type::_S_key(base_type::_S_maximum(l)),key));
		}
	      else
		{
		  l= t;
		  black_h_l = black_node;
		  force_black_root(l, black_h_l);
		  _GLIBCXX_PARALLEL_ASSERT(l == NULL or base_type::_M_impl._M_key_compare(base_type::_S_key(base_type::_S_maximum(l)),key));
		}

	      r = NULL;
	      black_h = 0;
	      black_h_r = 0;
	    }
	}
      return black_h + black_node;
    }

    /** @brief Color the root black and update the black height accordingly.
     *
     * @param t Root of the tree.
     * @param black_h Black height of the tree @c t (out) */
    static void force_black_root(_Rb_tree_node_base* t, int& black_h)
    {
      if (t != NULL and t->_M_color == std::_S_red)
	{
	  t->_M_color = std::_S_black;
	  ++ black_h;
	}
    }

    /** @brief Split the tree in two parts: the minimum element from a
	tree (i. e. leftmost) and the rest (right subtree)
     *  @param t Root of the tree
     *  @param root Minimum element (out)
     *  @param r Right subtree: @c t - {@c root}
     *  @param black_h_r Black height of the right subtree.
     *  @return Black height of the original tree  */
    int
    extract_min(const _Rb_tree_node_ptr t, _Rb_tree_node_ptr& root, 
		_Rb_tree_node_ptr& r, int& black_h_r) const
    {
      _GLIBCXX_PARALLEL_ASSERT (t != NULL);
      int black_h, b_h;
      int black_node = 0;
      if (t->_M_color == std::_S_black)
	++black_node;

      if (t->_M_left != NULL )
	{
	  // t->M_right is at most one node
	  // go to the left
	  b_h = black_h = extract_min( static_cast<_Rb_tree_node_ptr>(t->_M_left), root, r, black_h_r);

	  // Join root and right subtree to already existing right
	  // half, leave left subtree
	  force_black_root(t->_M_right, b_h);
	  concatenate(t, r, static_cast<_Rb_tree_node_ptr>(t->_M_right), black_h_r, b_h, r, black_h_r);
	}
      else
	{
	  // t->M_right is at most one node
	  root = t;
	  if (t->_M_right == NULL)
	    {
	      r = NULL;
	      black_h_r = 0;
	    }
	  else
	    {
	      r = static_cast<_Rb_tree_node_ptr>(t->_M_right);
	      black_h_r = 1;
	      r->_M_color = std::_S_black;
	    }
	  black_h = 0;
	}
      return black_h + black_node;
    }


    /** @brief Split the tree in two parts: the greatest element from
	a tree (i. e. rightmost) and the rest (left subtree)
     *  @param t Root of the tree
     *  @param root Maximum element (out)
     *  @param l Left subtree: @c t - {@c root}
     *  @param black_h_l Black height of the left subtree.
     *  @return Black height of the original tree  */
    int
    extract_max(const _Rb_tree_node_ptr t, _Rb_tree_node_ptr& root, 
		_Rb_tree_node_ptr& l, int& black_h_l) const
    {
      _GLIBCXX_PARALLEL_ASSERT (t != NULL);
      int black_h, b_h;
      int black_node = 0;
      if (t->_M_color == std::_S_black)
	++black_node;

      if (t->_M_right != NULL )
	{
	  b_h = black_h = extract_max(static_cast<_Rb_tree_node_ptr>(t->_M_right), root, l,  black_h_l);

	  // Join root and left subtree to already existing left half,
	  // leave right subtree.
	  force_black_root(t->_M_left, b_h);

	  concatenate(t, static_cast<_Rb_tree_node_ptr>(t->_M_left), l, b_h, black_h_l, l, black_h_l);
	}
      else
	{
	  root = t;
	  if (t->_M_left == NULL)
	    {
	      l = NULL;
	      black_h_l = 0;
	    }
	  else
	    {
	      l = static_cast<_Rb_tree_node_ptr>(t->_M_left);
	      black_h_l = 1;
	      l->_M_color = std::_S_black;
	    }
	  black_h = 0;
	}
      return black_h + black_node;
    }

    /** @brief Split tree according to key in two parts: a left tree
     * and a right subtree
     *
     *  Trees are concatenated once the recursive call returns. That
     *  is, from bottom to top (i. e. smaller to larger), so the cost
     *  bounds for split hold.
     *  @param t Root of the tree to split.
     *  @param key Key to split according to.
     *  @param l Root of left subtree returned, nodes less than @c key.
     *  @param r Root of right subtree returned, nodes greater than @c key.
     *  @param black_h_l Black height of the left subtree.
     *  @param black_h_r Black height of the right subtree.
     *  @return Black height of the original tree */
    int
    split(const _Rb_tree_node_ptr t, const key_type& key, 
	  _Rb_tree_node_ptr& l, _Rb_tree_node_ptr& r, int& black_h_l, 
	  int& black_h_r) const
    {
      if (t != NULL)
	{
	  int black_h, b_h;
	  int black_node = 0;
	  if (t->_M_color == std::_S_black)
	    ++black_node;
	  if (not (base_type::_M_impl._M_key_compare(base_type::_S_key(t), key)))
	    {
	      // Go to the left.
	      b_h = black_h = split( static_cast<_Rb_tree_node_ptr>(t->_M_left), key, l, r, black_h_l, black_h_r);

	      // Join root and right subtree to already existing right
	      // half, leave left subtree.
	      force_black_root(t->_M_right, b_h);
	      concatenate(t, r, static_cast<_Rb_tree_node_ptr>(t->_M_right), black_h_r, b_h, r, black_h_r);
	    }
	  else
	    {
	      // Go to the right.
	      b_h = black_h = split(static_cast<_Rb_tree_node_ptr>(t->_M_right), key, l, r, black_h_l, black_h_r);

	      // Join root and left subtree to already existing left
	      // half, leave right subtree.
	      force_black_root(t->_M_left, b_h);
	      concatenate(t, static_cast<_Rb_tree_node_ptr>(t->_M_left), l, b_h, black_h_l, l, black_h_l);
	    }
	  return black_h + black_node;
	}
      else
	{
	  r = NULL;
	  l = NULL;
	  black_h_r = 0;
	  black_h_l = 0;
	  return 0;
	}
    }

    /** @brief Insert an existing node in tree and rebalance it, if
     * appropriate.
     *
     *  The keyword "local" is used because no attributes of the
     *  red-black tree are changed, so this insertion is not yet seen
     *  by the global data structure.
     *  @param t Root of tree to insert into.
     *  @param new_t Existing node to insert.
     *  @param existing Number of existing elements before insertion
     *  (in) and after (out). Specifically, the counter is incremented
     *  by one for unique containers if the key of new_t was already
     *  in the tree.
     *  @param black_h Black height of the resulting tree (out)
     *  @param strictly_less_or_less_equal Comparator to deal
     *  transparently with repetitions with respect to the uniqueness
     *  of the wrapping container
     *  @return Resulting tree after insertion */
    template<typename StrictlyLessOrLessEqual>
    _Rb_tree_node_ptr
    _M_insert_local(_Rb_tree_node_base* t, const _Rb_tree_node_ptr new_t, 
		    size_type& existing, int& black_h, 
		    StrictlyLessOrLessEqual strictly_less_or_less_equal)
    {
      _GLIBCXX_PARALLEL_ASSERT(t != NULL);
      if (_M_insert_local_top_down(t, new_t, NULL, NULL, true, strictly_less_or_less_equal))
	{
	  t->_M_parent = NULL;
	  black_h += _Rb_tree_rebalance(new_t, t);
	  _GLIBCXX_PARALLEL_ASSERT(t->_M_color == std::_S_black);
	  return static_cast<_Rb_tree_node_ptr>(t);
	}
      else
	{
	  base_type::_M_destroy_node(new_t);
	  ++existing;
	  force_black_root(t, black_h);
	  return static_cast<_Rb_tree_node_ptr>(t);
	}
    }

    /*****	Dealing with repetitions (CORRECTNESS ISSUE) *****/
    /** @brief Insert an existing node in tree, do no rebalancing.
     *  @param t Root of tree to insert into.
     *  @param new_t Existing node to insert.
     *  @param eq_t Node candidate to be equal than new_t, only
     *  relevant for unique containers
     *  @param parent Parent node of @c t
     *  @param is_left True if @c t is a left child of @c
     *  parent. False otherwise.
     *  @param strictly_less_or_less_equal Comparator to deal
     *  transparently with repetitions with respect to the uniqueness
     *  of the wrapping container

     *  @return Success of the insertion 
     */
    template<typename StrictlyLessOrLessEqual>
    bool
    _M_insert_local_top_down(_Rb_tree_node_base* t, 
			     const _Rb_tree_node_ptr new_t, 
			     _Rb_tree_node_base* eq_t, 
			     _Rb_tree_node_base* parent, const bool is_left, 
		    StrictlyLessOrLessEqual strictly_less_or_less_equal) const
    {
      if (t != NULL)
	{
	  if (strictly_less_or_less_equal(_S_key(new_t), _S_key(static_cast<_Rb_tree_node_ptr>(t))))
	    {
	      return _M_insert_local_top_down(t->_M_left, new_t, eq_t, t, true, strictly_less_or_less_equal);
	    }
	  else
	    {
	      return _M_insert_local_top_down(t->_M_right, new_t, t, t, false, strictly_less_or_less_equal);
	    }
	}

      _GLIBCXX_PARALLEL_ASSERT(parent != NULL);

      // Base case.
      if (eq_t == NULL or strictly_less_or_less_equal(_S_key(static_cast<_Rb_tree_node_ptr>(eq_t)), _S_key(new_t)))
	{
	  // The element to be inserted did not existed.
	  if (is_left)
	    {
	      parent->_M_left = new_t;
	    }
	  else
	    {
	      parent->_M_right = new_t;
	    }

	  new_t->_M_parent = parent;
	  new_t->_M_left = NULL;
	  new_t->_M_right = NULL;
	  new_t->_M_color = std::_S_red;

	  return true;
	}
      else
	return false;
    }

    /** @brief Rebalance a tree locally.
     *
     *  Essentially, it is the same function as insert_erase from the
     *  base class, but without the insertion and without using any
     *  tree attributes.
     *  @param __x Root of the current subtree to rebalance.
     *  @param __root Root of tree where @c __x is in (rebalancing
     *  stops when root is reached)
     *  @return Increment in the black height after rebalancing
     */
    static int
    _Rb_tree_rebalance(_Rb_tree_node_base* __x, _Rb_tree_node_base*& __root)
    {
      _GLIBCXX_PARALLEL_ASSERT(__root->_M_color == std::_S_black);
      // Rebalance.
      while (__x != __root and __x->_M_parent != __root and
	     __x->_M_parent->_M_color == std::_S_red)
	{
	  _Rb_tree_node_base* const __xpp = __x->_M_parent->_M_parent;

	  if (__x->_M_parent == __xpp->_M_left)
	    {
	      _Rb_tree_node_base* const __y = __xpp->_M_right;
	      if (__y && __y->_M_color == std::_S_red)
		{
		  __x->_M_parent->_M_color = std::_S_black;
		  __y->_M_color = std::_S_black;
		  __xpp->_M_color = std::_S_red;
		  __x = __xpp;
		}
	      else
		{
		  if (__x == __x->_M_parent->_M_right)
		    {
		      __x = __x->_M_parent;
		      std::_Rb_tree_rotate_left(__x, __root);
		    }
		  __x->_M_parent->_M_color = std::_S_black;
		  __xpp->_M_color = std::_S_red;
		  std::_Rb_tree_rotate_right(__xpp, __root);
		}
	    }
	  else
	    {
	      _Rb_tree_node_base* const __y = __xpp->_M_left;
	      if (__y && __y->_M_color == std::_S_red)
		{
		  __x->_M_parent->_M_color = std::_S_black;
		  __y->_M_color = std::_S_black;
		  __xpp->_M_color = std::_S_red;
		  __x = __xpp;
		}
	      else
		{
		  if (__x == __x->_M_parent->_M_left)
		    {
		      __x = __x->_M_parent;
		      std::_Rb_tree_rotate_right(__x, __root);
		    }
		  __x->_M_parent->_M_color = std::_S_black;
		  __xpp->_M_color = std::_S_red;
		  std::_Rb_tree_rotate_left(__xpp, __root);
		}
	    }
	}
      if (__root->_M_color == std::_S_red)
	{
	  __root->_M_color = std::_S_black;
	  _GLIBCXX_PARALLEL_ASSERT(rb_verify_tree(static_cast<typename base_type::_Const_Link_type>(__root)));
	  return 1;
	}
      _GLIBCXX_PARALLEL_ASSERT(rb_verify_tree(static_cast<typename base_type::_Const_Link_type>(__root)));
      return 0;
    }

    /** @brief Analogous to class method rb_verify() but only for a subtree.
     *  @param __x Pointer to root of subtree to check.
     *  @param count Returned number of nodes.
     *  @return Tree correct. 
     */
    bool
    rb_verify_tree(const typename base_type::_Const_Link_type __x, int& count) const
    {
      int bh;
      return rb_verify_tree_node(__x) and rb_verify_tree(__x, count, bh);
    }

    /** @brief Verify that a subtree is binary search tree (verifies
	key relationships)
     *  @param __x Pointer to root of subtree to check.
     *  @return Tree correct. 
     */
    bool
    rb_verify_tree_node(const typename base_type::_Const_Link_type __x) const
    {
      if (__x == NULL)
	return true;
      else
	{
	  return rb_verify_node(__x) and
	    rb_verify_tree_node(base_type::_S_left(__x)) and
	    rb_verify_tree_node( base_type::_S_right(__x));
	}
    }

    /** @brief Verify all the properties of a red-black tree except
	for the key ordering
     *  @param __x Pointer to (subtree) root node.
     *  @return Tree correct. 
     */
    static  bool
    rb_verify_tree(const typename base_type::_Const_Link_type __x)
    {
      int bh, count;
      return rb_verify_tree(__x, count, bh);
    }

    /** @brief Verify all the properties of a red-black tree except
	for the key ordering
     *  @param __x Pointer to (subtree) root node.
     *  @param count Number of nodes of @c __x (out).
     *  @param black_h Black height of @c __x (out).
     *  @return Tree correct. 
     */
    static bool
    rb_verify_tree(const typename base_type::_Const_Link_type __x, int& count, 
		   int& black_h)
    {
      if (__x == NULL)
	{
	  count = 0;
	  black_h = 0;
	  return true;
	}
      typename base_type::_Const_Link_type __L = base_type::_S_left(__x);
      typename base_type::_Const_Link_type __R = base_type::_S_right(__x);
      int countL, countR = 0, bhL, bhR;
      bool ret = rb_verify_tree(__L, countL, bhL);
      ret = ret and rb_verify_tree(__R, countR, bhR);
      count = 1 + countL + countR;
      ret = ret and bhL == bhR;
      black_h = bhL + ((__x->_M_color == std::_S_red)? 0 : 1);
      return ret;
    }

    /** @brief Verify red-black properties (including key based) for a node
     *  @param __x Pointer to node.
     *  @return Node correct. 
     */
    bool
    rb_verify_node(const typename base_type::_Const_Link_type __x) const
    {
      typename base_type::_Const_Link_type __L = base_type::_S_left(__x);
      typename base_type::_Const_Link_type __R = base_type::_S_right(__x);
      if (__x->_M_color == std::_S_red)
	if ((__L && __L->_M_color == std::_S_red)
	    || (__R && __R->_M_color == std::_S_red))
	  {
	    return false;
	  }
      if (__L != NULL)
	{
	  __L = static_cast<typename base_type::_Const_Link_type>(base_type::_S_maximum(__L));
	  if (base_type::_M_impl._M_key_compare(base_type::_S_key(__x), base_type::_S_key(__L)))
	    {
	      return false;
	    }
	}

      if (__R != NULL)
	{
	  __R = static_cast<typename base_type::_Const_Link_type>(base_type::_S_minimum(__R));
	  if (base_type::_M_impl._M_key_compare(base_type::_S_key(__R), base_type::_S_key(__x)))
	    {
	      return false;
	    }
	}

      return true;
    }

    /** @brief Print all the information of the root.
     *  @param t Root of the tree. 
     */
    static void
    print_root(_Rb_tree_node_base* t)
    {
      /*
       if (t != NULL)
       std::cout<< base_type::_S_key(t) << std::endl;
       else
       std::cout<< "NULL" << std::endl;
      */
    }

    /** @brief Print all the information of the tree.
     *  @param t Root of the tree. 
     */
    static void
    print_tree(_Rb_tree_node_base* t)
    {
      /*
       if (t != NULL)
       {
       print_tree(t->_M_left);
       std::cout<< base_type::_S_key(t) << std::endl;
       print_tree(t->_M_right);
       }
      */
    }

    /** @brief Print blanks.
     *  @param b Number of blanks to print.
     *  @return A string with @c b blanks */
    inline static std::string
    blanks(int b)
    {
      /*
       std::string s = "";
       for (int i=0; i < b; ++i)
       s += " ";
       return s;
      */
    }

    /** @brief Print all the information of the tree.
     *  @param t Root of the tree.
     *  @param c Width of a printed key. 
     */
    template<typename Pointer>
    static void
    draw_tree(Pointer t, const int c)
    {
      /*
       if (t == NULL)
       {
       std::cout << blanks(c) << "NULL" << std::endl;
       return;
       }
       draw_tree(static_cast<Pointer>(t->_M_right), c + 8);
       std::cout << blanks(c) << "" << base_type::_S_key(t) << " ";
       if (t->_M_color == std::_S_black)
       std::cout << "B" << std::endl;
       else
       std::cout << "R" << std::endl;
       draw_tree(static_cast<Pointer>(t->_M_left), c + 8);
      */
    }

  public:
    /** @brief Verify that all the red-black tree properties hold for
	the stored tree, as well as the additional properties that the
	STL implementation imposes.
     */
    bool
    rb_verify()
    {
      if (base_type::_M_impl._M_node_count == 0 || base_type::begin() == base_type::end())
	{
	  bool res = base_type::_M_impl._M_node_count == 0 && base_type::begin() == base_type::end()
	    && base_type::_M_impl._M_header._M_left ==base_type::_M_end()
	    && base_type::_M_impl._M_header._M_right == base_type::_M_end();
	  _GLIBCXX_PARALLEL_ASSERT(res);
	  return res;
	}
      size_type i=0;
      unsigned int __len = _Rb_tree_black_count(base_type::_M_leftmost(), base_type::_M_root());
      for (typename base_type::const_iterator __it =base_type::begin(); __it != base_type::end(); ++__it)
	{
	  typename base_type::_Const_Link_type __x = static_cast<typename base_type::_Const_Link_type>(__it._M_node);
	  if (not rb_verify_node(__x)) return false;
	  if (!base_type::_S_left(__x)&& !base_type::_S_right(__x) && _Rb_tree_black_count(__x,base_type::_M_root()) != __len)
	    {
	      _GLIBCXX_PARALLEL_ASSERT(false);
	      return false;
	    }
	  ++i;
	}

      if (i != base_type::_M_impl._M_node_count)
	printf("%ld != %ld\n", i, base_type::_M_impl._M_node_count);

      if (base_type::_M_leftmost() != std::_Rb_tree_node_base::_S_minimum(base_type::_M_root()))
	{
	  _GLIBCXX_PARALLEL_ASSERT(false);
	  return false;
	}
      if (base_type::_M_rightmost() != std::_Rb_tree_node_base::_S_maximum(base_type::_M_root()))
	{
	  _GLIBCXX_PARALLEL_ASSERT(false);
	  return false;
	}
      _GLIBCXX_PARALLEL_ASSERT(i == base_type::_M_impl._M_node_count);
      return true;
    }
  };

}

#endif
