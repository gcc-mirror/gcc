/*
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1996,1997
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

#ifndef _CPP_BACKWARD_ALGO_H
#define _CPP_BACKWARD_ALGO_H 1

#include "backward_warning.h"
#include "algobase.h"
#include "tempbuf.h"
#include "iterator.h"
#include <bits/stl_algo.h>
#include <bits/stl_numeric.h>

// Names from <stl_algo.h>
using std::for_each; 
using std::find; 
using std::find_if; 
using std::adjacent_find; 
using std::count; 
using std::count_if; 
using std::search; 
using std::search_n; 
using std::swap_ranges; 
using std::transform; 
using std::replace; 
using std::replace_if; 
using std::replace_copy; 
using std::replace_copy_if; 
using std::generate; 
using std::generate_n; 
using std::remove; 
using std::remove_if; 
using std::remove_copy; 
using std::remove_copy_if; 
using std::unique; 
using std::unique_copy; 
using std::reverse; 
using std::reverse_copy; 
using std::rotate; 
using std::rotate_copy; 
using std::random_shuffle; 
using std::random_sample; 
using std::random_sample_n; 
using std::partition; 
using std::stable_partition; 
using std::sort; 
using std::stable_sort; 
using std::partial_sort; 
using std::partial_sort_copy; 
using std::nth_element; 
using std::lower_bound; 
using std::upper_bound; 
using std::equal_range; 
using std::binary_search; 
using std::merge; 
using std::inplace_merge; 
using std::includes; 
using std::set_union; 
using std::set_intersection; 
using std::set_difference; 
using std::set_symmetric_difference; 
using std::min_element; 
using std::max_element; 
using std::next_permutation; 
using std::prev_permutation; 
using std::find_first_of; 
using std::find_end; 
using std::is_sorted; 
using std::is_heap; 

// Names from stl_heap.h
using std::push_heap;
using std::pop_heap;
using std::make_heap;
using std::sort_heap;

// Names from stl_numeric.h
using std::accumulate; 
using std::inner_product; 
using std::partial_sum; 
using std::adjacent_difference; 
using std::power; 
using std::iota; 

#endif /* _CPP_BACKWARD_ALGO_H */

// Local Variables:
// mode:C++
// End:
