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

/** @file parallel/settings.h
 *  @brief Settings and tuning parameters, heuristics to decide
 *  whether to use parallelized algorithms.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 *
 *  @section parallelization_decision  The decision whether to run an algorithm in parallel.
 *
 *  There are several ways the user can switch on and off the 
 *  parallel execution of an algorithm, both at compile- and 
 *  run-time.
 *
 *  Only sequential execution can be forced at compile-time.
 *  This reduces code size and protects code parts that have 
 *  non-thread-safe side effects.
 *
 *  Ultimately forcing parallel execution at compile-time does 
 *  make much sense.
 *  Often, the sequential algorithm implementation is used as
 *  a subroutine, so no reduction in code size can be achieved.
 *  Also, the machine the program is run on might have only one
 *  processor core, so to avoid overhead, the algorithm is
 *  executed sequentially.
 *
 *  To force sequential execution of an algorithm ultimately
 *  at compile-time, the user must add the tag 
 *  __gnu_parallel::sequential_tag() to the end of the
 *  parameter list, e. g.
 *
 *  \code
 *  std::sort(v.begin(), v.end(), __gnu_parallel::sequential_tag());
 *  \endcode
 *
 *  This is compatible with all overloaded algorithm variants.
 *  No additional code will be instantiated, at all.
 *  The same holds for most algorithm calls with iterators 
 *  not providing random access.
 *
 *  If the algorithm call is not forced to be executed sequentially
 *  at compile-time, the decision is made at run-time, for each call.
 *  First, the two (conceptually) global variables 
 *  __gnu_parallel::Settings::force_sequential and 
 *  __gnu_parallel::Settings::force_parallel are executed.
 *  If the former one is true, the sequential algorithm is executed.
 *  If the latter one is true and the former one is false, 
 *  the algorithm is executed in parallel.
 *
 *  If none of these conditions has fired so far, a heuristic is used.
 *  The parallel algorithm implementation is called only if the
 *  input size is sufficiently large.
 *  For most algorithms, the input size is the (combined) length of 
 *  the input sequence(s).
 *  The threshold can be set by the user, individually for each
 *  algorithm.
 *  The according variables are called 
 *  __gnu_parallel::Settings::[algorithm]_minimal_n .
 *
 *  For some of the algorithms, there are even more tuning options,
 *  e. g. the ability to choose from multiple algorithm variants.
 *  See the __gnu_parallel::Settings class for details.
 */

// Written by Johannes Singler and Felix Putze.

#ifndef _GLIBCXX_PARALLEL_SETTINGS_H
#define _GLIBCXX_PARALLEL_SETTINGS_H 1

#include <omp.h>
#include <parallel/types.h>

/** 
  * @brief The extensible condition on whether the parallel variant of
  * an algorithm sould be called.
  * @param c A condition that is overruled by
  * __gnu_parallel::Settings::force_parallel, i. e. usually a decision based on
  * the input size.
  */
#define _GLIBCXX_PARALLEL_CONDITION(c) (!(__gnu_parallel::Settings::force_sequential) && ((__gnu_parallel::get_max_threads() > 1 && (c)) || __gnu_parallel::Settings::force_parallel))

namespace __gnu_parallel
{
  // NB: Including this file cannot produce (unresolved) symbols from
  // the OpenMP runtime unless the parallel mode is actually invoked
  // and active, which imples that the OpenMP runtime is actually
  // going to be linked in.
  inline int
  get_max_threads() 
  { return omp_get_max_threads() > 1 ? omp_get_max_threads() : 1; }

namespace 
{
  // XXX look at _Tune in mt_allocator.h
  /** @brief Run-time settings for the parallel mode. */
  struct Settings
  {
    /** @brief Different parallel sorting algorithms to choose
	from: multi-way mergesort, quicksort, load-balanced
	quicksort. */
    enum SortAlgorithm 
    { MWMS, QS, QS_BALANCED };

    /** @brief Different merging algorithms: bubblesort-alike,
	loser-tree variants, enum sentinel */
    enum MultiwayMergeAlgorithm
    { BUBBLE, LOSER_TREE_EXPLICIT, LOSER_TREE, LOSER_TREE_COMBINED, LOSER_TREE_SENTINEL, MWM_ALGORITHM_LAST };

    /** @brief Different splitting strategies for sorting/merging:
	by sampling, exact */
    enum Splitting 
    { SAMPLING, EXACT };

    /** @brief Different partial sum algorithms: recursive, linear */
    enum PartialSumAlgorithm 
    { RECURSIVE, LINEAR };

    /** @brief Different find distribution strategies: growing
	blocks, equal-sized blocks, equal splitting. */
    enum FindDistribution 
    { GROWING_BLOCKS, CONSTANT_SIZE_BLOCKS, EQUAL_SPLIT };

    /** @brief Force all algorithms to be executed sequentially.
     * This setting cannot be overwritten. */
    static volatile bool force_sequential;

    /** @brief Force all algorithms to be executed in parallel.
     * This setting can be overridden by __gnu_parallel::sequential_tag
     * (compile-time), and force_sequential (run-time). */
    static volatile bool force_parallel;

    /** @brief Algorithm to use for sorting. */
    static volatile SortAlgorithm sort_algorithm;

    /** @brief Strategy to use for splitting the input when
	sorting (MWMS). */
    static volatile Splitting sort_splitting;

    /** @brief Minimal input size for parallel sorting. */
    static volatile sequence_index_t sort_minimal_n;

    /** @brief Oversampling factor for parallel std::sort (MWMS). */
    static volatile unsigned int sort_mwms_oversampling;

    /** @brief Such many samples to take to find a good pivot
	(quicksort). */
    static volatile unsigned int sort_qs_num_samples_preset;

    /** @brief Maximal subsequence length to switch to unbalanced
     * base case.  Applies to std::sort with dynamically
     * load-balanced quicksort. */
    static volatile sequence_index_t sort_qsb_base_case_maximal_n;

    /** @brief Minimal input size for parallel std::partition. */
    static volatile sequence_index_t partition_minimal_n;

    /** @brief Chunk size for parallel std::partition. */
    static volatile sequence_index_t partition_chunk_size;

    /** @brief Chunk size for parallel std::partition, relative to
     * input size.  If >0.0, this value overrides
     * partition_chunk_size. */
    static volatile double partition_chunk_share;

    /** @brief Minimal input size for parallel std::nth_element. */
    static volatile sequence_index_t nth_element_minimal_n;

    /** @brief Minimal input size for parallel std::partial_sort. */
    static volatile sequence_index_t partial_sort_minimal_n;

    /** @brief Minimal input size for parallel std::adjacent_difference. */
    static volatile unsigned int adjacent_difference_minimal_n;

    /** @brief Minimal input size for parallel std::partial_sum. */
    static volatile unsigned int partial_sum_minimal_n;

    /** @brief Algorithm to use for std::partial_sum. */
    static volatile PartialSumAlgorithm partial_sum_algorithm;

    /** @brief Assume "sum and write result" to be that factor
     *  slower than just "sum".  This value is used for
     *  std::partial_sum. */
    static volatile float partial_sum_dilatation;

    /** @brief Minimal input size for parallel std::random_shuffle. */
    static volatile unsigned int random_shuffle_minimal_n;

    /** @brief Minimal input size for parallel std::merge. */
    static volatile sequence_index_t merge_minimal_n;

    /** @brief Splitting strategy for parallel std::merge. */
    static volatile Splitting merge_splitting;

    /** @brief Oversampling factor for parallel std::merge.
     * Such many samples per thread are collected. */
    static volatile unsigned int merge_oversampling;

    /** @brief Algorithm to use for parallel
	__gnu_parallel::multiway_merge. */
    static volatile MultiwayMergeAlgorithm multiway_merge_algorithm;

    /** @brief Splitting strategy to use for parallel
	__gnu_parallel::multiway_merge. */
    static volatile Splitting multiway_merge_splitting;

    //// Oversampling factor for parallel __gnu_parallel::multiway_merge.
    static volatile unsigned int multiway_merge_oversampling;

    /// Minimal input size for parallel __gnu_parallel::multiway_merge.
    static volatile sequence_index_t multiway_merge_minimal_n;

    /// Oversampling factor for parallel __gnu_parallel::multiway_merge.
    static volatile int multiway_merge_minimal_k;

    /** @brief Minimal input size for parallel std::unique_copy. */
    static volatile sequence_index_t unique_copy_minimal_n;

    static volatile sequence_index_t workstealing_chunk_size;

    /** @brief Minimal input size for parallel std::for_each. */
    static volatile sequence_index_t for_each_minimal_n;

    /** @brief Minimal input size for parallel std::count and
	std::count_if. */
    static volatile sequence_index_t count_minimal_n;

    /** @brief Minimal input size for parallel std::transform. */
    static volatile sequence_index_t transform_minimal_n;

    /** @brief Minimal input size for parallel std::replace and
	std::replace_if. */
    static volatile sequence_index_t replace_minimal_n;

    /** @brief Minimal input size for parallel std::generate. */
    static volatile sequence_index_t generate_minimal_n;

    /** @brief Minimal input size for parallel std::fill. */
    static volatile sequence_index_t fill_minimal_n;

    /** @brief Minimal input size for parallel std::min_element. */
    static volatile sequence_index_t min_element_minimal_n;

    /** @brief Minimal input size for parallel std::max_element. */
    static volatile sequence_index_t max_element_minimal_n;

    /** @brief Minimal input size for parallel std::accumulate. */
    static volatile sequence_index_t accumulate_minimal_n;

    /** @brief Distribution strategy for parallel std::find. */
    static volatile FindDistribution find_distribution;

    /** @brief Start with looking for that many elements
	sequentially, for std::find. */
    static volatile sequence_index_t find_sequential_search_size;

    /** @brief Initial block size for parallel std::find. */
    static volatile sequence_index_t find_initial_block_size;

    /** @brief Maximal block size for parallel std::find. */
    static volatile sequence_index_t find_maximum_block_size;

    /** @brief Block size increase factor for parallel std::find. */
    static volatile double find_increasing_factor;

    //set operations
    /** @brief Minimal input size for parallel std::set_union. */
    static volatile sequence_index_t set_union_minimal_n;

    /** @brief Minimal input size for parallel
	std::set_symmetric_difference. */
    static volatile sequence_index_t set_symmetric_difference_minimal_n;

    /** @brief Minimal input size for parallel std::set_difference. */
    static volatile sequence_index_t set_difference_minimal_n;

    /** @brief Minimal input size for parallel std::set_intersection. */
    static volatile sequence_index_t set_intersection_minimal_n;

    //hardware dependent tuning parameters
    /** @brief Size of the L1 cache in bytes (underestimation). */
    static volatile unsigned long long L1_cache_size;

    /** @brief Size of the L2 cache in bytes (underestimation). */
    static volatile unsigned long long L2_cache_size;

    /** @brief Size of the Translation Lookaside Buffer
	(underestimation). */
    static volatile unsigned int TLB_size;

    /** @brief Overestimation of cache line size.  Used to avoid
     * false sharing, i. e. elements of different threads are at
     * least this amount apart. */
    static unsigned int cache_line_size;

    //statistics
    /** @brief Statistic on the number of stolen ranges in
	load-balanced quicksort.*/
    static volatile sequence_index_t qsb_steals;
  };

  volatile bool Settings::force_parallel = false;
  volatile bool Settings::force_sequential = false;
  volatile  Settings::SortAlgorithm Settings::sort_algorithm = Settings::MWMS;
  volatile  Settings::Splitting Settings::sort_splitting = Settings::EXACT;
  volatile sequence_index_t Settings::sort_minimal_n = 1000;

  volatile unsigned int Settings::sort_mwms_oversampling = 10;
  volatile unsigned int Settings::sort_qs_num_samples_preset = 100;
  volatile sequence_index_t Settings::sort_qsb_base_case_maximal_n = 100;
  volatile sequence_index_t Settings::partition_minimal_n = 1000;
  volatile sequence_index_t Settings::nth_element_minimal_n = 1000;
  volatile sequence_index_t Settings::partial_sort_minimal_n = 1000;
  volatile sequence_index_t Settings::partition_chunk_size = 1000;
  volatile double Settings::partition_chunk_share = 0.0;
  volatile unsigned int Settings::adjacent_difference_minimal_n = 1000;
  volatile  Settings::PartialSumAlgorithm Settings::partial_sum_algorithm = Settings::LINEAR;
  volatile unsigned int Settings::partial_sum_minimal_n = 1000;
  volatile float Settings::partial_sum_dilatation = 1.0f;
  volatile unsigned int Settings::random_shuffle_minimal_n = 1000;
  volatile  Settings::Splitting Settings::merge_splitting = Settings::EXACT;
  volatile sequence_index_t Settings::merge_minimal_n = 1000;
  volatile unsigned int Settings::merge_oversampling = 10;
  volatile sequence_index_t Settings::multiway_merge_minimal_n = 1000;
  volatile int Settings::multiway_merge_minimal_k = 2;

  // unique copy
  volatile sequence_index_t Settings::unique_copy_minimal_n = 10000;
  volatile  Settings::MultiwayMergeAlgorithm Settings::multiway_merge_algorithm = Settings::LOSER_TREE;
  volatile  Settings::Splitting Settings::multiway_merge_splitting = Settings::EXACT;
  volatile unsigned int Settings::multiway_merge_oversampling = 10;
  volatile  Settings::FindDistribution Settings::find_distribution = Settings::CONSTANT_SIZE_BLOCKS;
  volatile sequence_index_t Settings::find_sequential_search_size = 256;
  volatile sequence_index_t Settings::find_initial_block_size = 256;
  volatile sequence_index_t Settings::find_maximum_block_size = 8192;
  volatile double Settings::find_increasing_factor = 2.0;
  volatile sequence_index_t Settings::workstealing_chunk_size = 100;
  volatile sequence_index_t Settings::for_each_minimal_n = 1000;
  volatile sequence_index_t Settings::count_minimal_n = 1000;
  volatile sequence_index_t Settings::transform_minimal_n = 1000;
  volatile sequence_index_t Settings::replace_minimal_n = 1000;
  volatile sequence_index_t Settings::generate_minimal_n = 1000;
  volatile sequence_index_t Settings::fill_minimal_n = 1000;
  volatile sequence_index_t Settings::min_element_minimal_n = 1000;
  volatile sequence_index_t Settings::max_element_minimal_n = 1000;
  volatile sequence_index_t Settings::accumulate_minimal_n = 1000;

  //set operations
  volatile sequence_index_t Settings::set_union_minimal_n = 1000;
  volatile sequence_index_t Settings::set_intersection_minimal_n = 1000;
  volatile sequence_index_t Settings::set_difference_minimal_n = 1000;
  volatile sequence_index_t Settings::set_symmetric_difference_minimal_n = 1000;
  volatile unsigned long long Settings::L1_cache_size = 16 << 10;
  volatile unsigned long long Settings::L2_cache_size = 256 << 10;
  volatile unsigned int Settings::TLB_size = 128;
  unsigned int Settings::cache_line_size = 64;

  //statistics
  volatile sequence_index_t Settings::qsb_steals = 0;
} // end anonymous namespace

}

#endif /* _GLIBCXX_SETTINGS_H */
