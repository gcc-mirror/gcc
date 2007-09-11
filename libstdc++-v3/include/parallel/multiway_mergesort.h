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

/** @file parallel/multiway_mergesort.h
 *  @brief Parallel multiway merge sort.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_MERGESORT_H
#define _GLIBCXX_PARALLEL_MERGESORT_H 1

#include <vector>

#include <parallel/basic_iterator.h>
#include <bits/stl_algo.h>
#include <parallel/parallel.h>
#include <parallel/multiway_merge.h>
#include <parallel/timing.h>

namespace __gnu_parallel
{

  /** @brief Subsequence description. */
  template<typename _DifferenceTp>
  struct Piece
  {
    typedef _DifferenceTp difference_type;

    /** @brief Begin of subsequence. */
    difference_type begin;

    /** @brief End of subsequence. */
    difference_type end;
  };

  /** @brief Data accessed by all threads.
   *
   *  PMWMS = parallel multiway mergesort */
  template<typename RandomAccessIterator>
  struct PMWMSSortingData
  {
    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef typename traits_type::difference_type difference_type;

    /** @brief Input begin. */
    RandomAccessIterator source;

    /** @brief Start indices, per thread. */
    difference_type* starts;

    /** @brief Temporary arrays for each thread.
     *
     *  Indirection Allows using the temporary storage in different
     *  ways, without code duplication.
     *  @see _GLIBCXX_MULTIWAY_MERGESORT_COPY_LAST */
    value_type** temporaries;

#if _GLIBCXX_MULTIWAY_MERGESORT_COPY_LAST
    /** @brief Storage in which to sort. */
    RandomAccessIterator* sorting_places;

    /** @brief Storage into which to merge. */
    value_type** merging_places;
#else
    /** @brief Storage in which to sort. */
    value_type** sorting_places;

    /** @brief Storage into which to merge. */
    RandomAccessIterator* merging_places;
#endif
    /** @brief Samples. */
    value_type* samples;

    /** @brief Offsets to add to the found positions. */
    difference_type* offsets;

    /** @brief Pieces of data to merge @c [thread][sequence] */
    std::vector<Piece<difference_type> >* pieces;
  };

  /** @brief Thread local data for PMWMS. */
  template<typename RandomAccessIterator>
  struct PMWMSSorterPU
  {
    /** @brief Total number of thread involved. */
    thread_index_t num_threads;
    /** @brief Number of owning thread. */
    thread_index_t iam;
    /** @brief Stable sorting desired. */
    bool stable;
    /** @brief Pointer to global data. */
    PMWMSSortingData<RandomAccessIterator>* sd;
  };

  /** 
   *  @brief Select samples from a sequence.
   *  @param d Pointer to thread-local data. Result will be placed in
   *  @c d->ds->samples.
   *  @param num_samples Number of samples to select. 
   */
  template<typename RandomAccessIterator, typename _DifferenceTp>
  inline void 
  determine_samples(PMWMSSorterPU<RandomAccessIterator>* d, 
		    _DifferenceTp& num_samples)
  {
    typedef _DifferenceTp difference_type;

    PMWMSSortingData<RandomAccessIterator>* sd = d->sd;

    num_samples = Settings::sort_mwms_oversampling * d->num_threads - 1;

    difference_type es[num_samples + 2];
    equally_split(sd->starts[d->iam + 1] - sd->starts[d->iam], num_samples + 1, es);

    for (difference_type i = 0; i < num_samples; i++)
      sd->samples[d->iam * num_samples + i] = sd->source[sd->starts[d->iam] + es[i + 1]];
  }

  /** @brief PMWMS code executed by each thread.
   *  @param d Pointer to thread-local data.
   *  @param comp Comparator. 
   */
  template<typename RandomAccessIterator, typename Comparator>
  inline void 
  parallel_sort_mwms_pu(PMWMSSorterPU<RandomAccessIterator>* d, 
			Comparator& comp)
  {
    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef typename traits_type::difference_type difference_type;

    Timing<sequential_tag> t;

    t.tic();

    PMWMSSortingData<RandomAccessIterator>* sd = d->sd;
    thread_index_t iam = d->iam;

    // Length of this thread's chunk, before merging.
    difference_type length_local = sd->starts[iam + 1] - sd->starts[iam];

#if _GLIBCXX_MULTIWAY_MERGESORT_COPY_LAST
    typedef RandomAccessIterator SortingPlacesIterator;

    // Sort in input storage.
    sd->sorting_places[iam] = sd->source + sd->starts[iam];
#else
    typedef value_type* SortingPlacesIterator;

    // Sort in temporary storage, leave space for sentinel.
    sd->sorting_places[iam] = sd->temporaries[iam] = static_cast<value_type*>(::operator new(sizeof(value_type) *(length_local + 1)));

    // Copy there.
    std::uninitialized_copy(sd->source + sd->starts[iam], sd->source + sd->starts[iam] + length_local, sd->sorting_places[iam]);
#endif

    // Sort locally.
    if (d->stable)
      __gnu_sequential::stable_sort(sd->sorting_places[iam], sd->sorting_places[iam] + length_local, comp);
    else
      __gnu_sequential::sort(sd->sorting_places[iam], sd->sorting_places[iam] + length_local, comp);

#if _GLIBCXX_ASSERTIONS
    _GLIBCXX_PARALLEL_ASSERT(is_sorted(sd->sorting_places[iam], sd->sorting_places[iam] + length_local, comp));
#endif

    // Invariant: locally sorted subsequence in sd->sorting_places[iam],
    // sd->sorting_places[iam] + length_local.
    t.tic("local sort");

    if (Settings::sort_splitting == Settings::SAMPLING)
      {
	difference_type num_samples;
	determine_samples(d, num_samples);

#pragma omp barrier

	t.tic("sample/wait");

#pragma omp single
	__gnu_sequential::sort(sd->samples, sd->samples + (num_samples * d->num_threads), comp);

#pragma omp barrier

	for (int s = 0; s < d->num_threads; s++)
	  {
	    // For each sequence.
	    if (num_samples * iam > 0)
	      sd->pieces[iam][s].begin = std::lower_bound(sd->sorting_places[s],
				 sd->sorting_places[s] + sd->starts[s + 1] - sd->starts[s],
				 sd->samples[num_samples * iam],
				 comp)
		- sd->sorting_places[s];
	    else
	      // Absolute beginning.
	      sd->pieces[iam][s].begin = 0;

	    if ((num_samples * (iam + 1)) < (num_samples * d->num_threads))
	      sd->pieces[iam][s].end = std::lower_bound(sd->sorting_places[s],
							sd->sorting_places[s] + sd->starts[s + 1] - sd->starts[s], sd->samples[num_samples * (iam + 1)], comp)
		- sd->sorting_places[s];
	    else
	      // Absolute end.
	      sd->pieces[iam][s].end = sd->starts[s + 1] - sd->starts[s];
	  }

      }
    else if (Settings::sort_splitting == Settings::EXACT)
      {
#pragma omp barrier

	t.tic("wait");

	std::vector<std::pair<SortingPlacesIterator, SortingPlacesIterator> > seqs(d->num_threads);
	for (int s = 0; s < d->num_threads; s++)
	  seqs[s] = std::make_pair(sd->sorting_places[s], sd->sorting_places[s] + sd->starts[s + 1] - sd->starts[s]);

	std::vector<SortingPlacesIterator> offsets(d->num_threads);

	// If not last thread.
	if (iam < d->num_threads - 1)
	  multiseq_partition(seqs.begin(), seqs.end(), sd->starts[iam + 1], offsets.begin(), comp);

	for (int seq = 0; seq < d->num_threads; seq++)
	  {
	    // For each sequence.
	    if (iam < (d->num_threads - 1))
	      sd->pieces[iam][seq].end = offsets[seq] - seqs[seq].first;
	    else
	      // Absolute end of this sequence.
	      sd->pieces[iam][seq].end = sd->starts[seq + 1] - sd->starts[seq];
	  }

#pragma omp barrier

	for (int seq = 0; seq < d->num_threads; seq++)
	  {
	    // For each sequence.
	    if (iam > 0)
	      sd->pieces[iam][seq].begin = sd->pieces[iam - 1][seq].end;
	    else
	      // Absolute beginning.
	      sd->pieces[iam][seq].begin = 0;
	  }
      }

    t.tic("split");

    // Offset from target begin, length after merging.
    difference_type offset = 0, length_am = 0;
    for (int s = 0; s < d->num_threads; s++)
      {
	length_am += sd->pieces[iam][s].end - sd->pieces[iam][s].begin;
	offset += sd->pieces[iam][s].begin;
      }

#if _GLIBCXX_MULTIWAY_MERGESORT_COPY_LAST
    // Merge to temporary storage, uninitialized creation not possible
    // since there is no multiway_merge calling the placement new
    // instead of the assignment operator.
    sd->merging_places[iam] = sd->temporaries[iam] = new value_type[length_am];
#else
    // Merge directly to target.
    sd->merging_places[iam] = sd->source + offset;
#endif
    std::vector<std::pair<SortingPlacesIterator, SortingPlacesIterator> > seqs(d->num_threads);

    for (int s = 0; s < d->num_threads; s++)
      {
	seqs[s] = std::make_pair(sd->sorting_places[s] + sd->pieces[iam][s].begin, sd->sorting_places[s] + sd->pieces[iam][s].end);

#if _GLIBCXX_ASSERTIONS
	_GLIBCXX_PARALLEL_ASSERT(is_sorted(seqs[s].first, seqs[s].second, comp));
#endif
      }

    multiway_merge(seqs.begin(), seqs.end(), sd->merging_places[iam], comp, length_am, d->stable, false, sequential_tag());

    t.tic("merge");

#if _GLIBCXX_ASSERTIONS
    _GLIBCXX_PARALLEL_ASSERT(is_sorted(sd->merging_places[iam], sd->merging_places[iam] + length_am, comp));
#endif

#	pragma omp barrier

#if _GLIBCXX_MULTIWAY_MERGESORT_COPY_LAST
    // Write back.
    std::copy(sd->merging_places[iam], sd->merging_places[iam] + length_am, 
	      sd->source + offset);
#endif

    delete[] sd->temporaries[iam];

    t.tic("copy back");

    t.print();
  }

  /** @brief PMWMS main call.
   *  @param begin Begin iterator of sequence.
   *  @param end End iterator of sequence.
   *  @param comp Comparator.
   *  @param n Length of sequence.
   *  @param num_threads Number of threads to use.
   *  @param stable Stable sorting.
   */
  template<typename RandomAccessIterator, typename Comparator>
  inline void
  parallel_sort_mwms(RandomAccessIterator begin, RandomAccessIterator end, Comparator comp, typename std::iterator_traits<RandomAccessIterator>::difference_type n, int num_threads, bool stable)
  {
    _GLIBCXX_CALL(n)
      
    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef typename traits_type::difference_type difference_type;

    if (n <= 1)
      return;

    // At least one element per thread.
    if (num_threads > n)
      num_threads = static_cast<thread_index_t>(n);

    PMWMSSortingData<RandomAccessIterator> sd;

    sd.source = begin;
    sd.temporaries = new value_type*[num_threads];

#if _GLIBCXX_MULTIWAY_MERGESORT_COPY_LAST
    sd.sorting_places = new RandomAccessIterator[num_threads];
    sd.merging_places = new value_type*[num_threads];
#else
    sd.sorting_places = new value_type*[num_threads];
    sd.merging_places = new RandomAccessIterator[num_threads];
#endif

    if (Settings::sort_splitting == Settings::SAMPLING)
      sd.samples = new value_type[num_threads * (Settings::sort_mwms_oversampling * num_threads - 1)];
    else
      sd.samples = NULL;

    sd.offsets = new difference_type[num_threads - 1];
    sd.pieces = new std::vector<Piece<difference_type> >[num_threads];
    for (int s = 0; s < num_threads; s++)
      sd.pieces[s].resize(num_threads);
    PMWMSSorterPU<RandomAccessIterator>* pus = new PMWMSSorterPU<RandomAccessIterator>[num_threads];
    difference_type* starts = sd.starts = new difference_type[num_threads + 1];

    difference_type chunk_length = n / num_threads, split = n % num_threads, start = 0;
    for (int i = 0; i < num_threads; i++)
      {
	starts[i] = start;
	start += (i < split) ? (chunk_length + 1) : chunk_length;
	pus[i].num_threads = num_threads;
	pus[i].iam = i;
	pus[i].sd = &sd;
	pus[i].stable = stable;
      }
    starts[num_threads] = start;

    // Now sort in parallel.
#pragma omp parallel num_threads(num_threads)
    parallel_sort_mwms_pu(&(pus[omp_get_thread_num()]), comp);

    // XXX sd as RAII
    delete[] starts;
    delete[] sd.temporaries;
    delete[] sd.sorting_places;
    delete[] sd.merging_places;

    if (Settings::sort_splitting == Settings::SAMPLING)
      delete[] sd.samples;

    delete[] sd.offsets;
    delete[] sd.pieces;

    delete[] pus;
  }

}

#endif
