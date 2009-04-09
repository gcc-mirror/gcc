// -*- C++ -*-

// Copyright (C) 2007, 2008, 2009 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file parallel/multiway_mergesort.h
 *  @brief Parallel multiway merge sort.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_MULTIWAY_MERGESORT_H
#define _GLIBCXX_PARALLEL_MULTIWAY_MERGESORT_H 1

#include <vector>

#include <parallel/basic_iterator.h>
#include <bits/stl_algo.h>
#include <parallel/parallel.h>
#include <parallel/multiway_merge.h>

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

    /** @brief Number of threads involved. */
    thread_index_t num_threads;

    /** @brief Input begin. */
    RandomAccessIterator source;

    /** @brief Start indices, per thread. */
    difference_type* starts;

    /** @brief Storage in which to sort. */
    value_type** temporary;

    /** @brief Samples. */
    value_type* samples;

    /** @brief Offsets to add to the found positions. */
    difference_type* offsets;

    /** @brief Pieces of data to merge @c [thread][sequence] */
    std::vector<Piece<difference_type> >* pieces;
};

/**
  *  @brief Select samples from a sequence.
  *  @param sd Pointer to algorithm data. Result will be placed in
  *  @c sd->samples.
  *  @param num_samples Number of samples to select.
  */
template<typename RandomAccessIterator, typename _DifferenceTp>
  void 
  determine_samples(PMWMSSortingData<RandomAccessIterator>* sd,
                    _DifferenceTp num_samples)
  {
    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef _DifferenceTp difference_type;

    thread_index_t iam = omp_get_thread_num();

    difference_type* es = new difference_type[num_samples + 2];

    equally_split(sd->starts[iam + 1] - sd->starts[iam], 
                  num_samples + 1, es);

    for (difference_type i = 0; i < num_samples; ++i)
      ::new(&(sd->samples[iam * num_samples + i]))
	  value_type(sd->source[sd->starts[iam] + es[i + 1]]);

    delete[] es;
  }

/** @brief Split consistently. */
template<bool exact, typename RandomAccessIterator,
          typename Comparator, typename SortingPlacesIterator>
  struct split_consistently
  {
  };

/** @brief Split by exact splitting. */
template<typename RandomAccessIterator, typename Comparator,
          typename SortingPlacesIterator>
  struct split_consistently
    <true, RandomAccessIterator, Comparator, SortingPlacesIterator>
  {
    void operator()(
      const thread_index_t iam,
      PMWMSSortingData<RandomAccessIterator>* sd,
      Comparator& comp,
      const typename
        std::iterator_traits<RandomAccessIterator>::difference_type
          num_samples)
      const
  {
#   pragma omp barrier

    std::vector<std::pair<SortingPlacesIterator, SortingPlacesIterator> >
        seqs(sd->num_threads);
    for (thread_index_t s = 0; s < sd->num_threads; s++)
      seqs[s] = std::make_pair(sd->temporary[s],
                                sd->temporary[s]
                                    + (sd->starts[s + 1] - sd->starts[s]));

    std::vector<SortingPlacesIterator> offsets(sd->num_threads);

    // if not last thread
    if (iam < sd->num_threads - 1)
      multiseq_partition(seqs.begin(), seqs.end(),
                          sd->starts[iam + 1], offsets.begin(), comp);

    for (int seq = 0; seq < sd->num_threads; seq++)
      {
        // for each sequence
        if (iam < (sd->num_threads - 1))
          sd->pieces[iam][seq].end = offsets[seq] - seqs[seq].first;
        else
          // very end of this sequence
          sd->pieces[iam][seq].end =
              sd->starts[seq + 1] - sd->starts[seq];
      }

#   pragma omp barrier

    for (thread_index_t seq = 0; seq < sd->num_threads; seq++)
      {
        // For each sequence.
        if (iam > 0)
          sd->pieces[iam][seq].begin = sd->pieces[iam - 1][seq].end;
        else
          // Absolute beginning.
          sd->pieces[iam][seq].begin = 0;
      }
  }   
  };

/** @brief Split by sampling. */ 
template<typename RandomAccessIterator, typename Comparator,
          typename SortingPlacesIterator>
  struct split_consistently<false, RandomAccessIterator, Comparator,
                             SortingPlacesIterator>
  {
    void operator()(
        const thread_index_t iam,
        PMWMSSortingData<RandomAccessIterator>* sd,
        Comparator& comp,
        const typename
          std::iterator_traits<RandomAccessIterator>::difference_type
            num_samples)
        const
    {
      typedef std::iterator_traits<RandomAccessIterator> traits_type;
      typedef typename traits_type::value_type value_type;
      typedef typename traits_type::difference_type difference_type;

      determine_samples(sd, num_samples);

#     pragma omp barrier

#     pragma omp single
      __gnu_sequential::sort(sd->samples,
                             sd->samples + (num_samples * sd->num_threads),
                             comp);

#     pragma omp barrier

      for (thread_index_t s = 0; s < sd->num_threads; ++s)
        {
          // For each sequence.
          if (num_samples * iam > 0)
            sd->pieces[iam][s].begin =
                std::lower_bound(sd->temporary[s],
                    sd->temporary[s]
                        + (sd->starts[s + 1] - sd->starts[s]),
                    sd->samples[num_samples * iam],
                    comp)
                - sd->temporary[s];
          else
            // Absolute beginning.
            sd->pieces[iam][s].begin = 0;

          if ((num_samples * (iam + 1)) < (num_samples * sd->num_threads))
            sd->pieces[iam][s].end =
                std::lower_bound(sd->temporary[s],
                        sd->temporary[s]
                            + (sd->starts[s + 1] - sd->starts[s]),
                        sd->samples[num_samples * (iam + 1)],
                        comp)
                - sd->temporary[s];
          else
            // Absolute end.
            sd->pieces[iam][s].end = sd->starts[s + 1] - sd->starts[s];
        }
    }
  };
  
template<bool stable, typename RandomAccessIterator, typename Comparator>
  struct possibly_stable_sort
  {
  };

template<typename RandomAccessIterator, typename Comparator>
  struct possibly_stable_sort<true, RandomAccessIterator, Comparator>
  {
    void operator()(const RandomAccessIterator& begin,
                     const RandomAccessIterator& end, Comparator& comp) const
    {
      __gnu_sequential::stable_sort(begin, end, comp); 
    }
  };

template<typename RandomAccessIterator, typename Comparator>
  struct possibly_stable_sort<false, RandomAccessIterator, Comparator>
  {
    void operator()(const RandomAccessIterator begin,
                     const RandomAccessIterator end, Comparator& comp) const
    {
      __gnu_sequential::sort(begin, end, comp); 
    }
  };

template<bool stable, typename SeqRandomAccessIterator,
          typename RandomAccessIterator, typename Comparator,
          typename DiffType>
  struct possibly_stable_multiway_merge
  {
  };

template<typename SeqRandomAccessIterator, typename RandomAccessIterator,
          typename Comparator, typename DiffType>
  struct possibly_stable_multiway_merge
    <true, SeqRandomAccessIterator, RandomAccessIterator, Comparator,
    DiffType>
  {
    void operator()(const SeqRandomAccessIterator& seqs_begin,
                      const SeqRandomAccessIterator& seqs_end,
                      const RandomAccessIterator& target,
                      Comparator& comp,
                      DiffType length_am) const
    {
      stable_multiway_merge(seqs_begin, seqs_end, target, length_am, comp,
                       sequential_tag());
    }
  };

template<typename SeqRandomAccessIterator, typename RandomAccessIterator,
          typename Comparator, typename DiffType>
  struct possibly_stable_multiway_merge
    <false, SeqRandomAccessIterator, RandomAccessIterator, Comparator,
    DiffType>
  {
    void operator()(const SeqRandomAccessIterator& seqs_begin,
                      const SeqRandomAccessIterator& seqs_end,
                      const RandomAccessIterator& target,
                      Comparator& comp,
                      DiffType length_am) const
    {
      multiway_merge(seqs_begin, seqs_end, target, length_am, comp,
                       sequential_tag());
    }
  };

/** @brief PMWMS code executed by each thread.
  *  @param sd Pointer to algorithm data.
  *  @param comp Comparator.
  */
template<bool stable, bool exact, typename RandomAccessIterator,
          typename Comparator>
  void 
  parallel_sort_mwms_pu(PMWMSSortingData<RandomAccessIterator>* sd,
                        Comparator& comp)
  {
    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef typename traits_type::difference_type difference_type;

    thread_index_t iam = omp_get_thread_num();

    // Length of this thread's chunk, before merging.
    difference_type length_local = sd->starts[iam + 1] - sd->starts[iam];

    // Sort in temporary storage, leave space for sentinel.

    typedef value_type* SortingPlacesIterator;

    sd->temporary[iam] =
        static_cast<value_type*>(
        ::operator new(sizeof(value_type) * (length_local + 1)));

    // Copy there.
    std::uninitialized_copy(sd->source + sd->starts[iam],
                            sd->source + sd->starts[iam] + length_local,
                            sd->temporary[iam]);

    possibly_stable_sort<stable, SortingPlacesIterator, Comparator>()
        (sd->temporary[iam], sd->temporary[iam] + length_local, comp);

    // Invariant: locally sorted subsequence in sd->temporary[iam],
    // sd->temporary[iam] + length_local.

    // No barrier here: Synchronization is done by the splitting routine.

    difference_type num_samples =
        _Settings::get().sort_mwms_oversampling * sd->num_threads - 1;
    split_consistently
      <exact, RandomAccessIterator, Comparator, SortingPlacesIterator>()
        (iam, sd, comp, num_samples);

    // Offset from target begin, length after merging.
    difference_type offset = 0, length_am = 0;
    for (thread_index_t s = 0; s < sd->num_threads; s++)
      {
        length_am += sd->pieces[iam][s].end - sd->pieces[iam][s].begin;
        offset += sd->pieces[iam][s].begin;
      }

    typedef std::vector<
      std::pair<SortingPlacesIterator, SortingPlacesIterator> >
        seq_vector_type;
    seq_vector_type seqs(sd->num_threads);

    for (int s = 0; s < sd->num_threads; ++s)
      {
        seqs[s] =
          std::make_pair(sd->temporary[s] + sd->pieces[iam][s].begin,
        sd->temporary[s] + sd->pieces[iam][s].end);
      }

    possibly_stable_multiway_merge<
        stable,
        typename seq_vector_type::iterator,
        RandomAccessIterator,
        Comparator, difference_type>()
          (seqs.begin(), seqs.end(),
           sd->source + offset, comp,
           length_am);

#   pragma omp barrier

    ::operator delete(sd->temporary[iam]);
  }

/** @brief PMWMS main call.
  *  @param begin Begin iterator of sequence.
  *  @param end End iterator of sequence.
  *  @param comp Comparator.
  *  @param n Length of sequence.
  *  @param num_threads Number of threads to use.
  */
template<bool stable, bool exact, typename RandomAccessIterator,
           typename Comparator>
  void
  parallel_sort_mwms(RandomAccessIterator begin, RandomAccessIterator end,
                     Comparator comp,
                     thread_index_t num_threads)
  {
    _GLIBCXX_CALL(end - begin)

    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef typename traits_type::difference_type difference_type;

    difference_type n = end - begin;

    if (n <= 1)
      return;

    // at least one element per thread
    if (num_threads > n)
      num_threads = static_cast<thread_index_t>(n);

    // shared variables
    PMWMSSortingData<RandomAccessIterator> sd;
    difference_type* starts;

#   pragma omp parallel num_threads(num_threads)
      {
        num_threads = omp_get_num_threads();  //no more threads than requested

#       pragma omp single
          {
            sd.num_threads = num_threads;
            sd.source = begin;

            sd.temporary = new value_type*[num_threads];

            if (!exact)
              {
                difference_type size =
                    (_Settings::get().sort_mwms_oversampling * num_threads - 1)
                        * num_threads;
                sd.samples = static_cast<value_type*>(
                              ::operator new(size * sizeof(value_type)));
              }
            else
              sd.samples = NULL;

            sd.offsets = new difference_type[num_threads - 1];
            sd.pieces = new std::vector<Piece<difference_type> >[num_threads];
            for (int s = 0; s < num_threads; ++s)
              sd.pieces[s].resize(num_threads);
            starts = sd.starts = new difference_type[num_threads + 1];

            difference_type chunk_length = n / num_threads;
            difference_type split = n % num_threads;
            difference_type pos = 0;
            for (int i = 0; i < num_threads; ++i)
              {
                starts[i] = pos;
                pos += (i < split) ? (chunk_length + 1) : chunk_length;
              }
            starts[num_threads] = pos;
          } //single

        // Now sort in parallel.
        parallel_sort_mwms_pu<stable, exact>(&sd, comp);
      } //parallel

    delete[] starts;
    delete[] sd.temporary;

    if (!exact)
      ::operator delete(sd.samples);

    delete[] sd.offsets;
    delete[] sd.pieces;
  }
} //namespace __gnu_parallel

#endif /* _GLIBCXX_PARALLEL_MULTIWAY_MERGESORT_H */
