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

/** @file parallel/random_shuffle.h
 *  @brief Parallel implementation of std::random_shuffle().
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_RANDOM_SHUFFLE_H
#define _GLIBCXX_PARALLEL_RANDOM_SHUFFLE_H 1

#include <limits>
#include <bits/stl_numeric.h>
#include <parallel/parallel.h>
#include <parallel/random_number.h>

namespace __gnu_parallel
{
/** @brief Type to hold the index of a bin.
  *
  *  Since many variables of this type are allocated, it should be
  *  chosen as small as possible.
  */
typedef unsigned short bin_index;

/** @brief Data known to every thread participating in
    __gnu_parallel::parallel_random_shuffle(). */
template<typename RandomAccessIterator>
  struct DRandomShufflingGlobalData
  {
    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef typename traits_type::difference_type difference_type;

    /** @brief Begin iterator of the source. */
    RandomAccessIterator& source;

    /** @brief Temporary arrays for each thread. */
    value_type** temporaries;

    /** @brief Two-dimensional array to hold the thread-bin distribution.
     *
     *  Dimensions (num_threads + 1) x (num_bins + 1). */
    difference_type** dist;

    /** @brief Start indexes of the threads' chunks. */
    difference_type* starts;

    /** @brief Number of the thread that will further process the
	corresponding bin. */
    thread_index_t* bin_proc;

    /** @brief Number of bins to distribute to. */
    int num_bins;

    /** @brief Number of bits needed to address the bins. */
    int num_bits;

    /** @brief Constructor. */
    DRandomShufflingGlobalData(RandomAccessIterator& _source)
    : source(_source) { }
  };

/** @brief Local data for a thread participating in
    __gnu_parallel::parallel_random_shuffle().
  */
template<typename RandomAccessIterator, typename RandomNumberGenerator>
  struct DRSSorterPU
  {
    /** @brief Number of threads participating in total. */
    int num_threads;

    /** @brief Begin index for bins taken care of by this thread. */
    bin_index bins_begin;

    /** @brief End index for bins taken care of by this thread. */
    bin_index bins_end;

    /** @brief Random seed for this thread. */
    uint32 seed;

    /** @brief Pointer to global data. */
    DRandomShufflingGlobalData<RandomAccessIterator>* sd;
  };

/** @brief Generate a random number in @c [0,2^logp).
  *  @param logp Logarithm (basis 2) of the upper range bound.
  *  @param rng Random number generator to use.
  */
template<typename RandomNumberGenerator>
  inline int
  random_number_pow2(int logp, RandomNumberGenerator& rng)
  { return rng.genrand_bits(logp); }

/** @brief Random shuffle code executed by each thread.
  *  @param pus Array of thread-local data records. */
template<typename RandomAccessIterator, typename RandomNumberGenerator>
  void 
  parallel_random_shuffle_drs_pu(DRSSorterPU<RandomAccessIterator,
                                 RandomNumberGenerator>* pus)
  {
    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef typename traits_type::difference_type difference_type;

    thread_index_t iam = omp_get_thread_num();
    DRSSorterPU<RandomAccessIterator, RandomNumberGenerator>* d = &pus[iam];
    DRandomShufflingGlobalData<RandomAccessIterator>* sd = d->sd;

    // Indexing: dist[bin][processor]
    difference_type length = sd->starts[iam + 1] - sd->starts[iam];
    bin_index* oracles = new bin_index[length];
    difference_type* dist = new difference_type[sd->num_bins + 1];
    bin_index* bin_proc = new bin_index[sd->num_bins];
    value_type** temporaries = new value_type*[d->num_threads];

    // Compute oracles and count appearances.
    for (bin_index b = 0; b < sd->num_bins + 1; ++b)
      dist[b] = 0;
    int num_bits = sd->num_bits;

    random_number rng(d->seed);

    // First main loop.
    for (difference_type i = 0; i < length; ++i)
      {
        bin_index oracle = random_number_pow2(num_bits, rng);
        oracles[i] = oracle;

        // To allow prefix (partial) sum.
        ++(dist[oracle + 1]);
      }

    for (bin_index b = 0; b < sd->num_bins + 1; ++b)
      sd->dist[b][iam + 1] = dist[b];

#   pragma omp barrier

#   pragma omp single
    {
      // Sum up bins, sd->dist[s + 1][d->num_threads] now contains the
      // total number of items in bin s
      for (bin_index s = 0; s < sd->num_bins; ++s)
        __gnu_sequential::partial_sum(sd->dist[s + 1],
                                      sd->dist[s + 1] + d->num_threads + 1,
                                      sd->dist[s + 1]);
    }

#   pragma omp barrier

    sequence_index_t offset = 0, global_offset = 0;
    for (bin_index s = 0; s < d->bins_begin; ++s)
      global_offset += sd->dist[s + 1][d->num_threads];

#   pragma omp barrier

    for (bin_index s = d->bins_begin; s < d->bins_end; ++s)
      {
	for (int t = 0; t < d->num_threads + 1; ++t)
	  sd->dist[s + 1][t] += offset;
	offset = sd->dist[s + 1][d->num_threads];
      }

    sd->temporaries[iam] = static_cast<value_type*>(
      ::operator new(sizeof(value_type) * offset));

#   pragma omp barrier

    // Draw local copies to avoid false sharing.
    for (bin_index b = 0; b < sd->num_bins + 1; ++b)
      dist[b] = sd->dist[b][iam];
    for (bin_index b = 0; b < sd->num_bins; ++b)
      bin_proc[b] = sd->bin_proc[b];
    for (thread_index_t t = 0; t < d->num_threads; ++t)
      temporaries[t] = sd->temporaries[t];

    RandomAccessIterator source = sd->source;
    difference_type start = sd->starts[iam];

    // Distribute according to oracles, second main loop.
    for (difference_type i = 0; i < length; ++i)
      {
        bin_index target_bin = oracles[i];
        thread_index_t target_p = bin_proc[target_bin];

        // Last column [d->num_threads] stays unchanged.
        ::new(&(temporaries[target_p][dist[target_bin + 1]++]))
	    value_type(*(source + i + start));
      }

    delete[] oracles;
    delete[] dist;
    delete[] bin_proc;
    delete[] temporaries;

#   pragma omp barrier

    // Shuffle bins internally.
    for (bin_index b = d->bins_begin; b < d->bins_end; ++b)
      {
        value_type* begin =
                    sd->temporaries[iam] +
                    ((b == d->bins_begin) ? 0 : sd->dist[b][d->num_threads]),
                  * end =
                    sd->temporaries[iam] + sd->dist[b + 1][d->num_threads];
        sequential_random_shuffle(begin, end, rng);
        std::copy(begin, end, sd->source + global_offset +
            ((b == d->bins_begin) ? 0 : sd->dist[b][d->num_threads]));
      }

    ::operator delete(sd->temporaries[iam]);
  }

/** @brief Round up to the next greater power of 2.
  *  @param x Integer to round up */
template<typename T>
  T 
  round_up_to_pow2(T x)
  {
    if (x <= 1)
      return 1;
    else
      return (T)1 << (__log2(x - 1) + 1);
  }

/** @brief Main parallel random shuffle step.
  *  @param begin Begin iterator of sequence.
  *  @param end End iterator of sequence.
  *  @param n Length of sequence.
  *  @param num_threads Number of threads to use.
  *  @param rng Random number generator to use.
  */
template<typename RandomAccessIterator, typename RandomNumberGenerator>
  void
  parallel_random_shuffle_drs(RandomAccessIterator begin,
			      RandomAccessIterator end,
			      typename std::iterator_traits
			      <RandomAccessIterator>::difference_type n,
			      thread_index_t num_threads,
			      RandomNumberGenerator& rng)
  {
    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef typename traits_type::difference_type difference_type;

    _GLIBCXX_CALL(n)

    const _Settings& __s = _Settings::get();

    if (num_threads > n)
      num_threads = static_cast<thread_index_t>(n);

    bin_index num_bins, num_bins_cache;

#if _GLIBCXX_RANDOM_SHUFFLE_CONSIDER_L1
    // Try the L1 cache first.

    // Must fit into L1.
    num_bins_cache = std::max<difference_type>(
        1, n / (__s.L1_cache_size_lb / sizeof(value_type)));
    num_bins_cache = round_up_to_pow2(num_bins_cache);

    // No more buckets than TLB entries, power of 2
    // Power of 2 and at least one element per bin, at most the TLB size.
    num_bins = std::min<difference_type>(n, num_bins_cache);

#if _GLIBCXX_RANDOM_SHUFFLE_CONSIDER_TLB
    // 2 TLB entries needed per bin.
    num_bins = std::min<difference_type>(__s.TLB_size / 2, num_bins);
#endif
    num_bins = round_up_to_pow2(num_bins);

    if (num_bins < num_bins_cache)
      {
#endif
        // Now try the L2 cache
        // Must fit into L2
        num_bins_cache = static_cast<bin_index>(std::max<difference_type>(
            1, n / (__s.L2_cache_size / sizeof(value_type))));
        num_bins_cache = round_up_to_pow2(num_bins_cache);

        // No more buckets than TLB entries, power of 2.
        num_bins = static_cast<bin_index>(
            std::min(n, static_cast<difference_type>(num_bins_cache)));
        // Power of 2 and at least one element per bin, at most the TLB size.
#if _GLIBCXX_RANDOM_SHUFFLE_CONSIDER_TLB
        // 2 TLB entries needed per bin.
        num_bins = std::min(
            static_cast<difference_type>(__s.TLB_size / 2), num_bins);
#endif
          num_bins = round_up_to_pow2(num_bins);
#if _GLIBCXX_RANDOM_SHUFFLE_CONSIDER_L1
      }
#endif

    num_threads = std::min<bin_index>(num_threads, num_bins);

    if (num_threads <= 1)
      return sequential_random_shuffle(begin, end, rng);

    DRandomShufflingGlobalData<RandomAccessIterator> sd(begin);
    DRSSorterPU<RandomAccessIterator, random_number >* pus;
    difference_type* starts;

#   pragma omp parallel num_threads(num_threads)
      {
        thread_index_t num_threads = omp_get_num_threads();
#       pragma omp single
          {
            pus = new DRSSorterPU<RandomAccessIterator, random_number>
                [num_threads];

            sd.temporaries = new value_type*[num_threads];
            sd.dist = new difference_type*[num_bins + 1];
            sd.bin_proc = new thread_index_t[num_bins];
            for (bin_index b = 0; b < num_bins + 1; ++b)
              sd.dist[b] = new difference_type[num_threads + 1];
            for (bin_index b = 0; b < (num_bins + 1); ++b)
              {
                sd.dist[0][0] = 0;
                sd.dist[b][0] = 0;
              }
            starts = sd.starts = new difference_type[num_threads + 1];
            int bin_cursor = 0;
            sd.num_bins = num_bins;
            sd.num_bits = __log2(num_bins);

            difference_type chunk_length = n / num_threads,
                            split = n % num_threads, start = 0;
            difference_type bin_chunk_length = num_bins / num_threads,
                            bin_split = num_bins % num_threads;
            for (thread_index_t i = 0; i < num_threads; ++i)
              {
                starts[i] = start;
                start += (i < split) ? (chunk_length + 1) : chunk_length;
                int j = pus[i].bins_begin = bin_cursor;

                // Range of bins for this processor.
                bin_cursor += (i < bin_split) ?
                    (bin_chunk_length + 1) : bin_chunk_length;
                pus[i].bins_end = bin_cursor;
                for (; j < bin_cursor; ++j)
                  sd.bin_proc[j] = i;
                pus[i].num_threads = num_threads;
                pus[i].seed = rng(std::numeric_limits<uint32>::max());
                pus[i].sd = &sd;
              }
            starts[num_threads] = start;
          } //single
        // Now shuffle in parallel.
        parallel_random_shuffle_drs_pu(pus);
      }  // parallel

    delete[] starts;
    delete[] sd.bin_proc;
    for (int s = 0; s < (num_bins + 1); ++s)
      delete[] sd.dist[s];
    delete[] sd.dist;
    delete[] sd.temporaries;

    delete[] pus;
  }

/** @brief Sequential cache-efficient random shuffle.
 *  @param begin Begin iterator of sequence.
 *  @param end End iterator of sequence.
 *  @param rng Random number generator to use.
 */
template<typename RandomAccessIterator, typename RandomNumberGenerator>
  void
  sequential_random_shuffle(RandomAccessIterator begin, 
                            RandomAccessIterator end,
                            RandomNumberGenerator& rng)
  {
    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::value_type value_type;
    typedef typename traits_type::difference_type difference_type;

    difference_type n = end - begin;
    const _Settings& __s = _Settings::get();

    bin_index num_bins, num_bins_cache;

#if _GLIBCXX_RANDOM_SHUFFLE_CONSIDER_L1
    // Try the L1 cache first, must fit into L1.
    num_bins_cache =
        std::max<difference_type>
            (1, n / (__s.L1_cache_size_lb / sizeof(value_type)));
    num_bins_cache = round_up_to_pow2(num_bins_cache);

    // No more buckets than TLB entries, power of 2
    // Power of 2 and at least one element per bin, at most the TLB size
    num_bins = std::min(n, (difference_type)num_bins_cache);
#if _GLIBCXX_RANDOM_SHUFFLE_CONSIDER_TLB
    // 2 TLB entries needed per bin
    num_bins = std::min((difference_type)__s.TLB_size / 2, num_bins);
#endif
    num_bins = round_up_to_pow2(num_bins);

    if (num_bins < num_bins_cache)
      {
#endif
        // Now try the L2 cache, must fit into L2.
        num_bins_cache =
            static_cast<bin_index>(std::max<difference_type>(
                1, n / (__s.L2_cache_size / sizeof(value_type))));
        num_bins_cache = round_up_to_pow2(num_bins_cache);

        // No more buckets than TLB entries, power of 2
        // Power of 2 and at least one element per bin, at most the TLB size.
        num_bins = static_cast<bin_index>
            (std::min(n, static_cast<difference_type>(num_bins_cache)));

#if _GLIBCXX_RANDOM_SHUFFLE_CONSIDER_TLB
        // 2 TLB entries needed per bin
        num_bins =
            std::min<difference_type>(__s.TLB_size / 2, num_bins);
#endif
        num_bins = round_up_to_pow2(num_bins);
#if _GLIBCXX_RANDOM_SHUFFLE_CONSIDER_L1
      }
#endif

    int num_bits = __log2(num_bins);

    if (num_bins > 1)
      {
        value_type* target = static_cast<value_type*>(
          ::operator new(sizeof(value_type) * n));
        bin_index* oracles = new bin_index[n];
        difference_type* dist0 = new difference_type[num_bins + 1],
                       * dist1 = new difference_type[num_bins + 1];

        for (int b = 0; b < num_bins + 1; ++b)
          dist0[b] = 0;

        random_number bitrng(rng(0xFFFFFFFF));

        for (difference_type i = 0; i < n; ++i)
          {
            bin_index oracle = random_number_pow2(num_bits, bitrng);
            oracles[i] = oracle;

            // To allow prefix (partial) sum.
            ++(dist0[oracle + 1]);
          }

        // Sum up bins.
        __gnu_sequential::partial_sum(dist0, dist0 + num_bins + 1, dist0);

        for (int b = 0; b < num_bins + 1; ++b)
          dist1[b] = dist0[b];

        // Distribute according to oracles.
        for (difference_type i = 0; i < n; ++i)
          ::new(&(target[(dist0[oracles[i]])++])) value_type(*(begin + i));

        for (int b = 0; b < num_bins; ++b)
          {
            sequential_random_shuffle(target + dist1[b],
                                      target + dist1[b + 1],
                                      rng);
          }

        // Copy elements back.
        std::copy(target, target + n, begin);

        delete[] dist0;
        delete[] dist1;
        delete[] oracles;
        ::operator delete(target);
      }
    else
      __gnu_sequential::random_shuffle(begin, end, rng);
  }

/** @brief Parallel random public call.
 *  @param begin Begin iterator of sequence.
 *  @param end End iterator of sequence.
 *  @param rng Random number generator to use.
 */
template<typename RandomAccessIterator, typename RandomNumberGenerator>
  inline void
  parallel_random_shuffle(RandomAccessIterator begin,
                          RandomAccessIterator end,
                          RandomNumberGenerator rng = random_number())
  {
    typedef std::iterator_traits<RandomAccessIterator> traits_type;
    typedef typename traits_type::difference_type difference_type;
    difference_type n = end - begin;
    parallel_random_shuffle_drs(begin, end, n, get_max_threads(), rng) ;
  }

}

#endif /* _GLIBCXX_PARALLEL_RANDOM_SHUFFLE_H */
