// -*- C++ -*-

// Copyright (C) 2007, 2008 Free Software Foundation, Inc.
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

/** @file parallel/multiseq_selection.h
 *  @brief Functions to find elements of a certain global rank in
 *  multiple sorted sequences.  Also serves for splitting such
 *  sequence sets.
 *
 *  The algorithm description can be found in 
 *
 *  P. J. Varman, S. D. Scheufler, B. R. Iyer, and G. R. Ricard.
 *  Merging Multiple Lists on Hierarchical-Memory Multiprocessors.
 *  Journal of Parallel and Distributed Computing, 12(2):171–177, 1991.
 *
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_MULTISEQ_SELECTION_H
#define _GLIBCXX_PARALLEL_MULTISEQ_SELECTION_H 1

#include <vector>
#include <queue>

#include <bits/stl_algo.h>

#include <parallel/sort.h>

namespace __gnu_parallel
{
  /** @brief Compare a pair of types lexicographically, ascending. */
  template<typename T1, typename T2, typename Comparator>
    class lexicographic
    : public std::binary_function<std::pair<T1, T2>, std::pair<T1, T2>, bool>
    {
    private:
      Comparator& comp;

    public:
      lexicographic(Comparator& _comp) : comp(_comp) { }

      bool
      operator()(const std::pair<T1, T2>& p1,
		 const std::pair<T1, T2>& p2) const
      {
	if (comp(p1.first, p2.first))
	  return true;

	if (comp(p2.first, p1.first))
	  return false;

	// Firsts are equal.
	return p1.second < p2.second;
      }
    };

  /** @brief Compare a pair of types lexicographically, descending. */
  template<typename T1, typename T2, typename Comparator>
    class lexicographic_reverse : public std::binary_function<T1, T2, bool>
    {
    private:
      Comparator& comp;

    public:
      lexicographic_reverse(Comparator& _comp) : comp(_comp) { }

      bool
      operator()(const std::pair<T1, T2>& p1,
		 const std::pair<T1, T2>& p2) const
      {
	if (comp(p2.first, p1.first))
	  return true;

	if (comp(p1.first, p2.first))
	  return false;

	// Firsts are equal.
	return p2.second < p1.second;
      }
    };

  /** 
   *  @brief Splits several sorted sequences at a certain global rank,
   *  resulting in a splitting point for each sequence.
   *  The sequences are passed via a sequence of random-access
   *  iterator pairs, none of the sequences may be empty.  If there
   *  are several equal elements across the split, the ones on the
   *  left side will be chosen from sequences with smaller number.
   *  @param begin_seqs Begin of the sequence of iterator pairs.
   *  @param end_seqs End of the sequence of iterator pairs.
   *  @param rank The global rank to partition at.
   *  @param begin_offsets A random-access sequence begin where the
   *  result will be stored in. Each element of the sequence is an
   *  iterator that points to the first element on the greater part of
   *  the respective sequence.
   *  @param comp The ordering functor, defaults to std::less<T>. 
   */
  template<typename RanSeqs, typename RankType, typename RankIterator,
            typename Comparator>
    void
    multiseq_partition(RanSeqs begin_seqs, RanSeqs end_seqs,
                       RankType rank,
                       RankIterator begin_offsets,
                       Comparator comp = std::less<
                       typename std::iterator_traits<typename
                       std::iterator_traits<RanSeqs>::value_type::
                       first_type>::value_type>()) // std::less<T>
    {
      _GLIBCXX_CALL(end_seqs - begin_seqs)

      typedef typename std::iterator_traits<RanSeqs>::value_type::first_type
        It;
      typedef typename std::iterator_traits<It>::difference_type
	       difference_type;
      typedef typename std::iterator_traits<It>::value_type value_type;

      lexicographic<value_type, int, Comparator> lcomp(comp);
      lexicographic_reverse<value_type, int, Comparator> lrcomp(comp);

      // Number of sequences, number of elements in total (possibly
      // including padding).
      difference_type m = std::distance(begin_seqs, end_seqs), N = 0,
                      nmax, n, r;

      for (int i = 0; i < m; i++)
        {
          N += std::distance(begin_seqs[i].first, begin_seqs[i].second);
          _GLIBCXX_PARALLEL_ASSERT(
            std::distance(begin_seqs[i].first, begin_seqs[i].second) > 0);
        }

      if (rank == N)
        {
          for (int i = 0; i < m; i++)
            begin_offsets[i] = begin_seqs[i].second; // Very end.
          // Return m - 1;
          return;
        }

      _GLIBCXX_PARALLEL_ASSERT(m != 0);
      _GLIBCXX_PARALLEL_ASSERT(N != 0);
      _GLIBCXX_PARALLEL_ASSERT(rank >= 0);
      _GLIBCXX_PARALLEL_ASSERT(rank < N);

      difference_type* ns = new difference_type[m];
      difference_type* a = new difference_type[m];
      difference_type* b = new difference_type[m];
      difference_type l;

      ns[0] = std::distance(begin_seqs[0].first, begin_seqs[0].second);
      nmax = ns[0];
      for (int i = 0; i < m; i++)
	{
	  ns[i] = std::distance(begin_seqs[i].first, begin_seqs[i].second);
	  nmax = std::max(nmax, ns[i]);
	}

      r = log2(nmax) + 1;

      // Pad all lists to this length, at least as long as any ns[i],
      // equality iff nmax = 2^k - 1.
      l = (1ULL << r) - 1;

      // From now on, including padding.
      N = l * m;

      for (int i = 0; i < m; i++)
	{
	  a[i] = 0;
	  b[i] = l;
	}
      n = l / 2;

      // Invariants:
      // 0 <= a[i] <= ns[i], 0 <= b[i] <= l

#define S(i) (begin_seqs[i].first)

      // Initial partition.
      std::vector<std::pair<value_type, int> > sample;

      for (int i = 0; i < m; i++)
	if (n < ns[i])	//sequence long enough
	  sample.push_back(std::make_pair(S(i)[n], i));
      __gnu_sequential::sort(sample.begin(), sample.end(), lcomp);

      for (int i = 0; i < m; i++)	//conceptual infinity
	if (n >= ns[i])	//sequence too short, conceptual infinity
	  sample.push_back(std::make_pair(S(i)[0] /*dummy element*/, i));

      difference_type localrank = rank * m / N ;

      int j;
      for (j = 0; j < localrank && ((n + 1) <= ns[sample[j].second]); ++j)
	a[sample[j].second] += n + 1;
      for (; j < m; j++)
	b[sample[j].second] -= n + 1;
      
      // Further refinement.
      while (n > 0)
	{
	  n /= 2;

	  int lmax_seq = -1;	// to avoid warning
	  const value_type* lmax = NULL; // impossible to avoid the warning?
	  for (int i = 0; i < m; i++)
	    {
	      if (a[i] > 0)
		{
		  if (!lmax)
		    {
		      lmax = &(S(i)[a[i] - 1]);
		      lmax_seq = i;
		    }
		  else
		    {
		      // Max, favor rear sequences.
		      if (!comp(S(i)[a[i] - 1], *lmax))
			{
			  lmax = &(S(i)[a[i] - 1]);
			  lmax_seq = i;
			}
		    }
		}
	    }

	  int i;
	  for (i = 0; i < m; i++)
	    {
	      difference_type middle = (b[i] + a[i]) / 2;
	      if (lmax && middle < ns[i] &&
		  lcomp(std::make_pair(S(i)[middle], i),
			std::make_pair(*lmax, lmax_seq)))
		a[i] = std::min(a[i] + n + 1, ns[i]);
	      else
		b[i] -= n + 1;
	    }

	  difference_type leftsize = 0, total = 0;
	  for (int i = 0; i < m; i++)
	    {
	      leftsize += a[i] / (n + 1);
	      total += l / (n + 1);
	    }
	  
	  difference_type skew = static_cast<difference_type>
	    (static_cast<uint64>(total) * rank / N - leftsize);

	  if (skew > 0)
	    {
	      // Move to the left, find smallest.
	      std::priority_queue<std::pair<value_type, int>,
		std::vector<std::pair<value_type, int> >,
		lexicographic_reverse<value_type, int, Comparator> >
		pq(lrcomp);
	      
	      for (int i = 0; i < m; i++)
		if (b[i] < ns[i])
		  pq.push(std::make_pair(S(i)[b[i]], i));

	      for (; skew != 0 && !pq.empty(); --skew)
		{
		  int source = pq.top().second;
		  pq.pop();

		  a[source] = std::min(a[source] + n + 1, ns[source]);
		  b[source] += n + 1;

		  if (b[source] < ns[source])
		    pq.push(std::make_pair(S(source)[b[source]], source));
		}
	    }
	  else if (skew < 0)
	    {
	      // Move to the right, find greatest.
	      std::priority_queue<std::pair<value_type, int>,
		std::vector<std::pair<value_type, int> >,
		lexicographic<value_type, int, Comparator> > pq(lcomp);

	      for (int i = 0; i < m; i++)
		if (a[i] > 0)
		  pq.push(std::make_pair(S(i)[a[i] - 1], i));

	      for (; skew != 0; ++skew)
		{
		  int source = pq.top().second;
		  pq.pop();

		  a[source] -= n + 1;
		  b[source] -= n + 1;

		  if (a[source] > 0)
		    pq.push(std::make_pair(S(source)[a[source] - 1], source));
		}
	    }
	}

      // Postconditions:
      // a[i] == b[i] in most cases, except when a[i] has been clamped
      // because of having reached the boundary

      // Now return the result, calculate the offset.

      // Compare the keys on both edges of the border.

      // Maximum of left edge, minimum of right edge.
      value_type* maxleft = NULL;
      value_type* minright = NULL;
      for (int i = 0; i < m; i++)
	{
	  if (a[i] > 0)
	    {
	      if (!maxleft)
		maxleft = &(S(i)[a[i] - 1]);
	      else
		{
		  // Max, favor rear sequences.
		  if (!comp(S(i)[a[i] - 1], *maxleft))
		    maxleft = &(S(i)[a[i] - 1]);
		}
	    }
	  if (b[i] < ns[i])
	    {
	      if (!minright)
		minright = &(S(i)[b[i]]);
	      else
		{
		  // Min, favor fore sequences.
		  if (comp(S(i)[b[i]], *minright))
		    minright = &(S(i)[b[i]]);
		}
	    }
	}

      int seq = 0;
      for (int i = 0; i < m; i++)
	begin_offsets[i] = S(i) + a[i];

      delete[] ns;
      delete[] a;
      delete[] b;
    }


  /** 
   *  @brief Selects the element at a certain global rank from several
   *  sorted sequences.
   *
   *  The sequences are passed via a sequence of random-access
   *  iterator pairs, none of the sequences may be empty.
   *  @param begin_seqs Begin of the sequence of iterator pairs.
   *  @param end_seqs End of the sequence of iterator pairs.
   *  @param rank The global rank to partition at.
   *  @param offset The rank of the selected element in the global
   *  subsequence of elements equal to the selected element. If the
   *  selected element is unique, this number is 0.
   *  @param comp The ordering functor, defaults to std::less. 
   */
  template<typename T, typename RanSeqs, typename RankType,
	   typename Comparator>
    T
    multiseq_selection(RanSeqs begin_seqs, RanSeqs end_seqs, RankType rank,
		       RankType& offset, Comparator comp = std::less<T>())
    {
      _GLIBCXX_CALL(end_seqs - begin_seqs)

      typedef typename std::iterator_traits<RanSeqs>::value_type::first_type
	It;
      typedef typename std::iterator_traits<It>::difference_type
	difference_type;

      lexicographic<T, int, Comparator> lcomp(comp);
      lexicographic_reverse<T, int, Comparator> lrcomp(comp);

      // Number of sequences, number of elements in total (possibly
      // including padding).
      difference_type m = std::distance(begin_seqs, end_seqs);
      difference_type N = 0;
      difference_type nmax, n, r;

      for (int i = 0; i < m; i++)
	N += std::distance(begin_seqs[i].first, begin_seqs[i].second);

      if (m == 0 || N == 0 || rank < 0 || rank >= N)
	{
	  // Result undefined when there is no data or rank is outside bounds.
	  throw std::exception();
	}


      difference_type* ns = new difference_type[m];
      difference_type* a = new difference_type[m];
      difference_type* b = new difference_type[m];
      difference_type l;

      ns[0] = std::distance(begin_seqs[0].first, begin_seqs[0].second);
      nmax = ns[0];
      for (int i = 0; i < m; ++i)
	{
	  ns[i] = std::distance(begin_seqs[i].first, begin_seqs[i].second);
	  nmax = std::max(nmax, ns[i]);
	}

      r = log2(nmax) + 1;

      // Pad all lists to this length, at least as long as any ns[i],
      // equality iff nmax = 2^k - 1
      l = pow2(r) - 1;

      // From now on, including padding.
      N = l * m;

      for (int i = 0; i < m; ++i)
	{
	  a[i] = 0;
	  b[i] = l;
	}
      n = l / 2;

      // Invariants:
      // 0 <= a[i] <= ns[i], 0 <= b[i] <= l

#define S(i) (begin_seqs[i].first)

      // Initial partition.
      std::vector<std::pair<T, int> > sample;

      for (int i = 0; i < m; i++)
	if (n < ns[i])
	  sample.push_back(std::make_pair(S(i)[n], i));
      __gnu_sequential::sort(sample.begin(), sample.end(),
			     lcomp, sequential_tag());

      // Conceptual infinity.
      for (int i = 0; i < m; i++)
	if (n >= ns[i])
	  sample.push_back(std::make_pair(S(i)[0] /*dummy element*/, i));

      difference_type localrank = rank * m / N ;

      int j;
      for (j = 0; j < localrank && ((n + 1) <= ns[sample[j].second]); ++j)
	a[sample[j].second] += n + 1;
      for (; j < m; ++j)
	b[sample[j].second] -= n + 1;

      // Further refinement.
      while (n > 0)
	{
	  n /= 2;

	  const T* lmax = NULL;
	  for (int i = 0; i < m; ++i)
	    {
	      if (a[i] > 0)
		{
		  if (!lmax)
		    lmax = &(S(i)[a[i] - 1]);
		  else
		    {
		      if (comp(*lmax, S(i)[a[i] - 1]))	//max
			lmax = &(S(i)[a[i] - 1]);
		    }
		}
	    }

	  int i;
	  for (i = 0; i < m; i++)
	    {
	      difference_type middle = (b[i] + a[i]) / 2;
	      if (lmax && middle < ns[i] && comp(S(i)[middle], *lmax))
		a[i] = std::min(a[i] + n + 1, ns[i]);
	      else
		b[i] -= n + 1;
	    }

	  difference_type leftsize = 0, total = 0;
	  for (int i = 0; i < m; ++i)
	    {
	      leftsize += a[i] / (n + 1);
	      total += l / (n + 1);
	    }

	  difference_type skew = ((unsigned long long)total * rank / N
				  - leftsize);

	  if (skew > 0)
	    {
	      // Move to the left, find smallest.
	      std::priority_queue<std::pair<T, int>,
		std::vector<std::pair<T, int> >,
		lexicographic_reverse<T, int, Comparator> > pq(lrcomp);

	      for (int i = 0; i < m; ++i)
		if (b[i] < ns[i])
		  pq.push(std::make_pair(S(i)[b[i]], i));

	      for (; skew != 0 && !pq.empty(); --skew)
		{
		  int source = pq.top().second;
		  pq.pop();
		  
		  a[source] = std::min(a[source] + n + 1, ns[source]);
		  b[source] += n + 1;
		  
		  if (b[source] < ns[source])
		    pq.push(std::make_pair(S(source)[b[source]], source));
		}
	    }
	  else if (skew < 0)
	    {
	      // Move to the right, find greatest.
	      std::priority_queue<std::pair<T, int>,
		std::vector<std::pair<T, int> >,
		lexicographic<T, int, Comparator> > pq(lcomp);

	      for (int i = 0; i < m; ++i)
		if (a[i] > 0)
		  pq.push(std::make_pair(S(i)[a[i] - 1], i));

	      for (; skew != 0; ++skew)
		{
		  int source = pq.top().second;
		  pq.pop();

		  a[source] -= n + 1;
		  b[source] -= n + 1;

		  if (a[source] > 0)
		    pq.push(std::make_pair(S(source)[a[source] - 1], source));
		}
	    }
	}

      // Postconditions:
      // a[i] == b[i] in most cases, except when a[i] has been clamped
      // because of having reached the boundary

      // Now return the result, calculate the offset.

      // Compare the keys on both edges of the border.

      // Maximum of left edge, minimum of right edge.
      bool maxleftset = false, minrightset = false;

      // Impossible to avoid the warning?
      T maxleft, minright;
      for (int i = 0; i < m; ++i)
	{
	  if (a[i] > 0)
	    {
	      if (!maxleftset)
		{
		  maxleft = S(i)[a[i] - 1];
		  maxleftset = true;
		}
	      else
		{
		  // Max.
		  if (comp(maxleft, S(i)[a[i] - 1]))
		    maxleft = S(i)[a[i] - 1];
		}
	    }
	  if (b[i] < ns[i])
	    {
	      if (!minrightset)
		{
		  minright = S(i)[b[i]];
		  minrightset = true;
		}
	      else
		{
		  // Min.
		  if (comp(S(i)[b[i]], minright))
		    minright = S(i)[b[i]];
		}
	    }
      }

      // Minright is the splitter, in any case.

      if (!maxleftset || comp(minright, maxleft))
	{
	  // Good luck, everything is split unambiguously.
	  offset = 0;
	}
      else
	{
	  // We have to calculate an offset.
	  offset = 0;

	  for (int i = 0; i < m; ++i)
	    {
	      difference_type lb = std::lower_bound(S(i), S(i) + ns[i],
						    minright,
						    comp) - S(i);
	      offset += a[i] - lb;
	    }
	}

      delete[] ns;
      delete[] a;
      delete[] b;

      return minright;
    }
}

#undef S

#endif

