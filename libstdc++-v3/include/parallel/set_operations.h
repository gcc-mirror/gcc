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

/**
 * @file parallel/set_operations.h
 * @brief Parallel implementations of set operations for random-access
 * iterators.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Marius Elvert and Felix Bondarenko.

#ifndef _GLIBCXX_PARALLEL_SET_OPERATIONS_H
#define _GLIBCXX_PARALLEL_SET_OPERATIONS_H 1

#include <omp.h>

#include <parallel/settings.h>
#include <parallel/multiseq_selection.h>

namespace __gnu_parallel
{
template<typename InputIterator, typename OutputIterator>
  OutputIterator
  copy_tail(std::pair<InputIterator, InputIterator> b,
            std::pair<InputIterator, InputIterator> e, OutputIterator r)
  {
    if (b.first != e.first)
      {
        do
          {
            *r++ = *b.first++;
          }
        while (b.first != e.first);
      }
    else
      {
        while (b.second != e.second)
          *r++ = *b.second++;
      }
    return r;
  }

template<typename InputIterator,
	 typename OutputIterator,
	 typename Comparator>
  struct symmetric_difference_func
  {
    typedef std::iterator_traits<InputIterator> traits_type;
    typedef typename traits_type::difference_type difference_type;
    typedef typename std::pair<InputIterator, InputIterator> iterator_pair;

    symmetric_difference_func(Comparator c) : comp(c) {}

    Comparator comp;

    OutputIterator
    invoke(InputIterator a, InputIterator b,
	   InputIterator c, InputIterator d,
	   OutputIterator r) const
    {
      while (a != b && c != d)
        {
          if (comp(*a, *c))
            {
              *r = *a;
              ++a;
              ++r;
            }
          else if (comp(*c, *a))
            {
              *r = *c;
              ++c;
              ++r;
            }
          else
            {
              ++a;
              ++c;
            }
        }
      return std::copy(c, d, std::copy(a, b, r));
    }

    difference_type
    count(InputIterator a, InputIterator b,
	  InputIterator c, InputIterator d) const
    {
      difference_type counter = 0;

      while (a != b && c != d)
        {
          if (comp(*a, *c))
            {
              ++a;
              ++counter;
            }
          else if (comp(*c, *a))
            {
              ++c;
              ++counter;
            }
          else
            {
              ++a;
              ++c;
            }
        }

      return counter + (b - a) + (d - c);
    }

    OutputIterator
    first_empty(InputIterator c, InputIterator d, OutputIterator out) const
    { return std::copy(c, d, out); }

    OutputIterator
    second_empty(InputIterator a, InputIterator b, OutputIterator out) const
    { return std::copy(a, b, out); }
  };


template<typename InputIterator,
	 typename OutputIterator,
	 typename Comparator>
  struct difference_func
  {
    typedef std::iterator_traits<InputIterator> traits_type;
    typedef typename traits_type::difference_type difference_type;
    typedef typename std::pair<InputIterator, InputIterator> iterator_pair;

    difference_func(Comparator c) : comp(c) {}

    Comparator comp;

    OutputIterator
    invoke(InputIterator a, InputIterator b, InputIterator c, InputIterator d,
          OutputIterator r) const
    {
      while (a != b && c != d)
        {
          if (comp(*a, *c))
            {
              *r = *a;
              ++a;
              ++r;
            }
          else if (comp(*c, *a))
            { ++c; }
          else
            {
              ++a;
              ++c;
            }
        }
      return std::copy(a, b, r);
    }

    difference_type
    count(InputIterator a, InputIterator b,
	  InputIterator c, InputIterator d) const
    {
      difference_type counter = 0;

      while (a != b && c != d)
        {
          if (comp(*a, *c))
            {
              ++a;
              ++counter;
            }
          else if (comp(*c, *a))
            { ++c; }
          else
            { ++a; ++c; }
        }

      return counter + (b - a);
    }

    inline OutputIterator
    first_empty(InputIterator c, InputIterator d, OutputIterator out) const
    { return out; }

    inline OutputIterator
    second_empty(InputIterator a, InputIterator b, OutputIterator out) const
    { return std::copy(a, b, out); }
  };


template<typename InputIterator,
	 typename OutputIterator,
	 typename Comparator>
  struct intersection_func
  {
    typedef std::iterator_traits<InputIterator> traits_type;
    typedef typename traits_type::difference_type difference_type;
    typedef typename std::pair<InputIterator, InputIterator> iterator_pair;

    intersection_func(Comparator c) : comp(c) {}

    Comparator comp;

    OutputIterator
    invoke(InputIterator a, InputIterator b, InputIterator c, InputIterator d,
          OutputIterator r) const
    {
      while (a != b && c != d)
        {
          if (comp(*a, *c))
            { ++a; }
          else if (comp(*c, *a))
            { ++c; }
          else
            {
              *r = *a;
              ++a;
              ++c;
              ++r;
            }
        }

      return r;
    }

    difference_type
    count(InputIterator a, InputIterator b,
	  InputIterator c, InputIterator d) const
    {
      difference_type counter = 0;

      while (a != b && c != d)
        {
          if (comp(*a, *c))
            { ++a; }
          else if (comp(*c, *a))
            { ++c; }
          else
            {
              ++a;
              ++c;
              ++counter;
            }
        }

      return counter;
    }

    inline OutputIterator
    first_empty(InputIterator c, InputIterator d, OutputIterator out) const
    { return out; }

    inline OutputIterator
    second_empty(InputIterator a, InputIterator b, OutputIterator out) const
    { return out; }
  };

template<class InputIterator, class OutputIterator, class Comparator>
  struct union_func
  {
    typedef typename std::iterator_traits<InputIterator>::difference_type
    difference_type;

    union_func(Comparator c) : comp(c) {}

    Comparator comp;

    OutputIterator
    invoke(InputIterator a, const InputIterator b, InputIterator c,
          const InputIterator d, OutputIterator r) const
    {
      while (a != b && c != d)
        {
          if (comp(*a, *c))
            {
              *r = *a;
              ++a;
            }
          else if (comp(*c, *a))
            {
              *r = *c;
              ++c;
            }
          else
            {
              *r = *a;
              ++a;
              ++c;
            }
          ++r;
        }
      return std::copy(c, d, std::copy(a, b, r));
    }

    difference_type
    count(InputIterator a, InputIterator b,
	  InputIterator c, InputIterator d) const
    {
      difference_type counter = 0;

      while (a != b && c != d)
        {
          if (comp(*a, *c))
            { ++a; }
          else if (comp(*c, *a))
            { ++c; }
          else
            {
              ++a;
              ++c;
            }
          ++counter;
        }

      counter += (b - a);
      counter += (d - c);
      return counter;
    }

    inline OutputIterator
    first_empty(InputIterator c, InputIterator d, OutputIterator out) const
    { return std::copy(c, d, out); }

    inline OutputIterator
    second_empty(InputIterator a, InputIterator b, OutputIterator out) const
    { return std::copy(a, b, out); }
  };

template<typename InputIterator,
	 typename OutputIterator,
	 typename Operation>
  OutputIterator
  parallel_set_operation(InputIterator begin1, InputIterator end1,
                         InputIterator begin2, InputIterator end2,
                         OutputIterator result, Operation op)
  {
    _GLIBCXX_CALL((end1 - begin1) + (end2 - begin2))

    typedef std::iterator_traits<InputIterator> traits_type;
    typedef typename traits_type::difference_type difference_type;
    typedef typename std::pair<InputIterator, InputIterator> iterator_pair;

    if (begin1 == end1)
      return op.first_empty(begin2, end2, result);

    if (begin2 == end2)
      return op.second_empty(begin1, end1, result);

    const difference_type size = (end1 - begin1) + (end2 - begin2);

    const iterator_pair sequence[ 2 ] =
        { std::make_pair(begin1, end1), std::make_pair(begin2, end2) } ;
    OutputIterator return_value = result;
    difference_type *borders;
    iterator_pair *block_begins;
    difference_type* lengths;

    thread_index_t num_threads =
        std::min<difference_type>(get_max_threads(),
            std::min(end1 - begin1, end2 - begin2));

#   pragma omp parallel num_threads(num_threads)
      {
#       pragma omp single
          {
            num_threads = omp_get_num_threads();

            borders = new difference_type[num_threads + 2];
            equally_split(size, num_threads + 1, borders);
            block_begins = new iterator_pair[num_threads + 1];
            // Very start.
            block_begins[0] = std::make_pair(begin1, begin2);
            lengths = new difference_type[num_threads];
          } //single

        thread_index_t iam = omp_get_thread_num();

        // Result from multiseq_partition.
        InputIterator offset[2];
        const difference_type rank = borders[iam + 1];

        multiseq_partition(sequence, sequence + 2, rank, offset, op.comp);

        // allowed to read?
        // together
        // *(offset[ 0 ] - 1) == *offset[ 1 ]
        if (offset[ 0 ] != begin1 && offset[ 1 ] != end2
            && !op.comp(*(offset[ 0 ] - 1), *offset[ 1 ])
            && !op.comp(*offset[ 1 ], *(offset[ 0 ] - 1)))
          {
            // Avoid split between globally equal elements: move one to
            // front in first sequence.
            --offset[ 0 ];
          }

        iterator_pair block_end = block_begins[ iam + 1 ] =
            iterator_pair(offset[ 0 ], offset[ 1 ]);

        // Make sure all threads have their block_begin result written out.
#       pragma omp barrier

        iterator_pair block_begin = block_begins[ iam ];

        // Begin working for the first block, while the others except
        // the last start to count.
        if (iam == 0)
          {
            // The first thread can copy already.
            lengths[ iam ] = op.invoke(block_begin.first, block_end.first,
                                       block_begin.second, block_end.second,
                                       result)
                              - result;
          }
        else
          {
            lengths[ iam ] = op.count(block_begin.first, block_end.first,
                        block_begin.second, block_end.second);
          }

        // Make sure everyone wrote their lengths.
#       pragma omp barrier

        OutputIterator r = result;

        if (iam == 0)
          {
            // Do the last block.
            for (int i = 0; i < num_threads; ++i)
              r += lengths[i];

            block_begin = block_begins[num_threads];

            // Return the result iterator of the last block.
            return_value = op.invoke(
                block_begin.first, end1, block_begin.second, end2, r);

          }
        else
          {
            for (int i = 0; i < iam; ++i)
              r += lengths[ i ];

            // Reset begins for copy pass.
            op.invoke(block_begin.first, block_end.first,
                  block_begin.second, block_end.second, r);
          }
      }
    return return_value;
  }


template<typename InputIterator,
	 typename OutputIterator,
	 typename Comparator>
  inline OutputIterator
  parallel_set_union(InputIterator begin1, InputIterator end1,
                     InputIterator begin2, InputIterator end2,
                     OutputIterator result, Comparator comp)
  {
    return parallel_set_operation(begin1, end1, begin2, end2, result,
        union_func< InputIterator, OutputIterator, Comparator>(comp));
  }

template<typename InputIterator,
	 typename OutputIterator,
	 typename Comparator>
  inline OutputIterator
  parallel_set_intersection(InputIterator begin1, InputIterator end1,
                            InputIterator begin2, InputIterator end2,
                            OutputIterator result, Comparator comp)
  {
    return parallel_set_operation(begin1, end1, begin2, end2, result,
        intersection_func<InputIterator, OutputIterator, Comparator>(comp));
  }

template<typename InputIterator,
	 typename OutputIterator,
	 typename Comparator>
  inline OutputIterator
  parallel_set_difference(InputIterator begin1, InputIterator end1,
                          InputIterator begin2, InputIterator end2,
                          OutputIterator result, Comparator comp)
  {
    return parallel_set_operation(begin1, end1, begin2, end2, result,
        difference_func<InputIterator, OutputIterator, Comparator>(comp));
  }

template<typename InputIterator,
	 typename OutputIterator,
	 typename Comparator>
  inline OutputIterator
  parallel_set_symmetric_difference(InputIterator begin1, InputIterator end1,
                                    InputIterator begin2, InputIterator end2,
                                    OutputIterator result, Comparator comp)
  {
    return parallel_set_operation(begin1, end1, begin2, end2, result,
        symmetric_difference_func<InputIterator, OutputIterator, Comparator>
            (comp));
  }

}

#endif /* _GLIBCXX_PARALLEL_SET_OPERATIONS_H */
