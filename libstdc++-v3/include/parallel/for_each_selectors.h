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

/** @file parallel/for_each_selectors.h
 *  @brief Functors representing different tasks to be plugged into the
 *  generic parallelization methods for embarrassingly parallel functions.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Felix Putze.

#ifndef _GLIBCXX_PARALLEL_FOR_EACH_SELECTORS_H
#define _GLIBCXX_PARALLEL_FOR_EACH_SELECTORS_H 1

#include <parallel/basic_iterator.h>

namespace __gnu_parallel
{

  /** @brief Generic selector for embarrassingly parallel functions. */
  template<typename It>
  struct generic_for_each_selector
  {
    /** @brief Iterator on last element processed; needed for some
     *  algorithms (e. g. std::transform()).
     */
    It finish_iterator;
  };


  /** @brief std::for_each() selector. */
  template<typename It>
    struct for_each_selector : public generic_for_each_selector<It>
    {
      /** @brief Functor execution.
       *  @param o Operator.
       *  @param i Iterator referencing object. */
      template<typename Op>
        bool
        operator()(Op& o, It i)
	{
	  o(*i);
	  return true;
	}
    };

  /** @brief std::generate() selector. */
  template<typename It>
    struct generate_selector : public generic_for_each_selector<It>
    {
      /** @brief Functor execution.
       *  @param o Operator.
       *  @param i Iterator referencing object. */
      template<typename Op>
        bool
        operator()(Op& o, It i)
        {
	  *i = o();
	  return true;
	}
    };

  /** @brief std::fill() selector. */
  template<typename It>
    struct fill_selector : public generic_for_each_selector<It>
    {
      /** @brief Functor execution.
       *  @param v Current value.
       *  @param i Iterator referencing object. */
      template<typename Val>
        bool
        operator()(Val& v, It i)
	{
	  *i = v;
	  return true;
	}
    };

  /** @brief std::transform() selector, one input sequence variant. */
  template<typename It>
    struct transform1_selector : public generic_for_each_selector<It>
    {
      /** @brief Functor execution.
       *  @param o Operator.
       *  @param i Iterator referencing object. */
      template<typename Op>
        bool
        operator()(Op& o, It i)
	{
	  *i.second = o(*i.first);
	  return true;
	}
    };

  /** @brief std::transform() selector, two input sequences variant. */
  template<typename It>
    struct transform2_selector : public generic_for_each_selector<It>
    {
      /** @brief Functor execution.
       *  @param o Operator.
       *  @param i Iterator referencing object. */
      template<typename Op>
        bool
        operator()(Op& o, It i)
	{
	  *i.third = o(*i.first, *i.second);
	  return true;
	}
    };

  /** @brief std::replace() selector. */
  template<typename It, typename T>
    struct replace_selector : public generic_for_each_selector<It>
    {
      /** @brief Value to replace with. */
      const T& new_val;

      /** @brief Constructor
       *  @param new_val Value to replace with. */
      explicit
      replace_selector(const T &new_val) : new_val(new_val) {}

      /** @brief Functor execution.
       *  @param v Current value.
       *  @param i Iterator referencing object. */
      bool
      operator()(T& v, It i)
      {
	if (*i == v)
	  *i = new_val;
	return true;
      }
    };

  /** @brief std::replace() selector. */
  template<typename It, typename Op, typename T>
    struct replace_if_selector : public generic_for_each_selector<It>
    {
      /** @brief Value to replace with. */
      const T& new_val;

      /** @brief Constructor.
       *  @param new_val Value to replace with. */
      explicit
      replace_if_selector(const T &new_val) : new_val(new_val) { }

      /** @brief Functor execution.
       *  @param o Operator.
       *  @param i Iterator referencing object. */
      bool
      operator()(Op& o, It i)
      {
	if (o(*i))
	  *i = new_val;
	return true;
      }
    };

  /** @brief std::count() selector. */
  template<typename It, typename Diff>
    struct count_selector : public generic_for_each_selector<It>
    {
      /** @brief Functor execution.
       *  @param v Current value.
       *  @param i Iterator referencing object.
       *  @return 1 if count, 0 if does not count. */
      template<typename Val>
        Diff
        operator()(Val& v, It i)
	{ return (v == *i) ? 1 : 0; }
    };

  /** @brief std::count_if () selector. */
  template<typename It, typename Diff>
    struct count_if_selector : public generic_for_each_selector<It>
    {
      /** @brief Functor execution.
       *  @param o Operator.
       *  @param i Iterator referencing object.
       *  @return 1 if count, 0 if does not count. */
      template<typename Op>
        Diff
        operator()(Op& o, It i)
	{ return (o(*i)) ? 1 : 0; }
    };

  /** @brief std::accumulate() selector. */
  template<typename It>
    struct accumulate_selector : public generic_for_each_selector<It>
    {
      /** @brief Functor execution.
       *  @param o Operator (unused).
       *  @param i Iterator referencing object.
       *  @return The current value. */
      template<typename Op>
        typename std::iterator_traits<It>::value_type operator()(Op o, It i)
	{ return *i; }
    };

  /** @brief std::inner_product() selector. */
  template<typename It, typename It2, typename T>
    struct inner_product_selector : public generic_for_each_selector<It>
    {
      /** @brief Begin iterator of first sequence. */
      It begin1_iterator;

      /** @brief Begin iterator of second sequence. */
      It2 begin2_iterator;

      /** @brief Constructor.
       *  @param b1 Begin iterator of first sequence.
       *  @param b2 Begin iterator of second sequence. */
      explicit
      inner_product_selector(It b1, It2 b2)
      : begin1_iterator(b1), begin2_iterator(b2) { }

      /** @brief Functor execution.
       *  @param mult Multiplication functor.
       *  @param current Iterator referencing object.
       *  @return Inner product elemental result. */
      template<typename Op>
        T
        operator()(Op mult, It current)
	{
	  typename std::iterator_traits<It>::difference_type position
	    = current - begin1_iterator;
	  return mult(*current, *(begin2_iterator + position));
	}
    };

  /** @brief Selector that just returns the passed iterator. */
  template<typename It>
    struct identity_selector : public generic_for_each_selector<It>
    {
      /** @brief Functor execution.
       *  @param o Operator (unused).
       *  @param i Iterator referencing object.
       *  @return Passed iterator. */
      template<typename Op>
        It
        operator()(Op o, It i)
	{ return i; }
    };

  /** @brief Selector that returns the difference between two adjacent
   *  elements.
   */
  template<typename It>
    struct adjacent_difference_selector : public generic_for_each_selector<It>
    {
      template<typename Op>
        bool
        operator()(Op& o, It i)
	{
	  typename It::first_type go_back_one = i.first;
	  --go_back_one;
	  *i.second = o(*i.first, *go_back_one);
	  return true;
	}
    };

  // XXX move into type_traits?
  /** @brief Functor doing nothing
   *
   *  For some reduction tasks (this is not a function object, but is
   *  passed as selector dummy parameter.
   */
  struct nothing
  {
    /** @brief Functor execution.
     *  @param i Iterator referencing object. */
    template<typename It>
      void
      operator()(It i) { }
  };

  /** @brief Reduction function doing nothing. */
  struct dummy_reduct
  {
    bool
    operator()(bool /*x*/, bool /*y*/) const
    { return true; }
  };

  /** @brief Reduction for finding the maximum element, using a comparator. */
  template<typename Comp, typename It>
    struct min_element_reduct
    {
      Comp& comp;

      explicit
      min_element_reduct(Comp &c) : comp(c) { }

      It
      operator()(It x, It y)
      {
	if (comp(*x, *y))
	  return x;
	else
	  return y;
      }
    };

  /** @brief Reduction for finding the maximum element, using a comparator. */
  template<typename Comp, typename It>
    struct max_element_reduct
    {
      Comp& comp;

      explicit
      max_element_reduct(Comp& c) : comp(c) { }

      It
      operator()(It x, It y)
      {
	if (comp(*x, *y))
	  return y;
	else
	  return x;
      }
    };

  /** @brief General reduction, using a binary operator. */
  template<typename BinOp>
    struct accumulate_binop_reduct
    {
      BinOp& binop;

      explicit
      accumulate_binop_reduct(BinOp& b) : binop(b) { }

      template<typename Result, typename Addend>
        Result
        operator()(const Result& x, const Addend& y)
	{ return binop(x, y); }
    };
}

#endif
