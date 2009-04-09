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

/** @file parallel/base.h
 *  @brief Sequential helper functions.
 *  This file is a GNU parallel extension to the Standard C++ Library.
 */

// Written by Johannes Singler.

#ifndef _GLIBCXX_PARALLEL_BASE_H
#define _GLIBCXX_PARALLEL_BASE_H 1

#include <functional>
#include <omp.h>
#include <parallel/features.h>
#include <parallel/basic_iterator.h>
#include <parallel/parallel.h>


// Parallel mode namespaces.

/**
 * @namespace std::__parallel
 * @brief GNU parallel code, replaces standard behavior with parallel behavior.
 */
namespace std 
{ 
  namespace __parallel { } 
}

/**
 * @namespace __gnu_parallel
 * @brief GNU parallel code for public use.
 */
namespace __gnu_parallel
{
  // Import all the parallel versions of components in namespace std.
  using namespace std::__parallel;
}

/**
 * @namespace __gnu_sequential
 * @brief GNU sequential classes for public use.
 */
namespace __gnu_sequential 
{ 
  // Import whatever is the serial version.
#ifdef _GLIBCXX_PARALLEL
  using namespace std::__norm;
#else
  using namespace std;
#endif   
}


namespace __gnu_parallel
{
  // NB: Including this file cannot produce (unresolved) symbols from
  // the OpenMP runtime unless the parallel mode is actually invoked
  // and active, which imples that the OpenMP runtime is actually
  // going to be linked in.
  inline int
  get_max_threads() 
  { 
    int __i = omp_get_max_threads();
    return __i > 1 ? __i : 1; 
  }

  
  inline bool 
  is_parallel(const _Parallelism __p) { return __p != sequential; }


  // XXX remove std::duplicates from here if possible,
  // XXX but keep minimal dependencies.

/** @brief Calculates the rounded-down logarithm of @c n for base 2.
  *  @param n Argument.
  *  @return Returns 0 for any argument <1.
  */
template<typename Size>
  inline Size
  __log2(Size n)
    {
      Size k;
      for (k = 0; n > 1; n >>= 1)
        ++k;
      return k;
    }

/** @brief Encode two integers into one __gnu_parallel::lcas_t.
  *  @param a First integer, to be encoded in the most-significant @c
  *  lcas_t_bits/2 bits.
  *  @param b Second integer, to be encoded in the least-significant
  *  @c lcas_t_bits/2 bits.
  *  @return __gnu_parallel::lcas_t value encoding @c a and @c b.
  *  @see decode2
  */
inline lcas_t
encode2(int a, int b)	//must all be non-negative, actually
{
  return (((lcas_t)a) << (lcas_t_bits / 2)) | (((lcas_t)b) << 0);
}

/** @brief Decode two integers from one __gnu_parallel::lcas_t.
  *  @param x __gnu_parallel::lcas_t to decode integers from.
  *  @param a First integer, to be decoded from the most-significant
  *  @c lcas_t_bits/2 bits of @c x.
  *  @param b Second integer, to be encoded in the least-significant
  *  @c lcas_t_bits/2 bits of @c x.
  *  @see encode2
  */
inline void
decode2(lcas_t x, int& a, int& b)
{
  a = (int)((x >> (lcas_t_bits / 2)) & lcas_t_mask);
  b = (int)((x >>               0 ) & lcas_t_mask);
}

/** @brief Equivalent to std::min. */
template<typename T>
  const T&
  min(const T& a, const T& b)
  { return (a < b) ? a : b; }

/** @brief Equivalent to std::max. */
template<typename T>
  const T&
  max(const T& a, const T& b)
  { return (a > b) ? a : b; }

/** @brief Constructs predicate for equality from strict weak
  *  ordering predicate
  */
// XXX comparator at the end, as per others
template<typename Comparator, typename T1, typename T2>
  class equal_from_less : public std::binary_function<T1, T2, bool>
  {
  private:
    Comparator& comp;

  public:
    equal_from_less(Comparator& _comp) : comp(_comp) { }

    bool operator()(const T1& a, const T2& b)
    {
      return !comp(a, b) && !comp(b, a);
    }
  };


/** @brief Similar to std::binder1st,
  *  but giving the argument types explicitly. */
template<typename _Predicate, typename argument_type>
  class unary_negate
  : public std::unary_function<argument_type, bool>
  {
  protected:
    _Predicate _M_pred;

  public:
    explicit
    unary_negate(const _Predicate& __x) : _M_pred(__x) { }

    bool
    operator()(const argument_type& __x)
    { return !_M_pred(__x); }
  };

/** @brief Similar to std::binder1st,
  *  but giving the argument types explicitly. */
template<typename _Operation, typename first_argument_type,
	 typename second_argument_type, typename result_type>
  class binder1st
  : public std::unary_function<second_argument_type, result_type>
  {
  protected:
    _Operation op;
    first_argument_type value;

  public:
    binder1st(const _Operation& __x,
              const first_argument_type& __y)
    : op(__x), value(__y) { }

    result_type
    operator()(const second_argument_type& __x)
    { return op(value, __x); }

    // _GLIBCXX_RESOLVE_LIB_DEFECTS
    // 109.  Missing binders for non-const sequence elements
    result_type
    operator()(second_argument_type& __x) const
    { return op(value, __x); }
  };

/**
  *  @brief Similar to std::binder2nd, but giving the argument types
  *  explicitly.
  */
template<typename _Operation, typename first_argument_type,
	 typename second_argument_type, typename result_type>
  class binder2nd
  : public std::unary_function<first_argument_type, result_type>
  {
  protected:
    _Operation op;
    second_argument_type value;

  public:
    binder2nd(const _Operation& __x,
              const second_argument_type& __y)
    : op(__x), value(__y) { }

    result_type
    operator()(const first_argument_type& __x) const
    { return op(__x, value); }

    // _GLIBCXX_RESOLVE_LIB_DEFECTS
    // 109.  Missing binders for non-const sequence elements
    result_type
    operator()(first_argument_type& __x)
    { return op(__x, value); }
  };

/** @brief Similar to std::equal_to, but allows two different types. */
template<typename T1, typename T2>
  struct equal_to : std::binary_function<T1, T2, bool>
  {
    bool operator()(const T1& t1, const T2& t2) const
    { return t1 == t2; }
  };

/** @brief Similar to std::less, but allows two different types. */
template<typename T1, typename T2>
  struct less : std::binary_function<T1, T2, bool>
  {
    bool
    operator()(const T1& t1, const T2& t2) const
    { return t1 < t2; }

    bool
    operator()(const T2& t2, const T1& t1) const
    { return t2 < t1; }
  };

// Partial specialization for one type. Same as std::less.
template<typename _Tp>
struct less<_Tp, _Tp> : public std::binary_function<_Tp, _Tp, bool>
  {
    bool
    operator()(const _Tp& __x, const _Tp& __y) const
    { return __x < __y; }
  };


  /** @brief Similar to std::plus, but allows two different types. */
template<typename _Tp1, typename _Tp2>
  struct plus : public std::binary_function<_Tp1, _Tp2, _Tp1>
  {
    typedef __typeof__(*static_cast<_Tp1*>(NULL)
		       + *static_cast<_Tp2*>(NULL)) result;

    result
    operator()(const _Tp1& __x, const _Tp2& __y) const
    { return __x + __y; }
  };

// Partial specialization for one type. Same as std::plus.
template<typename _Tp>
  struct plus<_Tp, _Tp> : public std::binary_function<_Tp, _Tp, _Tp>
  {
    typedef __typeof__(*static_cast<_Tp*>(NULL)
		       + *static_cast<_Tp*>(NULL)) result;

    result
    operator()(const _Tp& __x, const _Tp& __y) const
    { return __x + __y; }
  };


/** @brief Similar to std::multiplies, but allows two different types. */
template<typename _Tp1, typename _Tp2>
  struct multiplies : public std::binary_function<_Tp1, _Tp2, _Tp1>
  {
    typedef __typeof__(*static_cast<_Tp1*>(NULL)
		       * *static_cast<_Tp2*>(NULL)) result;

    result
    operator()(const _Tp1& __x, const _Tp2& __y) const
    { return __x * __y; }
  };

// Partial specialization for one type. Same as std::multiplies.
template<typename _Tp>
  struct multiplies<_Tp, _Tp> : public std::binary_function<_Tp, _Tp, _Tp>
  {
    typedef __typeof__(*static_cast<_Tp*>(NULL)
		       * *static_cast<_Tp*>(NULL)) result;

    result
    operator()(const _Tp& __x, const _Tp& __y) const
    { return __x * __y; }
  };


template<typename T, typename _DifferenceTp>
  class pseudo_sequence;

/** @brief Iterator associated with __gnu_parallel::pseudo_sequence.
  *  If features the usual random-access iterator functionality.
  *  @param T Sequence value type.
  *  @param difference_type Sequence difference type.
  */
template<typename T, typename _DifferenceTp>
  class pseudo_sequence_iterator
  {
  public:
    typedef _DifferenceTp difference_type;

  private:
    typedef pseudo_sequence_iterator<T, _DifferenceTp> type;

    const T& val;
    difference_type pos;

  public:
    pseudo_sequence_iterator(const T& val, difference_type pos)
    : val(val), pos(pos) { }

    // Pre-increment operator.
    type&
    operator++()
    {
      ++pos;
      return *this;
    }

    // Post-increment operator.
    const type
    operator++(int)
    { return type(pos++); }

    const T&
    operator*() const
    { return val; }

    const T&
    operator[](difference_type) const
    { return val; }

    bool
    operator==(const type& i2)
    { return pos == i2.pos; }

    difference_type
    operator!=(const type& i2)
    { return pos != i2.pos; }

    difference_type
    operator-(const type& i2)
    { return pos - i2.pos; }
  };

/** @brief Sequence that conceptually consists of multiple copies of
    the same element.
  *  The copies are not stored explicitly, of course.
  *  @param T Sequence value type.
  *  @param difference_type Sequence difference type.
  */
template<typename T, typename _DifferenceTp>
  class pseudo_sequence
  {
    typedef pseudo_sequence<T, _DifferenceTp> type;

  public:
    typedef _DifferenceTp difference_type;

    // Better case down to uint64, than up to _DifferenceTp.
    typedef pseudo_sequence_iterator<T, uint64> iterator;

    /** @brief Constructor.
      *  @param val Element of the sequence.
      *  @param count Number of (virtual) copies.
      */
    pseudo_sequence(const T& val, difference_type count)
    : val(val), count(count)  { }

    /** @brief Begin iterator. */
    iterator
    begin() const
    { return iterator(val, 0); }

    /** @brief End iterator. */
    iterator
    end() const
    { return iterator(val, count); }

  private:
    const T& val;
    difference_type count;
  };

/** @brief Functor that does nothing */
template<typename _ValueTp>
  class void_functor
  {
    inline void
    operator()(const _ValueTp& v) const { }
  };

/** @brief Compute the median of three referenced elements,
    according to @c comp.
  *  @param a First iterator.
  *  @param b Second iterator.
  *  @param c Third iterator.
  *  @param comp Comparator.
  */
template<typename RandomAccessIterator, typename Comparator>
  RandomAccessIterator
  median_of_three_iterators(RandomAccessIterator a, RandomAccessIterator b,
                            RandomAccessIterator c, Comparator& comp)
  {
    if (comp(*a, *b))
      if (comp(*b, *c))
        return b;
      else
        if (comp(*a, *c))
          return c;
        else
          return a;
    else
      {
        // Just swap a and b.
        if (comp(*a, *c))
          return a;
        else
          if (comp(*b, *c))
            return c;
          else
            return b;
      }
  }

#define _GLIBCXX_PARALLEL_ASSERT(_Condition) __glibcxx_assert(_Condition)

} //namespace __gnu_parallel

#endif /* _GLIBCXX_PARALLEL_BASE_H */
