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
 * Copyright (c) 1996
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

#ifndef _SGI_STL_ALGOBASE_H
#define _SGI_STL_ALGOBASE_H

#include <string.h>
#include <limits.h>
#include <function.h>
#include <pair.h>
#include <iterator.h>
#include <new.h>
#include <type_traits.h>

template <class ForwardIterator1, class ForwardIterator2, class T>
inline void __iter_swap(ForwardIterator1 a, ForwardIterator2 b, T*) {
    T tmp = *a;
    *a = *b;
    *b = tmp;
}

template <class ForwardIterator1, class ForwardIterator2>
inline void iter_swap(ForwardIterator1 a, ForwardIterator2 b) {
    __iter_swap(a, b, value_type(a));
}

template <class T>
inline void swap(T& a, T& b) {
    T tmp = a;
    a = b;
    b = tmp;
}

#ifdef __BORLANDC__
#include <stdlib.h>
#else

template <class T>
inline const T& min(const T& a, const T& b) {
    return b < a ? b : a;
}

template <class T>
inline const T& max(const T& a, const T& b) {
    return  a < b ? b : a;
}

#endif

template <class T, class Compare>
inline const T& min(const T& a, const T& b, Compare comp) {
    return comp(b, a) ? b : a;
}

template <class T, class Compare>
inline const T& max(const T& a, const T& b, Compare comp) {
    return comp(a, b) ? b : a;
}

template <class InputIterator, class Distance>
inline void __distance(InputIterator first, InputIterator last, Distance& n, 
                       input_iterator_tag) {
    while (first != last) { ++first; ++n; }
}

template <class RandomAccessIterator, class Distance>
inline void __distance(RandomAccessIterator first, RandomAccessIterator last, 
		       Distance& n, random_access_iterator_tag) {
    n += last - first;
}

template <class InputIterator, class Distance>
inline void distance(InputIterator first, InputIterator last, Distance& n) {
    __distance(first, last, n, iterator_category(first));
}

#ifdef __STL_CLASS_PARTIAL_SPECIALIZATION

template <class InputIterator>
inline iterator_traits<InputIterator>::difference_type
__distance(InputIterator first, InputIterator last, input_iterator_tag) {
  iterator_traits<InputIterator>::difference_type n = 0;
  while (first != last) {
    ++first; ++n;
  }
  return n;
}

template <class RandomAccessIterator>
inline iterator_traits<RandomAccessIterator>::difference_type
__distance(RandomAccessIterator first, RandomAccessIterator last,
           random_access_iterator_tag) {
  return last - first;
}

template <class InputIterator>
inline iterator_traits<InputIterator>::difference_type
distance(InputIterator first, InputIterator last) {
  return __distance(first, last,
                    iterator_traits<InputIterator>::iterator_category());
}

#endif /* __STL_CLASS_PARTIAL_SPECIALIZATION */

template <class InputIterator, class Distance>
inline void __advance(InputIterator& i, Distance n, input_iterator_tag) {
    while (n--) ++i;
}

#if defined(__sgi) && !defined(__GNUC__) && (_MIPS_SIM != _MIPS_SIM_ABI32)
#pragma set woff 1183
#endif

template <class BidirectionalIterator, class Distance>
inline void __advance(BidirectionalIterator& i, Distance n, 
                      bidirectional_iterator_tag) {
    if (n >= 0)
	while (n--) ++i;
    else
	while (n++) --i;
}

#if defined(__sgi) && !defined(__GNUC__) && (_MIPS_SIM != _MIPS_SIM_ABI32)
#pragma reset woff 1183
#endif

template <class RandomAccessIterator, class Distance>
inline void __advance(RandomAccessIterator& i, Distance n, 
		      random_access_iterator_tag) {
    i += n;
}

template <class InputIterator, class Distance>
inline void advance(InputIterator& i, Distance n) {
    __advance(i, n, iterator_category(i));
}

template <class InputIterator, class OutputIterator>
inline OutputIterator __copy(InputIterator first, InputIterator last,
                             OutputIterator result, input_iterator_tag)
{
  for ( ; first != last; ++result, ++first)
    *result = *first;
  return result;
}

template <class RandomAccessIterator, class OutputIterator, class Distance>
inline OutputIterator
__copy_d(RandomAccessIterator first, RandomAccessIterator last,
         OutputIterator result, Distance*)
{
  for (Distance n = last - first; n > 0; --n, ++result, ++first) 
    *result = *first;
  return result;
}

template <class RandomAccessIterator, class OutputIterator>
inline OutputIterator 
__copy(RandomAccessIterator first, RandomAccessIterator last,
       OutputIterator result, random_access_iterator_tag)
{
  return __copy_d(first, last, result, distance_type(first));
}

template <class InputIterator, class OutputIterator>
struct __copy_dispatch
{
  OutputIterator operator()(InputIterator first, InputIterator last,
                            OutputIterator result) {
    return __copy(first, last, result, iterator_category(first));
  }
};

#ifdef __STL_CLASS_PARTIAL_SPECIALIZATION 

template <class T>
inline T* __copy_t(const T* first, const T* last, T* result, __true_type) {
  memmove(result, first, sizeof(T) * (last - first));
  return result + (last - first);
}

template <class T>
inline T* __copy_t(const T* first, const T* last, T* result, __false_type) {
  return __copy_d(first, last, result, (ptrdiff_t*) 0);
}

template <class T>
struct __copy_dispatch<T*, T*>
{
  T* operator()(T* first, T* last, T* result) {
    return __copy_t(first, last, result,
                    __type_traits<T>::has_trivial_assignment_operator());
  }
};

template <class T>
struct __copy_dispatch<const T*, T*>
{
  T* operator()(const T* first, const T* last, T* result) {
    return __copy_t(first, last, result, 
                    __type_traits<T>::has_trivial_assignment_operator());
  }
};

#endif /* __STL_CLASS_PARTIAL_SPECIALIZATION */

template <class InputIterator, class OutputIterator>
inline OutputIterator copy(InputIterator first, InputIterator last,
                           OutputIterator result)
{
  return __copy_dispatch<InputIterator,OutputIterator>()(first, last, result);
}

inline char* copy(const char* first, const char* last, char* result) {
  memmove(result, first, last - first);
  return result + (last - first);
}

inline wchar_t* copy(const wchar_t* first, const wchar_t* last,
                     wchar_t* result) {
  memmove(result, first, sizeof(wchar_t) * (last - first));
  return result + (last - first);
}

template <class BidirectionalIterator1, class BidirectionalIterator2>
inline BidirectionalIterator2 __copy_backward(BidirectionalIterator1 first, 
                                              BidirectionalIterator1 last, 
                                              BidirectionalIterator2 result) {
  while (first != last) *--result = *--last;
  return result;
}


template <class BidirectionalIterator1, class BidirectionalIterator2>
struct __copy_backward_dispatch
{
  BidirectionalIterator2 operator()(BidirectionalIterator1 first, 
                                    BidirectionalIterator1 last, 
                                    BidirectionalIterator2 result) {
    return __copy_backward(first, last, result);
  }
};

#ifdef __STL_CLASS_PARTIAL_SPECIALIZATION 

template <class T>
inline T* __copy_backward_t(const T* first, const T* last, T* result,
                            __true_type) {
  const ptrdiff_t N = last - first;
  memmove(result - N, first, sizeof(T) * N);
  return result - N;
}

template <class T>
inline T* __copy_backward_t(const T* first, const T* last, T* result,
                            __false_type) {
  return __copy_backward(first, last, result);
}

template <class T>
struct __copy_backward_dispatch<T*, T*>
{
  T* operator()(T* first, T* last, T* result) {
    return
      __copy_backward_t(first, last, result,
                        __type_traits<T>::has_trivial_assignment_operator());
  }
};

template <class T>
struct __copy_backward_dispatch<const T*, T*>
{
  T* operator()(const T* first, const T* last, T* result) {
    return
      __copy_backward_t(first, last, result,
                        __type_traits<T>::has_trivial_assignment_operator());
  }
};

#endif /* __STL_CLASS_PARTIAL_SPECIALIZATION */

template <class BidirectionalIterator1, class BidirectionalIterator2>
inline BidirectionalIterator2 copy_backward(BidirectionalIterator1 first, 
                                            BidirectionalIterator1 last, 
                                            BidirectionalIterator2 result) {
  return __copy_backward_dispatch<BidirectionalIterator1, 
                                  BidirectionalIterator2>()(first, last, 
                                                            result);
}

template <class InputIterator, class Size, class OutputIterator>
OutputIterator __copy_n(InputIterator first, Size count,
                        OutputIterator result,
                        input_iterator_tag) {
  for ( ; count > 0; --count, ++first, ++result)
    *result = *first;
  return result;
}

template <class RandomAccessIterator, class Size, class OutputIterator>
inline OutputIterator __copy_n(RandomAccessIterator first, Size count,
                               OutputIterator result,
                               random_access_iterator_tag) {
  return copy(first, first + count, result);
}

template <class InputIterator, class Size, class OutputIterator>
inline OutputIterator copy_n(InputIterator first, Size count,
                             OutputIterator result) {
  return __copy_n(first, count, result, iterator_category(first));
}

template <class ForwardIterator, class T>
void fill(ForwardIterator first, ForwardIterator last, const T& value) {
  for ( ; first != last; ++first)
    *first = value;
}

template <class OutputIterator, class Size, class T>
OutputIterator fill_n(OutputIterator first, Size n, const T& value) {
  for ( ; n > 0; --n, ++first)
    *first = value;
  return first;
}

template <class InputIterator1, class InputIterator2>
pair<InputIterator1, InputIterator2> mismatch(InputIterator1 first1,
					      InputIterator1 last1,
					      InputIterator2 first2) {
    while (first1 != last1 && *first1 == *first2) {
	++first1;
	++first2;
    }
    return pair<InputIterator1, InputIterator2>(first1, first2);
}

template <class InputIterator1, class InputIterator2, class BinaryPredicate>
pair<InputIterator1, InputIterator2> mismatch(InputIterator1 first1,
					      InputIterator1 last1,
					      InputIterator2 first2,
					      BinaryPredicate binary_pred) {
    while (first1 != last1 && binary_pred(*first1, *first2)) {
	++first1;
	++first2;
    }
    return pair<InputIterator1, InputIterator2>(first1, first2);
}

template <class InputIterator1, class InputIterator2>
inline bool equal(InputIterator1 first1, InputIterator1 last1,
		  InputIterator2 first2) {
  for ( ; first1 != last1; ++first1, ++first2)
    if (*first1 != *first2)
      return false;
  return true;
}

template <class InputIterator1, class InputIterator2, class BinaryPredicate>
inline bool equal(InputIterator1 first1, InputIterator1 last1,
		  InputIterator2 first2, BinaryPredicate binary_pred) {
  for ( ; first1 != last1; ++first1, ++first2)
    if (!binary_pred(*first1, *first2))
      return false;
  return true;
}

template <class InputIterator1, class InputIterator2>
bool lexicographical_compare(InputIterator1 first1, InputIterator1 last1,
			     InputIterator2 first2, InputIterator2 last2) {
  for ( ; first1 != last1 && first2 != last2; ++first1, ++first2) {
    if (*first1 < *first2)
      return true;
    if (*first2 < *first1)
      return false;
  }
  return first1 == last1 && first2 != last2;
}

template <class InputIterator1, class InputIterator2, class Compare>
bool lexicographical_compare(InputIterator1 first1, InputIterator1 last1,
			     InputIterator2 first2, InputIterator2 last2,
			     Compare comp) {
  for ( ; first1 != last1 && first2 != last2; ++first1, ++first2) {
    if (comp(*first1, *first2))
      return true;
    if (comp(*first2, *first1))
      return false;
  }
  return first1 == last1 && first2 != last2;
}

inline bool 
lexicographical_compare(const unsigned char* first1,
                        const unsigned char* last1,
                        const unsigned char* first2,
                        const unsigned char* last2)
{
  const size_t len1 = last1 - first1;
  const size_t len2 = last2 - first2;
  const int result = memcmp(first1, first2, min(len1, len2));
  return result != 0 ? result < 0 : len1 < len2;
}

inline bool lexicographical_compare(const char* first1, const char* last1,
                                    const char* first2, const char* last2)
{
#if CHAR_MAX == SCHAR_MAX
  return lexicographical_compare((const signed char*) first1,
                                 (const signed char*) last1,
                                 (const signed char*) first2,
                                 (const signed char*) last2);
#else
  return lexicographical_compare((const unsigned char*) first1,
                                 (const unsigned char*) last1,
                                 (const unsigned char*) first2,
                                 (const unsigned char*) last2);
#endif
}

template <class InputIterator1, class InputIterator2>
int lexicographical_compare_3way(InputIterator1 first1, InputIterator1 last1,
                                 InputIterator2 first2, InputIterator2 last2)
{
    while (first1 != last1 && first2 != last2) {
        if (*first1 < *first2) return -1;
        if (*first2 < *first1) return 1;
	++first1; ++first2;
    }
    if (first2 == last2) {
	return !(first1 == last1);
    } else {
        return -1;
    }
}

inline int
lexicographical_compare_3way(const unsigned char* first1,
                             const unsigned char* last1,
                             const unsigned char* first2,
                             const unsigned char* last2)
{
  const int len1 = last1 - first1;
  const int len2 = last2 - first2;
  const int result = memcmp(first1, first2, min(len1, len2));
  return result == 0 ?  len1 - len2 : result;
}

inline int lexicographical_compare_3way(const char* first1, const char* last1,
                                        const char* first2, const char* last2)
{
#if CHAR_MAX == SCHAR_MAX
  return lexicographical_compare_3way(
				(const signed char*) first1,
                                (const signed char*) last1,
                                (const signed char*) first2,
                                (const signed char*) last2);
#else
  return lexicographical_compare_3way((const unsigned char*) first1,
                                      (const unsigned char*) last1,
                                      (const unsigned char*) first2,
                                      (const unsigned char*) last2);
#endif
}

template <class T>
inline void destroy(T* pointer) {
    pointer->~T();
}

template <class T1, class T2>
inline void construct(T1* p, const T2& value) {
    new (p) T1(value);
}

template <class ForwardIterator>
inline void
__destroy_aux(ForwardIterator first, ForwardIterator last, __false_type) {
  for ( ; first < last; ++first)
    destroy(&*first);
}

template <class ForwardIterator> 
inline void __destroy_aux(ForwardIterator, ForwardIterator, __true_type) {
}

template <class ForwardIterator, class T>
inline void __destroy(ForwardIterator first, ForwardIterator last, T*) {
  __destroy_aux(first, last, __type_traits<T>::has_trivial_destructor());
}

template <class ForwardIterator>
inline void destroy(ForwardIterator first, ForwardIterator last) {
  __destroy(first, last, value_type(first));
}

inline void destroy(char*, char*) {}
inline void destroy(wchar_t*, wchar_t*) {}

// Valid if copy construction is equivalent to assignment, and if the
//  destructor is trivial.
template <class InputIterator, class ForwardIterator>
inline ForwardIterator 
__uninitialized_copy_aux(InputIterator first, InputIterator last,
                         ForwardIterator result,
                         __true_type) {
  return copy(first, last, result);
}

template <class InputIterator, class ForwardIterator>
ForwardIterator 
__uninitialized_copy_aux(InputIterator first, InputIterator last,
                         ForwardIterator result,
                         __false_type) {
  ForwardIterator cur = result;
#     ifdef __STL_USE_EXCEPTIONS
  try {
#     endif /* __STL_USE_EXCEPTIONS */
    for ( ; first != last; ++first, ++cur)
      construct(&*cur, *first);
    return cur;
#     ifdef __STL_USE_EXCEPTIONS
  }
  catch(...) {
    destroy(result, cur);
    throw;
  }
#     endif /* __STL_USE_EXCEPTIONS */
}


template <class InputIterator, class ForwardIterator, class T>
inline ForwardIterator
__uninitialized_copy(InputIterator first, InputIterator last,
                     ForwardIterator result, T*) {
  return __uninitialized_copy_aux(first, last, result,
                                  __type_traits<T>::is_POD_type());
}

template <class InputIterator, class ForwardIterator>
inline ForwardIterator
  uninitialized_copy(InputIterator first, InputIterator last,
                     ForwardIterator result) {
  return __uninitialized_copy(first, last, result, value_type(result));
}

inline char* uninitialized_copy(const char* first, const char* last,
                                char* result) {
  memmove(result, first, last - first);
  return result + (last - first);
}

inline wchar_t* uninitialized_copy(const wchar_t* first, const wchar_t* last,
                                   wchar_t* result) {
  memmove(result, first, sizeof(wchar_t) * (last - first));
  return result + (last - first);
}

template <class InputIterator, class Size, class ForwardIterator>
ForwardIterator __uninitialized_copy_n(InputIterator first, Size count,
                                       ForwardIterator result,
                                       input_iterator_tag) {
  ForwardIterator cur = result;
#     ifdef __STL_USE_EXCEPTIONS
  try {
#     endif /* __STL_USE_EXCEPTIONS */
    for ( ; count > 0 ; --count, ++first, ++cur) 
      construct(&*cur, *first);
    return cur;
#     ifdef __STL_USE_EXCEPTIONS
  }
  catch(...) {
    destroy(result, cur);
    throw;
  }
#     endif /* __STL_USE_EXCEPTIONS */
}

template <class RandomAccessIterator, class Size, class ForwardIterator>
inline ForwardIterator
__uninitialized_copy_n(RandomAccessIterator first, Size count,
                       ForwardIterator result,
                       random_access_iterator_tag) {
  return uninitialized_copy(first, first + count, result);
}

template <class InputIterator, class Size, class ForwardIterator>
inline ForwardIterator uninitialized_copy_n(InputIterator first, Size count,
                                            ForwardIterator result) {
  return __uninitialized_copy_n(first, count, result,
                                iterator_category(first));
}

// Valid if copy construction is equivalent to assignment, and if the
//  destructor is trivial.
template <class ForwardIterator, class T>
inline void
__uninitialized_fill_aux(ForwardIterator first, ForwardIterator last, 
                         const T& x, __true_type)
{
  fill(first, last, x);
}

template <class ForwardIterator, class T>
void
__uninitialized_fill_aux(ForwardIterator first, ForwardIterator last, 
                         const T& x, __false_type)
{
  ForwardIterator cur = first;
#     ifdef __STL_USE_EXCEPTIONS
  try {
#     endif /* __STL_USE_EXCEPTIONS */
    for ( ; cur != last; ++cur)
      construct(&*cur, x);
#     ifdef __STL_USE_EXCEPTIONS
  }
  catch(...) {
    destroy(first, cur);
    throw;
  }
#     endif /* __STL_USE_EXCEPTIONS */
}

template <class ForwardIterator, class T, class T1>
inline void __uninitialized_fill(ForwardIterator first, ForwardIterator last, 
                                 const T& x, T1*) {
  __uninitialized_fill_aux(first, last, x,
                           __type_traits<T1>::is_POD_type());
}

template <class ForwardIterator, class T>
inline void uninitialized_fill(ForwardIterator first, ForwardIterator last, 
                               const T& x) {
  __uninitialized_fill(first, last, x, value_type(first));
}

// Valid if copy construction is equivalent to assignment, and if the
//  destructor is trivial.
template <class ForwardIterator, class Size, class T>
inline ForwardIterator
__uninitialized_fill_n_aux(ForwardIterator first, Size n,
                           const T& x, __true_type) {
  return fill_n(first, n, x);
}

template <class ForwardIterator, class Size, class T>
ForwardIterator
__uninitialized_fill_n_aux(ForwardIterator first, Size n,
                           const T& x, __false_type) {
  ForwardIterator cur = first;
#     ifdef __STL_USE_EXCEPTIONS
  try {
#     endif /* __STL_USE_EXCEPTIONS */
    for ( ; n > 0; --n, ++cur)
      construct(&*cur, x);
    return cur;
#     ifdef __STL_USE_EXCEPTIONS
  }
  catch(...) {
    destroy(first, cur);
    throw;
  }
#     endif /* __STL_USE_EXCEPTIONS */
}

template <class ForwardIterator, class Size, class T, class T1>
inline ForwardIterator __uninitialized_fill_n(ForwardIterator first, Size n,
                                              const T& x, T1*) {
  return __uninitialized_fill_n_aux(first, n, x,
                                    __type_traits<T1>::is_POD_type());
}

template <class ForwardIterator, class Size, class T>
inline ForwardIterator uninitialized_fill_n(ForwardIterator first, Size n,
                                            const T& x) {
  return __uninitialized_fill_n(first, n, x, value_type(first));
}

// Copies [first1, last1) into [result, result + (last1 - first1)), and
//  copies [first2, last2) into
//  [result, result + (last1 - first1) + (last2 - first2)).

template <class InputIterator1, class InputIterator2, class ForwardIterator>
inline ForwardIterator
__uninitialized_copy_copy(InputIterator1 first1, InputIterator1 last1,
                          InputIterator2 first2, InputIterator2 last2,
                          ForwardIterator result) {
  ForwardIterator mid = uninitialized_copy(first1, last1, result);
#     ifdef __STL_USE_EXCEPTIONS
  try {
#     endif /* __STL_USE_EXCEPTIONS */
    return uninitialized_copy(first2, last2, mid);
#     ifdef __STL_USE_EXCEPTIONS
  }
  catch(...) {
    destroy(result, mid);
    throw;
  }
#     endif /* __STL_USE_EXCEPTIONS */
}

// Fills [result, mid) with x, and copies [first, last) into
//  [mid, mid + (last - first)).
template <class ForwardIterator, class T, class InputIterator>
inline ForwardIterator 
__uninitialized_fill_copy(ForwardIterator result, ForwardIterator mid,
                          const T& x,
                          InputIterator first, InputIterator last) {
  uninitialized_fill(result, mid, x);
#     ifdef __STL_USE_EXCEPTIONS
  try {
#     endif /* __STL_USE_EXCEPTIONS */
    return uninitialized_copy(first, last, mid);
#     ifdef __STL_USE_EXCEPTIONS
  }
  catch(...) {
    destroy(result, mid);
    throw;
  }
#     endif /* __STL_USE_EXCEPTIONS */
}

// Copies [first1, last1) into [first2, first2 + (last1 - first1)), and
//  fills [first2 + (last1 - first1), last2) with x.
template <class InputIterator, class ForwardIterator, class T>
inline void
__uninitialized_copy_fill(InputIterator first1, InputIterator last1,
                          ForwardIterator first2, ForwardIterator last2,
                          const T& x) {
  ForwardIterator mid2 = uninitialized_copy(first1, last1, first2);
#     ifdef __STL_USE_EXCEPTIONS
  try {
#     endif /* __STL_USE_EXCEPTIONS */
    uninitialized_fill(mid2, last2, x);
#     ifdef __STL_USE_EXCEPTIONS
  }
  catch(...) {
    destroy(first2, mid2);
    throw;
  }
#     endif /* __STL_USE_EXCEPTIONS */
}

#endif /* _SGI_STL_ALGOBASE_H */
