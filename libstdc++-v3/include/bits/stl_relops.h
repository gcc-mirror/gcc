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
 * Copyright (c) 1996,1997
 * Silicon Graphics
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 */

/* NOTE: This is an internal header file, included by other STL headers.
 *   You should not attempt to use it directly.
 */

/**** libstdc++-v3 note:  Inclusion of this file has been removed from
 * all of the other STL headers for safety reasons, except std_utility.h.
 * For more information, see the thread of about twenty messages starting
 * with <URL:http://gcc.gnu.org/ml/libstdc++/2001-01/msg00223.html>, or the
 * FAQ at <URL:http://gcc.gnu.org/onlinedocs/libstdc++/faq/index.html#4_4>.
 *
 * Short summary:  the rel_ops operators cannot be made to play nice.
 * Don't use them.
*/

#ifndef _CPP_BITS_STL_RELOPS_H
#define _CPP_BITS_STL_RELOPS_H 1

namespace std
{
  namespace rel_ops
  {

template <class _Tp>
inline bool operator!=(const _Tp& __x, const _Tp& __y) {
  return !(__x == __y);
}

template <class _Tp>
inline bool operator>(const _Tp& __x, const _Tp& __y) {
  return __y < __x;
}

template <class _Tp>
inline bool operator<=(const _Tp& __x, const _Tp& __y) {
  return !(__y < __x);
}

template <class _Tp>
inline bool operator>=(const _Tp& __x, const _Tp& __y) {
  return !(__x < __y);
}

  } // namespace rel_ops
} // namespace std

#endif /* _CPP_BITS_STL_RELOPS_H */

// Local Variables:
// mode:C++
// End:
