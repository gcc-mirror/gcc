// Temporary buffer implementation -*- C++ -*-

// Copyright (C) 2001 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

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
 * Copyright (c) 1996,1997
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

/* NOTE: This is an internal header file, included by other STL headers.
 *   You should not attempt to use it directly.
 */

#ifndef __SGI_STL_INTERNAL_TEMPBUF_H
#define __SGI_STL_INTERNAL_TEMPBUF_H

namespace std
{

template <class _Tp>
pair<_Tp*, ptrdiff_t> 
__get_temporary_buffer(ptrdiff_t __len, _Tp*)
{
  if (__len > ptrdiff_t(INT_MAX / sizeof(_Tp)))
    __len = INT_MAX / sizeof(_Tp);

  while (__len > 0) {
    _Tp* __tmp = (_Tp*) malloc((size_t)__len * sizeof(_Tp));
    if (__tmp != 0)
      return pair<_Tp*, ptrdiff_t>(__tmp, __len);
    __len /= 2;
  }

  return pair<_Tp*, ptrdiff_t>((_Tp*)0, 0);
}

template <class _Tp>
inline pair<_Tp*, ptrdiff_t> get_temporary_buffer(ptrdiff_t __len) {
  return __get_temporary_buffer(__len, (_Tp*) 0);
}

// This overload is not required by the standard; it is an extension.
// It is supported for backward compatibility with the HP STL, and
// because not all compilers support the language feature (explicit
// function template arguments) that is required for the standard
// version of get_temporary_buffer.
template <class _Tp>
inline pair<_Tp*, ptrdiff_t> get_temporary_buffer(ptrdiff_t __len, _Tp*) {
  return __get_temporary_buffer(__len, (_Tp*) 0);
}

template <class _Tp>
void return_temporary_buffer(_Tp* __p) {
  free(__p);
}

template <class _ForwardIterator, class _Tp>
class _Temporary_buffer {
private:
  ptrdiff_t  _M_original_len;
  ptrdiff_t  _M_len;
  _Tp*       _M_buffer;

  void _M_allocate_buffer() {
    _M_original_len = _M_len;
    _M_buffer = 0;

    if (_M_len > (ptrdiff_t)(INT_MAX / sizeof(_Tp)))
      _M_len = INT_MAX / sizeof(_Tp);

    while (_M_len > 0) {
      _M_buffer = (_Tp*) malloc(_M_len * sizeof(_Tp));
      if (_M_buffer)
        break;
      _M_len /= 2;
    }
  }

  void _M_initialize_buffer(const _Tp&, __true_type) {}
  void _M_initialize_buffer(const _Tp& val, __false_type) {
    uninitialized_fill_n(_M_buffer, _M_len, val);
  }

public:
  ptrdiff_t size() const { return _M_len; }
  ptrdiff_t requested_size() const { return _M_original_len; }
  _Tp* begin() { return _M_buffer; }
  _Tp* end() { return _M_buffer + _M_len; }

  _Temporary_buffer(_ForwardIterator __first, _ForwardIterator __last) {
    // Workaround for a __type_traits bug in the pre-7.3 compiler.
    typedef typename __type_traits<_Tp>::has_trivial_default_constructor
            _Trivial;

    __STL_TRY {
      _M_len = 0;
      distance(__first, __last, _M_len);
      _M_allocate_buffer();
      if (_M_len > 0)
        _M_initialize_buffer(*__first, _Trivial());
    }
    __STL_UNWIND(free(_M_buffer); _M_buffer = 0; _M_len = 0);
  }
 
  ~_Temporary_buffer() {  
    _Destroy(_M_buffer, _M_buffer + _M_len);
    free(_M_buffer);
  }

private:
  // Disable copy constructor and assignment operator.
  _Temporary_buffer(const _Temporary_buffer&) {}
  void operator=(const _Temporary_buffer&) {}
};

// Class temporary_buffer is not part of the standard.  It is an extension.

template <class _ForwardIterator, 
          class _Tp 
                    = typename iterator_traits<_ForwardIterator>::value_type
         >
struct temporary_buffer : public _Temporary_buffer<_ForwardIterator, _Tp>
{
  temporary_buffer(_ForwardIterator __first, _ForwardIterator __last)
    : _Temporary_buffer<_ForwardIterator, _Tp>(__first, __last) {}
  ~temporary_buffer() {}
};
    
} // namespace std

#endif /* __SGI_STL_INTERNAL_TEMPBUF_H */

// Local Variables:
// mode:C++
// End:
