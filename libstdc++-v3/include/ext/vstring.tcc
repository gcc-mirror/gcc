// Versatile string -*- C++ -*-

// Copyright (C) 2005 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

/** @file ext/vstring.tcc
 *  This file is a GNU extension to the Standard C++ Library.
 *  This is an internal header file, included by other library headers.
 *  You should not attempt to use it directly.
 */

#ifndef _VSTRING_TCC
#define _VSTRING_TCC 1

#pragma GCC system_header

namespace __gnu_cxx
{
  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    const typename __versa_string<_CharT, _Traits, _Alloc, _Base>::size_type
    __versa_string<_CharT, _Traits, _Alloc, _Base>::npos;

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    __versa_string<_CharT, _Traits, _Alloc, _Base>&
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    assign(const _CharT* __s, size_type __n)
    {
      __glibcxx_requires_string_len(__s, __n);
      _M_check_length(this->size(), __n, "__versa_string::assign");
      if (_M_disjunct(__s) || this->_M_is_shared())
	return _M_replace_safe(size_type(0), this->size(), __s, __n);
      else
	{
	  // Work in-place.
	  const size_type __pos = __s - this->_M_data();
	  if (__pos >= __n)
	    this->_S_copy(this->_M_data(), __s, __n);
	  else if (__pos)
	    this->_S_move(this->_M_data(), __s, __n);
	  this->_M_set_length(__n);
	  return *this;
	}
     }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    __versa_string<_CharT, _Traits, _Alloc, _Base>&
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    append(size_type __n, _CharT __c)
    {
      if (__n)
	{
	  _M_check_length(size_type(0), __n, "__versa_string::append");	  
	  const size_type __len = __n + this->size();
	  if (__len > this->capacity() || this->_M_is_shared())
	    this->reserve(__len);
	  this->_S_assign(this->_M_data() + this->size(), __n, __c);
	  this->_M_set_length(__len);
	}
      return *this;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    __versa_string<_CharT, _Traits, _Alloc, _Base>&
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    append(const _CharT* __s, size_type __n)
    {
      __glibcxx_requires_string_len(__s, __n);
      if (__n)
	{
	  _M_check_length(size_type(0), __n, "__versa_string::append");
	  const size_type __len = __n + this->size();
	  if (__len > this->capacity() || this->_M_is_shared())
	    {
	      if (_M_disjunct(__s))
		this->reserve(__len);
	      else
		{
		  const size_type __off = __s - this->_M_data();
		  this->reserve(__len);
		  __s = this->_M_data() + __off;
		}
	    }
	  this->_S_copy(this->_M_data() + this->size(), __s, __n);
	  this->_M_set_length(__len);
	}
      return *this;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    __versa_string<_CharT, _Traits, _Alloc, _Base>&
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    append(const __versa_string& __str)
    {
      const size_type __size = __str.size();
      if (__size)
	{
	  const size_type __len = __size + this->size();
	  if (__len > this->capacity() || this->_M_is_shared())
	    this->reserve(__len);
	  this->_S_copy(this->_M_data() + this->size(), __str._M_data(),
			__size);
	  this->_M_set_length(__len);
	}
      return *this;
    }    

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    __versa_string<_CharT, _Traits, _Alloc, _Base>&
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    append(const __versa_string& __str, size_type __pos, size_type __n)
    {
      __str._M_check(__pos, "__versa_string::append");
      __n = __str._M_limit(__pos, __n);
      if (__n)
	{
	  const size_type __len = __n + this->size();
	  if (__len > this->capacity() || this->_M_is_shared())
	    this->reserve(__len);
	  this->_S_copy(this->_M_data() + this->size(),
			__str._M_data() + __pos, __n);
	  this->_M_set_length(__len);	  
	}
      return *this;
    }

   template<typename _CharT, typename _Traits, typename _Alloc,
	    template <typename, typename, typename> class _Base>
     __versa_string<_CharT, _Traits, _Alloc, _Base>&
     __versa_string<_CharT, _Traits, _Alloc, _Base>::
     insert(size_type __pos, const _CharT* __s, size_type __n)
     {
       __glibcxx_requires_string_len(__s, __n);
       _M_check(__pos, "__versa_string::insert");
       _M_check_length(size_type(0), __n, "__versa_string::insert");
       if (_M_disjunct(__s) || this->_M_is_shared())
         return _M_replace_safe(__pos, size_type(0), __s, __n);
       else
         {
           // Work in-place.
           const size_type __off = __s - this->_M_data();
           this->_M_mutate(__pos, 0, __n);
           __s = this->_M_data() + __off;
           _CharT* __p = this->_M_data() + __pos;
           if (__s  + __n <= __p)
             this->_S_copy(__p, __s, __n);
           else if (__s >= __p)
             this->_S_copy(__p, __s + __n, __n);
           else
             {
	       const size_type __nleft = __p - __s;
               this->_S_copy(__p, __s, __nleft);
               this->_S_copy(__p + __nleft, __p + __n, __n - __nleft);
             }
           return *this;
         }
     }

   template<typename _CharT, typename _Traits, typename _Alloc,
	    template <typename, typename, typename> class _Base>
     __versa_string<_CharT, _Traits, _Alloc, _Base>&
     __versa_string<_CharT, _Traits, _Alloc, _Base>::
     replace(size_type __pos, size_type __n1, const _CharT* __s,
	     size_type __n2)
     {
       __glibcxx_requires_string_len(__s, __n2);
       _M_check(__pos, "__versa_string::replace");
       __n1 = _M_limit(__pos, __n1);
       _M_check_length(__n1, __n2, "__versa_string::replace");
       bool __left;
       if (_M_disjunct(__s) || this->_M_is_shared())
         return _M_replace_safe(__pos, __n1, __s, __n2);
       else if ((__left = __s + __n2 <= this->_M_data() + __pos)
		|| this->_M_data() + __pos + __n1 <= __s)
	 {
	   // Work in-place: non-overlapping case.
	   size_type __off = __s - this->_M_data();
	   __left ? __off : (__off += __n2 - __n1);
	   this->_M_mutate(__pos, __n1, __n2);
	   this->_S_copy(this->_M_data() + __pos,
			 this->_M_data() + __off, __n2);
	   return *this;
	 }
       else
	 {
	   // Todo: overlapping case.
	   const __versa_string __tmp(__s, __n2);
	   return _M_replace_safe(__pos, __n1, __tmp._M_data(), __n2);
	 }
     }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    void
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    swap(__versa_string& __s)
    {
      if (this->_M_is_leaked())
	this->_M_set_sharable();
      if (__s._M_is_leaked())
	__s._M_set_sharable();
      if (this->get_allocator() == __s.get_allocator())
	this->_M_swap(__s);
      // The code below can usually be optimized away.
      else
	{
	  const __versa_string __tmp1(_M_ibegin(), _M_iend(),
				      __s.get_allocator());
	  const __versa_string __tmp2(__s._M_ibegin(), __s._M_iend(),
				      this->get_allocator());
	  *this = __tmp2;
	  __s = __tmp1;
	}
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    void
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    resize(size_type __n, _CharT __c)
    {
      const size_type __size = this->size();
      _M_check_length(__size, __n, "__versa_string::resize");
      if (__size < __n)
	this->append(__n - __size, __c);
      else if (__n < __size)
	this->erase(__n);
      // else nothing (in particular, avoid calling _M_mutate() unnecessarily.)
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    template<typename _InputIterator>
      __versa_string<_CharT, _Traits, _Alloc, _Base>&
      __versa_string<_CharT, _Traits, _Alloc, _Base>::
      _M_replace_dispatch(iterator __i1, iterator __i2, _InputIterator __k1,
			  _InputIterator __k2, __false_type)
      {
	const __versa_string __s(__k1, __k2);
	const size_type __n1 = __i2 - __i1;
	_M_check_length(__n1, __s.size(),
			"__versa_string::_M_replace_dispatch");
	return _M_replace_safe(__i1 - _M_ibegin(), __n1, __s._M_data(),
			       __s.size());
      }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    __versa_string<_CharT, _Traits, _Alloc, _Base>&
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    _M_replace_aux(size_type __pos1, size_type __n1, size_type __n2,
		   _CharT __c)
    {
      _M_check_length(__n1, __n2, "__versa_string::_M_replace_aux");
      this->_M_mutate(__pos1, __n1, __n2);
      if (__n2)
	this->_S_assign(this->_M_data() + __pos1, __n2, __c);
      return *this;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    __versa_string<_CharT, _Traits, _Alloc, _Base>&
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    _M_replace_safe(size_type __pos1, size_type __n1, const _CharT* __s,
		    size_type __n2)
    {
      this->_M_mutate(__pos1, __n1, __n2);
      if (__n2)
	this->_S_copy(this->_M_data() + __pos1, __s, __n2);
      return *this;
    }
   
  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    __versa_string<_CharT, _Traits, _Alloc, _Base>
    operator+(const _CharT* __lhs,
	      const __versa_string<_CharT, _Traits, _Alloc>& __rhs)
    {
      __glibcxx_requires_string(__lhs);
      typedef __versa_string<_CharT, _Traits, _Alloc> __string_type;
      typedef typename __string_type::size_type	  __size_type;
      const __size_type __len = _Traits::length(__lhs);
      __string_type __str;
      __str.reserve(__len + __rhs.size());
      __str.append(__lhs, __len);
      __str.append(__rhs);
      return __str;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    __versa_string<_CharT, _Traits, _Alloc, _Base>
    operator+(_CharT __lhs,
	      const __versa_string<_CharT, _Traits, _Alloc, _Base>& __rhs)
    {
      typedef __versa_string<_CharT, _Traits, _Alloc> __string_type;
      typedef typename __string_type::size_type	  __size_type;
      __string_type __str;
      const __size_type __len = __rhs.size();
      __str.reserve(__len + 1);
      __str.append(__size_type(1), __lhs);
      __str.append(__rhs);
      return __str;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    typename __versa_string<_CharT, _Traits, _Alloc, _Base>::size_type
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    copy(_CharT* __s, size_type __n, size_type __pos) const
    {
      _M_check(__pos, "__versa_string::copy");
      __n = _M_limit(__pos, __n);
      __glibcxx_requires_string_len(__s, __n);
      if (__n)
	this->_S_copy(__s, this->_M_data() + __pos, __n);
      // 21.3.5.7 par 3: do not append null.  (good.)
      return __n;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    typename __versa_string<_CharT, _Traits, _Alloc, _Base>::size_type
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    find(const _CharT* __s, size_type __pos, size_type __n) const
    {
      __glibcxx_requires_string_len(__s, __n);
      size_type __ret = npos;
      const size_type __size = this->size();
      if (__pos + __n <= __size)
	{
	  const _CharT* __data = this->_M_data();
	  const _CharT* __p = std::search(__data + __pos, __data + __size,
					  __s, __s + __n, traits_type::eq);
	  if (__p != __data + __size || __n == 0)
	    __ret = __p - __data;
	}
      return __ret;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    typename __versa_string<_CharT, _Traits, _Alloc, _Base>::size_type
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    find(_CharT __c, size_type __pos) const
    {
      size_type __ret = npos;
      const size_type __size = this->size();
      if (__pos < __size)
	{
	  const _CharT* __data = this->_M_data();
	  const size_type __n = __size - __pos;
	  const _CharT* __p = traits_type::find(__data + __pos, __n, __c);
	  if (__p)
	    __ret = __p - __data;
	}
      return __ret;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    typename __versa_string<_CharT, _Traits, _Alloc, _Base>::size_type
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    rfind(const _CharT* __s, size_type __pos, size_type __n) const
    {
      __glibcxx_requires_string_len(__s, __n);
      const size_type __size = this->size();
      if (__n <= __size)
	{
	  __pos = std::min(size_type(__size - __n), __pos);
	  const _CharT* __data = this->_M_data();
	  do
	    {
	      if (traits_type::compare(__data + __pos, __s, __n) == 0)
		return __pos;
	    }
	  while (__pos-- > 0);
	}
      return npos;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    typename __versa_string<_CharT, _Traits, _Alloc, _Base>::size_type
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    rfind(_CharT __c, size_type __pos) const
    {
      size_type __size = this->size();
      if (__size)
	{
	  if (--__size > __pos)
	    __size = __pos;
	  for (++__size; __size-- > 0; )
	    if (traits_type::eq(this->_M_data()[__size], __c))
	      return __size;
	}
      return npos;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    typename __versa_string<_CharT, _Traits, _Alloc, _Base>::size_type
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    find_first_of(const _CharT* __s, size_type __pos, size_type __n) const
    {
      __glibcxx_requires_string_len(__s, __n);
      for (; __n && __pos < this->size(); ++__pos)
	{
	  const _CharT* __p = traits_type::find(__s, __n,
						this->_M_data()[__pos]);
	  if (__p)
	    return __pos;
	}
      return npos;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    typename __versa_string<_CharT, _Traits, _Alloc, _Base>::size_type
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    find_last_of(const _CharT* __s, size_type __pos, size_type __n) const
    {
      __glibcxx_requires_string_len(__s, __n);
      size_type __size = this->size();
      if (__size && __n)
	{
	  if (--__size > __pos)
	    __size = __pos;
	  do
	    {
	      if (traits_type::find(__s, __n, this->_M_data()[__size]))
		return __size;
	    }
	  while (__size-- != 0);
	}
      return npos;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    typename __versa_string<_CharT, _Traits, _Alloc, _Base>::size_type
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    find_first_not_of(const _CharT* __s, size_type __pos, size_type __n) const
    {
      __glibcxx_requires_string_len(__s, __n);
      for (; __pos < this->size(); ++__pos)
	if (!traits_type::find(__s, __n, this->_M_data()[__pos]))
	  return __pos;
      return npos;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    typename __versa_string<_CharT, _Traits, _Alloc, _Base>::size_type
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    find_first_not_of(_CharT __c, size_type __pos) const
    {
      for (; __pos < this->size(); ++__pos)
	if (!traits_type::eq(this->_M_data()[__pos], __c))
	  return __pos;
      return npos;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    typename __versa_string<_CharT, _Traits, _Alloc, _Base>::size_type
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    find_last_not_of(const _CharT* __s, size_type __pos, size_type __n) const
    {
      __glibcxx_requires_string_len(__s, __n);
      size_type __size = this->size();
      if (__size)
	{
	  if (--__size > __pos)
	    __size = __pos;
	  do
	    {
	      if (!traits_type::find(__s, __n, this->_M_data()[__size]))
		return __size;
	    }
	  while (__size--);
	}
      return npos;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    typename __versa_string<_CharT, _Traits, _Alloc, _Base>::size_type
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    find_last_not_of(_CharT __c, size_type __pos) const
    {
      size_type __size = this->size();
      if (__size)
	{
	  if (--__size > __pos)
	    __size = __pos;
	  do
	    {
	      if (!traits_type::eq(this->_M_data()[__size], __c))
		return __size;
	    }
	  while (__size--);
	}
      return npos;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    int
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    compare(size_type __pos, size_type __n, const __versa_string& __str) const
    {
      _M_check(__pos, "__versa_string::compare");
      __n = _M_limit(__pos, __n);
      const size_type __osize = __str.size();
      const size_type __len = std::min(__n, __osize);
      int __r = traits_type::compare(this->_M_data() + __pos,
				     __str.data(), __len);
      if (!__r)
	__r = __n - __osize;
      return __r;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    int
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    compare(size_type __pos1, size_type __n1, const __versa_string& __str,
	    size_type __pos2, size_type __n2) const
    {
      _M_check(__pos1, "__versa_string::compare");
      __str._M_check(__pos2, "__versa_string::compare");
      __n1 = _M_limit(__pos1, __n1);
      __n2 = __str._M_limit(__pos2, __n2);
      const size_type __len = std::min(__n1, __n2);
      int __r = traits_type::compare(this->_M_data() + __pos1,
				     __str.data() + __pos2, __len);
      if (!__r)
	__r = __n1 - __n2;
      return __r;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    int
    __versa_string<_CharT, _Traits, _Alloc, _Base>::
    compare(const _CharT* __s) const
    {
      __glibcxx_requires_string(__s);
      const size_type __size = this->size();
      const size_type __osize = traits_type::length(__s);
      const size_type __len = std::min(__size, __osize);
      int __r = traits_type::compare(this->_M_data(), __s, __len);
      if (!__r)
	__r = __size - __osize;
      return __r;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    int
    __versa_string <_CharT, _Traits, _Alloc, _Base>::
    compare(size_type __pos, size_type __n1, const _CharT* __s) const
    {
      __glibcxx_requires_string(__s);
      _M_check(__pos, "__versa_string::compare");
      __n1 = _M_limit(__pos, __n1);
      const size_type __osize = traits_type::length(__s);
      const size_type __len = std::min(__n1, __osize);
      int __r = traits_type::compare(this->_M_data() + __pos, __s, __len);
      if (!__r)
	__r = __n1 - __osize;
      return __r;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
	   template <typename, typename, typename> class _Base>
    int
    __versa_string <_CharT, _Traits, _Alloc, _Base>::
    compare(size_type __pos, size_type __n1, const _CharT* __s,
	    size_type __n2) const
    {
      __glibcxx_requires_string_len(__s, __n2);
      _M_check(__pos, "__versa_string::compare");
      __n1 = _M_limit(__pos, __n1);
      const size_type __len = std::min(__n1, __n2);
      int __r = traits_type::compare(this->_M_data() + __pos, __s, __len);
      if (!__r)
	__r = __n1 - __n2;
      return __r;
    }

} // namespace __gnu_cxx

namespace std
{
  template<typename _CharT, typename _Traits, typename _Alloc,
           template <typename, typename, typename> class _Base>
    basic_istream<_CharT, _Traits>&
    operator>>(basic_istream<_CharT, _Traits>& __in,
	       __gnu_cxx::__versa_string<_CharT, _Traits,
	                                 _Alloc, _Base>& __str)
    {
      typedef basic_istream<_CharT, _Traits>	        __istream_type;
      typedef typename __istream_type::int_type		__int_type;
      typedef typename __istream_type::__streambuf_type __streambuf_type;
      typedef typename __istream_type::__ctype_type	__ctype_type;
      typedef __gnu_cxx::__versa_string<_CharT, _Traits, _Alloc, _Base>
	                                                __string_type;
      typedef typename __string_type::size_type		__size_type;

      __size_type __extracted = 0;
      ios_base::iostate __err = ios_base::iostate(ios_base::goodbit);
      typename __istream_type::sentry __cerb(__in, false);
      if (__cerb)
	{
	  try
	    {
	      // Avoid reallocation for common case.
	      __str.erase();
	      _CharT __buf[128];
	      __size_type __len = 0;
	      const streamsize __w = __in.width();
	      const __size_type __n = __w > 0 ? static_cast<__size_type>(__w)
		                              : __str.max_size();
	      const __ctype_type& __ct = use_facet<__ctype_type>(__in.getloc());
	      const __int_type __eof = _Traits::eof();
	      __streambuf_type* __sb = __in.rdbuf();
	      __int_type __c = __sb->sgetc();

	      while (__extracted < __n
		     && !_Traits::eq_int_type(__c, __eof)
		     && !__ct.is(ctype_base::space, _Traits::to_char_type(__c)))
		{
		  if (__len == sizeof(__buf) / sizeof(_CharT))
		    {
		      __str.append(__buf, sizeof(__buf) / sizeof(_CharT));
		      __len = 0;
		    }
		  __buf[__len++] = _Traits::to_char_type(__c);
		  ++__extracted;
		  __c = __sb->snextc();
		}
	      __str.append(__buf, __len);

	      if (_Traits::eq_int_type(__c, __eof))
		__err |= ios_base::eofbit;
	      __in.width(0);
	    }
	  catch(...)
	    {
	      // _GLIBCXX_RESOLVE_LIB_DEFECTS
	      // 91. Description of operator>> and getline() for string<>
	      // might cause endless loop
	      __in._M_setstate(ios_base::badbit);
	    }
	}
      // 211.  operator>>(istream&, string&) doesn't set failbit
      if (!__extracted)
	__err |= ios_base::failbit;
      if (__err)
	__in.setstate(__err);
      return __in;
    }      

  template<typename _CharT, typename _Traits, typename _Alloc,
           template <typename, typename, typename> class _Base>
    basic_ostream<_CharT, _Traits>&
    operator<<(basic_ostream<_CharT, _Traits>& __out,
	       const __gnu_cxx::__versa_string<_CharT, _Traits,
	                                       _Alloc, _Base>& __str)
    {
      typedef basic_ostream<_CharT, _Traits>            __ostream_type;

      typename __ostream_type::sentry __cerb(__out);
      if (__cerb)
	{
	  const streamsize __w = __out.width();
	  streamsize __len = static_cast<streamsize>(__str.size());
	  const _CharT* __s = __str.data();

	  // _GLIBCXX_RESOLVE_LIB_DEFECTS
	  // 25. String operator<< uses width() value wrong
	  if (__w > __len)
	    {
	      _CharT* __cs = (static_cast<
			      _CharT*>(__builtin_alloca(sizeof(_CharT) * __w)));
	      __pad<_CharT, _Traits>::_S_pad(__out, __out.fill(), __cs,
					     __s, __w, __len, false);
	      __s = __cs;
	      __len = __w;
	    }
	  __out._M_write(__s, __len);
	  __out.width(0);
	}
      return __out;
    }

  template<typename _CharT, typename _Traits, typename _Alloc,
           template <typename, typename, typename> class _Base>
    basic_istream<_CharT, _Traits>&
    getline(basic_istream<_CharT, _Traits>& __in,
	    __gnu_cxx::__versa_string<_CharT, _Traits, _Alloc, _Base>& __str,
	    _CharT __delim)
    {
      typedef basic_istream<_CharT, _Traits>	        __istream_type;
      typedef typename __istream_type::int_type		__int_type;
      typedef typename __istream_type::__streambuf_type __streambuf_type;
      typedef typename __istream_type::__ctype_type	__ctype_type;
      typedef __gnu_cxx::__versa_string<_CharT, _Traits, _Alloc, _Base>
	                                                __string_type;
      typedef typename __string_type::size_type		__size_type;

      __size_type __extracted = 0;
      const __size_type __n = __str.max_size();
      ios_base::iostate __err = ios_base::iostate(ios_base::goodbit);
      typename __istream_type::sentry __cerb(__in, true);
      if (__cerb)
	{
	  try
	    {
	      // Avoid reallocation for common case.
	      __str.erase();
	      _CharT __buf[128];
	      __size_type __len = 0;
	      const __int_type __idelim = _Traits::to_int_type(__delim);
	      const __int_type __eof = _Traits::eof();
	      __streambuf_type* __sb = __in.rdbuf();
	      __int_type __c = __sb->sgetc();

	      while (__extracted < __n
		     && !_Traits::eq_int_type(__c, __eof)
		     && !_Traits::eq_int_type(__c, __idelim))
		{
		  if (__len == sizeof(__buf) / sizeof(_CharT))
		    {
		      __str.append(__buf, sizeof(__buf) / sizeof(_CharT));
		      __len = 0;
		    }
		  __buf[__len++] = _Traits::to_char_type(__c);
		  ++__extracted;
		  __c = __sb->snextc();
		}
	      __str.append(__buf, __len);

	      if (_Traits::eq_int_type(__c, __eof))
		__err |= ios_base::eofbit;
	      else if (_Traits::eq_int_type(__c, __idelim))
		{
		  ++__extracted;		  
		  __sb->sbumpc();
		}
	      else
		__err |= ios_base::failbit;
	    }
	  catch(...)
	    {
	      // _GLIBCXX_RESOLVE_LIB_DEFECTS
	      // 91. Description of operator>> and getline() for string<>
	      // might cause endless loop
	      __in._M_setstate(ios_base::badbit);
	    }
	}
      if (!__extracted)
	__err |= ios_base::failbit;
      if (__err)
	__in.setstate(__err);
      return __in;
    }      
  
} // namespace std

#endif // _VSTRING_TCC
