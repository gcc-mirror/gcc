// Components for manipulating sequences of characters -*- C++ -*-

// Copyright (C) 2000, 1999, 1998, 1997 Free Software Foundation, Inc.
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

//
// ISO C++ 14882: 21  Strings library
//

// This file is included by <string>.  It is not meant to be included
// separately.

// Written by Jason Merrill based upon the specification by Takanori Adachi
// in ANSI X3J16/94-0013R2.  Rewritten by Nathan Myers to ISO-14882.

#ifndef _CPP_BITS_STRING_TCC
#define _CPP_BITS_STRING_TCC 1

namespace std
{

  template<typename _CharT, typename _Traits, typename _Alloc>
    _CharT 
    basic_string<_CharT, _Traits, _Alloc>::
    _Rep::_S_terminal = _CharT();

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::size_type 
    basic_string<_CharT, _Traits, _Alloc>::
    _Rep::_S_max_size = (((npos - sizeof(_Rep))/sizeof(_CharT)) - 1) / 4;

  // NB: This is the special case for Input Iterators, used in
  // istreambuf_iterators, etc.
  // Input Iterators have a cost structure very different from
  // pointers, calling for a different coding style.
  template<typename _CharT, typename _Traits, typename _Alloc>
    template<typename _InIter>
      _CharT*
      basic_string<_CharT, _Traits, _Alloc>::
      _S_construct(_InIter __beg, _InIter __end, const _Alloc& __a,
		   input_iterator_tag)
      {
	if (__beg == __end && __a == _Alloc())
	  return _S_empty_rep()._M_refcopy();
	// Avoid reallocation for common case.
	_CharT __buf[100];
	size_type __i = 0;
	while (__beg != __end && __i < sizeof(__buf) / sizeof(_CharT))
	  { 
	    __buf[__i++] = *__beg; 
	    ++__beg; 
	  }
	_Rep* __r = _Rep::_S_create(__i, __a);
	traits_type::copy(__r->_M_refdata(), __buf, __i);
	__r->_M_length = __i;
	try {
	  // NB: this loop looks precisely this way because
	  // it avoids comparing __beg != __end any more
	  // than strictly necessary; != might be expensive!
	  for (;;)
	    {
	      _CharT* __p = __r->_M_refdata() + __r->_M_length;
	      _CharT* __last = __r->_M_refdata() + __r->_M_capacity;
	      for (;;)
		{
		  if (__beg == __end)
		    {
		      __r->_M_length = __p - __r->_M_refdata();
		      *__p = _Rep::_S_terminal;       // grrr.
		      return __r->_M_refdata();
		    }
		  if (__p == __last)
		    break;
		  *__p++ = *__beg; 
		  ++__beg;
		}
	      // Allocate more space.
	      size_type __len = __p - __r->_M_refdata();
	      _Rep* __another = _Rep::_S_create(__len + 1, __a);
	      traits_type::copy(__another->_M_refdata(), 
				__r->_M_refdata(), __len);
	      __r->_M_destroy(__a);
	      __r = __another;
	      __r->_M_length = __len;
	    }
	}
	catch (...) {
	    __r->_M_destroy(__a); 
	    throw;
	}
	return 0;
      }

  template<typename _CharT, typename _Traits, typename _Alloc>
    template <class _InIter>
      _CharT*
      basic_string<_CharT,_Traits,_Alloc>::
      _S_construct(_InIter __beg, _InIter __end, const _Alloc& __a, 
		   forward_iterator_tag)
      {
	size_type __dnew = static_cast<size_type>(distance(__beg, __end));

	if (__beg == __end && __a == _Alloc())
	  return _S_empty_rep()._M_refcopy();

	// Check for out_of_range and length_error exceptions.
	_Rep* __r = _Rep::_S_create(__dnew, __a);
	try { 
	  _S_copy_chars(__r->_M_refdata(), __beg, __end); 
	}
	catch (...) { 
	  __r->_M_destroy(__a); 
	  throw; 
	}
	__r->_M_length = __dnew;

	__r->_M_refdata()[__dnew] = _Rep::_S_terminal;  // grrr.
	return __r->_M_refdata();
      }

  template<typename _CharT, typename _Traits, typename _Alloc>
    _CharT*
    basic_string<_CharT,_Traits, _Alloc>::
    _S_construct(size_type __n, _CharT __c, const _Alloc& __a)
    {
      if (__n == 0 && __a == _Alloc())
	return _S_empty_rep()._M_refcopy();

      // Check for out_of_range and length_error exceptions.
      _Rep* __r = _Rep::_S_create(__n, __a);
      try { 
	if (__n) 
	  traits_type::assign(__r->_M_refdata(), __n, __c); 
      }
      catch (...) { 
	__r->_M_destroy(__a); 
	throw; 
      }
      __r->_M_length = __n;
      __r->_M_refdata()[__n] = _Rep::_S_terminal;  // grrr
      return __r->_M_refdata();
    }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::
    basic_string(const basic_string& __str)
    : _M_dataplus(__str._M_rep()->_M_grab(_Alloc(), __str.get_allocator()),
		 __str.get_allocator())
    { }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::
    basic_string(const _Alloc& __a)
    : _M_dataplus(_S_construct(size_type(), _CharT(), __a), __a)
    { }
 
  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::
    basic_string(const basic_string& __str, size_type __pos, size_type __n)
    : _M_dataplus(_S_construct(__str._M_check(__pos), 
			       __str._M_fold(__pos, __n), _Alloc()), _Alloc())
    { }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::
    basic_string(const basic_string& __str, size_type __pos,
		 size_type __n, const _Alloc& __a)
    : _M_dataplus(_S_construct(__str._M_check(__pos), 
			       __str._M_fold(__pos, __n), __a), __a)
    { }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::
    basic_string(const _CharT* __s, size_type __n, const _Alloc& __a)
    : _M_dataplus(_S_construct(__s, __s + __n, __a), __a)
    { }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::
    basic_string(const _CharT* __s, const _Alloc& __a)
    : _M_dataplus(_S_construct(__s, __s + traits_type::length(__s), __a), __a)
    { }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::
    basic_string(size_type __n, _CharT __c, const _Alloc& __a)
    : _M_dataplus(_S_construct(__n, __c, __a), __a)
    { }
 
  template<typename _CharT, typename _Traits, typename _Alloc>
    template<typename _InputIter>
    basic_string<_CharT, _Traits, _Alloc>::
    basic_string(_InputIter __beg, _InputIter __end, const _Alloc& __a)
    : _M_dataplus(_S_construct(__beg, __end, __a), __a)
    { }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>&
    basic_string<_CharT, _Traits, _Alloc>::assign(const basic_string& __str)
    {
      if (_M_rep() != __str._M_rep())
	{
	  // XXX MT
	  allocator_type __a = this->get_allocator();
	  _CharT* __tmp = __str._M_rep()->_M_grab(__a, __str.get_allocator());
	  _M_rep()->_M_dispose(__a);
	  _M_data(__tmp);
	}
      return *this;
    }
  
  template<typename _CharT, typename _Traits, typename _Alloc>
    void
    basic_string<_CharT, _Traits, _Alloc>::_Rep::
    _M_destroy(const _Alloc& __a) throw ()
    {
      size_type __size = sizeof(_Rep) + (_M_capacity + 1) * sizeof(_CharT);
      _Raw_bytes_alloc(__a).deallocate(reinterpret_cast<char*>(this), __size);
    }

  template<typename _CharT, typename _Traits, typename _Alloc>
    void
    basic_string<_CharT, _Traits, _Alloc>::_M_leak_hard()
    {
      if (_M_rep()->_M_is_shared()) 
	_M_mutate(0, 0, 0);
      _M_rep()->_M_set_leaked();
    }

  template<typename _CharT, typename _Traits, typename _Alloc>
    void
    basic_string<_CharT, _Traits, _Alloc>::
    _M_mutate(size_type __pos, size_type __len1, size_type __len2)
    {
      size_type       __old_size = this->size();
      const size_type __new_size = __old_size + __len2 - __len1;
      const _CharT*        __src = _M_data()  + __pos + __len1;
      const size_type __how_much = __old_size - __pos - __len1;
      
      if (_M_rep()->_M_is_shared() || __new_size > capacity())
	{
	  // Must reallocate.
	  allocator_type __a = get_allocator();
	  _Rep* __r = _Rep::_S_create(__new_size, __a);
	  try {
	    if (__pos)
	      traits_type::copy(__r->_M_refdata(), _M_data(), __pos);
	    if (__how_much)
	      traits_type::copy(__r->_M_refdata() + __pos + __len2, 
				__src, __how_much);
	  }
	  catch (...) { 
	    __r->_M_dispose(get_allocator()); 
	    throw; 
	  }
	  _M_rep()->_M_dispose(__a);
	  _M_data(__r->_M_refdata());
      }
      else if (__how_much && __len1 != __len2)
	{
	  // Work in-place
	  traits_type::move(_M_data() + __pos + __len2, __src, __how_much);
	}
      _M_rep()->_M_set_sharable();
      _M_rep()->_M_length = __new_size;
      _M_data()[__new_size] = _Rep::_S_terminal; // grrr. (per 21.3.4)
    // You cannot leave those LWG people alone for a second.
    }
  
  template<typename _CharT, typename _Traits, typename _Alloc>
    void
    basic_string<_CharT, _Traits, _Alloc>::reserve(size_type __res)
    {
      if (__res > this->capacity() || _M_rep()->_M_is_shared())
        {
	  __LENGTHERROR(__res > this->max_size());
	  allocator_type __a = get_allocator();
	  _CharT* __tmp = _M_rep()->_M_clone(__a, __res - this->size());
	  _M_rep()->_M_dispose(__a);
	  _M_data(__tmp);
        }
    }
  
  template<typename _CharT, typename _Traits, typename _Alloc>
    void basic_string<_CharT, _Traits, _Alloc>::swap(basic_string& __s)
    {
      if (_M_rep()->_M_is_leaked()) 
	_M_rep()->_M_set_sharable();
      if (__s._M_rep()->_M_is_leaked()) 
	__s._M_rep()->_M_set_sharable();
      if (this->get_allocator() == __s.get_allocator())
	{
	  _CharT* __tmp = _M_data();
	  _M_data(__s._M_data());
	  __s._M_data(__tmp);
	}
      // The code below can usually be optimized away.
      else 
	{
	  basic_string __tmp1(_M_ibegin(), _M_iend(), __s.get_allocator());
	  basic_string __tmp2(__s._M_ibegin(), __s._M_iend(), 
			      this->get_allocator());
	  *this = __tmp2;
	  __s = __tmp1;
	}
    }

#ifdef _GLIBCPP_ALLOC_CONTROL
  template<typename _CharT, typename _Traits, typename _Alloc>
    bool (*basic_string<_CharT, _Traits, _Alloc>::_Rep::_S_excess_slop) 
    (size_t, size_t) = 
    basic_string<_CharT, _Traits, _Alloc>::_Rep::_S_default_excess;
#endif

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::_Rep*
    basic_string<_CharT, _Traits, _Alloc>::_Rep::
    _S_create(size_t __capacity, const _Alloc& __alloc)
    {
#ifdef _GLIBCPP_RESOLVE_LIB_DEFECTS
      // 83.  String::npos vs. string::max_size()
      typedef basic_string<_CharT, _Traits, _Alloc> __string_type;
      __LENGTHERROR(__capacity > _S_max_size);
#else
      __LENGTHERROR(__capacity == npos);
#endif

      // NB: Need an array of char_type[__capacity], plus a
      // terminating null char_type() element, plus enough for the
      // _Rep data structure. Whew. Seemingly so needy, yet so elemental.
      size_t __size = (__capacity + 1) * sizeof(_CharT) + sizeof(_Rep);
      // NB: Might throw, but no worries about a leak, mate: _Rep()
      // does not throw.
      void* __place = _Raw_bytes_alloc(__alloc).allocate(__size);
      _Rep *__p = new (__place) _Rep;
      __p->_M_capacity = __capacity;
      __p->_M_set_sharable();  // one reference
      __p->_M_length = 0;
      return __p;
    }

  template<typename _CharT, typename _Traits, typename _Alloc>
    _CharT*
    basic_string<_CharT, _Traits, _Alloc>::_Rep::
    _M_clone(const _Alloc& __alloc, size_type __res)
    {
      _Rep* __r = _Rep::_S_create(_M_length + __res, __alloc);
      if (_M_length)
	{
	  try { 
	    traits_type::copy(__r->_M_refdata(), _M_refdata(), _M_length); 
	  }
	  catch (...)  { 
	    __r->_M_destroy(__alloc); 
	    throw; 
	  }
	}
      __r->_M_length = _M_length;
      return __r->_M_refdata();
    }
  
  template<typename _CharT, typename _Traits, typename _Alloc>
  inline bool
#ifdef _GLIBCPP_ALLOC_CONTROL
    basic_string<_CharT, _Traits, _Alloc>::_Rep::
    _S_default_excess(size_t __s, size_t __r)
#else
    basic_string<_CharT, _Traits, _Alloc>::_Rep::
    _S_excess_slop(size_t __s, size_t __r)
#endif
    {
      return 2 * (__s <= 16 ? 16 : __s) < __r;
    }
  
  // Linker sets _S_empty_rep_storage to all 0s (one reference, empty string)
  // at static init time (before static ctors are run).
  template<typename _CharT, typename _Traits, typename _Alloc>
    typename _Alloc::size_type
    basic_string<_CharT, _Traits, _Alloc>::_S_empty_rep_storage[
    (sizeof(_Rep) + sizeof(_CharT) + sizeof(size_type) - 1)/sizeof(size_type)];

  template<typename _CharT, typename _Traits, typename _Alloc>
    void
    basic_string<_CharT, _Traits, _Alloc>::resize(size_type __n, _CharT __c)
    {
      __LENGTHERROR(__n > max_size());
      size_type __size = this->size();
      if (__size < __n)
	this->append(__n - __size, __c);
      else if (__n < __size)
	this->erase(__n);
      // else nothing (in particular, avoid calling _M_mutate() unnecessarily.)
    }
  
  template<typename _CharT, typename _Traits, typename _Alloc>
    template<typename _InputIter>
      basic_string<_CharT, _Traits, _Alloc>&
      basic_string<_CharT, _Traits, _Alloc>::
      _M_replace(iterator __i1, iterator __i2, _InputIter __j1, 
		 _InputIter __j2, input_iterator_tag)
      {
	basic_string __s(__j1, __j2);
	return this->replace(__i1, __i2, __s._M_ibegin(), __s._M_iend());
      }

  template<typename _CharT, typename _Traits, typename _Alloc>
    template<typename _ForwardIter>
      basic_string<_CharT, _Traits, _Alloc>&
      basic_string<_CharT, _Traits, _Alloc>::
      _M_replace(iterator __i1, iterator __i2, _ForwardIter __j1, 
		 _ForwardIter __j2, forward_iterator_tag)
      {
	size_type __dold = __i2 - __i1;
	size_type __dmax = this->max_size();
	size_type __dnew = static_cast<size_type>(distance(__j1, __j2));

	__LENGTHERROR(__dmax <= __dnew);
	size_type __off = __i1 - _M_ibegin();
	_M_mutate(__off, __dold, __dnew);
	// Invalidated __i1, __i2
	if (__dnew)
	  _S_copy_chars(_M_data() + __off, __j1, __j2);
	
	return *this;
      }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>&
    basic_string<_CharT, _Traits, _Alloc>::
    replace(size_type __pos1, size_type __n1, const basic_string& __str,
	    size_type __pos2, size_type __n2)
    {
      return this->replace(_M_check(__pos1), _M_fold(__pos1, __n1),
			   __str._M_check(__pos2), 
			   __str._M_fold(__pos2, __n2));
    }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT,_Traits,_Alloc>&
    basic_string<_CharT,_Traits,_Alloc>::
    append(const basic_string& __str)
    {
      // Iff appending itself, string needs to pre-reserve the
      // correct size so that _M_mutate does not clobber the
      // iterators formed here.
      size_type __size = __str.size();
      size_type __len = __size + this->size();
      if (__len > this->capacity())
	this->reserve(__len);
      return this->replace(_M_iend(), _M_iend(), __str._M_ibegin(),
			   __str._M_iend());
    }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT,_Traits,_Alloc>&
    basic_string<_CharT,_Traits,_Alloc>::
    append(const basic_string& __str, size_type __pos, size_type __n)
    {
      // Iff appending itself, string needs to pre-reserve the
      // correct size so that _M_mutate does not clobber the
      // iterators formed here.
      size_type __len = min(__str.size() - __pos, __n) + this->size();
      if (__len > this->capacity())
	this->reserve(__len);
      return this->replace(_M_iend(), _M_iend(), __str._M_check(__pos),
			   __str._M_fold(__pos, __n));
    }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT,_Traits,_Alloc>&
    basic_string<_CharT,_Traits,_Alloc>::
    append(const _CharT* __s, size_type __n)
    {
      size_type __len = __n + this->size();
      if (__len > this->capacity())
	this->reserve(__len);
      return this->replace(_M_iend(), _M_iend(), __s, __s + __n);
    }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT,_Traits,_Alloc>&
    basic_string<_CharT,_Traits,_Alloc>::
    append(size_type __n, _CharT __c)
    {
      size_type __len = __n + this->size();
      if (__len > this->capacity())
	this->reserve(__len);
       return this->replace(_M_iend(), _M_iend(), __n, __c);
    }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT,_Traits,_Alloc>
    operator+(const _CharT* __lhs,
             const basic_string<_CharT,_Traits,_Alloc>& __rhs)
    {
      typedef basic_string<_CharT,_Traits,_Alloc> __string_type;
      typedef typename __string_type::size_type	  __size_type;
      __size_type __len = _Traits::length(__lhs);
      __string_type __str;
      __str.reserve(__len + __rhs.size());
      __str.append(__lhs, __lhs + __len);
      __str.append(__rhs);
      return __str;
    }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT,_Traits,_Alloc>
    operator+(_CharT __lhs, const basic_string<_CharT,_Traits,_Alloc>& __rhs)
    {
      typedef basic_string<_CharT,_Traits,_Alloc> __string_type;
      typedef typename __string_type::size_type	  __size_type;
      __string_type __str;
      __size_type __len = __rhs.size();
      __str.reserve(__len + 1);
      __str.append(__string_type::size_type(1), __lhs);
      __str.append(__rhs);
      return __str;
    }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>&
    basic_string<_CharT, _Traits, _Alloc>::
    replace(iterator __i1, iterator __i2, size_type __n2, _CharT __c)
    {
      size_type __n1 = __i2 - __i1;
      size_type __off1 = __i1 - _M_ibegin();
      __LENGTHERROR(max_size() - (this->size() - __n1) <= __n2);
      _M_mutate (__off1, __n1, __n2);
      // Invalidated __i1, __i2
      if (__n2)
	traits_type::assign(_M_data() + __off1, __n2, __c);
      return *this;
    }
  
  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::size_type
    basic_string<_CharT, _Traits, _Alloc>::
    copy(_CharT* __s, size_type __n, size_type __pos) const
    {
      __OUTOFRANGE(__pos > this->size());
      
      if (__n > this->size() - __pos)
	__n = this->size() - __pos;
      
      traits_type::copy(__s, _M_data() + __pos, __n);
      // 21.3.5.7 par 3: do not append null.  (good.)
      return __n;
    }

  // String operations
  // NB: This is specialized for the standard char_traits<char>
  // specialization to use the same optimizations as strchr.
  template<typename _CharT, typename _Traits, typename _Alloc>
    const _CharT*
    basic_string<_CharT, _Traits, _Alloc>::
    _S_find(const _CharT* __beg, const _CharT* __end, _CharT __c)
    {
      return find_if(__beg, __end, _Char_traits_match<_CharT, _Traits>(__c));
    }

  // Specialization for char, definitions in src/string-inst.cc.
  template<>
    const char* 
    string::_S_find(const char* __beg, const char* __end, char __c);

  // Specialization for wchar_t.
#ifdef _GLIBCPP_USE_WCHAR_T
  template<>
    const wchar_t* 
    wstring::_S_find(const wchar_t* __beg, const wchar_t* __end, wchar_t __c);
#endif

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::size_type
    basic_string<_CharT, _Traits, _Alloc>::
    find(const _CharT* __s, size_type __pos, size_type __n) const
    {
      size_t __xpos = __pos;
      const _CharT* __data = _M_data();
      for (; __xpos + __n <= this->size(); ++__xpos)
	if (traits_type::eq(__data[__xpos], *__s)
	    && traits_type::compare(__data + __xpos, __s, __n) == 0)
	  return __xpos;
      return npos;
    }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::size_type
    basic_string<_CharT, _Traits, _Alloc>::
    find(_CharT __c, size_type __pos) const
    {
      size_type __size = this->size();
      size_type __retval = npos;
      if (__pos < __size)
	{
	  const _CharT* __data = _M_data();
	  const _CharT* __end = __data + __size;
	  const _CharT* __p = _S_find(__data + __pos, __end, __c);
	  if (__p != __end)
	    __retval = __p - __data;
	}
      return __retval;
    }


  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::size_type
    basic_string<_CharT, _Traits, _Alloc>::
    rfind(const _CharT* __s, size_type __pos, size_type __n) const
    {
      size_type __size = this->size();
      if (__n <= __size)
	{
	  size_t __xpos = __size - __n;
	  if (__xpos > __pos)
	    __xpos = __pos;
      
	  for (++__xpos; __xpos-- > 0; )
	    if (traits_type::eq(_M_data()[__xpos], *__s)
		&& traits_type::compare(_M_data() + __xpos, __s, __n) == 0)
	      return __xpos;
	}
      return npos;
    }
  
  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::size_type
    basic_string<_CharT, _Traits, _Alloc>::
    rfind(_CharT __c, size_type __pos) const
    {
      size_type __size = this->size();
      if (__size)
	{
	  size_t __xpos = __size - 1;
	  if (__xpos > __pos)
	    __xpos = __pos;
      
	  for (++__xpos; __xpos-- > 0; )
	    if (traits_type::eq(_M_data()[__xpos], __c))
	      return __xpos;
	}
      return npos;
    }
  
  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::size_type
    basic_string<_CharT, _Traits, _Alloc>::
    find_first_of(const _CharT* __s, size_type __pos, size_type __n) const
    {
      const _CharT* __end = __s + __n;
      for (; __n && __pos < this->size(); ++__pos)
	{
	  const _CharT* __p = _S_find(__s, __end, _M_data()[__pos]);
	  if (__p != __end)
	    return __pos;
	}
      return npos;
    }
 
  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::size_type
    basic_string<_CharT, _Traits, _Alloc>::
    find_last_of(const _CharT* __s, size_type __pos, size_type __n) const
    {
      size_type __size = this->size();
      if (__size && __n)
	{ 
	  if (--__size > __pos) 
	    __size = __pos;
	  do
	    {
	      const _CharT* __p = _S_find(__s, __s + __n, _M_data()[__size]);
	      if (__p  != __s + __n)
		return __size;
	    } 
	  while (__size-- != 0);
	}
      return npos;
    }
  
  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::size_type
    basic_string<_CharT, _Traits, _Alloc>::
    find_first_not_of(const _CharT* __s, size_type __pos, size_type __n) const
    {
      size_t __xpos = __pos;
      for (; __n && __xpos < this->size(); ++__xpos)
	if (_S_find(__s, __s + __n, _M_data()[__xpos]) == __s + __n)
	  return __xpos;
      return npos;
    }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::size_type
    basic_string<_CharT, _Traits, _Alloc>::
    find_first_not_of(_CharT __c, size_type __pos) const
    {
      size_t __xpos = __pos;
      for (; __xpos < size(); ++__xpos)
	if (!traits_type::eq(_M_data()[__xpos], __c))
	  return __xpos;
      return npos;
    }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::size_type
    basic_string<_CharT, _Traits, _Alloc>::
    find_last_not_of(const _CharT* __s, size_type __pos, size_type __n) const
    {
      size_type __size = this->size();
      if (__size && __n)
	{ 
	  if (--__size > __pos) 
	    __size = __pos;
	  do
	    {
	      if (_S_find(__s, __s + __n, _M_data()[__size]) == __s + __n)
		return __size;
	    } 
	  while (__size--);
	}
      return npos;
    }

  template<typename _CharT, typename _Traits, typename _Alloc>
    basic_string<_CharT, _Traits, _Alloc>::size_type
    basic_string<_CharT, _Traits, _Alloc>::
    find_last_not_of(_CharT __c, size_type __pos) const
    {
      size_type __size = this->size();
      if (__size)
	{ 
	  if (--__size > __pos) 
	    __size = __pos;
	  do
	    {
	      if (!traits_type::eq(_M_data()[__size], __c))
		return __size;
	    } 
	  while (__size--);
	}
      return npos;
    }
  
  template<typename _CharT, typename _Traits, typename _Alloc>
    int
    basic_string<_CharT, _Traits, _Alloc>::
    compare(size_type __pos, size_type __n, const basic_string& __str) const
    {
      size_type __size = this->size();
      size_type __osize = __str.size();
      __OUTOFRANGE(__pos > __size);
      
      size_type __rsize= min(__size - __pos, __n);
      size_type __len = min(__rsize, __osize);
      int __r = traits_type::compare(_M_data() + __pos, __str.data(), __len);
      if (!__r)
	__r = __rsize - __osize;
      return __r;
    }

  template<typename _CharT, typename _Traits, typename _Alloc>
    int
    basic_string<_CharT, _Traits, _Alloc>::
    compare(size_type __pos1, size_type __n1, const basic_string& __str,
	    size_type __pos2, size_type __n2) const
    {
      size_type __size = this->size();
      size_type __osize = __str.size();
      __OUTOFRANGE(__pos1 > __size);
      __OUTOFRANGE(__pos2 > __osize);
      
      size_type __rsize = min(__size - __pos1, __n1);
      size_type __rosize = min(__osize - __pos2, __n2);
      size_type __len = min(__rsize, __rosize);
      int __r = traits_type::compare(_M_data() + __pos1, 
				     __str.data() + __pos2, __len);
      if (!__r)
	__r = __rsize - __rosize;
      return __r;
    }


  template<typename _CharT, typename _Traits, typename _Alloc>
    int
    basic_string<_CharT, _Traits, _Alloc>::
    compare(const _CharT* __s) const
    {
      size_type __size = this->size();
      int __r = traits_type::compare(_M_data(), __s, __size);
      if (!__r)
	__r = __size - traits_type::length(__s);
      return __r;
    }


  template<typename _CharT, typename _Traits, typename _Alloc>
    int
    basic_string <_CharT,_Traits,_Alloc>::
    compare(size_type __pos, size_type __n1, const _CharT* __s, 
	    size_type __n2) const
    {
      size_type __size = this->size();
      __OUTOFRANGE(__pos > __size);
      
      size_type __osize = min(traits_type::length(__s), __n2);
      size_type __rsize = min(__size - __pos, __n1);
      size_type __len = min(__rsize, __osize);
      int __r = traits_type::compare(_M_data() + __pos, __s, __len);
      if (!__r)
	__r = __rsize - __osize;
      return __r;
    }

  template <class _CharT, class _Traits, class _Alloc>
    void
    _S_string_copy(const basic_string<_CharT, _Traits, _Alloc>& __str,
		   _CharT* __buf, typename _Alloc::size_type __bufsiz)
    {
      typedef typename _Alloc::size_type size_type;
      size_type __strsize = __str.size();
      size_type __bytes = min(__strsize, __bufsiz - 1);
      _Traits::copy(__buf, __str.data(), __bytes);
      __buf[__bytes] = _CharT();
    }

} // std::

#endif /* _CPP_BITS_STRING_TCC */
