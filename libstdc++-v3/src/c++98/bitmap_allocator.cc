// Bitmap Allocator. Out of line function definitions. -*- C++ -*-

// Copyright (C) 2004-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include <ext/bitmap_allocator.h>

namespace __gnu_cxx _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  using std::size_t;

  namespace __detail
  {
    template class __mini_vector<
      std::pair<bitmap_allocator<char>::_Alloc_block*,
		bitmap_allocator<char>::_Alloc_block*> >;

    template class __mini_vector<
      std::pair<bitmap_allocator<wchar_t>::_Alloc_block*,
		bitmap_allocator<wchar_t>::_Alloc_block*> >;

    template class __mini_vector<size_t*>;

    template size_t** __lower_bound(size_t**, size_t**, size_t const&,
				    free_list::_LT_pointer_compare);
  }

  size_t*
  free_list::
  _M_get(size_t __sz) throw(std::bad_alloc)
  {
#if defined __GTHREADS
    __mutex_type& __bfl_mutex = _M_get_mutex();
    __bfl_mutex.lock();
#endif
    const vector_type& __free_list = _M_get_free_list();
    using __gnu_cxx::__detail::__lower_bound;
    iterator __tmp = __lower_bound(__free_list.begin(), __free_list.end(),
				   __sz, _LT_pointer_compare());

    if (__tmp == __free_list.end() || !_M_should_i_give(**__tmp, __sz))
      {
	// We release the lock here, because operator new is
	// guaranteed to be thread-safe by the underlying
	// implementation.
#if defined __GTHREADS
	__bfl_mutex.unlock();
#endif
	// Try twice to get the memory: once directly, and the 2nd
	// time after clearing the free list. If both fail, then throw
	// std::bad_alloc().
	int __ctr = 2;
	while (__ctr)
	  {
	    size_t* __ret = 0;
	    --__ctr;
	    __try
	      {
		__ret = reinterpret_cast<size_t*>
		  (::operator new(__sz + sizeof(size_t)));
	      }
	    __catch(const std::bad_alloc&)
	      {
		this->_M_clear();
	      }
	    if (!__ret)
	      continue;
	    *__ret = __sz;
	    return __ret + 1;
	  }
	std::__throw_bad_alloc();
      }
    else
      {
	size_t* __ret = *__tmp;
	_M_get_free_list().erase(__tmp);
#if defined __GTHREADS
	__bfl_mutex.unlock();
#endif
	return __ret + 1;
      }
  }

  void
  free_list::
  _M_clear()
  {
#if defined __GTHREADS
    __gnu_cxx::__scoped_lock __bfl_lock(_M_get_mutex());
#endif
    vector_type& __free_list = _M_get_free_list();
    iterator __iter = __free_list.begin();
    while (__iter != __free_list.end())
      {
	::operator delete((void*)*__iter);
	++__iter;
      }
    __free_list.clear();
  }

  // Instantiations.
  template class bitmap_allocator<char>;
  template class bitmap_allocator<wchar_t>;

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
