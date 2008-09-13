// thread -*- C++ -*-

// Copyright (C) 2008 Free Software Foundation, Inc.
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

#include <thread>
#include <bits/move.h> // std::move

#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1)

namespace std
{
  namespace 
  {
    extern "C"
    {
      void* __thread_proxy(void* __p)
      {
	__thread_data_base* __t = static_cast<__thread_data_base*>(__p);
	__thread_data_ptr __local_thread_data = __t->_M_this_ptr;
	__t->_M_this_ptr.reset();

	try
	  {
	    __local_thread_data->__run();
	  }
	catch(...)
	  {
	    std::terminate();
	  }

	return 0;
      }
    }
  }

  thread::thread()
  { }

  thread::~thread()
  {
    detach();
  }

  thread::id
  thread::get_id() const
  {
    if(_M_thread_data)
      return thread::id(_M_thread_data->_M_thread_handle); 
    else
      return thread::id();
  }

  bool
  thread::joinable() const
  { return get_id() != id(); }
  
  void
  thread::join()
  {
    if(joinable())
      {
	void* __r = 0;
	int __e = __gthread_join(_M_thread_data->_M_thread_handle, &__r);
	if(__e)
	  __throw_system_error(__e);

	lock_guard<mutex> __lock(_M_thread_data_mutex);
	_M_thread_data.reset();
      }
  }

  void
  thread::detach()
  {    
    if(joinable())
      {
	int __e = __gthread_detach(_M_thread_data->_M_thread_handle);
	if(__e)
	  __throw_system_error(__e);

	lock_guard<mutex> __lock(_M_thread_data_mutex);
	_M_thread_data.reset();
      }
  }

  void
  thread::swap(thread&& __t)
  {
    std::swap(_M_thread_data, __t._M_thread_data);
  }

  void 
  thread::__start_thread()
  {
    _M_thread_data->_M_this_ptr = _M_thread_data;
    int __e = __gthread_create(&_M_thread_data->_M_thread_handle, 
			       &__thread_proxy, _M_thread_data.get());
    if(__e)
      __throw_system_error(__e);
  }

  namespace this_thread
  {
    thread::id
    get_id()
    { return thread::id(__gthread_self()); }
    
    void
    yield()
    { __gthread_yield(); }   
  }
}

#endif // _GLIBCXX_HAS_GTHREADS && _GLIBCXX_USE_C99_STDINT_TR1
