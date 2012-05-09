// Debugging mode support code -*- C++ -*-

// Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
// 2011, 2012 Free Software Foundation, Inc.
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

#include <debug/debug.h>
#include <debug/safe_sequence.h>
#include <debug/safe_unordered_container.h>
#include <debug/safe_iterator.h>
#include <debug/safe_local_iterator.h>
#include <algorithm>
#include <cassert>
#include <cstring>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <functional>

using namespace std;

namespace
{
  /** Returns different instances of __mutex depending on the passed address
   *  in order to limit contention without breaking current library binary
   *  compatibility. */
  __gnu_cxx::__mutex&
  get_safe_base_mutex(void* __address)
  {
    const size_t mask = 0xf;
    static __gnu_cxx::__mutex safe_base_mutex[mask + 1];
    const size_t index = _Hash_impl::hash(__address) & mask;
    return safe_base_mutex[index];
  }

  void
  swap_its(__gnu_debug::_Safe_sequence_base& __lhs,
	   __gnu_debug::_Safe_iterator_base*& __lhs_its,
	   __gnu_debug::_Safe_sequence_base& __rhs,
	   __gnu_debug::_Safe_iterator_base*& __rhs_its)
  {
    swap(__lhs_its, __rhs_its);
    __gnu_debug::_Safe_iterator_base* __iter;
    for (__iter = __rhs_its; __iter; __iter = __iter->_M_next)
      __iter->_M_sequence = &__rhs;
    for (__iter = __lhs_its; __iter; __iter = __iter->_M_next)
      __iter->_M_sequence = &__lhs;
  }

  void
  swap_seq(__gnu_debug::_Safe_sequence_base& __lhs,
	   __gnu_debug::_Safe_sequence_base& __rhs)
  {
    swap(__lhs._M_version, __rhs._M_version);
    swap_its(__lhs, __lhs._M_iterators,
	     __rhs, __rhs._M_iterators);
    swap_its(__lhs, __lhs._M_const_iterators,
	     __rhs, __rhs._M_const_iterators);
  }

  void
  swap_ucont(__gnu_debug::_Safe_unordered_container_base& __lhs,
	    __gnu_debug::_Safe_unordered_container_base& __rhs)
  {
    swap_seq(__lhs, __rhs);
    swap_its(__lhs, __lhs._M_local_iterators,
	     __rhs, __rhs._M_local_iterators);
    swap_its(__lhs, __lhs._M_const_local_iterators,
	     __rhs, __rhs._M_const_local_iterators);
  }

  void
  detach_all(__gnu_debug::_Safe_iterator_base* __iter)
  {
    for (; __iter;)
      {
	__gnu_debug::_Safe_iterator_base* __old = __iter;
	__iter = __iter->_M_next;
	__old->_M_reset();
      }
  }
} // anonymous namespace

namespace __gnu_debug
{
  const char* _S_debug_messages[] = 
  {
    // General Checks
    "function requires a valid iterator range [%1.name;, %2.name;)",
    "attempt to insert into container with a singular iterator",
    "attempt to insert into container with an iterator"
    " from a different container",
    "attempt to erase from container with a %2.state; iterator",
    "attempt to erase from container with an iterator"
    " from a different container",
    "attempt to subscript container with out-of-bounds index %2;,"
    " but container only holds %3; elements",
    "attempt to access an element in an empty container",
    "elements in iterator range [%1.name;, %2.name;)"
    " are not partitioned by the value %3;",
    "elements in iterator range [%1.name;, %2.name;)"
    " are not partitioned by the predicate %3; and value %4;",
    "elements in iterator range [%1.name;, %2.name;) are not sorted",
    "elements in iterator range [%1.name;, %2.name;)"
    " are not sorted according to the predicate %3;",
    "elements in iterator range [%1.name;, %2.name;) do not form a heap",
    "elements in iterator range [%1.name;, %2.name;)"
    " do not form a heap with respect to the predicate %3;",
    // std::bitset checks
    "attempt to write through a singular bitset reference",
    "attempt to read from a singular bitset reference",
    "attempt to flip a singular bitset reference",
    // std::list checks
    "attempt to splice a list into itself",
    "attempt to splice lists with unequal allocators",
    "attempt to splice elements referenced by a %1.state; iterator",
    "attempt to splice an iterator from a different container",
    "splice destination %1.name;"
    " occurs within source range [%2.name;, %3.name;)",
    // iterator checks
    "attempt to initialize an iterator that will immediately become singular",
    "attempt to copy-construct an iterator from a singular iterator",
    "attempt to construct a constant iterator"
    " from a singular mutable iterator",
    "attempt to copy from a singular iterator",
    "attempt to dereference a %1.state; iterator",
    "attempt to increment a %1.state; iterator",
    "attempt to decrement a %1.state; iterator",
    "attempt to subscript a %1.state; iterator %2; step from"
    " its current position, which falls outside its dereferenceable range",
    "attempt to advance a %1.state; iterator %2; steps,"
    " which falls outside its valid range",
    "attempt to retreat a %1.state; iterator %2; steps,"
    " which falls outside its valid range",
    "attempt to compare a %1.state; iterator to a %2.state; iterator",
    "attempt to compare iterators from different sequences",
    "attempt to order a %1.state; iterator to a %2.state; iterator",
    "attempt to order iterators from different sequences",
    "attempt to compute the difference between a %1.state;"
    " iterator to a %2.state; iterator",
    "attempt to compute the different between two iterators"
    " from different sequences",
    // istream_iterator
    "attempt to dereference an end-of-stream istream_iterator",
    "attempt to increment an end-of-stream istream_iterator",
    // ostream_iterator
    "attempt to output via an ostream_iterator with no associated stream",
    // istreambuf_iterator
    "attempt to dereference an end-of-stream istreambuf_iterator"
    " (this is a GNU extension)",
    "attempt to increment an end-of-stream istreambuf_iterator",
    // std::forward_list
    "attempt to insert into container after an end iterator",
    "attempt to erase from container after a %2.state; iterator not followed"
    " by a dereferenceable one",
    "function requires a valid iterator range (%2.name;, %3.name;)"
    ", \"%2.name;\" shall be before and not equal to \"%3.name;\"",
    // std::unordered_container::local_iterator
    "attempt to compare local iterators from different unordered container"
    " buckets",
    "function requires a non-empty iterator range [%1.name;, %2.name;)",
    "attempt to self move assign"
  };

  void
  _Safe_sequence_base::
  _M_detach_all()
  {
    __gnu_cxx::__scoped_lock sentry(_M_get_mutex());
    detach_all(_M_iterators);
    _M_iterators = 0;
    
    detach_all(_M_const_iterators);
    _M_const_iterators = 0;
  }

  void
  _Safe_sequence_base::
  _M_detach_singular()
  {
    __gnu_cxx::__scoped_lock sentry(_M_get_mutex());
    for (_Safe_iterator_base* __iter = _M_iterators; __iter;)
      {
	_Safe_iterator_base* __old = __iter;
	__iter = __iter->_M_next;
	if (__old->_M_singular())
	  __old->_M_detach_single();
      }

    for (_Safe_iterator_base* __iter2 = _M_const_iterators; __iter2;)
      {
	_Safe_iterator_base* __old = __iter2;
	__iter2 = __iter2->_M_next;
	if (__old->_M_singular())
	  __old->_M_detach_single();
      }
  }

  void
  _Safe_sequence_base::
  _M_revalidate_singular()
  {
    __gnu_cxx::__scoped_lock sentry(_M_get_mutex());
    for (_Safe_iterator_base* __iter = _M_iterators; __iter;
	 __iter = __iter->_M_next)
      __iter->_M_version = _M_version;

    for (_Safe_iterator_base* __iter2 = _M_const_iterators; __iter2;
	 __iter2 = __iter2->_M_next)
      __iter2->_M_version = _M_version;
  }

  void
  _Safe_sequence_base::
  _M_swap(_Safe_sequence_base& __x)
  {
    // We need to lock both sequences to swap
    using namespace __gnu_cxx;
    __mutex *__this_mutex = &_M_get_mutex();
    __mutex *__x_mutex = &__x._M_get_mutex();
    if (__this_mutex == __x_mutex)
      {
	__scoped_lock __lock(*__this_mutex);
	swap_seq(*this, __x);
      }
    else
      {
	__scoped_lock __l1(__this_mutex < __x_mutex
			     ? *__this_mutex : *__x_mutex);
	__scoped_lock __l2(__this_mutex < __x_mutex
			     ? *__x_mutex : *__this_mutex);
	swap_seq(*this, __x);
      }
  }

  __gnu_cxx::__mutex&
  _Safe_sequence_base::
  _M_get_mutex() throw ()
  { return get_safe_base_mutex(this); }

  void
  _Safe_sequence_base::
  _M_attach(_Safe_iterator_base* __it, bool __constant)
  {
    __gnu_cxx::__scoped_lock sentry(_M_get_mutex());
    _M_attach_single(__it, __constant);
  }

  void
  _Safe_sequence_base::
  _M_attach_single(_Safe_iterator_base* __it, bool __constant) throw ()
  {
    _Safe_iterator_base*& __its =
      __constant ? _M_const_iterators : _M_iterators;
    __it->_M_next = __its;
    if (__it->_M_next)
      __it->_M_next->_M_prior = __it;
    __its = __it;
  }

  void
  _Safe_sequence_base::
  _M_detach(_Safe_iterator_base* __it)
  {
    // Remove __it from this sequence's list
    __gnu_cxx::__scoped_lock sentry(_M_get_mutex());
    _M_detach_single(__it);
  }

  void
  _Safe_sequence_base::
  _M_detach_single(_Safe_iterator_base* __it) throw ()
  {
    // Remove __it from this sequence's list
    __it->_M_unlink();
    if (_M_const_iterators == __it)
      _M_const_iterators = __it->_M_next;
    if (_M_iterators == __it)
      _M_iterators = __it->_M_next;
  }

  void
  _Safe_iterator_base::
  _M_attach(_Safe_sequence_base* __seq, bool __constant)
  {
    _M_detach();
    
    // Attach to the new sequence (if there is one)
    if (__seq)
      {
	_M_sequence = __seq;
	_M_version = _M_sequence->_M_version;
	_M_sequence->_M_attach(this, __constant);
      }
  }
  
  void
  _Safe_iterator_base::
  _M_attach_single(_Safe_sequence_base* __seq, bool __constant) throw ()
  {
    _M_detach_single();
    
    // Attach to the new sequence (if there is one)
    if (__seq)
      {
	_M_sequence = __seq;
	_M_version = _M_sequence->_M_version;
	_M_sequence->_M_attach_single(this, __constant);
      }
  }

  void
  _Safe_iterator_base::
  _M_detach()
  {
    if (_M_sequence)
      _M_sequence->_M_detach(this);

    _M_reset();
  }

  void
  _Safe_iterator_base::
  _M_detach_single() throw ()
  {
    if (_M_sequence)
      _M_sequence->_M_detach_single(this);

    _M_reset();
  }

  void
  _Safe_iterator_base::
  _M_reset() throw ()
  {
    _M_sequence = 0;
    _M_version = 0;
    _M_prior = 0;
    _M_next = 0;
  }

  bool
  _Safe_iterator_base::
  _M_singular() const throw ()
  { return !_M_sequence || _M_version != _M_sequence->_M_version; }
    
  bool
  _Safe_iterator_base::
  _M_can_compare(const _Safe_iterator_base& __x) const throw ()
  {
    return (!_M_singular() 
	    && !__x._M_singular() && _M_sequence == __x._M_sequence);
  }

  __gnu_cxx::__mutex&
  _Safe_iterator_base::
  _M_get_mutex() throw ()
  { return get_safe_base_mutex(_M_sequence); }

  _Safe_unordered_container_base*
  _Safe_local_iterator_base::
  _M_get_container() const _GLIBCXX_NOEXCEPT
  { return static_cast<_Safe_unordered_container_base*>(_M_sequence); }

  void
  _Safe_local_iterator_base::
  _M_attach(_Safe_sequence_base* __cont, bool __constant)
  {
    _M_detach();
    
    // Attach to the new container (if there is one)
    if (__cont)
      {
	_M_sequence = __cont;
	_M_version = _M_sequence->_M_version;
	_M_get_container()->_M_attach_local(this, __constant);
      }
  }
  
  void
  _Safe_local_iterator_base::
  _M_attach_single(_Safe_sequence_base* __cont, bool __constant) throw ()
  {
    _M_detach_single();
    
    // Attach to the new container (if there is one)
    if (__cont)
      {
	_M_sequence = __cont;
	_M_version = _M_sequence->_M_version;
	_M_get_container()->_M_attach_local_single(this, __constant);
      }
  }

  void
  _Safe_local_iterator_base::
  _M_detach()
  {
    if (_M_sequence)
      _M_get_container()->_M_detach_local(this);

    _M_reset();
  }

  void
  _Safe_local_iterator_base::
  _M_detach_single() throw ()
  {
    if (_M_sequence)
      _M_get_container()->_M_detach_local_single(this);

    _M_reset();
  }

  void
  _Safe_unordered_container_base::
  _M_detach_all()
  {
    __gnu_cxx::__scoped_lock sentry(_M_get_mutex());
    detach_all(_M_iterators);
    _M_iterators = 0;
    
    detach_all(_M_const_iterators);
    _M_const_iterators = 0;

    detach_all(_M_local_iterators);
    _M_local_iterators = 0;

    detach_all(_M_const_local_iterators);
    _M_const_local_iterators = 0;
  }

  void
  _Safe_unordered_container_base::
  _M_swap(_Safe_unordered_container_base& __x)
  {
    // We need to lock both containers to swap
    using namespace __gnu_cxx;
    __mutex *__this_mutex = &_M_get_mutex();
    __mutex *__x_mutex = &__x._M_get_mutex();
    if (__this_mutex == __x_mutex)
      {
	__scoped_lock __lock(*__this_mutex);
	swap_ucont(*this, __x);
      }
    else
      {
	__scoped_lock __l1(__this_mutex < __x_mutex
			     ? *__this_mutex : *__x_mutex);
	__scoped_lock __l2(__this_mutex < __x_mutex
			     ? *__x_mutex : *__this_mutex);
	swap_ucont(*this, __x);
      }
  }

  void
  _Safe_unordered_container_base::
  _M_attach_local(_Safe_iterator_base* __it, bool __constant)
  {
    __gnu_cxx::__scoped_lock sentry(_M_get_mutex());
    _M_attach_local_single(__it, __constant);
  }

  void
  _Safe_unordered_container_base::
  _M_attach_local_single(_Safe_iterator_base* __it, bool __constant) throw ()
  {
    _Safe_iterator_base*& __its =
      __constant ? _M_const_local_iterators : _M_local_iterators;
    __it->_M_next = __its;
    if (__it->_M_next)
      __it->_M_next->_M_prior = __it;
    __its = __it;
  }

  void
  _Safe_unordered_container_base::
  _M_detach_local(_Safe_iterator_base* __it)
  {
    // Remove __it from this container's list
    __gnu_cxx::__scoped_lock sentry(_M_get_mutex());
    _M_detach_local_single(__it);
  }

  void
  _Safe_unordered_container_base::
  _M_detach_local_single(_Safe_iterator_base* __it) throw ()
  {
    // Remove __it from this container's list
    __it->_M_unlink();
    if (_M_const_local_iterators == __it)
      _M_const_local_iterators = __it->_M_next;
    if (_M_local_iterators == __it)
      _M_local_iterators = __it->_M_next;
  }

  void
  _Error_formatter::_Parameter::
  _M_print_field(const _Error_formatter* __formatter, const char* __name) const
  {
    assert(this->_M_kind != _Parameter::__unused_param);
    const int __bufsize = 64;
    char __buf[__bufsize];
    
    if (_M_kind == __iterator)
      {
	if (strcmp(__name, "name") == 0)
	  {
	    assert(_M_variant._M_iterator._M_name);
	    __formatter->_M_print_word(_M_variant._M_iterator._M_name);
	  }
	else if (strcmp(__name, "address") == 0)
	  {
	    __formatter->_M_format_word(__buf, __bufsize, "%p", 
					_M_variant._M_iterator._M_address);
	    __formatter->_M_print_word(__buf);
	  }
	else if (strcmp(__name, "type") == 0)
	  {
	    if (!_M_variant._M_iterator._M_type)
	      __formatter->_M_print_word("<unknown type>");
	    else
	      // TBD: demangle!
	      __formatter->_M_print_word(_M_variant._M_iterator.
					 _M_type->name());
	  }
	else if (strcmp(__name, "constness") == 0)
	  {
	    static const char* __constness_names[__last_constness] =
	      {
		"<unknown>",
		"constant",
		"mutable"
	      };
	    __formatter->_M_print_word(__constness_names[_M_variant.
							 _M_iterator.
							 _M_constness]);
	  }
	else if (strcmp(__name, "state") == 0)
	  {
	    static const char* __state_names[__last_state] = 
	      {
		"<unknown>",
		"singular",
		"dereferenceable (start-of-sequence)",
		"dereferenceable",
		"past-the-end",
		"before-begin"
	      };
	    __formatter->_M_print_word(__state_names[_M_variant.
						     _M_iterator._M_state]);
	  }
	else if (strcmp(__name, "sequence") == 0)
	  {
	    assert(_M_variant._M_iterator._M_sequence);
	    __formatter->_M_format_word(__buf, __bufsize, "%p", 
					_M_variant._M_iterator._M_sequence);
	    __formatter->_M_print_word(__buf);
	  }
	else if (strcmp(__name, "seq_type") == 0)
	  {
	    if (!_M_variant._M_iterator._M_seq_type)
	      __formatter->_M_print_word("<unknown seq_type>");
	    else
	      // TBD: demangle!
	      __formatter->_M_print_word(_M_variant._M_iterator.
					 _M_seq_type->name());
	  }
	else
	  assert(false);
      }
    else if (_M_kind == __sequence)
      {
	if (strcmp(__name, "name") == 0)
	  {
	    assert(_M_variant._M_sequence._M_name);
	    __formatter->_M_print_word(_M_variant._M_sequence._M_name);
	  }
	else if (strcmp(__name, "address") == 0)
	  {
	    assert(_M_variant._M_sequence._M_address);
	    __formatter->_M_format_word(__buf, __bufsize, "%p", 
					_M_variant._M_sequence._M_address);
	    __formatter->_M_print_word(__buf);
	  }
	else if (strcmp(__name, "type") == 0)
	  {
	    if (!_M_variant._M_sequence._M_type)
	      __formatter->_M_print_word("<unknown type>");
	    else
	      // TBD: demangle!
	      __formatter->_M_print_word(_M_variant._M_sequence.
					 _M_type->name());
	  }
	else
	  assert(false);
      }
    else if (_M_kind == __integer)
      {
	if (strcmp(__name, "name") == 0)
	  {
	    assert(_M_variant._M_integer._M_name);
	    __formatter->_M_print_word(_M_variant._M_integer._M_name);
	  }
	else
	assert(false);
      }
    else if (_M_kind == __string)
      {
	if (strcmp(__name, "name") == 0)
	  {
	    assert(_M_variant._M_string._M_name);
	    __formatter->_M_print_word(_M_variant._M_string._M_name);
	  }
	else
	  assert(false);
      }
    else
      {
	assert(false);
      }
  }
  
  void
  _Error_formatter::_Parameter::
  _M_print_description(const _Error_formatter* __formatter) const
  {
    const int __bufsize = 128;
    char __buf[__bufsize];
    
    if (_M_kind == __iterator)
      {
	__formatter->_M_print_word("iterator ");
	if (_M_variant._M_iterator._M_name)
	  {
	    __formatter->_M_format_word(__buf, __bufsize, "\"%s\" ", 
					_M_variant._M_iterator._M_name);
	    __formatter->_M_print_word(__buf);
	  }
	
	__formatter->_M_format_word(__buf, __bufsize, "@ 0x%p {\n", 
				    _M_variant._M_iterator._M_address);
	__formatter->_M_print_word(__buf);
	if (_M_variant._M_iterator._M_type)
	  {
	    __formatter->_M_print_word("type = ");
	    _M_print_field(__formatter, "type");
	    
	    if (_M_variant._M_iterator._M_constness != __unknown_constness)
	      {
		__formatter->_M_print_word(" (");
		_M_print_field(__formatter, "constness");
		__formatter->_M_print_word(" iterator)");
	      }
	    __formatter->_M_print_word(";\n");
	  }
	
	if (_M_variant._M_iterator._M_state != __unknown_state)
	  {
	    __formatter->_M_print_word("  state = ");
	    _M_print_field(__formatter, "state");
	    __formatter->_M_print_word(";\n");
	  }
	
	if (_M_variant._M_iterator._M_sequence)
	  {
	    __formatter->_M_print_word("  references sequence ");
	    if (_M_variant._M_iterator._M_seq_type)
	      {
		__formatter->_M_print_word("with type `");
		_M_print_field(__formatter, "seq_type");
		__formatter->_M_print_word("' ");
	      }
	    
	    __formatter->_M_format_word(__buf, __bufsize, "@ 0x%p\n", 
					_M_variant._M_sequence._M_address);
	    __formatter->_M_print_word(__buf);
	  }
	__formatter->_M_print_word("}\n");
      }
    else if (_M_kind == __sequence)
      {
	__formatter->_M_print_word("sequence ");
	if (_M_variant._M_sequence._M_name)
	  {
	    __formatter->_M_format_word(__buf, __bufsize, "\"%s\" ", 
					_M_variant._M_sequence._M_name);
	    __formatter->_M_print_word(__buf);
	  }
	
	__formatter->_M_format_word(__buf, __bufsize, "@ 0x%p {\n", 
				    _M_variant._M_sequence._M_address);
	__formatter->_M_print_word(__buf);
	
	if (_M_variant._M_sequence._M_type)
	  {
	    __formatter->_M_print_word("  type = ");
	    _M_print_field(__formatter, "type");
	    __formatter->_M_print_word(";\n");
	  }	  
	__formatter->_M_print_word("}\n");
      }
  }

  const _Error_formatter&
  _Error_formatter::_M_message(_Debug_msg_id __id) const throw ()
  { return this->_M_message(_S_debug_messages[__id]); }
  
  void
  _Error_formatter::_M_error() const
  {
    const int __bufsize = 128;
    char __buf[__bufsize];
    
    // Emit file & line number information
    _M_column = 1;
    _M_wordwrap = false;
    if (_M_file)
      {
	_M_format_word(__buf, __bufsize, "%s:", _M_file);
	_M_print_word(__buf);
	_M_column += strlen(__buf);
      }
    
    if (_M_line > 0)
      {
	_M_format_word(__buf, __bufsize, "%u:", _M_line);
	_M_print_word(__buf);
	_M_column += strlen(__buf);
      }
    
    if (_M_max_length)
      _M_wordwrap = true;
    _M_print_word("error: ");
    
    // Print the error message
    assert(_M_text);
    _M_print_string(_M_text);
    _M_print_word(".\n");
    
    // Emit descriptions of the objects involved in the operation
    _M_wordwrap = false;
    bool __has_noninteger_parameters = false;
    for (unsigned int __i = 0; __i < _M_num_parameters; ++__i)
      {
	if (_M_parameters[__i]._M_kind == _Parameter::__iterator
	    || _M_parameters[__i]._M_kind == _Parameter::__sequence)
	  {
	    if (!__has_noninteger_parameters)
	      {
		_M_first_line = true;
		_M_print_word("\nObjects involved in the operation:\n");
		__has_noninteger_parameters = true;
	      }
	    _M_parameters[__i]._M_print_description(this);
	  }
      }
    
    abort();
  }

  template<typename _Tp>
    void
    _Error_formatter::_M_format_word(char* __buf, 
				     int __n __attribute__ ((__unused__)), 
				     const char* __fmt, _Tp __s) const throw ()
    {
#ifdef _GLIBCXX_USE_C99
      std::snprintf(__buf, __n, __fmt, __s);
#else
      std::sprintf(__buf, __fmt, __s);
#endif
    }

  
  void 
  _Error_formatter::_M_print_word(const char* __word) const
  {
    if (!_M_wordwrap) 
      {
	fprintf(stderr, "%s", __word);
	return;
      }
    
    size_t __length = strlen(__word);
    if (__length == 0)
      return;
    
    if ((_M_column + __length < _M_max_length)
	|| (__length >= _M_max_length && _M_column == 1)) 
      {
	// If this isn't the first line, indent
	if (_M_column == 1 && !_M_first_line)
	  {
	    char __spacing[_M_indent + 1];
	    for (int i = 0; i < _M_indent; ++i)
	      __spacing[i] = ' ';
	    __spacing[_M_indent] = '\0';
	    fprintf(stderr, "%s", __spacing);
	    _M_column += _M_indent;
	  }
	
	fprintf(stderr, "%s", __word);
	_M_column += __length;
	
	if (__word[__length - 1] == '\n') 
	  {
	    _M_first_line = false;
	    _M_column = 1;
	  }
      }
    else
      {
	_M_column = 1;
	_M_print_word("\n");
	_M_print_word(__word);
      }
  }
  
  void
  _Error_formatter::
  _M_print_string(const char* __string) const
  {
    const char* __start = __string;
    const char* __finish = __start;
    const int __bufsize = 128;
    char __buf[__bufsize];

    while (*__start)
      {
	if (*__start != '%')
	  {
	    // [__start, __finish) denotes the next word
	    __finish = __start;
	    while (isalnum(*__finish))
	      ++__finish;
	    if (__start == __finish)
	      ++__finish;
	    if (isspace(*__finish))
	      ++__finish;
	    
	    const ptrdiff_t __len = __finish - __start;
	    assert(__len < __bufsize);
	    memcpy(__buf, __start, __len);
	    __buf[__len] = '\0';
	    _M_print_word(__buf);
	    __start = __finish;
	    
	    // Skip extra whitespace
	    while (*__start == ' ') 
	      ++__start;
	    
	    continue;
	  } 
	
	++__start;
	assert(*__start);
	if (*__start == '%')
	  {
	    _M_print_word("%");
	    ++__start;
	    continue;
	  }
	
	// Get the parameter number
	assert(*__start >= '1' && *__start <= '9');
	size_t __param = *__start - '0';
	--__param;
	assert(__param < _M_num_parameters);
      
	// '.' separates the parameter number from the field
	// name, if there is one.
	++__start;
	if (*__start != '.')
	  {
	    assert(*__start == ';');
	    ++__start;
	    __buf[0] = '\0';
	    if (_M_parameters[__param]._M_kind == _Parameter::__integer)
	      {
		_M_format_word(__buf, __bufsize, "%ld", 
			       _M_parameters[__param]._M_variant._M_integer._M_value);
		_M_print_word(__buf);
	      }
	    else if (_M_parameters[__param]._M_kind == _Parameter::__string)
	      _M_print_string(_M_parameters[__param]._M_variant._M_string._M_value);
	    continue;
	  }
	
	// Extract the field name we want
	enum { __max_field_len = 16 };
	char __field[__max_field_len];
	int __field_idx = 0;
	++__start;
	while (*__start != ';')
	  {
	    assert(*__start);
	    assert(__field_idx < __max_field_len-1);
	    __field[__field_idx++] = *__start++;
	  }
	++__start;
	__field[__field_idx] = 0;
	
	_M_parameters[__param]._M_print_field(this, __field);		  
      }
  }

  void
  _Error_formatter::_M_get_max_length() const throw ()
  {
    const char* __nptr = std::getenv("GLIBCXX_DEBUG_MESSAGE_LENGTH");
    if (__nptr)
      {
	char* __endptr;
	const unsigned long __ret = std::strtoul(__nptr, &__endptr, 0);
	if (*__nptr != '\0' && *__endptr == '\0')
	  _M_max_length = __ret;
      }
  }

  // Instantiations.
  template
    void
    _Error_formatter::_M_format_word(char*, int, const char*, 
				     const void*) const;

  template
    void
    _Error_formatter::_M_format_word(char*, int, const char*, long) const;

  template
    void
    _Error_formatter::_M_format_word(char*, int, const char*, 
				     std::size_t) const;

  template
    void
    _Error_formatter::_M_format_word(char*, int, const char*, 
				     const char*) const;
} // namespace __gnu_debug
