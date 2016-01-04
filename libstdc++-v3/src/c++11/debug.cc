// Debugging mode support code -*- C++ -*-

// Copyright (C) 2003-2016 Free Software Foundation, Inc.
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

#include <bits/move.h>
#include <bits/stl_iterator_base_types.h>

#include <debug/formatter.h>
#include <debug/safe_base.h>
#include <debug/safe_unordered_base.h>
#include <debug/safe_iterator.h>
#include <debug/safe_local_iterator.h>

#include <cassert>
#include <cstdio>

#include <algorithm> // for std::min
#include <functional> // for _Hash_impl

#include <cxxabi.h> // for __cxa_demangle

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
  const char* const _S_debug_messages[] =
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
    "attempt to self move assign",
    "attempt to access container with out-of-bounds bucket index %2;,"
    " container only holds %3; buckets",
    "load factor shall be positive",
    "allocators must be equal",
    "attempt to insert with an iterator range [%1.name;, %2.name;) from this"
    " container",
    "comparison doesn't meet irreflexive requirements, assert(!(a < a))"
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
  _M_swap(_Safe_sequence_base& __x) noexcept
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
  _M_get_container() const noexcept
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
  _M_swap(_Safe_unordered_container_base& __x) noexcept
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
}

namespace
{
  using _Error_formatter = __gnu_debug::_Error_formatter;
  using _Parameter = __gnu_debug::_Error_formatter::_Parameter;

  template<typename _Tp>
    int
    format_word(char* buf, int n, const char* fmt, _Tp s)
    { return std::min(__builtin_snprintf(buf, n, fmt, s), n - 1); }

  void
  get_max_length(std::size_t& max_length)
  {
    const char* nptr = std::getenv("GLIBCXX_DEBUG_MESSAGE_LENGTH");
    if (nptr)
      {
	char* endptr;
	const unsigned long ret = std::strtoul(nptr, &endptr, 0);
	if (*nptr != '\0' && *endptr == '\0')
	  max_length = ret;
      }
  }

  struct PrintContext
  {
    PrintContext()
      : _M_max_length(78), _M_column(1), _M_first_line(true), _M_wordwrap(false)
    { get_max_length(_M_max_length); }

    std::size_t	_M_max_length;
    enum { _M_indent = 4 } ;
    std::size_t	_M_column;
    bool	_M_first_line;
    bool	_M_wordwrap;
  };

  void
  print_word(PrintContext& ctx, const char* word,
	     std::ptrdiff_t count = -1)
  {
    size_t length = count >= 0 ? count : __builtin_strlen(word);
    if (length == 0)
      return;

    // Consider first '\n' at begining cause it impacts column.
    if (word[0] == '\n')
      {
	fprintf(stderr, "\n");
	ctx._M_column = 1;
	++word;
	--length;

	if (length == 0)
	  return;
      }

    size_t visual_length
      = isspace(word[length - 1]) ? length - 1 : length;
    if (visual_length == 0
	|| !ctx._M_wordwrap
	|| (ctx._M_column + visual_length < ctx._M_max_length)
	|| (visual_length >= ctx._M_max_length && ctx._M_column == 1))
      {
	// If this isn't the first line, indent
	if (ctx._M_column == 1 && !ctx._M_first_line)
	  {
	    char spacing[ctx._M_indent + 1];
	    for (int i = 0; i < ctx._M_indent; ++i)
	      spacing[i] = ' ';
	    spacing[ctx._M_indent] = '\0';
	    fprintf(stderr, "%s", spacing);
	    ctx._M_column += ctx._M_indent;
	  }

	int written = fprintf(stderr, "%s", word);

	if (word[length - 1] == '\n')
	  {
	    ctx._M_first_line = false;
	    ctx._M_column = 1;
	  }
	else
	  ctx._M_column += written;
      }
    else
      {
	print_word(ctx, "\n", 1);
	print_word(ctx, word, count);
      }
  }

  void
  print_type(PrintContext& ctx,
	     const type_info* info,
	     const char* unknown_name)
  {
    if (!info)
      print_word(ctx, unknown_name);
    else
      {
	int status;
	char* demangled_name =
	  __cxxabiv1::__cxa_demangle(info->name(), NULL, NULL, &status);
	print_word(ctx, status == 0 ? demangled_name : info->name());
	free(demangled_name);
      }
  }

  bool
  print_field(PrintContext& ctx,
	      const char* name, const _Parameter::_Type& type)
  {
    if (__builtin_strcmp(name, "name") == 0)
      {
	assert(type._M_name);
	print_word(ctx, type._M_name);
      }
    else if (__builtin_strcmp(name, "type") == 0)
      print_type(ctx, type._M_type, "<unknown type>");
    else
      return false;

    return true;
  }

  bool
  print_field(PrintContext& ctx,
	      const char* name, const _Parameter::_Instance& inst)
  {
    const _Parameter::_Type& type = inst;
    if (print_field(ctx, name, type))
      { }
    else if (__builtin_strcmp(name, "address") == 0)
      {
	char buf[64];
	int ret = __builtin_sprintf(buf, "%p", inst._M_address);
	print_word(ctx, buf, ret);
      }
    else
      return false;

    return true;
  }

  void
  print_field(PrintContext& ctx, const _Parameter& param, const char* name)
  {
    assert(param._M_kind != _Parameter::__unused_param);
    const int bufsize = 64;
    char buf[bufsize];

    const auto& variant = param._M_variant;
    switch (param._M_kind)
    {
    case _Parameter::__iterator:
      {
	const auto& iterator = variant._M_iterator;
	if (print_field(ctx, name, iterator))
	  { }
	else if (__builtin_strcmp(name, "constness") == 0)
	  {
	    static const char*
	      constness_names[_Error_formatter::__last_constness] =
	      {
		"<unknown>",
		"constant",
		"mutable"
	      };
	    print_word(ctx, constness_names[iterator._M_constness]);
	  }
	else if (__builtin_strcmp(name, "state") == 0)
	  {
	    static const char*
	      state_names[_Error_formatter::__last_state] =
	      {
		"<unknown>",
		"singular",
		"dereferenceable (start-of-sequence)",
		"dereferenceable",
		"past-the-end",
		"before-begin"
	      };
	    print_word(ctx, state_names[iterator._M_state]);
	  }
	else if (__builtin_strcmp(name, "sequence") == 0)
	  {
	    assert(iterator._M_sequence);
	    int written = __builtin_sprintf(buf, "%p", iterator._M_sequence);
	    print_word(ctx, buf, written);
	  }
	else if (__builtin_strcmp(name, "seq_type") == 0)
	  print_type(ctx, iterator._M_seq_type, "<unknown seq_type>");
	else
	  assert(false);
      }
      break;

    case _Parameter::__sequence:
      if (!print_field(ctx, name, variant._M_sequence))
	assert(false);
      break;

    case _Parameter::__integer:
      if (__builtin_strcmp(name, "name") == 0)
	{
	  assert(variant._M_integer._M_name);
	  print_word(ctx, variant._M_integer._M_name);
	}
      else
	assert(false);
      break;

    case _Parameter::__string:
      if (__builtin_strcmp(name, "name") == 0)
	{
	  assert(variant._M_string._M_name);
	  print_word(ctx, variant._M_string._M_name);
	}
      else
	assert(false);
      break;

    case _Parameter::__instance:
      if (!print_field(ctx, name, variant._M_instance))
	assert(false);
      break;

    case _Parameter::__iterator_value_type:
      if (!print_field(ctx, name, variant._M_iterator_value_type))
	assert(false);
      break;

    default:
      assert(false);
      break;
    }
  }

  void
  print_description(PrintContext& ctx, const _Parameter::_Type& type)
  {
    if (type._M_name)
      {
	const int bufsize = 64;
	char buf[bufsize];
	int written
	  = format_word(buf, bufsize, "\"%s\"", type._M_name);
	print_word(ctx, buf, written);
      }

    print_word(ctx, " {\n");

    if (type._M_type)
      {
	print_word(ctx, "  type = ");
	print_type(ctx, type._M_type, "<unknown type>");
	print_word(ctx, ";\n");
      }
  }

  void
  print_description(PrintContext& ctx, const _Parameter::_Instance& inst)
  {
    const int bufsize = 64;
    char buf[bufsize];

    if (inst._M_name)
      {
	int written
	  = format_word(buf, bufsize, "\"%s\" ", inst._M_name);
	print_word(ctx, buf, written);
      }

    int written
      = __builtin_sprintf(buf, "@ 0x%p {\n", inst._M_address);
    print_word(ctx, buf, written);

    if (inst._M_type)
      {
	print_word(ctx, "  type = ");
	print_type(ctx, inst._M_type, "<unknown type>");
      }
  }

  void
  print_description(PrintContext& ctx, const _Parameter& param)
  {
    const int bufsize = 128;
    char buf[bufsize];

    const auto& variant = param._M_variant;
    switch (param._M_kind)
      {
      case _Parameter::__iterator:
	{
	  const auto& ite = variant._M_iterator;

	  print_word(ctx, "iterator ");
	  print_description(ctx, ite);

	  if (ite._M_type)
	    {
	      if (ite._M_constness != _Error_formatter::__unknown_constness)
		{
		  print_word(ctx, " (");
		  print_field(ctx, param, "constness");
		  print_word(ctx, " iterator)");
		}

	      print_word(ctx, ";\n");
	    }

	  if (ite._M_state != _Error_formatter::__unknown_state)
	    {
	      print_word(ctx, "  state = ");
	      print_field(ctx, param, "state");
	      print_word(ctx, ";\n");
	    }

	  if (ite._M_sequence)
	    {
	      print_word(ctx, "  references sequence ");
	      if (ite._M_seq_type)
		{
		  print_word(ctx, "with type '");
		  print_field(ctx, param, "seq_type");
		  print_word(ctx, "' ");
		}

	      int written
		= __builtin_sprintf(buf, "@ 0x%p\n", ite._M_sequence);
	      print_word(ctx, buf, written);
	    }

	  print_word(ctx, "}\n", 2);
	}
	break;

      case _Parameter::__sequence:
	print_word(ctx, "sequence ");
	print_description(ctx, variant._M_sequence);

	if (variant._M_sequence._M_type)
	  print_word(ctx, ";\n", 2);

	print_word(ctx, "}\n", 2);
	break;

      case _Parameter::__instance:
	print_word(ctx, "instance ");
	print_description(ctx, variant._M_instance);

	if (variant._M_instance._M_type)
	  print_word(ctx, ";\n", 2);

	print_word(ctx, "}\n", 2);
	break;

      case _Parameter::__iterator_value_type:
	print_word(ctx, "iterator::value_type ");
	print_description(ctx, variant._M_iterator_value_type);
	print_word(ctx, "}\n", 2);
	break;

      default:
	break;
      }
  }

  void
  print_string(PrintContext& ctx, const char* string,
	       const _Parameter* parameters, std::size_t num_parameters)
  {
    const char* start = string;
    const int bufsize = 128;
    char buf[bufsize];
    int bufindex = 0;

    while (*start)
      {
	if (isspace(*start))
	  {
	    buf[bufindex++] = *start++;
	    buf[bufindex] = '\0';
	    print_word(ctx, buf, bufindex);
	    bufindex = 0;
	    continue;
	  }

	if (*start != '%')
	  {
	    // Normal char.
	    buf[bufindex++] = *start++;
	    continue;
	  }

	if (*++start == '%')
	  {
	    // Escaped '%'
	    buf[bufindex++] = *start++;
	    continue;
	  }

	// We are on a parameter property reference, we need to flush buffer
	// first.
	if (bufindex != 0)
	  {
	    buf[bufindex] = '\0';
	    print_word(ctx, buf, bufindex);
	    bufindex = 0;
	  }

	// Get the parameter number
	assert(*start >= '1' && *start <= '9');
	size_t param_index = *start - '0' - 1;
	assert(param_index < num_parameters);
	const auto& param = parameters[param_index];

	// '.' separates the parameter number from the field
	// name, if there is one.
	++start;
	if (*start != '.')
	  {
	    assert(*start == ';');
	    ++start;
	    if (param._M_kind == _Parameter::__integer)
	      {
		int written
		  = __builtin_sprintf(buf, "%ld",
				      param._M_variant._M_integer._M_value);
		print_word(ctx, buf, written);
	      }
	    else if (param._M_kind == _Parameter::__string)
	      print_string(ctx, param._M_variant._M_string._M_value,
			   parameters, num_parameters);
	    continue;
	  }

	// Extract the field name we want
	const int max_field_len = 16;
	char field[max_field_len];
	int field_idx = 0;
	++start;
	while (*start != ';')
	  {
	    assert(*start);
	    assert(field_idx < max_field_len - 1);
	    field[field_idx++] = *start++;
	  }
	++start;
	field[field_idx] = '\0';

	print_field(ctx, param, field);
      }

    // Might need to flush.
    if (bufindex)
      {
	buf[bufindex] = '\0';
	print_word(ctx, buf, bufindex);
      }
  }
}

namespace __gnu_debug
{
  _Error_formatter&
  _Error_formatter::_M_message(_Debug_msg_id __id) const throw ()
  {
    return const_cast<_Error_formatter*>(this)
      ->_M_message(_S_debug_messages[__id]);
  }

  void
  _Error_formatter::_M_error() const
  {
    const int bufsize = 128;
    char buf[bufsize];

    // Emit file & line number information
    bool go_to_next_line = false;
    PrintContext ctx;
    if (_M_file)
      {
	int written = format_word(buf, bufsize, "%s:", _M_file);
	print_word(ctx, buf, written);
	go_to_next_line = true;
      }

    if (_M_line > 0)
      {
	int written = __builtin_sprintf(buf, "%u:", _M_line);
	print_word(ctx, buf, written);
	go_to_next_line = true;
      }

    if (go_to_next_line)
      print_word(ctx, "\n", 1);

    if (ctx._M_max_length)
      ctx._M_wordwrap = true;

    print_word(ctx, "Error: ");

    // Print the error message
    assert(_M_text);
    print_string(ctx, _M_text, _M_parameters, _M_num_parameters);
    print_word(ctx, ".\n", 2);

    // Emit descriptions of the objects involved in the operation
    ctx._M_first_line = true;
    ctx._M_wordwrap = false;
    bool has_header = false;
    for (unsigned int i = 0; i < _M_num_parameters; ++i)
      {
	switch (_M_parameters[i]._M_kind)
	  {
	  case _Parameter::__iterator:
	  case _Parameter::__sequence:
	  case _Parameter::__instance:
	  case _Parameter::__iterator_value_type:
	    if (!has_header)
	      {
		print_word(ctx, "\nObjects involved in the operation:\n");
		has_header = true;
	      }
	    print_description(ctx, _M_parameters[i]);
	    break;

	  default:
	    break;
	  }
      }

    abort();
  }

  // Deprecated methods kept for backward compatibility.
  void
  _Error_formatter::_Parameter::_M_print_field(
	const _Error_formatter*, const char*) const
  { }

  void
  _Error_formatter::_Parameter::_M_print_description(const _Error_formatter*) const
  { }

  template<typename _Tp>
    void
    _Error_formatter::_M_format_word(char*, int, const char*, _Tp)
    const throw ()
    { }

  void
  _Error_formatter::_M_print_word(const char*) const
  { }

  void
  _Error_formatter::_M_print_string(const char*) const
  { }

  void
  _Error_formatter::_M_get_max_length() const throw ()
  { }

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
