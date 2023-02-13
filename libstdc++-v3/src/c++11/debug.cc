// Debugging mode support code -*- C++ -*-

// Copyright (C) 2003-2023 Free Software Foundation, Inc.
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
#include <debug/vector>

#include <cassert>
#include <cstdio>	// for std::fprintf, stderr
#include <cstdlib>	// for std::abort
#include <cctype>	// for std::isspace.
#include <cstring>	// for std::strstr.
#include <climits>	// for INT_MAX

#include <algorithm>	// for std::min.

#include <cxxabi.h>	// for __cxa_demangle.

#include "mutex_pool.h"

#ifdef _GLIBCXX_VERBOSE_ASSERT
namespace std
{
  [[__noreturn__]]
  void
  __glibcxx_assert_fail(const char* file, int line,
			const char* function, const char* condition) noexcept
  {
    if (file && function && condition)
      fprintf(stderr, "%s:%d: %s: Assertion '%s' failed.\n",
	      file, line, function, condition);
    else if (function)
      fprintf(stderr, "%s: Undefined behavior detected.\n", function);
    abort();
  }
}
#endif

using namespace std;

namespace
{
  /** Returns different instances of __mutex depending on the passed address
   *  in order to limit contention without breaking current library binary
   *  compatibility. */
  __gnu_cxx::__mutex&
  get_safe_base_mutex(void* address)
  {
    // Use arbitrarily __gnu_debug::vector<int> as the container giving
    // alignment of debug containers.
    const auto alignbits = __builtin_ctz(alignof(__gnu_debug::vector<int>));
    const unsigned char index
      = (reinterpret_cast<std::size_t>(address) >> alignbits)
      & __gnu_internal::mask;
    return __gnu_internal::get_mutex(index);
  }

#pragma GCC diagnostic push
// Suppress -Wabi=2 warnings due to PR c++/51322 mangling change
#pragma GCC diagnostic warning "-Wabi=6"

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
  swap_seq_single(__gnu_debug::_Safe_sequence_base& __lhs,
		  __gnu_debug::_Safe_sequence_base& __rhs)
  {
    swap(__lhs._M_version, __rhs._M_version);
    swap_its(__lhs, __lhs._M_iterators,
	     __rhs, __rhs._M_iterators);
    swap_its(__lhs, __lhs._M_const_iterators,
	     __rhs, __rhs._M_const_iterators);
  }
#pragma GCC diagnostic pop

  template<typename _Action>
    void
    lock_and_run(__gnu_cxx::__mutex& lhs_mutex, __gnu_cxx::__mutex& rhs_mutex,
		 _Action action)
    {
      // We need to lock both sequences to run action.
      if (&lhs_mutex == &rhs_mutex)
	{
	  __gnu_cxx::__scoped_lock sentry(lhs_mutex);
	  action();
	}
      else
	{
	  __gnu_cxx::__scoped_lock sentry1(&lhs_mutex < &rhs_mutex
					   ? lhs_mutex : rhs_mutex);
	  __gnu_cxx::__scoped_lock sentry2(&lhs_mutex < &rhs_mutex
					   ? rhs_mutex : lhs_mutex);
	  action();
	}
    }

  void
  swap_seq(__gnu_cxx::__mutex& lhs_mutex,
	   __gnu_debug::_Safe_sequence_base& lhs,
	   __gnu_cxx::__mutex& rhs_mutex,
	   __gnu_debug::_Safe_sequence_base& rhs)
  {
    lock_and_run(lhs_mutex, rhs_mutex,
		 [&lhs, &rhs]() { swap_seq_single(lhs, rhs); });
  }

  void
  swap_ucont_single(__gnu_debug::_Safe_unordered_container_base& __lhs,
		    __gnu_debug::_Safe_unordered_container_base& __rhs)
  {
    swap_seq_single(__lhs, __rhs);
    swap_its(__lhs, __lhs._M_local_iterators,
	     __rhs, __rhs._M_local_iterators);
    swap_its(__lhs, __lhs._M_const_local_iterators,
	     __rhs, __rhs._M_const_local_iterators);
  }

  void
  swap_ucont(__gnu_cxx::__mutex& lhs_mutex,
	     __gnu_debug::_Safe_unordered_container_base& lhs,
	     __gnu_cxx::__mutex& rhs_mutex,
	     __gnu_debug::_Safe_unordered_container_base& rhs)
  {
    lock_and_run(lhs_mutex, rhs_mutex,
		 [&lhs, &rhs]() { swap_ucont_single(lhs, rhs); });
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
    // __msg_valid_range
    "function requires a valid iterator range [%1.name;, %2.name;)",
    // __msg_insert_singular
    "attempt to insert into container with a singular iterator",
    // __msg_insert_different
    "attempt to insert into container with an iterator"
    " from a different container",
    // __msg_erase_bad
    "attempt to erase from container with a %2.state; iterator",
    // __msg_erase_different
    "attempt to erase from container with an iterator"
    " from a different container",
    // __msg_subscript_oob
    "attempt to subscript container with out-of-bounds index %2;,"
    " but container only holds %3; elements",
    // __msg_empty
    "attempt to access an element in an empty container",
    // __msg_unpartitioned
    "elements in iterator range [%1.name;, %2.name;)"
    " are not partitioned by the value %3;",
    // __msg_unpartitioned_pred
    "elements in iterator range [%1.name;, %2.name;)"
    " are not partitioned by the predicate %3; and value %4;",
    // __msg_unsorted
    "elements in iterator range [%1.name;, %2.name;) are not sorted",
    // __msg_unsorted_pred
    "elements in iterator range [%1.name;, %2.name;)"
    " are not sorted according to the predicate %3;",
    // __msg_not_heap
    "elements in iterator range [%1.name;, %2.name;) do not form a heap",
    // __msg_not_heap_pred
    "elements in iterator range [%1.name;, %2.name;)"
    " do not form a heap with respect to the predicate %3;",
    // std::bitset checks
    // __msg_bad_bitset_write
    "attempt to write through a singular bitset reference",
    // __msg_bad_bitset_read
    "attempt to read from a singular bitset reference",
    // __msg_bad_bitset_flip
    "attempt to flip a singular bitset reference",
    // std::list checks
    // __msg_self_splice
    "attempt to splice a list into itself",
    // __msg_splice_alloc
    "attempt to splice lists with unequal allocators",
    // __msg_splice_bad
    "attempt to splice elements referenced by a %1.state; iterator",
    // __msg_splice_other
    "attempt to splice an iterator from a different container",
    // __msg_splice_overlap
    "splice destination %1.name;"
    " occurs within source range [%2.name;, %3.name;)",
    // iterator checks
    // __msg_init_singular
    "attempt to initialize an iterator that will immediately become singular",
    // __msg_init_copy_singular
    "attempt to copy-construct an iterator from a singular iterator",
    // __msg_init_const_singular
    "attempt to construct a constant iterator"
    " from a singular mutable iterator",
    // __msg_copy_singular
    "attempt to copy from a singular iterator",
    // __msg_bad_deref
    "attempt to dereference a %1.state; iterator",
    // __msg_bad_inc
    "attempt to increment a %1.state; iterator",
    // __msg_bad_dec
    "attempt to decrement a %1.state; iterator",
    // __msg_iter_subscript_oob
    "attempt to subscript a %1.state; iterator %2; step from"
    " its current position, which falls outside its dereferenceable range",
    // __msg_advance_oob
    "attempt to advance a %1.state; iterator %2; steps,"
    " which falls outside its valid range",
    // __msg_retreat_oob
    "attempt to retreat a %1.state; iterator %2; steps,"
    " which falls outside its valid range",
    // __msg_iter_compare_bad
    "attempt to compare a %1.state; iterator to a %2.state; iterator",
    // __msg_compare_different
    "attempt to compare iterators from different sequences",
    // __msg_iter_order_bad
    "attempt to order a %1.state; iterator to a %2.state; iterator",
    // __msg_order_different
    "attempt to order iterators from different sequences",
    // __msg_distance_bad
    "attempt to compute the difference between a %1.state;"
    " iterator to a %2.state; iterator",
    // __msg_distance_different
    "attempt to compute the different between two iterators"
    " from different sequences",
    // istream_iterator
    // __msg_deref_istream
    "attempt to dereference an end-of-stream istream_iterator",
    // __msg_inc_istream
    "attempt to increment an end-of-stream istream_iterator",
    // ostream_iterator
    // __msg_output_ostream
    "attempt to output via an ostream_iterator with no associated stream",
    // istreambuf_iterator
    // __msg_deref_istreambuf
    "attempt to dereference an end-of-stream istreambuf_iterator"
    " (this is a GNU extension)",
    // __msg_inc_istreambuf
    "attempt to increment an end-of-stream istreambuf_iterator",
    // std::forward_list
    // __msg_insert_after_end
    "attempt to insert into container after an end iterator",
    // __msg_erase_after_bad
    "attempt to erase from container after a %2.state; iterator not followed"
    " by a dereferenceable one",
    // __msg_valid_range2
    "function requires a valid iterator range (%2.name;, %3.name;)"
    ", \"%2.name;\" shall be before and not equal to \"%3.name;\"",
    // std::unordered_container::local_iterator
    // __msg_local_iter_compare_bad
    "attempt to compare local iterators from different unordered container"
    " buckets",
    // __msg_non_empty_range
    "function requires a non-empty iterator range [%1.name;, %2.name;)",
    // __msg_self_move_assign
    "attempt to self move assign",
    // __msg_bucket_index_oob
    "attempt to access container with out-of-bounds bucket index %2;,"
    " container only holds %3; buckets",
    // __msg_valid_load_factor
    "load factor shall be positive",
    // __msg_equal_allocs
    "allocators must be equal",
    // __msg_insert_range_from_self
    "attempt to insert with an iterator range [%1.name;, %2.name;) from this"
    " container",
    // __msg_irreflexive_ordering
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
  { swap_seq(_M_get_mutex(), *this, __x._M_get_mutex(), __x); }

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
    // This function can run concurrently with the sequence destructor,
    // so there is a TOCTTOU race here: the sequence could be destroyed
    // after we check that _M_sequence is not null. Use the pointer value
    // to acquire the mutex (rather than via _M_sequence->_M_get_mutex()).
    // If the sequence destructor runs between loading the pointer and
    // locking the mutex, it will detach this iterator and set _M_sequence
    // to null, and then _M_detach_single() will do nothing.
    if (auto seq = __atomic_load_n(&_M_sequence, __ATOMIC_ACQUIRE))
      {
	__gnu_cxx::__scoped_lock sentry(get_safe_base_mutex(seq));
	_M_detach_single();
      }
  }

  void
  _Safe_iterator_base::
  _M_detach_single() throw ()
  {
    if (_M_sequence)
      {
	_M_sequence->_M_detach_single(this);
	_M_reset();
      }
  }

  void
  _Safe_iterator_base::
  _M_reset() throw ()
  {
    __atomic_store_n(&_M_sequence, (_Safe_sequence_base*)0, __ATOMIC_RELEASE);
    // Do not reset version, so that a detached iterator does not look like a
    // value-initialized one.
    // _M_version = 0;
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
  { return _M_sequence == __x._M_sequence; }

  __gnu_cxx::__mutex&
  _Safe_iterator_base::
  _M_get_mutex() throw ()
  { return _M_sequence->_M_get_mutex(); }

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
    if (auto seq = __atomic_load_n(&_M_sequence, __ATOMIC_ACQUIRE))
      {
	__gnu_cxx::__scoped_lock sentry(get_safe_base_mutex(seq));
	_M_detach_single();
      }
  }

  void
  _Safe_local_iterator_base::
  _M_detach_single() throw ()
  {
    if (_M_sequence)
      {
	_M_get_container()->_M_detach_local_single(this);
	_M_reset();
      }
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
  { swap_ucont(_M_get_mutex(), *this, __x._M_get_mutex(), __x); }

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

    static constexpr int _S_indent = 4;
    std::size_t	_M_max_length;
    std::size_t	_M_column;
    bool	_M_first_line;
    bool	_M_wordwrap;
  };

  using _Print_func_t = void (PrintContext&, const char*, ptrdiff_t);

  template<size_t Length>
    void
    print_literal(PrintContext& ctx, const char(&word)[Length])
    { print_word(ctx, word, Length - 1); }

  void
  print_word(PrintContext& ctx, const char* word, ptrdiff_t nbc = -1)
  {
    size_t length = nbc >= 0 ? nbc : __builtin_strlen(word);
    if (length == 0)
      return;

    // First consider '\n' at the beginning because it impacts the column.
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
      = isspace((unsigned char)word[length - 1]) ? length - 1 : length;
    if (visual_length == 0
	|| !ctx._M_wordwrap
	|| (ctx._M_column + visual_length < ctx._M_max_length)
	|| (visual_length >= ctx._M_max_length && ctx._M_column == 1))
      {
	// If this isn't the first line, indent.
	if (ctx._M_column == 1 && !ctx._M_first_line)
	  ctx._M_column += fprintf(stderr, "%*c", PrintContext::_S_indent, ' ');

	int written = fprintf(stderr, "%.*s", (int)length, word);

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
	print_literal(ctx, "\n");
	print_word(ctx, word, nbc);
      }
  }

  void
  pretty_print(PrintContext& ctx, const char* str, _Print_func_t print_func)
  {
    const char cxx1998[] = "cxx1998::";
    for (;;)
      {
	if (auto pos = strstr(str, "__"))
	  {
	    if (pos != str)
	      print_func(ctx, str, pos - str);

	    pos += 2; // advance past "__"
	    if (memcmp(pos, cxx1998, 9) == 0)
	      pos += 9; // advance past "cxx1998::"

	    str = pos;
	  }
	else
	  {
	    print_func(ctx, str, -1);
	    break;
	  }
      }
  }

  template<size_t Length>
    void
    print_type_info(PrintContext& ctx,
		    const type_info* info,
		    const char(&unknown_name)[Length])
    {
      if (!info)
	print_literal(ctx, unknown_name);
      else
	{
	  int status;
	  char* demangled_name =
	    __cxxabiv1::__cxa_demangle(info->name(), NULL, NULL, &status);
	  if (status == 0)
	    pretty_print(ctx, demangled_name, &print_word);
	  else
	    print_word(ctx, info->name());
	  free(demangled_name);
	}
    }

  void
  print_address(PrintContext& ctx, const char* fmt, const void* address)
  {
    char buf[128];
    int written = __builtin_sprintf(buf, fmt, address);
    print_word(ctx, buf, written);
  }

  void
  print_address(PrintContext& ctx, const void* address)
  { print_address(ctx, "%p", address); }

  void
  print_integer(PrintContext& ctx, long integer)
  {
    char buf[64];
    int written = __builtin_sprintf(buf, "%ld", integer);
    print_word(ctx, buf, written);
  }

  void
  print_named_name(PrintContext& ctx, const _Parameter::_Named& named)
  {
    assert(named._M_name);
    pretty_print(ctx, named._M_name, print_word);
  }

  template<typename _Iterator>
    void
    print_iterator_constness(PrintContext& ctx, const _Iterator& iterator)
    {
      static const char*
	constness_names[_Error_formatter::__last_constness] =
	{
	 "<unknown constness>",
	 "constant",
	 "mutable"
	};
      print_word(ctx, constness_names[iterator._M_constness]);
    }

  template<typename _Iterator>
    void
    print_iterator_state(PrintContext& ctx, const _Iterator& iterator)
    {
      static const char*
	state_names[_Error_formatter::__last_state] =
	{
	 "<unknown state>",
	 "singular",
	 "dereferenceable (start-of-sequence)",
	 "dereferenceable",
	 "past-the-end",
	 "before-begin",
	 "dereferenceable (start-of-reverse-sequence)",
	 "dereferenceable (reverse)",
	 "past-the-reverse-end",
	 "singular (value-initialized)"
	};
      print_word(ctx, state_names[iterator._M_state]);
    }

  template<typename _Iterator>
    void
    print_iterator_seq_type(PrintContext& ctx, const _Iterator& iterator)
    { print_type_info(ctx, iterator._M_seq_type, "<unknown seq_type>"); }

  bool
  print_named_field(PrintContext& ctx,
		    const char* fname, const _Parameter::_Named& named)
  {
    if (__builtin_strcmp(fname, "name") == 0)
      print_named_name(ctx, named);
    else
      return false;

    return true;
  }

  bool
  print_type_field(PrintContext& ctx,
		   const char* fname, const _Parameter::_Type& type)
  {
    if (print_named_field(ctx, fname, type))
      { }
    else if (__builtin_strcmp(fname, "type") == 0)
      print_type_info(ctx, type._M_type, "<unknown type>");
    else
      return false;

    return true;
  }

  bool
  print_instance_field(PrintContext& ctx,
		       const char* fname, const _Parameter::_Instance& inst)
  {
    if (print_type_field(ctx, fname, inst))
      { }
    else if (__builtin_strcmp(fname, "address") == 0)
      print_address(ctx, inst._M_address);
    else
      return false;

    return true;
  }

  template<typename _Iterator>
    bool
    print_iterator_field(PrintContext& ctx,
			 const char* fname, const _Iterator& iterator)
    {
      if (print_instance_field(ctx, fname, iterator))
	{ }
      else if (__builtin_strcmp(fname, "constness") == 0)
	print_iterator_constness(ctx, iterator);
      else if (__builtin_strcmp(fname, "state") == 0)
	print_iterator_state(ctx, iterator);
      else if (__builtin_strcmp(fname, "sequence") == 0)
	{
	  assert(iterator._M_sequence);
	  print_address(ctx, iterator._M_sequence);
	}
      else if (__builtin_strcmp(fname, "seq_type") == 0)
	print_iterator_seq_type(ctx, iterator);
      else
	return false;

      return true;
    }

  void
  print_field(PrintContext& ctx, const _Parameter& param, const char* fname)
  {
    assert(param._M_kind != _Parameter::__unused_param);

    const auto& variant = param._M_variant;
    switch (param._M_kind)
    {
    case _Parameter::__iterator:
      if (!print_iterator_field(ctx, fname, variant._M_iterator))
	assert(false);
      break;

    case _Parameter::__sequence:
      if (!print_instance_field(ctx, fname, variant._M_sequence))
	assert(false);
      break;

    case _Parameter::__integer:
      if (!print_named_field(ctx, fname, variant._M_integer))
	assert(false);
      break;

    case _Parameter::__string:
      if (!print_named_field(ctx, fname, variant._M_string))
	assert(false);
      break;

    case _Parameter::__instance:
      if (!print_instance_field(ctx, fname, variant._M_instance))
	assert(false);
      break;

    case _Parameter::__iterator_value_type:
      if (!print_type_field(ctx, fname, variant._M_iterator_value_type))
	assert(false);
      break;

    default:
      assert(false);
      break;
    }
  }

  void
  print_quoted_named_name(PrintContext& ctx, const _Parameter::_Named& named)
  {
    if (named._M_name)
      {
	print_literal(ctx, "\"");
	print_named_name(ctx, named);
	print_literal(ctx, "\" ");
      }
  }

  void
  print_type_type(PrintContext& ctx, const _Parameter::_Type& type,
		  bool close_desc = true)
  {
    if (type._M_type)
      {
	print_literal(ctx, "  type = ");
	print_type_info(ctx, type._M_type, "<unknown type>");
	if (close_desc)
	  print_literal(ctx, ";\n");
      }
  }

  void
  print_type(PrintContext& ctx, const _Parameter::_Type& type)
  {
    print_quoted_named_name(ctx, type);
    print_literal(ctx, " {\n");
    print_type_type(ctx, type);
    print_literal(ctx, "}\n");
  }

  void
  print_instance(PrintContext& ctx, const _Parameter::_Instance& inst,
		 bool close_desc = true)
  {
    print_quoted_named_name(ctx, inst);
    print_address(ctx, "@ %p {\n", inst._M_address);
    print_type_type(ctx, inst, close_desc);

    if (close_desc)
      print_literal(ctx, "}\n");
  }

  void
  print_description(PrintContext& ctx, const _Parameter& param)
  {
    const auto& variant = param._M_variant;
    switch (param._M_kind)
      {
      case _Parameter::__iterator:
	{
	  const auto& ite = variant._M_iterator;

	  print_literal(ctx, "iterator ");
	  print_instance(ctx, ite, false);

	  if (ite._M_type)
	    {
	      if (ite._M_constness != _Error_formatter::__unknown_constness)
		{
		  print_literal(ctx, " (");
		  print_iterator_constness(ctx, ite);
		  print_literal(ctx, " iterator)");
		}

	      print_literal(ctx, ";\n");
	    }

	  if (ite._M_state != _Error_formatter::__unknown_state)
	    {
	      print_literal(ctx, "  state = ");
	      print_iterator_state(ctx, ite);
	      print_literal(ctx, ";\n");
	    }

	  if (ite._M_sequence)
	    {
	      print_literal(ctx, "  references sequence ");
	      if (ite._M_seq_type)
		{
		  print_literal(ctx, "with type '");
		  print_iterator_seq_type(ctx, ite);
		  print_literal(ctx, "' ");
		}

	      print_address(ctx, "@ %p\n", ite._M_sequence);
	    }

	  print_literal(ctx, "}\n");
	}
	break;

      case _Parameter::__sequence:
	print_literal(ctx, "sequence ");
	print_instance(ctx, variant._M_sequence);
	break;

      case _Parameter::__instance:
	print_literal(ctx, "instance ");
	print_instance(ctx, variant._M_instance);
	break;

      case _Parameter::__iterator_value_type:
	print_literal(ctx, "iterator::value_type ");
	print_type(ctx, variant._M_iterator_value_type);
	break;

      default:
	break;
      }
  }

  void
  print_string(PrintContext& ctx, const char* str, ptrdiff_t nbc,
	       const _Parameter* parameters, std::size_t num_parameters)
  {
    const char* start = str;
    const char* end = nbc >= 0 ? start + nbc : nullptr;

    while ((end && str != end) || (!end && *str))
      {
	if (isspace((unsigned char)*str))
	  {
	    ++str;
	    print_word(ctx, start, str - start);
	    start = str;
	    continue;
	  }

	if (!parameters || *str != '%')
	  {
	    // Normal char or no parameter to look for.
	    ++str;
	    continue;
	  }

	if (*++str == '%')
	  {
	    // Escaped '%'
	    print_word(ctx, start, str - start);
	    ++str;
	    start = str;
	    continue;
	  }

	// We are on a parameter property reference, we need to flush buffer
	// first.
	if (str != start)
	  {
	    // Avoid printing the '%'.
	    if (str - start > 1)
	      print_word(ctx, start, str - start - 1);
	    start = str;
	  }

	// Get the parameter number
	assert(*str >= '1' && *str <= '9');
	size_t param_index = *str - '0' - 1;
	assert(param_index < num_parameters);
	const auto& param = parameters[param_index];

	// '.' separates the parameter number from the field
	// name, if there is one.
	++str;
	if (*str != '.')
	  {
	    assert(*str == ';');
	    ++str;
	    if (param._M_kind == _Parameter::__integer)
	      print_integer(ctx, param._M_variant._M_integer._M_value);
	    else if (param._M_kind == _Parameter::__string)
	      print_string(ctx, param._M_variant._M_string._M_value, -1,
			   parameters, num_parameters);
	    start = str;
	    continue;
	  }

	// Extract the field name we want
	const int max_field_len = 16;
	char field[max_field_len];
	int field_idx = 0;
	++str;
	while (*str != ';')
	  {
	    assert(*str);
	    assert(field_idx < max_field_len - 1);
	    field[field_idx++] = *str++;
	  }
	++str;
	field[field_idx] = '\0';

	print_field(ctx, param, field);
	start = str;
      }

    // Might need to flush.
    if (str != start)
      print_word(ctx, start, str - start);
  }

  void
  print_string(PrintContext& ctx, const char* str, ptrdiff_t nbc)
  { print_string(ctx, str, nbc, nullptr, 0); }

#if _GLIBCXX_HAVE_STACKTRACE
  void
  print_raw(PrintContext& ctx, const char* str, ptrdiff_t nbc)
  {
    if (nbc == -1)
      nbc = INT_MAX;
    ctx._M_column += fprintf(stderr, "%.*s", (int)nbc, str);
  }

  int
  print_backtrace(void* data, __UINTPTR_TYPE__ pc, const char* filename,
		  int lineno, const char* function)
  {
    const int bufsize = 64;
    char buf[bufsize];

    PrintContext& ctx = *static_cast<PrintContext*>(data);

    int written = __builtin_sprintf(buf, "%p ", (void*)pc);
    print_word(ctx, buf, written);

    int ret = 0;
    if (function)
      {
	int status;
	char* demangled_name =
	  __cxxabiv1::__cxa_demangle(function, NULL, NULL, &status);
	if (status == 0)
	  pretty_print(ctx, demangled_name, &print_raw);
	else
	  print_word(ctx, function);

	free(demangled_name);
	ret = strstr(function, "main") ? 1 : 0;
      }

    print_literal(ctx, "\n");

    if (filename)
      {
	bool wordwrap = false;
	swap(wordwrap, ctx._M_wordwrap);
	print_word(ctx, filename);

	if (lineno)
	  {
	    written = __builtin_sprintf(buf, ":%u\n", lineno);
	    print_word(ctx, buf, written);
	  }
	else
	  print_literal(ctx, "\n");
	swap(wordwrap, ctx._M_wordwrap);
      }
    else
      print_literal(ctx, "???:0\n");

    return ret;
  }

  void
  print_backtrace_error(void* data, const char* msg, int errnum)
  {
    PrintContext& ctx = *static_cast<PrintContext*>(data);

    print_literal(ctx, "Backtrace unavailable: ");
    print_word(ctx, msg ? msg : "<unknown error>");
    if (errnum > 0)
      {
	char buf[64];
	int written = __builtin_sprintf(buf, " (errno=%d)\n", errnum);
	print_word(ctx, buf, written);
      }
    else
      print_literal(ctx, "\n");
  }
#endif
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
    // Emit file & line number information
    bool go_to_next_line = false;
    PrintContext ctx;
    if (_M_file)
      {
	ctx._M_column += fprintf(stderr, "%s", _M_file);
	print_literal(ctx, ":");
	go_to_next_line = true;
      }

    if (_M_line > 0)
      {
	ctx._M_column += fprintf(stderr, "%u", _M_line);
	print_literal(ctx, ":");
	go_to_next_line = true;
      }

    if (go_to_next_line)
      print_literal(ctx, "\n");

    if (ctx._M_max_length)
      ctx._M_wordwrap = true;

    if (_M_function)
      {
	print_literal(ctx, "In function:\n");
	pretty_print(ctx, _M_function, &print_string);
	print_literal(ctx, "\n");
	ctx._M_first_line = true;
	print_literal(ctx, "\n");
      }

#if _GLIBCXX_HAVE_STACKTRACE
    if (_M_backtrace_state)
      {
	print_literal(ctx, "Backtrace:\n");
	_M_backtrace_full(
	  _M_backtrace_state, 1, print_backtrace, print_backtrace_error, &ctx);
	ctx._M_first_line = true;
	print_literal(ctx, "\n");
      }
#endif

    print_literal(ctx, "Error: ");

    // Print the error message
    assert(_M_text);
    print_string(ctx, _M_text, -1, _M_parameters, _M_num_parameters);
    print_literal(ctx, ".\n");

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
		print_literal(ctx, "\nObjects involved in the operation:\n");
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

#if !_GLIBCXX_INLINE_VERSION
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
#endif

} // namespace __gnu_debug
