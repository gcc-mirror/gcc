// Methods for Exception Support for -*- C++ -*-

// Copyright (C) 2014-2024 Free Software Foundation, Inc.
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

//
// ISO C++ 14882: 19.1  Exception classes
//

// Enable hooks for support for the Transactional Memory TS (N4514).
#define _GLIBCXX_TM_TS_INTERNAL
void
_txnal_cow_string_C1_for_exceptions(void* that, const char* s, void* exc);
const char*
_txnal_cow_string_c_str(const void* that);
void
_txnal_cow_string_D1(void* that);
void
_txnal_cow_string_D1_commit(void* that);
void*
_txnal_logic_error_get_msg(void* e);
void*
_txnal_runtime_error_get_msg(void* e);

// All exception classes still use the classic COW std::string.
#define _GLIBCXX_USE_CXX11_ABI 0
#define _GLIBCXX_DEFINE_STDEXCEPT_COPY_OPS 1
#define __cow_string __cow_stringxxx
#include <stdexcept>
#undef __cow_string

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Copy/move constructors and assignment operators defined using COW string.
  // These operations are noexcept even though copying a COW string is not,
  // but we know that the string member in an exception has not been "leaked"
  // so copying is a simple reference count increment.

  logic_error::logic_error(const logic_error& e) noexcept
  : exception(e), _M_msg(e._M_msg) { }

  logic_error& logic_error::operator=(const logic_error& e) noexcept
  { _M_msg = e._M_msg; return *this; }

  logic_error::logic_error(logic_error&& e) noexcept = default;

  logic_error&
  logic_error::operator=(logic_error&& e) noexcept = default;

  runtime_error::runtime_error(const runtime_error& e) noexcept
  : exception(e), _M_msg(e._M_msg) { }

  runtime_error&
  runtime_error::operator=(const runtime_error& e) noexcept
  { _M_msg = e._M_msg; return *this; }

  runtime_error::runtime_error(runtime_error&& e) noexcept = default;

  runtime_error&
  runtime_error::operator=(runtime_error&& e) noexcept = default;

  // New C++11 constructors:

  logic_error::logic_error(const char* __arg)
  : exception(), _M_msg(__arg) { }

  domain_error::domain_error(const char* __arg)
  : logic_error(__arg) { }

  invalid_argument::invalid_argument(const char* __arg)
  : logic_error(__arg) { }

  length_error::length_error(const char* __arg)
  : logic_error(__arg) { }

  out_of_range::out_of_range(const char* __arg)
  : logic_error(__arg) { }

  runtime_error::runtime_error(const char* __arg)
  : exception(), _M_msg(__arg) { }

  range_error::range_error(const char* __arg)
  : runtime_error(__arg) { }

  overflow_error::overflow_error(const char* __arg)
  : runtime_error(__arg) { }

  underflow_error::underflow_error(const char* __arg)
  : runtime_error(__arg) { }

#if _GLIBCXX_USE_DUAL_ABI
  // Converting constructor from COW std::string to SSO string.
  __sso_string::__sso_string(const string& s)
  : __sso_string(s.c_str(), s.length()) { }

  // Redefine __cow_string so that we can define and export its members
  // in terms of the COW std::string.
  struct __cow_string
  {
    union {
      const char* _M_p;
      char _M_bytes[sizeof(_M_p)];
      std::string _M_str;
    };

    __cow_string();
    __cow_string(const std::string& s);
    __cow_string(const char*, size_t n);
    __cow_string(const __cow_string&) noexcept;
    __cow_string& operator=(const __cow_string&) noexcept;
    ~__cow_string();
    __cow_string(__cow_string&&) noexcept;
    __cow_string& operator=(__cow_string&&) noexcept;
  };

  __cow_string::__cow_string() : _M_str() { }

  __cow_string::__cow_string(const std::string& s) : _M_str(s) { }

  __cow_string::__cow_string(const char* s, size_t n) : _M_str(s, n) { }

  __cow_string::__cow_string(const __cow_string& s) noexcept
  : _M_str(s._M_str) { }

  __cow_string&
  __cow_string::operator=(const __cow_string& s) noexcept
  {
    _M_str = s._M_str;
    return *this;
  }

  __cow_string::~__cow_string() { _M_str.~basic_string(); }

  __cow_string::__cow_string(__cow_string&& s) noexcept
  : _M_str(std::move(s._M_str)) { }

  __cow_string&
  __cow_string::operator=(__cow_string&& s) noexcept
  {
    _M_str = std::move(s._M_str);
    return *this;
  }

  static_assert(sizeof(__cow_string) == sizeof(std::string),
                "sizeof(std::string) has changed");
  static_assert(alignof(__cow_string) == alignof(std::string),
                "alignof(std::string) has changed");
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

// Support for the Transactional Memory TS (N4514).
//
// logic_error and runtime_error both carry a message in the form of a COW
// string.  This COW string is never made visible to users of the exception
// because what() returns a C string.  The COW string can be constructed as
// either a copy of a COW string of another logic_error/runtime_error, or
// using a C string or SSO string; thus, the COW string's _Rep is only
// accessed by logic_error operations.  We control all txnal clones of those
// operations and thus can ensure that _Rep is never accessed transactionally.
// Furthermore, _Rep will always have been allocated or deallocated via
// global new or delete, so nontransactional writes we do to _Rep cannot
// interfere with transactional accesses.

// We depend on having support for referencing functions declared weak that
// are not defined by us.  Without such support, the exceptions will not be
// declared transaction-safe, so we just don't provide transactional clones
// in this case.
#if _GLIBCXX_USE_WEAK_REF
#ifdef _GLIBCXX_USE_C99_STDINT

#include <stdint.h>

using std::size_t;

extern "C" {

#ifndef _GLIBCXX_MANGLE_SIZE_T
#error Mangled name of size_t type not defined.
#endif
#define CONCAT1(x,y)		x##y
#define CONCAT(x,y)		CONCAT1(x,y)
#define _ZGTtnaX		CONCAT(_ZGTtna,_GLIBCXX_MANGLE_SIZE_T)

#ifdef __i386__
/* Only for 32-bit x86.  */
# define ITM_REGPARM	__attribute__((regparm(2)))
#else
# define ITM_REGPARM
#endif

// Declare all libitm symbols we rely on, but make them weak so that we do
// not depend on libitm.
extern void* _ZGTtnaX (size_t sz) __attribute__((weak));
extern void _ZGTtdlPv (void* ptr) __attribute__((weak));
extern uint8_t _ITM_RU1(const uint8_t *p)
  ITM_REGPARM __attribute__((weak));
extern uint16_t _ITM_RU2(const uint16_t *p)
  ITM_REGPARM __attribute__((weak));
extern uint32_t _ITM_RU4(const uint32_t *p)
  ITM_REGPARM __attribute__((weak));
extern uint64_t _ITM_RU8(const uint64_t *p)
  ITM_REGPARM __attribute__((weak));
extern void _ITM_memcpyRtWn(void *, const void *, size_t)
  ITM_REGPARM __attribute__((weak));
extern void _ITM_memcpyRnWt(void *, const void *, size_t)
  ITM_REGPARM __attribute__((weak));
extern void _ITM_addUserCommitAction(void (*)(void *), uint64_t, void *)
  ITM_REGPARM __attribute__((weak));

}

// A transactional version of basic_string::basic_string(const char *s)
// that also notifies the TM runtime about allocations belonging to this
// exception.
void
_txnal_cow_string_C1_for_exceptions(void* that, const char* s,
				    void *exc __attribute__((unused)))
{
  typedef std::basic_string<char> bs_type;
  bs_type *bs = (bs_type*) that;

  // First, do a transactional strlen, but including the trailing zero.
  bs_type::size_type len = 1;
  for (const char *ss = s; _ITM_RU1((const uint8_t*) ss) != 0; ss++, len++);


  // Allocate memory for the string and the refcount.  We use the
  // transactional clone of global new[]; if this throws, it will do so in a
  // transaction-compatible way.
  // The allocation belongs to this exception, so tell the runtime about it.
  // TODO Once this is supported, link the following allocation to this
  // exception: void *prev = _ITM_setAssociatedException(exc);
  bs_type::_Rep *rep;
  __try
    {
      rep = (bs_type::_Rep*) _ZGTtnaX (len + sizeof (bs_type::_Rep));
    }
  __catch (...)
    {
      // Pop the association with this exception.
      // TODO Once this is supported, link the following allocation to this
      // exception: _ITM_setAssociatedException(prev);
      // We do not need to instrument a rethrow.
      __throw_exception_again;
    }
  // Pop the association with this exception.
  // TODO Once this is supported, link the following allocation to this
  // exception: _ITM_setAssociatedException(prev);

  // Now initialize the rest of the string and copy the C string.  The memory
  // will be freshly allocated, so nontransactional accesses are sufficient,
  // including the writes when copying the string (see above).
  rep->_M_set_sharable();
  rep->_M_length = rep->_M_capacity = len - 1;
  _ITM_memcpyRtWn(rep->_M_refdata(), s, len);
  new (&bs->_M_dataplus) bs_type::_Alloc_hider(rep->_M_refdata(),
					       bs_type::allocator_type());
}

static void* txnal_read_ptr(void* const * ptr)
{
  static_assert(sizeof(uint64_t) == sizeof(void*)
		|| sizeof(uint32_t) == sizeof(void*)
		|| sizeof(uint16_t) == sizeof(void*),
		"Pointers must be 16 bits, 32 bits or 64 bits wide");
#if __UINTPTR_MAX__ == __UINT64_MAX__
  return (void*)_ITM_RU8((const uint64_t*)ptr);
#elif __UINTPTR_MAX__ == __UINT32_MAX__
  return (void*)_ITM_RU4((const uint32_t*)ptr);
#else
  return (void*)_ITM_RU2((const uint16_t*)ptr);
#endif
}

// We must access the data pointer in the COW string transactionally because
// another transaction can delete the string and reuse the memory.
const char*
_txnal_cow_string_c_str(const void* that)
{
  typedef std::basic_string<char> bs_type;
  const bs_type *bs = (const bs_type*) that;

  return (const char*) txnal_read_ptr((void**)&bs->_M_dataplus._M_p);
}

#if _GLIBCXX_USE_DUAL_ABI
const char*
_txnal_sso_string_c_str(const void* that)
{
  return (const char*) txnal_read_ptr(
      (void* const*)const_cast<char* const*>(
	  &((const std::__sso_string*) that)->_M_s._M_p));
}
#endif

void
_txnal_cow_string_D1_commit(void* data)
{
  typedef std::basic_string<char> bs_type;
  bs_type::_Rep *rep = (bs_type::_Rep*) data;
  rep->_M_dispose(bs_type::allocator_type());
}

void
_txnal_cow_string_D1(void* that)
{
  typedef std::basic_string<char> bs_type;
  bs_type::_Rep *rep = reinterpret_cast<bs_type::_Rep*>(
      const_cast<char*>(_txnal_cow_string_c_str(that))) - 1;

  // The string can be shared, in which case we would need to decrement the
  // reference count.  We cannot undo that because we might lose the string
  // otherwise.  Therefore, we register a commit action that will dispose of
  // the string's _Rep.
  enum {_ITM_noTransactionId  = 1};
  _ITM_addUserCommitAction(_txnal_cow_string_D1_commit, _ITM_noTransactionId,
			   rep);
}

void*
_txnal_logic_error_get_msg(void* e)
{
  std::logic_error* le = (std::logic_error*) e;
  return &le->_M_msg;
}

void*
_txnal_runtime_error_get_msg(void* e)
{
  std::runtime_error* le = (std::runtime_error*) e;
  return &le->_M_msg;
}

// The constructors are only declared transaction-safe if the C++11 ABI is
// used for std::string and the exception classes use a COW string internally.
// A user must not call these constructors otherwise; if they do, it will
// result in undefined behavior, which is in this case not initializing this
// string.
#if _GLIBCXX_USE_DUAL_ABI
#define CTORS_FROM_SSOSTRING(NAME, CLASS, BASE)			\
void									\
_ZGTtNSt##NAME##C1ERKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE( \
    CLASS* that, const std::__sso_string& s)				\
{									\
  CLASS e("");								\
  _ITM_memcpyRnWt(that, &e, sizeof(CLASS));				\
  /* Get the C string from the SSO string.  */				\
  _txnal_cow_string_C1_for_exceptions(_txnal_##BASE##_get_msg(that),	\
				      _txnal_sso_string_c_str(&s), that); \
}									\
void									\
_ZGTtNSt##NAME##C2ERKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE( \
    CLASS*, const std::__sso_string&) __attribute__((alias		\
("_ZGTtNSt" #NAME							\
  "C1ERKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE")));
#else
#define CTORS_FROM_SSOSTRING(NAME, CLASS, BASE)
#endif

// This macro defines transaction constructors and destructors for a specific
// exception class.  NAME is the variable part of the mangled name, CLASS is
// the class name, and BASE must be logic_error or runtime_error (which is
// then used to call the proper friend function that can return a pointer to
// the _M_msg member declared by the given (base) class).
#define CTORDTOR(NAME, CLASS, BASE)					\
void									\
_ZGTtNSt##NAME##C1EPKc (CLASS* that, const char* s)			\
{									\
  /* This will use the singleton _Rep for an empty string and just	\
     point to it instead of allocating memory.  Thus, we can use it as	\
     source, copy it into the object we are constructing, and then	\
     construct the COW string in the latter manually.  Note that the	\
     exception classes will not be declared transaction_safe if the	\
     shared empty _Rep is disabled with --enable-fully-dynamic-string	\
     (in which case _GLIBCXX_FULLY_DYNAMIC_STRING is nonzero).  */	\
  CLASS e("");								\
  _ITM_memcpyRnWt(that, &e, sizeof(CLASS));				\
  _txnal_cow_string_C1_for_exceptions(_txnal_##BASE##_get_msg(that),	\
				      s, that);				\
}									\
void									\
_ZGTtNSt##NAME##C2EPKc (CLASS*, const char*)				\
  __attribute__((alias ("_ZGTtNSt" #NAME "C1EPKc")));			\
CTORS_FROM_SSOSTRING(NAME, CLASS, BASE)					\
void									\
_ZGTtNSt##NAME##D1Ev(CLASS* that)					\
{ _txnal_cow_string_D1(_txnal_##BASE##_get_msg(that)); }		\
void									\
_ZGTtNSt##NAME##D2Ev(CLASS*)						\
__attribute__((alias ("_ZGTtNSt" #NAME "D1Ev")));			\
void									\
_ZGTtNSt##NAME##D0Ev(CLASS* that)					\
{									\
  _ZGTtNSt##NAME##D1Ev(that);						\
  _ZGTtdlPv(that);							\
}

// Now create all transactional constructors and destructors, as well as the
// two virtual what() functions.
extern "C" {

CTORDTOR(11logic_error, std::logic_error, logic_error)

const char*
_ZGTtNKSt11logic_error4whatEv(const std::logic_error* that)
{
  return _txnal_cow_string_c_str(_txnal_logic_error_get_msg(
      const_cast<std::logic_error*>(that)));
}

CTORDTOR(12domain_error, std::domain_error, logic_error)
CTORDTOR(16invalid_argument, std::invalid_argument, logic_error)
CTORDTOR(12length_error, std::length_error, logic_error)
CTORDTOR(12out_of_range, std::out_of_range, logic_error)


CTORDTOR(13runtime_error, std::runtime_error, runtime_error)

const char*
_ZGTtNKSt13runtime_error4whatEv(const std::runtime_error* that)
{
  return _txnal_cow_string_c_str(_txnal_runtime_error_get_msg(
      const_cast<std::runtime_error*>(that)));
}

CTORDTOR(11range_error, std::range_error, runtime_error)
CTORDTOR(14overflow_error, std::overflow_error, runtime_error)
CTORDTOR(15underflow_error, std::underflow_error, runtime_error)

}

#endif  // _GLIBCXX_USE_C99_STDINT
#endif  // _GLIBCXX_USE_WEAK_REF
