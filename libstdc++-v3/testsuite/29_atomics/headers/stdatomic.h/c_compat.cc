// { dg-options "-std=gnu++23" }
// { dg-do compile { target c++23 } }

#include <stdatomic.h>

#ifndef __cpp_lib_stdatomic_h
# error "Feature test macro for stdatomic.h is missing in <stdatomic.h>"
#elif __cpp_lib_stdatomic_h != 202011L
# error "Feature test macro for stdatomic.h has wrong value in <stdatomic.h>"
#endif

#ifndef ATOMIC_BOOL_LOCK_FREE
#error ATOMIC_BOOL_LOCK_FREE is not defined in <stdatomic.h>
#endif
#ifndef ATOMIC_CHAR_LOCK_FREE
#error ATOMIC_CHAR_LOCK_FREE is not defined in <stdatomic.h>
#endif
#ifndef ATOMIC_CHAR16_T_LOCK_FREE
#error ATOMIC_CHAR16_T_LOCK_FREE is not defined in <stdatomic.h>
#endif
#ifndef ATOMIC_CHAR32_T_LOCK_FREE
#error ATOMIC_CHAR32_T_LOCK_FREE is not defined in <stdatomic.h>
#endif
#ifndef ATOMIC_WCHAR_T_LOCK_FREE
#error ATOMIC_WCHAR_T_LOCK_FREE is not defined in <stdatomic.h>
#endif
#ifndef ATOMIC_SHORT_LOCK_FREE
#error ATOMIC_SHORT_LOCK_FREE is not defined in <stdatomic.h>
#endif
#ifndef ATOMIC_INT_LOCK_FREE
#error ATOMIC_INT_LOCK_FREE is not defined in <stdatomic.h>
#endif
#ifndef ATOMIC_LONG_LOCK_FREE
#error ATOMIC_LONG_LOCK_FREE is not defined in <stdatomic.h>
#endif
#ifndef ATOMIC_LLONG_LOCK_FREE
#error ATOMIC_LLONG_LOCK_FREE is not defined in <stdatomic.h>
#endif
#ifndef ATOMIC_POINTER_LOCK_FREE
#error ATOMIC_POINTER_LOCK_FREE is not defined in <stdatomic.h>
#endif

constexpr const memory_order* orders[] = {
  &memory_order_relaxed, &memory_order_consume, &memory_order_acquire,
  &memory_order_release, &memory_order_acq_rel, &memory_order_seq_cst
};

constexpr atomic_flag flag{};

template<typename A, typename B> constexpr bool is_same = false;
template<typename A> constexpr bool is_same<A, A> = true;

static_assert(is_same<atomic_bool,     _Atomic(bool)>);
static_assert(is_same<atomic_char,     _Atomic(char)>);
static_assert(is_same<atomic_schar,    _Atomic(signed char)>);
static_assert(is_same<atomic_uchar,    _Atomic(unsigned char)>);
static_assert(is_same<atomic_short,    _Atomic(short)>);
static_assert(is_same<atomic_ushort,   _Atomic(unsigned short)>);
static_assert(is_same<atomic_int,      _Atomic(int)>);
static_assert(is_same<atomic_uint,     _Atomic(unsigned int)>);
static_assert(is_same<atomic_long,     _Atomic(long)>);
static_assert(is_same<atomic_ulong,    _Atomic(unsigned long)>);
static_assert(is_same<atomic_llong,    _Atomic(long long)>);
static_assert(is_same<atomic_ullong,   _Atomic(unsigned long long)>);
#ifdef _GLIBCXX_USE_CHAR8_T
static_assert(is_same<atomic_char8_t,  _Atomic(char8_t)>);
#endif
static_assert(is_same<atomic_char16_t, _Atomic(char16_t)>);
static_assert(is_same<atomic_char32_t, _Atomic(char32_t)>);
static_assert(is_same<atomic_wchar_t,  _Atomic(wchar_t)>);

#include <stdint.h>
#ifdef _GLIBCXX_USE_C99_STDINT_TR1
static_assert(is_same<atomic_int8_t,   _Atomic(int8_t)>);
static_assert(is_same<atomic_uint8_t,  _Atomic(uint8_t)>);
static_assert(is_same<atomic_int16_t,  _Atomic(int16_t)>);
static_assert(is_same<atomic_uint16_t, _Atomic(uint16_t)>);
static_assert(is_same<atomic_int32_t,  _Atomic(int32_t)>);
static_assert(is_same<atomic_uint32_t, _Atomic(uint32_t)>);
static_assert(is_same<atomic_int64_t,  _Atomic(int64_t)>);
static_assert(is_same<atomic_uint64_t, _Atomic(uint64_t)>);
static_assert(is_same<atomic_int_least8_t,   _Atomic(int_least8_t)>);
static_assert(is_same<atomic_uint_least8_t,  _Atomic(uint_least8_t)>);
static_assert(is_same<atomic_int_least16_t,  _Atomic(int_least16_t)>);
static_assert(is_same<atomic_uint_least16_t, _Atomic(uint_least16_t)>);
static_assert(is_same<atomic_int_least32_t,  _Atomic(int_least32_t)>);
static_assert(is_same<atomic_uint_least32_t, _Atomic(uint_least32_t)>);
static_assert(is_same<atomic_int_least64_t,  _Atomic(int_least64_t)>);
static_assert(is_same<atomic_uint_least64_t, _Atomic(uint_least64_t)>);
static_assert(is_same<atomic_int_fast8_t,   _Atomic(int_fast8_t)>);
static_assert(is_same<atomic_uint_fast8_t,  _Atomic(uint_fast8_t)>);
static_assert(is_same<atomic_int_fast16_t,  _Atomic(int_fast16_t)>);
static_assert(is_same<atomic_uint_fast16_t, _Atomic(uint_fast16_t)>);
static_assert(is_same<atomic_int_fast32_t,  _Atomic(int_fast32_t)>);
static_assert(is_same<atomic_uint_fast32_t, _Atomic(uint_fast32_t)>);
static_assert(is_same<atomic_int_fast64_t,  _Atomic(int_fast64_t)>);
static_assert(is_same<atomic_uint_fast64_t, _Atomic(uint_fast64_t)>);
#endif
static_assert(is_same<atomic_intptr_t,  _Atomic(intptr_t)>);
static_assert(is_same<atomic_uintptr_t, _Atomic(uintptr_t)>);
#ifdef _GLIBCXX_USE_C99_STDINT_TR1
static_assert(is_same<atomic_intmax_t,  _Atomic(intmax_t)>);
static_assert(is_same<atomic_uintmax_t, _Atomic(uintmax_t)>);
#endif
#include <stddef.h>
static_assert(is_same<atomic_size_t,    _Atomic(size_t)>);
static_assert(is_same<atomic_ptrdiff_t, _Atomic(ptrdiff_t)>);

static_assert( requires (::atomic_int* i, int* e) {
  ::atomic_is_lock_free(i);
  ::atomic_load(i);
  ::atomic_load_explicit(i, memory_order_relaxed);
  ::atomic_store(i, 2);
  ::atomic_store_explicit(i, 3, memory_order_release);
  ::atomic_exchange(i, 2);
  ::atomic_exchange_explicit(i, 3, memory_order_acq_rel);
  ::atomic_compare_exchange_strong(i, e, 2);
  ::atomic_compare_exchange_strong_explicit(i, e, 3,
					    memory_order_acq_rel,
					    memory_order_relaxed);
  ::atomic_compare_exchange_weak(i, e, 2);
  ::atomic_compare_exchange_weak_explicit(i, e, 3,
					  memory_order_acq_rel,
					  memory_order_relaxed);

  ::atomic_fetch_add(i, 1);
  ::atomic_fetch_add_explicit(i, 1, memory_order_relaxed);
  ::atomic_fetch_sub(i, 1);
  ::atomic_fetch_sub_explicit(i, 1, memory_order_relaxed);
  ::atomic_fetch_and(i, 1);
  ::atomic_fetch_and_explicit(i, 1, memory_order_relaxed);
  ::atomic_fetch_or(i, 1);
  ::atomic_fetch_or_explicit(i, 1, memory_order_relaxed);
  ::atomic_fetch_xor(i, 1);
  ::atomic_fetch_xor_explicit(i, 1, memory_order_relaxed);
} );

static_assert( requires (::atomic_flag* f) {
  ::atomic_flag_test_and_set(f);
  ::atomic_flag_test_and_set_explicit(f, memory_order_relaxed);
  ::atomic_flag_clear(f);
  ::atomic_flag_clear_explicit(f, memory_order_acq_rel);
} );

static_assert( requires (::memory_order o) {
  ::atomic_thread_fence(o);
  ::atomic_signal_fence(o);
} );
