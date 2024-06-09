// { dg-do compile { target c++20 } }
// { dg-add-options no_pch }
// { dg-require-atomic-cmpxchg-word "PR libstdc++/114103" }

#include <atomic>

#ifndef __cpp_lib_atomic_lock_free_type_aliases
# error "Feature test macro for lock-free type aliases is missing in <atomic>"
#elif __cpp_lib_atomic_lock_free_type_aliases != 201907L
# error "Feature test macro for lock-free type aliases has wrong value in <atomic>"
#endif

template<typename T>
constexpr bool is_atomic_specialization = false;
template<typename T>
constexpr bool is_atomic_specialization<std::atomic<T>> = true;

// The type aliases atomic_signed_lock_free and atomic_unsigned_lock_free
// name specializations of atomic
static_assert( is_atomic_specialization<std::atomic_signed_lock_free> );
static_assert( is_atomic_specialization<std::atomic_unsigned_lock_free> );

#include <type_traits>

// ... whose template arguments are integral types,
static_assert( std::is_integral_v<std::atomic_signed_lock_free::value_type> );
static_assert( std::is_integral_v<std::atomic_unsigned_lock_free::value_type> );

// ... respectively signed and unsigned,
static_assert( std::is_signed_v<std::atomic_signed_lock_free::value_type> );
static_assert( std::is_unsigned_v<std::atomic_unsigned_lock_free::value_type> );

// and whose is_always_lock_free property is true.
static_assert( std::atomic_signed_lock_free::is_always_lock_free );
static_assert( std::atomic_unsigned_lock_free::is_always_lock_free );
