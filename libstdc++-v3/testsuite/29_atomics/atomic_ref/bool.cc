// { dg-do compile { target c++20 } }

#include <atomic>

template<class T> concept has_and = requires (T& a) { a &= false; };
template<class T> concept has_or = requires (T& a) { a |= false; };
template<class T> concept has_xor = requires (T& a) { a ^= false; };
template<class T> concept has_fetch_add = requires (T& a) { a.fetch_add(true); };
template<class T> concept has_fetch_sub = requires (T& a) { a.fetch_sub(true); };

static_assert( not has_and<std::atomic_ref<bool>> );
static_assert( not has_or<std::atomic_ref<bool>> );
static_assert( not has_xor<std::atomic_ref<bool>> );
static_assert( not has_fetch_add<std::atomic_ref<bool>> );
static_assert( not has_fetch_sub<std::atomic_ref<bool>> );
