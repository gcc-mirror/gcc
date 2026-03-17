// { dg-do compile { target c++20 } }

// LWG 4243. as_bytes/as_writable_bytes is broken with span<volatile T>

#include <span>

template<typename ElementType, std::size_t Extent = std::dynamic_extent>
concept can_call_as_bytes
  = requires (std::span<ElementType, Extent> s) { std::as_bytes(s); };

static_assert( can_call_as_bytes<int> );
static_assert( can_call_as_bytes<int, 10> );
static_assert( can_call_as_bytes<const int> );
static_assert( can_call_as_bytes<const int, 10> );
static_assert( ! can_call_as_bytes<volatile int> );
static_assert( ! can_call_as_bytes<volatile int, 10> );
static_assert( ! can_call_as_bytes<const volatile int> );
static_assert( ! can_call_as_bytes<const volatile int, 10> );

template<typename ElementType, std::size_t Extent = std::dynamic_extent>
concept can_call_as_writable_bytes
  = requires (std::span<ElementType, Extent> s) { std::as_writable_bytes(s); };

static_assert( can_call_as_writable_bytes<int> );
static_assert( can_call_as_writable_bytes<int, 10> );
static_assert( ! can_call_as_writable_bytes<const int> );
static_assert( ! can_call_as_writable_bytes<const int, 10> );
static_assert( ! can_call_as_writable_bytes<volatile int> );
static_assert( ! can_call_as_writable_bytes<volatile int, 10> );
static_assert( ! can_call_as_writable_bytes<const volatile int> );
static_assert( ! can_call_as_writable_bytes<const volatile int, 10> );
