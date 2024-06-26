// { dg-do compile { target c++11 } }

// Check alignment of the buffer types used for uninitialized storage.

#include <ext/aligned_buffer.h>

template<typename T> using membuf = __gnu_cxx::__aligned_membuf<T>;
template<typename T> using objbuf = __gnu_cxx::__aligned_buffer<T>;

template<typename T>
constexpr bool
check_alignof_membuf()
{
  return alignof(membuf<T>) == alignof(T)
    && __alignof__(membuf<T>) == alignof(T);
}

template<typename T>
constexpr bool
check_alignof_objbuf()
{
#if _GLIBCXX_INLINE_VERSION
  // For the gnu-versioned-namespace ABI __aligned_buffer == __aligned_membuf.
  return check_alignof_membuf<T>();
#else
  return alignof(objbuf<T>) == __alignof__(T)
    && __alignof__(objbuf<T>) == __alignof__(T);
#endif
}

struct S { long long l; };
struct alignas(128) X { char x; };
static_assert( check_alignof_membuf<int>(), "membuf<int>" );
static_assert( check_alignof_membuf<long long>(), "membuf<long long>" );
static_assert( check_alignof_membuf<void*>(), "membuf<void*>" );
static_assert( check_alignof_membuf<S>(), "membuf<S>" );
static_assert( check_alignof_membuf<X>(), "membuf<X>" );
static_assert( check_alignof_objbuf<int>(), "objbuf<int>" );
static_assert( check_alignof_objbuf<long long>(), "objbuf<long long>" );
static_assert( check_alignof_objbuf<void*>(), "objbuf<void*>" );
static_assert( check_alignof_objbuf<S>(), "objbuf<S>" );
static_assert( check_alignof_objbuf<X>(), "objbuf<X>" );
