// { dg-do compile { target c++20 } }

#include <atomic>

template<typename T>
concept is_supported
  = !std::is_volatile_v<T>
  || std::atomic_ref<std::remove_cv_t<T>>::is_always_lock_free;

template<class T> concept has_and = requires (T& a) { a &= false; };
template<class T> concept has_or = requires (T& a) { a |= false; };
template<class T> concept has_xor = requires (T& a) { a ^= false; };
template<class T> concept has_fetch_add = requires (T& a) { a.fetch_add(true); };
template<class T> concept has_fetch_sub = requires (T& a) { a.fetch_sub(true); };

static constexpr std::memory_order mo = std::memory_order_seq_cst;

#define HAS(op) (requires (std::atomic_ref<T> a, T t) { op; })

template<typename T>
void
no_stores()
{
  if constexpr (is_supported<T>)
  {
    static_assert( !HAS(a = t) );
    static_assert( !HAS(a.store(t)) );
    static_assert( !HAS(a.store(t, mo)) );
    static_assert( !HAS(a.exchange(t)) );
    static_assert( !HAS(a.exchange(t, mo)) );

    static_assert( !HAS(a.compare_exchange_weak(t, t)) );
    static_assert( !HAS(a.compare_exchange_weak(t, t, mo)) );
    static_assert( !HAS(a.compare_exchange_weak(t, t, mo, mo)) );

    static_assert( !HAS(a.compare_exchange_strong(t, t)) );
    static_assert( !HAS(a.compare_exchange_strong(t, t, mo)) );
    static_assert( !HAS(a.compare_exchange_strong(t, t, mo, mo)) );
  }
}

template<typename T>
void
no_additions()
{
  if constexpr (is_supported<T>)
  {
    static_assert( !HAS(a++) );
    static_assert( !HAS(++a) );
    static_assert( !HAS(a += t) );
    static_assert( !HAS(a.fetch_add(t)) );
    static_assert( !HAS(a.fetch_add(t, mo)) );

    static_assert( !HAS(a--) );
    static_assert( !HAS(--a) );
    static_assert( !HAS(a -= t) );
    static_assert( !HAS(a.fetch_sub(t)) );
    static_assert( !HAS(a.fetch_sub(t, mo)) );
  }
}

template<typename T>
void
no_bitops()
{
  if constexpr (is_supported<T>)
  {
    static_assert( !HAS(a &= t) );
    static_assert( !HAS(a.fetch_and(t)) );
    static_assert( !HAS(a.fetch_and(t, mo)) );

    static_assert( !HAS(a |= t) );
    static_assert( !HAS(a.fetch_or(t)) );
    static_assert( !HAS(a.fetch_or(t, mo)) );

    static_assert( !HAS(a ^= t) );
    static_assert( !HAS(a.fetch_xor(t)) );
    static_assert( !HAS(a.fetch_xor(t, mo)) );
  }
}

template<typename T>
void
no_math()
{
  no_additions<T>();
  no_bitops<T>();
}

template<typename T>
void
no_mutations()
{
  no_stores<T>();
  no_math<T>();
}

struct S
{
  int x;
  int y;
};

int main()
{
  no_mutations<const int>();
  no_mutations<const volatile int>();

  no_bitops<float>();
  no_bitops<volatile float>();
  no_mutations<const float>();

  no_bitops<int*>();
  no_bitops<int* volatile>();
  no_mutations<int* const>();
  no_mutations<int* const volatile>();

  no_math<bool>();
  no_math<volatile bool>();
  no_mutations<const bool>();
  no_mutations<const volatile bool>();

  no_math<S>();
  no_math<volatile S>();
  no_mutations<const S>();
  no_mutations<const volatile S>();
}
