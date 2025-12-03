// { dg-do run { target c++26 } }
// { dg-require-atomic-cmpxchg-word "" }
// { dg-add-options libatomic }

#include <atomic>
#include <testsuite_hooks.h>

struct X
{
  X() = default;
  X(int i) : i(i) { }
  int i;

  friend bool
  operator==(X, X) = default;
};

template<typename T>
void testTemporary()
{
  static_assert( !std::is_constructible_v<std::atomic_ref<T>, T> );
  static_assert( !std::is_constructible_v<std::atomic_ref<const T>, T> );
  static_assert( !std::is_constructible_v<std::atomic_ref<T>, const T> );
  static_assert( !std::is_constructible_v<std::atomic_ref<const T>, const T> );

  if constexpr (std::atomic_ref<T>::is_always_lock_free)
  {
    static_assert( !std::is_constructible_v<std::atomic_ref<volatile T>, T> );
    static_assert( !std::is_constructible_v<std::atomic_ref<volatile T>, volatile T> );
    static_assert( !std::is_constructible_v<std::atomic_ref<const volatile T>, T> );
    static_assert( !std::is_constructible_v<std::atomic_ref<const volatile T>, const volatile T> );
  }

  struct X { X(T) {} };
  struct Overload 
  {
    static int operator()(X) { return 1; }
    static int operator()(std::atomic_ref<T>) { return 2; }
  };
  VERIFY( Overload{}(T()) == 1 );
  static_assert( !requires { Overload{}({T()}); } );
}

template<typename T>
bool same_address(const std::atomic_ref<T>& t, const std::type_identity_t<T>& u)
{
#if (__cplusplus >  202302L)
  return t.address() == &u;
#endif
  return true;
}

template<typename From, typename To = From>
void
testConv()
{
  alignas(std::atomic_ref<From>::required_alignment) From val{};
  std::atomic_ref<From> src(val);
  std::atomic_ref<const From> csrc(val);

  std::atomic_ref<const To> d1(src);
  VERIFY( same_address(d1, val) );
  std::atomic_ref<const To> d2(csrc);
  VERIFY( same_address(d2, val) );
  static_assert( !std::is_convertible_v<std::atomic_ref<const From>,
					std::atomic_ref<To>> );
 
  if constexpr (std::atomic_ref<From>::is_always_lock_free)
  {
    std::atomic_ref<const volatile To> d4(src);
    VERIFY( same_address(d4, val) );
    std::atomic_ref<const volatile To> d5(csrc);
    VERIFY( same_address(d5, val) );
    if constexpr (std::is_same_v<From, To>)
    {	    
      std::atomic_ref<volatile To> d3(src);
      VERIFY( same_address(d3, val) );
    }

    static_assert( !std::is_convertible_v<std::atomic_ref<volatile From>,
					  std::atomic_ref<To>> );
    static_assert( !std::is_convertible_v<std::atomic_ref<volatile From>,
					  std::atomic_ref<const To>> );
    static_assert( !std::is_convertible_v<std::atomic_ref<const volatile From>,
					  std::atomic_ref<To>> );
    static_assert( !std::is_convertible_v<std::atomic_ref<const volatile From>,
					  std::atomic_ref<const To>> );
  }
}

template<typename From, typename To>
void
testSimilarConv()
{
  testConv<From, To>();
  static_assert( !std::is_convertible_v<      To,       From> );
  static_assert( !std::is_convertible_v<      To, const From> );
  static_assert( !std::is_convertible_v<const To,       From> );
  static_assert( !std::is_convertible_v<const To, const From> );

  if constexpr (std::atomic_ref<From>::is_always_lock_free)
  {
    static_assert( !std::is_convertible_v<volatile To,          From> );
    static_assert( !std::is_convertible_v<         To, volatile From> );
    static_assert( !std::is_convertible_v<volatile To, volatile From> );

    static_assert( !std::is_convertible_v<const volatile To,                From> );
    static_assert( !std::is_convertible_v<               To, const volatile From> );
    static_assert( !std::is_convertible_v<const volatile To, const volatile From> );

    static_assert( !std::is_convertible_v<      To, volatile From> );
    static_assert( !std::is_convertible_v<const To, volatile From> );
    static_assert( !std::is_convertible_v<      To, const volatile From> );
    static_assert( !std::is_convertible_v<const To, const volatile From> );

    static_assert( !std::is_convertible_v<volatile To,       From> );
    static_assert( !std::is_convertible_v<volatile To, const From> );
    static_assert( !std::is_convertible_v<const volatile To,       From> );
    static_assert( !std::is_convertible_v<const volatile To, const From> );
  }
}

template<typename T, template<typename> typename MakePtr = std::add_pointer_t>
void
testPtrConv()
{
  testConv<MakePtr<T>>();
  testSimilarConv<MakePtr<T>, MakePtr<const T>>(); 
  testSimilarConv<MakePtr<T*>, MakePtr<const T* const>>(); 
  testSimilarConv<MakePtr<const T*>, MakePtr<const T* const>>(); 
  testSimilarConv<MakePtr<T* const>, MakePtr<const T* const>>();

  testSimilarConv<MakePtr<T[2]>, MakePtr<const T[2]>>(); 
  testSimilarConv<MakePtr<T[]>, MakePtr<const T[]>>(); 

  testSimilarConv<MakePtr<T[2]>, MakePtr<T[]>>(); 
  testSimilarConv<MakePtr<T[2]>, MakePtr<const T[]>>(); 
  testSimilarConv<MakePtr<const T[2]>, MakePtr<const T[]>>(); 

  if constexpr (std::atomic_ref<MakePtr<T>>::is_always_lock_free)
  {
    testSimilarConv<MakePtr<T>, MakePtr<volatile T>>(); 
    testSimilarConv<MakePtr<T>, MakePtr<const volatile T>>(); 
    testSimilarConv<MakePtr<volatile T>, MakePtr<const volatile T>>(); 
    testSimilarConv<MakePtr<T*>, MakePtr<volatile T* const>>(); 
    testSimilarConv<MakePtr<volatile T*>, MakePtr<volatile T* const>>();
    testSimilarConv<MakePtr<T*>, MakePtr<const volatile T* const>>(); 
    testSimilarConv<MakePtr<volatile T*>, MakePtr<const volatile T* const>>(); 
    testSimilarConv<MakePtr<volatile T* const>, MakePtr<const volatile T* const>>(); 

    testSimilarConv<MakePtr<T[2]>, MakePtr<volatile T[2]>>(); 
    testSimilarConv<MakePtr<T[2]>, MakePtr<const volatile T[2]>>(); 
    testSimilarConv<MakePtr<const T[2]>, MakePtr<const volatile T[2]>>(); 
    testSimilarConv<MakePtr<volatile T[2]>, MakePtr<const volatile T[2]>>(); 

    testSimilarConv<MakePtr<T[]>, MakePtr<volatile T[]>>(); 
    testSimilarConv<MakePtr<T[]>, MakePtr<const volatile T[]>>(); 
    testSimilarConv<MakePtr<const T[]>, MakePtr<const volatile T[]>>(); 
    testSimilarConv<MakePtr<volatile T[]>, MakePtr<const volatile T[]>>(); 

    testSimilarConv<MakePtr<T[2]>, MakePtr<volatile T[]>>(); 
    testSimilarConv<MakePtr<volatile T[2]>, MakePtr<volatile T[]>>(); 
    testSimilarConv<MakePtr<const T[2]>, MakePtr<const volatile T[]>>(); 
    testSimilarConv<MakePtr<volatile T[2]>, MakePtr<const volatile T[]>>(); 
    testSimilarConv<MakePtr<const volatile T[2]>, MakePtr<const volatile T[]>>(); 
  }
}

struct D : X {};
static_assert( !std::is_convertible_v<std::atomic_ref<D>, std::atomic_ref<X>> );
static_assert( !std::is_convertible_v<std::atomic_ref<D>, std::atomic_ref<const X>> );
static_assert( !std::is_convertible_v<std::atomic_ref<D*>, std::atomic_ref<const X* const>> );
static_assert( !std::is_convertible_v<std::atomic_ref<const D*>, std::atomic_ref<const X* const>> );

template<typename T>
using member_pointer_t = T X::*;

int
main()
{
  testTemporary<bool>();
  testTemporary<int>();
  testTemporary<float>();
  testTemporary<int*>();
  testTemporary<X>();

  testConv<bool>();
  testConv<int>();
  testConv<float>();
  testConv<X>();
  testPtrConv<int>();
  testPtrConv<int, member_pointer_t>();
}
