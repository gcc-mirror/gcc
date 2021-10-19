// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }

#include <optional>
#include <testsuite_hooks.h>


constexpr bool
test_assign()
{
  std::optional<int> oi(1);
  std::optional<unsigned> ou(2u), ou3(3u);

  // optional& operator=(nullopt_t);
  oi = std::nullopt;
  VERIFY( ! oi.has_value() );
  oi = std::nullopt;
  VERIFY( ! oi.has_value() );

  struct S {
    constexpr S() { }
    constexpr S(char, int, unsigned) { }
  };
  std::optional<S> os1, os2;

  // template<class U = T> optional& operator=(U&&);
  os1 = {'0', 1, 2u};
  VERIFY( os1.has_value() );
  os2 = {'3', 4, 5u};
  VERIFY( os2.has_value() );
  oi = 0u;
  VERIFY( *oi == 0 );
  oi = 1u;
  VERIFY( *oi == 1 );

  // template<class U> optional& operator=(const optional<U>&);
  oi = ou;
  VERIFY( *oi == 2 );
  oi = ou3;
  VERIFY( *oi == 3 );

  // template<class U> optional& operator=(optional<U>&&);
  oi = std::move(ou);
  VERIFY( *oi == 2 );
  oi = std::move(ou);
  VERIFY( *oi == 2 );
  oi = std::move(ou3);
  VERIFY( *oi == 3 );

  return true;
}

static_assert( test_assign() );

constexpr bool
test_emplace()
{
  struct S
  {
    constexpr S(int i) : val(i) { }
    constexpr S(int i, int j) : val(i + j) { }
    constexpr S(std::initializer_list<char> l, int i = 0) : val(i)
    {
      for (char c : l)
	val -= c;
    }

    int val;

    constexpr bool operator==(int i) const { return val == i; }
  };


  std::optional<S> os;

  // template<class... Args> constexpr T& emplace(Args&&...);
  os.emplace(1);
  VERIFY( *os == 1 );
  os.emplace(2);
  VERIFY( *os == 2 );
  os.emplace(2, 3);
  VERIFY( *os == 5 );

  // template<class U, class... Args>
  // constexpr T& emplace(initializer_list<U>, Args&&...);
  os.emplace({'3', '4', '5'});
  VERIFY( *os == -156 );
  os.emplace({'6', '7', '8'}, 25);
  VERIFY( *os == -140 );

  return true;
}

static_assert( test_emplace() );
