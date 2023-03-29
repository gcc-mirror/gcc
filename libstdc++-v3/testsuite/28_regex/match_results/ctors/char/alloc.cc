// { dg-do run { target c++11 } }

#include <regex>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

// LWG 2195. Missing constructors for match_results

void
test01()
{
  using Alloc = std::cmatch::allocator_type;
  std::cmatch m1;
  std::cmatch m2(m1, m1.get_allocator());
  VERIFY( m2 == m1 );

  static_assert( ! std::is_nothrow_constructible<std::cmatch,
						 const std::cmatch&,
						 const Alloc&>(),
		 "Allocator-extended copy ctor is potentially-throwing" );

  std::cmatch m3(std::move(m1), m2.get_allocator());
  VERIFY( m3 == m2 );

  // Libstdc++ extension:
  static_assert( std::is_nothrow_constructible<std::cmatch,
					       std::cmatch,
					       const Alloc&>(),
		 "Allocator-extended move ctor is non-throwing" );
}

void
test02()
{
  using Alloc = __gnu_test::uneq_allocator<std::csub_match>;
  using MR = std::match_results<const char*, Alloc>;

  MR m1(Alloc(1));
  MR m2(m1, Alloc(2));
  VERIFY( m2 == m1 );

  static_assert( ! std::is_nothrow_constructible<MR, const MR&, const Alloc&>(),
		 "Allocator-extended copy ctor is potentially-throwing" );

  MR m3(std::move(m1), Alloc(3));
  VERIFY( m3 == m2 );

  static_assert( ! std::is_nothrow_constructible<MR, MR, const Alloc&>(),
		 "Allocator-extended move ctor is potentially-throwing" );
}

int main()
{
  test01();
  test02();
}
