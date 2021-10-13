// { dg-options "-fchar8_t" }
// { dg-do run { target c++17 } }

#include <filesystem>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::input_iterator_wrapper;
using __gnu_test::input_container;

void test01()
{
  const char8_t src[] = u8"/long/path/to/a/file/to/avoid/small/string";
  input_container<const char8_t> c1(src);     // includes null terminator
  std::filesystem::path p1(c1.begin());       // read up to null terminator
  VERIFY( p1.u8string() == src );

  std::u8string_view sv = src;
  input_container<const char8_t> c2(sv.data(), sv.data() + sv.size());
  std::filesystem::path p2(c2.begin(), c2.end()); // PR libstdc++/102592
  VERIFY( p2.u8string() == src );
  VERIFY( p1 == p2 );
}

int main()
{
  test01();
}
