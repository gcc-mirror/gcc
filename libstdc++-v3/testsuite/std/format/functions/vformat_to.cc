// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }

#include <format>
#include <string>
#include <iterator>
#include <testsuite_hooks.h>

template<typename C>
struct move_only_iterator
{
  using iterator = std::back_insert_iterator<std::basic_string<C>>;
  using value_type = iterator::value_type;
  using difference_type = iterator::difference_type;
  using iterator_category = std::output_iterator_tag;

  move_only_iterator(iterator b) : base_(b) { }
  move_only_iterator(move_only_iterator&&) = default;
  move_only_iterator& operator=(move_only_iterator&&) = default;

  move_only_iterator& operator++() { ++base_; return *this; }
  move_only_iterator operator++(int) { auto tmp = *this; ++base_; return tmp; }

  decltype(auto) operator*() { return *base_; }

private:
  iterator base_;
};

void
test_move_only()
{
  std::string str;
  move_only_iterator<char> mo(std::back_inserter(str));
  auto res = std::vformat_to(std::move(mo), "for{:.3} that{:c}",
			     std::make_format_args("matte", (int)'!'));
  static_assert(std::is_same_v<decltype(res), decltype(mo)>);
  VERIFY( str == "format that!" );

  std::wstring wstr;
  move_only_iterator<wchar_t> wmo(std::back_inserter(wstr));
  auto wres = std::vformat_to(std::move(wmo), L"for{:.3} that{:c}",
			      std::make_wformat_args(L"matte", (long)L'!'));
  static_assert(std::is_same_v<decltype(wres), decltype(wmo)>);
  VERIFY( wstr == L"format that!" );
}

int main()
{
  test_move_only();
}
