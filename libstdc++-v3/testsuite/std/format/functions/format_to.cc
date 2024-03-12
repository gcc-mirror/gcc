// { dg-do run { target c++20 } }

#include <format>
#include <locale>
#include <vector>
#include <cstring>
#include <testsuite_hooks.h>

struct punct : std::numpunct<char>
{
  std::string do_grouping() const override { return "\2"; }
};

void
test()
{
  char buf[32] = { };
  auto out = std::format_to(buf, "test");
  VERIFY( out == buf+4 );

  std::locale loc({}, new punct);
  auto out2 = std::format_to(buf, loc, "{:Ld}", 12345);
  VERIFY( out2 == buf+7 );
  VERIFY( std::string_view(buf, out2 - buf) == "1,23,45" );
}

struct wpunct : std::numpunct<wchar_t>
{
  std::string do_grouping() const override { return "\2"; }
};

void
test_wchar()
{
  wchar_t buf[32] = { };
  auto out = std::format_to(buf, L"123 + 456 = {}", 579);
  VERIFY( out == buf+15 );

  std::locale loc({}, new wpunct);
  auto out2 = std::format_to(buf, loc, L"{:Ld}", 12345);
  VERIFY( out2 == buf+7 );
  VERIFY( std::wstring_view(buf, out2 - buf) == L"1,23,45" );
}

template<typename I>
struct move_only_iterator
{
  using iterator = I;
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
  move_only_iterator mo(std::back_inserter(str));
  [[maybe_unused]] auto res
    = std::format_to(std::move(mo), "for{:.3} that{:c}", "matte", (int)'!');
  VERIFY( str == "format that!" );

  std::vector<wchar_t> vec;
  move_only_iterator wmo(std::back_inserter(vec));
  [[maybe_unused]] auto wres
    = std::format_to(std::move(wmo), L"for{:.3} hat{:c}", L"matte", (long)L'!');
  VERIFY( std::wstring_view(vec.data(), vec.size()) == L"format hat!" );
}

void
test_pr110917()
{
  // PR libstdc++/110917
  // std::format_to(int*, ...) fails to compile because of _S_make_span
  unsigned char buf[7];
  auto res = std::format_to(buf, "{} {}", "abc", 123);
  VERIFY( res == buf + 7 );
  VERIFY( ! std::memcmp(buf, "abc 123", 7) );
}

int main()
{
  test();
  test_wchar();
  test_move_only();
  test_pr110917();
}
