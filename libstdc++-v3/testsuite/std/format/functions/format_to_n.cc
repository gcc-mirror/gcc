// { dg-do run { target c++20 } }

#include <format>
#include <vector>
#include <testsuite_hooks.h>

struct punct : std::numpunct<char>
{
  std::string do_grouping() const override { return "\2"; }
  std::string do_truename() const override { return "troo"; }
  std::string do_falsename() const override { return "falz"; }
};

void
test()
{
  char buf[4] = { };
  auto [out, len] = std::format_to_n(buf, 3, "123 + 456 = {}", 579);
  VERIFY( out == buf+3 );
  VERIFY( len == 15 );

  std::locale loc({}, new punct);
  auto [out2, len2] = std::format_to_n(buf, 4, loc, "{:Ld}", 12345);
  VERIFY( out2 == buf+4 );
  VERIFY( len2 == 7 );
  VERIFY( std::string_view(buf, 4) == "1,23" );
}

struct wpunct : std::numpunct<wchar_t>
{
  std::string do_grouping() const override { return "\2"; }
  std::wstring do_truename() const override { return L"troo"; }
  std::wstring do_falsename() const override { return L"falz"; }
};

void
test_wchar()
{
  wchar_t buf[4] = { };
  auto [out, len] = std::format_to_n(buf, 3, L"123 + 456 = {}", 579);
  VERIFY( out == buf+3 );
  VERIFY( len == 15 );

  std::locale loc({}, new wpunct);
  auto [out2, len2] = std::format_to_n(buf, 4, loc, L"{:Ld}", 12345);
  VERIFY( out2 == buf+4 );
  VERIFY( len2 == 7 );
  VERIFY( std::wstring_view(buf, 4) == L"1,23" );
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
  auto [res, len] = std::format_to_n(std::move(mo), 4, "for{:.3} that{:c}",
				     "matte", (int)'!');
  VERIFY( str == "form" );
  VERIFY( len == 12 );

  std::vector<wchar_t> vec;
  move_only_iterator wmo(std::back_inserter(vec));
  auto [wres, wlen] = std::format_to_n(std::move(wmo), 9, L"for{:.3} hat{:c}",
				       L"matte", (long)L'!');
  VERIFY( std::wstring_view(vec.data(), vec.size()) == L"format ha" );
  VERIFY( wlen == 11 );
}

void
test_pr110990()
{
  // PR libstdc++/110990 - format_to_n returns wrong value

  char buf[2];
  auto [ptr, len] = std::format_to_n(buf, 2, "x");
  VERIFY( len == 1 );
  VERIFY( ptr == buf + len );

  wchar_t wbuf[3];
  auto [wptr, wlen] = std::format_to_n(wbuf, 3, L"yz");
  VERIFY( wlen == 2 );
  VERIFY( wptr == wbuf + wlen );
}

int main()
{
  test();
  test_wchar();
  test_move_only();
  test_pr110990();
}
