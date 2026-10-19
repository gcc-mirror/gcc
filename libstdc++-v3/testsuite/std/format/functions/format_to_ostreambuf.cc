// { dg-do run { target c++20 } }

#include <format>
#include <sstream>
#include <string>
#include <testsuite_hooks.h>

template<std::streamsize BufSize>
struct ShortWriteStreambuf : std::streambuf
{
  std::string data;

  std::streamsize
  xsputn(const char_type* s, std::streamsize n) override
  {
    std::streamsize limit = std::max<std::streamsize>(0, BufSize - data.size());
    std::streamsize to_write = std::min(n, limit);
    if (to_write > 0)
      data.append(s, to_write);
    return to_write;
  }

  int_type
  overflow(int_type c) override
  {
    if (data.size() < BufSize)
      {
	data.push_back(traits_type::to_char_type(c));
	return c;
      }
    return traits_type::eof();
  }
};

bool
check_suffix(std::string_view s, size_t n, char c)
{
  if (s.size() < n)
    return false;

  s.remove_prefix(s.size() - n);
  for (char v : s)
    if (v != c)
      return !n;
    else if (!n)
      return false;
    else
      --n;
  return !n;
}

void
test_format_to()
{
  {
    std::ostringstream os;
    std::format_to(std::ostreambuf_iterator<char>(os), "{}", 42);
    VERIFY( os.view() == "42" );
  }

  {
    std::ostringstream os;
    std::string big(800, 'y');
    std::format_to(std::ostreambuf_iterator<char>(os), "{}", big);
    VERIFY( check_suffix(os.view(), 800, 'y') );
  }

  {
    std::ostringstream os;
    std::string part(700, 'P');
    std::format_to(std::ostreambuf_iterator<char>(os),
		   "{}{}{}", part, part, part);
    VERIFY( check_suffix(os.view(), 2100, 'P') );
  }

  {
    std::ostringstream os;
    std::string body(400, 'B');
    std::format_to(std::ostreambuf_iterator<char>(os),
		   "header-{}-footer", body);

    std::string_view sv = os.view();
    VERIFY( sv.starts_with("header-") );
    sv.remove_prefix(7);
    VERIFY( sv.ends_with("-footer") );
    sv.remove_suffix(7);
    VERIFY( check_suffix(sv, 400, 'B') );
  }
}

void
test_format_to_n()
{
  {
    std::ostringstream os;
    auto res = std::format_to_n(std::ostreambuf_iterator<char>(os),
				1, "{}", "A");
    VERIFY( res.size == 1 );
    VERIFY( os.view() == "A" );
  }

  {
    std::ostringstream os;
    auto res = std::format_to_n(std::ostreambuf_iterator<char>(os),
				1, "{}", "AAA");
    VERIFY( res.size == 3 );
    VERIFY( os.view() == "A" );
  }

  {
    std::ostringstream os;
    auto res = std::format_to_n(std::ostreambuf_iterator<char>(os),
				3, "{}", "hello");
    VERIFY( res.size == 5 );
    VERIFY( os.view() == "hel" );
  }

  {
    std::ostringstream os;
    auto res = std::format_to_n(std::ostreambuf_iterator<char>(os),
				10, "{}", std::string(100, 'x'));
    VERIFY( res.size == 100 );
    VERIFY( check_suffix(os.view(), 10, 'x') );
  }

  {
    std::ostringstream os;
    auto res = std::format_to_n(std::ostreambuf_iterator<char>(os),
				10, "{}", std::string(1000, 'x'));
    VERIFY( res.size == 1000 );
    VERIFY( check_suffix(os.view(), 10, 'x') );
  }

  {
    std::ostringstream os;
    auto res = std::format_to_n(std::ostreambuf_iterator<char>(os),
				256, "{}", std::string(1000, 'x'));
    VERIFY( res.size == 1000 );
    VERIFY( check_suffix(os.view(), 256, 'x') );
  }

  {
    std::ostringstream os;
    auto res = std::format_to_n(std::ostreambuf_iterator<char>(os),
				257, "{}", std::string(1000, 'x'));
    VERIFY( res.size == 1000 );
    VERIFY( check_suffix(os.view(), 257, 'x') );
  }

  {
    std::ostringstream os;
    auto res = std::format_to_n(std::ostreambuf_iterator<char>(os),
				8, "{}{}{}", "AAAA", "BBBB", "CCCC");
    VERIFY( res.size == 12 );
    VERIFY( os.view() == "AAAABBBB" );
  }

  {
    std::ostringstream os;
    auto res = std::format_to_n(std::ostreambuf_iterator<char>(os),
				2, "{}{}", "AAAA", "BBBB");
    VERIFY( res.size == 8 );
    VERIFY( os.view() == "AA" );
  }

  {
    std::ostringstream os;
    auto res = std::format_to_n(std::ostreambuf_iterator<char>(os),
				8, "{}{}", "AAAA", "BBBB");
    VERIFY( res.size == 8 );
    VERIFY( os.view() == "AAAABBBB" );
  }

  {
    std::string big(257, 'A');
    std::ostringstream os;
    auto res = std::format_to_n(std::ostreambuf_iterator<char>(os),
                                257, "{}", big);
    VERIFY( res.size == 257 );
    std::string_view sv = os.view();
    VERIFY( os.view() == big );
  }

  {
    std::string big(300, 'A');
    std::ostringstream os;
    auto res = std::format_to_n(std::ostreambuf_iterator<char>(os),
                                257, "{}", big);
    VERIFY( res.size == 300 );
    VERIFY( check_suffix(os.view(), 257, 'A') );
  }
}

void
test_format_to_n_padding()
{
  {
    std::ostringstream os;
    auto res = std::format_to_n(std::ostreambuf_iterator<char>(os),
				3, "{:>10}", "hello");
    VERIFY( res.size == 10 );
    VERIFY( os.view().size() == 3 );
  }

  {
    std::ostringstream os;
    auto res = std::format_to_n(std::ostreambuf_iterator<char>(os),
				3, "{:.5}", std::string(100, 'z'));
    VERIFY( res.size == 5 );
    VERIFY( check_suffix(os.view(), 3, 'z') );
  }
}

void
test_format_to_n_prefilled()
{
  // Pre-filled ostringstream: after seekp(0) the streambuf has a put
  // area pointing into existing data.  This exercises the
  // _M_use_put_area code path combined with format_to_n truncation.
  {
    std::string init(100, 'Z');
    std::ostringstream os(init);
    os.seekp(0);
    auto res = std::format_to_n(std::ostreambuf_iterator<char>(os),
				5, "{}", "1234567890");
    VERIFY( res.size == 10 );
    VERIFY( os.view().substr(0, 10) == "12345ZZZZZ" );
    VERIFY( check_suffix(os.view(), 90, 'Z') );
  }
  {
    std::string init(100, 'Z');
    std::ostringstream os(init);
    os.seekp(0);
    auto res = std::format_to_n(std::ostreambuf_iterator<char>(os),
				8, "{:=>15}", "1234567890");
    VERIFY( res.size == 15 );
    VERIFY( os.view().substr(0, 10) == "=====123ZZ" );
    VERIFY( check_suffix(os.view(), 90, 'Z') );
  }
  {
    std::string init(100, 'Z');
    std::ostringstream os(init);
    os.seekp(0);
    auto res = std::format_to_n(std::ostreambuf_iterator<char>(os),
				3, "{}", 12345);
    VERIFY( res.size == 5 );
    VERIFY( os.view().substr(0, 10) == "123ZZZZZZZ" );
    VERIFY( check_suffix(os.view(), 90, 'Z') );
  }
  {
    std::string init(100, 'Z');
    std::ostringstream os(init);
    os.seekp(0);
    auto res = std::format_to_n(std::ostreambuf_iterator<char>(os),
				7, "{:=>10}", 12345);
    VERIFY( res.size == 10 );
    VERIFY( os.view().substr(0, 10) == "=====12ZZZ" );
    VERIFY( check_suffix(os.view(), 90, 'Z') );
  }
}

void
test_format_to_sentinel()
{
  // Test cases writting of default constructed (sentinel)
  // ostream_iterator. No characters can be written.
  const std::ostreambuf_iterator<char> sent;
  {
    auto res = std::format_to(sent, "");
    VERIFY( res.failed() );
  }
  {
    auto res = std::format_to(sent, "{}", "");
    VERIFY( res.failed() );
  }
  {
    auto res = std::format_to_n(sent, 0, "");
    VERIFY( res.out.failed() );
    VERIFY( res.size == 0 );
  }
  {
    auto res = std::format_to_n(sent, 0, "{}", "");
    VERIFY( res.out.failed() );
    VERIFY( res.size == 0 );
  }
  {
    auto res = std::format_to_n(sent, 0, "{}", "abc");
    VERIFY( res.out.failed() );
    VERIFY( res.size == 3 );
  }
  {
    auto res = std::format_to_n(sent, 0, "{}", "10");
    VERIFY( res.out.failed() );
    VERIFY( res.size == 2 );
  }
}

void
test_write_failure()
{
  const std::string payload(20, 'Z');

  {
    ShortWriteStreambuf<20> buf;
    auto it = std::ostreambuf_iterator<char>(&buf);
    auto res = std::format_to(it, "{}", payload);
    VERIFY( !res.failed() );
    VERIFY( check_suffix(buf.data, 20, 'Z') );
  }

  {
    ShortWriteStreambuf<19> buf;
    auto it = std::ostreambuf_iterator<char>(&buf);
    auto res = std::format_to(it, "{}", payload);
    VERIFY( res.failed() );
    VERIFY( check_suffix(buf.data, 19, 'Z') );
  }

  {
    ShortWriteStreambuf<10> buf;
    auto it = std::ostreambuf_iterator<char>(&buf);
    auto res = std::format_to_n(it, 10, "{}", payload);
    VERIFY( !res.out.failed() );
    VERIFY( res.size == 20 );
    VERIFY( check_suffix(buf.data, 10, 'Z') );
  }

  {
    ShortWriteStreambuf<10> buf;
    auto it = std::ostreambuf_iterator<char>(&buf);
    auto res = std::format_to_n(it, 11, "{}", payload);
    VERIFY( res.out.failed() );
    VERIFY( res.size == 20 );
    VERIFY( check_suffix(buf.data, 10, 'Z') );
  }

  {
    ShortWriteStreambuf<100> buf;
    auto it = std::ostreambuf_iterator<char>(&buf);
    auto res = std::format_to(it, "{}", std::string(300, 'Y'));
    VERIFY( res.failed() );
    VERIFY( buf.data.size() == 100 );
    VERIFY( check_suffix(buf.data, 100, 'Y') );
  }
  // format_to_n after write failure: size must still be the total
  // number of characters that would have been produced.
  {
    ShortWriteStreambuf<50> buf;
    auto it = std::ostreambuf_iterator<char>(&buf);
    auto res = std::format_to_n(it, 100,
				"{}{}{}",
				std::string(20, 'A'),
				std::string(30, 'B'),
				std::string(40, 'C'));
    // The first two arguments fill the buffer (20 + 30 == 50).
    // The third argument (40 C's) triggers write failure and the
    // remaining chars must still be counted in res.size.
    VERIFY( res.out.failed() );
    VERIFY( res.size == 90 );

    std::string_view sv = buf.data;
    VERIFY( buf.data.size() == 50 );
    VERIFY( check_suffix(sv, 30, 'B') );
    sv.remove_suffix(30);
    VERIFY( check_suffix(sv, 20, 'A') );
  }
}

int main()
{
  test_format_to();
  test_format_to_n();
  test_format_to_n_padding();
  test_format_to_n_prefilled();
  test_format_to_sentinel();
  test_write_failure();
}
