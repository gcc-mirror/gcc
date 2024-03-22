// { dg-options "-lstdc++exp" }
// { dg-do run { target c++23 } }
// { dg-require-fileio "" }

#include <print>
#include <system_error>
#include <climits>
#include <cstdio>
#include <cstring>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

#ifdef _WIN32
#include <io.h>
#endif

namespace std
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
  // This is an internal implementation detail that must not be used directly.
  // We need to use it here to test the behaviour
  error_code __write_to_terminal(void*, span<char>);
_GLIBCXX_END_NAMESPACE_VERSION
}

// Test the internal __write_to_terminal function that vprintf_unicode uses.
// The string parameter will be written to a file, then the bytes of the file
// will be read back again. On Windows those bytes will be a UTF-16 string.
// Returns true if the string was valid UTF-8.
bool
as_printed_to_terminal(std::string& s)
{
  __gnu_test::scoped_file f;
  FILE* strm = std::fopen(f.path.string().c_str(), "w");
  VERIFY( strm );
#ifdef _WIN32
  void* handle = (void*)_get_osfhandle(_fileno(strm));
  const auto ec = std::__write_to_terminal(handle, s);
#else
  const auto ec = std::__write_to_terminal(strm, s);
#endif
  if (ec && ec != std::make_error_code(std::errc::illegal_byte_sequence))
    {
      std::println("Failed to : {}", ec.message());
      VERIFY(!ec);
    }
  std::fclose(strm);
  std::ifstream in(f.path);
  s.assign(std::istreambuf_iterator<char>(in), {});
  return !ec;
}

void
test_utf8_validation()
{
#ifndef _WIN32
  std::string s = (const char*)u8"£🇬🇧 €🇪🇺";
  const std::string s2 = s;
  VERIFY( as_printed_to_terminal(s) );
  VERIFY( s == s2 );

  s += " \xa3 10.99 \xee \xdd";
  const std::string s3 = s;
  VERIFY( ! as_printed_to_terminal(s) );
  VERIFY( s != s3 );
  std::string repl = (const char*)u8"\uFFFD";
  const std::string s4 = s2 + " " + repl + " 10.99 " + repl + " " + repl;
  VERIFY( s == s4 );

  s = "\xc0\x80";
  VERIFY( ! as_printed_to_terminal(s) );
  VERIFY( s == repl + repl );
  s = "\xc0\xae";
  VERIFY( ! as_printed_to_terminal(s) );
  VERIFY( s == repl + repl );

  // Examples of U+FFFD substitution from Unicode standard.
  std::string r4 = repl + repl + repl + repl;
  s = "\xc0\xaf\xe0\x80\xbf\xf0\x81\x82\x41"; // Table 3-8
  VERIFY( ! as_printed_to_terminal(s) );
  VERIFY( s == r4 + r4 + "\x41" );
  s = "\xed\xa0\x80\xed\xbf\xbf\xed\xaf\x41"; // Table 3-9
  VERIFY( ! as_printed_to_terminal(s) );
  VERIFY( s == r4 + r4 + "\x41" );
  s = "\xf4\x91\x92\x93\xff\x41\x80\xbf\x42"; // Table 3-10
  VERIFY( ! as_printed_to_terminal(s) );
  VERIFY( s == r4 + repl + "\x41" + repl + repl + "\x42" );
  s = "\xe1\x80\xe2\xf0\x91\x92\xf1\xbf\x41"; // Table 3-11
  VERIFY( ! as_printed_to_terminal(s) );
  VERIFY( s == r4 + "\x41" );
#endif
}

// Create a std::u16string from the bytes in a std::string.
std::u16string
utf16_from_bytes(const std::string& s)
{
  std::u16string u16;
  // s should have an even number of bytes. If it doesn't, we'll copy its
  // null terminator into the result, which will not match the expected value.
  const auto len = (s.size() + 1) / 2;
  u16.resize_and_overwrite(len, [&s](char16_t* p, size_t n) {
    std::memcpy(p, s.data(), n * sizeof(char16_t));
    return n;
  });
  return u16;
}

void
test_utf16_transcoding()
{
#ifdef _WIN32
  // FIXME: We can't test __write_to_terminal for Windows, because it
  // returns an INVALID_HANDLE Windows error when writing to a normal file.

  std::string s = (const char*)u8"£🇬🇧 €🇪🇺";
  const std::u16string s2 = u"£🇬🇧 €🇪🇺";
  VERIFY( as_printed_to_terminal(s) );
  VERIFY( utf16_from_bytes(s) == s2 );

  s = (const char*)u8"£🇬🇧 €🇪🇺";
  s += " \xa3 10.99 \xee\xdd";
  VERIFY( ! as_printed_to_terminal(s) );
  std::u16string repl = u"\uFFFD";
  const std::u16string s3 = s2 + u" " + repl + u" 10.99 " + repl + repl;
  VERIFY( utf16_from_bytes(s) == s3 );

  s = "\xc0\x80";
  VERIFY( ! as_printed_to_terminal(s) );
  VERIFY( utf16_from_bytes(s) == repl + repl );
  s = "\xc0\xae";
  VERIFY( ! as_printed_to_terminal(s) );
  VERIFY( utf16_from_bytes(s) == repl + repl );

  // Examples of U+FFFD substitution from Unicode standard.
  std::u16string r4 = repl + repl + repl + repl;
  s = "\xc0\xaf\xe0\x80\xbf\xf0\x81\x82\x41"; // Table 3-8
  VERIFY( ! as_printed_to_terminal(s) );
  VERIFY( utf16_from_bytes(s) == r4 + r4 + u"\x41" );
  s = "\xed\xa0\x80\xed\xbf\xbf\xed\xaf\x41"; // Table 3-9
  VERIFY( ! as_printed_to_terminal(s) );
  VERIFY( utf16_from_bytes(s) == r4 + r4 + u"\x41" );
  s = "\xf4\x91\x92\x93\xff\x41\x80\xbf\x42"; // Table 3-10
  VERIFY( ! as_printed_to_terminal(s) );
  VERIFY( utf16_from_bytes(s) == r4 + repl + u"\x41" + repl + repl + u"\x42" );
  s = "\xe1\x80\xe2\xf0\x91\x92\xf1\xbf\x41"; // Table 3-11
  VERIFY( ! as_printed_to_terminal(s) );
  VERIFY( utf16_from_bytes(s) == r4 + u"\x41" );
#endif
}

int main()
{
  test_utf8_validation();
  test_utf16_transcoding();
}
