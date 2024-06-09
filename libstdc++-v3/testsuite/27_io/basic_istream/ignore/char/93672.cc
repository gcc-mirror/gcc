// { dg-do run }

#include <sstream>
#include <limits>
#include <testsuite_hooks.h>

void
test_pr93672() // std::basic_istream::ignore hangs if delim MSB is set
{
  std::istringstream in(".\xfc..\xfd...\xfe.");

  // This should find '\xfd' even on platforms where char is signed,
  // because the delimiter is correctly converted to the stream's int_type.
  in.ignore(100, std::char_traits<char>::to_int_type('\xfc'));
  VERIFY( in.gcount() == 2 );
  VERIFY( ! in.eof() );

  // This should work equivalently to traits_type::to_int_type
  in.ignore(100, (unsigned char)'\xfd');
  VERIFY( in.gcount() == 3 );
  VERIFY( ! in.eof() );

  // This only works if char is unsigned.
  in.ignore(100, '\xfe');
  if (std::numeric_limits<char>::is_signed)
  {
    // When char is signed, '\xfe' != traits_type::to_int_type('\xfe')
    // so the delimiter does not match the character in the input sequence,
    // and ignore consumes all input until EOF.
    VERIFY( in.gcount() == 5 );
    VERIFY( in.eof() );
  }
  else
  {
    // When char is unsigned, '\xfe' == to_int_type('\xfe') so the delimiter
    // matches the character in the input sequence, and doesn't reach EOF.
    VERIFY( in.gcount() == 4 );
    VERIFY( ! in.eof() );
  }

  in.clear();
  in.str(".a.");
  in.ignore(100, 'a' + 256); // Should not match 'a'
  VERIFY( in.gcount() == 3 );
  VERIFY( in.eof() );
}

// Custom traits type that inherits all behaviour from std::char_traits<char>.
struct traits : std::char_traits<char> { };

void
test_primary_template()
{
  // Check that the primary template for std::basic_istream::ignore
  // works the same as the std::istream::ignore specialization.
  // The infinite loop bug was never present in the primary template,
  // because it doesn't use traits_type::find to search the input sequence.

  std::basic_istringstream<char, traits> in(".\xfc..\xfd...\xfe.");

  // This should find '\xfd' even on platforms where char is signed,
  // because the delimiter is correctly converted to the stream's int_type.
  in.ignore(100, std::char_traits<char>::to_int_type('\xfc'));
  VERIFY( in.gcount() == 2 );
  VERIFY( ! in.eof() );

  // This should work equivalently to traits_type::to_int_type
  in.ignore(100, (unsigned char)'\xfd');
  VERIFY( in.gcount() == 3 );
  VERIFY( ! in.eof() );

  // This only works if char is unsigned.
  in.ignore(100, '\xfe');
  if (std::numeric_limits<char>::is_signed)
  {
    // When char is signed, '\xfe' != traits_type::to_int_type('\xfe')
    // so the delimiter does not match the character in the input sequence,
    // and ignore consumes all input until EOF.
    VERIFY( in.gcount() == 5 );
    VERIFY( in.eof() );
  }
  else
  {
    // When char is unsigned, '\xfe' == to_int_type('\xfe') so the delimiter
    // matches the character in the input sequence, and doesn't reach EOF.
    VERIFY( in.gcount() == 4 );
    VERIFY( ! in.eof() );
  }

  in.clear();
  in.str(".a.");
  in.ignore(100, 'a' + 256); // Should not match 'a'
  VERIFY( in.gcount() == 3 );
  VERIFY( in.eof() );
}

int main()
{
  test_pr93672();
  test_primary_template();
}
