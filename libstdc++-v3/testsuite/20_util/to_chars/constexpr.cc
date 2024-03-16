// { dg-do compile { target c++23 } }
// { dg-add-options no_pch }

#include <charconv>

#ifndef __cpp_lib_constexpr_charconv
# error "Feature-test macro for constexpr charconv missing in <charconv>"
#elif __cpp_lib_constexpr_charconv != 202207L
# error "Feature-test macro for constexpr charconv has wrong value in <charconv>"
#endif

#include <testsuite_hooks.h>

constexpr bool
test_base10()
{
  std::to_chars_result res;
  char buf[10] = "XXXXXXXXX";
  res = std::to_chars(buf, buf+3, 1234);
  VERIFY( res.ptr == buf+3 );
  VERIFY( res.ec == std::errc::value_too_large );
  res = std::to_chars(buf, buf+4, -1234);
  VERIFY( res.ptr == buf+4 );
  VERIFY( res.ec == std::errc::value_too_large );
  res = std::to_chars(buf, buf+4, 1234);
  VERIFY( res.ptr == buf+4 );
  VERIFY( res.ec == std::errc{} );
  VERIFY( buf[0] == '1' );
  VERIFY( buf[1] == '2' );
  VERIFY( buf[2] == '3' );
  VERIFY( buf[3] == '4' );
  VERIFY( buf[4] == 'X' );
  res = std::to_chars(buf, buf+10, -567, 10);
  VERIFY( res.ptr == buf+4 );
  VERIFY( res.ec == std::errc{} );
  VERIFY( buf[0] == '-' );
  VERIFY( buf[1] == '5' );
  VERIFY( buf[2] == '6' );
  VERIFY( buf[3] == '7' );
  VERIFY( buf[4] == 'X' );
  return true;
}

static_assert( test_base10() );

constexpr bool
test_base16()
{
  std::to_chars_result res;
  char buf[10] = "XXXXXXXXX";
  res = std::to_chars(buf, buf+3, 0x1234, 16);
  VERIFY( res.ptr == buf+3 );
  VERIFY( res.ec == std::errc::value_too_large );
  res = std::to_chars(buf, buf+4, -0x1234, 16);
  VERIFY( res.ptr == buf+4 );
  VERIFY( res.ec == std::errc::value_too_large );
  res = std::to_chars(buf, buf+4, 0x1234, 16);
  VERIFY( res.ptr == buf+4 );
  VERIFY( res.ec == std::errc{} );
  VERIFY( buf[0] == '1' );
  VERIFY( buf[1] == '2' );
  VERIFY( buf[2] == '3' );
  VERIFY( buf[3] == '4' );
  VERIFY( buf[4] == 'X' );
  res = std::to_chars(buf, buf+10, -0x567, 16);
  VERIFY( res.ptr == buf+4 );
  VERIFY( res.ec == std::errc{} );
  VERIFY( buf[0] == '-' );
  VERIFY( buf[1] == '5' );
  VERIFY( buf[2] == '6' );
  VERIFY( buf[3] == '7' );
  VERIFY( buf[5] == 'X' );
  return true;
}

static_assert( test_base16() );

constexpr bool
test_base8()
{
  std::to_chars_result res;
  char buf[10] = "XXXXXXXXX";
  res = std::to_chars(buf, buf+2, 01234, 8);
  VERIFY( res.ptr == buf+2 );
  VERIFY( res.ec == std::errc::value_too_large );
  res = std::to_chars(buf, buf+3, -01234, 8);
  VERIFY( res.ptr == buf+3 );
  VERIFY( res.ec == std::errc::value_too_large );
  res = std::to_chars(buf, buf+4, 01234, 8);
  VERIFY( res.ptr == buf+4 );
  VERIFY( res.ec == std::errc{} );
  VERIFY( buf[0] == '1' );
  VERIFY( buf[1] == '2' );
  VERIFY( buf[2] == '3' );
  VERIFY( buf[3] == '4' );
  VERIFY( buf[4] == 'X' );
  res = std::to_chars(buf, buf+10, -0567, 8);
  VERIFY( res.ptr == buf+4 );
  VERIFY( res.ec == std::errc{} );
  VERIFY( buf[0] == '-' );
  VERIFY( buf[1] == '5' );
  VERIFY( buf[2] == '6' );
  VERIFY( buf[3] == '7' );
  VERIFY( buf[4] == 'X' );
  return true;
}

static_assert( test_base8() );

constexpr bool
test_base2()
{
  std::to_chars_result res;
  char buf[10] = "XXXXXXXXX";
  res = std::to_chars(buf, buf+4, 0b10001, 2);
  VERIFY( res.ptr == buf+4 );
  VERIFY( res.ec == std::errc::value_too_large );
  res = std::to_chars(buf, buf+5, -0b10001, 2);
  VERIFY( res.ptr == buf+5 );
  VERIFY( res.ec == std::errc::value_too_large );
  res = std::to_chars(buf, buf+5, 0b10001, 2);
  VERIFY( res.ptr == buf+5 );
  VERIFY( res.ec == std::errc{} );
  VERIFY( buf[0] == '1' );
  VERIFY( buf[1] == '0' );
  VERIFY( buf[2] == '0' );
  VERIFY( buf[3] == '0' );
  VERIFY( buf[4] == '1' );
  VERIFY( buf[5] == 'X' );
  res = std::to_chars(buf, buf+10, -0b11011, 2);
  VERIFY( res.ptr == buf+6 );
  VERIFY( res.ec == std::errc{} );
  VERIFY( buf[0] == '-' );
  VERIFY( buf[1] == '1' );
  VERIFY( buf[2] == '1' );
  VERIFY( buf[3] == '0' );
  VERIFY( buf[4] == '1' );
  VERIFY( buf[5] == '1' );
  VERIFY( buf[6] == 'X' );
  return true;
}

static_assert( test_base2() );

constexpr bool
test_base36()
{
  std::to_chars_result res;
  char buf[10] = "XXXXXXXXX";
  res = std::to_chars(buf, buf+1, 1234, 36);
  VERIFY( res.ptr == buf+1 );
  VERIFY( res.ec == std::errc::value_too_large );
  res = std::to_chars(buf, buf+2, -1234, 36);
  VERIFY( res.ptr == buf+2 );
  VERIFY( res.ec == std::errc::value_too_large );
  res = std::to_chars(buf, buf+3, 1234, 36);
  VERIFY( res.ptr == buf+2 );
  VERIFY( res.ec == std::errc{} );
  VERIFY( buf[0] == 'y' );
  VERIFY( buf[1] == 'a' );
  VERIFY( buf[3] == 'X' );
  res = std::to_chars(buf, buf+10, -567, 36);
  VERIFY( res.ptr == buf+3 );
  VERIFY( res.ec == std::errc{} );
  VERIFY( buf[0] == '-' );
  VERIFY( buf[1] == 'f' );
  VERIFY( buf[2] == 'r' );
  VERIFY( buf[4] == 'X' );
  return true;
}

static_assert( test_base36() );
