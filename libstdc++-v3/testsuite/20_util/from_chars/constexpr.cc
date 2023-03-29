// { dg-options "-std=gnu++23" }
// { dg-do compile { target c++23 } }

#include <charconv>
#include <testsuite_hooks.h>

constexpr bool
test()
{
  const char str[] = "-01234afz###";
  const char* end = str + sizeof(str);

  std::from_chars_result res;
  int ival = 99;
  unsigned uval = 99;

  res = std::from_chars(str, str+1, ival, 10);
  VERIFY( res.ptr == str );
  VERIFY( res.ec == std::errc::invalid_argument );
  VERIFY( ival == 99 );
  res = std::from_chars(str, str+4, ival, 10);
  VERIFY( res.ptr == str+4 );
  VERIFY( res.ec == std::errc{} );
  VERIFY( ival == -12 );
  res = std::from_chars(str, end, ival, 10);
  VERIFY( res.ptr == str+6 );
  VERIFY( res.ec == std::errc{} );
  VERIFY( ival == -1234 );

  res = std::from_chars(str, end, uval, 10);
  VERIFY( res.ptr == str );
  VERIFY( res.ec == std::errc::invalid_argument );
  VERIFY( uval == 99 );
  res = std::from_chars(str+1, end, uval, 10);
  VERIFY( res.ptr == str+6 );
  VERIFY( res.ec == std::errc{} );
  VERIFY( uval == 1234 );

  res = std::from_chars(str, end, ival, 3);
  VERIFY( res.ptr == str+4 );
  VERIFY( res.ec == std::errc{} );
  VERIFY( ival == -5 );

  res = std::from_chars(str, end, ival, 16);
  VERIFY( res.ptr == str+8 );
  VERIFY( res.ec == std::errc{} );
  VERIFY( ival == -1193135 );

  res = std::from_chars(str+1, end, uval, 36);
  VERIFY( res.ptr == str+1+8 );
  VERIFY( res.ec == std::errc{} );
  VERIFY( uval == 2302953695 );

  return true;
}

static_assert( test() );
