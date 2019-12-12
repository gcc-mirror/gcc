// Copyright (C) 2017-2019 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }
// { dg-require-string-conversions "" }

#include <charconv>
#include <string_view>

template<typename I>
bool
check_to_chars(I val, std::string_view expected, int base = 0)
{
  // Space for minus sign, 64 binary digits, final '*', and null terminator:
  char buf[67] = "******************************************************************";
  std::to_chars_result r = base == 0
    ? std::to_chars(buf, buf+sizeof(buf), val)
    : std::to_chars(buf, buf+sizeof(buf), val, base);
  return r.ec == std::errc{} && *r.ptr == '*' && std::string_view(buf, r.ptr - buf) == expected;
}

#include <string>
#include <climits>
#include <testsuite_hooks.h>

// Using default base 10
void
test01()
{
  VERIFY( check_to_chars<char>(0, "0") );
  VERIFY( check_to_chars<signed char>(0, "0") );
  VERIFY( check_to_chars<unsigned char>(0, "0") );
  VERIFY( check_to_chars<signed short>(0, "0") );
  VERIFY( check_to_chars<unsigned short>(0, "0") );
  VERIFY( check_to_chars<signed int>(0, "0") );
  VERIFY( check_to_chars<unsigned int>(0, "0") );
  VERIFY( check_to_chars<signed long>(0, "0") );
  VERIFY( check_to_chars<unsigned long>(0, "0") );
  VERIFY( check_to_chars<signed long long>(0, "0") );
  VERIFY( check_to_chars<unsigned long long>(0, "0") );

  VERIFY( check_to_chars<char>(1, "1") );
  VERIFY( check_to_chars<signed char>(1, "1") );
  VERIFY( check_to_chars<unsigned char>(1, "1") );
  VERIFY( check_to_chars<signed short>(1, "1") );
  VERIFY( check_to_chars<unsigned short>(1, "1") );
  VERIFY( check_to_chars<signed int>(1, "1") );
  VERIFY( check_to_chars<unsigned int>(1, "1") );
  VERIFY( check_to_chars<signed long>(1, "1") );
  VERIFY( check_to_chars<unsigned long>(1, "1") );
  VERIFY( check_to_chars<signed long long>(1, "1") );
  VERIFY( check_to_chars<unsigned long long>(1, "1") );

  VERIFY( check_to_chars<char>(123, "123") );
  VERIFY( check_to_chars<signed char>(123, "123") );
  VERIFY( check_to_chars<unsigned char>(123, "123") );
  VERIFY( check_to_chars<signed short>(123, "123") );
  VERIFY( check_to_chars<unsigned short>(123, "123") );
  VERIFY( check_to_chars<signed int>(123, "123") );
  VERIFY( check_to_chars<unsigned int>(123, "123") );
  VERIFY( check_to_chars<signed long>(123, "123") );
  VERIFY( check_to_chars<unsigned long>(123, "123") );
  VERIFY( check_to_chars<signed long long>(123, "123") );
  VERIFY( check_to_chars<unsigned long long>(123, "123") );

  if constexpr (std::is_signed_v<char>)
    VERIFY( check_to_chars<char>(-79, "-79") );
  VERIFY( check_to_chars<signed char>(-79, "-79") );
  VERIFY( check_to_chars<signed short>(-79, "-79") );
  VERIFY( check_to_chars<signed int>(-79, "-79") );
  VERIFY( check_to_chars<signed long>(-79, "-79") );
  VERIFY( check_to_chars<signed long long>(-79, "-79") );

  using std::to_string;

  VERIFY( check_to_chars<char>(CHAR_MAX, to_string(CHAR_MAX)) );
  VERIFY( check_to_chars<signed char>(SCHAR_MAX, to_string(SCHAR_MAX)) );
  VERIFY( check_to_chars<unsigned char>(UCHAR_MAX, to_string(UCHAR_MAX)) );
  VERIFY( check_to_chars<signed short>(SHRT_MAX, to_string(SHRT_MAX)) );
  VERIFY( check_to_chars<unsigned short>(USHRT_MAX, to_string(USHRT_MAX)) );
  VERIFY( check_to_chars<signed int>(INT_MAX, to_string(INT_MAX)) );
  VERIFY( check_to_chars<unsigned int>(UINT_MAX, to_string(UINT_MAX)) );
  VERIFY( check_to_chars<signed long>(LONG_MAX, to_string(LONG_MAX)) );
  VERIFY( check_to_chars<unsigned long>(ULONG_MAX, to_string(ULONG_MAX)) );
  VERIFY( check_to_chars<signed long long>(LLONG_MAX, to_string(LLONG_MAX)) );
  VERIFY( check_to_chars<unsigned long long>(ULLONG_MAX, to_string(ULLONG_MAX)) );

  VERIFY( check_to_chars<char>(CHAR_MIN, to_string(CHAR_MIN)) );
  VERIFY( check_to_chars<signed char>(SCHAR_MIN, to_string(SCHAR_MIN)) );
  VERIFY( check_to_chars<signed short>(SHRT_MIN, to_string(SHRT_MIN)) );
  VERIFY( check_to_chars<signed int>(INT_MIN, to_string(INT_MIN)) );
  VERIFY( check_to_chars<signed long>(LONG_MIN, to_string(LONG_MIN)) );
  VERIFY( check_to_chars<signed long long>(LLONG_MIN, to_string(LLONG_MIN)) );

  VERIFY( check_to_chars<char>(CHAR_MAX/2, to_string(CHAR_MAX/2)) );
  VERIFY( check_to_chars<signed char>(SCHAR_MAX/2, to_string(SCHAR_MAX/2)) );
  VERIFY( check_to_chars<unsigned char>(UCHAR_MAX/2, to_string(UCHAR_MAX/2)) );
  VERIFY( check_to_chars<signed short>(SHRT_MAX/2, to_string(SHRT_MAX/2)) );
  VERIFY( check_to_chars<unsigned short>(USHRT_MAX/2, to_string(USHRT_MAX/2)) );
  VERIFY( check_to_chars<signed int>(INT_MAX/2, to_string(INT_MAX/2)) );
  VERIFY( check_to_chars<unsigned int>(UINT_MAX/2, to_string(UINT_MAX/2)) );
  VERIFY( check_to_chars<signed long>(LONG_MAX/2, to_string(LONG_MAX/2)) );
  VERIFY( check_to_chars<unsigned long>(ULONG_MAX/2, to_string(ULONG_MAX/2)) );
  VERIFY( check_to_chars<signed long long>(LLONG_MAX/2, to_string(LLONG_MAX/2)) );
  VERIFY( check_to_chars<unsigned long long>(ULLONG_MAX/2, to_string(ULLONG_MAX/2)) );
}

// Using explicit base 10
void
test02()
{
  VERIFY( check_to_chars<char>(0, "0", 10) );
  VERIFY( check_to_chars<signed char>(0, "0", 10) );
  VERIFY( check_to_chars<unsigned char>(0, "0", 10) );
  VERIFY( check_to_chars<signed short>(0, "0", 10) );
  VERIFY( check_to_chars<unsigned short>(0, "0", 10) );
  VERIFY( check_to_chars<signed int>(0, "0", 10) );
  VERIFY( check_to_chars<unsigned int>(0, "0", 10) );
  VERIFY( check_to_chars<signed long>(0, "0", 10) );
  VERIFY( check_to_chars<unsigned long>(0, "0", 10) );
  VERIFY( check_to_chars<signed long long>(0, "0", 10) );
  VERIFY( check_to_chars<unsigned long long>(0, "0", 10) );

  VERIFY( check_to_chars<char>(1, "1", 10) );
  VERIFY( check_to_chars<signed char>(1, "1", 10) );
  VERIFY( check_to_chars<unsigned char>(1, "1", 10) );
  VERIFY( check_to_chars<signed short>(1, "1", 10) );
  VERIFY( check_to_chars<unsigned short>(1, "1", 10) );
  VERIFY( check_to_chars<signed int>(1, "1", 10) );
  VERIFY( check_to_chars<unsigned int>(1, "1", 10) );
  VERIFY( check_to_chars<signed long>(1, "1", 10) );
  VERIFY( check_to_chars<unsigned long>(1, "1", 10) );
  VERIFY( check_to_chars<signed long long>(1, "1", 10) );
  VERIFY( check_to_chars<unsigned long long>(1, "1", 10) );

  VERIFY( check_to_chars<char>(123, "123", 10) );
  VERIFY( check_to_chars<signed char>(123, "123", 10) );
  VERIFY( check_to_chars<unsigned char>(123, "123", 10) );
  VERIFY( check_to_chars<signed short>(123, "123", 10) );
  VERIFY( check_to_chars<unsigned short>(123, "123", 10) );
  VERIFY( check_to_chars<signed int>(123, "123", 10) );
  VERIFY( check_to_chars<unsigned int>(123, "123", 10) );
  VERIFY( check_to_chars<signed long>(123, "123", 10) );
  VERIFY( check_to_chars<unsigned long>(123, "123", 10) );
  VERIFY( check_to_chars<signed long long>(123, "123", 10) );
  VERIFY( check_to_chars<unsigned long long>(123, "123", 10) );

  if constexpr (std::is_signed_v<char>)
    VERIFY( check_to_chars<char>(-79, "-79", 10) );
  VERIFY( check_to_chars<signed char>(-79, "-79", 10) );
  VERIFY( check_to_chars<signed short>(-79, "-79", 10) );
  VERIFY( check_to_chars<signed int>(-79, "-79", 10) );
  VERIFY( check_to_chars<signed long>(-79, "-79", 10) );
  VERIFY( check_to_chars<signed long long>(-79, "-79", 10) );

  using std::to_string;

  VERIFY( check_to_chars<char>(CHAR_MAX, to_string(CHAR_MAX), 10) );
  VERIFY( check_to_chars<signed char>(SCHAR_MAX, to_string(SCHAR_MAX), 10) );
  VERIFY( check_to_chars<unsigned char>(UCHAR_MAX, to_string(UCHAR_MAX), 10) );
  VERIFY( check_to_chars<signed short>(SHRT_MAX, to_string(SHRT_MAX), 10) );
  VERIFY( check_to_chars<unsigned short>(USHRT_MAX, to_string(USHRT_MAX), 10) );
  VERIFY( check_to_chars<signed int>(INT_MAX, to_string(INT_MAX), 10) );
  VERIFY( check_to_chars<unsigned int>(UINT_MAX, to_string(UINT_MAX), 10) );
  VERIFY( check_to_chars<signed long>(LONG_MAX, to_string(LONG_MAX), 10) );
  VERIFY( check_to_chars<unsigned long>(ULONG_MAX, to_string(ULONG_MAX), 10) );
  VERIFY( check_to_chars<signed long long>(LLONG_MAX, to_string(LLONG_MAX), 10) );
  VERIFY( check_to_chars<unsigned long long>(ULLONG_MAX, to_string(ULLONG_MAX), 10) );

  VERIFY( check_to_chars<char>(CHAR_MIN, to_string(CHAR_MIN), 10) );
  VERIFY( check_to_chars<signed char>(SCHAR_MIN, to_string(SCHAR_MIN), 10) );
  VERIFY( check_to_chars<signed short>(SHRT_MIN, to_string(SHRT_MIN), 10) );
  VERIFY( check_to_chars<signed int>(INT_MIN, to_string(INT_MIN), 10) );
  VERIFY( check_to_chars<signed long>(LONG_MIN, to_string(LONG_MIN), 10) );
  VERIFY( check_to_chars<signed long long>(LLONG_MIN, to_string(LLONG_MIN), 10) );

  VERIFY( check_to_chars<char>(CHAR_MAX/2, to_string(CHAR_MAX/2), 10) );
  VERIFY( check_to_chars<signed char>(SCHAR_MAX/2, to_string(SCHAR_MAX/2), 10) );
  VERIFY( check_to_chars<unsigned char>(UCHAR_MAX/2, to_string(UCHAR_MAX/2), 10) );
  VERIFY( check_to_chars<signed short>(SHRT_MAX/2, to_string(SHRT_MAX/2), 10) );
  VERIFY( check_to_chars<unsigned short>(USHRT_MAX/2, to_string(USHRT_MAX/2), 10) );
  VERIFY( check_to_chars<signed int>(INT_MAX/2, to_string(INT_MAX/2), 10) );
  VERIFY( check_to_chars<unsigned int>(UINT_MAX/2, to_string(UINT_MAX/2), 10) );
  VERIFY( check_to_chars<signed long>(LONG_MAX/2, to_string(LONG_MAX/2), 10) );
  VERIFY( check_to_chars<unsigned long>(ULONG_MAX/2, to_string(ULONG_MAX/2), 10) );
  VERIFY( check_to_chars<signed long long>(LLONG_MAX/2, to_string(LLONG_MAX/2), 10) );
  VERIFY( check_to_chars<unsigned long long>(ULLONG_MAX/2, to_string(ULLONG_MAX/2), 10) );
}

// Using all bases
void
test03()
{
  // -2017 in all bases from [2,36]
  const char* str2017[37] = { nullptr, nullptr,
    "-11111100001",
    "-2202201",
    "-133201",
    "-31032",
    "-13201",
    "-5611",
    "-3741",
    "-2681",
    "-2017",
    "-1574",
    "-1201",
    "-bc2",
    "-a41",
    "-8e7",
    "-7e1",
    "-6gb",
    "-641",
    "-5b3",
    "-50h",
    "-4c1",
    "-43f",
    "-3ig",
    "-3c1",
    "-35h",
    "-2pf",
    "-2kj",
    "-2g1",
    "-2bg",
    "-277",
    "-232",
    "-1v1",
    "-1s4",
    "-1pb",
    "-1mm",
    "-1k1"
  };
  // -12345 in all bases from [2,36]
  const char* str12345[37] = { nullptr, nullptr,
    "-11000000111001",
    "-121221020",
    "-3000321",
    "-343340",
    "-133053",
    "-50664",
    "-30071",
    "-17836",
    "-12345",
    "-9303",
    "-7189",
    "-5808",
    "-46db",
    "-39d0",
    "-3039",
    "-28c3",
    "-221f",
    "-1f3e",
    "-1ah5",
    "-16ki",
    "-13b3",
    "-107h",
    "-la9",
    "-jik",
    "-i6l",
    "-gp6",
    "-fkp",
    "-ejk",
    "-dlf",
    "-cq7",
    "-c1p",
    "-bb3",
    "-an3",
    "-a2p",
    "-9ix"
  };
  // -23456 in all bases from [2,36]
  const char* str23456[37] = { nullptr, nullptr,
    "-101101110100000",
    "-1012011202",
    "-11232200",
    "-1222311",
    "-300332",
    "-125246",
    "-55640",
    "-35152",
    "-23456",
    "-16694",
    "-116a8",
    "-a8a4",
    "-8796",
    "-6e3b",
    "-5ba0",
    "-4d2d",
    "-4072",
    "-37ia",
    "-2icg",
    "-2b3k",
    "-24a4",
    "-1l7j",
    "-1gh8",
    "-1cd6",
    "-18i4",
    "-154k",
    "-11pk",
    "-rpo",
    "-q1q",
    "-ock",
    "-mt0",
    "-lhq",
    "-k9u",
    "-j56",
    "-i3k"
  };
  // INT_MIN in all bases from [2,36]
  const char* strINT_MIN[37] = { nullptr, nullptr,
    "-10000000000000000000000000000000",
    "-12112122212110202102",
    "-2000000000000000",
    "-13344223434043",
    "-553032005532",
    "-104134211162",
    "-20000000000",
    "-5478773672",
    "-2147483648",
    "-a02220282",
    "-4bb2308a8",
    "-282ba4aab",
    "-1652ca932",
    "-c87e66b8",
    "-80000000",
    "-53g7f549",
    "-3928g3h2",
    "-27c57h33",
    "-1db1f928",
    "-140h2d92",
    "-ikf5bf2",
    "-ebelf96",
    "-b5gge58",
    "-8jmdnkn",
    "-6oj8ioo",
    "-5ehnckb",
    "-4clm98g",
    "-3hk7988",
    "-2sb6cs8",
    "-2d09uc2",
    "-2000000",
    "-1lsqtl2",
    "-1d8xqrq",
    "-15v22un",
    "-zik0zk"
  };

  for (int base = 2; base <= 36; ++base)
  {
    VERIFY( check_to_chars<char>(0, "0", base) );
    VERIFY( check_to_chars<signed char>(0, "0", base) );
    VERIFY( check_to_chars<unsigned char>(0, "0", base) );
    VERIFY( check_to_chars<signed short>(0, "0", base) );
    VERIFY( check_to_chars<unsigned short>(0, "0", base) );
    VERIFY( check_to_chars<signed int>(0, "0", base) );
    VERIFY( check_to_chars<unsigned int>(0, "0", base) );
    VERIFY( check_to_chars<signed long>(0, "0", base) );
    VERIFY( check_to_chars<unsigned long>(0, "0", base) );
    VERIFY( check_to_chars<signed long long>(0, "0", base) );
    VERIFY( check_to_chars<unsigned long long>(0, "0", base) );

    VERIFY( check_to_chars<char>(1, "1", base) );
    VERIFY( check_to_chars<signed char>(1, "1", base) );
    VERIFY( check_to_chars<unsigned char>(1, "1", base) );
    VERIFY( check_to_chars<signed short>(1, "1", base) );
    VERIFY( check_to_chars<unsigned short>(1, "1", base) );
    VERIFY( check_to_chars<signed int>(1, "1", base) );
    VERIFY( check_to_chars<unsigned int>(1, "1", base) );
    VERIFY( check_to_chars<signed long>(1, "1", base) );
    VERIFY( check_to_chars<unsigned long>(1, "1", base) );
    VERIFY( check_to_chars<signed long long>(1, "1", base) );
    VERIFY( check_to_chars<unsigned long long>(1, "1", base) );

    if constexpr (std::is_signed_v<char>)
      VERIFY( check_to_chars<char>(-1, "-1", base) );
    VERIFY( check_to_chars<signed char>(-1, "-1", base) );
    VERIFY( check_to_chars<signed short>(-1, "-1", base) );
    VERIFY( check_to_chars<signed int>(-1, "-1", base) );
    VERIFY( check_to_chars<signed long>(-1, "-1", base) );
    VERIFY( check_to_chars<signed long long>(-1, "-1", base) );

    if (base > 2)
    {
      VERIFY( check_to_chars<char>(2, "2", base) );
      VERIFY( check_to_chars<signed char>(2, "2", base) );
      VERIFY( check_to_chars<unsigned char>(2, "2", base) );
      VERIFY( check_to_chars<signed short>(2, "2", base) );
      VERIFY( check_to_chars<unsigned short>(2, "2", base) );
      VERIFY( check_to_chars<signed int>(2, "2", base) );
      VERIFY( check_to_chars<unsigned int>(2, "2", base) );
      VERIFY( check_to_chars<signed long>(2, "2", base) );
      VERIFY( check_to_chars<unsigned long>(2, "2", base) );
      VERIFY( check_to_chars<signed long long>(2, "2", base) );
      VERIFY( check_to_chars<unsigned long long>(2, "2", base) );

      if constexpr (std::is_signed_v<char>)
	VERIFY( check_to_chars<char>(-2, "-2", base) );
      VERIFY( check_to_chars<signed char>(-2, "-2", base) );
      VERIFY( check_to_chars<signed short>(-2, "-2", base) );
      VERIFY( check_to_chars<signed int>(-2, "-2", base) );
      VERIFY( check_to_chars<signed long>(-2, "-2", base) );
      VERIFY( check_to_chars<signed long long>(-2, "-2", base) );
    }

    VERIFY( check_to_chars(2017u, str2017[base]+1, base) );
    VERIFY( check_to_chars(2017, str2017[base]+1, base) );
    VERIFY( check_to_chars(-2017, str2017[base], base) );
    VERIFY( check_to_chars(12345u, str12345[base]+1, base) );
    VERIFY( check_to_chars(12345, str12345[base]+1, base) );
    VERIFY( check_to_chars(-12345, str12345[base], base) );
    VERIFY( check_to_chars(23456u, str23456[base]+1, base) );
    VERIFY( check_to_chars(23456, str23456[base]+1, base) );
    VERIFY( check_to_chars(-23456, str23456[base], base) );
    VERIFY( check_to_chars(INT_MAX + 1ull, strINT_MIN[base]+1, base) );
    VERIFY( check_to_chars(INT_MAX + 1ll, strINT_MIN[base]+1, base) );
    VERIFY( check_to_chars(INT_MIN, strINT_MIN[base], base) );
  }

  VERIFY( check_to_chars(1155, "xx", 34) );
  VERIFY( check_to_chars(1224, "yy", 35) );
  VERIFY( check_to_chars(1295, "zz", 36) );
}

#include <sstream>
#include <ios>

// base 8
void
test04()
{
  auto to_string = [](auto val) {
    std::ostringstream ss;
    ss << std::oct;
    if (val < 0)
      ss << '-' << (~val + 1ull);
    else if (sizeof(val) == 1)
      ss << (int)val;
    else
      ss << val;
    return ss.str();
  };

  VERIFY( check_to_chars<char>(123, to_string(123), 8) );
  VERIFY( check_to_chars<signed char>(123, to_string(123), 8) );
  VERIFY( check_to_chars<unsigned char>(123, to_string(123), 8) );
  VERIFY( check_to_chars<signed short>(123, to_string(123), 8) );
  VERIFY( check_to_chars<unsigned short>(123, to_string(123), 8) );
  VERIFY( check_to_chars<signed int>(123, to_string(123), 8) );
  VERIFY( check_to_chars<unsigned int>(123, to_string(123), 8) );
  VERIFY( check_to_chars<signed long>(123, to_string(123), 8) );
  VERIFY( check_to_chars<unsigned long>(123, to_string(123), 8) );
  VERIFY( check_to_chars<signed long long>(123, to_string(123), 8) );
  VERIFY( check_to_chars<unsigned long long>(123, to_string(123), 8) );

  if constexpr (std::is_signed_v<char>)
    VERIFY( check_to_chars<char>(-79, to_string(-79), 8) );
  VERIFY( check_to_chars<signed char>(-79, to_string(-79), 8) );
  VERIFY( check_to_chars<signed short>(-79, to_string(-79), 8) );
  VERIFY( check_to_chars<signed int>(-79, to_string(-79), 8) );
  VERIFY( check_to_chars<signed long>(-79, to_string(-79), 8) );
  VERIFY( check_to_chars<signed long long>(-79, to_string(-79), 8) );

  VERIFY( check_to_chars<char>(CHAR_MAX, to_string(CHAR_MAX), 8) );
  VERIFY( check_to_chars<signed char>(SCHAR_MAX, to_string(SCHAR_MAX), 8) );
  VERIFY( check_to_chars<unsigned char>(UCHAR_MAX, to_string(UCHAR_MAX), 8) );
  VERIFY( check_to_chars<signed short>(SHRT_MAX, to_string(SHRT_MAX), 8) );
  VERIFY( check_to_chars<unsigned short>(USHRT_MAX, to_string(USHRT_MAX), 8) );
  VERIFY( check_to_chars<signed int>(INT_MAX, to_string(INT_MAX), 8) );
  VERIFY( check_to_chars<unsigned int>(UINT_MAX, to_string(UINT_MAX), 8) );
  VERIFY( check_to_chars<signed long>(LONG_MAX, to_string(LONG_MAX), 8) );
  VERIFY( check_to_chars<unsigned long>(ULONG_MAX, to_string(ULONG_MAX), 8) );
  VERIFY( check_to_chars<signed long long>(LLONG_MAX, to_string(LLONG_MAX), 8) );
  VERIFY( check_to_chars<unsigned long long>(ULLONG_MAX, to_string(ULLONG_MAX), 8) );

  VERIFY( check_to_chars<char>(CHAR_MIN, to_string(CHAR_MIN), 8) );
  VERIFY( check_to_chars<signed char>(SCHAR_MIN, to_string(SCHAR_MIN), 8) );
  VERIFY( check_to_chars<signed short>(SHRT_MIN, to_string(SHRT_MIN), 8) );
  VERIFY( check_to_chars<signed int>(INT_MIN, to_string(INT_MIN), 8) );
  VERIFY( check_to_chars<signed long>(LONG_MIN, to_string(LONG_MIN), 8) );
  VERIFY( check_to_chars<signed long long>(LLONG_MIN, to_string(LLONG_MIN), 8) );

  VERIFY( check_to_chars<char>(CHAR_MAX/2, to_string(CHAR_MAX/2), 8) );
  VERIFY( check_to_chars<signed char>(SCHAR_MAX/2, to_string(SCHAR_MAX/2), 8) );
  VERIFY( check_to_chars<unsigned char>(UCHAR_MAX/2, to_string(UCHAR_MAX/2), 8) );
  VERIFY( check_to_chars<signed short>(SHRT_MAX/2, to_string(SHRT_MAX/2), 8) );
  VERIFY( check_to_chars<unsigned short>(USHRT_MAX/2, to_string(USHRT_MAX/2), 8) );
  VERIFY( check_to_chars<signed int>(INT_MAX/2, to_string(INT_MAX/2), 8) );
  VERIFY( check_to_chars<unsigned int>(UINT_MAX/2, to_string(UINT_MAX/2), 8) );
  VERIFY( check_to_chars<signed long>(LONG_MAX/2, to_string(LONG_MAX/2), 8) );
  VERIFY( check_to_chars<unsigned long>(ULONG_MAX/2, to_string(ULONG_MAX/2), 8) );
  VERIFY( check_to_chars<signed long long>(LLONG_MAX/2, to_string(LLONG_MAX/2), 8) );
  VERIFY( check_to_chars<unsigned long long>(ULLONG_MAX/2, to_string(ULLONG_MAX/2), 8) );
}

// base 16
void
test05()
{
  auto to_string = [](auto val) {
    std::ostringstream ss;
    ss << std::hex;
    if (val < 0)
      ss << '-' << (~val + 1ull);
    else if (sizeof(val) == 1)
      ss << (int)val;
    else
      ss << val;
    return ss.str();
  };

  VERIFY( check_to_chars<char>(123, to_string(123), 16) );
  VERIFY( check_to_chars<signed char>(123, to_string(123), 16) );
  VERIFY( check_to_chars<unsigned char>(123, to_string(123), 16) );
  VERIFY( check_to_chars<signed short>(123, to_string(123), 16) );
  VERIFY( check_to_chars<unsigned short>(123, to_string(123), 16) );
  VERIFY( check_to_chars<signed int>(123, to_string(123), 16) );
  VERIFY( check_to_chars<unsigned int>(123, to_string(123), 16) );
  VERIFY( check_to_chars<signed long>(123, to_string(123), 16) );
  VERIFY( check_to_chars<unsigned long>(123, to_string(123), 16) );
  VERIFY( check_to_chars<signed long long>(123, to_string(123), 16) );
  VERIFY( check_to_chars<unsigned long long>(123, to_string(123), 16) );

  if constexpr (std::is_signed_v<char>)
    VERIFY( check_to_chars<char>(-79, to_string(-79), 16) );
  VERIFY( check_to_chars<signed char>(-79, to_string(-79), 16) );
  VERIFY( check_to_chars<signed short>(-79, to_string(-79), 16) );
  VERIFY( check_to_chars<signed int>(-79, to_string(-79), 16) );
  VERIFY( check_to_chars<signed long>(-79, to_string(-79), 16) );
  VERIFY( check_to_chars<signed long long>(-79, to_string(-79), 16) );

  VERIFY( check_to_chars<char>(CHAR_MAX, to_string(CHAR_MAX), 16) );
  VERIFY( check_to_chars<signed char>(SCHAR_MAX, to_string(SCHAR_MAX), 16) );
  VERIFY( check_to_chars<unsigned char>(UCHAR_MAX, to_string(UCHAR_MAX), 16) );
  VERIFY( check_to_chars<signed short>(SHRT_MAX, to_string(SHRT_MAX), 16) );
  VERIFY( check_to_chars<unsigned short>(USHRT_MAX, to_string(USHRT_MAX), 16) );
  VERIFY( check_to_chars<signed int>(INT_MAX, to_string(INT_MAX), 16) );
  VERIFY( check_to_chars<unsigned int>(UINT_MAX, to_string(UINT_MAX), 16) );
  VERIFY( check_to_chars<signed long>(LONG_MAX, to_string(LONG_MAX), 16) );
  VERIFY( check_to_chars<unsigned long>(ULONG_MAX, to_string(ULONG_MAX), 16) );
  VERIFY( check_to_chars<signed long long>(LLONG_MAX, to_string(LLONG_MAX), 16) );
  VERIFY( check_to_chars<unsigned long long>(ULLONG_MAX, to_string(ULLONG_MAX), 16) );

  VERIFY( check_to_chars<char>(CHAR_MIN, to_string(CHAR_MIN), 16) );
  VERIFY( check_to_chars<signed char>(SCHAR_MIN, to_string(SCHAR_MIN), 16) );
  VERIFY( check_to_chars<signed short>(SHRT_MIN, to_string(SHRT_MIN), 16) );
  VERIFY( check_to_chars<signed int>(INT_MIN, to_string(INT_MIN), 16) );
  VERIFY( check_to_chars<signed long>(LONG_MIN, to_string(LONG_MIN), 16) );
  VERIFY( check_to_chars<signed long long>(LLONG_MIN, to_string(LLONG_MIN), 16) );

  VERIFY( check_to_chars<char>(CHAR_MAX/2, to_string(CHAR_MAX/2), 16) );
  VERIFY( check_to_chars<signed char>(SCHAR_MAX/2, to_string(SCHAR_MAX/2), 16) );
  VERIFY( check_to_chars<unsigned char>(UCHAR_MAX/2, to_string(UCHAR_MAX/2), 16) );
  VERIFY( check_to_chars<signed short>(SHRT_MAX/2, to_string(SHRT_MAX/2), 16) );
  VERIFY( check_to_chars<unsigned short>(USHRT_MAX/2, to_string(USHRT_MAX/2), 16) );
  VERIFY( check_to_chars<signed int>(INT_MAX/2, to_string(INT_MAX/2), 16) );
  VERIFY( check_to_chars<unsigned int>(UINT_MAX/2, to_string(UINT_MAX/2), 16) );
  VERIFY( check_to_chars<signed long>(LONG_MAX/2, to_string(LONG_MAX/2), 16) );
  VERIFY( check_to_chars<unsigned long>(ULONG_MAX/2, to_string(ULONG_MAX/2), 16) );
  VERIFY( check_to_chars<signed long long>(LLONG_MAX/2, to_string(LLONG_MAX/2), 16) );
  VERIFY( check_to_chars<unsigned long long>(ULLONG_MAX/2, to_string(ULLONG_MAX/2), 16) );
}

#include <bitset>

// base 2
void
test06()
{
  auto to_string = [](auto val) {
    std::string s, sign;
    if (val < 0)
    {
      auto absval = ~val + 1ull;
      s = std::bitset<sizeof(absval) * CHAR_BIT>(absval).to_string();
      sign = '-';
    }
    else
      s = std::bitset<sizeof(val) * CHAR_BIT>(val).to_string();
    auto pos = s.find_first_not_of("0");
    if (pos == std::string::npos)
      s.resize(1);
    else
      s.erase(0, pos);
    return sign + s;
  };

  VERIFY( check_to_chars<char>(123, to_string(123), 2) );
  VERIFY( check_to_chars<signed char>(123, to_string(123), 2) );
  VERIFY( check_to_chars<unsigned char>(123, to_string(123), 2) );
  VERIFY( check_to_chars<signed short>(123, to_string(123), 2) );
  VERIFY( check_to_chars<unsigned short>(123, to_string(123), 2) );
  VERIFY( check_to_chars<signed int>(123, to_string(123), 2) );
  VERIFY( check_to_chars<unsigned int>(123, to_string(123), 2) );
  VERIFY( check_to_chars<signed long>(123, to_string(123), 2) );
  VERIFY( check_to_chars<unsigned long>(123, to_string(123), 2) );
  VERIFY( check_to_chars<signed long long>(123, to_string(123), 2) );
  VERIFY( check_to_chars<unsigned long long>(123, to_string(123), 2) );

  if constexpr (std::is_signed_v<char>)
    VERIFY( check_to_chars<char>(-79, to_string(-79), 2) );
  VERIFY( check_to_chars<signed char>(-79, to_string(-79), 2) );
  VERIFY( check_to_chars<signed short>(-79, to_string(-79), 2) );
  VERIFY( check_to_chars<signed int>(-79, to_string(-79), 2) );
  VERIFY( check_to_chars<signed long>(-79, to_string(-79), 2) );
  VERIFY( check_to_chars<signed long long>(-79, to_string(-79), 2) );

  VERIFY( check_to_chars<char>(CHAR_MAX, to_string(CHAR_MAX), 2) );
  VERIFY( check_to_chars<signed char>(SCHAR_MAX, to_string(SCHAR_MAX), 2) );
  VERIFY( check_to_chars<unsigned char>(UCHAR_MAX, to_string(UCHAR_MAX), 2) );
  VERIFY( check_to_chars<signed short>(SHRT_MAX, to_string(SHRT_MAX), 2) );
  VERIFY( check_to_chars<unsigned short>(USHRT_MAX, to_string(USHRT_MAX), 2) );
  VERIFY( check_to_chars<signed int>(INT_MAX, to_string(INT_MAX), 2) );
  VERIFY( check_to_chars<unsigned int>(UINT_MAX, to_string(UINT_MAX), 2) );
  VERIFY( check_to_chars<signed long>(LONG_MAX, to_string(LONG_MAX), 2) );
  VERIFY( check_to_chars<unsigned long>(ULONG_MAX, to_string(ULONG_MAX), 2) );
  VERIFY( check_to_chars<signed long long>(LLONG_MAX, to_string(LLONG_MAX), 2) );
  VERIFY( check_to_chars<unsigned long long>(ULLONG_MAX, to_string(ULLONG_MAX), 2) );

  VERIFY( check_to_chars<char>(CHAR_MIN, to_string(CHAR_MIN), 2) );
  VERIFY( check_to_chars<signed char>(SCHAR_MIN, to_string(SCHAR_MIN), 2) );
  VERIFY( check_to_chars<signed short>(SHRT_MIN, to_string(SHRT_MIN), 2) );
  VERIFY( check_to_chars<signed int>(INT_MIN, to_string(INT_MIN), 2) );
  VERIFY( check_to_chars<signed long>(LONG_MIN, to_string(LONG_MIN), 2) );
  VERIFY( check_to_chars<signed long long>(LLONG_MIN, to_string(LLONG_MIN), 2) );

  VERIFY( check_to_chars<char>(CHAR_MAX/2, to_string(CHAR_MAX/2), 2) );
  VERIFY( check_to_chars<signed char>(SCHAR_MAX/2, to_string(SCHAR_MAX/2), 2) );
  VERIFY( check_to_chars<unsigned char>(UCHAR_MAX/2, to_string(UCHAR_MAX/2), 2) );
  VERIFY( check_to_chars<signed short>(SHRT_MAX/2, to_string(SHRT_MAX/2), 2) );
  VERIFY( check_to_chars<unsigned short>(USHRT_MAX/2, to_string(USHRT_MAX/2), 2) );
  VERIFY( check_to_chars<signed int>(INT_MAX/2, to_string(INT_MAX/2), 2) );
  VERIFY( check_to_chars<unsigned int>(UINT_MAX/2, to_string(UINT_MAX/2), 2) );
  VERIFY( check_to_chars<signed long>(LONG_MAX/2, to_string(LONG_MAX/2), 2) );
  VERIFY( check_to_chars<unsigned long>(ULONG_MAX/2, to_string(ULONG_MAX/2), 2) );
  VERIFY( check_to_chars<signed long long>(LLONG_MAX/2, to_string(LLONG_MAX/2), 2) );
  VERIFY( check_to_chars<unsigned long long>(ULLONG_MAX/2, to_string(ULLONG_MAX/2), 2) );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
}
