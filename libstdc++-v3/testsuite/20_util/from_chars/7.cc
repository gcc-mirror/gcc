// Testcases for binary64 hexfloat std::from_chars.
// { dg-do run { target c++17 } }
// { dg-require-effective-target ieee_floats }

#include <charconv>

#include <cfenv>
#include <cmath>
#include <cstring>
#include <cstdio>
#include <limits>
#include <testsuite_hooks.h>

struct testcase {
  const char* input;
  size_t correct_idx;
  std::errc correct_ec;
  double correct_value;
};

constexpr testcase testcases[] = {
  { "1.fffffffffffff8p0", 18, {}, 0x1.fffffffffffff8p0 },
  { "0.fffffffffffff8p-1022", 22, std::errc{}, 0x0.fffffffffffffep-1022 },
  { "inf", 3, {}, std::numeric_limits<double>::infinity() },
  { "inff", 3, {}, std::numeric_limits<double>::infinity() },
  { "-inf", 4, {}, -std::numeric_limits<double>::infinity() },
  { "-inff", 4, {}, -std::numeric_limits<double>::infinity() },
  { "NAN", 3, {}, std::numeric_limits<double>::quiet_NaN() },
  { "-NAN", 4, {}, std::numeric_limits<double>::quiet_NaN() },
  { "NAN()", 5, {}, std::numeric_limits<double>::quiet_NaN() },
  { "-NAN()", 6, {}, std::numeric_limits<double>::quiet_NaN() },
  { "-NAN(test)", 10, {}, std::numeric_limits<double>::quiet_NaN() },
  { "-NAN(test", 4, {}, std::numeric_limits<double>::quiet_NaN() },
  { "-NAN(", 4, {}, std::numeric_limits<double>::quiet_NaN() },
  { "0.000000000000001p-100000000000000000", 37, std::errc::result_out_of_range, 0 },
  { "-lol", 0, std::errc::invalid_argument, 1 },
  { " 0", 0, std::errc::invalid_argument, 1 },
  { "", 0, std::errc::invalid_argument, 0 },
  { "1", 1, {}, 1 },
  { "2", 1, {}, 2 },
  { "3", 1, {}, 3 },
  { "4", 1, {}, 4 },
  { "5", 1, {}, 5 },
  { "6", 1, {}, 6 },
  { "7", 1, {}, 7 },
  { "8", 1, {}, 8 },
  { "9", 1, {}, 9 },
  { "a", 1, {}, 0xa },
  { "b", 1, {}, 0xb },
  { "c", 1, {}, 0xc },
  { "d", 1, {}, 0xd },
  { "e", 1, {}, 0xe },
  { "f", 1, {}, 0xf },
  { "0.000000000000000000000000000000000000000000001p-1022", 53,
      std::errc::result_out_of_range, 0 },
  { "0.00000000000000p-1022", 22, {}, 0 },
  { "0.00000000000009", 16, {}, 0x0.00000000000009p0 },
  { "0.0", 3, {}, 0 },
  { "1p10000000000000000000000", 25, std::errc::result_out_of_range, 0 },
  { "-0.0", 4, {}, -0.0 },
  { "0.00000000000000", 16, {}, 0 },
  { "0.0000000000000p-1022", 21, {}, 0 },
  { ".", 0, std::errc::invalid_argument, 0 },
  { "-.", 0, std::errc::invalid_argument, 0 },
  { "0", 1, {}, 0 },
  { "00", 2, {}, 0 },
  { "00.", 3, {}, 0 },
  { "0.", 2, {}, 0 },
  { "1.ffffFFFFFFFFFF", 16, {}, 0x2 },
  { "1.ffffffffffffff", 16, {}, 0x2 },
  { "1.00000000000029", 16, {}, 0x1.0000000000003p0 },
  { "0.00000000000008p-1022", 22, std::errc::result_out_of_range, 0 },
  { "1.fffffffffffffp-1023", 21, {}, 0x1p-1022 },
  { "1.fffffffffffff8p+1023", 22, std::errc::result_out_of_range, 0 },
  { "0.ffffffffffffe8p-1022", 22, {}, 0x0.ffffffffffffep-1022 },
  { "2.11111111111111", 16, {},   0x1.0888888888889p+1 },
  { "1.1111111111111", 15, {}, 0x1.1111111111111p0 },
  { "1.11111111111111", 16, {}, 0x1.1111111111111p0 },
  { "1.11111111111118", 16, {}, 0x1.1111111111112p0 },
  { "1.11111111111128", 16, {}, 0x1.1111111111112p0 },
  { "1.1111111111112801", 18, {}, 0x1.1111111111113p0 },
  { "1.08888888888888", 16, {}, 0x1.0888888888888p0 },
  { "1.088888888888888", 17, {}, 0x1.0888888888889p0 },
  { "2.00000000000029", 16, {}, 0x2.0000000000002p0 },
  { "0.ffffffffffffep-1022", 21, {}, 0x0.ffffffffffffep-1022 },
  { "3.ffffffffffffep-1024", 21, {}, 0x1p-1022 },
  { "1.00000000000008p+0", 19, {}, 1 },
  { "1p-1023", 7, {}, 0x0.8p-1022 },
  { "1p-1022", 7, {}, 0x1p-1022 },
  { "1.1p-1033", 9, {}, 0x1.1p-1033 }, // 0.0022p-1022
  { "22p-1038", 8, {}, 0x1.1p-1033 },
  { "5", 1, {}, 0x5 },
  { "a", 1, {}, 0xa },
  { "1", 1, {}, 1.0 },
  { "1p1", 3, {}, 0x1p1 },
  { "1p-1", 4, {}, 0x1p-1 },
  { "0", 1, {}, 0.0 },
  { "A", 1, {}, 0xA },
  { "1.ABCDEFP+10", 12, {}, 0x1.ABCDEFP+10 },
  { "-1", 2, {}, -1.0 },
  { "-0", 2, {}, -0.0 },
  { "42", 2, {}, 0x42p0 },
  { "-42", 3, {}, -0x42p0 },
  { ".1", 2, {}, 0x0.1p0 },
  { "22p-1000", 8, {}, 0x22p-1000 },
  { ".0000008", 8, {}, 0x.0000008p0 },
  { ".0000008p-1022", 14, {}, 0x.0000008p-1022 },
  { "1p-1074", 7, {}, 0x.0000000000001p-1022 },
  { "9999999999999", 13, {}, 0x9999999999999p0 },
  { "1.000000000000a000", 18, {}, 0x1.000000000000ap0 },
  { "1.000000000000a001", 18, {}, 0x1.000000000000ap0 },
  { "1.000000000000a800", 18, {}, 0x1.000000000000ap0 },
  { "1.000000000000a801", 18, {}, 0x1.000000000000bp0 },
  { "1.000000000000b800", 18, {}, 0x1.000000000000cp0 },
  { "000000", 6, {}, 0x0 },
  { "1p", 1, {}, 0x1 },
  { "0p99999999999999999999", 22, {}, 0 },
  { "1p99999999999999999999", 22, std::errc::result_out_of_range, 0 },
  { "0p-99999999999999999999", 23, {}, 0 },
  { "1p-99999999999999999999", 23, std::errc::result_out_of_range, 0 },
  { "99999999999999999999999", 23, {}, 0x99999999999999999999999p0 },
  { "-1.fffffffffffffp-1023", 22, {}, -0x1p-1022 },
  { "1.337", 5, {}, 0x1.337p0 },
};

void
test01()
{
  for (auto [input,correct_idx,correct_ec,correct_value] : testcases)
    {
      double value;
      auto [ptr,ec] = std::from_chars(input, input+strlen(input),
				      value, std::chars_format::hex);
      VERIFY( ptr == input + correct_idx );
      VERIFY( ec == correct_ec );
      if (ec == std::errc{})
	{
	  if (std::isnan(value) || std::isnan(correct_value))
	    VERIFY( std::isnan(value) && std::isnan(correct_value) );
	  else
	    {
	      VERIFY( value == correct_value );
	      VERIFY( !memcmp(&value, &correct_value, sizeof(double)) );
	    }
	}
    }
}

int
main()
{
  test01();
}
