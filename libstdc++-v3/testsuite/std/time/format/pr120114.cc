// { dg-do run { target c++23 } }
// { dg-options "-fexec-charset=UTF-8" }
// { dg-timeout-factor 2 }

#include <algorithm>
#include <chrono>
#include <testsuite_hooks.h>

#define WIDEN_(C, S) ::std::__format::_Widen<C>(S, L##S)
#define WIDEN(S) WIDEN_(_CharT, S)

template<typename _CharT>
void
test_from_format_string()
{
  std::basic_string<_CharT> res;
  using namespace std::chrono_literals;
  auto date = 2025y/std::chrono::May/05d;

  res = std::format(WIDEN("{:+<13%F\U0001f921}"), date);
  VERIFY( res == WIDEN("2025-05-05\U0001f921+") );

  res = std::format(WIDEN("{:->15%F\U0001f921}"), date);
  VERIFY( res == WIDEN("---2025-05-05\U0001f921") );

  res = std::format(WIDEN("{:=^20%F\U0001f921}"), date);
  VERIFY( res == WIDEN("====2025-05-05\U0001f921====") );
}

template<typename _CharT>
void
test_formatted_value()
{
  // Custom time_put facet which returns Ideographic Telegraph Symbol
  // for given month for Om.
  struct TimePut : std::time_put<_CharT>
  {
    using iter_type = std::time_put<_CharT>::iter_type;
    using char_type = std::time_put<_CharT>::char_type;

    iter_type
    do_put(iter_type out, std::ios_base& io, char_type fill, const tm* t,
	   char format, char modifier) const override
    {
      if (format != 'm' && modifier != 'm')
	return std::time_put<_CharT>::do_put(out, io, fill, t, format, modifier);
      std::basic_string_view<_CharT> str;
      switch (t->tm_mon)
       {
	 case 0:
	   str = WIDEN("\u32C0");
	   break;
	 case 1:
	   str = WIDEN("\u32C1");
	   break;
	 case 2:
	   str = WIDEN("\u32C2");
	   break;
	 case 3:
	   str = WIDEN("\u32C3");
	   break;
	 case 4:
	   str = WIDEN("\u32C4");
	   break;
	 case 5:
	   str = WIDEN("\u32C5");
	   break;
	 case 6:
	   str = WIDEN("\u32C6");
	   break;
	 case 7:
	   str = WIDEN("\u32C7");
	   break;
	 case 8:
	   str = WIDEN("\u32C8");
	   break;
	 case 9:
	   str = WIDEN("\u32C9");
	   break;
	 case 10:
	   str = WIDEN("\u32CA");
	   break;
	 case 11:
	   str = WIDEN("\u32CB");
	   break;
       };
       return std::copy(str.begin(), str.end(), out);
    }
  };
  const std::locale loc(std::locale::classic(), new TimePut);

  std::basic_string<_CharT> res;

  res = std::format(loc, WIDEN("{:<1L%Om}"), std::chrono::January);
  VERIFY( res == WIDEN("\u32C0") );

  res = std::format(loc, WIDEN("{:>2L%Om}"), std::chrono::February);
  VERIFY( res == WIDEN("\u32C1") );

  res = std::format(loc, WIDEN("{:<3L%Om}"), std::chrono::March);
  VERIFY( res == WIDEN("\u32C2 ") );

  res = std::format(loc, WIDEN("{:^4L%Om}"), std::chrono::April);
  VERIFY( res == WIDEN(" \u32C3 ") );

  res = std::format(loc, WIDEN("{:>5L%Om}"), std::chrono::May);
  VERIFY( res == WIDEN("   \u32C4") );

  res = std::format(loc, WIDEN("{:+<6L%Om}"), std::chrono::June);
  VERIFY( res == WIDEN("\u32C5++++") );

  res = std::format(loc, WIDEN("{:=^7L%Om}"), std::chrono::July);
  VERIFY( res == WIDEN("==\u32C6===") );

  res = std::format(loc, WIDEN("{:->8L%Om}"), std::chrono::August);
  VERIFY( res == WIDEN("------\u32C7") );
}

int main()
{
  test_from_format_string<char>();
  test_from_format_string<wchar_t>();
  test_formatted_value<char>();
  test_formatted_value<wchar_t>();
}
