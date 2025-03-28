// { dg-do run { target c++23 } }
// { dg-timeout-factor 2 }

#include <format>
#include <vector>
#include <testsuite_hooks.h>

static_assert(!std::formattable<std::vector<bool>::reference, int>);
static_assert(!std::formattable<std::vector<bool>::reference, char32_t>);

template<typename... Args>
bool
is_format_string_for(const char* str, Args&&... args)
{
  try {
    (void) std::vformat(str, std::make_format_args(args...));
    return true;
  } catch (const std::format_error&) {
    return false;
  }
}

#define WIDEN_(C, S) ::std::__format::_Widen<C>(S, L##S)
#define WIDEN(S) WIDEN_(_CharT, S)

void
test_format_string()
{
  std::vector<bool> v(1, true);
  VERIFY( !is_format_string_for("{:?}", v[0]) );
  VERIFY( !is_format_string_for("{:P}", v[0]) );

  // width needs to be integer type
  VERIFY( !is_format_string_for("{:{}}", v[0], 1.0f) );
}

template<typename _CharT>
void
test_output()
{
  std::basic_string<_CharT> res;
  size_t size = 0;
  std::vector<bool> v{true, false};

  res = std::format(WIDEN("{}"), v[0]);
  VERIFY( res == WIDEN("true") );

  res = std::format(WIDEN("{:s}"), v[1]);
  VERIFY( res == WIDEN("false") );

  res = std::format(WIDEN("{:d} {:#B} {:#o} {:#x}"), v[0], v[1], v[0], v[1]);
  VERIFY( res == WIDEN("1 0B0 01 0x0") );

  res = std::format(WIDEN("{:{}}"), v[0], 6);
  VERIFY( res == WIDEN("true  ") );

  res = std::format(WIDEN("{:=^#7X}"), v[1]);
  VERIFY( res == WIDEN("==0X0==") );

  res = std::format(WIDEN("{}"), v);
  VERIFY( res == WIDEN("[true, false]") );

  res = std::format(WIDEN("{::d}"), v);
  VERIFY( res == WIDEN("[1, 0]") );
}

int main()
{
  test_format_string();
  test_output<char>();
  test_output<wchar_t>();
}
