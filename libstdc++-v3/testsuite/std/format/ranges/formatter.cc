// { dg-do run { target c++23 } }

#include <flat_map>
#include <format>
#include <testsuite_hooks.h>
#include <vector>

#define WIDEN_(C, S) ::std::__format::_Widen<C>(S, L##S)
#define WIDEN(S) WIDEN_(_CharT, S)

template<typename T,
	 template<typename, typename> class Formatter = std::range_formatter>
struct MyVector : std::vector<T>
{
  using std::vector<T>::vector;
};

template<typename T,
	 template<typename, typename> class Formatter,
	 typename CharT>
struct std::formatter<MyVector<T, Formatter>, CharT>
{
  constexpr formatter() noexcept
  {
    using _CharT = CharT;
    _formatter.set_brackets(WIDEN("<"), WIDEN(">"));
    _formatter.set_separator(WIDEN("; "));
  }

  constexpr std::basic_format_parse_context<CharT>::iterator
  parse(std::basic_format_parse_context<CharT>& pc)
  { return _formatter.parse(pc);  }

  template<typename Out>
  typename std::basic_format_context<Out, CharT>::iterator
  format(const MyVector<T, Formatter>& mv,
	 std::basic_format_context<Out, CharT>& fc) const
  { return _formatter.format(mv, fc); }

private:
  Formatter<T, CharT> _formatter;
};

template<typename _CharT, template<typename, typename> class Formatter>
void
test_default()
{
  MyVector<int, Formatter> vec{1, 2, 3};
  std::basic_string<_CharT> res;

  res = std::format(WIDEN("{}"), vec);
  VERIFY( res == WIDEN("<1; 2; 3>") );
  res = std::format(WIDEN("{:}"), vec);
  VERIFY( res == WIDEN("<1; 2; 3>") );
  res = std::format(WIDEN("{:n}"), vec);
  VERIFY( res == WIDEN("1; 2; 3") );

  res = std::format(WIDEN("{:3}"), vec);
  VERIFY( res == WIDEN("<1; 2; 3>") );

  res = std::format(WIDEN("{:10}"), vec);
  VERIFY( res == WIDEN("<1; 2; 3> ") );

  res = std::format(WIDEN("{:{}}"), vec, 10);
  VERIFY( res == WIDEN("<1; 2; 3> ") );

  res = std::format(WIDEN("{1:{0}}"), 10, vec);
  VERIFY( res == WIDEN("<1; 2; 3> ") );

  res = std::format(WIDEN("{:10n}"), vec);
  VERIFY( res == WIDEN("1; 2; 3   ") );

  res = std::format(WIDEN("{:*<11}"), vec);
  VERIFY( res == WIDEN("<1; 2; 3>**") );

  res = std::format(WIDEN("{:->12}"), vec);
  VERIFY( res == WIDEN("---<1; 2; 3>") );

  res = std::format(WIDEN("{:=^13}"), vec);
  VERIFY( res == WIDEN("==<1; 2; 3>==") );

  res = std::format(WIDEN("{:=^13n}"), vec);
  VERIFY( res == WIDEN("===1; 2; 3===") );

  res = std::format(WIDEN("{::#x}"), vec);
  VERIFY( res == WIDEN("<0x1; 0x2; 0x3>") );

  res = std::format(WIDEN("{:|^25n:#05x}"), vec);
  VERIFY( res == WIDEN("|||0x001; 0x002; 0x003|||") );

  // ':' is start of the format string for element
  res = std::format(WIDEN("{::^+4}"), vec);
  VERIFY( res == WIDEN("< +1 ;  +2 ;  +3 >") );
}

template<typename _CharT, template<typename, typename> class Formatter>
void
test_override()
{
  MyVector<_CharT, Formatter> vc{'a', 'b', 'c', 'd'};
  MyVector<std::pair<int, int>, Formatter> vp{{1, 11}, {2, 21}};
  std::basic_string<_CharT> res;

  res = std::format(WIDEN("{:s}"), vc);
  VERIFY( res == WIDEN("abcd") );
  res = std::format(WIDEN("{:?s}"), vc);
  VERIFY( res == WIDEN("\"abcd\"") );
  res = std::format(WIDEN("{:+^6s}"), vc);
  VERIFY( res == WIDEN("+abcd+") );

  res = std::format(WIDEN("{:m}"), vp);
  VERIFY( res == WIDEN("{1: 11, 2: 21}") );
  res = std::format(WIDEN("{:=^20m}"), vp);
  VERIFY( res == WIDEN("==={1: 11, 2: 21}===") );
}

template<template<typename, typename> class Formatter>
void test_outputs()
{
  test_default<char, Formatter>();
  test_default<wchar_t, Formatter>();
  test_override<char, Formatter>();
  test_override<wchar_t, Formatter>();
}

void
test_nested()
{
  MyVector<MyVector<int>> v
  {
    {1, 2},
    {11, 12}
  };

  std::string res = std::format("{}", v);
  VERIFY( res == "<<1; 2>; <11; 12>>" );

  res = std::format("{:+^18:n:02}", v);
  VERIFY( res == "+<01; 02; 11; 12>+" );
}

struct MyFlatMap : std::flat_map<int, int>
{
  using std::flat_map<int, int>::flat_map;
};

template<typename CharT>
struct std::formatter<MyFlatMap, CharT>
  // This cannot apply format BitVector const&, because formatted type would
  // be std::pair<int const&, int const&>, and formatter for
  // pair<int const&, int> cannot format it.
  : std::range_formatter<MyFlatMap::reference>
{};

void test_const_ref_type_mismatch()
{
  MyFlatMap m{{1, 11}, {2, 22}};
  std::string res = std::format("{:m}", m);
  VERIFY( res == "{1: 11, 2: 22}" );
}

template<typename T, typename CharT>
using VectorFormatter = std::formatter<std::vector<T>, CharT>;

int main()
{
  test_outputs<std::range_formatter>();
  test_outputs<VectorFormatter>();
  test_nested();
  test_const_ref_type_mismatch();
}
