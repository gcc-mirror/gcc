// { dg-do compile { target c++23 } }

#include <format>
#include <vector>

// only format_kind::sequence provides set_brackets and set_separator methods 

template<std::range_format fk, typename T>
struct MyCont : std::vector<T>
{
  using std::vector<T>::vector;
};

template<std::range_format fk, typename T>
constexpr std::range_format std::format_kind<MyCont<fk, T>> = fk;

void test_sequence()
{
  std::formatter<MyCont<std::range_format::sequence, int>, char> fmtter;
  fmtter.set_brackets("{", "}");
  fmtter.set_separator(",");
}

void test_map()
{
  std::formatter<MyCont<std::range_format::map, std::pair<int, int>>, char> fmtter;
  fmtter.set_brackets("{", "}"); // { dg-error "here" }
  fmtter.set_separator(","); // { dg-error "here" }
}

void test_set()
{
  std::formatter<MyCont<std::range_format::set, int>, char> fmtter;
  fmtter.set_brackets("{", "}"); // { dg-error "here" }
  fmtter.set_separator(","); // { dg-error "here" }
}

void test_string()
{
  std::formatter<MyCont<std::range_format::string, char>, char> fmtter;
  fmtter.set_brackets("{", "}"); // { dg-error "here" }
  fmtter.set_separator(","); // { dg-error "here" }
}

void test_debug_string()
{
  std::formatter<MyCont<std::range_format::debug_string, char>, char> fmtter;
  fmtter.set_brackets("{", "}"); // { dg-error "here" }
  fmtter.set_separator(","); // { dg-error "here" }
}

// { dg-error "no matching function for call to 'std::formatter<" "" { target *-*-* } 0 }
