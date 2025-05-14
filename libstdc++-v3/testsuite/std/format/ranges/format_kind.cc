// { dg-do run { target c++23 } }

#include <deque>
#include <flat_map>
#include <flat_set>
#include <format>
#include <list>
#include <map>
#include <set>
#include <testsuite_hooks.h>
#include <unordered_map>
#include <unordered_set>
#include <vector>

static_assert( std::format_kind<std::vector<int>> == std::range_format::sequence );
static_assert( std::format_kind<std::deque<int>> == std::range_format::sequence );
static_assert( std::format_kind<std::list<int>> == std::range_format::sequence );

static_assert( std::format_kind<std::set<int>> == std::range_format::set );
static_assert( std::format_kind<std::multiset<int>> == std::range_format::set );
static_assert( std::format_kind<std::unordered_set<int>> == std::range_format::set );
static_assert( std::format_kind<std::unordered_multiset<int>> == std::range_format::set );
static_assert( std::format_kind<std::flat_set<int>> == std::range_format::set );
static_assert( std::format_kind<std::flat_multiset<int>> == std::range_format::set );

static_assert( std::format_kind<std::map<int, int>> == std::range_format::map );
static_assert( std::format_kind<std::multimap<int, int>> == std::range_format::map );
static_assert( std::format_kind<std::unordered_map<int, int>> == std::range_format::map );
static_assert( std::format_kind<std::unordered_multimap<int, int>> == std::range_format::map );
static_assert( std::format_kind<std::flat_map<int, int>> == std::range_format::map );
static_assert( std::format_kind<std::flat_multimap<int, int>> == std::range_format::map );

template<typename T>
struct MyVec : std::vector<T>
{};

static_assert( std::format_kind<MyVec<int>> == std::range_format::sequence );

template<typename T>
struct MySet : std::vector<T>
{
  using key_type = T;
};

static_assert( std::format_kind<MySet<int>> == std::range_format::set );

template<typename T>
struct MyMap : std::vector<T>
{
  using key_type = T;
  using mapped_type = int;
};

static_assert( std::format_kind<MyMap<std::pair<int, int>>> == std::range_format::map );
static_assert( std::format_kind<MyMap<std::tuple<int, int>>> == std::range_format::map );
static_assert( std::format_kind<MyMap<int>> == std::range_format::set );

template<typename T, std::range_format rf>
struct CustFormat : std::vector<T>
{
  using std::vector<T>::vector;
};

template<typename T, std::range_format rf>
constexpr auto std::format_kind<CustFormat<T, rf>> = rf;

void test_override()
{
  CustFormat<int, std::range_format::disabled> disabledf;
  static_assert( !std::formattable<decltype(disabledf), char> );

  CustFormat<int, std::range_format::sequence> seqf{1, 2, 3};
  VERIFY( std::format("{}", seqf) == "[1, 2, 3]" );

  CustFormat<int, std::range_format::set> setf{1, 2, 3};
  VERIFY( std::format("{}", setf) == "{1, 2, 3}" );

  // TODO test map once formatter for pair is implenented

  CustFormat<char, std::range_format::string> stringf{'a', 'b', 'c', 'd'};
  VERIFY( std::format("{}", stringf) == "abcd" );
  // Support precision as string do
  VERIFY( std::format("{:.2}", stringf) == "ab" );

  CustFormat<char, std::range_format::debug_string> debugf{'a', 'b', 'c', 'd'};
  VERIFY( std::format("{}", debugf) == R"("abcd")" );
  // Support precision as string do
  VERIFY( std::format("{:.3}", debugf) == R"("ab)" );
}

int main()
{
  test_override();
}
