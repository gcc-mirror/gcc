// { dg-do run { target c++23 } }

#include <expected>
#include <string_view>
#include <testsuite_hooks.h>

constexpr bool
test_and_then()
{
  std::expected<int, int> e1(1);
  VERIFY( e1.and_then([]<typename T>(T&& v) {
    static_assert( std::is_same_v<T, int&> );
    VERIFY( v == 1 );
    return std::expected<long, int>(100);
  }).value() == 100 );
  VERIFY( std::move(e1).and_then([]<typename T>(T&& v) {
    static_assert( std::is_same_v<T, int> );
    VERIFY( v == 1 );
    return std::expected<long, int>(101);
  }).value() == 101 );
  const auto& ce1 = e1;
  VERIFY( ce1.and_then([]<typename T>(T&& v) {
    static_assert( std::is_same_v<T, const int&> );
    VERIFY( v == 1 );
    return std::expected<long, int>(102);
  }).value() == 102 );
  VERIFY( std::move(ce1).and_then([]<typename T>(T&& v) {
    static_assert( std::is_same_v<T, const int> );
    VERIFY( v == 1 );
    return std::expected<long, int>(103);
  }).value() == 103 );

  auto fail = [] (auto&&) -> std::expected<void, int> { throw 1; };
  std::expected<int, int> e2(std::unexpect, 2);
  VERIFY( e2.and_then(fail).error() == 2 );
  VERIFY( std::move(e2).and_then(fail).error() == 2 );
  const auto& ce2 = e2;
  VERIFY( ce2.and_then(fail).error() == 2 );
  VERIFY( std::move(ce2).and_then(fail).error() == 2 );

  int i = 100;
  auto vpass = [&] -> std::expected<int, int> { return i++; };
  std::expected<void, int> v1;
  VERIFY( v1.and_then(vpass).value() == 100 );
  VERIFY( std::move(v1).and_then(vpass).value() == 101 );
  const auto& cv1 = v1;
  VERIFY( cv1.and_then(vpass).value() == 102 );
  VERIFY( std::move(cv1).and_then(vpass).value() == 103 );

  auto vfail = [] -> std::expected<int, int> { throw 1; };
  std::expected<void, int> v2(std::unexpect, 2);
  VERIFY( v2.and_then(vfail).error() == 2 );
  VERIFY( std::move(v2).and_then(vfail).error() == 2 );
  const auto& cv2 = v2;
  VERIFY( cv2.and_then(vfail).error() == 2 );
  VERIFY( std::move(cv2).and_then(vfail).error() == 2 );

  static_assert(std::is_same_v<decltype(v1.and_then(vpass)),
			       decltype(vpass())>);
  static_assert(std::is_same_v<decltype(cv1.and_then(vpass)),
			       decltype(vpass())>);

  return true;
}

constexpr bool
test_or_else()
{
  std::expected<int, int> e1(std::unexpect, 1);
  VERIFY( e1.or_else([]<typename T>(T&& v) {
    static_assert( std::is_same_v<T, int&> );
    VERIFY( v == 1 );
    return std::expected<int, long>(100);
  }).value() == 100 );
  VERIFY( std::move(e1).or_else([]<typename T>(T&& v) {
    static_assert( std::is_same_v<T, int> );
    VERIFY( v == 1 );
    return std::expected<int, long>(101);
  }).value() == 101 );
  const auto& ce1 = e1;
  VERIFY( ce1.or_else([]<typename T>(T&& v) {
    static_assert( std::is_same_v<T, const int&> );
    VERIFY( v == 1 );
    return std::expected<int, long>(102);
  }).value() == 102 );
  VERIFY( std::move(ce1).or_else([]<typename T>(T&& v) {
    static_assert( std::is_same_v<T, const int> );
    VERIFY( v == 1 );
    return std::expected<int, long>(103);
  }).value() == 103 );

  auto f = [] (auto) -> std::expected<int, long> { throw 1; };
  std::expected<int, int> e2(2);
  VERIFY( e2.or_else(f).value() == 2 );
  VERIFY( std::move(e2).or_else(f).value() == 2 );
  const auto& ce2 = e2;
  VERIFY( ce2.or_else(f).value() == 2 );
  VERIFY( std::move(ce2).or_else(f).value() == 2 );

  auto vf = [] (auto) -> std::expected<void, long> { return {}; };
  std::expected<void, int> v1(std::unexpect, 1);
  VERIFY( v1.or_else(vf).has_value() );
  VERIFY( std::move(v1).or_else(vf).has_value() );
  const auto& cv1 = v1;
  VERIFY( cv1.or_else(vf).has_value() );
  VERIFY( std::move(cv1).or_else(vf).has_value() );

  auto vfail = [] (auto) -> std::expected<void, long> { throw 1; };
  std::expected<void, int> v2;
  VERIFY( v2.or_else(vfail).has_value() );
  VERIFY( std::move(v2).or_else(vfail).has_value() );
  const auto& cv2 = v2;
  VERIFY( cv2.or_else(vfail).has_value() );
  VERIFY( std::move(cv2).or_else(vfail).has_value() );

  static_assert(std::is_same_v<decltype(v1.or_else(vf)), decltype(vf(1))>);
  static_assert(std::is_same_v<decltype(cv1.or_else(vf)), decltype(vf(1))>);

  return true;
}

constexpr bool
test_transform()
{
  std::expected<int, int> e1(1);
  VERIFY( e1.transform([]<typename T>(T&& v) {
    static_assert( std::is_same_v<T, int&> );
    VERIFY( v == 1 );
    return std::string_view("100");
  }).value() == "100" );
  VERIFY( std::move(e1).transform([]<typename T>(T&& v) {
    static_assert( std::is_same_v<T, int> );
    VERIFY( v == 1 );
    return std::string_view("101");
  }).value() == "101" );
  const auto& ce1 = e1;
  VERIFY( ce1.transform([]<typename T>(T&& v) {
    static_assert( std::is_same_v<T, const int&> );
    VERIFY( v == 1 );
    return std::string_view("102");
  }).value() == "102" );
  VERIFY( std::move(ce1).transform([]<typename T>(T&& v) {
    static_assert( std::is_same_v<T, const int> );
    VERIFY( v == 1 );
    return std::string_view("103");
  }).value() == "103" );

  auto fail = [] (auto&&) -> std::string_view { throw 1; };
  std::expected<int, int> e2(std::unexpect, 2);
  VERIFY( e2.transform(fail).error() == 2 );
  VERIFY( std::move(e2).transform(fail).error() == 2 );
  const auto& ce2 = e2;
  VERIFY( ce2.transform(fail).error() == 2 );
  VERIFY( std::move(ce2).transform(fail).error() == 2 );

  auto vpass = [&] -> std::string_view { return "ok"; };
  std::expected<void, int> v1;
  VERIFY( v1.transform(vpass).value() == "ok" );
  VERIFY( std::move(v1).transform(vpass).value() == "ok" );
  const auto& cv1 = v1;
  VERIFY( cv1.transform(vpass).value() == "ok" );
  VERIFY( std::move(cv1).transform(vpass).value() == "ok" );

  auto vfail = [] -> std::string_view { throw 1; };
  std::expected<void, int> v2(std::unexpect, 2);
  VERIFY( v2.transform(vfail).error() == 2 );
  VERIFY( std::move(v2).transform(vfail).error() == 2 );
  const auto& cv2 = v2;
  VERIFY( cv2.transform(vfail).error() == 2 );
  VERIFY( std::move(cv2).transform(vfail).error() == 2 );

  static_assert(std::is_same_v<decltype(v1.transform(vpass)),
			       std::expected<decltype(vpass()), int>>);
  static_assert(std::is_same_v<decltype(cv1.transform(vpass)),
			       std::expected<decltype(vpass()), int>>);

  return true;
}

constexpr bool
test_transform_error()
{
  std::expected<int, int> e1(std::unexpect, 1);
  VERIFY( e1.transform_error([]<typename T>(T&& v) {
    static_assert( std::is_same_v<T, int&> );
    VERIFY( v == 1 );
    return std::string_view("100");
  }).error() == "100" );
  VERIFY( std::move(e1).transform_error([]<typename T>(T&& v) {
    static_assert( std::is_same_v<T, int> );
    VERIFY( v == 1 );
    return std::string_view("101");
  }).error() == "101" );
  const auto& ce1 = e1;
  VERIFY( ce1.transform_error([]<typename T>(T&& v) {
    static_assert( std::is_same_v<T, const int&> );
    VERIFY( v == 1 );
    return std::string_view("102");
  }).error() == "102" );
  VERIFY( std::move(ce1).transform_error([]<typename T>(T&& v) {
    static_assert( std::is_same_v<T, const int> );
    VERIFY( v == 1 );
    return std::string_view("103");
  }).error() == "103" );

  auto fail = [] (auto&&) -> std::string_view { throw 1; };
  std::expected<int, int> e2(2);
  VERIFY( e2.transform_error(fail).value() == 2 );
  VERIFY( std::move(e2).transform_error(fail).value() == 2 );
  const auto& ce2 = e2;
  VERIFY( ce2.transform_error(fail).value() == 2 );
  VERIFY( std::move(ce2).transform_error(fail).value() == 2 );

  auto vpass = [&] (auto) -> std::string_view { return "ok"; };
  std::expected<void, int> v1(std::unexpect, 1);
  VERIFY( v1.transform_error(vpass).error() == "ok" );
  VERIFY( std::move(v1).transform_error(vpass).error() == "ok" );
  const auto& cv1 = v1;
  VERIFY( cv1.transform_error(vpass).error() == "ok" );
  VERIFY( std::move(cv1).transform_error(vpass).error() == "ok" );

  auto vfail = [] (auto) -> std::string_view { throw 1; };
  std::expected<void, int> v2;
  VERIFY( v2.transform_error(vfail).has_value() );
  VERIFY( std::move(v2).transform_error(vfail).has_value() );
  const auto& cv2 = v2;
  VERIFY( cv2.transform_error(vfail).has_value() );
  VERIFY( std::move(cv2).transform_error(vfail).has_value() );

  static_assert(std::is_same_v<decltype(v1.transform_error(vpass)),
			       std::expected<void, decltype(vpass(1))>>);
  static_assert(std::is_same_v<decltype(cv1.transform_error(vpass)),
			       std::expected<void, decltype(vpass(1))>>);

  return true;
}

constexpr bool
test_temporary_materialization()
{
  struct NonCopyable {
    constexpr NonCopyable(int i) : i(i) { }
    NonCopyable(const NonCopyable&) = delete;
    int i;
  };

  auto xform = [](int i) { return NonCopyable(i); };

  std::expected<int, int> e1(1);
  std::expected<NonCopyable, int> n1 = e1.transform(xform);
  VERIFY( n1.value().i == 1 );
  std::expected<int, int> e2(std::unexpected<int>(2));
  std::expected<int, NonCopyable> n2 = e2.transform_error(xform);
  VERIFY( n2.error().i == 2 );

  auto vxform = [] { return NonCopyable(999); };
  std::expected<void, int> v1;
  std::expected<NonCopyable, int> nv1 = v1.transform(vxform);
  VERIFY( nv1.value().i == 999 );
  std::expected<void, int> v2(std::unexpected<int>(22));
  std::expected<void, NonCopyable> nv2 = v2.transform_error(xform);
  VERIFY( nv2.error().i == 22 );

  return true;
}

int main()
{
  static_assert( test_and_then() );
  test_and_then();
  static_assert( test_or_else() );
  test_or_else();
  static_assert( test_transform() );
  test_transform();
  static_assert( test_transform_error() );
  test_transform_error();
  static_assert( test_temporary_materialization() );
  test_temporary_materialization();
}
