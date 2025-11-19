// { dg-do run { target c++26 } }

#include <chrono>
#include <unordered_set>
#include <limits.h>
#include <testsuite_hooks.h>

#if _GLIBCXX_USE_CXX11_ABI
#  if !defined(__cpp_lib_chrono)
#    error "__cpp_lib_chrono not defined"
#  elif __cpp_lib_chrono < 202306L
#    error "Wrong value for __cpp_lib_chrono"
#  endif
#endif

template <typename T>
struct arithmetic_wrapper
{
  arithmetic_wrapper() = default;
  arithmetic_wrapper(T t) : t(t) {}
  friend bool operator==(arithmetic_wrapper, arithmetic_wrapper) = default;
  T t;
};

template <typename T>
struct std::hash<arithmetic_wrapper<T>>
{
  size_t operator()(arithmetic_wrapper<T> val) const noexcept
  { return std::hash<T>{}(val.t); }
};

template <typename T>
struct non_hashable_arithmetic_wrapper
{
  non_hashable_arithmetic_wrapper() = default;
  non_hashable_arithmetic_wrapper(T t) : t(t) {}
  friend bool operator==(non_hashable_arithmetic_wrapper, non_hashable_arithmetic_wrapper) = default;
  T t;
};

template <typename T>
constexpr bool is_hash_poisoned = !std::is_default_constructible_v<std::hash<T>>;

template <typename T>
void test_unordered_set(const T& t)
{
  std::unordered_set<T> set;

  set.insert(t);
  VERIFY(set.size() == 1);
  VERIFY(set.contains(t));

  set.erase(t);
  VERIFY(set.size() == 0);
  VERIFY(!set.contains(t));
}

template <typename T>
void test_hash(const T& t)
{
  static_assert(noexcept(std::hash<T>{}(t)));
  static_assert(std::__is_fast_hash<T>::value);
  test_unordered_set(t);
}

void test01()
{
  using namespace std::chrono;
  using namespace std::literals::chrono_literals;

  // duration
  test_hash(-999s);
  test_hash(1234ms);
#if defined __SIZEOF_INT128__
  test_hash(duration<__int128>(123456));
#endif
  test_hash(duration<double>(123.45));
  using AWint = arithmetic_wrapper<int>;
  test_hash(duration<AWint>(AWint(1234)));
  using AWdouble = arithmetic_wrapper<double>;
  test_hash(duration<AWdouble>(AWdouble(123.45)));

  // time_point
  test_hash(sys_seconds(1234s));
#if defined __SIZEOF_INT128__
  test_hash(sys_time<duration<__int128>>(duration<__int128>(123456)));
#endif
  test_hash(sys_time<duration<double>>(duration<double>(123.45)));
  test_hash(utc_seconds(1234s));
  test_hash(local_days(days(1234)));
  test_hash(system_clock::now());
  test_hash(steady_clock::now());
  test_hash(utc_clock::now());
  test_hash(gps_clock::now());

  // day
  test_hash(1d);
  test_hash(0d);
  test_hash(255d);
  test_hash(1234d);
  test_hash(day(UINT_MAX));

  // month
  test_hash(January);
  test_hash(September);
  test_hash(month(0u));
  test_hash(month(255u));
  test_hash(month(1234u));
  test_hash(month(UINT_MAX));

  // year
  test_hash(2024y);
  test_hash(0y);
  test_hash(year::min());
  test_hash(year::max());
  test_hash(year(INT_MAX));
  test_hash(year(INT_MIN));

  // weekday
  test_hash(Monday);
  test_hash(Thursday);
  test_hash(weekday(255u));
  test_hash(weekday(UINT_MAX));

  // weekday_indexed
  test_hash(Monday[0u]);
  test_hash(Monday[7u]);
  test_hash(Monday[1234u]);
  test_hash(weekday(1234u)[0u]);

  // weekday_last
  test_hash(Monday[last]);
  test_hash(Friday[last]);
  test_hash(weekday(1234u)[last]);

  // month_day
  test_hash(March / 3);
  test_hash(March / 31);
  test_hash(February / 31);
  test_hash(February / 1234);
  test_hash(month(1234u) / 1);

  // month_day_last
  test_hash(March / last);
  test_hash(month(1234u) / last);

  // month_weekday
  test_hash(March / Tuesday[2u]);
  test_hash(month(1234u) / Tuesday[2u]);
  test_hash(March / weekday(1234u)[2u]);
  test_hash(March / Tuesday[1234u]);

  // month_weekday_last
  test_hash(April / Sunday[last]);
  test_hash(month(1234u) / Tuesday[last]);
  test_hash(April / weekday(1234u)[last]);

  // year_month
  test_hash(2024y / August);
  test_hash(1'000'000y / August);
  test_hash(2024y / month(1234u));

  // year_month_day
  test_hash(2024y / August / 31);
  test_hash(-10y / March / 5);
  test_hash(2024y / February / 31);
  test_hash(1'000'000y / March / 5);
  test_hash(2024y / month(1234u) / 5);
  test_hash(2024y / March / 1234);

  // year_month_day_last
  test_hash(2024y / August / last);
  test_hash(1'000'000y / August / last);
  test_hash(2024y / month(1234u) / last);

  // year_month_weekday
  test_hash(2024y / August / Tuesday[2u]);
  test_hash(-10y / August / Tuesday[2u]);
  test_hash(1'000'000y / August / Tuesday[2u]);
  test_hash(2024y / month(1234u) / Tuesday[2u]);
  test_hash(2024y / August / weekday(1234u)[2u]);
  test_hash(2024y / August / Tuesday[1234u]);

  // year_month_weekday_last
  test_hash(2024y / August / Tuesday[last]);
  test_hash(-10y / August / Tuesday[last]);
  test_hash(1'000'000y / August / Tuesday[last]);
  test_hash(2024y / month(1234u) / Tuesday[last]);
  test_hash(2024y / August / weekday(1234u)[last]);

#if _GLIBCXX_USE_CXX11_ABI
  // zoned_time
  test_hash(zoned_seconds("Europe/Rome", sys_seconds(1234s)));
  test_hash(zoned_time("Europe/Rome", system_clock::now()));

  // leap_second
  for (leap_second l : get_tzdb().leap_seconds)
    test_hash(l);
#endif
}

void test02()
{
  using namespace std::chrono;
  using namespace std::literals::chrono_literals;

  {
    std::unordered_set<milliseconds> set;
    set.insert(2000ms);
    VERIFY(set.contains(2000ms));
    VERIFY(set.contains(2s));
    VERIFY(!set.contains(1234ms));
    VERIFY(!set.contains(1234s));
  }
  {
    using TP = sys_time<milliseconds>;
    std::unordered_set<TP> set;
    set.insert(TP(2000ms));
    VERIFY(set.contains(TP(2000ms)));
    VERIFY(set.contains(sys_seconds(2s)));
    VERIFY(!set.contains(TP(1234ms)));
    VERIFY(!set.contains(sys_seconds(1234s)));
  }
}

void test03()
{
  using namespace std::chrono;

  static constexpr
  auto test_hash = []<typename T>(const T& t)
  {
    static_assert(noexcept(std::hash<T>{}(t)));
  };

  static constexpr
  auto test = []<typename D>(const D& d) 
  {
    test_hash(d);
    test_hash(sys_time<D>(d));
#if _GLIBCXX_USE_CXX11_ABI
    test_hash(zoned_time<D>(sys_time<D>(d)));
#endif
  };
  
  test(duration<int>(123));
  test(duration<int, std::ratio<1, 1000>>(123));
  test(duration<int, std::ratio<1000, 1>>(123));
  test(duration<double>(123.456));
  test(duration<arithmetic_wrapper<int>>(arithmetic_wrapper<int>(123)));
}

void test04()
{
  using namespace std::chrono;

  static_assert(!is_hash_poisoned<duration<int>>);
  static_assert(!is_hash_poisoned<duration<double>>);
  static_assert(!is_hash_poisoned<duration<arithmetic_wrapper<int>>>);
  static_assert(!is_hash_poisoned<duration<arithmetic_wrapper<double>>>);
  static_assert(is_hash_poisoned<duration<non_hashable_arithmetic_wrapper<int>>>);
  static_assert(is_hash_poisoned<duration<non_hashable_arithmetic_wrapper<double>>>);

#if _GLIBCXX_USE_CXX11_ABI
  static_assert(!is_hash_poisoned<zoned_time<duration<int>>>);
  static_assert(!is_hash_poisoned<zoned_time<duration<double>>>);
  static_assert(!is_hash_poisoned<zoned_time<duration<arithmetic_wrapper<int>>>>);
  static_assert(!is_hash_poisoned<zoned_time<duration<arithmetic_wrapper<double>>>>);
  static_assert(is_hash_poisoned<zoned_time<duration<non_hashable_arithmetic_wrapper<int>>>>);
  static_assert(is_hash_poisoned<zoned_time<duration<non_hashable_arithmetic_wrapper<double>>>>);
#endif
}

int main()
{
  test01();
  test02();
  test03();
  test04();
}
