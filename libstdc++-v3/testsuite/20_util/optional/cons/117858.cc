// { dg-do compile { target c++17 } }

// PR 117858 std::optional with a constructor template<typename T> ctor(T)
// PR 117889 Failure to build qtwebengine-6.8.1

#include <optional>

struct Focus
{
  template<class T> Focus(T) { }
};

void test_pr117858(std::optional<Focus>& f)
{
  f = f;
  f = std::move(f);
}

void test_pr117889(std::optional<Focus>& f)
{
  std::optional<Focus> f2 = f;
  std::optional<Focus> f3 = std::move(f);
}
