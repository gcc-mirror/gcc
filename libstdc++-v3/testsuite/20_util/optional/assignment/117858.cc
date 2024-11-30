// { dg-do compile { target c++17 } }

// PR 117858 std::optional with a constructor template<typename T> ctor(T)

#include <optional>

struct Focus
{
  template<class T>
  Focus(T newValue)  { }
};

void g(std::optional<Focus> f)
{
  f = f;
  f = std::move(f);
}
