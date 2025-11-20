// { dg-do compile { target c++17 } }

// LWG 4370. Comparison of optional<T> to T may be ill-formed

#include <optional>
#include <testsuite_hooks.h>

struct Bool
{
  Bool(bool);
  operator bool() const;
};

template<typename T> void operator&&(Bool, T) = delete;
template<typename T> void operator&&(T, Bool) = delete;
template<typename T> void operator||(Bool, T) = delete;
template<typename T> void operator||(T, Bool) = delete;

struct S
{
  Bool operator==(S) const;
  Bool operator!=(S) const;
  Bool operator<(S) const;
  Bool operator>(S) const;
  Bool operator<=(S) const;
  Bool operator>=(S) const;
};

void
test_lwg4370()
{
  std::optional<S> o;
  (void)(o == o);
  (void)(o != o);
  (void)(o < o);
  (void)(o > o);
  (void)(o <= o);
  (void)(o >= o);

  S s;
  (void)(o == s);
  (void)(s == o);
  (void)(o != s);
  (void)(s != o);
  (void)(o < s);
  (void)(s < o);
  (void)(o > s);
  (void)(s > o);
  (void)(o <= s);
  (void)(s <= o);
  (void)(o >= s);
  (void)(s >= o);
}


