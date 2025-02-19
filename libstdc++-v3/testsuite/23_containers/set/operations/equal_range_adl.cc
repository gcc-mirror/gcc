// { dg-do compile }

#include <set>

namespace adl
{
#if __cplusplus < 201103L
  template<typename T> void make_pair(const T&, const T&) { }
#else
  template<typename T> void make_pair(T&&, T&&) { }
#endif

  struct X { bool operator<(const X&) const { return false; } };
}

typedef std::set<adl::X> Set;

void
test_equal_range(Set& s, const adl::X& x)
{
  // _Rb_tree::_M_equal_range was using make_pair unqualified.
  (void) s.equal_range(x);
  const Set& cs = s;
  // Similarly for the const overload.
  (void) cs.equal_range(x);
}
